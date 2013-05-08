### SAMPLE DATA


### MAKE SURE TO LOAD IN THE FUNCTIONS FIRST 



### LOAD IN THE DATA FRAME
DF <- 
    structure(list(node = c(500, 501, 502, 503, 504, 505), type = c("artist", 
    "artist", "artist", "artist", "artist", "artist"), name = c("6 Market Blvd.", 
    "6 Pack Deep", "60 Second Crush", "60's Rock n' Roll Show: Tommy James & the Shondells", 
    "60th Anniversary Celebration", "65daysofstatic"), id = c("ART000500", 
    "ART000501", "ART000502", "ART000503", "ART000504", "ART000505"
    ), sourceGrp = c("Concs", "Concs", "Concs", "Concs", "Concs", 
    "Concs")), .Names = c("node", "type", "name", "id", "sourceGrp"
    ), class = "data.frame", row.names = c(NA, -6L))

## CREATE SOME JSONS's FROM EACH ROW
props1 <- toJSON(DF[1, ])
props2 <- toJSON(DF[2, ])
props3 <- toJSON(DF[3, ])
props4 <- toJSON(DF[4, ])

# LOAD IN SOME NODES
newNode1  <- createNode(props1)
newNode2 <- createNode(props2)


## Have a look: 
  newNode1$data
## Compare: 
  DF[1, ]


# -------------------------- ## -------------------------- ## -------------------------- #


#  R2neo4j.r

library(RCurl)
library(rjson)


# -------------------------------------------- #
#                                              #
#             URL's & CURL OPTIONS             #
#                                              #
# -------------------------------------------- #


u.base   <- "http://localhost:7474/db/data/"
u.cypher <- as.path(u.base, "cypher")
u.node   <- as.path(u.base, "node")
u.rel    <- as.path(u.base, "relationship")


# options for CURL
jsonHeader = c("Content-Type" = "application/json", "Accept" = "application/json")



# ------------------------------------------- #
#                                             #
#              WRAPPER FUNCTIONS              #
#                                             #
# ------------------------------------------- #


JU <- function(URL) { 
# Wrapper for fromJSON + getURL
  fromJSON(getURL(URL))
}

JR <- function(raw) { 
  fromJSON(rawToChar(raw))
}

getNode <- function(NODE.Number, JSON=TRUE) { 
# Wrapper for getting node from api
  html <- getURL(as.path(u.node, NODE.Number))

  if (JSON)
    return(fromJSON(html))
  return(html)
}


# ------------------------------------------- #
#                                             #
#                NODE FUNCTIONS               #
#                                             #
# ------------------------------------------- #



deleteNode <- function(NODE.Number, force=FALSE, verbose=TRUE) { 
# TODO:  Put in more error-chcecking. 
#        Confirm Node exists
#        Confirm does not have any relationships
#        Confirm deletion occured (ie, node not found)
#
#  Returns TRUE  if deletion occured
#          FALSE if attempted but failed
#          NULL  if user canceletd at confirmation
# ---------------------------------------------- #


    if (verbose)
      cat ("Deleting Node # ", NODE.Number, "\n")   

    # only ask for confirmation if `force` is false
    if (!force) 
      confirm   <- readline("are you sure [y/n]?  > ")


    if (force || substr(tolower(confirm), 1, 1) == "y" ) {
      H.delete  <- try(httpDELETE(as.path(u.node, NODE.Number), httpheader = jsonHeader), silent=TRUE)
      
      if (inherits(H.delete, "try-error")) {
        if (grepl("Error : Not Found", H.delete[[1]])) {
          cat("\tNode ", NODE.Number, " was not found and hence could not be deleted.\n")
          return(FALSE)
        } else if (grepl("Error : Conflict", H.delete[[1]])) {
          cat("\tNode ", NODE.Number, " has a conflict (possible existing relationship) and hence could not be deleted.\n")
        } else {
          # else: dont know why it delete failed: 
          cat("This Error: \n\t", H.delete, "\n")
          warning("Node ", NODE.Number, " could not be deleted for an unknown reason.\n")
          return (FALSE)
        }    
      }

      # node was succesfully deleted 
      return(TRUE) 
    }

    # else, user did not select "yes" at confirmation
    if (verbose)
      cat("Node-deletion was canceled by user")
    return(NULL)
}


createNode <- function(properties=NULL, retJSON=TRUE) {
# TODO:   What is the difference between giving an explicit node number or not? 
#         What if the node number already exists. 
#
# Args:   
#     Properties:  Character. expected as a single JSON string.  This could change in the future. 
#     retJSON   :  Boolean.   if TRUE, return value will be converted to JSON prior to returning.  
#                   (converting to JSON increase object size 4x-fold. If this is a concern, turn off)

  # error check
  if (length(properties) > 1) { 
    stop("\n\n\t`properties` should be a unit-length character in R,\n\tcontaining a JSON string.\n\n")
  }

  # create the object
  H.post <- httpPOST(u.node, httpheader = jsonHeader, postfields = properties)
  H.post <- rawToChar(H.post)

  if (retJSON)
    return(fromJSON(H.post))
  return(H.post)
}


updateNode <- function(NODE.numer, properties=NULL, retJSON=TRUE)  { 
# Args:     
#     NODE.Number:  Numeric or Character. Which node number to udpate
#     Properties :  Character. expected as a single JSON string.  This could change in the future. 
#     retJSON    :  Boolean.   if TRUE, return value will be converted to JSON prior to returning.  
#                   (converting to JSON increase object size 4x-fold. If this is a concern, turn off)

  # error check
  if (length(properties) > 1) { 
    stop("\n\n\t`properties` should be a unit-length character in R,\n\tcontaining a JSON string.\n\n")
  }

  # create the object
  U <- as.path(u.node, NODE.number, "properties")
  H.put <- httpPUT(U, httpheader = jsonHeader, postfields = properties)
  H.put <- rawToChar(H.put)

  # if H.put is a blank string, return that
  if (!nchar(H.put))
    return(H.put)

  if (retJSON)
    return(fromJSON(H.put))
  return(H.put)
}

NodeNumberFromSelf <- function(U) { 
## Parse the node number for the "..$self" returned form some queries

  if (!any(names(U)=="self")) { 
      # if 'self' not found, recurse until no more to recurse on.
      if (length(U) > 1) {
        return(sapply(U, NodeNumberFromSelf))
      } else 
   stop ("No element with 'self' found.")
  }

  pattern <- paste0(u.base, "(.)+/")  # todo: look ahead: [0-9]+
  as.integer(gsub(pattern, "", U[["self"]]))
}
 

##  THE FOLLOWING FUNCTION DELETES ALL NODES
##  There is a safety bool that must be set to TRUE in order for the delete to occur
deleteALL.allowed <- FALSE
deleteAllNodes <- function()  { 
# this is handy when setting things up. 

  if (!deleteALL.allowed) {
    stop("Delete All Not Allowed.\nPlease change `deleteALL.allowed` to TRUE: \n\n\tdeleteALL.allowed <- TRUE\n\n")
  }

  cat("\n\n\n\nTo continue, you must type:   Yes, I am sure.\n\n")
  confirm <- readline("WARNING!! This will delete ALL NODES in the neo4j database. Are you sure? > ")
  
  if (confirm=="Yes, I am sure.") { 
      QRY   <- "START n=node(*)  RETURN ID(n)"
      Nodes <- CypherQry(QRY)
      
      if (!nrow(Nodes)) { 
        warning("No nodes found. Nothing to delete.")
        return(TRUE)
      }

      ret   <- apply(Nodes, 1, deleteNode, force=TRUE)
      return(all(ret))
  } 

  warning("\nYou did not type `Yes, I am sure.`    <~~  It's gotta be exact.\nNothing deleted.\n\n")
  return(invisible(NULL))
}

createNodesFromDT <- function(DT) { 
# creates a node for each row in DT, where each column is a property value

  ret <- apply(DT, 1, function(x) createNode(properties=toJSON(x))  ) 

  # return node numbers 
  nodesCreated <- NodeNumberFromSelf(ret)

  return(as.integer(nodesCreated))
}

getAllNodeIDs <- function(retVec=FALSE) { 

  allNodeIDs.cyp <- "START n=node(*)  RETURN ID(n)"
  ret <- CypherQry(allNodeIDs.cyp)
  setnames(ret, "NodeID")

  if (retVec)
    return(ret[, NodeID])

  #else
  ret
}


# ------------------------------------------- #
#                                             #
#           RELATIONSHIP FUNCTIONS            #
#                                             #
# ------------------------------------------- #


getAllRelIDsFromNodes <- function(nodes=getAllNodeIDs(retVec=TRUE), BiDirectional=FALSE, retVec=FALSE, force=FALSE) { 
## Returns all relations

  # If asking for lots of nodes, it will take a long time. 
  if (!force && (missing(nodes) || length(nodes>1e3)) )
    confirm <- readline("\n\nThis will take a while, are you sure? [y/n] > ")
  
  if (!force && ! substr(tolower(confirm), 1, 1) =="y") {
    cat("Smart Choice. Will not run a query for all relationships")
    return(invisible(NULL))
  }

  QRY <- paste0("START n=node({startNodes}) MATCH n-[r]-", ifelse(BiDirectional, "", ">"), " friend RETURN ID(r)")
  PARAMS<- list(startNodes=nodes)
  ret <- CypherQry(allRelIDs.cyp, PARAMS)
  setnames(ret, "NodeID")

  if (retVec)
    return(ret[, NodeID])

  #else
  ret
}


deleteRelationship <- function(REL.Number, force=FALSE, verbose=TRUE) { 
# TODO:  Put in more error-chcecking. 
#        Confirm Relationship exists
#        Confirm does not have any relationships
#        Confirm deletion occured (ie, relationship not found)
#
#  Returns TRUE  if deletion occured
#          FALSE if attempted but failed
#          NULL  if user canceletd at confirmation
# 

    if (verbose)
      cat ("Deleting Relationship # ", REL.Number, "\n")   

    # only ask for confirmation if `force` is false
    if (!force) 
      confirm   <- readline("are you sure [y/n]?  > ")


    if (force || substr(tolower(confirm), 1, 1) == "y" ) {
      H.delete  <- try(httpDELETE(as.path(u.rel, REL.Number), httpheader = jsonHeader), silent=TRUE)
      
      if (inherits(H.delete, "try-error")) {
        if (grepl("Error : Not Found", H.delete[[1]])) {
          cat("\tRelationship ", REL.Number, " was not found and hence could not be deleted.\n")
          return(FALSE)
        } else if (grepl("Error : Conflict", H.delete[[1]])) {
          cat("\tRelationship ", REL.Number, " has a conflict (possible existing relationship) and hence could not be deleted.\n")
        } else {
          # else: dont know why it delete failed: 
          cat("This Error: \n\t", H.delete, "\n")
          warning("Relationship ", REL.Number, " could not be deleted for an unknown reason.\n")
          return (FALSE)
        }    
      }

      # relationship was succesfully deleted 
      return(TRUE) 
    }

    # else, user did not select "yes" at confirmation
    if (verbose)
       cat("Relationship-deletion was canceled by user")
    return(NULL)
}

getAllRelIDsFromNodes(force=TRUE, retVec=TRUE)

deleteALL.allowed <- FALSE
deleteAllRelationships <- function()  { 
# this is handy when setting things up. 

  if (!deleteALL.allowed) {
    stop("Delete All Not Allowed.\nPlease change `deleteALL.allowed` to TRUE: \n\n\tdeleteALL.allowed <- TRUE\n\n")
  }

  cat("\n\n\n\nTo continue, you must type:   Yes, I am sure.\n\n")
  confirm <- readline("WARNING!! This will delete ALL REL in the neo4j database. Are you sure? > ")
  
  if (confirm=="Yes, I am sure.") { 
      Rel <- getAllRelIDsFromNodes(force=TRUE, retVec=FALSE)
      
      if (!nrow(Rel)) { 
        warning("No relationships found. Nothing to delete.")
        return(TRUE)
      }

      ret   <- apply(Rel, 1, deleteRelationship, force=TRUE)
      return(all(ret))
  } 

  warning("\nYou did not type `Yes, I am sure.`    <~~  It's gotta be exact.\nNothing deleted.\n\n")
  return(invisible(NULL))
}


createRelationship <- function(start, type, end, props.list, dontParse=FALSE, retList=FALSE) { 
# start / end  should be integers, or integer like.
# props.list should be a list, NOT a JSON string. 

# TODO:  Add more error-check


  # error-check
  if(!is.list(props.list))
    stop("`props.list` should be a list.")

  # identify  START / END  url's
  U.start <- as.path(u.node, start, "relationships")
  U.end   <- as.path(u.node, end)

  # Create nested JSON for POST method
  nestedProperties <- toJSON(list(to=U.end, type=type, data=props.list))

  # create relationship
  H.rel <- httpPOST(U.start, httpheader = jsonHeader, postfields = nestedProperties)


  H.rel <- rawToChar(H.rel)

  # if flagged, return unparsed char
  if (dontParse)
    return(H.rel)

  ret.list <- fromJSON(H.rel)

  # if flagged, return the list of JSON parts
  if (retList)
    return(ret.list)

  # else return the new relationship number
  NodeNumberFromSelf(ret.list)

}





# -------------------------------------- #
#                                        #
#             CYPHER QUERIES             #
#                                        #
# -------------------------------------- #




qu <- function(x) { 
 # wraps quote mark
  # x should be a unit-length vector. All other types, user *ply or other iterations
  paste0("\"", x,  "\"")
}

qu.s <- function(..., sep=" ", collapse = NULL) { 
 # takes all arguments passed, pastes them, then wraps single quotes around the whole argument
 # x should be a unit-length vector. All other types, user *ply or other iterations

 x <- paste(unlist(list(...)), sep=sep, collapse=collapse)
 paste0("'", x,  "'")
}

qu.braced <- function(...) { 
  x <- paste(unlist(list(...)), sep=" ", collapse=", ")
  paste0("{", x,  "}")
}

CypherQry <- function(QRY, PARAMS="", dontParse=FALSE, retDF=TRUE)  {

  if (is.list(PARAMS))
    PARAMS <- toJSON(PARAMS)

  QuotedQRY    <- paste0( qu("query"),  ":",  qu(QRY) )
  QuotedPARAMS <- paste0(qu("params"), ":", ifelse(nchar(PARAMS),  PARAMS, "{}"))
  # buggy:  QuotedPARAMS <- paste0( qu("params"), ":",  ifelse(nchar(PARAMS), qu(PARAMS), "{}"))

 
  properties <- qu.braced(QuotedQRY, QuotedPARAMS)

  H.cyp <- httpPOST(u.cypher, httpheader = jsonHeader, postfields = properties)

  H.cyp <- rawToChar(H.cyp)

  # if flagged, return unparsed char
  if (dontParse)
    return(H.cyp)

  ret.list <- fromJSON(H.cyp)

  # if flagged, don't return DF, just the JSON
  if (!retDF)
    return(ret.list)

  # else, convert to data.table if possible, else to data.frame and return that  

  nms <- ret.list$columns
  DF  <- ret.list$data

  # check if data.table present
  if (exists("data.table")){
    DT <- setnames(data.table(DF), nms) 
    return(DT )
  }

  # else, return data.frame
  return(setNames(as.data.frame(DF), nms))
}




set.seed(1); DT.Rel.Concs.artist.played_at.venue[1e3 + sample(1e5, 10)] -> DTSample.rels
row <- DTSample.rels[1]
props.list <- as.list(row[, 3:4])
names(props.list) <- names(row)[3:4]

# -------------------------- ## -------------------------- ## -------------------------- #

                                ## LOAD UP TO ABOVE THIS LINE 

# -------------------------- ## -------------------------- ## -------------------------- #



# ------------------------------------------ #
#                                            #
#                  EXAMPLES                  #
#                                            #
# ------------------------------------------ #




  
  ## EXAMPLE: 
  # get all nodes
  QRY <- "START n=node(*)  RETURN ID(n)"
  CypherQry(QRY)



lsos(pattern="DT.Nodes")

# sample the data.table
set.seed(1)
DTSample <- DT.Nodes.Concs.artist[sample(5e3, 100)]


  createNodesFromDT(DTSample)



++++++++++++++++++++++++++++++++++++++++++++++++++++++


POST http://localhost:7474/db/data/node
Accept: application/json
Content-Type: application/json
{
  "foo" : "bar"
}


curl.opts <- list(
      httpheader = c('Content-Type' = 'application/json', Accept = 'application/json')
    , postfields = egJ
  )

U <- "http://localhost:7474/db/data/node"

httpPOST(u.node, .opts=curl.opts)

H.post <- httpPOST(u.node, httpheader = c('Content-Type' = 'application/json', Accept = 'application/json'), postfields = egJ)
H.put  <- httpPUT (u.node, httpheader = c('Content-Type' = 'application/json', Accept = 'application/json'), postfields = egJ)

H.delete  <- httpDELETE(as.path(u.node, 11), httpheader = c('Content-Type' = 'application/json', Accept = 'application/json'))


, postfields = egJ)




H2 <- httpPOST(u.node, 'Content-Type' = 'application/json', Accept = 'application/json', postfields = egJ)


charToRaw

postForm(U, .opts = curl.opts)

POST http://localhost:7474/db/data/node
Accept: application/json
Content-Type: application/json




------

curl -k -u myusername:mypassword -d '{"text":"Hello World!","level":"Noob"}' -H "Content-Type: application/json" -H "Accept: application/json" "http://api.website/v1/access?"

handle <- getCurlHandle()

postForm(u.node,
         .opts = list(postfields = toJSON(list(text = "Hello World!", level = "Noob")),
                      httpheader = c('Content-Type' = 'application/json', Accept = 'application/json')
         ) 


curl.opts <- list(userpwd = "username:password", 
                  httpheader = "Content-Type: application/json",
                  httpheader = "Accept: application/json",
                  timeout = 20, 
                  connecttimeout = 20, 
                  verbose = TRUE, 
                  useragent = "RCurl",
                  cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))

postForm("http://api.theysay.io:60000/v1/sentiment?", .params = c(data = '{\\"text\\":\\"Have a nice day!\\"}'), .opts = curl.opts)
This



httpPOST(as.path(u.node, 1))

httpPOST(as.path(u.node, 1), content=egJ)

Usage

httpPUT(url, content,  ..., curl = getCurlHandle())
httpPOST(url, ..., curl = getCurlHandle())
httpDELETE(url, ..., curl = getCurlHandle())
httpGET(url, ..., curl = getCurlHandle())
httpHEAD(url, ..., curl = getCurlHandle())
httpOPTIONS(url, ..., curl = getCurlHandle())
Arguments

url 
the URL of the server to which the HTTP request is to be made

content 
the value that is to be used as the content of the PUT request. This can be a character or a raw object.

... 
additional arguments passed to getURLContent

curl  
the curl handle to be used to make the request

Value

The content returned by the server as a result of the request.





fromJSON(getURL("http://localhost:7474/db/data/"))


getURL(egCall)
