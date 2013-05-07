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

deleteNode <- function(NODE.Number, force=FALSE) { 
# TODO:  Put in more error-chcecking. 
#        Confirm Node exists
#        Confirm does not have any relationships
#        Confirm deletion occured (ie, node not found)
#
#  Returns TRUE  if deletion occured
#          FALSE if attempted but failed
#          NULL  if user canceletd at confirmation
# ---------------------------------------------- #


    cat ("\n\nDeleting Node # ", NODE.Number, "\n")   

    # only ask for confirmation if `force` is false
    if (!force) 
      confirm   <- readline("are you sure [y/n]?  > ")


    if (force || substr(tolower(confirm), 1, 1) == "y" ) {
      H.delete  <- try(httpDELETE(as.path(u.node, NODE.Number), httpheader = jsonHeader), silent=TRUE)
      
      if (inherits(H.delete, "try-error")) {
        if (grepl("Error : Not Found", H.delete[[1]])) {
          warning("Node ", NODE.Number, " was not found and hence could not be deleted.")
          return(FALSE)
        }
        # else: dont know why it delete failed: 
        warning("Node ", NODE.Number, " could not be deleted for an unknown reason.")
        return (FALSE)    
      }

      # node was succesfully deleted 
      return(TRUE) 
    }

    # else, user did not select "yes" at confirmation
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
  H.post <- httpPUT(U, httpheader = jsonHeader, postfields = properties)
  H.post <- rawToChar(H.post)

  # if H.post is a blank string, return that
  if (!nchar(H.post))
    return(H.post)

  if (retJSON)
    return(fromJSON(H.post))
  return(H.post)
}


}


# ------------------------------------------ #
#                                            #
#                  EXAMPLES                  #
#                                            #
# ------------------------------------------ #


JU(as.path(u.node, 11))$data

getNode(11)$data


egJ <- toJSON(DT.Nodes.Concs.artist[4e3])

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
