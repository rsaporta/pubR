#  BulkLoadVia_R2neo4j.R

## approx 7 seconds per 1,000

  Artists.NodeInputs <- createNodesFromDT ( DT.Nodes.Concs.artist) 
  DT.Nodes.Concs.artist.Loaded <- cbind(DT.Nodes.Concs.artist, Artists.NodeInputs)
  



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


u.batch <- as.path(u.base, "batch")

POST http://localhost:7474/db/data/batch
Accept: application/json
Content-Type: application/json
[ {
  "method" : "PUT",
  "to" : "/node/41/properties",
  "body" : {
    "age" : 1
  },
  "id" : 0
}, {
  "method" : "GET",
  "to" : "/node/41",
  "id" : 1
}, {
  "method" : "POST",
  "to" : "/node",
  "body" : {
    "age" : 1
  },
  "id" : 2
}, {
  "method" : "POST",
  "to" : "/node",
  "body" : {
    "age" : 1
  },
  "id" : 3
} ]


[ 
  {
    "method":"POST","
     to":"/relationship",
     "body": 
      {
        "type":"artist","name":"Afortiori","id":"ART001328","sourceGrp":"Concs"},"id": 
      {
        "node":"1328"}},  
      {
        "method":"POST","to":"/relationship","body": 
      {
        "type":"artist","name":"Almost There","id":"ART001861","sourceGrp":"Concs"},"id": 
      {
        "node":"1861"}},  
      {
        "method":"POST","to":"/relationship","body": 
      {
        "type":"artist","name":"Arrulo","id":"ART002864","sourceGrp":"Concs"},"id": 
      {
        "node":"2864"}},  
      {
        "method":"POST","to":"/relationship","body": 
      {
        "type":"artist","name":"Big Sound","id":"ART004539","sourceGrp":"Concs"},"id": 
      {
        "node":"4539"}},  
      {
        "method":"POST","to":"/relationship","body": 
      {
        "type":"artist","name":"Aap Ka Sarror","id":"ART001008","sourceGrp":"Concs"},"id": 
      {
        "node":"1008"}}]


# COPY DT
DT.arts <- copy(DT)

# we DO want the node column. This wil server as JOB_ID
DT.arts[, toJSON(list(.SD)), by=1:nrow(DT.arts)]$V1

createFromDT <- function(DT, what=c("nodes", "relationships"), idcol="node", saveToGlobal=TRUE) {
## TODO: have a sepearte method for data.frame / data.table.  Different syntax

  # confirm idcol is in the names of DT. 
  if(any(idcol==names(DT))) {
    useID <- TRUE
    idcol.idx <- which(idcol==names(DT))
  } else {
      if (!missing(idcol))
        warning("Couldn't find idcol, '", idcol, "' amongst the names of DT")
  }

  # `what` should be either "nodes" or "relationships" (or a shorter form of either)
  what <- match.arg(what)

  method <- "POST"
  to     <- paste0("/", substr(what, 1, nchar(what)-1))
  
  # This creates the body:  body   <- apply(DT.arts, 1, as.list)
  #  we will combine with `method` & `to`, above, into one shot. 

  # repeated lines, but faster code 
  if (useID) {
    batchCall <- apply(DT.arts, 1, function(x) 
                   toJSON(list(method=method, to=to, body=as.list(x[-idcol.idx]), id=as.numeric(x[idcol.idx])  ))   )
  } else {
    batchCall <- apply(DT.arts, 1, function(x) 
                 toJSON(list(method=method, to=to, body=as.list(x)))   )
  }

  content <- paste0("[", paste(batchCall, collapse=", "), "]" )
  H.post  <- getURL(u.batch, httpheader = jsonHeader, postfields = content)

  # incase user forgot to assign the output to an object, we dont want the handle to just dissappear. 
  if (saveToGlobal) {
    saveTo <- paste0("LastBatchCreate.", what)
    assign(saveTo, H.post, envir=.GlobalEnv)
    cat("Neo4j response saved to\n  `", saveTo, "`\n", sep="")
  }

  return(H.post.JSON)
}




  cat(H.post)

  cat(content)


POST http://localhost:7474/db/data/batch
Accept: application/json
Content-Type: application/json



}

createFromDT(DT, "node")
createFromDT(DT, "rel")
createFromDT(DT, "n")






