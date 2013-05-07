#  R2neo4j.r

library(RCurl)
library(rjson)


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


# ------------------------------------------ #
#                                            #
#                   URL's                    #
#                                            #
# ------------------------------------------ #

u.base   <- "http://localhost:7474/db/data/"
u.cypher <- as.path(u.base, "cypher")
u.node   <- as.path(u.base, "node")


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

H1 <- httpPOST(u.node, httpheader = c('Content-Type' = 'application/json', Accept = 'application/json'), postfields = egJ)

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
