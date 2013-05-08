## ---------------------------------------------------------------------------- #
##               THIS REQUIRES TWO DATA.TABLES (OR DATA.FRAMES)
## ---------------------------------------------------------------------------- #
##  NodesDT -  table of Nodes + properties
##  RelsDT  -  table of Edges + properties
##
##  The NodesDT should have a column named "node" which will 
##  serve as a (temporary!)* unique identifier for that node. 
##  
##  In the RelsDT table, the 'start' and 'end' values should
##  correspond to the 'node' value in NodesDT
##  
##  *why temporary:  This 'node' value is used only for importing. 
##                    and may be subject to change. 
##   Note that the NodesDT can have another column which is a proper node id. 
## ---------------------------------------------------------------------------- #



batchCreateNodesAndRels <- function(NodesDT, RelsDT) { 
# TODO:  Explore passing the name of the DT. Will this save efficiency (memory or time)?

  content.nodes <- batchMethodsForNodes(NodesDT)
  content.rels  <- batchMethodsForRels(RelsDT)

  # note:  collapse if c(..), else use sep
  content <- paste0("[", paste(c(content.nodes, content.rels), collapse=", "), "]" )

  H.post  <- getURL(u.batch, httpheader = jsonHeader, postfields = content)

  # incase user forgot to assign the output to an object, we dont want the handle to just dissappear. 
  if (saveToGlobal) {
    saveTo <- paste0("LastBatchCreate.", what)
    assign(saveTo, H.post, envir=.GlobalEnv)
    cat("Neo4j response saved to\n  `", saveTo, "`\n", sep="")
  }

  return(H.post)

}




batchMethodsForNodes <- function(DT, idcol="node") {
## TODO: have a sepearte method for data.frame / data.table.  Different syntax
## TODO:  Error check the arguments and whats expected

  # allows for names of the DT to be passed instead of the whole object
  if (is.character(DT))
    DT <- get(DT)


  # confirm idcol is in the names of DT. 
  if(any(idcol==names(DT))) {
      useID <- TRUE
      idcol.idx <- which(idcol==names(DT))
  } else {
     stop("Couldn't find idcol, '", idcol, "' amongst the names of DT")
  }

  # return
  apply(DT.arts, 1, function(x) 
            toJSON(list(method="POST", to="/node", 
               body=as.list(x[-idcol.idx]), id=as.numeric(x[idcol.idx])  ))   )
}


batchMethodsForRels <- function(DT)  {

  # allows for names of the DT to be passed instead of the whole object
  if (is.character(DT))
    DT <- get(DT)

  dataCols <- setdiff(names(DT), c("start", "end", "rel.id", "type"))

  contentForBatch <- function(x, dataCols) {
  # applying to each row of x
  toJSON(list(
    method = "POST" , 
    to =  paste0("{", as.integer(x[["start"]]),  "}/relationships") , 
    id =  as.integer(x[["rel.id"]] ), 
    body = list(
      to = paste0("{", as.integer(x[["end"]]),  "}"),
      data = x[dataCols], 
      type = x[["type"]]
      )
    ))
  }

  # return
  apply(DT, 1, contentForBatch, dataCols)
}





