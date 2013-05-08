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

##  SAMPLE TABLES
#
#  NodesDT 
#
#         node   type             name        id sourceGrp
#        44154 artist   Theodis Easley ART044154     Concs
#         7553 artist Chelsea Williams ART007553     Concs
#        18414 artist      Howard Ghee ART018414     Concs
#        40392 artist    The Balconies ART040392     Concs
#        21770 artist    Junior League ART021770     Concs 
#
#      
#  RelsDT
#   (note that every value of start & end corresponds to a 'node' in NodesDT) 
#
#       start   end        type source concertID concertDate rel.id
#       21770 44154   played_at  Concs     18011  2002-01-25      1
#       21770 40392 played_with  Concs     86843  2006-08-29      2
#       21770  7553 played_with  Concs     30094  2003-05-24      3
#       40392 40392   played_at  Concs     58171  2004-11-13      4
#        7553 18414       is_in  Concs                            5
#       21770 40392 played_with  Concs     26415  2002-12-14      6
#       21770 21770   played_at  Concs    126718  2009-03-28      7
#       21770  7553   played_at  Concs     31761  2003-08-01      8
#        7553 40392   played_at  Concs    141813  2010-03-27      9
#        7553 21770   played_at  Concs    141799  2010-04-10     10
#       21770  7553 played_with  Concs     33195          NA     11
#        7553 40392   played_at  Concs    133523  2009-04-17     12



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





