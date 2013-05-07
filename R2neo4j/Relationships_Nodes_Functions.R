# Functions to Create Nodes & Relationships CSVs to import into neo4j

# -------------------------------------------------- #
#  Depends: data.table                               #
#           fw0(), gapply(), pasteNoBlanks()         #
#                                                    #
# -------------------------------------------------- #





# -------------------------------------------------------- #
#                                                          #
#       Relation between two items in a SINGLE ROW         #
#                                                          #
# -------------------------------------------------------- #

getRelationsFromRows <- function(sourceDT, start.col, relation, end.col, include=list()
                                , additional=list(), sourceGrp=character(), keep.s.e=FALSE
                                , insertSourceAsCol=nchar(sourceGrp), pos=1) {

  if (length(sourceGrp) && insertSourceAsCol)
    additional <- c(additional, "source"=sourceGrp)
    

  DT.nm <- mkDTRelName(start.col, relation, end.col, sourceGrp)
#  DT.nm <- paste("DT.rel", start.col, relation, end.col, sep=".") 
  cols  <- unlist(c(start.col, end.col, include))
  cols  <- cols[!is.na(cols) & cols != ""]

  # -------------------------------------------------- #
  #   error check
  # -------------------------------------------------- #
    if (length(missing <- setdiff(cols, names(sourceDT))))
      stop("The following columns are not in the sourceDT: \n\t", paste(missing, collapse=", "), "\n")

    if (length(additional) && (!is.list(additional) || is.null(names(additional)) || any(names(additional)=="")))
      stop("`additional` should be a named list.\n  eg:\n      additional = list( Misc = 'Hello',   Avogadro = 6.023e23 )\n\n")
  # -------------------------------------------------- #

  # Create the table and assign it to the name contained in DT.nm
  assign(DT.nm, copy( sourceDT[, .SD, .SDcols=cols] ), envir=parent.frame(pos))


  # ----------------     FROM HERE ON DOWN, THE TWO FUNCTIONS ARE IDENTICAL.     ---------------- #


  # grab a handle on the new DT (ease of coding to avoid repetitive `get(DT.nm)`)
  DT <- get(DT.nm)

  # Nodes: Add start & end nodes table and assign it to the name contained in DT.nm
  getStartEndNodes(DT, start.col, end.col, sourceGrp=sourceGrp)

  # add relation type
  DT[, type := relation ]


  # now with nodes added, drop the original start & end columns. (If flagged)
  if (!keep.s.e)
    DT[ , c(start.col, end.col) := NULL]

  # add additional columns
  if (length(additional))
    DT[, c(names(additional)) := additional]

  # set up new colorder.                                          # `by` here can be replaced with `include` in other func.
  colorder <- c("start", {if(keep.s.e) start.col}, "end", {if(keep.s.e) end.col}, "type", by, names(additional) )
  colorder <- intersect(colorder, names(DT))
  setcolorder(DT, colorder)

  # All good! 
  rm(DT)
  return(invisible(TRUE))
}
# ------------------------------------------------------- #


# -------------------------------------------------------- #
#                                                          #
#      Relation between two items in a SINGLE COLUMN       #
#                                                          #
# -------------------------------------------------------- #

getRelationsWithinColumnByKey <- function(sourceDT, start.col, relation, by=list(), additional=list()
                                      , sourceGrp=character(), selfRelate=FALSE, keep.s.e=FALSE
                                      , insertSourceAsCol=nchar(sourceGrp), pos=1) {
  
  if (length(sourceGrp) && insertSourceAsCol)
    additional <- c(additional, "source"=sourceGrp)

  DT.nm <- mkDTRelName(start.col, relation, start.col, sourceGrp)
  # DT.nm <- paste("DT.rel", start.col, relation, start.col, sep=".") 
  cols  <- unlist(c(start.col, end.col, by))
  cols  <- cols[!is.na(cols) & cols != ""]

  # -------------------------------------------------- #
  #   error check
  # -------------------------------------------------- #
    if (length(missing <- setdiff(cols, names(sourceDT))))
      stop("The following columns are not in the sourceDT: \n\t", paste(missing, collapse=", "), "\n")

    if (length(additional) && (!is.list(additional) || is.null(names(additional)) || any(names(additional)=="")))
      stop("`additional` should be a named list.\n  eg:\n      additional = list( Misc = 'Hello',   Avogadro = 6.023e23 )\n\n")
  # -------------------------------------------------- #

  # Create the table and assign it to the name contained in DT.nm
  assign(DT.nm, envir=parent.frame(pos), val=copy( 
    sourceDT[, c(.SD,N=.N), by=by ][ N > 1, {
        e.g <- expand.grid(start=unique(get(start.col)), end=unique(get(start.col)))

        # if flagged that the column can NOT self relate, then set the index to only those 
        #   values were start is not same as end.  Else set index to TRUE (ie, all values)
        indx <- if (!selfRelate)  e.g$start != e.g$end else TRUE

        list(start.nm=as.character(e.g$start[indx]), end.nm=as.character(e.g$end[indx])) 
       }, by=by]
   ))
  start.col <- "start.nm"
  end.col <- "end.nm"


  # ----------------     FROM HERE ON DOWN, THE TWO FUNCTIONS ARE IDENTICAL.     ---------------- #
  # ----------------           assuming we let  `include <- by`                  ---------------- #


  # grab a handle on the new DT (ease of coding to avoid repetitive `get(DT.nm)`)
  DT <- get(DT.nm)

  # Nodes: Add start & end nodes table and assign it to the name contained in DT.nm
  getStartEndNodes(DT, c(artist=start.col), c(artist=end.col), sourceGrp=sourceGrp, selfRelate=selfRelate)

  # add relation type
  DT[, type := relation ]

  # now with nodes added, drop the original start & end columns. (If flagged)
  if (!keep.s.e)
    DT[ , c(start.col, end.col) := NULL]

  # add additional columns
  if (length(additional))
    DT[, c(names(additional)) := additional]


  # set up new colorder.                                          
  colorder <- c("start", {if(keep.s.e) start.col}, "end", {if(keep.s.e) end.col}, "type", by, names(additional) )
  colorder <- intersect(colorder, names(DT))
  setcolorder(DT, colorder)

  # All good! 
  rm(DT)
  return(invisible(TRUE))
}

# ------------------------------------------------------- #


# -------------------------------------------------------- #
#                                                          #
#    Find the Node # from the respective Nodes tables      #
#                                                          #
# -------------------------------------------------------- #

# -------------------------------------------------------- #
getStartEndNodes <- function(DT, start.col, end.col, sourceGrp, selfRelate=FALSE)  {

  # some incoming DT's have been modified and might have column names different
  #   from what would be expected in typical  DT.source.XXXXX format. 
  # For these, apply a `names()` to start.col / end.col
  #   eg:   ..., start.col=c("artist"="start.nm"), end.col=c("artist"="end.nm"), ...
  # 
  # Here we check for names. If present, we use those.  

  start.name <- ifelse( is.null(names(start.col)),  start.col, names(start.col) )
  end.name   <- ifelse( is.null(names(end.col)),    end.col,   names(end.col)   )  

  start.DT.nm <- paste0(paste("DT", sourceGrp, sep="."), ".", start.name) # the inner `paste` is to allow for an empty `sourceGrp` without causing extra dot
  end.DT.nm   <- paste0(paste("DT", sourceGrp, sep="."), ".", end.name  ) # the inner `paste` is to allow for an empty `sourceGrp` without causing extra dot

  # Add node id's
  setkeyv(DT, start.col)
  DT[get(start.DT.nm), start:=node]

  setkeyv(DT, end.col)
  DT[get(end.DT.nm), end:=node]

  # Error Check
  if(!selfRelate && nrow(DT[start==end]) == 0)
    warning ("Something may have gone wrong. Some start-nodes and end-nodes are the same.")
}
# ------------------------------------------------------- #


# all rels must be a single file. If they do not have a property, that column should be blank.
combineRelDTs <- function(RELS.DT.Names) {
 
  RELS.DT.as.list <- lapply(RELS.DT.Names, get)    
  
  allNames  <- lapply(RELS.DT.as.list, names)
  allNames  <- unique(unlist(allNames))

  # ordering
  firstCols <- c("start", "end", "type", "source", "WRONG")
  allNames  <- c(intersect(firstCols, allNames),  setdiff(allNames, firstCols))

  # create it in each table
  for (i in seq(RELS.DT.as.list) ) {
    missingCols <- setdiff(allNames, names(RELS.DT.as.list[[i]]))
    RELS.DT.as.list[[i]] <- RELS.DT.as.list[[i]][, lapply(.SD, as.character)]
    if(length(missingCols))
       RELS.DT.as.list[[i]][, c(missingCols) := ""]
    setcolorder(RELS.DT.as.list[[i]], allNames)
  }

  rbindlist(RELS.DT.as.list)
}


# ---------------------------------------------------- #
#                                                      #
#              CREATE NODES FROM SOURCEDT              #
#                                                      #
# ---------------------------------------------------- #

  createAllNodes <- function(sourceDT,  NODE.TYPES, ID.prfx, ID.digs, sourceGrp=character(), insertSourceAsCol=nchar(sourceGrp), pos=1  ) {

    if (length(sourceGrp) && insertSourceAsCol)
      insertSourceAsCol <- TRUE

  
    for (nt.nm in names(NODE.TYPES)) {
  
      # current node tracks the node index for the whole environment
      # TODO check NODES table for existince of current node. 
      currentNode <- 
        ifelse(exists(".currentNode", envir=parent.frame(pos)), 
                  get(".currentNode", envir=parent.frame(pos)),  0) 

      NameOfDT <- mkDTNodesName( NODE.TYPES[nt.nm], sourceGrp )    
      tmp.DT   <- setkey(sourceDT[, list(name=as.character(unique(get(nt.nm))))], "name") 

      tmp.DT[ , id     := paste0(ID.prfx[nt.nm], fw0(1:.N, ID.digs[nt.nm])) ]
      tmp.DT[ , type   := NODE.TYPES[nt.nm] ]
      tmp.DT[ , node   := currentNode + (1:.N) ]

      if (insertSourceAsCol)
       tmp.DT[ , sourceGrp := sourceGrp ] 

      setcolorder(tmp.DT, c("node", "type", "name", "id", {if (insertSourceAsCol) "sourceGrp"}))


      # Create data.table
      assign(NameOfDT,  copy(tmp.DT), envir=parent.frame(pos))

      # success
      if (identical( tmp.DT,  get(NameOfDT, envir=parent.frame(pos)) )) {
        # update .currentNode every time DT created succesfully 
        assign(".currentNode", (currentNode + nrow(tmp.DT)), envir=parent.frame(pos)) 
        cat("currentNode is ", currentNode, " -- While .currentNode is ", .currentNode, "\n")
      } else {
        warning (NameOfDT, " was *NOT* created succesfully")
      }

      rm(tmp.DT)
    } # //End for loop
  }


# ---------------------------------------------------- #

  mkDTRelName <- function(start.col, relation, end.col, sourceGrp=character(), silent=FALSE) { 
    if (!silent && !length(sourceGrp))
      warning("No Source Specified for `mlDTName`")
    paste("DT", "Rel", sourceGrp, start.col, relation, end.col, sep=".") 
  }

  mkDTNodesName <- function(ntype, sourceGrp=character(), silent=FALSE) { 
    if (!silent && !length(sourceGrp))
      warning("No Source Specified for `mlDTName`")
    pasteNoBlanks("DT", "Nodes", sourceGrp, ntype, sep=".")
  }
# ---------------------------------------------------- #





