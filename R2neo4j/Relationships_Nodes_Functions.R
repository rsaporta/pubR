# Functions to Create Nodes & Relationships CSVs to import into neo4j

# -------------------------------------------------- #
#  Depends: data.table                               #
#           fw0(), gapply(), pasteNoBlanks()         #
#                                                    #
# -------------------------------------------------- #

  # --------------------------------------------------------------------------------------------------- #
  #                                                                                                     #
      ## Lad Utils File For Dependency functions
      library(RCurl)
      utilsFile <- c("https://raw.github.com/rsaporta/pubR/gitbranch/utilsRS_pub.r")
      eval(parse(text=getURL(utilsFile, followlocation=TRUE, 
                 cainfo=system.file("CurlSSL", "cacert.pem", package="RCurl"))), envir=environment())  
  #                                                                                                     #
  # --------------------------------------------------------------------------------------------------- #




# -------------------------------------------------------- #
#                                                          #
#       Relation between two items in a SINGLE ROW         #
#                                                          #
# -------------------------------------------------------- #

getRelationsFromRows <- function(sourceDT, start.col, relation, end.col, include=list()
                                , additional=list(), sourceGrp=character(), keep.names=FALSE
                                , insertSourceAsCol=nchar(sourceGrp), pos=1, verbose=FALSE) {

  ## The results will be assigned into a var in the parent environment

  # Determination of whether we're inserting a column for the source of the data
  #   is based on if `sourceGrp` is given and `insertSourceAsCol` is TRUE 
  if (length(sourceGrp) && insertSourceAsCol)
    additional <- c(additional, "source"=sourceGrp)
    

  ## Create the name of the table where the results will be stored
  ##        eg:   DT.rel.artist.is_in.city
  DT.nm <- mkDTRelName(start.col, relation, end.col, sourceGrp)

  # Put into a single vector, all of the columns from the sourceDT that will be used in the relationship
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

  completeRelsDT( DT=get(DT.nm, envir=parent.frame(pos)) 
                 , start.col=start.col, end.col=end.col, sourceGrp=sourceGrp
                 , keep.names=keep.names, additional=additional) 

  if (verbose) 
    msgCreatedTable(DT.nm)

  # All good! 
  return(invisible(TRUE))

}
# ------------------------------------------------------- #


# -------------------------------------------------------- #
#                                                          #
#      Relation between two items in a SINGLE COLUMN       #
#                                                          #
# -------------------------------------------------------- #

getRelationsWithinColumnByKey <- function(sourceDT, start.col, relation, by=list(), additional=list()
                                      , sourceGrp=character(), selfRelate=FALSE, keep.names=FALSE
                                      , insertSourceAsCol=nchar(sourceGrp), pos=1, verbose=FALSE) {
  
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


  completeRelsDT( DT=get(DT.nm, envir=parent.frame(pos)) 
                 , start.col=start.col, end.col=end.col, sourceGrp=sourceGrp
                 , keep.names=keep.names, additional=additional) 

  if (verbose) 
    msgCreatedTable(DT.nm)

  # All good! 
  return(invisible(TRUE))

}

# ------------------------------------------------------- #



completeRelsDT <- function(DT, start.col, end.col, sourceGrp, keep.names=FALSE, additional=NULL) { 
## Once each relations data.table (regardless if column-wise or row-wise) is created, this function will 
##    then call the look-up-nodes funciton and then clean up the rest of the table

  if (is.character(DT))  
    DT <- get(DT)

  if (!is.data.table(DT))
    stop("`DT` should be a data.table or the name of a data.table")

  # Nodes: Add start & end nodes table and assign it to the name contained in DT.nm
  getStartEndNodes(DT, start.col, end.col, sourceGrp=sourceGrp)

  # add node type, ie, "relation"
  DT[, type := relation ]

  # now with nodes added, drop the original start & end columns. (If flagged)
  if (!keep.names)
    DT[ , c(start.col, end.col) := NULL]

  # add additional columns
  if (length(additional))
    DT[, c(names(additional)) := additional]

  # set up new colorder.                                          # `by` here can be replaced with `include` in other func.
  colorder <- c("start", {if(keep.names) start.col}, "end", {if(keep.names) end.col}, "type", by, names(additional) )
  colorder <- intersect(colorder, names(DT))
  setcolorder(DT, colorder)

  # All good! 
  rm(DT)
  return(invisible(TRUE))

}




# -------------------------------------------------------- #
#                                                          #
#    Find the Node # from the respective Nodes tables      #
#                                                          #
# -------------------------------------------------------- #

# -------------------------------------------------------- #
getStartEndNodes <- function(DT, start.col, end.col, sourceGrp, selfRelate=FALSE, base="DT.Nodes")  {

  # some incoming DT's have been modified and might have column names different
  #   from what would be expected in typical  DT.source.XXXXX format. 
  # For these, apply a `names()` to start.col / end.col
  #   eg:   ..., start.col=c("artist"="start.nm"), end.col=c("artist"="end.nm"), ...
  # 
  # Here we check for names. If present, we use those.  

  start.name <- ifelse( is.null(names(start.col)),  start.col, names(start.col) )
  end.name   <- ifelse( is.null(names(end.col)),    end.col,   names(end.col)   )  

  start.DT.nm <- paste0(paste(base, sourceGrp, sep="."), ".", start.name) # the inner `paste` is to allow for an empty `sourceGrp` without causing extra dot
  end.DT.nm   <- paste0(paste(base, sourceGrp, sep="."), ".", end.name  ) # the inner `paste` is to allow for an empty `sourceGrp` without causing extra dot

  # Add node id's
  setkeyv(DT, start.col)
  DT[get(start.DT.nm), start:=node]

  setkeyv(DT, end.col)
  DT[get(end.DT.nm), end:=node]

  # Error Check:  If we don't allow for selfRelate, then `DT[start==end]` should have zero rows. 
  if(!selfRelate && !nrow(DT[start==end]) == 0)
    warning ("Something may have gone wrong. Some start-nodes and end-nodes are the same.")
}
# ------------------------------------------------------- #

msgCreatedTable <- function(DT.nm) { 
      cat("\nCreated DT:\n\t", DT.nm, "\n\n")  
}

combineRelDTs <- function(RELS.DT.Names, verbose=FALSE) {
## This function rbind's a collection of relationship DT's.  
##
## More specifically, it takes as input a vector (or list) of DT names
##   pre-processes each DT, then flattens with `rbindlist`
##

  # The final RELS DT will have one column for each property. 
  #   If a specific type of relationship does not have a given property, 
  #   then that column will be blank for the row(s) corresponding to that property
  # Therefore, we grab all the (unique) column names of all the tables. 
  allNames  <- lapply(RELS.DT.Names, function(x) names(get(x, envir=parent.frame())[1]))    
  allNames  <- unique(unlist(allNames))

  # We then order the values of `allNames` according to the order which the final output will have
  # column ordering. Certain columns come to the front. Then the rest simply organized
  firstCols <- c("start", "end", "type", "source", "id", "WRONG") # note, `WRONG` is included just to confirm that it is dropped in the next line. And id will be included only if present

  # grab the intersection of all names (ie, names present) with firstCols and then `setdiff` gives us all of the `allNames` that remain
  allNames  <- c(intersect(firstCols, allNames),  setdiff(allNames, firstCols))

  # Iterate over each table, cleaning up column classes and padding empty properties
  for (i in seq(RELS.DT.Names) ) {

    DTR <- get(RELS.DT.Names[[i]], envir=parent.frame())

    # identify which columns will need to be created and padded with blanks
    missingCols <- setdiff(allNames, names(DTR))

    # convert most columns to characters (except numerics)
    convertColumnsToCharacter(DTR, "nonNumeric")

    # if padded is needing, do it
    if(length(missingCols))
       DTR[, c(missingCols) := ""]

    # reorder according to the ordering in `allNames`
    setcolorder(DTR, allNames)
  }

  # clean up the reference
  rm(DTR)

  # collapse it: (if not verbose, just return, else, collapse it and give feedback on it) 

  if (! verbose)
    return(rbindlist(lapply(RELS.DT.Names, get, envir=parent.frame())))


  # else
  nice <- function(x) (format(x, big.mark=","))
  ret <- rbindlist(lapply(RELS.DT.Names, get, envir=parent.frame()))
  counts <- ret[, list(Rels=nice(.N)), by=type]
  cat("Relationships table created with a total of ", nice(nrow(ret)), "edges represented.\nBreakdown is as follows:\n\n")
  print(counts)

  return(ret)
}



#  This function replaced by  `convertColumnsToCharacter`
# ----------------------------------------------------- #
# %   allColumnsToCharacter <- function(DT) { 
# %     DT <- RELS.DT.as.list[[i]]
# %     
# %     # identify, by name, the columns that are not characters
# %     nonCharCols <- DT[1, names(.SD)[!sapply(.SD, is.character)]]
# %   
# %     DT[, c(nonCharCols) := lapply(.SD, as.character), .SDcols=nonCharCols]
# %   
# %     return(invisible(TRUE))
# %   }


convertColumnsToCharacter <- function(DT, convert=c("all", "factors", "numerics", "nonNumeric", "nonNumbers")) { 
## No Value returned.  Changes in place. 

  # Determine which function to use to identify the columns to convert.  
  #  This is based on what the user submited to convert 
  choices <- c("all", "factors", "numerics", "nonNumeric", "nonNumbers")
  convert <- match.arg(convert, choices)
  FUNC <- {
    if (convert == "all")
        Negate(is.character)
    else if (convert == "factors")
        is.factor
    else if (convert == "numerics")
        is.numeric
    else if (convert == "nonNumeric" ||  convert == "nonNumbers")
        function(x) {!(is.numeric(x) || is.character(x))}
    else 
      stop ("Somethings wrong with the use of `match.arg.` This line should never be reached")
  }

  # identify, by name, the columns that are not characters
  ColsToConvert <- DT[1, names(.SD)[sapply(.SD, FUNC)]]

  # Only try to convert if there are columns to convert
  if (length(ColsToConvert))
    DT[, c(ColsToConvert) := lapply(.SD, as.character), .SDcols=ColsToConvert]

  return(invisible(TRUE))
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





