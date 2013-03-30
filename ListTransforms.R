#------------------------------------------------------#
#    THESE ARE THE FUNCTIONS PRESENT IN THIS FILE      #
#------------------------------------------------------#
#------------------------------------------------------#
 #  depth ( x, counter=0 ) 
 #  listStr ( obj, showValues=TRUE ) 
 #  longestLength ( obj, currentMax=0 ) 
 #  listFlatten ( obj, filler=NA ) 
 #  tableFlatten ( tableWithLists, filler="" ) 
 #  insertListAsCols ( input, target, targetCols, replaceOriginalTargetCol=FALSE, preserveNames=TRUE ) 
 #  insertListAsCols.list ( input, target, targetCols, replaceOriginalTargetCol=FALSE, preserveNames=TRUE ) 
 #  findGroupRanges ( booleanVec ) 
 #  nestedIndx ( obj, pre=NULL, expandLast=FALSE, asDF=FALSE ) 

###############################################################
###########              LIST UTILITIES             ###########   
###############################################################    

depth <- function(x, counter=0) {
  # Returns the depth of a list-like object.  
  # Vectors are considered to have depth 0
  # an un-nested list has depth 1  
  ifelse (!is.list(x), counter, max(sapply(x, depth, counter+1)))  
}

#--------------------------------------------

listStr <- function(obj, showValues=TRUE)  {
  # A cleaner way to view the structure of a list. 
  #   (ie, by index, instead of by indentation)
  # returns a data frame indicating the structure of a nested list
  # Optionally returns values at end of list
  # 
  # Args: 
  #        obj: a list-like object whose structure is to be determined
  # showValues: FALSE:  values of `obj` are not returned (only indices)
  #                -1:  values are returned in column 1
  #              TRUE:  values are returned in final column 

  inds <- nestedIndx(obj, expandLast=showValues)
  
  # flag of -1 indicates to put values first in the df.
  if (showValues==-1)  {
    print("TRUE")
    inds <- data.frame(cbind(value=apply(inds, 1, function(ind) obj[[ind[!is.na(ind)]]]), inds))
  
  # flag of 1/T indicates to put values last in the df
  } else if (showValues)  {
    inds <- data.frame(cbind(inds, value=apply(inds, 1, function(ind) obj[[ind[!is.na(ind)]]])))
  }   

  # flag of F indicates no values in the df

  return(data.frame(inds))
}

#--------------------------------------------

longestLength <- function(obj, currentMax=0)  {
  ## returns the length of the longest row or longest list in obj
  # obj should be list-like or matrix-like

  # If we're at a vector, return the max between its length and running ma
  if (is.vector(obj))  # (!is.list(obj) && is.null(dim(obj))) 
    return(max(currentMax, length(obj)))

  # If obj is array or matrix, find the max of each row
  if (!is.null(dim(obj)))
    return(max(currentMax, apply(obj, 1, longestLength, currentMax=currentMax)))

  # If obj is list, find max within each element
  if (is.list(obj))
    return(max(currentMax, sapply(obj, longestLength, currentMax=currentMax)))

  stop("Uknown Object Type")
}

#--------------------------------------------


listFlatten <- function(obj, filler=NA) {
## Flattens obj like rbind, but if elements are of different length, plugs in value filler
## DEPENDS ON: insertListAsCols (and insertListAsCols.list )

  # if obj is a list of all single elements, pop them up one level
  if (is.list(obj) && all(sapply(obj, length) == 1)) {
    obj <- sapply(obj, function(x) x)   ## TODO:  Double check that this does not need to be transposed.  Or perhaps use SimplfyTo..
  }


  # If obj contains a mix of lists/non-lists elements, then 
  #    all list elements need to be handled first via a recursive call to listFlatten
  listIndex <- sapply(obj, is.list) 
  if (any(listIndex)) {
    input <- sapply(obj[listIndex], listFlatten, filler=filler, simplify=FALSE)

      # if object is a list without columns (ie, not dataframe, etc), then we can just insert the input back in. 
      # Otherwise, we need to call isertListAsCols
      if (is.list(obj) && is.null(dim(obj)))  {
        obj[listIndex] <- input
      } else {
        obj <- insertListAsCols(input, target=obj, targetCols=which(listIndex), replaceOriginalTargetCol=TRUE, preserveNames=TRUE)          
      }
  } # end if (any(listIndex))
 
  # Next, Any elements of obj that are factors need to be converted to character
  factorIndex <- sapply(obj, is.factor) 
  obj[factorIndex]  <-  sapply(obj[factorIndex], as.character)


  # Initialize Vars
  bind <- FALSE
    
  # IF ALL ELEMENTS ARE MATRIX-LIKE OR VECTORS, MAKE SURE SAME NUMBER OF COLUMNS
  matLike <- sapply(obj, function(x) !is.null(dim(x)))
  vecLike <- sapply(obj, is.vector)

  # If all matrix-like. 
  if (all(matLike))   {
    maxLng <- max(sapply(obj[matLike], ncol))
    obj[matLike] <- lapply(obj[matLike], function(x) t(apply(x, 1, c, rep(filler, maxLng - ncol(x)))))
    bind <- TRUE
  
  # If all vector-like
  }  else if (all(vecLike))  {
    maxLng <- max(sapply(obj[vecLike], length))
    obj[vecLike] <- lapply(obj[vecLike], function(x) c(x, rep(filler, maxLng - length(x)))) 
    bind <- TRUE

  # If all are either matrix- or vector-like 
  }  else if (all(matLike | vecLike))   {  # TODO:  Double check this.  I had this with '&' before. I think that was incorrect. 

    maxLng <- max(sapply(obj[matLike], ncol), sapply(obj[vecLike], length))

    # Add in filler's as needed
    obj[matLike] <- 
       lapply(obj[matLike], function(x) t(apply(x, 1, c, rep(filler, maxLng - ncol(x)))))
    obj[vecLike] <- 
       lapply(obj[vecLike], function(x) c(x, rep(filler, maxLng - length(x))))
    bind <- TRUE
  } 

  # If processed and ready to be returned, then just clean it up
  if(bind)  {

    # If obj is a data.frame, then it might be all ready to go
    if (is.data.frame(obj) && length(obj) == ncol(obj))
      return(obj)

    # Otherwise, flatten 'obj' with rbind. 
    ret <- (do.call(rbind, obj))
    colnames(ret) <- paste0("L", fw0(1:ncol(ret), digs=2))
    return(ret)
  }

  # Otherwise, if obj is sitll a list, continue recursively    
  if (is.list(obj)) { 
      return(lapply(obj, listFlatten))
  }

  # If none of the above, return an error. 
  stop("Unknown object type")
}


#--------------------------------------------

tableFlatten <- function(tableWithLists, filler="") {
# takes as input a table-like object with lists and returns a flat table
#  empty spots in lists are filled with value of 'filler'
#
# depends on: listFlatten(.), findGroupRanges(.), fw0(.)

  # index which columns are lists
  listCols <- sapply(tableWithLists, is.list)

  tableWithLists[listCols]
  tableWithLists[!listCols]

  # flatten lists into table
  flattened <- sapply(tableWithLists[listCols], listFlatten, filler=filler, simplify=FALSE)

  # fix names
  for (i in 1:length(flattened)) colnames(flattened[[i]]) <- fw0(ncol(flattened[[i]]), 2)

  # REASSEMBLE, IN ORDER
    # find pivot point counts
    pivots <- sapply(findGroupRanges(listCols), length)

    #index markers
    indNonList <- indList <- 1

    # nonListGrp <- (0:(length(pivots)/2)) * 2 + 1
    # ListGrp <- (1:(length(pivots)/2)) * 2
    final <- data.frame(row.names=row.names(tableWithLists))
    for (i in 1:length(pivots)) {
      if(i %% 2 == 1) {
          final <- cbind(final, 
                       tableWithLists[!listCols][indNonList:((indNonList<-indNonList+pivots[[i]])-1)]
                       )
      }  else  {
          final <- cbind(final, 
                       flattened[indList:((indList<-indList+pivots[[i]])-1)]
                       )
      }
    }
    
    return(final)
}


#---------------------------------------------


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#___________________________________________%
  # Generic form of insertListAsCols

  insertListAsCols <- function(input, target, targetCols, replaceOriginalTargetCol=FALSE, preserveNames=TRUE)
      UseMethod("insertListAsCols")
#___________________________________________%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  

insertListAsCols.list <- function(input, target, targetCols, replaceOriginalTargetCol=FALSE, preserveNames=TRUE) {
# input should be table-like or a list of table-like elements.  
# if input is list or multidimensional, but targetCols has length 1, is an error. 

# note: If uncareful, preserveNames=TRUE can cause infinite loop  # TODO:  insert safetybreak

  ## ERROR CHECK
  if (length(input) != length(targetCols))
    stop("length(input) and length(targetCols) do not match")

  ## If there are no names to preserve, then adjust the flag accordingly
  ## If there are target names, but not list names, then 
  if (is.null(names(input))) {
    if (is.null(names(target))) {
          preserveNames <- FALSE
    } else {
      names(input) <- paste("L", fw0(seq(length(input)), 2), sep="_") 
    }
  }

  # If prserve names, then call function just on names. They get reapplied at end. 
  if (preserveNames) {
    # OLD nms:  mapply(function(name, thelist) {t(rep(name, ncol(thelist)))}, names(input), input, SIMPLIFY=FALSE)
    nms <- mapply(function(name, thelist) {t(paste(name, 1:ncol(thelist), sep="."))}, names(input), input, SIMPLIFY=FALSE)

    targetNames <- insertListAsCols(input=nms, target=rbind(names(target)), 
                    targetCols=targetCols, replaceOriginalTargetCol=replaceOriginalTargetCol, preserveNames=FALSE)
  } 


  # If we are preserving the original, then we add 1 to the index values. 
  #  ie, rOT is 0 if replaceOriginalTargetCol is TRUE
  rOT <- as.numeric(!replaceOriginalTargetCol)

  ## length of targetcols used many times 
  numbOfSplices <- length(targetCols)  # this variable might need a better name.  Does 'A B' have one splice (the space) or two (the A and the B)?

  ## we take the amount of each padding to be the number of columns of each input
  padAmounts <- unlist(sapply(input, ncol))
  padAmounts[is.null(padAmounts)] <- 1  # TODO: confirm that this in fact is acceptable (and not that we are masking errors)
  padAmounts <- padAmounts - (1-rOT)

  ## Pad target with filler-columns

  # a filler column of just NA's will be used for padding
  fillerCol <- rep(NA, nrow(target))

  for (i in seq_along(targetCols))  {
    t <- targetCols[[i]]
    ln <- padAmounts[[i]]

    #------------------
    #   This pads 'target' once. Then we have to re-adjust our indicies
    #------------------
        target.tmp <- target[,1:t, drop=FALSE]  # was 's:t+rOT' but I dont think thats needed
        for (j in seq(ln))
          target.tmp <- cbind(target.tmp, fillerCol)

        # if we just padded the last columns of target, then just replace target, else append appropriately
        if (t < ncol(target)) {
          target <- cbind(target.tmp, target[,(t+1):ncol(target), drop=FALSE])  
        } else {
          target <- target.tmp 
        }


        # increment all targetCols beyond the i'th one by the number of reps, so long as there are any left
        if (i < numbOfSplices)
          targetCols[(i+1):numbOfSplices] <- targetCols[(i+1):numbOfSplices] + ln
    #------------------    
  } # end for-loop


  # Shift target Cols according to whether we are preserving column in current spot or not
  targetCols <- targetCols + rOT  # shift over 1 if we are preserving the original

           # # make a matrix of indexes, we will iterate over each row. 
           # indxs <- t(mapply(seq, from=targetCols, to=targetCols+padAmounts-rOT))  # Note that padAmounts has already been adjusted by rOT  
 # OLD     
           # # Insert the input columns in their appropriate spots in the target 
           # for (i in seq_along(padAmounts)) {
           #   target[ indxs[i, ] ] <- input[[i]]   # or...   <- apply(input[[i]], 2, function(x) x)
           # }

  # make a matrix of indexes, we will iterate over each row. 
  indxs <- mapply(seq, from=targetCols, to=targetCols+padAmounts-rOT, SIMPLIFY=FALSE) # Note that padAmounts has already been adjusted by rOT 

  # Insert the input columns in their appropriate spots in the target 
  for (i in seq_along(padAmounts)) {
    target[ indxs[[i]] ] <- input[[i]]   # or...   <- apply(input[[i]], 2, function(x) x)
  }

  # cleanup names of 'target' and remove last NA of 'targetCol'
  if (preserveNames) {
    names(target) <-  targetNames
  } else {
    names(target) <- 1:ncol(target)  
  }

  # return modified target
  return(target)

}



#--------------------------------------------

findGroupRanges <- function(booleanVec) {
# returns list of indexes indicating a series of identical values
  pivots <- which(sapply(2:length(booleanVec), function(i) booleanVec[[i]] != booleanVec[[i-1]])) 

  pivots <- c(0, pivots, length(booleanVec))
  lapply(seq(2, length(pivots)), function(i)
    seq(pivots[i-1]+1, pivots[i])
  )
}

#--------------------------------------------

nestedIndx <- function(obj, pre=NULL, expandLast=FALSE, asDF=FALSE) {
  ## returns a matrix (or data.frame) whose rows are
  ##   the extended indecies of a nested list
  ## DEPENDENT ON: listFlatten(), fw0()

  # final depth level (ie, end recursion)
  if (!is.list(obj)) {
    # not flagged otherwise, return the last column just as a length value
    if (!expandLast)
      return(c(pre, length(obj)))
    
    # otherwise, expand it. 
    # If there is no pre, then we're just seq on obj
    if (is.null(pre))
      return(cbind(seq(obj)))
    # otherwise, combine seq with pre, transposing for correct orientation
    return(t(sapply(seq(obj), function(x) c(pre, x))))
  }

  s <- seq(length(obj))
  soFar <- lapply (s, function(i) c(pre, i) )
  
  ret <- listFlatten (lapply(s, function(i) nestedIndx(obj=obj[[i]], pre=soFar[[i]], expandLast=expandLast)))

  if (is.null(pre)) {
    colnames(ret) <- gsub("L", "Lev_", colnames(ret)) 
  }

  # clean up name of last column, if not flagged to expand last depth
  if(!expandLast)
    colnames(ret)[ncol(ret)] <- "Length"

  if (asDF)
    return(data.frame(ret))

  ret
}

#--------------------------------------------
