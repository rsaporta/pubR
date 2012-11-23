# for broken keyboard with no \ key
nl <- "\n"

# useful shorthand
len <- length
p <- paste0

JavaTest <- function(stopRun=TRUE, runInit=TRUE) {
  ## ensures that rJava is up and running.  If Java is NOT running and...
  ##    stopRun=T, will throw an error. If stop=False, will throw a warning.
  ## If runInit=T, will load rJava and run .jinit() prior to running .jcheck()

  # Load rJava and initialize java
  if (runInit) {
    library(rJava)
    .jinit()
  }

  # Run .jCheck
  errString <- ".jcheck() failed.  Please troubleshoot rJava"
  if (isErr(.jcheck())) {
    if (stopRun) {
      stop(errString)
    }
    else  {
      warning(errString)
    }
  }
}

isErr <- function(expression)  {
  #  Boolean; Tries to evaluate the expresion; returns T if an error is thrown
  #  Args:
  #    expression:  Make sure to use expression() to pass an expression (dont use Strings)
  #  Returns:
  #    T if expression throws an Error // F if expression is evaluated without error
  #    NOTE:  The actual evaluation of the expression is NOT RETURNED
  
  return( class(try(eval(expression), silent=T))=="try-error" )
}

showProg <- function(flag, outp, reset=FALSE, done=FALSE, t=1)  {
  # wrapper function for: 
  # if flag is true, then cat() outp. 
  if (reset) 
    cat ("========================\nProgress Indication....\n")
  if (flag)
    cat(tbs(t), outp, "\n", sep="")
  if (done) 
    cat ("\n----------------\nProcess Complete\n========================\n")
} 

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
  # returns a data frame or matrix indicating the structure of a nested list

  inds <- nestedIndx(obj)
  
  # flag of -1 indicates to put values first in the df.
  if (showValues==-1)  {
    print("TRUE")
    inds <- data.frame(cbind(value=apply(inds, 1, function(ind) obj[[ind[!is.na(ind)]]]), inds))
  
  # flag of 1/T indicates to put values last in the df
  } else if (showValues)  {
    inds <- data.frame(cbind(inds, value=apply(inds, 1, function(ind) obj[[ind[!is.na(ind)]]])))
  }   

  # flag of F indicates no values in the df

  return(inds)
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

listFlatten <- function(obj) {
  ## Flattens like rbind, but if elements are of different length, plugs in NA's

  # Initialize Vars
  bind <- FALSE
    
  # IF ALL ELEMENTS ARE MATRIX-LIKE OR VECTORS, MAKE SURE SAME NUMBER OF COLUMNS
  matLike <- sapply(obj, function(x) !is.null(dim(x)))
  vecLike <- sapply(obj, is.vector)

  # If all matrix-like. 
  if (all(matLike))   {
    maxLng <- max(sapply(obj[matLike], ncol))
    obj[matLike] <- lapply(obj[matLike], function(x) t(apply(x, 1, c, rep(NA, maxLng - ncol(x)))))
    bind <- TRUE
  
  # If all vector-like
  }  else if (all(vecLike))  {
    maxLng <- max(sapply(obj[vecLike], length))
    obj[vecLike] <- lapply(obj[vecLike], function(x) c(x, rep(NA, maxLng - length(x)))) 
    bind <- TRUE

  # If all are either matrix- or vector-like 
  }  else if (all(matLike & vecLike))   {

    maxLng <- max(sapply(obj[matLike], ncol), sapply(obj[vecLike], length))

    # Add in NA's as needed
    obj[matLike] <- 
       lapply(obj[matLike], function(x) t(apply(x, 1, c, rep(NA, maxLng - ncol(x)))))
    obj[vecLike] <- 
       lapply(obj[vecLike], function(x) c(x, rep(NA, maxLng - length(x))))
    bind <- TRUE
  } 

  # If processed and ready to be returned, then just clean it up
  if(bind)  {
    ret <- (do.call(rbind, obj))
    colnames(ret) <- paste0("L", fw0(1:ncol(ret), digs=2))
    return(ret)
  }


  # Otherwise, if obj is sitll a list, continue recursing    
  if (is.list(obj)) { 
      return(lapply(obj, listFlatten))
  }

  # If none of the above, return an error. 
  stop("Unknown object type")
}

#--------------------------------------------

nestedIndx <- function(this, pre=NULL, thisdepth=0) {
  ## returns a matrix indicating whose rows are the extended indecies of a nested list
  ## DEPENDENT ON: listFlatten()

  if (!is.list(this))
    return(pre)

  s <- seq(length(this))
  soFar <- lapply (s, function(i) c(pre, i) )
  
  listFlatten (lapply(s, function(i) nestedIndx(this[[i]], pre=soFar[[i]])))

}

#--------------------------------------------



fw0 <- function(obj, digs=NULL)  {
  ## formats digits with leading 0's. 
  ## obj should be an integer or range of integers.  

  if (!is.vector(obj)) {
    stop("Obj should be integer or vector")
  }

  # TODO 1:  put more error check
  # TODO 2:  clean up the if statements. Consider using recursion


  # If digs is specified, also consider the obj specified (do not expand to range)
  if(!is.null(digs)) {
    sequ <- obj
  
  # Otherwise, calculate range, based on length of obj. Then calculate digs
  } else {


    if(!length(obj)>1) {
        sequ <- (1:as.numeric(obj))
    } else  {
        sequ <- obj 
    }
   
    digs <- nchar(max(sequ))    
  }

  # return
  sapply(sequ, function(x) paste0(paste0(rep(0, max(0, digs-nchar(x))), collapse=""), x))
}

###############################################################
###############################################################

CMT <- getCMT <- getClassModeTypeof <- function(obj)  { 
  # Returns as a vector, the class, mode, typeof  of the obj
  # getCMT(), CMT() are useful shorthands
  return(c("class"=class(obj),"mode"=mode(obj),"typeof"=typeof(obj)))
}

jythonIsGlobal <- function()  {
  #  Checks to see if jython is properly set, if not then sets it
  #
  # Returns FALSE if jython was not previously set; else returns TRUE
  
  testForjython <- try(class(jython), silent=TRUE)
  if (class(testForjython) == "try-error"  |  testForjython!="jobjRef")  {  
        require(rJython)
    .jinit()
    jython <<- rJython()
    return(FALSE)
  }
  return(TRUE)
}

python <- function(jythonStatement)  {
  #  Executes in python the string passed
  #    (Simply a wrapper for an easier way to make python calls)
  #  Arg:
  #    jythonStatement: an executable line of python code of type string
  # 
  #  Returns:
  #   passes through the return from the jython.exec command (generally NULL)
  
  jythonIsGlobal()
  return(jython.exec(jython, jythonStatement))
  
  # OLD:  return(jython.exec(jython, paste(jythonStatement)))

}

pythonGet <- function(pythonObj)  {
  #  From Python Environment, Gets the value of pythonObj.
  #    (Simply a wrapper for an easier way to get a python object)
  #
  #  Arg:
  #    pythonObj: name of object in python whose value will be retrieved & returned
  #
  #  Returns:
  #   the value of pythonObj in the python environment

  jythonIsGlobal()
  return(jython.get(jython, pythonObj))
  # OLD  return(jython.get(jython, paste(substitute(pythonObj))))
}

pythonSet <- function(rObj)  {
  #  Sets the value of a python object of same name as rObj to value of rObj.
  #    same as pythonSetDiffName() but with one less argument to have to type
  #
  #  Arg:
  #    rObj: object in R; value will be assigned to object of same name in python
  #          
  #
  #  NOTE: when rObj is a string, pythonSet will create 
  #        a variable whose name is the value of the string 
  #        and whose value is also the value of the string. 
  # 
  #  Returns:
  #   passes through the return from the jython.assign command (generally NULL)
  
  jythonIsGlobal()
  return(jython.assign(jython, substitute(rObj), rObj))
}

pythonSetDiffName <- function(pythonObj, rObj)  {
  #  Sets the value of a python object named pythonObj to that of rObj
  #    
  #  Arg:
  #    pythonObj: name of object in python environment that will receive rObj
  #    rObj: object in R whose value is getting assigned to pythonObj
  #
  #  Returns:
  #   passes through the return from the jython.assign command (generally NULL)
  
  jythonIsGlobal()
  return(jython.assign(jython, pythonObj, rObj))
}


pyParse <- function(strToParse)  {
  #  Uses Python to parse a string along any non-char delim
  #  Arg:
  #    strToParse: any string needing parsing
  #
  #  Returns:
  #   list of parsed strings

  python("import re")
  pythonSetDiffName("strToParse123b4c5", strToParse)   
  return(pythonGet(paste("re.findall('\\w+', str(strToParse123b4c5))")))
}


form <- function(x, dig=3)  {
  # just a wrapper for format(x), with options defualted to 3
  return(as.numeric(format(x, digits=dig, nsmall=dig)))
}

getNCMT <- getNameClassModeTypeof <- function(obj)  { 
  # Returns as a vector, the name, class, mode, typeof  of the obj
  # getNCMT is a useful shorthand
  return(c("name"=names(obj), "class"=class(obj),"mode"=mode(obj),"typeof"=typeof(obj)))
}

countNA01s <- function(vec)  {
  # in a given vector,  how many are there of each: NA, 0, 1, -1, >1, <(-1), 'other'
  #  useful for helping to determine if the vector is in fact logical    
  #
  # Args: vec;  a vector
  #  NOTE: if the vector is of class "factor", then 'lt-1' and 'gt1' will not calculate
  #        in this case, 'nota' (none of the above) is helpful
  #        CAREFUL: even if 'lt-1', 'gt1' ARE calculated, 'nota' will still count those elements

  return( c("NAs"=sum(is.na(vec)), 
            "lt-1"=sum(vec < (-1) & !is.na(vec)), 
            "-1s"=sum(vec == (-1) & !is.na(vec)), 
            "0s"=sum(vec == 0 & !is.na(vec)),
            "1s"=sum(vec == 1 & !is.na(vec)), 
            "gt1"=sum(vec > 1 & !is.na(vec)),
            "nota"=sum(vec != 1 & vec != 0 & vec != (-1) & !is.na(vec))  #none of the above
        ))
}


insert <- function(lis, obj, pos=0, objIsMany=FALSE) {
  # Inserts obj into lis *at* position
  #    all existing items in list, form pos onward, are moved forward
  #    NOTE: If position > len(list), obj is inserted at end
  #
  # Args:
  # lis:  the list object
  # obj:  the object being inserted
  #   pos:  the position of insert
  #   objIsMany: (TODO) If T, each item in obj is inserted separately
  #
  # Returns:
  #   list with obj inserted at position 
  #
  # TODO: modify for objIsMany=TRUE
  
  
  leng <- len(lis)
  if (pos > leng) {   # note strictly greater (not greater or equal!)
    return (c(lis,obj))
    ## TODO:  Check for objIsMany
    ## ifelse(objIsMany, for(i in....))
    }
    
    
  if(pos <= 1)  {
    c(obj,lis)
  } else {
    c(lis[1:pos-1], obj, lis[pos:leng])
  }
}


as.path <- function(..., fsep=.Platform$file.sep, expand=TRUE) {

  ## If starts with fsep, we will preserve it.
  startWith <- ifelse(substr(..1,1,1) == fsep, fsep, "")
  
  cleaned <- lapply(list(...), function(x) {      
      # remove any leading slashes
      x <- ifelse(substr(x, 1, 1) == fsep, substr(x, 2, nchar(x)), x) 
      
      # remove any trailing slashes
      lng <- nchar(x)
      x <- ifelse(substr(x, lng, lng) == fsep, substr(x, 1, lng-1), x) 

      # return x to cleaned
      x
    })

  # put back any starting fsep
  cleaned[[1]] <- paste0(startWith,cleaned[[1]])

  if (!expand)
      return(do.call(file.path, c(cleaned, fsep=fsep)))
  return(path.expand(do.call(file.path, c(cleaned, fsep=fsep))))
}

dosDir <- function(wrkDir, gitData=FALSE, mkdir=TRUE) {
  # makes data, out, src directory inside the directory wrkDir
  #   and creates variables with full path to these directories
  #   in the parent environment  (the environment that called this func) 
  # 

  grp <- list("data", "out", "src")

  # create vars (+'Dir') and vals (paths)
  vars <- paste0(grp, "Dir")
  vals <- mapply(as.path, wrkDir, grp, MoreArgs=list(expand=F), USE.NAMES=F)

  # if flagged, data dir will be in different directory
  if (gitData)
    vals[[1]] <- sub("/git/", "/gitData/", eval(vals[[1]], envir=parent.frame()))

  # if flagged, create directories if they do not exist
  if (mkdir)
    sapply(path.expand(vals), dir.create, showWarnings=F)

  # assign vals to appropriate var names in the calling environment                  
  mapply(assign, vars, vals, MoreArgs=c(pos=parent.frame()))

}
 

rmDupLines <- function(obj)  {
  # removes duplicate lines from obj and returns the modified object.
  # especially useful for captured output of summary.lm() 
  if (is.null(dim(obj)))
    return(obj[!sapply(seq(obj)[-1L], function(i) obj[[i]]==obj[[i-1]])])

  return(obj[!sapply(seq(obj)[-1L], function(i) obj[i,]==obj[i-1,])])
}


cordl <- function(...)  {
  # Capture Output, Remove Duplicate Lines, wrapper function. 
  rmDupLines(capture.output(...))
}



makeDictFromCSV <- function(csvFile)  {
  # Creates a dictionary out of a CSV file where 
  #    col1 of the CSV are the keys and col2 are the values.
  #
  # Arg:
  #   dictCSVPath: A path to a CSV file
  #
  # Returns a dictionary (list) s|t  dict["key"] = "value"
  #   eg: dict["LooonngWooord"] = "shortwrd" 
   
  c <- read.csv(csvFile)
  dict <- list(as.character(c[[2]]))
  names(dict[[1]]) <-(as.character(c[[1]]))
  rm(c) # keep it clean
  
  return(dict[[1]])
}

isSubstrAtEnd <- function(x, pattern, ignorecase=TRUE)  {
  # Checks if x ends with  pattern

  if (ignorecase)
     return (tolower(substr(x, nchar(x)-(nchar(pattern)-1), nchar(x)))==tolower(pattern))

  return (substr(x, nchar(x)-(nchar(pattern)-1), nchar(x))==pattern)
}

s <- smry <-function(x, rows=6, cols=6, cmt=TRUE) {
  # prints out a the first rows & cols of x
  #  if either is negative, prints from the end for that axis
  # 

  # Also print out the Class, Mode, Typeof of the object
  cat("\n")
  print(CMT(x))
  cat("\n")
  
  # check if x is Multidimensional or not
  isArr <- ifelse(is.null(dim(x)),FALSE,TRUE)  

  # MULTIDIMENSIONAL
  if (isArr)  {
    rx <- nrow(x)
    cx <- ncol(x)
  
    cat("  TOTAL ROWS: ", rx, "\t  TOTAL COLS: ", cx, "\n\n")

    #rows to print
    if (rows < 0) {
      rowsRange <- rx:max(1, rx+rows) #rows is negative
    } else {
      rowsRange <- 1:min(rows, rx) 
    }
    
    #cols to print
    if (cols < 0)  { 
      colsRange <- cx:max(1, cx+cols) #cols is negative
    } else {
      colsRange <- 1:min(cols, cx) 
    }
    
    return(print(x[rowsRange, colsRange]))
  }


  # UNI-DIMENSIONAL
  else  {
    rx <- length(x)
    
    cat("  TOTAL ROWS: ", rx, "\n\n")

    #rows to print
    if (rows < 0) { 
      rowsRange <- rx:max(1, rx+rows) #rows is negative
    } else {
      rowsRange <- 1:min(rows, rx) 
    }
    
  return(print(x[rowsRange]))
  }
}

c4 <-function(x, rows=20, cols=4, cmt=TRUE) {
  ## wrapper to function smry, with rows=20 and cols=4. (hence c4)
  ##   note, calling c4(x, 35) will give 35 rows and 4 cols. (simpler than s(x, 35, 4)) 
  smry(x, rows, cols, cmt)
}
  

# UPDATED NOTE TO SELF: 
#... I BELIEVE randomRows is useless, considering sample(1:nrow(x), ...)
randomRows <- function(x, percent=10, rows=0)  {
  # returns a vector of random indices to x
  # the size of the return vector is equal to rows (if not 0), or to percent * nrow(x)
  #
  # args: x any matrix or vec object
  #       percent:  0 < percent < 100;  calculate rows as a percent of nrow(x);  ignored if rows > 0.
  #       rows:  an integer, number of rows to select from x. cannot be < 0.  if == 0, then will be calculated using percent. 
  #      
  
  #error check: 
        if (length(x)==1 && mode(x)=="numeric") {
          errmsg <- paste0("\n\tReceived a single numeric ('", x, "') when expecting a range. Using range  '1:", as.integer(x), "'' instead.\n")
          warning (errmsg)
          x <- 1:x
        }
        rows <- as.integer(rows) 
        if (rows < 0) {
          errmsg <- paste0("\n\tInvalid value for rows: '", rows, "'.  Using percentage (",percent, "%) instead.\n")
          warning (errmsg)
          rows <- 0
        }
        if (percent < 0 || percent > 100) {
          errmsg <- paste0("\n\tInvalid percent value: '", percent, "'.  Using default value of 10.\n")
          warning (errmsg)
          percent <- 10
        }
  #end error check

  # rx is the number of rows of x (if x is uni-dimensional, use length(x) instead)
  rx <- ifelse(is.null(dim(x)), length(x), nrow(x))  

  # if user did not indicate rows, then calculate it as a percentage of total rows 
  if (!rows) {
    rows <- as.integer(rx * (percent / 100))
  }

  # generate the indices 
  indices <- sample(1:rx, rows)          # ALT distributed around median:   indices <- (as.integer(rnorm(rows, rx/2, rx/4))  %% rx)  + 1

  return(indices)
}

qy <- quity <- function(dir='~/')  {
  ## quits R and saves the .RData and .Rhistory to dir
  setwd(dir)
  quit('yes')
}

tbs <- function(n)  {
  # returns a string of n-many tabs, concatenated together
  return(paste0(rep("\t", n), collapse=""))  
}

miniframe <- function(data, rows=200)  {
  ## returns a dataframe similar to data but with a randomly selected rows 
  miniLength <- 200
  l <- nrow(data)
  ind <- abs(rnorm(miniLength))* l
  ind <- round(ind)  %% l
  cat(ind)
  return(data[ind,])
}



makeDictWithIntegerKeys <- function(KVraw, applyLabels=TRUE)  { 
    ###  problem: if 
    # we want a dict such that dict[aritstid] = artist_name
    # PROBLEM:  since artistid's are integers, dict[artistid] will return the artistid'th (nth) item 
    # eg:  dict[510] will return the 510th item of dict, not the artist whose id is 510  *rather, not necessarily..  
    #      that is,  dict[510] != dict["510"]
    # 
    # this wouldnt be a problem if we can ensure that each artistid gets loaded 
    # into dict at the position of its integer value
    # then dict[artistid] and dict[sQuote(artistid)] will return the same value
    #
    # Args: KVraw should be two-dim matrix with col1==Keys, and col2==Values, 
    #       applyLabels: if T, dict will have names st dict["123"] == dict[123]; 
    #                    if F, dict["123"] is undefined
    #                    NOTE: The labels are needed in order to be able to make calls like 
    #                          which(names(dict) %in% subsetOfKeys) where subsetOfKeys
    #                          is some collection of keys and we want the corresponding values
    # Return:
    #   a one-dim list where dict[key] == value, where key is an integer


    ## initialize the dict
    largestK <- max(KVraw[[1]])  # make sure we create enough room in dict
    dict <- rep(NA,largestK)     # note that len(dict) >= len(KVraw)
    names <- dict

    ## assign values
    for (i in 1:nrow(KVraw) )  {
      dict[as.integer(KVraw[[1]][i])] <- KVraw[[2]][i]
      names[as.integer(KVraw[[1]][i])] <- as.character(KVraw[[1]][i])
    }

    ## assign labels if option'd
    if (applyLabels) {
      names(dict) <- names      
    }

    return(dict)
}
  
  
chkp <-chkpt <- function(logStr, chkpOn=TRUE, final=FALSE) {
  # Logs the string to the console for checkpointing & troubleshooting
  # Args:
  # logStr:  a string that will be logged to stdout
  # chkpOn:  If FALSE, then logging does not occur. (for quickly turning chkp on/off)
  # 
  # Returns Null
  
  if (chkpOn) {
    if (nchar(logStr)<3)
      logStr <- paste0("\t\t  ",logStr)
    else if (nchar(logStr)<12)
      logStr <- paste0("\t\t",logStr)
    else if (nchar(logStr)<15)
      logStr <- paste0("\t",logStr)
    else if (nchar(logStr)<17)
      logStr <- paste0("  ",logStr)
    else if (nchar(logStr)<20)
      logStr <- paste0(" ",logStr)

    #log
    cat(paste0("\t\t",
          ")*(   checkpoint   )*(","\n\t\t",logStr,"\n", collapse=""))
  }
  
  if (final) {
    cat("\n\n")  #for cleanliness
  }

  return()
}

 

pgDisconnectAll <- function(drv=dbDriver("PostgreSQL")) {
  # Closes all open connections to drv
  for (conn in dbListConnections(drv)) {
    dbDisconnect(conn)
  }
}

replaceBadCharsUnderscore <- function(str, WhiteList=NULL){
  # replaces any none-standard chars with '_'
  # eg: "Sigur RÃ³s"  ==>  "Sigur R__s"

  str <- as.character(str)
  okChars <- c(LETTERS,letters, 0:9,"_","-"," ", WhiteList)

  for (i in 1:nchar(str)) {
    substr(str, i, i) <- ifelse(substr(str, i, i) %in% okChars,  substr(str, i, i), "_")
  }
  return(str)
}



ts <- timeStamp <- function(seconds=FALSE) {
  # basic time stamp:   20111231_2350  for Dec 31, 2012, 11:50pn

    if(seconds)
        return(format(Sys.time(), "%Y%m%d_%H%M%S"))
    

    return(format(Sys.time(), "%Y%m%d_%H%M%S"))
}



saveit <- function(obj, directory=getwd(), createSubDir=TRUE)  {
  ## saves obj to an .Rda to a file of 
  ##     the same name, with a time stamp
  ##     in location: directory
  ##     createSubDir:  if TRUE, will create subdirectory data_bak 
  ##                inside directory and use that folder. (if alreaddy exists, will just use)
  ##
  ## returns:  the path/to/file.Rda where obj was saved
  ##
  ## TODO:  The Rdata file names the data 'obj'.  I dont think this can be fixed, other than to load the .Rda and then myName <- obj;  rm(obj, pos=#).  
  ##   this function is useful primarily for a quick save with a timestamp in a useful directory. 

  #cleanup the strings for a proper filename
  objName <- replaceBadCharsUnderscore(substitute(obj))
  
  # make sure no ending slash, as it will be added later
  if (isSubstrAtEnd(directory,"/")) {
      directory <- substr(directory,1,nchar(directory)-1)
  }

  # use subdirectory data_bak unless indicated not to  (create it if needed)
  if (createSubDir) {
    dir.create(file.path(directory, "data_bak"), showWarnings=FALSE)
    directory <- paste0(directory, "/data_bak")
  }

  # create the filename, then save it
  fileName <- paste0(directory,"/",objName,"_",ts(),".Rda")
  save(obj, file=fileName)
  return(fileName)
}

plength <- printlength <- function(opt=200) {
## Changes the environment's setting for how many elements to output for print command
## 
## Arg:  opt is the maximum number of elements that will be outputed when print is called
##
## Returns the value returned by the options call, which is the previous max.print setting
  return(options("max.print" = opt))
}

saveToFile_TabDelim <- function(obj, directory=getwd())  {
  ## saves obj as a .csv file of  
  ##     the same name, with a time stamp
  ##     in location: directory
  ##
  ## Argss:  Obj should be matrix or df-like
  ##
  ## returns:  the path/to/file.Rda where obj was saved

  #cleanup the strings for a proper filename
  objName <- replaceBadCharsUnderscore(substitute(obj))
  if (isSubstrAtEnd(directory,"/")) {
      directory <- substr(directory,1,nchar(directory)-1)
  }

  # create the filename, then save it
  fileName <- paste0(directory,"/",objName,"_",ts(),".csv")
  write.table(obj, file=fileName, sep="\t", eol="\n",
              col.names=TRUE, row.names=TRUE, append=TRUE, quote=F, qmethod="double")

#  write.table(rbind(obj), file=fileName, sep="\t", eol="\n",
       #       col.names=TRUE, row.names=TRUE, append=T, quote=F, qmethod="double")
  return(fileName)
}

logscale <- function(range=2:5, intervals=2, base=10)  {
# returns a sorted vector of powers of the base. 
# range is a vector of powers
# intervals is applied AFTER powers, so that intervals=3  for range=1:4 would return:
#     (10 * 1/3), (10 * 2/3), (10 * 3/3),  (100 * 1/3), (100 * 2/3), (100 * 3/3),  etc...
#
  factors <- seq(intervals) / (intervals)
  ret <- sort(unlist(lapply((base^range), function(x) {x*factors})))
  return (ret)
}

asCurr <- function(x, decim=2, noSpacesAfterSymb=1, symbol="$") {
# returns a currency-formatted string
  
  ## BLANK SPACES AFTER SYMBOL
  spaces <- ""
  if (noSpacesAfterSymb > 0) {
    for (i in 1:noSpacesAfterSymb) {
      spaces <- paste0(spaces, " ")
    }
  }

  ## DECIMAL DIGIT
  if (decim==0) {
    deciDigsStr = ""
  } else {
    decDigs <- round((x %% 1) * (10^decim), 0)  
    #check for missing leading zero's, as in $777.02 (otherwise would pring $777.2)
    padding <- ""
    i <- decim - 1
    while (decDigs < 10^i  &&  i > 0) {
      padding <- paste("0", padding, sep="")  
      i <- i-1; 
    }
    deciDigsStr <- paste(".", padding, prettyNum(decDigs, scientific=FALSE), sep="")
  }
  

  ## BODY OF NUMBER
  xStr <- prettyNum(round(x,0), scientific=FALSE, big.mark=",")

  return(noquote(paste0(symbol, spaces, xStr, deciDigsStr)))
}  # END asCurr

lP <- listPacker <- function(receiver, ...)  {
  # takes all arguments (...) and appends them to receiver
  #
  # receiver should be list-like
  #  value returned is list-like 

  return(c(receiver, list(...)))

  # TODO:  Decide if any of this is still useful, else chuck it. 
        # if (length(list(...)) > 0L) {
        #   receiver[length(receiver) + 1L] <- ..1
        #   if (length(list(...)) > 1L)  {
        #       receiver <- listPacker(receiver, list(...)[-1L])    
        #   }
        # } else {
        #   warning("There were nothing to add to the list.")
        # }
        # receiver
}

  

#/@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@\#
##               DIRECTORY FUNCTIONS               ##

initialDir  <- getwd()
previousdir <- getwd()  # generally: "~/"


previouswd <- function()
{
# sets Working Directory to previous working directory

  previousdir_tempvar <- getwd()   #Store the current working dir
  setwd(previousdir)
  previousdir <- previousdir_tempvar  #Store the previous dir
}

homewd <- function()
{
  previousdir <- getwd()   #Store the current working dir
  setwd('~/')
}

devwd <- function()
{
  previousdir <- getwd()   #Store the current working dir
  ScriptsR <- '~/Dropbox/dev/R/!ScriptsR'
  cat("\n [dev dir set to dropbox.  Alternatively, consider 'gitwd()']\n")
  setwd(ScriptsR)
}

gitwd <- function()
{
  previousdir <- getwd()   #Store the current working dir
  git <- '~/git'
  setwd(git)
}


msdwd <- function()
{ 
  previousdir <- getwd()   #Store the current working dir
  msdDir <- '~/Dropbox/dev/! PROJECTS/Kaggle Challenges/Million Song Dataset Challenge/Data'
  setwd(msdDir)
}

nbswd <- function()
{ 
  previousdir <- getwd()   #Store the current working dir
  nbsDir <- '~/git/nbs/'
  setwd(nbsDir)
}


###  PURPOSE OF THE xxxxsource(file) FUNCTIONS ARE TO BE ABLE 
###    TO CALL A FILE MORE RAPIDLY WITHOUT HAVING TO RETYPE PATHS

devsource <- function(file, dir="~/Dropbox/dev/R/!ScriptsR/"){
  # Calls source on file located in dev folder

  source(paste(dir,file,sep=""))
}

gitsource <- function(file, dir="~/git/misc/rscripts/"){
  # Calls source on file located in git folder

  source(paste(dir,file,sep=""))
}

nbssource <- function(file, dir="~/git/nbs/src/"){
  # Calls source on file located in NBS Data folder
  # NOTE: nbs/src,  not nbs/data,   etc. 

  source(paste(dir,file,sep=""))
}


homesource <- function(file, dir="~/"){
  # Calls source on file located in home folder

  source(paste(dir,file,sep=""))
}


## ---------------------------------------------##
##                 FUNC FORM                    ##
##               FOR SOURCING URLS              ##
##       note the difference in envir=(.)       ##
##                                              ##
## ---------------------------------------------##

source.url <- function(...) {
 # load package
 require(RCurl)

 urls <- list(...)
 eval(parse(text=getURL(urls, followlocation=TRUE, cainfo=system.file("CurlSSL", "cacert.pem", package="RCurl"))),
      envir=parent.frame(1))
}
## ---------------------------------------------##




#####%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%########
### ----------------------------------------------------------------------------
###   BATCH ASSIGN
###   
###   Source: 
###     http://strugglingthroughproblems.wordpress.com/2010/08/27/matlab-style-multiple-assignment-in%C2%A0r/
###
###
    # Generic form
    '%=%' = function(l, r, ...) UseMethod('%=%')
###
###
    # Binary Operator
    '%=%.lbunch' = function(l, r, ...) {
      Envir = as.environment(-1)

      if (length(r) > length(l))
        warning("RHS has more args than LHS. Only first", length(l), "used.")

      if (length(l) > length(r))  {
        warning("LHS has more args than RHS. RHS will be repeated.")
        r <- extendToMatch(r, l)
      }

      for (II in 1:length(l)) {
        do.call('<-', list(l[[II]], r[[II]]), envir=Envir)
      }
    }
###
###
    # Used if LHS is larger than RHS
    extendToMatch <- function(source, destin) {
      s <- length(source)
      d <- length(destin)

      # Assume that destin is a length when it is a single number and source is not
      if(d==1 && s>1 && !is.null(as.numeric(destin)))
        d <- destin

      dif <- d - s
      if (dif > 0) {
        source <- rep(source, ceiling(d/s))[1:d]
      }
      return (source)
    }
###
###
    # Grouping the left hand side
    g = function(...) {
      List = as.list(substitute(list(...)))[-1L]
      class(List) = 'lbunch'
      return(List)
    }
###
###

### ----------------------------------------------------------------------------
###
###  TO EXECUTE: 
###    Group the left hand side using the new function `g()`
###    The right hand side should be a vector or a list
###    Use the newly-created binary operator `%=%`
###
###         eg:  g(a, b, c)  %=%  list("hello", 123, list("apples, oranges"))
####
### ----------------------------------------------------------------------------
#####%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%########
