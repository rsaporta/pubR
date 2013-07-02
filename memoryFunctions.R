
# QUICK USAGE: 

## BASIC: 
#  lsos()  ## Show all objects with memory size over 1KB, not including functions

## RESTRICT NUMBER OF OUTPUTS
#  lsos(n=12)  ## Same as above, but just top 12, by `order.by` (default: decreasing size) 

## SPECIAL USAGE: DATA.TABLES
# lsos(DT)    ## show just data.tables.   different from...
# lsos("DT")  ## show any object whose name contains "DT"
# lsos("DT", type="data.table")   ## Show data.tables whos name contains "DT"
# lsos(DT, mb=11)  ## show data.tables whose size is >= 11 MB


## DON'T SHOW CHARACTERS OR LOGICALS.  Any of the following work
#  lsos(type=c("!character", "!logical"))  # ! to indicate not. Quoted vecotr
#  lsos(type=c("!character", "logical"))  # ! on first element, applied to ALL elements
#  lsos(type=c("!character,logical"))  # single quoted string, comma separated, works too

## NOTE: `type` defaults to "!function", but if the desired output is (for example) 
##        'not functions and not characters', then both need to be specified
#  lsos(type="!function,!character")  OR:   lsos(type="!function,character")

# .ls.objects was adapted from Petr Pikal and David Hinds via Dirk Eddelbuettel
# http://stackoverflow.com/questions/1358003/tricks-to-manage-the-available-memory-in-an-r-session


# ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------- #

.ls.objects <- function (pos = 1, pattern, order.by, decreasing=FALSE, head=FALSE, n=5, type="", all.names = FALSE, ignore.case=TRUE) {

  napply <- function(names, fn) 
                   sapply(names, function(x) fn(get(x, pos = pos)))

  names     <- ls(pos = pos, all.names=all.names)
  names     <- grep(pattern, names, value=TRUE, ignore.case=ignore.case)

  # if no names found that match the pattern, return an empty data.frame
  if (!length(names)) {
    return(data.frame(matrix(nrow=0,ncol=4
           , dimnames=list(NULL, c("Size", "Type", "Rows", "Columns")))))
  }

  obj.class <- napply(names, function(x) as.character(class(x))[1])
  obj.mode  <- napply(names, mode)
  obj.type  <- ifelse(is.na(obj.class), obj.mode, obj.class)
  obj.size  <- napply(names, object.size)
  obj.dim   <- t(napply(names, function(x) as.numeric(dim(x))[1:2]))

  vec <- is.na(obj.dim)[, 1] & (obj.type != "function")
  obj.dim[vec, 1] <- napply(names, length)[vec]

  out        <- data.frame(obj.size, obj.type, obj.dim)
  names(out) <- c("Size", "Type", "Rows", "Columns")

  # ordering:  (Note that at this point `Name` is the rownames, and already ordered by, thus we do nothing if Name requested)
  if (!missing(order.by)){

    # check to make sure that the requested column is valid
    if (!(order.by %in% c("Name", names(out)))) {
      warning("Cannot order by ", order.by, " since it is not a valid column name.")
    } else {
      if (order.by == "Name") 
        ordering <- order(tolower(rownames(out)), decreasing=decreasing) 
      else
        ordering <- order(out[[(order.by)]], decreasing=decreasing) 

      out <- out[ordering, ]
    }
  }

  if (head)
      out <- head(out, n)

  # only return objects of type specified
  if(nchar(type) && !is.na(type)) { 

    if (type=="all")
      return(out)

    # split it, to allow for quoted string or vector of quote strings
    type <- unlist(strsplit(type, ","))

    ## TODO: Allow for plural & mis-typings, eg data.frames, dataframes, etc
    type <- gsub("^f$", "function", type, ignore.case=TRUE) 
    type <- gsub("functions", "function", type, ignore.case=TRUE) 
    type <- gsub("dataframe", "data.frame", type, ignore.case=TRUE) 
    type <- gsub("df", "data.frame", type, ignore.case=TRUE) 
    type <- gsub("dt", "data.table", type, ignore.case=TRUE) 


    # look for "!" prefix
    isneg <- grepl("^!", type)

    # negative in **only** the first element is considered negative to all. 
    # negative mixed in other manner is considered positive and issues warning due to ambiguity
    if (isneg[[1]])
      isneg[ ] <- TRUE

    # should not be mixed neg/pos
    if (any(isneg) && !all(isneg)) {
      warning("Cannot mix positive and negative types. Ignoring negative types.")
      out <- out[out$Type %in% type[!isneg], ]
    } else if (all(isneg)) { 
      type <- gsub("^!", "", type)
      out <- out[! out$Type %in% type, ]
    } else
      out <- out[out$Type %in% type, ]
  } # close if( type )

  out
}

# ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------- #
lsos <- function(pattern="", order.by="Size", decreasing=TRUE, n=10, MB=TRUE, KB=TRUE
                  , type="", all.names = FALSE, showfuncs=FALSE, byteMin=b
                  , b=1000, mb=NA, ignore.case=TRUE, functions.returned.normally=FALSE) {
# shorthand wrapper to .ls.objects
# cleaner output and uses data.table if available
#
#  b  : minimum amount of bytes
#  mb : alternatively, the minimum amount of megabytes
# MB / KB : logical.  Should Size be converted to either of these. MB superscedes KB
# type :  Which type of objects to display or not display

  # for if statement later
  byteMinMissing <- missing(byteMin)

  if (!missing(mb))
    byteMin <- mb * 1e6

  if(missing(type) && !showfuncs)
    type <- "!function"

  # ------------------------------------------ #
  #    Allowing for flexibility in order.by    #
  # ------------------------------------------ #
  if(!missing(order.by)) {
    # Check if order.by can be coerced to character. If not, then substitute its value. (If that still fails, R will throw an error.)
    if (isErr(order.by.new <- try(as.character(order.by), silent=TRUE)))
      order.by <- as.character(substitute(order.by))
    else 
      order.by <- order.by.new
  }

  # The 'Size' column is sometimes displayed as MB or KB, but order.by should still be "Size"
  if (order.by=="Kb" || order.by=="Mb")
    order.by <- "Size"
  # ------------------------------------------ #


  # ------------------------------------------ #
  # This allows for calls such as lsos("check", f) "show all functions with the name check"
  if (order.by == "F" || substr(order.by, 1, 3) == "Fun") {
    order.by <- "Name"
    type <- "function"
  }

  # ------------------------------------------ #
  if(!missing(type)) {
    # Check if type can be coerced to character. If not, then substitute its value. (If that still fails, R will throw an error.)
    if (isErr(type.new <- try(as.character(type), silent=TRUE)))
      type <- as.character(substitute(type))
    else 
      type <- type.new
  }

  if (missing(order.by) && tolower(type) %in% c("f", "fun", "func", "functions", "function")) {
    order.by <- "Name"
  }


  ## This has to come after any checks for `missing(order.by)`
  # All column names are proper-caps and hence so should order.by 
  order.by <- topropper(order.by)


  # change default of `decreasing` when `order.by` is "Name" 
  if ( missing(decreasing) && order.by=="Name")
    decreasing <- FALSE

  mc <- match.call()
  # if only one argument, and not a character
  if (length(mc) > 1 && is.name(mc[[2]])) {
    pat <- as.character(mc[[2]])

    if (pat == "DT" && type=="!function") {
      pattern <- ""
      type    <- "data.table"

    # check if the input is a variable containing a valid pattern (ie a character). If so, do nothing. 
    #          NOT:     Is it a variable?          if so,       Is the value a character
    } else if (! (exists(pat, envir =parent.frame()) && is.character(get(pat, envir=parent.frame())))) {
      pattern <- pat
    }
    # else, do nothing  
  }


  out <- .ls.objects(pattern=pattern, order.by=order.by, n=n, type=type, decreasing=decreasing, all.names=all.names, head=!missing(n), ignore.case=ignore.case)

  # no objects found
  if (nrow(out)==0) {
    warning("\n\tNo objects of selected type in memory.\n\tTry argument:  lsos(type=\"all\") ")
    return(NA)
  }

  # Add a column for lists, indicating their type
  if (any(out.l <- out$Type=="list")) {
    out$ListContent <- ""
    out[out.l, "ListContent"] <- sapply(rownames(out[out.l, ]), function(x) listType(get(x)) )
  }

  # replace NA `Columns` value with blank
  out[is.na(out[, "Columns"]), "Columns"] <- ""


  # as long as byteMin is a valid number
  suppressWarnings(byteMin <- as.numeric(byteMin))
  if(!(is.null(byteMin) || is.na(byteMin))) {
    aboveSize <- out$Size > byteMin
    
    # no values larger than requested limit
    if(!any(aboveSize)) {
      if (!byteMinMissing) 
        warning("No object is larger than ", ifelse(missing(mb), paste(byteMin, "bytes"), paste(byteMin / 1e6, "MB")), ". Displaying all objects of (un)selected type(s).")
    } else {
      out <- out[aboveSize, ]
    }
  }

  if (missing(MB) && missing(KB)) { 
    maxsize <- max(as.numeric(as.character(out[["Size"]])))
    if (maxsize < 1e3){
      MB <- KB <- FALSE
    } else if (maxsize >= 1e3 && maxsize < 1e6) {
      KB <- TRUE
      MB <- FALSE
    } else {
      MB <- TRUE
    }
  }

  # Note that "MB" will be appended row-by-row to each size
  if (MB) {
    out[, "KB"] <- formatKB(out[, "Size"] / 2^10, MB=TRUE)
    out <- out[ , c("KB", setdiff(names(out), c("KB", "Size")))]
  }

  if (KB & !MB)  {
    out[, "Size"] <- formatKB(out[, "Size"] / 2^10)
    names(out)[names(out) == "Size"] <- "KB"    
  }

  # if returning only function, return that as a vector, to show them all
  if (all(out[, "Type"] == "function") && !functions.returned.normally)
    return( invisible(print(rownames(out)[order(toupper(rownames(out)))], quote=FALSE))  )

  if (!exists("data.table"))
    return(out)
  # else
  data.table(Name=rownames(out), out)
}
# ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------- #

listType <- function(L) { 
  are <- lapply(L, is)

  are <- unique(are)

  if (length(are) == 1)
    return(are[[1]][[1]])

  if (length(are) == 2)
    return( paste( are[[1]][[1]], "&", are[[2]][[1]])  )

  return("Mixed")
}

# ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------- #

formatKB <- function(x, MB=FALSE, MBthresh=600) { 
# This is a quick and dirty function to convert bytes to KB
# for cleaner output
  ret <- x
  for (i in 5:(-2))
    ret[x < 5 * 10^(i)] <- round( x[x < 5 * 10^(i)],  -(i)+1)

  if (MB) {
    mb <- ret[ret>MBthresh]/1000
    ret[ret>MBthresh] <- ifelse(mb > 100, paste(round(mb), "MB"), paste(round(mb, 1), "MB"))
  }
  prettyNum(ret)
}

