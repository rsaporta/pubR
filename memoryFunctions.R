
# QUICK USAGE: 

## BASIC: 
#  lsos()  ## Show all objects with memory size over 1KB, not including functions

## RESTRICT NUMBER OF OUTPUTS
#  lsos(n=12)  ## Same as above, but just top 12, by `order.by` (default: decreasing size) 

## DON'T SHOW CHARACTERS OR LOGICALS.  Any of the following work
#  lsos(type=c("!character", "!logical"))  # ! to indicate not. Quoted vecotr
#  lsos(type=c("!character", "logical"))  # ! on first element, applied to ALL elements
#  lsos(type=c("!character,logical"))  # single quoted string, comma separated, works too

## NOTE: `type` defaults to "!function", but if the desired output is (for example) 
##        'not functions and not characters', then both need to be specified
#  lsos(type="!function,!character")  OR:   lsos(type="!function,character")

# .ls.objects was adapted from Petr Pikal and David Hinds via Dirk Eddelbuettel
# http://stackoverflow.com/questions/1358003/tricks-to-manage-the-available-memory-in-an-r-session

.ls.objects <- function (pos = 1, pattern, order.by, decreasing=FALSE, head=FALSE, n=5, type="", all.names = FALSE) {

  napply <- function(names, fn) 
              sapply(names, function(x) fn(get(x, pos = pos)))
  names     <- ls(pos = pos, pattern = pattern, all.names=all.names)
  obj.class <- napply(names, function(x) as.character(class(x))[1])
  obj.mode  <- napply(names, mode)
  obj.type  <- ifelse(is.na(obj.class), obj.mode, obj.class)
  obj.size  <- napply(names, object.size)
  obj.dim   <- t(napply(names, function(x) as.numeric(dim(x))[1:2]))

  vec <- is.na(obj.dim)[, 1] & (obj.type != "function")
  obj.dim[vec, 1] <- napply(names, length)[vec]

  out        <- data.frame(obj.size, obj.dim, obj.type)
  names(out) <- c("Size", "Rows", "Columns", "Type")
 
  if (!missing(order.by))
      out <- out[order(out[[order.by]], decreasing=decreasing), ]
  if (head)
      out <- head(out, n)

  # only return objects of type specified
  if(nchar(type) && !is.na(type)) { 

    if (type=="all")
      return(out)

    # split it, to allow for quoted string or vector of quote strings
    type <- unlist(strsplit(type, ","))

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

# shorthand wrapper to .ls.objects
# cleaner output and uses data.table if available
lsos <- function(..., n=10, MB=TRUE, KB=TRUE, type="", showfuncs=FALSE, byteMin=b, b=1000) {

  # for if statement later
  byteMinMissing <- missing(byteMin)

  if(missing(type) && !showfuncs)
    type <- "!function"

  out <- .ls.objects(..., order.by="Size", decreasing=TRUE, head=!missing(n), n=n, type=type)

  if (nrow(out)==0) {
    warning("\n\tNo objects of selected type in memory.\n\tTry argument:  lsos(type=\"all\") ")
    return(NA)
  }

  # as long as byteMin is a valid number
  suppressWarnings(byteMin <- as.numeric(byteMin))
  if(!(is.null(byteMin) || is.na(byteMin))) {
    aboveSize <- out$Size > byteMin
    
    # no values larger than requested limit
    if(!any(aboveSize)) {
      if (!byteMinMissing) 
        warning("No object is larger than ", byteMin, " bytes. Displaying all objects of (un)selected type(s).")
    } else {
      out <- out[aboveSize, ]
    }
  }

  if (MB) {
    out[, "KB"] <- formatKB(out[, "Size"] / 2^10, MB=TRUE)
    out <- out[ , c("KB", setdiff(names(out), c("KB", "Size")))]
  }


  if (KB & !MB)  {
    out[, "Size"] <- formatKB(out[, "Size"] / 2^10)
    names(out)[names(out) == "Size"] <- "KB"    
  }


  if (!exists("data.table"))
    return(out)
  # else
  data.table(Name=rownames(out), out)
}

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

