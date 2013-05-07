# BASIC USAGE: 
#  Call the lsos() function, which wraps .ls.objects()
#  lsos()
#  lsos(n=23)
#  lsos(n=23, type=c("!function", "!logical"))


# From Petr Pikal and David Hinds 
# via Dirk Eddelbuettel
# http://stackoverflow.com/questions/1358003/tricks-to-manage-the-available-memory-in-an-r-session

# improved list of objects
.ls.objects <- function (pos = 1, pattern, order.by, decreasing=FALSE, head=FALSE, n=5, type="") {

  napply <- function(names, fn) 
              sapply(names, function(x) fn(get(x, pos = pos)))
  names     <- ls(pos = pos, pattern = pattern)
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

    # look for "!" prefix
    isneg <- grepl("^!", type)

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

# shorthand, edited for data.table use
lsos <- function(..., n=10, KB=TRUE, type="", showfuncs=FALSE, byteMin=1000) {

  if(missing(type) && !showfuncs)
    type <- "!function"

  out <- .ls.objects(..., order.by="Size", decreasing=TRUE, head=!missing(n), n=n, type=type)

  # as long as byteMin is a valid number
  suppressWarnings(byteMin <- as.numeric(byteMin))
  if(!(is.null(byteMin) || is.na(byteMin))) {
    aboveSize <- out$Size > byteMin
    
    # no values larger than requested limit
    if(!any(aboveSize)) {
      if (!missing(byteMin)) warning("No object is larger than ", byteMin, " bytes. Displaying all objects of (un)selected type(s).")
    } else {
      out <- out[aboveSize, ]
    }
  }

  if (KB)  {
    out[, "Size"] <- formatKB(out[, "Size"] / 2^10)
    names(out)[names(out) == "Size"] <- "KB"    
  }

  if (!exists("data.table"))
    return(out)
  # else
  data.table(Name=rownames(out), out)
}

formatKB <- function(x, MB=FALSE) { 
  ret <- x
  for (i in 5:(-2))
    ret[x < 5 * 10^(i)] <- round( x[x < 5 * 10^(i)],  -(i)+1)

  if (MB)
    ret[ret>1000] <- paste(round(ret[ret>1000]/1000), "MB")
  
  prettyNum(ret)
}

