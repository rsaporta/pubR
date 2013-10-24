# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #
#  
#  ### USAGE: 
#  
#  ## data.frame
#    set.seed(1)
#    DF <- data.frame("ID"=sample(LETTERS[1:7], 120, TRUE), "SERIES" = sample(seq(1000, 9999, 200), 120, TRUE), replicate(12, round(rnorm(120), 3)) )
#  
#    # simple call
#    reproduce(DF)
#    
#  
#    # notice the difference between `head` and `rows` (see the last few rows in the sample)
#    reproduce(DF, head=12)
#    reproduce(DF, rows=12)
#  
#    reproduce(DF, rows=12, shuffle=TRUE)
#  
#  
#    # sample by column name
#    reproduce(DF, cols=c("ID", "X4", "X10"))
#  
#    # sample by column number. (a single number is interpreted via `seq(cols)` )
#    reproduce(DF, cols=c(1, 6, 12))
#    reproduce(DF, cols=6)
#  
#    # `head` and `shuffle` apply only to rows, not to columns
#    reproduce(DF, cols=4, rows=3)
#    reproduce(DF, cols=4, head=3)
#    reproduce(DF, cols=4, shuffle=TRUE)
#  
#    # `head` superscedes `shuffle` and `rows``
#    reproduce(DF, cols=4, head=3, rows=100, shuffle=TRUE)  
#    reproduce(DF, cols=4, rows=3, shuffle=TRUE)  
#  
#  
#    # if the whole object is desired, shortcut call: 
#    reproduce(DF, whole)
#    reproduce(DF, whole=TRUE)
#    identical(reproduce(DF, whole), reproduce(DF, whole=TRUE))
#  
#  ## data.table
#    DT <- data.table(DF, key=c("ID", "SERIES"))
#  
#    # notice that with data.table, the row numbers apply to the sample, not to the original source. 
#    # However, examine the values of ID and SERIES and observe they are different. 
#    reproduce(DT, head=12)
#    reproduce(DT, rows=12)
#    reproduce(DT, rows=12, shuffle=TRUE)
#  
#  
#    reproduce(DT, rows=12, shuffle=TRUE)
#    reproduce(DT, cols=4, rows=12, shuffle=TRUE)
#  
#    # head takes priority
#    reproduce(DT, cols=4, rows=12, head=7, shuffle=TRUE)
#  
#  
#  ## OTHER DATA TYPES
#  
#  ## lists
#    myList <- list(x=1:5, "hello", "Abc", 1:7, c("a", "b", "c"), c("last", "element", "in", "the", "list"))
#    reproduce(myList)
#  
#    reproduce(myList, 2)
#  
#    # Shuffle not implemented on lists.  Instead use sample
#    set.seed(7)
#    reproduce(myList, 2, shuffle=TRUE)
#    reproduce(sample(myList, 2), 7, head=5,  name="myList")
#  
#
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #
# -------------------------------------------------------------------------------------------------------------------------------------------- #

# ## Update Oct 2013: 
# argument `lines.out` has been added. 
# The user can specify the total number of lines that the output should be broken up into
# The function attempts to add line breaks at logical points, but this is purely a buest guess based on regex. 
# 
# example:   reproduce(DF, lines.out=4)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #
# -------------------------------------------------------------------------------------------------------------------------------------------- #






# ---------------------------------------------------------- #
#                                                            #
#                    FUNCTIONS START HERE                    #
#                                                            #
# ---------------------------------------------------------- #




## You can copy and paste the output, or 
## if on  Mac OS X, the output will be automaticall copied to the clipboard 
##    ie, just run the function, then hit CMD+v  in your text editor or website.

reproduce <- function(x, rows=10, head=NA, cols=NA, clipboard=TRUE, whole=FALSE
                      , shuffle=FALSE, name=NA, verbose=!silent, silent=FALSE, pos=1
                      , lines.out=1) { 
# If rows is a single number, then that is *how many* rows will be selected
# If rows is a vector of numbers, then that is *which rows* will be selected
# Same for cols.  Note, vlaue can be integer or character
#
# This function is data.table aware in that it handles data.table differently from data.frame

  # grab the name of x from the previous env. 
  mc <- match.call()

  if (missing(name)) {
    mc2 <- mc[[2]]

    while(is.call(mc2))
      mc2 <- mc2[[2]]

    name <- as.character(mc2)
  }
  d <- dim(x)


  # allows for the call `reproduce(x, whole)` and setting whole to TRUE and rows to default. 
  w <- tolower(if(length(mc) >2) as.character(mc[[3]]) else (NA))
  if (!is.na(w) && (w=="whole" || w=="hole" || w=="all")){
    whole <- TRUE
    rows  <- 10
  }

  ## CHECK IF LIST
#  if(is.list())

  # check that rows & cols do not exceed the dims.  If so, scale them back
  #------------------------
    # check that x has dim
    if (!is.null(d)) {
      # check if `rows` exceeds rows(x)
      if (rows > d[[1]])
        rows <- d[[1]]
      # check if x has cols, and `cols` has been set and `cols` exceeds cols(x)
      if (length(d) > 1 && !is.na(cols) && cols > d[[2]])
        cols <- d[[2]]
    }

  ## SAMPLE THE ROWS
  # if x has dimension && user did not flag for the whole thing
  #  we will take only a sampling
  if (!whole  && length(d)) {

    if (!missing(head) && !is.na(head) && missing(rows)) {
      x <- x[1:(min(nrow(x), head)), ]
    } else if (length(rows) || d[[1]] > rows) {
      # check for human error
      if (!missing(head))
        warning("Both `head` and `rows` has been selected. `head` will be ignored, only `rows` used.")
      
      # if rows has length, then select those specific rows
      if (length(rows) > 1) {
          # make sure that rows are all present in x
          if(is.character(rows))
            rows <- setdiff(rows, names(x))
          if (is.numeric(rows))
            rows <- rows[rows <= d[[1]]]
          x <- x[rows, ]

      # rows is a single number, and that is how many rows to select.
      } else {
        # Sample randomly
        if (shuffle) { 
          x <- x[sample(nrow(x), rows, FALSE), ]
        
        # rows of 1 or 2 messes up the arithmetic equation being used as an index 
        } else if (rows < 3) { 
          x <- x[1:rows, ]

        # If not shuffling, use 60% of the value of rows to grab from the head and the remaing rows to come form the tail
        } else {
          he <- ceiling(rows * 0.62)
          tl <- rows - he
          x  <- x[c(1:he, 1+d[[1]]-(tl:1)), ]
        }
      }
    }
  }

  # select only certian columns, assuming cols specified and x has dimension
  if (!missing(cols) && !is.na(cols) & length(d)) {
    if (is.numeric(cols) && length(cols)==1) {

      if (cols > d[[2]])
        cols <- d[[2]]
      
      cols <- seq(cols)
    }
    if (is.data.table(x)) {      
      x <- x[, cols, with=FALSE]
    } else if (length(d) > 2) {
      indx <- as.list(c(NA, TRUE, NA, rep(TRUE, d[[2]] - 2)))
      indx[[3]] <- cols
      indx[[1]] <- x3
      x <- do.call(`[`, indx)
    } else {
      x <- x[, cols]
    }
  }

  ## Sample from single dim'd objects
  if (!length(d) && length(x)>1) {
    L <- length(x)
    if (!missing(head)) {
      x <- x[seq(min(L, head))]
    } else if (!missing(rows)) {

      # if rows has length, then take those specific elements
      if (length(rows)) {
        # make sure that "rows" are all present in x            
        if(is.character(rows))
          rows <- setdiff(rows, names(x))
        if (is.numeric(rows))
          rows <- rows[rows <= L]
        # then sample
        x <- x[rows]

      # .. otherwise take the first rows-many elements
      } else {
        rows <- min(rows, L) # make sure rows does not exceed length(x)
        # Sample randomly
        if (shuffle) { 
          x <- x[sample(L, rows, FALSE), ]
        # sample head and tail
        } else {
          he <- ceiling(rows * 0.62)
          tl <- rows - he
          x  <- x[c(1:he, (L+1)-(tl:1)), ]
        }
      }

    }
  }

  ## GRAB THE DPUT
  z <- capture.output(dput(x))
  ## remove white space, just because it takes up room
  z <- gsub("^\\s+|\\s+$", "", z)
  # collapse it all into a single string
  z <- paste(z, collapse="")

  
  # data.table: 
  if (is.data.table(x)) {

    # grab the key if there is one, so as to preserve it in the output
    k  <- key(x)
    ky <- if (is.null(k)) "" else paste0(", key='", paste(k, collapse=","), "'")

    # enclose in data.table
    z <- paste0("data.table(", z, ky, ")")
  }

  ## this is the final output
  ret <- paste0(name, " <- ", z)

  # remove pointer info (specific to data.table, but might be caught in a list)
  pattern <- ", \\.internal\\.selfref \\= <pointer\\: [a-zA-Z0-9]{8,12}>"
  ret <- gsub(pattern, "", ret)

  ## Chops the output into lines.  Attempts to break lines at reasonable points
  ##   Also indents subsequent lines to match up after the `<-`, assuming the data name
  ##    is less than 32 chars long
  if (lines.out > 1 && nchar(ret) > 125) {
    nc.start <- 2+regexpr("<\\-", ret)[[1]]
    if (nc.start > 35)
      nc.start > 0
    nc <- nchar(ret) - nc.start
    widths <- (nc / lines.out)

    for (i in seq(lines.out-1))
      ret[i:(i+1)] <- 
         chopLine(ret[i], width=widths, flex=widths, splitOn = "\\), \\w+ = ", maxNumberOfBreaks=1L, collapse=NULL, padToSecondSpace=FALSE)

    ret[2:length(ret)] <- paste0(pasteR(" ", nc.start), ret[2:length(ret)])
    ret <- paste(ret, collapse="\n")
  }

  # if on Mac OSX and flagged to true, then copy to clipboard
  if(clipboard &&  Sys.info()[['sysname']] == "Darwin") {
    con <- pipe("pbcopy", "w")
    writeLines(ret, con)
    close(con)
  }

  if (verbose) {
    cat("\n\n\n\nThis is what the sample looks like: \n\n")
    print(x, quote=FALSE)
    cat("\n\n")

    ln <- function() cat("    ==X", rep("=", 62), "X==\n", sep="")
    ln()
    cat({if (clipboard) "" else rep(" ", 18)}, sep="")
    cat("         Copy+Paste this part.", ifelse(clipboard, "(If on a Mac, it is already copied!)\n", "\n")) 
    ln()
    cat("\n", ret, "\n\n")
    ln()
  }

  # also return invisibly
  return(invisible(name))
}



# ------------------------------------------------------ #

## Utils functions 

## These functions are normally kept in another file. Hence only source them from here if absent
if (!exists("chopLine")) {

  chopLine <- function(line, collapse="\n", width=88L, flex=15L
                                , padding=0L, maxNumberOfBreaks=2L, dotsBeyondMax=FALSE
                                , padToSecondSpace=FALSE
                                , trimSpace=TRUE
                                , showWarnings=TRUE
                                , splitOn="\\s+") {
  ## Takes a line of text and breaks it up into width
    if (!is.atomic(line) || length(line) > 1)
      stop ("`line` should be an atomic vector of length exactly 1.")

    if (!length(line) || nchar(line) < width)
      return(line)

    if (padToSecondSpace) {
      if (is.character(padding)) {
            warning("`padding` given explicitly as a string, but `padToSecondSpace` is set to TRUE.\nThis will overwrite `padding`.")
            padding <- 0
        }
        # find the first letter of the second word. Then go back one space from there. (We want the END of the second space)
        secondspace <- attr(regexpr("(\\s*)([^\\s]+)(\\s+)([^\\s])", line, perl=TRUE), "match.length")[[1]] - 1
        padding <- max(secondspace, padding)
    }

    if (is.numeric(padding))
      padding <- pasteR(" ", padding)

    ## NOTE dont pre allocate. Otherwise would then need to check for blank lines.
    ret <- line

    # dont chop if maxNumberOfBreaks==0
    if (maxNumberOfBreaks)
      for (i in seq.int(maxNumberOfBreaks)) {
        ## only continue if enough chars
        if (nchar(ret[[i]]) < width + (.6*flex))
          break

        spaces <- findCharBetween(ret[[i]], char.to.find=splitOn, from=width-(3*flex), to=width+(4*flex))
        if (!length(spaces))
          bk <- width
        else 
          bk <- spaces[which.min(abs(spaces - width))]
        current <- substr(ret[[i]], 1, bk- (splitOn=="\\s+") )  # if not splitting on space, do not remove it
        new     <- substr(ret[[i]], bk+1, nchar(ret[[i]]))

        # add a hyphen if mid-work break
        if (!length(spaces))
          current <- paste0(current, "-")

        # clean whitespace
        if (trimSpace)
          new <- gsub("^\\s+", "", new)

        ret[[i]]   <- current
        ret[[i+1]] <- paste0(padding, new)
      }

    ## add dots
    L <- length(ret)
    if (dotsBeyondMax && nchar(ret[[L]]) > width) {
      ret[[L]] <- paste0(substr(ret[[L]], 1, width-3), " ...")
    }

    return(paste(ret, collapse=collapse))
  }
}


if (!exists("pasteR"))  {

  pasteR <- function(x="-", n) {
  ## Repeats x n-times in a flat string
  ##  When n is non-positive, a blank string, "", is returned
  

    ## allow for `pasteR(n)`
    if (missing(n) && is.numeric(x)) {
      n <- x
      x <- "-"
    }

    # if n is not a single number, iterate
    if (length(n) > 1) {
      if (length(n) == length(x))
        return(mapply(pasteR, x, n))
      return( sapply(n, function(n1) pasteR(x, n1)) )
    }

    # use the chararacter length of n, if it is a string
    if(is.character(n))
      n <- nchar(n)

    # Negative values are considered 0. 
    n[n<0] <- 0

    # otehrwise, simple return
    pasteC(rep(unlist(x), n))
  }

}

