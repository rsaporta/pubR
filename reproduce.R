
### USAGE: 

## data.frame
  set.seed(1)
  DF <- data.frame("ID"=sample(LETTERS[1:7], 120, TRUE), "SERIES" = sample(seq(1000, 9999, 200), 120, TRUE), replicate(12, round(rnorm(120), 3)) )

  # simple call
  reproduce(DF)
  

  # notice the difference between `head` and `rows` (see the last few rows in the sample)
  reproduce(DF, head=12)
  reproduce(DF, rows=12)

  reproduce(DF, rows=12, shuffle=TRUE)


  # sample by column name
  reproduce(DF, cols=c("ID", "X4", "X10"))

  # sample by column number. (a single number is interpreted via `seq(cols)` )
  reproduce(DF, cols=c(1, 6, 12))
  reproduce(DF, cols=6)

  # `head` and `shuffle` apply only to rows, not to columns
  reproduce(DF, cols=4, rows=3)
  reproduce(DF, cols=4, head=3)
  reproduce(DF, cols=4, shuffle=TRUE)

  # `head` superscedes `shuffle` and `rows``
  reproduce(DF, cols=4, head=3, rows=100, shuffle=TRUE)  
  reproduce(DF, cols=4, rows=3, shuffle=TRUE)  


  # if the whole object is desired, shortcut call: 
  reproduce(DF, whole)
  reproduce(DF, whole=TRUE)
  identical(reproduce(DF, whole), reproduce(DF, whole=TRUE))

## data.table
  DT <- data.table(DF, key=c("ID", "SERIES"))

  # notice that with data.table, the row numbers apply to the sample, not to the original source. 
  # However, examine the values of ID and SERIES and observe they are different. 
  reproduce(DT, head=12)
  reproduce(DT, rows=12)
  reproduce(DT, rows=12, shuffle=TRUE)


  reproduce(DT, rows=12, shuffle=TRUE)
  reproduce(DT, cols=4, rows=12, shuffle=TRUE)

  # head takes priority
  reproduce(DT, cols=4, rows=12, head=7, shuffle=TRUE)


## OTHER DATA TYPES

## lists
  myList <- list(x=1:5, "hello", "Abc", 1:7, c("a", "b", "c"), c("last", "element", "in", "the", "list"))
  reproduce(myList)

  reproduce(myList, 2)

  # Shuffle not implemented on lists.  Instead use sample
  set.seed(7)
  reproduce(myList, 2, shuffle=TRUE)
  reproduce(sample(myList, 2), 7, head=5,  name="myList")

  

## You can copy and paste the output, or if on a mac, you can 
## If on a Mac, hit CMD+V in any editing window. 

reproduce <- function(x, rows=10, head=NA, cols=NA, clipboard=TRUE, whole=FALSE, shuffle=FALSE, name=NA, verbose=!silent, silent=FALSE, pos=1) { 

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
  w <- if(length(mc) >2) as.character(mc[[3]]) else (NA)
  if (!is.na(w) && (tolower(w)=="whole" || tolower(w)=="hole")){
    whole <- TRUE
    rows  <- 10
  }

  ## SAMPLE THE ROWS
  # if x has dimension && user did not flag for the whole thing
  #  we will take only a sampling
  if (!whole  && length(d)) {

    if (!missing(head) & !is.na(head)) {
      x <- x[1:(min(nrow(x), head)), ]
    } else if (d[[1]] > rows) {
      # Sample randomly
      if (shuffle) { 
        x <- x[sample(nrow(x), rows, FALSE), ]
      } else {
        he <- ceiling(rows * 0.62)
        tl <- rows - he
        x  <- x[c(1:he, 1+d[[1]]-(tl:1)), ]
      }
    }
  }

  # select only certian columns
  if (!missing(cols) && !is.na(cols)) {
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
      x <- x[seq(min(L, rows))]
    }
  }

  ## GRAB THE DPUT
  z <- capture.output(dput(x))
  z <- paste(z, collapse="")


  # data.table: 
  if (is.data.table(x)) {

    # grab the key if there is one, so as to preserve it in the output
    k  <- key(x)
    ky <- ifelse(is.null(x), "", paste0(", key='", paste(k, collapse=","), "'") )

    # remove the pointer
    pattern <- ", .internal.selfref = <pointer: .*>"
    z <- gsub(pattern, "", z)
    z <- paste0("data.table(", z, ky, ")")
  }

  ## this is the final output
  ret <- paste0(name, " <- ", z)

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

    ln <- function() cat("    ==X", rep("=", 61), "X==\n", sep="")
    ln()
    cat("       This is copy+paste'able. (If on a Mac, it is already copied)\n") 
    ln()
    cat("\n", ret, "\n\n")
    ln()
  }

  # also return invisibly
  return(invisible(name))
}

