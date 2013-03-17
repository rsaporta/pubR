cordl <- function(..., length=NULL, justSize=FALSE, crop=TRUE, chop=TRUE)  {
  # Capture Output, Remove Duplicate Lines, wrapper function.
  # Will run paraLineChop unless either of crop or chop are FALSE 
  #  crop and chop serve the same purpose.  Allowing for synonyms 
  #  for forgetful programmers. 

  ret <- rmDupLines(capture.output(eval(substitute(...))))

  if (!crop)
    return(ret)

  return(paraLineChop(ret, length=length, justSize=justSize))
}


paraLineChop <- function(so, length=NULL, lines=NULL, justSize=FALSE) {
# chops up the lines in a capture.ouput paragraph to length
# so is some output from capture.output
#
# if justSize, then will output value of chop and how many lines will be chopped

    # if user provided a length, use that. Else calculate it as a weighted average
    if (!is.null(length)) {
        chop <- length
        feather <- 0
    } else {
        feather <- 5
        lngs <- sort(nchar(so))
        lngs <- lngs[!lngs == 0]
        
        # we want to trim, but only if there is something to tirm
        L <- length(lngs)
        trm <- ceiling(max(1, .15*L, .08*L))

        weigtd <- mean(lngs[-(1:trm)])
        chop <- round(mean(c(lngs[L-trm], mean(lngs[-(1:trm)])))) + 6
    }

    # if no value for chop determined
    if (is.na(chop)) {
        warning("couldnt chop")
        return(so)
    }

    # determine which lines need cropping
    lines <- nchar(so) > chop 

    if (justSize)
        return (c(lines=sum(lines), chop=chop))

    # if there are no lines to crop, return the thing now
    if(!any(lines))
        return(so)

    matches <- regexpr(" ", substr(so[lines], chop-11, chop+feather))
    
    # TODO:  Deal with NA by chopping at chop-1, then adding a hyphon
    matches[matches<0] <- NA

    # Mark the specific spot in each line where the chop will happen    
    markers <- chop-11 + matches

    # 2nd Halfs
    sublines <- substr(so[lines], markers, nchar(so[lines]))

    # add some tabs
    sublines[nchar(sublines) < (chop - 8)] <- paste0(tbs(2), sublines[nchar(sublines) < (chop - 8)])
    sublines[nchar(sublines) < (chop - 4)] <- paste0(tbs(1), sublines[nchar(sublines) < (chop - 4)])

    # 1st Halfs
    so[lines] <- substr(so[lines], 1, markers-1)

    numbLines <- length(sublines)
    for (j in seq(numbLines)) {

        antij <- numbLines - j +1
        i <- which(lines)[[antij]]
        tail <- seq(i+1, length(so))

        # error prevention for the last line, so that we dont have leng:(leng+1)
        if (i == length(so))
            tail <- i

        so[tail+1] <- so[tail]
        so[i+1]    <- sublines[[antij]] 
    }

    # if any long lines remain, recurse
    if (any(nchar(so) > chop))
        return(paraLineChop(so, length=chop))
    
    return(so)
}
