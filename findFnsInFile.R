# These functions are used to read my .r files and 
# produce a list of all the functions in that file


#-----------------------------------------------------------------#
  # EXAMPLE OF USAGE
  #----------------#
    # f <- "~/git/misc/rscripts/utilsRS.r"
    # f <- "~/git/misc/rscripts/Confusion Matrix.R"
    # f <- "~/git/misc/rscripts/mini.R"

    # findFnsInFile(f)
    # printFunctList(f, "")
#-----------------------------------------------------------------#


printFunctList <- function(fnlist, pre="# ") {
  if (inherits(fnlist, "file") || (length(f)==1 && grepl("\\.[rR]$", fnlist)))
    fnlist <- findFnsInFile(fnlist)  
  cat("\n", paste(pre, fnlist, "\n"), "\n")
}

#------------

findFnsInFile <- function(f) {
# find any named (non-annonymous) functions in a file

  library(stringr)  # for findLastBracket

  # If f is not a connection (ie a string of file path), open file connection
  if (! inherits(f, "file"))
    f <- file(as.path(f), open="rt")

  # read in individual lines, collapse into a single string
  tlines <- readLines(f)
  close(f)

  # remove all comments
  tlines <- gsub("\\s*#.*", "", tlines)
  # remove blank lines
  tlines <-  tlines[tlines != ""]
  # collapse into a single string
  tlines <- paste0(tlines, collapse=" ")


    # SOME EXAMPLE OF WHAT I NEED TO FIND
    # --------------------------------- #
    # testString <- c("<- function(",
    # "<- function (",
    # "<- function 
    # (",
    # "NOT < -function(")

  # THESE ARE THE REGEX PATTERNS TO FIND FUNCTIONS
  #               first var name      (s  <- s second var name)*  s  <- s
  vname <- "\\b[.A-Za-z][.A-Za-z0-9$]*(\\s*<-\\s*[.A-Za-z0-9$]*)*\\s*<-\\s*"
  funct <- "function\\s*\\(" 
  # TODO find `excep %=% tions`  ie "^'{1}[pattern1]'$" 
  # TODO add in  equal sign


      # using regexc instead of stringr
      # inds <- sapply(regexec(paste0(vname, funct), tlines), function(x) c(st=x[[1]], en=abs(x[[1]])-1+attr(x,"match.length")[[1]])); 

  # grab the nchar location of the start of each function name(s) 
  inds <- data.table(str_locate_all(pattern=paste0(vname, funct), string=tlines)[[1]], key="start")
  setnames(inds, "end", "parenStart")
  # find the closing bracket for each
  inds[, end:=findLastBracket(tlines, parenStart), by=start]
  #  TODO:  instead of calling 'findLastBracket' 20+times, 
  #         call it once, create a list of all closing brackets
  #         then for each "start" grab the min(which(closingBracket > start))

  # close file and return
  allFuncs <- str_sub(tlines, inds$start, inds$end)

  Names_N_Args <- gsub("\\s*(<-|=)\\s*function\\s*\\(", " \\( ", allFuncs)
  # add a space before last bracket
  Names_N_Args <- gsub("\\)$", " \\)", Names_N_Args)
  return(Names_N_Args)

}

#------------

findLastBracket <- function(string, start, nchars=100) {
  # not using argument nchars for now
  # this function depends on library(stringr), which is called in the previous env.
  # library(stringr)

  # ditch everything prior to start, then count brackets
  string        <- str_sub(string, start, -1);
  openBrackets  <- str_locate_all(string, "\\(")[[1]][,1]
  closeBrackets <- str_locate_all(string, "\\)")[[1]][,1]

    ## TODO something like this, with a paren inside a quote will throw off the count 
    # objNames <- function( x= if(substr(ob, 1, 5)=="eval(")   eval(parse(text=substr(ob, 6, nchar(ob)-1)), envir=parent.frame(pos+1))  else  ob ))
    # 
    #  I can probably resolve this if implementing a single count of parens for the whole file
    #  I can use the same counting mechanism to determine when something is within quotes, and ignore it
    
    # temp work around for '=="eval(")' ~~> ')'     
    errd <- regexpr('==\\"eval\\(\\"\\)', string) + 7
    openBrackets <- openBrackets[openBrackets!=errd]


  # whenever CL[n] > OP[n+1], then n is the index of a closing paren 
  #  We simply want to find the lowest such n. 

  # things to watch out for:  uneven sizes in OP & CL
  len.OP <- length(openBrackets)
  len.CL <- length(closeBrackets)

  if (len.OP < len.CL) 
    warning("Too MANY closing parens following '", substr(string, 1, 10), "'.")

  # We can deal with too many closing parens, but not with too few. 
  if (len.OP - len.CL > 2)
    stop("There are too many missing closing parens. Cannot continue.")

  if (len.OP - len.CL == 1) {
    warning("A closing paren is missing following '", substr(string, 1, 10), "'.")
    closeBrackets <- c(closeBrackets, nchar(string))
  }

    # [58] "allPosCombsList ( dat, choose=seq(ncol(dat)), yName=\"y\" )"
    # [59] "allPosCombsMatrix.TakesTooLong ( dat, choose=-1 )" 
    # Here is an example of two lines from a file. Notice that the location of 
    #   of the last paren in [58] is greater than all the open parens in [58]
    #   but it is less than the next opening paren, ie in line [59] (but not neccessary for them to be on separate lines. This all will be collapsed into one string, anyway)
    # Hence, by comparing open[i] to close[i-1], the latter will be less than 
    #   the former only when the latter is the last paren in a series.  
    diffs <- closeBrackets - c(openBrackets[-1], nchar(string)+1)
    closeParen <- min(which(diffs < 0))
 
 # OLD HAD THIS EXPRESSION WITH nchar(...)... 
 #  I dont think this is necessary.  (I think i only had it in for when I Was trying to read just a few lines at a time.)
 #  closeParen <- min(which(closeBrackets - c(openBrackets[-1], nchar(string)+1) < 0))

  if (identical(closeParen, integer(0))) {
    warning("Could not match up the parens.")
    return(NULL)
  }

  return(closeBrackets[closeParen]+start-1)
  # if closeParen is empty, then either (a) closing bracket doesnt exist [ie impromper input]
  # (b) we need to read more lines
  # (c) max(CL) is the last 

}

