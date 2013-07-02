#------------------------------------------------------#
#    THESE ARE THE FUNCTIONS PRESENT IN THIS FILE      #
#------------------------------------------------------#
#------------------------------------------------------#
  #  JavaTest ( stopRun=TRUE, runInit=TRUE ) 
  #  isErr ( expression )
  #  isNumber ( x )
  #  showProg ( flag, outp, header=FALSE, done=FALSE, tb=1 )
  #  pasteC ( ... )
  #  paste_ ( ... )
  #  pasteR ( x, n )
  #  pasteNoBlanks ( ..., sep=" ", collapse=NULL, na.rm=FALSE )
  #  fw0 ( num, digs=NULL, mkseq=TRUE, pspace=FALSE )
  #  fw0.older ( obj, digs=NULL )
  #  fw ( x, dec=4, digs=4, w=NULL, ... )
  #  fw3 ( x, dec=3, digs=3, w=NULL, ... )
  #  fwp ( x, dec=2, sep=" " )
  #  roundOutToX ( obj, x=10 )
  #  clipPaste ( flat=TRUE )
  #  clipCopy ( txt, sep="" )
  #  meantrm ( x, p=6 )
  #  CMT <- getCMT <- getClassModeTypeof ( obj )
  #  jythonIsGlobal (  )
  #  python ( jythonStatement )
  #  pythonGet ( pythonObj )
  #  pythonSet ( rObj )
  #  pythonSetDiffName ( pythonObj, rObj )
  #  pyParse ( strToParse )
  #  form ( x, dig=3 )
  #  getNCMT <- getNameClassModeTypeof ( obj )
  #  countNA01s ( vec )
  #  insert ( lis, obj, pos=0, objIsMany=FALSE )
  #  as.path ( ..., ext="", fsep=.Platform$file.sep, expand=TRUE, verbose=TRUE )
  #  cleanDotDotPath.split ( pathParts, fsep=.Platform$file.sep, expand=TRUE )
  #  cleanDotDotPath.combine ( splat, fsep=.Platform$file.sep, expand=TRUE )
  #  makeDictFromCSV ( csvFile )
  #  isSubstrAtEnd ( x, pattern, ignorecase=TRUE )
  #  s <- summary2 ( x, rows=6, cols=6, cmt=TRUE )
  #  c4 ( x, rows=20, cols=4, cmt=TRUE )
  #  printdims ( X, justTheValue=FALSE )
  #  topropper ( x )
  #  topropper_withPunc ( x )
  #  qy <- quity ( dir='~/' )
  #  qn <- quitn ( dir='~/' )
  #  tbs ( n, nl=FALSE )
  #  pip (  )
  #  slash (  )
  #  miniframe ( data, rows=200 )
  #  makeDictWithIntegerKeys ( KVraw, applyLabels=TRUE )
  #  chkp <-chkpt ( logStr, chkpOn=TRUE, final=FALSE )
  #  pgDisconnectAll ( drv=dbDriver("PostgreSQL") )
  #  mgsub ( pattern, replacement, x, ..., fixed=TRUE )
  #  cleanChars ( text, replacement="_", Whitelist=NULL )
  #  replaceBadCharsUnderscore ( str, WhiteList=NULL )
  #  timeStamp ( seconds=FALSE )
  #  detectAssignment ( obj, single=TRUE, simplify=FALSE )
  #  loadbak ( f, env=parent.frame() )
  #  saveit ( obj, dir=ifelse(exists("outDir"), outDir, as.path(getwd(), "out")), subDir=TRUE, pos=1, addTimeStamp=TRUE, useSeconds=FALSE )
  #  jesusForData ( ..., dir=dataDir, sub=FALSE, stampFile=TRUE, stampDir=FALSE, pos=1, envir="" )
  #  savethem <- jesus ( ..., dir=ifelse(exists("outDir"), outDir, as.path(getwd(), "out")), subDir=sub,                                  pos=1, sub=TRUE, stampDir=TRUE, stampFile=FALSE, summary=TRUE, envir="" )
  #  mkSaveFileNameWithPath ( objName, dir, pos=2, addTimeStamp=FALSE, ext=".Rda" )
  #  dimToString ( objName, pos=2, prefix="-" )
  #  plength <- printlength ( opt=200 )
  #  reminder (  )
  #  saveToFile_TabDelim ( obj, directory=getwd() )
  #  retTst ( n )
  #  allPosCombsList ( dat, choose=seq(ncol(dat)), yName="y" )
  #  formulasList ( dat, yName="y", VARS.list=NULL, interact=TRUE, intercept=TRUE )
  #  logscale ( range=2:5, intervals=2, base=10 )
  #  asCurr ( x, decim=2, noSpacesAfterSymb=1, symbol="$" )
  #  lP <- listPacker ( receiver, ... )
  #  lsnf ( ... )
  #  lsi ( what, invert=FALSE, rm=FALSE )
  #  devsource ( file, dir="~/Dropbox/dev/R/!ScriptsR/" )
  #  gitsource ( file, dir="~/git/misc/rscripts/" )
  #  homesource ( file, dir="~/" )
  #  source.url ( ... )
  #  extendToMatch ( source, destin )
  #  rmDupLines ( obj, trim=T )
  #  cordl ( ..., length=NULL, justSize=FALSE, crop=TRUE, chop=TRUE )
  #  paraLineChop ( so, length=NULL, lines=NULL, justSize=FALSE )
  #  coefTable ( model )
  #  splitEvery ( string, n, remSpace = FALSE )
  #  cls ( LINES=100 )
  #  pkgFind ( toFind )
  #  regexAll ( pattern, stringVec, replace="@@@", ignore.case=FALSE, fixed=FALSE, perl=FALSE, value=FALSE )
  #  tbls ( envir=.GlobalEnv )
  #  colquote ( colNamesAsStrings )
  #  uniqueRows ( DT )
  #  getdotsWithEval (  )
  #  setkeyE ( x, ..., verbose = getOption("datatable.verbose") )
  #  shift ( x )
  #  shiftb ( x )
  #  namesdetect ( x, pattern )
  #  namesIn ( x, vec, positive=TRUE )
  #  namesNotIn ( x, vec )
  #  orderedColumns ( DT, frontCols=NULL, ignoreCase=TRUE, endCols=NULL )
  #  combineRows ( x )
  #  wordCount ( obj, words, ignore.case=TRUE, preservePunct=FALSE )
  #  dateCheck ( d )
  #  is.allNA ( x )
  #  invDict ( dict )
  #  setNamesDict ( DT, dict, replaceMissing=NULL, silent=FALSE )
  #  uniqueKeys ( DT )
  #  convertClass ( DT, colnameVector, to, from=NULL, originDate="1970-01-01", excelOriginName=".xlorigin" )
  #  convertClass.default ( DT, ... )
  #  convertClass.data.table ( DT, colnameVector, to, from=NULL, originDate="1970-01-01", excelOriginName=".xlorigin" )
  #  areEqual.slow ( x, na.rm=TRUE )
  #  areEqual ( x, na.rm=TRUE, tolerance = .Machine$double.eps ^ 0.5, NoWarnings=FALSE )
  #  CamelCaseSplit ( string,  flat=FALSE )
  #  gapply ( X, FUN, ..., simplify=FALSE, pos=1 )
  #  xapply ( X, qFUN, ..., simplify=FALSE )
  #  catTitle ( Title, pref="", suf="", tabs=1, topline=FALSE, dash="-", center=TRUE )
  #  centerText ( x, eol="\n", padWith=" ", trim=TRUE, tabs="" )
  #  alignText ( x, eol="\n", padWith=" ", trim=TRUE, tabs="", halign="center" )
  #  printBox ( x, width=68, dash="~", sides="#", crop=FALSE, topspace=0, bottomspace=0, tabs=1, header="" )
  #  splitToWidth ( x, width, safetyBreak=100 )
  #  isFALSE ( x )
  #  modelDescrFromCall ( ... )
  #  modelDescrFromCall.Arima ( M )
  #  modelDescrFromCall.lm ( M )
  #  modelDescrFromCall.default ( M )
  #  paste.call ( ordr )
  #  modelDataSetFromCall ( ... )
  #  modelDataSetFromCall.Arima ( M )
  #  modelDataSetFromCall.lm ( M )
  #  dimCompare ( ..., decr=NA, decreasing=decr, sortOn=NA )
  #  rangesFromInt ( int, numberOfRanges, sizeOfEach, pairs=TRUE, aslist=TRUE, sequence=TRUE, fractions=FALSE )
  #  r.t ( x=clipPaste(), header=TRUE, sep=NULL, to=NULL, value=!(is.character(to)), pos=1, file=NULL )
  #  r.d ( x=clipPaste(), header=TRUE, sep=NULL, to=NULL, value=!(is.character(to)), pos=1, file=NULL )
  #  First ( silent=FALSE )
  #  as.path ( ... )
  #  howManyNAs ( x )
  #  sortXbyY ( X, Y, justIndex=FALSE, names=FALSE, names.X=names, names.Y=names )
  #  seasonFromDate ( D, factors=TRUE )
  #  revString ( x )
  #  mds ( includeInput=TRUE, x=clipPaste() )
  #  mkdshr ( x=clipPaste(), space=TRUE, pound="#", dash="-", leftSpace=TRUE, includeInput=TRUE, top=TRUE, minWidth=20, align=NA, mindent=4, dontSmoothPreSpace=FALSE, fancy=FALSE, match=FALSE )
  #  topAndBottom ( vec, n=1 )
  #  mr ( x=clipPaste(), mindent=9, minWidth=60, align="left", top=TRUE, match=FALSE, ... )
  #  mkdsh ( x=clipPaste(), space=TRUE, pound="#", dash="-", leftSpace=TRUE, includeInput=TRUE )
  #  spacecnt ( x=clipPaste() )
  #  dtWideToLong ( DT, cols=names(DT), cnames=c("Name", "Value") )
  #  knito ( input, output=gsub("src", "out", dirname(input)), encoding="UTF-8", ... )
  #  mbench ( ..., maxSeconds=20, maxReps=200L, verbose=TRUE )
  #  utilSource ( .Pfm=Sys.info()[['sysname']] )
  #  plrl ( word.pluarl.form, count, singular=(length(count)==1) )
  #  whichFactors ( x )
  #  getNamesFromDTCols ( DT, na.rm=TRUE, uniquify=TRUE )
  #  orderedHeadTail ( x, n=min(length(x), 5), na.last=TRUE, decreasing=FALSE, logical=FALSE, value=FALSE, f=c("head", "tail") )
  #  orderedHead ( x, n=min(length(x), 5), na.last=TRUE, decreasing=FALSE, logical=FALSE, value=FALSE )
  #  orderedTail ( x, n=min(length(x), 5), na.last=TRUE, decreasing=FALSE, logical=FALSE, value=FALSE )
  #  lib ( pkg, newest=FALSE, dependencies=TRUE, rforge=FALSE, update=FALSE )
  #  are ( ll, simplify=TRUE )
  #  findLastSpace ( x, space=" " )
  #  sourceEntireFolder ( folderName )
  #  cnt ( col, DT=defaultDT )
  #  mergeDTlist ( DTlist, suffixes=NULL, checkKeys=TRUE ) 
#------------------------------------------------------

  .Pfm <- Sys.info()[['sysname']]

  # Load Memory Functions
  try(  ## Wrapping in `try` so that if fails, does not affect rest of the utils load
      if (.Pfm=="Linux"){ 
        source(path.expand("~/NBS-R/utils/memoryFunctions.R"))
        wrkDir <- "~/NBS-R/"
      } else 
        source(path.expand("~/git/misc/rscripts/utils/memoryFunctions.R"))
  , silent=TRUE)

  .RForge <- "http://R-Forge.R-project.org"
#------------------------------------------------------


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
  
	return( inherits(try(eval(expression), silent=T), "try-error") )
}

isNumber <- function(x)  {
# the purpose of this function is to avoid the warnings that 
# come with 'is.numeric(as.numeric(x))' when x is not a number.

  if (is.list(x))
    return(lapply(x, isNumber))

  if (length(x) > 1)
    return(sapply(x, isNumber))

  ifelse (nchar(x) == attr(gregexpr("^[[:digit:]]*$", x)[[1]], "match.length"), 
      is.numeric(as.numeric(x)), FALSE)

}

showProg <- function(flag, outp, header=FALSE, done=FALSE, tb=1)  {
  # wrapper function for: 
  # if flag is true, then cat() outp. 

  # put tabs after any line break
  outp <- sub("\n", tbs(tb, T), outp)
  
  # If header or done: set tb to 0, unless user defined value
  tb <- ifelse(missing(tb) && (header || done), 0, tb)

  if (header) 
    cat ("","========================","Progress Indication....", sep=tbs(tb,T))
  if (flag)
    cat(tbs(tb), outp, "\n", sep="")
  if (done) 
    cat ("", "----------------", "Process Complete", "========================", sep=tbs(tb,T))
} 


# --------------------------------------------------- #
#                                                     #
#                   PASTE FUNCTIONS                   #
#                                                     #
# --------------------------------------------------- #

  ## TODO:  Put this in the dictionaries file
  dict.parens <- c("(" = ")", "[" = "]", "{" = "}", "<" = ">")

  # like paste0, but with collapse="" 
  pasteC <- function(...)
    paste(..., collapse="")
  paste_ <- function(...)
    paste(..., collapse="_")
  pasteR <- function(x, n) {
    # if n is not a single number, iterate
    if (length(n) > 1) {
      if (length(n) == length(x))
        return(mapply(pasteR, x, n))
      return( sapply(n, function(n1) pasteR(x, n1)) )
    }
    # otehrwise, siple return
    pasteC(rep(unlist(x), n))
  }

  pasteQ <- function(...,  q="'", wrap="(", sep="", collapse=", ") { 
    # Encloses the terms in a quotes.
    # If `wrap` is not NULL, also adds those to each end. 
    #    `wrap` defaults to "("..")" and should be set to NULL/FALSE to turn off

    # alternates to NULL should be interpreted to NULL
    if (is.null(wrap) || is.na(wrap) || wrap=="" || identical(wrap, FALSE))
      wrap <- NULL

    # `wrapR` is the closing-equiv of `wrap.` If no such equiv found, use `wrap`.
    wrapR <- dict.parens[wrap]
    wrapR <- ifelse(is.na(wrapR), wrap, wrapR)

    paste0(wrap, 
      paste(q, unlist(list(...)), q
        , sep=sep, collapse=collapse)
      ,wrapR) 
  }



  pasteNoBlanks <- 
  function(..., sep=" ", collapse=NULL, na.rm=FALSE) { 
    dots <- list(...)
    # remove NAs
    if(na.rm)
      dots <- dots[!is.na(dots)]

    # remove blanks
    remove <- identical(sep, paste0(dots, sep)) | (nchar(dots)==0)  |  (lapply(dots, length)==0)
    dots   <- dots[!remove] 

    f <- function(..., sep2=sep, collapse2=collapse){
      paste(..., sep=sep2, collapse=collapse2)
    }

    # return pasted value
    return(Reduce(f, dots))
  }
# --------------------------------------------------- #


fw0 <- function(num, digs=NULL, mkseq=TRUE, pspace=FALSE)  {
  ## formats digits with leading 0's. 
  ## num should be an integer or range of integers.
  ## if mkseq=T, then an num of length 1 will be expanded to seq(1, num).   
  #
  # Note that if num is a list, digs will not be automatically compared across the list, and therefore should be manually slected. 

  # TODO 1:  put more error check

  # when num is a list, call recursively.  mkseq should not expand the list into seq, unless specifically user sets flag or entire list is just length one element
  if (is.list(num))
    return(lapply(num, fw0, digs=digs, mkseq=ifelse(missing(mkseq), !length(num) > 1, mkseq)))

if (!is.vector(num) & !is.matrix(num)) {
    stop("num should be integer or vector")
  }

  # capture the dims and we will put it back
  dims <- dim(num)

  # convert strings to numbers
  num <- as.numeric(num)

  # If num is a single number and mkseq is T, expand to seq(1, num)
  if(mkseq && !length(num)>1)
    num <- (1:num)

  # number of digits is that of largest number or digs, whichever is max
  digs <- max(nchar(max(abs(num))), digs)  

  # if there are a mix of neg & pos numbers, add a space for pos numbers 
  #   (checking first for 0)
  #   OR if pspace is flagged as TRUE
  posSpace <- ifelse((min(num) != 0 &  sign(max(num)) != sign(min(num)) | pspace==TRUE), " ", "")

  # return: paste appropriate 0's and preface neg/pos mark
  ret <- 
    sapply(num, function(x) ifelse(x<0, 
    paste0("-", paste0(rep(0, max(0, digs-nchar(abs(x)))), collapse=""), abs(x)),
    paste0(posSpace, paste0(rep(0, max(0, digs-nchar(abs(x)))), collapse=""), x)
    ))

  # put back in original form.  ie, make it a matrix if it was originally. Otherwise, this will just be NULL
  dim(ret) <- dims

  return(ret)
}

#-----------------------------------------------

## THIS IS THE OLDER INTERPRETATION OF fw0. 
## SPECIFICALLY FOR HOW IT HANDLES fw(199, digs=2) 
fw0.older <- function(obj, digs=NULL)  {
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

#--------------------------------------------
#-----------------------------------------------

## functino to format numerics
fw <- function(x, dec=4, digs=4, w=NULL, ...) {
  ## wrapper to function format(.)
  format(x, nsmall=dec, digits=digs, width=w, ...)
}

## functino to format numerics
fw3 <- function(x, dec=3, digs=3, w=NULL, ...) {
  ## wrapper to function format(.)
  format(x, nsmall=dec, digits=digs, width=w, ...)
}

fwp <- function(x, dec=2, sep=" ", simplify=TRUE, pad=FALSE) {
# Formats as percentage

  ret <- sapply(x, function(y) paste(fw3(100*y, dec), "%", sep=sep), simplify=simplify)

  # add spaces
  if (pad) {
    nc <- nchar(ret)
    max.char <- max(nc)
    ret <- paste0(sapply(max.char- nc, pasteR, x=" "), ret)
  }

  if (is.null(dim(x)) || !simplify)
    return(ret)

  dim(ret) <- dim(x)
  dimnames(ret) <- dimnames(x)

  return(ret)
}


roundOutToX <- function(obj, x=10) 
# rounds away from 0 to the nearest x
  if(x==0) return(round(obj)) else 
  ceiling(abs(obj) / x) * (obj/abs(obj)) * x

clipPaste <- function(flat=TRUE)  {
# equivalent of CMD+v piped through. 
#
# returns whatsever in the OS's clipboard
# if flat is TRUE, then it is collapsed into a single element 
#    as opposed to multiple lines
  
  con <- pipe("pbpaste", open="rb")
  ret <- readLines(con, warn=FALSE)

  if (flat)
    ret <- paste0(ret, collapse="\n")

  close(con)
  return(ret)
}

clipCopy <- function(txt, sep="") { 
# equivalent of highlighting txt and hitting  CMD+C 

  txt <- paste(txt, collapse="\n")

  con <- pipe("pbcopy", "w")
  writeLines(txt, con, sep=sep)
  close(con)

  return(invisible(TRUE))
}

###############################################################
###############################################################

# mean with paramters. Trimming top/bottom p observations (or 1/4 if too few obs present)
meantrm <- function(x, p=6)
  mean(x, trim=min(.25, p/length(x)), na.rm=TRUE)

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


insert <- function(lis, obj, at=0, objIsMany=FALSE) {
  # Inserts obj into list *at* atition at
  #    all existing items in list, form at onward, are moved forward
  #    NOTE: If atition > length(list), obj is inserted at end
  #
  # Args:
  # lis:  the list object
  # obj:  the object being inserted
  #   at:  the atition of insert
  #   objIsMany: (TODO) If T, each item in obj is inserted separately
  #
  # Returns:
  #   list with obj inserted at atition 
  #
  # TODO: modify for objIsMany=TRUE
  
  
  leng <- length(lis)
  if (at > leng) {   # note strictly greater (not greater or equal!)
    return (c(lis,obj))
    ## TODO:  Check for objIsMany
    ## ifelse(objIsMany, for(i in....))
    }
    
    
  if(at <= 1)  {
    c(obj,lis)
  } else {
    c(lis[1:at-1], obj, lis[at:leng])
  }
}


#--------------------

sapply.preserving.attributes = function(l, ...) {
# by @Owen from http://stackoverflow.com/questions/7698797/why-does-mapply-not-return-date-objects
    r = sapply(l, ...)
    attributes(r) = attributes(l)
    r
}

#--------------------

as.path <- function(..., ext="", fsep=.Platform$file.sep, expand=TRUE, verbose=TRUE) {
# concatenates the `...` into a valid path, accounting for extra slashes and dot-dot's
##
##  Depends on:  cleanDotDotPath.split() & cleanDotDotPath.combine()

  # grab the dots, remove any null values. 
  dots <- list(...)
  dots <- dots[!sapply(dots, is.null)]


  ## If first argument starts with "http" or "ftp" and `fsep` hasn't been explicitly set, 
  ## Then fsep should be "/". 
  if(any(grepl("^(http|ftp)", as.character(dots[[1]]))) && missing(fsep))
    fsep <- "/"

  # error check
  if (any(grepl("^/~", dots[[1]])))
    stop ("Path cannot start with `/~`\nDid you mean to use simply `~` ?")

  ## If starts with fsep, we will preserve it.
  startWith <- ifelse(substr(dots[[1]], 1, 1) == fsep, fsep, "")
  
  # Clean up the input (removing superfluous slashes, dots, etc)
  cleaned <- lapply(dots, function(x) {      
                # remove any leading slashes
                x <- ifelse(substr(x, 1, 1) == fsep, substr(x, nchar(fsep)+1, nchar(x)), x) 
                
                # remove any trailing slashes
                lng <- nchar(x)
                x <- ifelse(substr(x, lng, lng) == fsep, substr(x, 1, lng-1), x) 

                # return x to cleaned
                x
              })

  ## TODO:  this was to prevent some edge case where an element in `cleaned` was blank
  ##        currently, I cannot identify such a case.  If found, document it. 
  cleaned <- cleaned[!sapply(cleaned, function(x) identical(nchar(x), integer(0)))]


  # put back any starting fsep
  cleaned[[1]] <- paste0(startWith,cleaned[[1]])

  # append '.ext' to last item
  if (!is.na(ext) && !ext=="")
    cleaned[[length(cleaned)]] <- paste0(cleaned[[length(cleaned)]], ".", gsub("^\\.", "", ext))

  # checking for '..'   ie:  "~/git/" +  "../out" ==>  "~/out"
  if(any (  grepl("\\.\\.", cleaned) )) {
    return(cleanDotDotPath.split(cleaned, fsep=fsep, expand=expand))
  }

  # else
  putTogether <- do.call(file.path, c(cleaned, fsep=fsep))

  if (!expand)
      return(putTogether)
  return(path.expand(putTogether))
}


cleanDotDotPath.split <- function(pathParts, fsep=.Platform$file.sep, expand=TRUE) {
## PURPOSE: Combine pathParts by taking into account `../`
## eg, if the pathParts is: 
#            list("~/git/nbs",  "../../../Shared/Adobe")
#       output should be: 
#            "/Users/Shared/Adobe"
#
# pathParts: A list of path-like objects that will be concatenated into a single path string
#            If it is not a list, it will be coerced into one. 
# fsep     : a character representing the seaparator between path parts. ie, "/" or "\\"
# expand   : If T, "~" will be expanded, normally to "/Users/usrName/" or similar, as per system
#            If F, path may be expanded anyway, if the amount of ".."'s require it. 

      # expand "~usr/"
      if (expand)
        pathParts <- lapply(pathParts, path.expand)

      # pathParts should be a list. Coerce if it isn't
      if (!is.list(pathParts))
        pathParts <- as.list(pathParts)

      # first paste the multi pieces together then split on fsep
      putTogether <- do.call(file.path, c(pathParts, fsep=fsep))
      splats <- strsplit(putTogether, fsep)

      if (length(splats) == 1)
        return(cleanDotDotPath.combine(splats[[1]], fsep=fsep, expand=expand))
      return(sapply(splats, cleanDotDotPath.combine, fsep=fsep, expand=expand))

}

cleanDotDotPath.combine <- function(splat, fsep=.Platform$file.sep, expand=TRUE) {

      # check for superfluous "", which came from 'dir1//dir2.'  
      # These should be ignored and hence removed
      # However if splat[1] is "", this came from '/dir1' and should be preserved
      if (any(splat[-1] == ""))
         splat <- c(splat[[1]], splat[-1][!splat[-1]==""] )
      
      # now each element in spat is a single directory or a dotdot
      # identify which are the dotdots.
      isdotdot <- splat == ".."

      # check if there are more dotdot's than folders before it.  eg: 
      #    FALSE    FALSE    FALSE    FALSE    FALSE     TRUE     FALSE    FALSE 
      #      "~"    "git"    "nbs"     ".."     ".."     ".."  "Shared"  "Adobe" 
      if(any(toroot <- cumsum(isdotdot) >= cumsum(!isdotdot))) {
        
        # if we hadn't expanded, rerun this function with expand being TRUE
        if(!expand)
           return(cleanDotDotPath(pathParts, fsep=fsep, expand=TRUE))

        # otherwise..
        # the first of the "too many dots" is the new root
        root <- min(which(toroot))
        # keep only those elements of splat after the new root. 
        #   adding in `""` which signifies "/" when pasted back
        splat <- c("", tail(splat, -root)) 

        # re-run this function from the new root
        return(cleanDotDotPath(splat, fsep=fsep, expand=expand))
      } # else:

      # for each index of dotdot, we are going to remove the index of the dir
      #   that is "right before" it, ie the max of the indecies less than it  
      areDots <- which(isdotdot)   # these are the indecies to the dots
      areDirs <- which(!isdotdot)  # these are the indecies to the directories
      
      # remove from areDirs, the largest index smaller than dot
      for(dot in areDots)
        areDirs <- setdiff(areDirs, which.max(areDirs[areDirs < dot]) )
    
      # replace splat with only the indecies being kept.  The `as.list` is for the `do.call`
      splat <- as.list(splat[areDirs])

      # paste it back together with fsep
      return(do.call(file.path, c(splat, fsep=fsep)) )
}



#--------------------------

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

s <- smry <- summary2 <- function(x, rows=6, cols=6, cmt=TRUE) {
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
  ## wrapper to function summary2, with rows=20 and cols=4. (hence c4)
  ##   note, calling c4(x, 35) will give 35 rows and 4 cols. (simpler than s(x, 35, 4)) 
  if(is.data.table(x))
    if (ncol(x) < 3)
      return(x)
    else 
      return(x[
              , unique(c(  1:min(ncol(x), (ifelse(missing(rows), 5, rows-2))),  ncol(x) + (-1:0) )  )
              , with=FALSE
              ])

  # else: 
  summary2(x, rows, cols, cmt)
}
 
 
printdims <- function(X, justTheValue=FALSE) {
  nm  <- as.character(match.call()[[2]])

   if (any(grepl("^\\[\\[", nm)))
      nm <- "[ ? ]"

  dims <- paste0(nm, ":  (", paste(dim(X), collapse=" x "), ")")

  if (justTheValue)
    return(dims)
  else (print(dims))
}


#--------------------------------------------#
    topropper <- function(x) {
      # Makes Proper Capitalization out of a string or collection of strings. 
      sapply(x, function(strn)
       { s <- strsplit(strn, "\\s")[[1]]
           paste0(toupper(substring(s, 1,1)), 
                 tolower(substring(s, 2)),
                 collapse=" ")}, USE.NAMES=FALSE)
    }

    topropper_withPunc <- function(x)
      gsub("\\b([a-z])([a-z]+)", "\\U\\1\\E\\2", x, perl=TRUE)

    ## compare: 
    #          topropper("last-one")   # [1] "Last-one"
    # topropper_withPunc("last-one")   # [1] "Last-One"
#--------------------------------------------#

qy <- quity <- function(dir='~/')  {
  ## quits R and saves the .RData and .Rhistory to dir
  setwd(dir)
  quit('yes')
}

qn <- quitn <- function(dir='~/')  {
  ## quits R and saves the .RData and .Rhistory to dir
  setwd(dir)
  quit('no')
}


tbs <- function(n, nl=FALSE)  {
  # returns a string of n-many tabs, concatenated together
  # if nl=T, will preface with a new line char.
  return(paste0(ifelse(nl, "\n", ""), paste0(rep("\t", n), collapse="")))  
}

pip <- function() {
# for broken keyboard, missing pipe
cat("
|

")
}

slash <- function() {
# for broken keyboard, missing pipe
cat("
\\ 
")
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
    # we want a dict such that dict[aritstid] = source_name
    # PROBLEM:  since sourceid's are integers, dict[sourceid] will return the sourceid'th (nth) item 
    # eg:  dict[510] will return the 510th item of dict, not the source whose id is 510  *rather, not necessarily..  
    #      that is,  dict[510] != dict["510"]
    # 
    # this wouldnt be a problem if we can ensure that each sourceid gets loaded 
    # into dict at the position of its integer value
    # then dict[sourceid] and dict[sQuote(sourceid)] will return the same value
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
    dict <- rep(NA,largestK)     # note that length(dict) >= length(KVraw)
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
 	#	logStr:  a string that will be logged to stdout
 	#	chkpOn:	 If FALSE, then logging does not occur. (for quickly turning chkp on/off)
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


mgsub <- function(pattern, replacement, x, ..., fixed=TRUE) {
# like an mapply on gsub, but done iteratively. 

  if(length(pattern) != length(replacement))
    stop("pattern and replacement differ should be the same length")

  ## TODO: add recycling and error-check 

  for(i in 1:length(pattern))
    x <- gsub(pattern[i], replacement[i], x, ..., fixed=fixed)
  
  return(x)
}


cleanChars <- function(text, replacement="_", Whitelist=NULL) {
  # wrapper for gsub, with regex pre-composed
  # replaces all non-basic chararcters with replacement (underscore by default)
  # Whitelist is non-functional for now  # TODO

  if (!is.null(Whitelist))
    stop("Whitelist is non-functional")

  Simple_regex <- "[^0-9a-zA-Z -.]"
  gsub(Simple_regex, replacement, text)
}

replaceBadCharsUnderscore <- function(str, WhiteList=NULL) {
  stop ("use cleanChars() instead")
}



timeStamp <- function(seconds=FALSE) {
  # basic time stamp:   20111231_2350  for Dec 31, 2012, 11:50pn

    if(seconds)
        return(format(Sys.time(), "%Y%m%d_%H%M%S"))
    return(format(Sys.time(), "%Y%m%d_%H%M"))
}


detectAssignment <- function(obj, single=TRUE, simplify=FALSE) {
  # detects whether an assignment operator is present. 
  # Returns T if detected. F if not detected. 
  #  obj can be list-like
  # if single=TRUE, returns a single element  (ie, any(unlist(.)) ) as opposed to a logical vector
  #   (useuful if one bad apple makes the whole bunch unusable)
  # simplify is passed through to the sapply call.  Single will override simplify

  # list of operators to search for
  ops <- c("<-", "<<-", "->", "->>")

  # compute grepl
  ret <- sapply(ops, grepl, obj, simplify=simplify)

  # return value
  if (single)
    return(any(unlist(ret)))
  return(ret)

}


#==========================================================================#
#--------------------------------------------------------------------------#
#                       SAVEIT & SAVETHEM & JESUS                          #
#                  mkSaveFileNameWithPath & dimToString                    #
#                                                                          #
#              depends: detectAssignment, timeStamp, as.path               #
#__________________________________________________________________________#

    ## TODO: I dont really use this dir setup anymore for data archiving.  
    ##       Instead using `jesusForData` along with the `workspace.R` functions.
    ##       Clean this up so it is usable again
    loadbak <- function(f, env=parent.frame())
    # wrapper function for loading from outDir/data_bak/<fileName>
      load(as.path(outDir, "data_bak", as.character(match.call()[[2]])), envir=env)

  saveit <- function(obj, dir=ifelse(exists("outDir"), outDir, as.path(getwd(), "out")), subDir=TRUE, pos=1, addTimeStamp=TRUE, useSeconds=FALSE)  {
    ##  Like savethem() but only takes a single obj argument
    ##     The advantage of using saveit() is not having to 
    ##     type 'dir=...'.   
    ##     Yep, that is all. (this func also was written first then modified to get savethem()) 
    ##     
    ## saves obj to file of type .Rda and with 
    ##     name of file same as name of obj + time stamp
    ##     in location: dir
    ##     subDir:  if TRUE, will create subdirectory data_bak 
    ##                inside dir and use that folder. (if alreaddy exists, will just use)
    ##
    ## returns:  the path/to/file.Rda where obj was saved


    # get object from the parent environment
    objName <- as.character(match.call()[[2]])

    #----- EVAL IN OBJ NAME ------#
    ## this allows the use of, eg, saveit(eval(paste0("model.", bestVal)), outDir)
    # that is, if obj begins with eval(), then eval-parse it for the correct string
    if(objName[[1]]=="eval")
        objName <- eval(parse(text=objName))
    #----- EVAL IN OBJ NAME ------#


    #----- ERROR CHECKS ------#
    # If multiple arguments passed to saveit, this may detect the mistake. 
    if(!is.character(dir) || !is.logical(subDir)) 
      warning("Did you mean to use savethem() instead of saveit()?")

    # If any of the assignment operators are found in the list, throw an error
    if(detectAssignment(objName)) 
      stop("Cannot assign in the call to this function.")
    #----- ERROR CHECKS ------#



    # use subdirectory data_bak unless indicated not to  (create it if needed)
    if (subDir) {
      dir <- as.path(dir, "data_bak")
    }

    # Create dir if needed
    dir.create(dir, recursive=TRUE, showWarnings=FALSE)

    # Create Suffix (based on dim, if applicable, and append timeStamp if required)
    suffix <- dimToString(objName, pos=pos+1)
    if(addTimeStamp)
      suffix <- paste0(suffix, "-", timeStamp(seconds=useSeconds))

    # create the filename, cleaning objName of bad chars
    fileName <- paste0(cleanChars(objName),suffix,".Rda")
    fileWithPath <- as.path(dir,fileName)
      
    # Save the object
    do.call(save, args=c(list(objName), envir=parent.frame(pos+1), file=fileWithPath) )
    
    # return the path/to/file
    return(fileWithPath)
  }

  #------------------------------------------------------------------------#

  ## WRAPPER
  jesusForData <- function(..., dir=dataDir, sub=FALSE, stampFile=TRUE, stampDir=FALSE, pos=1, envir="") {
    jesus(..., dir=dir, sub=sub, stampFile=stampFile, stampDir=stampDir, pos=pos+1, envir=envir)
  }
  ###

  savethem <- jesus <- function(..., dir=ifelse(exists("outDir"), outDir, as.path(getwd(), "out")), subDir=sub, 
                                pos=1, sub=TRUE, stampDir=TRUE, stampFile=FALSE, summary=TRUE, envir="")  {
    ##  Like saveit() but can take multiple objects as arguments
    ##
    ##     saves objects passed as (...) arguments to file of type .Rda and with 
    ##     name of file same as name of obj + time stamp
    ##     in location: dir
    ##     subDir:  if TRUE, will create subdir data_bak 
    ##                    inside dir and use that folder. (if alreaddy exists, will just use)
    #S     sub:  a synonym for subDir. (since use of ... does not allow for partial matches) 
    ##
    ## returns:  the path/to/file.Rda where objects were saved

    ## NOTE TO SELF:  You cannot use  `dots.list` and `list(...)` interchangeably in substitute
    ##                    dots.list <- list(...)
    

    # get objects from dots
    objNames <- as.list(as.character(substitute(list(...)))[-1L])

    # check for arguments being (eval(...))
    whichAreEval <- sapply(objNames, function(x) grepl("^eval\\(.+\\)$", x))

    if (any(whichAreEval))  {
      # confirm they are calls
      whichAreCalls <- sapply(substitute(list(...))[-1], is.call)
      # proceed only if they match
      if (identical(whichAreCalls, whichAreEval)) {
        objNames2 <-  list(...)[whichAreCalls]
        objNames <- unlist(c(objNames2, objNames[!whichAreCalls]))
      }
    }

### TODO:  June 2013.  Apparently the `eval(vector.of.obj.names)` was not working. I wrote the part immediately above this. 
###        Confirm all is working correctly.  
# -- check this -- #    # TODO:  double-check pos value.  It might be off. 
# -- check this -- #    # check any value is eval(XX), if so parse it. Collect all values into a single vector.   
# -- check this -- #    objNames <- unlist( lapply(objNames, function(ob) 
# -- check this -- #      if(substr(ob, 1, 5)=="eval(")   eval(parse(text=substr(ob, 6, nchar(ob)-1)), envir=ifelse(is.environment(envir), envir, parent.frame(pos+1)) )  else  ob
# -- check this -- #    ) )


    # No need to save any object twice
    objNames <- unique(objNames)

    #----- ERROR CHECKS ------#
    # If any of the assignment operators are found in the list, throw an error
    if(detectAssignment(objNames)) 
      stop("Cannot assign in the call to this function.")
    #----- ERROR CHECKS ------#

    # Check that the objects to be saved exist
    NotPresent <- !(sapply(objNames, exists))
    if (any(NotPresent)) {
      warning("The following objects were not found and hence could not be saved:\n    ", paste(objNames[NotPresent], collapse="    "), "\n")
      objNames <- objNames[!NotPresent]
    }

    # if flag is true, add appropriate subdir
    if (subDir) 
      dir <- as.path(dir, "data_bak")

    # add timeStamp to dir if required
    if(stampDir)
      dir <- paste0(as.path(dir), "_", timeStamp())

    # Create dir if needed
    dir.create(as.path(dir), recursive=TRUE, showWarnings=FALSE)

    # create the file paths, cleaning objNames of bad chars
    fileWithPath <- sapply(objNames, mkSaveFileNameWithPath, dir=dir, addTimeStamp=stampFile)

    # Save the object
    tryCatch(mapply(function(obj, thefile)
              # note that with the save+do.call we are going in an extra two environments, hence pos + 2  (also, tested with pos+1, pos+3, both wrong)
              do.call(save, args=list(obj, envir=parent.frame(pos+2), file=thefile) )  # pos + 3 will be off if 
          , objNames, fileWithPath), 
        error = saveErrorHandle)

            ## This does NOT work. 
            # filesCreated <- do.call(saveit, args=list(objNames, pos=pos+1, dir=dir, addTimeStamp=stampFile))
            # return(filesCreated )
    
    # return the path/to/files or just a summary
    if (summary)
      return(list(summary2=paste(length(fileWithPath), "files were created in:"), dir=dir))
    return(fileWithPath)
  }



#__________________________________________________________________________#

    #--------------------------------------------#

saveErrorHandle <- function(e) {
  if(grepl("error writing to connection", e)) {
    warning("Could not write to connection. Object has not been saved.\n Check that the disk is not full.")
  } else
  stop(e)
}

mkSaveFileNameWithPath <- function(objName, dir, pos=2, addTimeStamp=FALSE, ext=".Rda") {
  ## This is a helper function for jesus()
    # error check
    if (!is.character(objName))
      stop("objName should be character. Did you forget quotation marks?")

    # Create Suffix (based on dim, if applicable, and append timeStamp if required)
    suffix <- dimToString(objName, pos=pos+1)
    if(addTimeStamp)
      suffix <- paste0(suffix, "-", timeStamp())

    # create the filename, cleaning objName of bad chars
    # return the file path
    as.path(dir, paste0(cleanChars(objName),suffix), ext=ext)
 }
    
    #--------------------------------------------#

dimToString <- function(objName, pos=2, prefix="-", markers="!") {
# gets the dimensions of an object and converts it to a string; if dim is NULL returns ""
    
    # error check
    if (!is.character(objName))
      stop("objName should be character. Did you forget quotation marks?")

    obj.dim <- dim(get(objName, envir=parent.frame(pos)) )

    suffix <- ""
    if (!is.null(obj.dim))
      suffix <- paste0(prefix, markers, paste(obj.dim, collapse="x"), markers)

    return(suffix)
}

    #--------------------------------------------#

stringToDim <- function(fileName, pos=2, prefix="-", markers="!", simplify=FALSE) {
# extracts the dims from a file name, if the file was saved using jesus() or saveit()
# example file name: "artist_and_dates-!47082x7!-20130607_1846.Rda"
  
    # error check
    if (!is.character(fileName))
      stop("objName should be character. Did you forget quotation marks?")

    # string pattern to search for 
    pat  <- paste0(markers, ".*", markers) 
    dims <- stringr::str_extract(string=fileName, pattern=pat)
    
    # remove the markers
    dims <- gsub(markers, "", dims)

    # split on the x
    dims <- strsplit(dims, "x")

    # convert to numeric
    dims <- lapply(dims, as.numeric)
    
    # convert to numeric, simultaneously adding names if 2-dimensional
    dims <- lapply(dims, function(x) 
        if (length(x)==2) setNames(as.numeric(x), c("rows", "cols")) else as.numeric(x))
    
    # dims will be a list. However, if only one file name sent we want to return a vector 
    if (length(fileName)==1)
      return(dims[[1]])

    if (!simplify)
      return(dims)

    # else, simplify

    # if not all the same length, cannot simplify.  Return the list as is
    if (!areEqual(lapply(dim, length)))
      return(dims)      

    # otherwise, simplify
    return(data.frame(do.call(rbind, dims)))
}

    #--------------------------------------------#
#__________________________________________________________________________#
#==========================================================================#


plength <- printlength <- function(opt=200) {
## Changes the environment's setting for how many elements to output for print command
## 
## Arg:  opt is the maximum number of elements that will be outputed when print is called
##
## Returns the value returned by the options call, which is the previous max.print setting
  return(options("max.print" = opt))
}


reminder <- function() {
## function to remind which op is which. 
  cat ("SINGLE: \n")
  cat("c(T, F, T)  &  c(T, F, T) = ",
      c(T, F, T)  &  c(T, F, T), "\n\n")

  cat ("DOUBLE: \n")
  cat("c(T, F, T)  &&  c(T, F, T) = ",
      c(T, F, T)  &&  c(T, F, T), "\n")
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
  objName <- cleanChars(substitute(obj))
  if (isSubstrAtEnd(directory,"/")) {
      directory <- substr(directory,1,nchar(directory)-1)
  }

  # create the filename, then save it
  fileName <- paste0(directory,"/",objName,"_",ts(),".csv")
  write.table(obj, file=fileName, sep="\t", eol="\n",
              col.names=TRUE, row.names=TRUE, append=TRUE, quote=FALSE, qmethod="double")

#  write.table(rbind(obj), file=fileName, sep="\t", eol="\n",
       #       col.names=TRUE, row.names=TRUE, append=T, quote=FALSE, qmethod="double")
  return(fileName)
}

retTst <- function(n) {
  ## used for trouble shooting
  # positive values of n return T
  # negative values of n return F
  # NA values of n return NA
  # all other values of n return NULL

  if (any(is.na(n) | is.null(n))) 
     return(NA)
  
  # return
  ret <- ifelse(n > 0, TRUE,  
    ifelse(n < 0, FALSE, 
      list(NULL)
  )) 

  if(length(ret)==1 && is.null(ret[[1]]))
    return(NULL)

  return(ret)
}


#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

allPosCombsList <- function(dat, choose=seq(ncol(dat)), yName="y") {
## returns list of column indicies

    #----------------------------------------------------------------------#
    ## NOTE TO SELF
    ##   CANNOT DO THIS.   dat must not contain y.  otherwise the columns will not match up
    ## 
    ##  This will simply have to be different from allModels for now
    ##
    # if (!any(colnames(dat) == yName)) 
    #   warning(yname, " not found in the colnames of dat; using whole dataframe.")
    #
    # # remove y-column 
    # dat <- dat[, colnames(dat) != yName]
    #----------------------------------------------------------------------#    
    
    n <- ncol(dat)

    # x <- c(rep(TRUE, 3), rep(FALSE, n-3))
    lapply(choose, function(r) cbind(permutations(n, r)))
}

#  allPosCombsMatrix.TakesTooLong <- function(dat, choose=-1) { 
#  ## Creates a matrix where each row is a logical-index corresponding  
#  ## to the columns (ie, variables) of dat 
#  ## Where the rows contain all possible 'choose'-combinations of the variables 
#  ##   
#  ## dat is a dataframe of response variables 
#  ## choose is a vector, indicating HOW MANY variables to co-select 
#  ##   eg  choose=3  will give only rows of 3-co-selections 
#  ##       choode=1:3 will give only rows of 1, 2, or 3 co-selections 
#  ##   choose=-1 selects ALL rows 
#   
#      n <- ncol(dat) 
#   
#      matr <- matrix(rep(c(TRUE, FALSE), n), nrow=n, byrow=TRUE) 
#      matr <- do.call(expand.grid, split(matr, row(matr))) 
#   
#      # reverse the columns for neatness 
#      matr <- matr[, n:1] 
#   
#      # add names 
#      colnames(matr) <- colnames(dat) 
#   
#      # if choose is flagged as -1, select all rows, otherwise only those requested 
#      whichRows <- if (all(choose == (-1))) seq(2^n) else rowSums(matr) %in% choose 
#   
#      # return 
#      matr[whichRows, ] 
#   
#  } 



formulasList <- function(dat, yName="y", VARS.list=NULL, interact=TRUE, intercept=TRUE)  {
  # creates list of formula strings from a dataframe and list of variable indexes
  #   Note:  VARS.INDEX should reference dat WITHOUT y present. 

  plusstar <- if (interact) "*" else "+"

  if (is.null(VARS.list))
    VARS.list <- allPosCombsList(dat[colnames(dat) != yName], 1:2)
  
  tilde   <- ifelse(intercept, "~ 1 + ", "~ -1 + ")
  vars    <- colnames(dat[colnames(dat) != yName])
  #  datName <- as.character(match.call()[[2]])  # NOT NEEDED

  formulasList <- lapply(VARS.list, function(varsIndex)
                     apply(varsIndex, 1, function(vec) 
                        # the mess with the vec[[1]] is necessary to accomadate the + in tilde, which is necessary for interact=TRUE
                        as.character(paste(c( paste(yName, tilde, vars[vec[[1]]]), vars[vec[-1]]), collapse=plusstar), env=parent.frame(3)) 
                    ))

  formulasList
}



#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

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

  ret <- paste0(symbol, spaces, xStr, deciDigsStr)

  if (length(names(x)))
    names(ret) <- names(x)

  return(noquote(ret))
}  # END asCurr

lP <- listPacker <- function(receiver, ...)  {
  # takes all arguments (...) and appends them to receiver
  #
  # receiver should be list-like
  #  value returned is list-like 

  return(c(receiver, list(...)))

  #-----------------------------------------------------------------------#
  # TODO:  Decide if any of the following is still useful, else chuck it. #
  #-----------------------------------------------------------------------#
        # if (length(list(...)) > 0L) {
        #   receiver[length(receiver) + 1L] <- ..1
        #   if (length(list(...)) > 1L)  {
        #       receiver <- listPacker(receiver, list(...)[-1L])    
        #   }
        # } else {
        #   warning("There were nothing to add to the list.")
        # }
        # receiver
  #-----------------------------------------------------------------------#
}

  


lsnf <- function(...){
# same as ls(), but such that object is not a function
  objs <- ls(..., envir=parent.frame(2))
  objs[!sapply(objs, function(x) is.function(get(x, envir=parent.frame(2))))]
}


lsi <- function(what, invert=FALSE, rm=FALSE){
# same as ls(), but such that object inherits `what`

  if (!is.character(what))
    what <- as.character(match.call()[[2]])

  objs <- ls(envir=parent.frame(2))
  indx <- sapply(objs, function(x) inherits(get(x, envir=parent.frame(3)), what))

  if (invert)
    indx <- !indx

  if (rm) {
    rm(list=objs[indx], envir=parent.frame(2))
    cat("The following objects have been removed:\n")
  }

  return(objs[indx])
}


#/@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@\#
##               DIRECTORY FUNCTIONS               ##



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
###    Group the left hand side using the new function 'g()'
###    The right hand side should be a vector or a list
###    Use the newly-created binary operator '%=%'
###
###         eg:  g(a, b, c)  %=%  list("hello", 123, list("apples, oranges"))
####
### ----------------------------------------------------------------------------
#####%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%########






          #_________________________________________#
          #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
          #-----------------------------------------#
          #     cordl, rmDupLines, paraLineChop     #
          #_________________________________________#
          #-----------------------------------------#


#--------------------------
rmDupLines <- function(obj, trim=T)  {
  # removes duplicate lines from obj and returns the modified object.
  # especially useful for captured output of summary.lm() 
  # trim only applies to vectors (ie, null dim)  

  if (!is.null(dim(obj)))
      return(obj[!sapply(seq(obj)[-1L], function(i) obj[i,]==obj[i-1,])])

  if (trim) {
    filler <- sapply(obj, identical, "", USE.NAMES=FALSE)
    obj <- obj[min(which(!filler)):max(which(!filler))]
  }
  
  return(obj[!sapply(seq(obj)[-1L], function(i) obj[[i]]==obj[[i-1]])])
}

#--------------------------------------------------------

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

#--------------------------------------------------------

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
#_________________________________________#

  #---------------------------------------#
 #_________________________________________#
###  GRAB COEFFICIENTS TABLE FROM SUMMARY ###

coefTable <- function(model) { 
  # captures the summary output table and returns it in a data.frame

  require(stringr)

  # this param indicates p-value less than machine precision. 
  #  we need to swap it out for the string splicing in read.table
  machPrec <- " < 2e-16"
  machPrec.replace <- "2e-16"

  # form a table of the pvalues, etc
  mout <- capture.output(summary(model))

  # find borders to the table, based on coefficients and ---   
  table.top <- grep("^Coefficients:", mout)  + 1
  table.bottom <- which(mout == "---")  - 1

  # if couldn't find bottom, look for next clue
  if (identical(table.bottom, numeric(0))) 
    table.bottom <- grep("^Residual standard error", mout) - 2
 
  # if still 0, count up 4 from bottom and issue warning
  if (identical(table.bottom, numeric(0))) {
    table.bottom <- length(mout) - 4
    warning("couldnt find exact bottom of table. Please confirm manually")
  }

  # get table
  m.table <- mout[(table.top+1):table.bottom]
  m.table <- sub("    $", " -- ", m.table)                     # clean significance column
  m.table <- sub(machPrec, machPrec.replace, m.table)           # clean p-value column
  m.table <- read.table(text=m.table, stringsAsFactors=FALSE)  # convert to matrix/datafrmae

  # Column Names
  cnames <- mout[table.top]
  cnames <- str_trim(mgsub(c("Std. Error", "t value", "Pr(>|t|)"), c("SE", "tVal", "pVal"), cnames)) 
  cnames <- c("Predictor", strsplit(cnames, " ")[[1]])

  # check if significance column is present.  (ie, there should be one more column than cnames)
    sigPresent <- ncol(m.table) > length(cnames)

  # add column names to table, adding Signif if column present
  colnames(m.table) <- if(sigPresent) c(cnames, "Signif")  else cnames
  
  # make signif column factor, if present. 
  if (sigPresent)
    m.table$Signif <- factor(m.table$Signif, levels=c("***", "**", "*", ".", "--"))

  return(m.table)
}


#_________________________________________#


splitEvery <- function(string, n, remSpace = FALSE)  {

  # if n is too small, return error
  if (n < 1)
    stop("n must be at least 1")

  # if vector, iterate over each
  if (length(string) > 1) {
    if(!is.ts(string))
      return(sapply(string, function(s) splitEvery(s, n)))
    return(sapply(seq(string), function(i) splitEvery(string[[i]], n)))
  }

  if(!is.character(string))
    string <- as.character(string)

  # remove space if selected
  if (remSpace)
    string <- gsub(" ", "", string)

  # for smaller n, do more quickly
  if (n == 1)
    return(strsplit(string, "")[[1]])

  if (n >= nchar(string))
    return(string)

  # error prevention: buffer will be added to end of string to avoid recycling of first letters  
  buffer <- rep("",  (0 - nchar(string)) %% n)

  if (n == 2)  {
    sst <- c(strsplit(string, "")[[1]], buffer)
    return(paste0(sst[c(TRUE, FALSE)], sst[c(FALSE, TRUE)]))
  }

  # else

  # create index vectors of T/F.  eg for n=4
    # T, F, F, F
    # F, T, F, F
    # F, F, T, F
    # F, F, F, T
  TrueFalseVec  <- rep(c(T, F), c(1, n-1))
  indexs <- lapply(rev(seq(n)), function(i)  TrueFalseVec[((1:n + i-1) %% n) + 1])

  # split the string by letter, adding buffer at end (to avoid recylcling of letters)
  sst <- c(strsplit(string, "")[[1]], buffer)

  # outer apply loop simply pastes the letters back together
  #  inner mapply loop selects the letters per group, using the F/T/F/F, etc/
  apply(mapply("[", list(sst), indexs), 1, paste0, collapse="")
}


#-----------------------------------

cls <- function(LINES=100) 
  cat(rep("\n", LINES))

#===================================================================#
  pkgFind <- function(toFind) { 
    # useful when you cant remember the capitalization, etc of a package
    #   ie, is it rCurl, RCurl, rcurl ... ? 
    pkgs <- dir(.libPaths())
    pkgs[stringr::str_detect(pkgs, stringr::ignore.case(toFind))]
  }


  regexAll <- function(pattern, stringVec, replace="@@@", ignore.case=FALSE, fixed=FALSE, perl=FALSE, value=FALSE) {
  # quick comparisons of the different family of regex options
  list("OriginalString"=x
            ,   "grep"     = grep    (pattern, stringVec, ignore.case=ignore.case, fixed=fixed, perl=perl, value=value)
            ,   "grepl"    = grepl   (pattern, stringVec, ignore.case=ignore.case, fixed=fixed, perl=perl) 
            ,   "regexpr"  = regexpr (pattern, stringVec, ignore.case=ignore.case, fixed=fixed, perl=perl) 
            ,   "gregexpr" = gregexpr(pattern, stringVec, ignore.case=ignore.case, fixed=fixed, perl=perl)  
            ,   "regexec"  = regexec (pattern, stringVec, ignore.case=ignore.case, fixed=fixed)  
            ,   "gsub"     = gsub(pattern, replace, stringVec, ignore.case=ignore.case, fixed=fixed, perl=perl)
            ,   "sub"      = sub (pattern, replace, stringVec, ignore.case=ignore.case, fixed=fixed, perl=perl)
            )
  }


#===================================================================#

        # --------------------------------------- #
        #                                         #
                  #------------------# 
                  # DATA TABLE UTILS #
                  #------------------# 
        #_________________________________________#


tbls <- function(envir=.GlobalEnv)  
  # shorter tables() summary with column count
  tables(env=envir, silent=TRUE)[,list(NAME, MB, NROW, NCOL= 1 + stringr::str_count(COLS, ","), KEY)]  


colquote <- function(colNamesAsStrings) {
  # Converts a vector of strings to a quoted (expression) list. 
  #  eg:  converts:   c("colName1", "colName2")
  #       to:         quote(list(colName1, colName2))
     
    as.call(lapply(c("list", colNamesAsStrings), as.symbol))
  }

            #---------------# 

  uniqueRows <- function(DT) { 
    # IF DT IS KEYED, FUNCTION ACTS SIMILAR TO unique.data.frame(.)

    # If not keyed (or not a DT), use regular unique(DT)
    if (!haskey(DT) ||  !is.data.table(x) )
      return(unique(DT))

    .key <- key(DT) 
    setkey(DT, NULL)
    setkeyE(unique(DT), eval(.key))
  }   


  getdotsWithEval <- function () {
      dots <- 
        as.character(match.call(sys.function(-1), call = sys.call(-1), 
            expand.dots = FALSE)$...)

      if (grepl("^eval\\(", dots) && grepl("\\)$", dots))
        return(eval(parse(text=dots)))
      return(dots)
  }

  setkeyE <- function (x, ..., verbose = getOption("datatable.verbose")) {
    # SAME AS setkey(.) WITH ADDITION THAT 
    # IF KEY IS WRAPPED IN eval(.) IT WILL BE PARSED
      if (is.character(x)) 
          stop("x may no longer be the character name of the data.table. The possibility was undocumented and has been removed.")
      #** THIS IS THE MODIFIED LINE **#
      # OLD**:  cols = getdots()
      cols <- getdotsWithEval()
      if (!length(cols)) 
          cols = colnames(x)
      else if (identical(cols, "NULL")) 
          cols = NULL
      setkeyv(x, cols, verbose = verbose)
  }

#_________________________________________#




#-------------------------------------#
##  FUNCTIONS
#-------------------------------------#
  shift <- function(x)
    c(x[-1], x[1])

  shiftb <- function(x)
    c(x[length(x)], x[-length(x)])

  namesdetect <- function(x, pattern)
    names(x)[grepl(pattern, names(x))]

  namesIn <- function(x, vec, positive=TRUE)
    names(x)[xor(!positive,  names(x) %in% vec)]

  namesNotIn <- function(x, vec)
    namesIn(x, vec, positive=FALSE)

    
  orderedColumns <- function(DT, frontCols=NULL, ignoreCase=TRUE, endCols=NULL) {

    # function to ignore case
    ifToUpp <- if (ignoreCase) toupper else function(x) x

    # returns metric columns in an ordered fashion
    nm <- names(DT)

    # set of columns to frontCols, if not supplied
    if (!length(frontCols)) 
      frontCols <-  c("artistID", "concertID", "Date", "Day", "artistName", "state", "venue", "perc", "isTraining", "name", "day", "month", "year", "MinDate")
    
    # which columns are `ends`
    ends <- ifToUpp(nm) %in% ifToUpp(endCols) 

    # which columns are 'non-metrics' and not `endCols`    
    non <- ifToUpp(nm) %in% ifToUpp(frontCols) 

    # reorder: first the `non-metrics` in the order they appeared
    #          then the `metrics` ordered alphabetically
    c(nm[non & !ends], nm[!non][order(nm[!non])], nm[ends])
  }


  combineRows <- function(x)
    if (all(is.na(x))) as.numeric(NA) else 
        if(anyDuplicated(x)) max(x, na.rm=TRUE) else sum(x, na.rm=TRUE)
      
#-------------------------------------#



wordCount <- function(obj, words, ignore.case=TRUE, preservePunct=FALSE) {
# basic word count, whole words only
# obj is the source to search for and count words
# words can be a single word, a vector or list of words, or it can be blank (for just a "total word" count)
# words, if it is only one word, does not to be quoted. 
# preservePunct, if TRUE, punctuation will be considered part of a word. 

  # split on whitespace and punctuation, unless flagged not to use punct. 
  splitOn <- "[[:space:]]"
  obj.split <- strsplit(obj, splitOn)

  if (!preservePunct)
    obj.split <- lapply(obj.split, gsub, pattern="[[:punct:]]", replacement="")

  # extra spaces etc, will have nchar of 0. Count only those > 0.
  .totalWords <- sapply(obj.split, function(x) sum(nchar(x) > 0))

  #initialize
  results <- NULL

  # count occurance of specific word
  #--------------------------------#
  if(!missing(words)) {
    # check if words exists and is character
    .tried <- try(sapply(words, is.character), silent=TRUE)
    if (inherits(.tried, "try-error") || !all(sapply(words, is.character)))
      words <- as.character(match.call()[[3]])

    # in case words is a list instead of a vector
    words <- unlist(words)

    # for each words, count the number of occurences in each x
    .wordCount <- sapply(words, function(word)
                    sapply(obj.split, function(x) sum(grepl(word, x, ignore.case=ignore.case)) ) )

     results <- data.frame(.wordCount)

  } else words <- NULL
  #--------------------------------#

  results <- data.frame(cbind(results, .totalWords))
  colnames(results) <- c(words, "TotalWords")
  rownames(results) <- names(obj)

  return(results)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

  dateCheck <- function(d) {
    if (lubridate::is.Date(d))
      return(d)
    return(lubridate::ymd(as.character(d)))
  }


  `%ni%` <- Negate(`%in%`) 

  # wrapper for use in *ply functions
  is.allNA <- function(x)
    all(is.na(x))

  invDict <- function(dict) 
  # inverts a dictionary (ie, swapping the names with the values)
    setNames(names(dict), dict)

setNamesDict <- function(DT, dict, replaceMissing=NULL, silent=FALSE) {
#  Replaces names of DT with values of `dict` where ever there is a match
#     between `names(dict)` and `names(DT)`
#  dict should be  "oldColumnName" = "newColumnName"
#  If replaceMissing is specified, column names of DT which are not present
#     in dict will be replaced with the value of replaceMissing

  nm <- names(DT)

  # check if dict needs to reversed
  if(sum(dict %in% nm) > sum(names(dict) %in% nm)) {
      dict <- setNames(names(dict), dict) 
      if (!silent)
        warning("inverting dictionary")
  }
  
  # check if there are no matching values
  matched <- nm %in% names(dict)
  if (!any(matched)) {
      if (!silent)
        warning("No values in the dict match the column names of the data.table")
      return(FALSE)
  }

  # Only partial matches... 
  if (!all(matched)) {
    # optionally warn: 
    if (!silent)
      warning("Not all names present: ", sum(!matched), " missing", ifelse(missing(replaceMissing), ".", " and being replaced."))

    # optionally replace missing values
    if (!missing(replaceMissing))
      setnames(DT, nm[!matched], paste(replaceMissing, 1:sum(!matched), sep="."))
  }

  setnames(DT, nm[matched], dict[nm[matched]])

  return(TRUE)
}

uniqueKeys <- function(DT) {
# returns all unique keys of a given data.table
  if (!is.data.table(DT))
    stop ("DT passed to `uniqueKeys(DT)` is not a data.table")
  unique(DT[,.SD, .SDcols=key(DT)])
}

#~~~~


convertClass <- function(DT, colnameVector, to, from=NULL, originDate="1970-01-01", excelOriginName=".xlorigin")
   UseMethod('convertClass')

convertClass.default <- function(DT, ...)
    stop(match.call()[[2]], " is not a data.table. (This function only works on data.tables)")

convertClass.data.table <- function(DT, colnameVector, to, from=NULL, originDate="1970-01-01", excelOriginName=".xlorigin")  {

  # possible values for from: 
  #   c("percent")


  # Convert from... 
  if(!is.null(from) && !is.na(from) && !from=="") {
    if(from=="percent") {
      for (cname in colnameVector)
        DT[, c(cname) := gsub("%", "", get(cname))]
    } else if (from=="excel" & to=="date") {
        if(!exists(excelOriginName))
          stop("Need excel origin date to convert. Cannot find ", excelOriginName, ".")  
  
       for (cname in colnameVector)
          DT[, c(cname) := as.Date(as.numeric(as.character(get(cname))), origin=get(excelOriginName))]
    } else {
      stop("dont know how to convert from ", from)
    }
  }


  # convert to:
  #--------------#

  ## FACTOR
  if (to == "factor") { 
    for (cname in colnameVector)
      DT[, c(cname) := factor(as.character(get(cname)))]
  
  ## DATE
  } else if(to=="date") {
    if (!from=="excel") {
      for (cname in colnameVector)
        DT[, c(cname) := as.Date(as.numeric(as.character(get(cname))), origin=originDate)]
    }

  ## GENERAL
  } else {
    for (cname in colnameVector)
      DT[, c(cname) := as(as.character(get(cname)), to)]
  }
} 

#-------- end convertClass.data.table  --------#

areEqual.slow <- function(x, na.rm=TRUE) {
  # checks if all elements in a single vector are equal
  # returns TRUE / FALSE

  # check input type  
  if(!is.null(dim(x)) && !is.matrix(x))
    stop("x must be a vector, a list, or a matrix")

  if(na.rm)
    x <- x[!is.na(x)] 

  # Compare each emelent in x against x[[1]]. They should all be the same. 
  ret <- sapply(x, all.equal, x[[1]])

  # If there were some that were not the same, ret will have a value other than T/F
  #   hence we need to check first that it is logical, and then that it is TRUE
  all(sapply(ret, function(y) is.logical(y) && y))
}

areEqual <- function(x, na.rm=TRUE, tolerance = .Machine$double.eps ^ 0.5, NoWarnings=FALSE) { 
#  Depends on:  areEqual.slow()
#
# checks if all of the elements of a vector, list, or matrix are equal
# Returns TRUE / FALSE  (does not give info on what is not equal)

  # check input # 
  #--------------------------------------#
    if(!is.null(dim(x)) && !is.matrix(x))
      stop("x must be a vector, a list, or a matrix")

    if(!length(x)) {
      if (!NoWarnings)
        warning("x is length 0")
      return(TRUE)
    }
  #--------------------------------------#

  # if checking if the elmeents of a list, use slower method, 
  #  which iterates over each element in the list
  if(length(x[[1]]) > 1)
    return(areEqual.slow(x, na.rm))

  # check if the whole vector is nothing but NA's
  if (all(is.na(x))) { 
    # if we remov NA's, issue a warning before returning TRUE
    if (na.rm && !NoWarnings)
      warning("`TRUE` was returned even though all ", length(x), " elements of x are NA.")
    return(TRUE)
  }

  # if x is a factor, we will compare its numeric value
  if (is.factor(x))
    x <- as.integer(x)

  # otherwise, calculate the range and compare the max and the min.
  #  Note that this works even if `x` is `character`
  rng <- range(x, na.rm=na.rm)

  ## TODO:  I had this next portion ni here before adding the check for `all(is.na(x))` (two sections up)
  ##        I think this is no longer needed, but not certain. Specifically, the `is.infinite` part. 
  ##        When else could I get unexpected infiinities? 
  # # if we're not removing NA's we need to check for all NA
  # if(all(is.na(rng)) || all(is.infinite(rng)))
  #   return(all(is.na(x)))

  # else: 
  ret <- all.equal(rng[[1]], rng[[2]], tolerance=tolerance)

  is.logical(ret) && ret
}

CamelCaseSplit <- function(string,  flat=FALSE) { 
  ## TODO:  Preserve   TheAmericanADLLeague
  splat <- strsplit(string, "(?<!^)(?=[A-Z])", perl=TRUE)
  
  if (flat)
    return(unlist(splat, recursive=FALSE)) 
  return(splat)

  # This does not work: 
  # http://stackoverflow.com/questions/7593969/regex-to-split-camelcase-or-titlecase-advanced
  #   strsplit(string, "(?<!(^|[A-Z]))(?=[A-Z])|(?<!^)(?=[A-Z][a-z])", fixed=TRUE)
}

# --------------------------------------------------------------------------------------------------- #

  # Wrappers for lapply/sapply to simplify two common uses
  #  gapply simplifies  `lapply(X, function(x) someFunc(get(x)))` to `gapply(X, someFunc)`
  #  xapply simplifies  `lapply(X, function(x) someFunc(x))`      to `xapply(X, someFunc)` 

  gapply <- function(X, FUN, ..., simplify=FALSE, pos=1){
    # this is just a wrapper for lapply/sapply, where X is the name of an object and hence needs to `get`'d
    # `get` is applied in the environment where `gapply` is called. If we want to get from the environment prior, increment pos
    en <- parent.frame(pos); force(en) # force it to make sure it evaluates
    print(en)
    sapply(X, function(x) FUN(get(x, envir=en), ...), simplify=simplify)
  }

  xapply <- function (X, qFUN, ..., simplify=FALSE) { 
    mc <- match.call()
    sapply(X, function(x) eval(mc[[3]]), ..., simplify=simplify)
  } 

  #         ## gapply test: 
  #         test.g1 <- "right"
  #         test.dummy <- "dummy"
  #
  #         gapply.test <- function(x) {
  #           print(x)
  #         }
  #
  #         gapply.mid <- function(x, pos=1) {
  #          cat("        pos in mid is ", pos, "\n")
  #           test.g1 <- "wrong -- mid"
  #           gapply(x, gapply.test, pos=pos+1)
  #           cat("\n")
  #         }
  #         test.objNames <- c("test.g1", "test.dummy")
  #         
  #         gapply.outter <- function(objNames, pos=0) {
  #           test.g1 <- "wrong -- outter"
  #           gapply.mid(test.objNames, pos=pos+1)
  #         }
  #       
  #          gapply.outter(test.objNames, pos=0)
  #          invisible(gapply(test.objNames, gapply.mid, `...`=list(pos=0)))

# --------------------------------------------------------------------------------------------------- #

catTitle <- function(Title, pref="", suf="", tabs=1, topline=FALSE, dash="-", center=TRUE) { 

  ## TODO: Figure out a situtaiton where this would be a problem. else delete. 
  # if (length(Title) > 1)
  #   warning("`Title` should be a single string. Results unpredictable")

  if(is.na(as.numeric(tabs))) {
    warning("`tabs` should be an integer. Defaulting to 1.")
    tabs <- 1
  }

  # split on "\n" if present
  Title <- unlist(strsplit(Title, "\n"))

  # if pref or suf are numeric, they represent number of "\n"
  if (is.numeric(pref)) pref <- pasteC(rep("\n", pref))
  if (is.numeric(suf)) suf <- pasteC(rep("\n", suf))

  # count the longest line
  nc <- max(nchar(Title))
  if (center) 
    nc <- max(nchar(stringr::str_trim(Title)))

  # the dashes are two spaces shorter than nc
  tabs   <- pasteC(rep("\t", tabs))
  sep    <- paste0("\n", tabs)
  
  dashes  <- pasteC(rep(dash, nc-(2*!center)))
  if (!(center || (nc %% 2)))
  dashes  <- paste0(" ", dashes, " ")
  preline <- if(topline) paste0(tabs, dashes) else character(0)
  Title   <- paste0(Title, collapse=sep) 
  ret     <- paste(preline, Title, dashes, sep=sep)

  if (center)
    ret <- centerText(ret, trim=TRUE, tabs=tabs)

  ret     <- paste(pref, ret, suf, "\n", sep="")
  cat(ret)
}

centerText <- function(x, eol="\n", padWith=" ", trim=TRUE, tabs="")
  alignText (x, eol, padWith, trim, tabs, halign="center")


alignText <- function(x, eol="\n", padWith=" ", trim=TRUE, tabs="", halign="center") {
# depends: `pasteC`, `stringr` package
# padWith:  character used 
# tabs: a string of white-space at beginning of a line that should be preserved. 

## TODO:  double-check the portion with extracted, and `spaces`.  Might have to rethink it. 
  require(stringr)

  # match halign
  halign.table <- c("left", "center", "right")
  hmatched <- pmatch(tolower(halign), halign.table)
  if (is.na(hmatched)) {
    warning('halign value should be from c("left", "center", "right").\nDefaulting to "center"')
    hmatched <- which(halign.table=="center")
  }
  halign <- halign.table[hmatched]

  # replace "\t" in `padWith`
  # "\t" is one char. It will throw off calculations
  padWith <- gsub("\t", "    ", padWith)

  # split on eol and unlist
  text <- unlist(strsplit(x, eol))

  # remove `tabs` at start of any line, it will be added back after
  pat   <- paste0("^",tabs, " ?")
  extracted <- str_extract(string=text, pattern=pat)
  extracted <- ifelse(is.na(extracted), "", extracted)
  text  <- gsub(pat, "", text)

  # trim whitespace
  if (isTRUE(trim))
    text <- stringr::str_trim(text)

  # count the chars, and find the longest line
  ncs <- nchar(text)
  nce <- nchar(extracted)
  nce <- nce - max(nce)
  mx  <- max(ncs) + max(nce)

  # the number of spaces required on each side
  spaces <- (mx - ncs - nce) / 2 
  spaces <- spaces / min(1, nchar(padWith))
  # dividing by 1, in case nchar(padWith) > 1
  #  using min(.) in case nchar(padWith) == 0

  # round down for left, round up for right. 
  left  <- lapply(spaces, function(s) pasteC(rep(padWith, floor(s))))
  right <- lapply(spaces, function(s) pasteC(rep(padWith, ceiling(s))))

  # padd each line in text with appropriate spaces
  padded <- 
      if (halign=="left") {
          paste(extracted, text, left, right, sep="")
      } else if (halign=="right") {
          paste(extracted, left, right, text, sep="")
      } else {
        paste(extracted, left, text, right, sep="")    
      }

  # paste back together with "\n" or other eol
  return(paste(padded, collapse=eol))
}

printBox <- function(x, width=68, dash="~", sides="#", crop=FALSE, topspace=0, bottomspace=0, tabs=1, header="") { 

  splat <- splitToWidth(x, width)
  ncs <- nchar(splat)

  # Add header if given 
  if (!is.na(header) && nchar(header))    
    splat <- c(Header, splat)

  # should the total width be based on the width param or 
  #   instead cropped down to the longest line
  mx <- ifelse(crop, max(ncs), width-2)
  
  # add spacer lines
  ## TODO:  
  splat <- c(rep("", topspace),  splat, rep("", bottomspace))

  # Add tabs. Converting numerics to string of spaces. 
  if(is.numeric(tabs))
    tabs <- pasteC(rep("  ", tabs))
  splat <- paste0(tabs, splat)

  # create line of dashes & add to splat
  dashes <- pasteC(rep(dash, mx))
  splat <- c(dashes, splat, dashes)

  # align text, padding with whitespaces
  padded <- alignText(splat, halign="left", tabs=tabs)

  final <- paste(sides, unlist(strsplit(padded, "\n")), sides, collapse="\n")
  cat(final)
}

splitToWidth <- function(x, width, safetyBreak=100) { 

  if (width < 20)
    stop("Width too small. Stay above 20")

  splat <- unlist(strsplit(x, "\n"))
  ncs   <- nchar(splat)
  tooWide <- (ncs > width)
  
  while(any(tooWide) && safetyBreak > 0) {

    # where to split, and where to force-insert white space
    TwoThirds  <- max(floor(width*2/3) - 5, 2)
    sp <- max(3, floor(width-TwoThirds / 2) - sample(1:5, 1))

    firstPart  <- substr(splat[tooWide], 1, TwoThirds)
    secondPart <- substr(splat[tooWide], TwoThirds+1, ncs[tooWide])

    hasWhiteSpace <- stringr::str_locate(secondPart, "\\s+")[, 'start']
    noWS <- is.na(hasWhiteSpace)

    if(any(noWS)){
      firstPart[noWS]  <- substr(splat[tooWide][noWS], 1, width-TwoThirds)
      secondPart[noWS] <- substr(splat[tooWide][noWS], width-TwoThirds+1, ncs[tooWide][noWS])
    }

    # secondPart[noWS] <- paste0(substr(secondPart[noWS], 1, sp), "- "
    #                          , substr(secondPart[noWS], sp+1, max(ncs)))
    secondPart <- stringr::str_replace(secondPart, "\\s+", "\n") 

    splat[tooWide] <- paste0(firstPart, secondPart)

    # re-split
    splat <- unlist(strsplit(splat, "\n"))
    ncs   <- nchar(splat)
    tooWide <- (ncs > width)
    safetyBreak <- safetyBreak - 1
  } 

  if(safetyBreak < 1)
    warning("Did not fully split")

  return(splat)
 }


isFALSE <- function(x) {
  if (is.logical(x))
    return(identical(x, FALSE))
  if (is.character(x))
    return(toupper(x)=="F")
  return(FALSE)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#     Model  Call Description              #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

modelDescrFromCall <- function(...) {
  UseMethod("modelDescrFromCall")
}

modelDescrFromCall.Arima <- function(M) {
# creates model description from the `call` entry of 
# the model output

  # call description from model
  parts <- M$call

  # extract needed parts
  type <- toupper(parts[[1]])
  ordr <- parts$order
  seasonal <- parts$seasonal

  s.out <- character()
  if(!is.null(seasonal))  {
    s.ordr <- seasonal$order
    s.prd  <- seasonal$period
    s.out  <- paste0(" x ", paste.call(s.ordr),"_", s.prd)
    ## TODO:  Allow for expressions in title, ie (..)[12] instead of _12
  }

  paste0(type, " ", paste.call(ordr), s.out)
}


modelDescrFromCall.lm <- function(M) {
# creates model description from the `call` entry of 
# the model output

  # call description from model
  parts <- M$call

  # extract needed parts
  type <- toupper(parts[[1]])
  formul <- parts$formula

  paste(type, paste.call(formul))
}

modelDescrFromCall.default <- function(M) {
# Returns as a nice string the call element of 
# model M. 
# If call element does not exist, returns NA. 

  # if M has no call element, return NA
  if(!"call" %in% names(M))
    return(NA)

  # call description from model
  parts <- M$call

  # extract needed parts
  type <- toupper(parts[[1]])

  paste(type, paste.call(parts))
}

# wrapper of paste call with defaults
paste.call <- function(ordr) {
  if(inherits(ordr[[1]], "name"))
    ordr <- ordr[-1]
  paste0("(", paste(ordr, collapse=", "), ")")
}

      # ------------ #

modelDataSetFromCall <- function(...) {
  UseMethod("modelDataSetFromCall")
}

modelDataSetFromCall.Arima <- function(M) { 
  # if M has no call element, return NA
  if(!"call" %in% names(M))
    return(NA)

  ret <- as.character(M$call$x)
  ret <- ret[!ret == "("]
  return(ret)

    # ----------------------------------------------------------------------------------------------- #
    #     This is for the findFnsInFile() that gets thrown off by the extra paren string above        #
          ignoreThis <- ")"                                                                            
          rm(ignoreThis)                                                                               
    # ----------------------------------------------------------------------------------------------- #
}

modelDataSetFromCall.lm <- function(M) { 
# depends on:  `areEqual()`

  # if M has no call element, return NA
  if(!"call" %in% names(M))
    return(NA)

  parts <- M$call
  
  # if data is explicitly set
  if(!is.null(parts$data))
    return(as.character(parts$data))

  # else
  if(is.null(parts$formula))
    return(NA)  # dont know how to parse

  # else - check the formula parts for the data name
  forml <- parts$formula
  forml.parts <- forml[!sapply(forml, function(x) is.name(x) )]
  forml.parts <- as.character(forml.parts)

  # split the formula portions into components
  splat1 <- unlist(strsplit(forml.parts, "\\s*(:|\\+)\\s*"))
  
  # we split on '$', and in each formula part, all terms except the last
  splat <- strsplit(splat1, "\\$")
  header <- unique(lapply(splat, head, -1))

  # if they are all the same, that is the data set
  if(length(header)==1)
    return(paste(header[[1]], collapse="$"))

  # else cannot determine data set
  return(NA)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #



dimCompare <- function (..., decr=NA, decreasing=decr, sortOn=NA)  {

  ## TODO:  Generalize. This works only if the dots come first. 
  nms <- as.character(match.call()[-1][1:length(list(...))])

  dims <- lapply(nms, function(x) dim(get(x)))
  dims <- do.call(rbind, dims)

  dimnames(dims) <- list(nms, c("row", "col"))

  # sort 
  if(!is.na(decreasing) || !is.na(sortOn)){

      # try to match sortOn
      if (!is.na(sortOn))
         sortOn <- pmatch(sortOn, c("row", "col"))
 
      # if missing, sort on whichever column has the largest variance
      if(is.na(sortOn)) {
        co <- sd(dims[, "col"])
        ro <- sd(dims[, "row"])
        sortOn <- ifelse(co > ro, "row", "col")
      }

      if (is.na(decreasing))
        decreasing <- TRUE

      dims[order(dims[, sortOn], decreasing=decreasing), ]
  }

  if (nrow(dims)==2)
    dims <- rbind(dims, "DIFF" = abs(apply(dims, 2, diff)))
  
  return(dims)
}

rangesFromInt <- function(int, numberOfRanges, sizeOfEach, pairs=TRUE, aslist=TRUE, sequence=TRUE, fractions=FALSE) { 

  if(!missing(numberOfRanges) & !missing(sizeOfEach))
    warning("Can use both, `numberOfRanges` and `sizeOfEach`. Defaulting to `sizeOfEach`")

  # if (pairs), then at the end we will be subtracting 1. Hence add it in now. 
  int <- int + pairs

  if (! missing (sizeOfEach)) {
      rngs <- seq(from=1, to=ceiling(1 + int/sizeOfEach)*sizeOfEach, by=sizeOfEach)
      rngs[length(rngs)] <- int
  } else {
      rngs <- seq(from=1, to=int+pairs, length.out=numberOfRanges+1)
  }

  if(!fractions)
    rngs <- round(rngs)

  if(!pairs)
    return(rngs)
  
  FUNC <- if(sequence) seq else c
  
  ret <- mapply(FUNC, head(rngs, -1), tail(rngs, -1) - 1, SIMPLIFY=FALSE)

  if (aslist)
    return(ret)

  return(do.call(rbind, ret))
}


# ------------------------------------------------------------------------ #
#          Wrappers for importing data form SO questions                   #
                                                                                
  r.t <- function(x=clipPaste(), header=TRUE, sep=NULL, to=NULL, value=!(is.character(to)), pos=1, file=NULL) {
    mc <- match.call()
    if(missing(to) && length(mc) > 1 && !(is.character(mc2 <- mc[[2]])) && names(mc)[[2]]=="x") { 
        to <- as.character(mc2)
        if (is.null(file))
          x=clipPaste()
        value <- ifelse(missing(value), FALSE, value)
    }                         

    if(!missing(to) && !is.character(to))
      to <- as.character(substitute(to))

    args <- list(file=file, 
                 text=x,
                 header=header, 
                 stringsAsFactors=FALSE,
                 sep=sep)
    args <- args[!sapply(args, is.null)]
    ret <- do.call(read.table, args)

    if (length(to) && !is.na(to)) {
      assign(to, ret, envir=parent.frame(pos))
      cat("Assigned to", to, "\n\n")
    }

    if (value)
      return(ret)

    return(invisible(TRUE))
  }                                                                              
  
  r.d <- function(x=clipPaste(), header=TRUE, sep=NULL, to=NULL, value=!(is.character(to)), pos=1, file=NULL) {                                  

    mc <- match.call()
    if(missing(to) && length(mc) > 1 && !(is.character(mc2 <- mc[[2]])) && names(mc)[[2]]=="x") { 
        to <- as.character(mc2)
        if (is.null(file))
          x=clipPaste()
        value <- ifelse(missing(value), FALSE, value)
    }    

    if(!missing(to) && !is.character(to))
      to <- as.character(substitute(to))
    
    ret <- data.table(r.t(x=x, header=header, sep=sep, to=NULL, value=TRUE, pos=pos+1, file=file))

    if (length(to)) {
      assign(to, ret, envir=parent.frame(pos))
      cat("Assigned to", to, "\n\n")
    }

    if (value)
      return(ret)
    return(invisible(TRUE))
  }                                                                              
# ------------------------------------------------------------------------ #


.First <- function(silent=FALSE) { 

  assign(".Pfm", Sys.info()[['sysname']], envir=.GlobalEnv)

  baseFolder <- ifelse(.Pfm=="Linux", "~/NBS-R/Ricks/src/", "~/git/misc/rscripts/" )
  try(source(paste0(baseFolder, "utilsRS.r")))
  
  if(!exists("as.path")) 
    as.path <- function(...) do.call(function(...) paste(..., sep="/"), list(...))

  utilsToSource <- c("workspace.R", "ListTransforms.R", "memoryFunctions.R")
  utilsFolder   <- as.path(baseFolder, "utils")
  utilsToSource <- as.path(baseFolder, "utils", utilsToSource)

  caught <- list()
  for (fil in utilsToSource)
    caught[[as.character(fil)]] <- try(source(fil))

  if (! any(sapply(caught, inherits, "try-error")))  {
      if(!silent) cat("\n\t\tUtils Loaded on Startup\n\n")
  } else 
    warning ("\n\tSome utils where not loaded\n")
    
  library(data.table)
}

howManyNAs <- function(x, returnPercentage=FALSE) { 

  if (exists("data.table") && is.data.table(x)) 
    if (returnPercentage)
     return(x[, c(lapply(.SD, function(col) sum(is.na(col)) / .N )) ])
    else 
     return(x[, c(lapply(.SD, function(col) sum(is.na(col)))) ])

  if (is.data.frame(x)) {
    if (returnPercentage)
      warning("Currently, `returnPercentage` is not implemented for data.frame's.\nReturning absolute counts instead.")
    return(as.data.frame(c(lapply(x, function(col) sum(is.na(col))) )))
  }

  if (is.list(x)) {
    if (returnPercentage)
      warning("Currently, `returnPercentage` is not implemented for lists.\nReturning absolute counts instead.")
    return( c(lapply(x, function(col) sum(is.na(col))) )) 
  }

  if (returnPercentage)
    return(sum(is.na(x)) / length(x))
  return(sum(is.na(x)))  
}

sortXbyY <- function(X, Y, justIndex=FALSE, names=FALSE, names.X=names, names.Y=names) { 
#  returns X, sorted by the order in Y

  if(names.Y)
    Y <- names(Y)

  # if justIndex is flagged, then using order instead of sort for returns
  sortFunc <- if(justIndex) order else sort

  if (names.X) {
    if(justIndex)
      return(sortFunc(names(X))[order(Y)] )
    return( X[ sortFunc(names(X))[order(Y)] ] )
  }

  # else
  return( sortFunc(X)[order(Y)] )
}

seasonFromDate <- function(D, factors=TRUE) { 
# given a date, return a factor/string of the corresponding Season

  # grab the month from the date
  M <- lubridate::month(D)
  # list of season values, in order
  Seasons <- c("Winter", "Spring", "Summer", "Fall")

  # Compute the numerical season-value 
  M.as.season <- ((M %% 12) %/% 3) + 1

  if (factors)
    return(factor(M.as.season, levels=c(1, 2, 3, 4), labels=Seasons))

  return(Seasons[M.as.season])
}

revString <- function(x)
        sapply(lapply(strsplit(x, NULL), rev), paste, collapse = "")

mds    <- function(includeInput=TRUE, x=clipPaste()) {
  mkdsh(x=x, includeInput = !isFALSE(includeInput) )  
}

mkdshr <- function(x=clipPaste(), space=TRUE, pound="#", dash="-", leftSpace=TRUE, includeInput=TRUE, top=TRUE, minWidth=20, align=NA, mindent=4, dontSmoothPreSpace=FALSE, fancy=FALSE, match=FALSE) {
## This function takes a header that contains space to its left and adds equal space to the right. 
## TODO:  Match does not work, and it will not work unless I strip any closing pounds. 
  
  ## this is the output of spacecnt
  if(exists("spacecntoutputvalues")) {
    mindent  <- spacecntoutputvalues[["mindent"]]
    minWidth <- spacecntoutputvalues[["minWidth"]]

    # this is a temp workaround until i properly account for pound spaces
    if (spacecntoutputvalues[["totalLength"]] - spacecntoutputvalues[["minWidth"]] == 4)
      minWidth <- minWidth + 2

    suppressWarnings(rm(spacecntoutputvalues, envir=.GlobalEnv))
  
    if (missing(top))
      top <- FALSE
  }


  ## TODO: double check minWidth & mindent
  ##      Currently, mindent is one too many, thus subtracting one
  mindent <- max(0, mindent-1)

  mc <- match.call() 

  # for shorthands, we're interested in evaluating what was typed at the console. 
  #  if the call came in through a wrapper, we want to evaluate that
  if (as.character(sys.call(1)[[1]]) == "mr")
    mc <- match.call(call=sys.call(1))

  # if there was at least one argument, then we will evaluate it
  mc2 <- if (length(mc) > 1)  mc[[2]]  else NULL
  
  ## Shorthands, allow for the first argument to be `align` or `top`
  if (is.numeric(mc2)) { 
      minWidth <- mc2
      x <- clipPaste()
  } else if(!is.null(mc2) && !is.logical(mc2)) {
    mc2 <- paste(as.character(mc2), collapse="")
    m <- substr(tolower(mc2), 1, 1)
    if (m %in% c("l", "c", "r", "n") && length(mc2) < 8) {   # "n" for "no align"
      align <- m
      x <- clipPaste()
    } else if (tolower(mc2)=="match") {
      match=TRUE
      x <- clipPaste()
    } else if (tolower(mc2)=="top") {
      x <- clipPaste()
      top <- TRUE      
    } else if (tolower(mc2)=="x") {
      x <- clipPaste()
    }
  }

  ## this allows to quickly set the other params in the function call
  if (x[[1]]=="x")
    x <- clipPaste()

  # for debugging, hold on to the original
  orig <- x

  # Collapse if multiple lines
  if (length(x) > 1) {
    x <- paste(x, collapse="\n")
  }

  ## remove any trailing  line-break  (normally from copy+pasting one extra line break)
  x <- gsub("\\n$", "", x)

  ## TODO:  Had this in here for some reason. Cannot remember why. Taking it out for now.
  # x <- gsub("\\\\n*", "", x)   # I dont remember why these are here, other than there was a use case that called for it. 

  # Split up the lines
  x  <- strsplit(x, "\n")[[1]]

  # remove any trailing spaces
  x <- sub("\\s*$", "", x)

  # any blank lines will get a starting pound, if all others have starting pounds
  blankLines <- which(x=="")

  if (match) {
    # identify the longest line with a closing pound
    minWidth <- max(nchar( grep(paste0(pound, "$"), x, value=TRUE) ))
  }


  # Bank & Strip any pre-white spaces
  wherePreSpace  <- regexpr("^\\s*", x)                                       ## Find  it
  preSpace <- substr(x, wherePreSpace, attr(wherePreSpace, "match.length"))   ## Bank  it  
  x <- substr(x, attr(wherePreSpace, "match.length")+1, nchar(x))             ## Strip it

  # Bank & Strip any opening pounds
  prePound  <- regexpr(pattern=paste0(pound, "+(%|\\*)*"), x)        ## Find  it
  leftPound <- substr(x, prePound, attr(prePound, "match.length"))   ## Bank  it  
  x <- substr(x, attr(prePound, "match.length")+1, nchar(x))         ## Strip it

  # Bank & Strip the main space
  whereSpace  <- regexpr(pattern="^\\s+", x)                          ## Find  it
  LSpace <- substr(x, whereSpace, attr(whereSpace, "match.length"))   ## Bank  it  
  x <- substr(x, attr(whereSpace, "match.length")+1, nchar(x))        ## Strip it

  ## add Pound to any blank lines
  leftPound[blankLines] <- names(which.max(table(leftPound)))

  # add top & bottom blank lines, if needed
  if (top) {
    x <- c("", x, "")
    topAndBottom <- function(vec, n=1) return(c(head(vec, n), vec, tail(vec, n)))
    leftPound <- topAndBottom(leftPound)
    LSpace <- topAndBottom(LSpace)
  }

  # for aligning left or right, extra spaces will be needed for pounds of differring widths
  poundPadding.count <- max(nchar(leftPound)) - nchar(leftPound)

  # total width of each line, minus any leftspace
  widths <- nchar(x) + 2 * (nchar(LSpace) + nchar(leftPound) + poundPadding.count) # padding left and right

  # the minimum width for all lines is the max amongst the requested width & all the widths
  minWidth <- ifelse(match, minWidth, max(widths, minWidth))   # if we are matching, then we stick to the minwidth

  ##$  TODO:   CHECK FOR LONG LINES ---    # Is the longestLine the longest padded pound
  ##$  TODO:   CHECK FOR LONG LINES ---    # long lines are any where they are within the poundPadding of the longest pound
  ##$  TODO:   CHECK FOR LONG LINES ---    widths - 2* nchar(leftPound)
  ##$  TODO:   CHECK FOR LONG LINES ---    longestLine <- intersect(which(nchar(x) == max(nchar(x))), which(nchar(leftPound) == max(nchar(leftPound))))


  # how many spaces does each line need  (note that the poundPadding gets compensated for here and in the switch statement)
  spacesNeeded <- minWidth - (nchar(x) + 2 * nchar(leftPound) ) + max(poundPadding.count)  # why 2*padding? In case the longest line also has the most 

## TODO:  look at this part in the next line: `min(spacesNeeded - poundPadding.count)`  Is that supposed to be minus? If so, why the `min`?
  # pad the spaces need with minimum of indent
  spacesNeeded <- spacesNeeded + max(0,  (2*mindent) - min(spacesNeeded - poundPadding.count))
  
  # if centering, divide spaces in half, otherwise just cound how many spaces on left, so that we know how many remain for right
  # if left align, use the smallest space for which there is text (ie nchar(x) > 0)
  # Left & Right aligns need to be adjusted for differing lengths of pounds. (Center is not neccssary)
  ncls <- nchar(LSpace)
  ncls.min <- ifelse(any(ncls > 0), min(ncls[ncls > 0]), 0)   # find the smallest ncls above zero, if it exists.
  minSpace <- max(mindent,  ncls.min)
  
  # check if any "should be" indented (6 Spaces more than the smallest non-zero space)
  extraIndent <- minSpace * as.integer(ncls > ncls.min+6)

  # calculate the number of spaces need on the LEFT and on the RIGHT
  LSpace.count <- 
    switch(substr(tolower(align), 1, 1), 
        l = {rep( minSpace,  length(spacesNeeded)) + poundPadding.count + extraIndent}, 
        r = {spacesNeeded - (minSpace + poundPadding.count + extraIndent)}, 
        c = {spacesNeeded / 2}, 
        ncls
     )

  LSpace.count <- floor(LSpace.count)
  RSpace.count <- spacesNeeded - LSpace.count

  # Create the spaces for each side
  LEFT  <- sapply(LSpace.count, pasteR, x=" ")
  RIGHT <- sapply(RSpace.count, pasteR, x=" ")

  # add the pounds back in
  LEFT  <- paste0(leftPound, LEFT)
  RIGHT <- paste0(RIGHT, revString(leftPound))

  # construct into single lines
  x <- paste(LEFT, x, RIGHT)

  ## Now all that is missing is the top and bototm dashes
  ##   and the preSpace on the left

  if (top) { 
    # if the leftPound are all the same length and all longer than length of pound, add a nice extra space
    nclp <- nchar(leftPound)
    addspace <- (fancy || (length(x) > 8 && all(nclp > length(pound)) && mean(nclp)==nclp[[1]]))

    # if we're not adding a space, the pound to use should be as long as the shortest pound already present
    if(!addspace)
      pound <- pasteR(pound, max(1, min(nclp)) )

    # use a slighlty shorter x, if we're adding space
    x.use <- ifelse (addspace,  substr(x, 2, nchar(x)-1)[[1]],  x[[1]])
    # create top & bottom bar from `mkdsh()`
    bar <- mkdsh(x.use, space=space, pound=pound, dash=dash, leftSpace=leftSpace, includeInput=FALSE )
    # add in the space
    bar <- ifelse(addspace, paste0(" ", bar, " "), bar)
    # add the bars into x
    x   <- c(bar, x, bar)
  }

  # smooth preSpace
  if (!dontSmoothPreSpace)
    preSpace[] <- preSpace[which.max(nchar(preSpace))]

  # add back any spaces in place before the `pound`  
  x <- paste0(preSpace, x)

  # collapse into a single string
  x <- paste(x, collapse="\n")

  clipCopy(x, sep="\n")
  return(invisible(x))
}

mr <- function(x=clipPaste(), mindent=9, minWidth=60, align="left", top=TRUE, match=FALSE, ...)
  mkdshr(..., top=top, mindent=mindent, minWidth=ifelse(match, 1, minWidth), align=align, match=match)


mkdsh <- function(x=clipPaste(), space=TRUE, pound="#", dash="-", leftSpace=TRUE, includeInput=TRUE) { 
# pound:  turn off by making it FALSE or making it ""
  
  ## this allows to quickly set the other params in the function call
  if (x[[1]]=="x")
    x <- clipPaste()

  ## TODO:  Are you sure you want to collapse? 
  # Collapse if multiple lines
  if (length(x) > 1) {
    x <- paste(x, collapse="\n")
  }


  ## Allow for a line of all whitespace. Thus, all the whitespace trimming is inside an if clause
  if (!identical(strsplit(x, "\\s*"), list(""))) {
    x <- gsub("\\n$", "", x)

    leftSpaces <- 0
    if(leftSpace)     # TODO:  turn this into regex with whitespace. Save the whole white space as a substring. Dont count it.  
      leftSpaces <- min(which(strsplit(x, " ", fixed=TRUE)[[1]]!="")) - 1
    original <- x


    x <- gsub("(^\\s*|\\s*$)", "", x)
  } else {
    leftSpace <- 0
    original  <- x
  }
  x <- sapply(strsplit(x, "\\n"), tail, 1)
  nc <- nchar(x)

  if (space)
    nc <- nc -2

  if (!isFALSE(pound) && nchar(pound))
    nc <- nc - 2*nchar(pound)

  dashes <- pasteR(dash, nc)

  if (space)
    dashes <- paste0(" ", dashes, " ")

  if (!isFALSE(pound) && nchar(pound))
    dashes <- paste0(pound, dashes, pound)

  if (leftSpace)
    dashes <- paste0(pasteR(" ", leftSpaces), dashes)

  # # output to console 
  # cat("\n\n", original,"\n", dashes, "\n\n", sep="")
  ## No need to output if using clipCopy

  if (includeInput)
    dashes <- paste(original, dashes, sep="\n") 

  # copy to clipboard
  clipCopy(dashes, sep="\n")
 
  # return invisibly
  return(invisible(dashes))
}

spacecnt <- function(x=clipPaste()) { 
## Counts the spacing for a given x (possibly in the clipboard)
## useful for mr()

  ## strip all white space at ends
  x <- gsub("^[[:space:]]*", "", x)
  x <- gsub("[[:space:]]*$", "", x)
  
  # count how many beginning or ending punctuation
  pounds.start <- attr(regexpr("^[[:punct:]]*", x), "match.length")                                     
  # for ending punct, we are looking for the same characters as that of pounds.start
  pounds.end   <- attr(regexpr(paste0(substr(x, 1, 1), "*$"), x), "match.length")   
  ## TODO:  Allow for mirrored punctuation start/end

  
  # count how many spaces in the substr after the pounds.start
  mindent <- attr(regexpr("^[[:space:]]*", substr(x, pounds.start+1, nchar(x))), "match.length")

  totalLength <- nchar(x)

  # minWidth is the total length of the stripped x, less any pounds on either end
  minWidth <- totalLength - sum(pounds.start, pounds.end)

  ret <- c(mindent=mindent, minWidth=minWidth, totalLength=totalLength)

  assign("spacecntoutputvalues", ret, envir=.GlobalEnv)

  return(ret)
}



dtWideToLong <- function(DT, cols=names(DT), cnames=c("Name", "Value")) { 
  copy(setnames(DT[, list(Name=rep(names(.SD), each=nrow(DT)), Value=unlist(.SD)), .SDcols=cols], cnames))
}


.a <- args


knito <- function(input, output=gsub("src", "out", dirname(input)), encoding="UTF-8", ...) {
  dir.create(dirname(output))  
  knit(input=input, output=output, encoding=encoding, ...)
}



mbench <- function(..., maxSeconds=20, maxReps=200L, verbose=TRUE, times=NA) { 
# aka benchPrep
  
  require(microbenchmark)
  
  dots <- list(...)

  ## Grab names, add to dots
  mc <- match.call()
  nms <- as.character(mc[(1:length(dots))+1])

  dots <- setNames(dots, nms)

  if(! all(sapply(dots, is.call)) )
    stop("All arguments must be quoted calls")

  # if times is not given explicitly, calculate it based on how long one execution of each takes
  if (is.na(times)) {
    times.matrix.single.run <- sapply(dots, function(x) system.time(eval(x)))

    user.times.single.run <- times.matrix.single.run["user.self", ]

    summed.run.time <- sum(user.times.single.run)

    times <- min(as.integer(floor(maxSeconds / summed.run.time)),  maxReps)
  } 

  times <- as.integer(times)

  if (verbose) {
    cat("Times for a singe run are: \n")
    print(data.frame(t(user.times.single.run)))
    cat("\n\tMicrobenchmark will run", times, "times")
    cat("\n\n\n")
  }

  return(microbenchmark(list=dots, times=times))
}

  # ------------------------------------------------------------------------------------------ #


utilSource <- function(.Pfm=Sys.info()[['sysname']], main=TRUE) {
## loads a series of util files from the utils folder
## if `main` is flagged TRUE, then will reload this utils file as well. 
 
  ## POSSIBLE MISSING FUNCTIONS
  if(!exists("as.path")) 
    as.path <- function(...) do.call(function(...) paste(..., sep="/"), list(...))
  if (!exists("plrl"))
    plrl <- function(x, y) return(x)


  utilsFolder <- ifelse(.Pfm=="Linux", "~/NBS-R/Ricks/src/utils", "~/git/misc/rscripts/utils")

  # only (re-)load the main utils file if flagged
  if (main) {
    caught.main <- try(source(as.path(utilsFolder, "../utilsRS.r")), silent=TRUE)
    if (inherits(caught.main, "try-error")) { 
      warning("Loading main `utilsRS` file was unsuccessful and encountered the following error\n\t\"", gsub("\n$", "", caught.main[[1]]), "\"\n")
    }
  }


  ## Load NBS Utils if on the science box
  if (.Pfm=="Linux") { 
    # Try to load, then report any error if present
    caught.nbsutils <- try(source("~/NBS-R/utils/utils.r"), silent=TRUE)
    if (inherits(caught.nbsutils, "try-error"))
      warning("Loading the NBS utils file was unsuccessful and encountered the following error\n\t\"", gsub("\n$", "", caught.nbsutils[[1]]), "\"\n")
  }

  utilsToLoad <- 
  c("dt_changeLevels.R", "findFnsInFile.R", "ggTSplot.R", "Introspection.R", "ListTransforms.R", 
  "memoryFunctions.R", "paraLineChop.R", "PlotMCestimateWithSE.R", 
  "reproduce.R", "sampleByGroup.data.table.R", 
  "setScience.R", "signifArima.R", "transferLibrary.R", "workspace.R")

  failed <- list()
  for (util in utilsToLoad) {
      caught.others <- try(source(as.path(utilsFolder, util)), silent=TRUE)
      if (inherits(caught.others, "try-error")) { 
        failed[[length(failed)+1]] <- util
      }
  }

  if (length(failed))
    warning("The following utils ", plrl("files were", failed), " not properly loaded:\n\t", paste(failed, collapse=",  "))
}

  # ------------------------------------------------------------------------------------------ #

plrl <- function(word.pluarl.form, count, singular=(length(count)==1)) { 
# Makes grammatically correct words based on the quanity of count

  plrl.single.dict <- c(were="was", are="is", have="has", files="file")

  # if vector of words
  if(length(word.pluarl.form) > 1)
    return(sapply(word.pluarl.form, plrl, count=count))

  # if single string of many words
  word.pluarl.form <- strsplit(word.pluarl.form, " ")[[1]]
  if (length(word.pluarl.form) > 1) { 
    ret <- sapply(word.pluarl.form, plrl, count=count)
    return(paste(ret, collapse=" "))
  }

  # Otherwise, proceed on just one word word.

  # if the word is singular process it
  if (singular) {
    # if the word is our dictionary, return its singular form
    if (word.pluarl.form %in% names(plrl.single.dict))
      return(plrl.single.dict[word.pluarl.form])
    # otherwise, return the word with a dropped final `s`, if found
    return(sub("(e)?s$", "", word.pluarl.form))
  } 

  # not singular, just return the word
  return(word.pluarl.form)
}


whichFactors <- function(x, names=FALSE) { 
# Identifies which columns in a df/dt (or elements in a list) are `factor`
# if names==TRUE, will return the names, else will return the indecies. 
  ret <- which(sapply(x, is.factor))

  # check if names are available. If not, throw a warning
  if (isTRUE(names) & is.null(names(ret)))
    warning("User flagged for names to be returned from `whichFactors` but names(x) is NULL.\nReturning indecies instead.")

  if (isTRUE(names) & ! is.null(names(ret)))
    return(names(ret))

  return(ret)
}

setkeyIfNot <- function(DT, ...) {
# sets the key to a DT, however, first checks if 
#  the key is already set to the given column(s)
#
# Purpose of this function is to save the overhead 
#    of setting the key when a key is already set.
  
  if (is.character(DT))
    DT <- get(DT, envir=parent.frame())


  dots <- as.character(substitute(list(...))[-1])

  if (length(dots)==1 && exists(dots, envir=parent.frame()))
    dots <- get(dots, envir=parent.frame())

  current <- key(DT)

  if (!identical(current, dots)) {
    setkeyv(DT, dots)
    return(TRUE)    
  }

  return(FALSE)
}


getNamesFromDTCols <- function(DT, na.rm=TRUE, uniquify=TRUE) { 
## The values in a DT column can have their own names, although may not be displayed. 
##  For example, if we run DT[, lapply(.SD, someFunc)]
##
## This function returns a vector of those names. 
## Specifically, it expects all of the columns in the DT to have the same structure   
  # grab the names from each element
  names.list <- lapply(DT, names)

  # grab only those that are not null
  whichNAs <- (sapply(names.list, length) == 0)
  if (na.rm) 
    names.list <- names.list[!whichNAs]
  else 
    names.list[whichNAs] <- NA 

  # grab the unique values
  if (uniquify)
    names.list <- unique(names.list) 
  
  return(names.list)
}


 # ----------------------------------------------------------------------------------------------------------- #

orderedHeadTail <- function(x, n=min(length(x), 5), na.last=TRUE, decreasing=FALSE, logical=FALSE, value=FALSE, f=c("head", "tail")) {
## User should not need to invoke this function, but instead the wrapper functions `orderHead` and `orderTail`
## returns an index to x indicating the top/bottom n values. 
## Useful for data.table indexing

  if (!is.atomic(x))
    stop("Currently, orderHead and orderTail are only implemented for atomic vectors. Try using unlist() or other workarounds.")

  len <- length(x)

  # set FUN to either `head()` or `tail()`
  f   <- match.arg(f)
  FUN <- match.fun(f)

   # ------------------------------------------------------------------- #
   #  human error check                                                  #
      if (n > len)                                                        
      warning("n is larger than ", len, "=length(x).  Using length(x).")  
                                                                          
      if (value & logical)                                                  
      warning ("Both `value` and `logical` were set to TRUE.\n",            
      "Retruning `value` superscedes (be careful if expected an index)")  
   # ------------------------------------------------------------------- #

  # grab the ordering
  ordering  <- order(x, na.last=na.last)   # note, the decreasing argument is not used in `order()` but rather in output
  
  # take the first or last n-many elements 
  indx.to.x <- FUN(ordering, n) 

  if (decreasing)
    indx.to.x <- rev(indx.to.x)

  if (value)
    return(x[indx.to.x])

  if (!logical)
    return(indx.to.x)

  # else, create a logical vector and return that
  ret <- rep(FALSE, len)

  ## TODO:  There should be a C way to flip these booleans
  ret[indx.to.x] <- TRUE

  return(ret)
}

orderedHead <- function(x, n=min(length(x), 5), na.last=TRUE, decreasing=FALSE, logical=FALSE, value=FALSE) 
  return(orderedHeadTail(f="head", x=x, n=n, na.last=na.last , decreasing=decreasing , logical=logical, value=value)) 

orderedTail <- function(x, n=min(length(x), 5), na.last=TRUE, decreasing=FALSE, logical=FALSE, value=FALSE) 
  return(orderedHeadTail(f="tail", x=x, n=n, na.last=na.last , decreasing=decreasing , logical=logical, value=value)) 

 # ----------------------------------------------------------------------------------------------------------- #


lib <- function(pkg, newest=FALSE, dependencies=TRUE, rforge=FALSE, update=FALSE) { 

  # newest is a shortcut for  rforge & update both TRUE
  if (newest)
    rforge <- update <- TRUE

  # The idea is that if package is not quoted, it should still work. 

  pkg.char <- as.character(substitute(pkg))

  ## TODO:  Cannot recall the logic here, 
  if (!(!exists(pkg.char) || is.character(pkg)))
    pkg <- pkg.char

  suppressWarnings(check <- do.call(require, list(eval(pkg))))

  ## if package is present and user did not force an update
  if (check && !update)
    return(invisible(check))

  # else
  repos <- ifelse(rforge, "http://R-Forge.R-project.org", getOption("repos"))
  install.packages(pkg, dependencies=dependencies, repos=repos)
  do.call(require, list(eval(pkg)))
}



## apply `is` to each element in a list, data.frame, etc, returning only the first response
are <- function(ll, simplify=TRUE) { 
  sapply(ll, function(x) is(x)[[1]], simplify=simplify)
}


## returns the char index to the last space in a string
findLastSpace <- function(x, space=" ") {
  if (length(x) > 1)
    return(sapply(x, findLastSpace))
  # stop ("x must be atomic")

  tail(gregexpr(space, x)[[1]], 1)
}

## finds all .R files within a folder and soruces them
sourceEntireFolder <- function(folderName) { 
  files <- list.files(folderName, full.names=TRUE)

  # Grab only R files
  files <- files[ grepl("\\.[rR]$", files) ]

  invisible(lapply(files, function(f) 
      try(source(f, local=FALSE, echo=FALSE), silent=TRUE)
    ))
}

## counts number of uique values for col in DT
cnt <- function(col, DT=defaultDT) {

  col <- substitute(col)

  DT[, 1, by=col][, .N]
}



mergeDTlist <- function(DTlist, suffixes=NULL, checkKeys=TRUE
                       , all=TRUE, all.x=all, all.y=all) {

  ## TODO:  Allow for DTlist to be the names of the table, by using `get()` further down in the code

  # DTlist should be an actual list of DT's.  If instead it is names, use `lapply(.., get)`
  if(all(sapply(DTlist, is.character)) && all(sapply(DTlist, length)==1)) {
    en <- parent.frame()
    DTlist <- lapply(DTsToMerge, get, envir=en)
  }

  ## check the keys 
  if (checkKeys) {
    intersection <- Reduce(intersect, lapply(DTlist, key))
    if (identical(character(0), intersection))
      stop("Cannot automatically merge this list of DTs. No shared key amongst the DTs")
    # else
    checkKeys <- FALSE 
  }

  ## set suffixes
  if (is.null(suffixes)) {
    suffixes <- paste0(".", fw0(length(DTlist)))
  }


  if (length(DTlist)==1)
    return(DTlist)
  if (length(DTlist)==2)
    return(merge(DTlist[[1]], DTlist[[2]], suffixes=suffixes, all=all, all.x=all.x, all.y=all.y))

  # if there are more than 2 elements, merge the 2nd into the 1st, and iterate
  DTlist[[1]] <- merge(DTlist[[1]], DTlist[[2]], suffixes=suffixes, all=all, all.x=all.x, all.y=all.y)
  DTlist[[2]] <- NULL
  suffixes <- suffixes[-2]

  return(mergeDTlist(DTlist, suffixes=suffixes, checkKeys=checkKeys, all=all, all.x=all.x, all.y=all.y))
}


regOr <- function(vec, brackets=TRUE, asterisk=NULL) { 
# combines a string vector into a regex `or` statement

  ret <- paste(c(vec), collapse="|")
  if (!brackets)
    return(paste0(ret, asterisk))

  # else return
  paste0("(", ret, ")", asterisk, collapse="")
}

greplAny <- function (pattern, x, ignore.case=FALSE, perl=FALSE, fixed=FALSE, useBytes=FALSE) {
# searchs for ANY value of pattern in `x`

  found <- sapply(pattern, grepl, x, ignore.case=ignore.case, perl=perl, fixed=fixed, useBytes=useBytes )
  ret <- rowSums(found) >= 1

  setNames(ret, x)
}

removeWord <- function(word, removeFrom, ignore.case=TRUE, preSpaceIfLastWord=TRUE) { 
# Removes `word` from `removeFrom`
  
  if (preSpaceIfLastWord)
    removeFrom <- gsub(paste0(" ?\\b", word, "$"), "", removeFrom, ignore.case=ignore.case)

  gsub(paste0("\\b", word, " ?\\b"), "", removeFrom, ignore.case=ignore.case)
}


lunique <- uniqlength <- function(x) {
  if (!is.null(dim(x)))
    return(dim(unique(x)))
  length(unique(x))  
}


expandGridByRow <- function(DT, vec, suffixes=c(".DT", ".vec"), keyToUse=key(DT), preserveList.vec=TRUE) { 
# preserveList.vec : if FALSE, we will attempt to coerce each list element into a DT column. If TRUE, we will leave each element as a DT row
# unlistSingle :  if TRUE, if vec is a list of length 1, it is treated as a vector

  # ----------------------------------------------------- #
  # Error Checks
  # ----------------------------------------------------- #
    # DT should be a data.frame or data.table
    if (!(inherits(DT, "data.frame")))
      stop("DT must be a data.table or data.frame")

    if (length(dim(vec)) > 2)
      stop("vec cannot be more than two-dimensional")
  # ----------------------------------------------------- #

  # ----------------------------------------------------- #
  #  Conver to data.tables
  # ----------------------------------------------------- #
    # if vec is not a data.table, convert to one
    if (!is.data.table(vec)) {
      # check if vec is a list (not a data.frame, data.table, etc)
      if (is.list(vec) && is.null(dim(vec))) {
        # if preserveList is set to TRUE, then we want to use `data.table(vec)` 
        #    not `as.data.table(vec)` as the latter "stands up" the list.
        vec <- if (preserveList.vec) data.table(vec) else as.data.table(vec)
      # check if it is an atomic vector
      } else if(is.null(dim(vec))) {
        vec <- data.table(vec)     
      # else, data.frame, etc, use `as.data.table`
      } else {
        vec <- as.data.table(vec)
      }
    }

    # convert to data.table, if not already. (easier for name-dup resolution)
    if (!is.data.table(DT))
      DT <- as.data.table(DT)
  # ----------------------------------------------------- #


  # ----------------------------------------------------- #
  # Ensure no duplicate names
  # ----------------------------------------------------- #

    # find any names present in both DT & vec
    dupNms <- intersect(names(DT), names(vec))

    # if there are any, resolve by appending suffix
    if (length(dupNms)) {
      setnames(DT,  dupNms, paste0(dupNms, suffixes[[1]]))
      setnames(vec, dupNms, paste0(dupNms, suffixes[[2]]))
    }

  # Note: When replicating DT & vec, one should repeat element(or row)-wise, one should repeat table-wise. 
  #       We will have DT be element-wise for two reasons. 
  #         (1) we can levarge key'ing and 
  #         (2) having vec repeat table-wise allows us to simply use cbind, and leverage R's recycling. 
  #             Alternatively, having vec repeat element wise is a lot more invovled, since we would 
  #             have to account for element-wise reps when vec is a vector and then row-wise when vec has dim. 

  # Make reps of DT
  # -------------------
  # reps is either the number of rows or the length of vec
  reps     <- ifelse(is.null(dim(vec)), length(vec), nrow(vec))
  DT.reppd <- data.table::rbindlist(replicate(reps, DT, simplify=FALSE))
  setkey(DT.reppd)

  # Add in reps of vec -  R will recycle vec automatically
  # -------------------
  DT.reppd <- cbind(DT.reppd, vec)

  # set key if `keyToUse` is not null
  if(length(keyToUse)) {
    if (all(keyToUse == "all")) {
      if ("all" %in% names(DT.reppd))
        warning("Problem with keying the expanded DT:\n  Argument `keyToUse` set to 'all', but there is also a column named 'all'.\n  Using all columns.")
      setkey(DT.reppd)
    }
    else 
      setkeyv(DT.reppd, keyToUse)
  }

  return(DT.reppd)
}

# ----------------------------------------------------------------------------------- #
validPercentage <- function(x, min=0, max=1, nm=substitute(x), silent=FALSE, fixAttempt=TRUE, stopif=FALSE) {
  #  Checks if x is inside [min, max]
  #  Returns the valid x if yes, FLASE if no
  #  fixAttempt:  if TRUE, this function will dividie or multiply by 100 to attempt to convert x to a valide percentage
  #               if x was modified and silent is TRUE, this function will throw a warning indicating as such
  #  stopif:  If TRUE and if x is not valid throws error.  
  #           If fixAttempt is also TRUE, will only throw error if failed to fix AND stopif is TRUE 

  # For now, this function only works on single values
  if (length(x) > 1 || !is.atomic(x))
    stop("`validPercentage` can only be called on a single value. Support for vectors, lists, etc is planned.")

  x.orig <- x 

  nm.char <- as.character(nm)
  nm <- ifelse (nm.char[[1]] == "[[", "x", paste0("`", deparse(nm), "`"))

  # Check for NA value in x
  if (is.na(x)) {
    if (!(silent))
      warning(nm, " has a value of NA and hence is not a valid percentage.\n")
    return(FALSE)
  }

  # Try to fix x.  
  x.was.fixed <- FALSE
  if (fixAttempt) {
    # how to fix
    if (max==1)
      fix <- function(z) z / 100
    if (max==100)
      fix <- function(z) z * 100

    # attempt to fix
    if (x < min || x > max) {
      x <- fix(x)
      x.was.fixed <- TRUE
    }
  }

  # If x is valid, return TRUE
  if (x >= min && x <= max) {
    if (!silent && x.was.fixed)
      warning("\n\n     ", nm, " should be a value in [", min, ", ", max, "].\n     ",
              nm, " was an invalid percentage, but has been\n     converted from ", x.orig, " to ", x, "\n")
    return(x)
    
  }

  msg <- paste0("\n", nm, " should be a value in [", min, ", ", max, "].\n", 
            nm, " = ", x.orig, " is not a valid precentage",
            ifelse(fixAttempt, " and could not be fixed.", ""),
            "\n")

  ## else, x is invalid
  
  # throw error, if flagged to do so
  if (stopif)
    stop(msg)

  # issue warning, unless flagged not to
  if (!silent)
    warning(msg)
#    warning("\n", nm, " should be a value in [", min, ", ", max, "].\n", 
#            nm, " = ", x.orig, " is not a valid precentage and could not be fixed.\n")

  # return FALSE (if no flag for errro)
  return(FALSE)
}

# ----------------------------------------------------------------------------------- #

dict.numbs <- c(
    "one" = 1
  , "two" = 2
  , "three" = 3
  , "four" = 4
  , "five" = 5
  , "six" = 6
  , "seven" = 7
  , "eigth" = 8
  , "nine" = 9
  , "ten" = 10

  , "14" = 14
  , "28" = 28
  , "29" = 29
  , "30" = 30
  )

# ----------------------------------------------------------------------------------- #

  as.num.as.char <- function(x)
    as.numeric(as.character(x))

  spliceOutDate.2.2.2 <- function(x, format="%m.%d.%y", simplify=TRUE) { 

    ## TODO: replace plus with {1-2}
    datePatterns <- c("[0-9][0-9]?\\.[0-9][0-9]?\\.[0-9][0-9]?")

    pat <- regOr(datePatterns)

    splat <- strsplit(x, " ")
    ret <- sapply(splat, function(x) grep(pat, x, value=TRUE), simplify=simplify)

    if(any(blanks <- sapply(ret, identical, character(0)))) {
      ret[blanks] <- NA    
      if (simplify)
        ret <- unlist(ret)
    }

    if (is.null(format) || is.na(format) || nchar(format)==0)
      return(ret)

    return(as.Date(ret, format=format))
  }


  meanIfThresh <- function(vec, thresh=12/15, len) { 
   # Calculates the mean of vec, however, 
   #   if the number of non-NA values of vec is less than thresh, returns NA 
  
   # thresh : represents how much data must be PRSENT. 
   #          ie, if thresh is 80%, then there must be at least 

    # for efficiency, allow len to be an argument. If not set, compute it. 
    if (missing(len))
      len <- length(vec)

    # find all NA's
    nas <- is.na(vec)

    # count how many NAs
    nacounts <- sum(nas)

    # if the proportion of NA's is greater than the threshold, return NA
    if( (nacounts / len) > thresh)
      return(NA_real_)
    # example:  if I'm looking at 14 days, and I have 12 NA's,
    #            my proportion is 85.7 % = (12 / 14)
    #           default thesh is  80.0 % = (12 / 15)
    #          Thus, 12 NAs out of 14 would be rejected
    

    # else manually compute the mean and return that 
    return(  sum(vec[!nas]) / (len-nacounts)  )
  }

  setFactorOrder <- function(fctr, order=sort(levels(fctr))) { 
  # Returns a factor ordered by order.  
  # If order is missing, defaults to  
  # Useful for ggplot, were ordering is based on the order of the levels

    if (!is.factor(fctr)) {
      warning("`fctr` is not a factor. Will coerce.")
      if (missing(order))
        order <- sort(unique(fctr))
    }

    factor(fctr, level=order)
  }

  # For when I'm too lazy to copy and paste
  dputc <- function(...) { 
    clipCopy(capture.output(dput(...)))
  }

  copyAsCol <- function(...) { 
    invisible(clipCopy(dput(...)))
  }


  s.t <- function(expr, msg="", verbose=TRUE, gcFirst=FALSE, title="") { 
  # wrapper for system.time with fancy output
  # title is an alternate for msg, where user needs simply give a name to the section being timed.
  # msg is for a custome message before the sytem.time output

    ret <- capture.output(system.time(expr=expr, gcFirst=gcFirst))
    ret <- paste(ret, collapse="\n")

    if (nchar(title))
      msg <- paste0("Time to complete ", title, ":")

    if (verbose){
      if (nchar(msg) == 0)
        cat(ret)
      else 
        cat(pasteC(msg), ret, sep="\n")
    }
  }

  verboseMsg <- function(verbose, ..., time=TRUE) {
    ## Wrapper function for verbose outputting
    ## time:  indicates whether or not to add time stamp
    
    if (!verbose) 
      return(invisible(NULL))

    if (time) {
      stamp <- format(Sys.time(), "%H:%M")
      stamp <- paste0(" -- [", stamp, "]")      

      dots <- list(...)
      lasteEl <- tail(dots, 1)[[1]]
      # if the last element is \n and only that (ie \n\n\n)
      if (grepl("\\n", lasteEl) && "" == gsub("\\n", "", lasteEl))
        out <- paste0(c(head(dots, -1), stamp, tail(dots, 1)), collapse="")
      else 
        out <- paste0(c(dots, stamp), collapse="")
      cat(out, "\n")
    } else {
      cat(..., "\n")
    }

    return(invisible(NULL))
  }

  is.twodim <- function(x) { 
    return(isTRUE(length(dim(x))==2))
  }

  has.listColumn <- function(x) { 
    return(any(sapply(x, is.list)))
  }

  wnames <- function(x, selection=NULL, copy=TRUE) { 
    nms <- names(x)
    names(nms) <- seq_along(nms)

    if (is.null(selection))
      return(nms)

    ret <- nms[selection]

    if (copy && .Pfm == "Darwin") {
      cat("\n\n (copied to clipboard)\n\n")
      dputc(unname(ret))

    }
    return(ret)
  }
    
  # meanIfThresh.old <- function(vec, thresh=12/15, len) { 
  #  # Calculates the mean of vec, however, 
  #  #   if the number of non-NA values of vec is less than thresh, returns NA 
  
  #  # thresh : represents how much data must be PRSENT. 
  #  #          ie, if thresh is 80%, then there must be at least 

  #   # for efficiency, allow len to be an argument. If not set, compute it. 
  #   if (missing(len))
  #     len <- length(vec)

  #   # if the proportion of NA's is greater than the threshold, return NA
  #   if( (sum(is.na(vec)) / len) > thresh)
  #     return(NA_real_)
  #   # example:  if I'm looking at 14 days, and I have 12 NA's,
  #   #            my proportion is 85.7 % = (12 / 14)
  #   #           default thesh is  80.0 % = (12 / 15)
  #   #          Thus, 12 NAs out of 14 would be rejected
    
  #   # else
  #   return(mean(vec, na.rm=TRUE))       
  # }




 # ----------------------------------------------------------------------------------------------------------- #


##  SIMILAR TO JESUS, BUT USES   write.table()  FOR MATRIX-LIKE OBJECTS THAT DO NOT HAVE LIST-LIKE COLUMNS
##   Once confirmed that this works properly, replace `jesus()`
jesus2 <- function(..., dir=ifelse(exists("outDir"), outDir, as.path(getwd(), "out")), subDir=sub, 
                                pos=1, sub=TRUE, stampDir=TRUE, stampFile=FALSE, summary=TRUE, envir="",
                                tablesAsCSV=TRUE,   row.names=FALSE, col.names=FALSE)  {
    ##  Like saveit() but can take multiple objects as arguments
    ##
    ##     saves objects passed as (...) arguments to file of type .Rda and with 
    ##     name of file same as name of obj + time stamp
    ##     in location: dir
    ##     tablesAsCSV:  if TRUE,  matrix-like (2-dim objects) will be written to csv
    ##     subDir:  if TRUE, will create subdir data_bak 
    ##                    inside dir and use that folder. (if alreaddy exists, will just use)
    #S     sub:  a synonym for subDir. (since use of ... does not allow for partial matches) 
    ##
    ## returns:  the path/to/file.Rda where objects were saved

    ## NOTE TO SELF:  You cannot use  `dots.list` and `list(...)` interchangeably in substitute
    ##                    dots.list <- list(...)
    

    # get objects from dots
    objNames <- as.list(as.character(substitute(list(...)))[-1L])

    # check for arguments being (eval(...))
    whichAreEval <- sapply(objNames, function(x) grepl("^eval\\(.+\\)$", x))

    if (any(whichAreEval))  {
      # confirm they are calls
      whichAreCalls <- sapply(substitute(list(...))[-1], is.call)
      # proceed only if they match
      if (identical(whichAreCalls, whichAreEval)) {
        objNames2 <-  list(...)[whichAreCalls]
        objNames <- unlist(c(objNames2, objNames[!whichAreCalls]))
      }
    }

### TODO:  June 2013.  Apparently the `eval(vector.of.obj.names)` was not working. I wrote the part immediately above this. 
###        Confirm all is working correctly.  
# -- check this -- #    # TODO:  double-check pos value.  It might be off. 
# -- check this -- #    # check any value is eval(XX), if so parse it. Collect all values into a single vector.   
# -- check this -- #    objNames <- unlist( lapply(objNames, function(ob) 
# -- check this -- #      if(substr(ob, 1, 5)=="eval(")   eval(parse(text=substr(ob, 6, nchar(ob)-1)), envir=ifelse(is.environment(envir), envir, parent.frame(pos+1)) )  else  ob
# -- check this -- #    ) )


    # No need to save any object twice
    objNames <- unique(objNames)

    #----- ERROR CHECKS ------#
    # If any of the assignment operators are found in the list, throw an error
    if(detectAssignment(objNames)) 
      stop("Cannot assign in the call to this function.")
    #----- ERROR CHECKS ------#

    # Check that the objects to be saved exist
    NotPresent <- !(sapply(objNames, exists))
    if (any(NotPresent)) {
      warning("The following objects were not found and hence could not be saved:\n    ", paste(objNames[NotPresent], collapse="    "), "\n")
      objNames <- objNames[!NotPresent]
    }

    # if flag is true, add appropriate subdir
    if (subDir) 
      dir <- as.path(dir, "data_bak")

    # add timeStamp to dir if required
    if(stampDir)
      dir <- paste0(as.path(dir), "_", timeStamp())

    # Create dir if needed
    dir.create(as.path(dir), recursive=TRUE, showWarnings=FALSE)


    if (tablesAsCSV) {

      ## Determine which are matrix-like
      twoDimmed <- gapply(objNames, is.twodim, pos=pos+1, simplify=TRUE)
      # determine which have list columns 
      hasLists  <- gapply(objNames, has.listColumn, pos=pos+1, simplify=TRUE)
      # keep only two dimmed that do not have list columns
      twoDimmed <- (twoDimmed)  & !(hasLists)

      # Track any failures  ## CURENTLY NOT IMPLEMENTED
      failed <- as.character(c())

      # isolate just those that will be CSV'd
      csv.objNames <- objNames[twoDimmed]

      # create the file paths, cleaning objNames of bad chars
      csv.fileWithPath <- sapply(objNames, mkSaveFileNameWithPath, ext=".csv", dir=dir, addTimeStamp=stampFile)

      for (i in seq_along(csv.objNames)) {
        obj <- get(csv.objNames[[i]], envir=pos+1)  # double check pos
        fil <- csv.fileWithPath[[i]]

        ## TODO: add try() and save any failures to `failed`
        write.table(obj, file=fil, append=FALSE, quote=TRUE, sep="|", 
                    row.names=row.names, col.names=col.names, qmethod="escape", fileEncoding="UTF-8")
      }

      # clear out those saved as CSV
      objNames <- objNames[!twoDimmed]
      objNames <- c(objNames, failed)
    }

    # create the file paths, cleaning objNames of bad chars
    rda.fileWithPath <- sapply(objNames, mkSaveFileNameWithPath, ext=".rda", dir=dir, addTimeStamp=stampFile)

    # Save the object
    tryCatch(mapply(function(obj, thefile)
              # note that with the save+do.call we are going in an extra two environments, hence pos + 2  (also, tested with pos+1, pos+3, both wrong)
              do.call(save, args=list(obj, envir=parent.frame(pos+2), file=thefile) )  # pos + 3 will be off if 
          , objNames, rda.fileWithPath), 
        error = saveErrorHandle)

            ## This does NOT work. 
            # filesCreated <- do.call(saveit, args=list(objNames, pos=pos+1, dir=dir, addTimeStamp=stampFile))
            # return(filesCreated )
    

    ret.fileWithPath <- c(csv.fileWithPath, rda.fileWithPath)
    # return the path/to/files or just a summary
    if (summary)
      return(list('quantity'=paste(length(ret.fileWithPath), "files were created in:"), 'dir'=dir))
    return('back.up.files'=ret.fileWithPath)
  }




 # ----------------------------------------------------------------------------------------------------------- #
 # ----------------------------------------------------------------------------------------------------------- #
 # ----------------------------------------------------------------------------------------------------------- #

invisible(c(utilsLoaded=TRUE))



