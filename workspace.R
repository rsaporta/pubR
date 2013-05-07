#===================================================================#

#-----------------------------------
#   WORKSPACE FUNCTIONS
#-----------------------------------


# source function `fresh()`
fresh <- function(save=TRUE, utils=TRUE, dt=TRUE, env=parent.frame(), all=NULL) {
# `all` is a shortcut argument. If set, all of the other flags will get the same value. 

  if(!is.null(all))
    save <- utils <- dt <- all

  if(save)
    saveImageTo(fresh=TRUE)

  ## CLEAN UP
  #---------------------------------------#
    # remove all packages
    .pkgs <- names(sessionInfo()$otherPkgs)
    sapply(.pkgs, function(pkg) eval(parse(text=paste0("detach(package:", pkg, ")"))))

    # remove all (most) namespaces. 
        # dependencies are listed as a comma delimd string. 
        #  split on the commas and count the number. 
        #  value of 1, usually indicates a dependency on ver #
        # Start with the highest number and work down
    attempts <- 10
    for (i in 1:attempts) { 
        # get all the namespaces
        .ns <- sessionInfo()$loadedOnly  # note, using loadedNamespaces() give insufficent information

        # count the number of dependancys 
        .counts <- sapply(.ns, function(x) ifelse(is.null(x$Depends), 0, length(strsplit(x$Depends, ",")[[1]])))
          
        # attempt to remove them, starting with the one with the highest count
        suppressWarnings(invisible(sapply(names(.ns[order(.counts)]), function(ns) try(unloadNamespace(ns), TRUE))))
    }

    # remove all objects
    eval(expression(rm(list=ls(all=TRUE))), envir=env)
  #---------------------------------------#

  setwd(path.expand("~"))

  # crearte platform variable in the global environment 
  assign(".Pfm", Sys.info()[['sysname']], envir=.GlobalEnv)
  
  # Load Utilities File
  if(utils) {
    sourceFile  <- path.expand(ifelse(.Pfm=="Linux", "~/NBS-R/Ricks/src/utilsRS.r", "~/git/misc/rscripts/utilsRS.r"))
    utilsLoaded <- try( source(sourceFile), silent=TRUE)
    if(inherits(utilsLoaded, "try-error"))
        warning("utils file not loaded. File tried: \n\t ", sourceFile, "\n")
  }

  # also load intenral utils & workspace functions (eg this file)
  if (.Pfm=="Linux") {
    try(source(path.expand("~/NBS-R/utils/utils.r")))
    try(source(path.expand("~/NBS-R/Ricks/src/utils/memoryFunctions.R")))
    try(source(path.expand("~/NBS-R/Ricks/src/utils/ListTransforms.R")))
    try(source(path.expand("~/NBS-R/Ricks/src/utils/workspace.R")))
    try(source(path.expand("~/NBS-R/Ricks/src/utils/setScience.R")))    
    try(source(path.expand("~/NBS-R/Ricks/src/utils/dt_changeLevels.R")))
  } else {
    try(source(path.expand("~/git/misc/rscripts/utils/memoryFunctions.R")))
    try(source(path.expand("~/git/misc/rscripts/utils/ListTransforms.R")))
    try(source(path.expand("~/git/misc/rscripts/utils/workspace.R")))
    try(source(path.expand("~/git/misc/rscripts/utils/setScience.R")))    
    try(source(path.expand("~/git/misc/rscripts/utils/dt_changeLevels.R")))    
  }

  if(dt)
    library(data.table)

  if(exists(".First"))
    .First(silent=TRUE)

  cat(rep("\n", 100))    # fake `cls()`
  cat(paste(rep(" ", 28), collapse=""), "So Fresh and So Clean")
  cat(rep("\n", 13))
}

isNBS <- function(dir=getwd(), projName="") { 
  ## Checks the directories for sign of currently in an NBS directory. Returns T/F accordingly
  
  if(exists(".Pfm") && .Pfm=="Linux")
    return(TRUE)

  dir <- unique(c(dir, getwd(), if(exists("dataDir"))dataDir, if(exists("wrkDir"))wrkDir), projName)
  return(any(grepl("nbs", tolower(dir))))
}


getProjName <- function(appendImgSave=FALSE, fresh=FALSE, pos=1) { 
## Checks the previous environment for the var `projName`
##   returns that value if found.  
## Also appends 'ImageSave' if appropriate

  # check the parent environments
  if (exists("projName", envir=parent.frame(pos))) {
    ret <- get("projName", envir=parent.frame(pos))
  } else{
    ret <- ""
  } 
 
  # append suffix to ret
  if (appendImgSave && !grepl("ImageSave", ret) && !fresh)       
      ret <- paste(ret, "ImageSave", sep=ifelse(nchar(ret), "_", "")) 
                    # use a sep only if projName has a value already
  return(ret)
}

saveImageTo <- function(projName, appendImgSave=TRUE, stamp=TRUE, fresh=FALSE, extension="Rda", dir="~/gitData/ImageSaves", .Pfm=Sys.info()[['sysname']], pos=1) { 
# Saves the image to file projName (with stamps) in the folder 'dir' (assigned by default)
# To load the image back, run `loadImageOf( <same parameters> )`
#  Note that default parameters are enviroment dependent, hence if using save/load, explicict params is recomended

  if(missing(projName)) {
    projName <- getProjName(appendImgSave, fresh=fresh, pos=pos+1)
    NotMissing <- FALSE
  } else
   NotMissing <- TRUE

  # if saving from fresh, append file name, to distinguish from manual saves
  if (fresh)
    projName <- ifelse(nchar(projName), paste0(projName, "_B4Fresh"), "ImageSave_B4Fresh")


  # save loaded libraries & projName
  assign(".libs", names(sessionInfo()$otherPkgs), envir=parent.frame(pos))
  # only assign projName if it was given explicitly in the call and there is no such value in the calling env. 
  if (NotMissing  &&  ! exists("projName", envir=parent.frame(pos), inherits=FALSE))
     assign("projName", projName, envir=parent.frame(pos))


  # create time stamp, if requested
  tms <- ifelse(stamp, paste0("_", timeStamp()), "")

  # extension may or may not already contain the dot, depending on user input. hence: 
  if (substr(extension, 1, 1) != ".")
    extension <- paste0(".", extension)

  # create file name
  fname <- paste0(projName, tms, extension)

  # search for "NBS" in  either `getwd` and `dataDir` for "NBS"
  # if found, switch the folder
  if (isNBS(dir))
     dir <- ifelse (.Pfm == "Linux",  "~/NBS-R/Ricks/ImageSaves", "~/gitData/nbs/ImageSaves")

  # create directory if not exist
  if(!file.exists(dir))
    dir.create(dir, recursive=TRUE)

  f <- as.path(dir, fname)
  ret <- try(save.image(file=f))

  if(inherits(ret, "try-error"))
    stop("File was not saved successfully.\nAttempted to save.image with destination: \n\n\t", f, "\n\n")
  
  return(f)
}

loadImageOf <- function(projName, appendImgSave=TRUE, stamp=TRUE, dir="~/gitData/ImageSaves", alldirs=FALSE, .Pfm=Sys.info()[['sysname']], nbs=FALSE, listdirs=FALSE, pos=1, verbose=TRUE) {  

  ## PARAMETER
  FullDirsList <- unique(c(dir, "~/NBS-R/Ricks/ImageSaves", "~/gitData/nbs/ImageSaves", "~/gitData/ImageSaves"))

  if(missing(projName)) {
      projName <- getProjName(appendImgSave, pos=pos+1)
      NotMissing <- FALSE
   } else
      NotMissing <- TRUE

  # helper.  Load file by number. Useful as a follow up call after using "?"
  if(is.numeric(projName) && projName < 100)  {
    projName <- imagesInDir(dir=FullDirsList, alldirs=TRUE)[projName]
    cat("\n\nYOU SELECTED:  [", projName, "]\n\n")
    return(loadImageOf(projName, pos=pos+1, alldirs=TRUE))
   }


  # helper. List all files.  First set `alldirs` to TRUE to grab all dirs to pass. 
  if(projName=="?")
    alldirs <- TRUE

  if(tolower(substr(projName,1,2))=="?d")
    listdirs <- TRUE
  
  if(tolower(substr(projName,1,2))=="?n")
    return(imagesInDir(dir=c("~/NBS-R/Ricks/ImageSaves", "~/gitData/nbs/ImageSaves")))
  

  # the second part of this is another helper function. listdirs. 
  if(alldirs || listdirs) {
    dir <- unique(c(dir, FullDirsList))
    if (listdirs) return(cbind(dir))

  # ELSE, check for NBS  by searching for "NBS" in  either `getwd` and `dataDir` for "NBS"
  } else if (missing(dir) && (nbs || (any(grepl("NBS", c(projName, getwd(), if(exists("dataDir")) dataDir),  ignore.case=TRUE))) )) {
      # if found, switch the folder
      dir <- ifelse (.Pfm == "Linux",  "~/NBS-R/Ricks/ImageSaves", "~/gitData/nbs/ImageSaves")
  }

  # helper.  List all files
  if(projName=="?")
    return(cbind(imagesInDir(dir=dir, alldirs=TRUE)))

  # append suffix to projName
  if (appendImgSave && !grepl("ImageSave", projName) && !grepl("B4Fresh", projName))
     projName <- paste(projName, "ImageSave", sep=ifelse(nchar(projName), "_", "")) 

  # try to load. If failed, search again in all dirs before giving up. 
  # second try denoted by alldirs=TRUE
  loaded <- try(LoadFromBackUp(projName, dir, pos=pos+2, imgload=TRUE)) # extra pos for "try(.)"
  if (inherits(loaded, "try-error")) {
    if (!alldirs)
      return(loadImageOf(projName, appendImgSave, stamp, dir, alldirs=TRUE, .Pfm, nbs, pos=pos+1))
    stop(loaded)
  }

  if(exists(".libs"))  {
    pkgsLoaded <- setNames(sapply(.libs, function(x) eval(parse(text=paste0("require(", x,")")))), .libs)
    if (verbose)
      {  cat("Packages Loaded:\n");  print(pkgsLoaded)  }
  }

  if (verbose) {
    cat("\n\nSome Environment Objects:\n")
  }

  return(suppressWarnings(lsos(n=12)))
}


imagesInDir <- function(dir="~/gitData/ImageSaves", NBS=FALSE, alldirs=FALSE, .Pfm=Sys.info()[['sysname']], hyphen="_-", nbs=NBS) { 
  # hyphen for splitting off time stamp
  h <- paste0("(", paste(unlist(strsplit(hyphen, "")), collapse="|"), ")")

  if(alldirs) {
    dir <- unique(c(dir, "~/NBS-R/Ricks/ImageSaves", "~/gitData/nbs/ImageSaves", "~/gitData/ImageSaves"))

  # if NBS directory
  } else if (nbs & missing(dir)) {
      dir <- ifelse (.Pfm == "Linux",  "~/NBS-R/Ricks/ImageSaves", "~/gitData/nbs/ImageSaves")
  }

  # get list of files
  files.nms <- list.files(dir, recursive=TRUE)
  files.fullpaths <- list.files(dir, recursive=TRUE, full.names=TRUE)
  names(files.fullpaths) <- files.nms

  # remove extension
  splat <- sub("\\.Rda(ta)?$", "", files.nms, ignore.case=TRUE)

  splat <- strsplit(splat, h)

  # find Date Stamp by searching for 201X
  dateStampStart <- lapply(splat, function(x) max(0, (which(grepl("201[0-9]", x)) - 1)) )

  # in case not found, use the whole length
  dateStampStart <- ifelse(dateStampStart < 1, lapply(splat, length), dateStampStart)

  # return the pasted string, minus anything after the date stamp
  ret <- mapply(function(S, d) paste(S[1:d], collapse=substr(hyphen,1,1)), splat, dateStampStart)

  # return
  unique(ret)
}


cleanImageFiles <- function(projName, appendImgSave=TRUE, stamp=TRUE, dir="~/gitData/ImageSaves", .Pfm=Sys.info()[['sysname']], verbose=TRUE, doNotPrompt=FALSE, pos=1) {  
# This function useful after having run `saveImageTo()` multiple times.
# This cleans up the `dir` by finding all files of pattern `projName`, evaluating the time stamp, 
#    and preserving the most recent one while deleting all the previous ones. 
# 
# Args:
#   Verbose:   TODO  (not yet implemented)
#   doNotPrompt:  IF TRUE, function will not ask for user confirmation, instead will assume user input of "yes"   
#  
# Returns the list of files with pattern that remain in dir at end of the process. 
#  That is, at a minimum, if all succesfull, returns a list of 1 file, the one preserved. 
#  If user selected NO at prompt, then returns a list of all the files in dir that matched the pattern 
#  If user selected YES, but there were errors in deleting the files, then returns a list of the 
#     file deliberately not-deleted along with all the other files that matched the pattern that were unsucessfully deleted

    # TODO: Move to trash instead of deleting


  if(missing(projName)) {
    projName <- getProjName(appendImgSave, pos=pos+1)
    NotMissing <- FALSE
  } else
   NotMissing <- TRUE

  # append suffix to projName
  if (appendImgSave && !grepl("ImageSave", projName))
    projName <- paste(projName, "ImageSave", sep=ifelse(nchar(projName), "_", "")) 

  # search for "NBS" in  either `getwd` and `dataDir` for "NBS"
  # if found, switch the folder
  if (missing(dir) && any(grepl("NBS", c(getwd(), if(exists("dataDir"))dataDir),  ignore.case=TRUE)))
    dir <- ifelse (.Pfm == "Linux",  "~/NBS-R/Ricks/ImageSaves", "~/gitData/nbs/ImageSaves")

  # get the file names with time stamps extracted
  files <- LoadFromBackUp(objToLoad=projName, BackUpDir=dir, pos=2, returnTimeStamps=TRUE)
  # sort them by time stamp, then grab just the names
  files <-sort(files, decreasing=TRUE)

  toDelete <- tail(names(files), -1)
  toDeleteDates <- tail(files, -1)

  toKeep   <- setdiff(names(files), toDelete)
  toKeepDates   <- setdiff(files, toDeleteDates)

  nfiles   <- length(toDelete)  # number of files to delete
  remain   <- rep(TRUE, length(toDelete))  # flag for which files remain

  cat("Deleting:\n--------", rev(paste(toDelete, toDeleteDates, sep="\t")), "", sep="\n\t")
  cat("Keeping:\n-------", paste(toKeep, toKeepDates, sep="\t"), "", sep="\n\t")


  # get user confirmation
  if (isTRUE(doNotPrompt)) {
    proceed <- "y"
  } else {
    cat("Proceed?\n")
    proceed <- readline(paste("Delete the", nfiles, "files above? [y/n]: > "))
    cat("\n")  # add some blank lines, that is all. 
  }

  # if user indicates to proceed
  if (tolower(substr(proceed, 1, 1))=="y"){

    # deleted has value 0 if no failures, 1 if a failure
    deleted <- try(unlink(as.path(dir,toDelete)), silent=TRUE)    
    
    # check if the files exists. all of `remain` should be FALSE
    remain <- file.exists(as.path(dir,toDelete))
    
    # check for erros
    if (any(deleted, remain)) {
      countsMsg <- paste0("Attempted to delete ", nfiles, " files. ", sum(remain), " file remain", ifelse(sum(remain)>1,".", "s."))
      if (deleted) {
        warning("Delete attempt returned failure code: `",deleted,"`\nFurther Details: ", countsMsg)
      } else warning(countsMsg)
#      return(c(toKeep, toDelete[remain]))
    
    # if no errors: 
    } else {
      countsMsg <- paste0(nfiles, " files have been deleted succesfully.\n\n")
      cat("\n",countsMsg, "\n")
    }
#    return(toKeep)
    
  }
  else cat("\nNothing was deleted.\n\n")
#  return(files)

  return(c(toKeep, toDelete[remain]))
}




##   TODO:  USAGE EXAMPLES NEED TO BE UPDATED
#========================================================================================#
# USAGE EXAMPLE 
#========================================================================================#
  ## Determine which objects to load
   #  obj <- c("CAconcerts.rec", "CAconcerts_by_artist")

  ## If using `sapply`, use `pos=3` and set USE.NAMES to FALSE (so that toRemove keeps proper names)
   #   toRemove <- sapply(obj, loadFromNBSData, pos=3, USE.NAMES=FALSE)
   #   # -- OR -- #
   #  toRemove <- sapply(obj, checkAndLoad, BackUpFrom="path/to/backups", pos=3, USE.NAMES=FALSE)

  ## toRemove is then flags for which objects to remove
   #  names(toRemove)[toRemove]  <~~ The objects to remove
   #  if (cleanUpAfterOurselves)
   #    rm(list = names(toRemove)[toRemove])
#========================================================================================#




LoadFromBackUp <- function(objToLoad, BackUpDir="~/NBS-R/Ricks/data/", CheckSubs=TRUE, DateFormat="%Y%m%d_%H%M", returnTimeStamps=FALSE, pos=1, verbose=TRUE, hyphen="[-_]", imgload=FALSE, showPath=imgload)  {
## TODO:  allow for both `.Rda` and `.RData` -- or better yet, strip the file extension 

# args: hyphen:  possible hyphen arguments. Will be shortened to h

  h <- hyphen

  # intialize toLoad.  Later will check if it already has a value
  toLoad <- ""

  # ---------------------------------------------- #
  # ----    IDENTIFY THE FILES IN THE PATH   ----- #
  # ---------------------------------------------- #
    # identify file names, no path
    filesAndPath <- dir(BackUpDir, recursive=CheckSubs, include.dirs=FALSE, full.names=TRUE)

    # grab only the file name, and not the leading path
    files <- sapply(strsplit(filesAndPath, .Platform$file.sep), tail, 1)

    # grab only those paths with objToLoad
    filesUsing <- grepl(paste0("^",objToLoad, h), files)

    # crop out the unused files from our list
    files <- files[filesUsing]
    filesAndPath <- filesAndPath[filesUsing]

    # Error if no files found. 
    if (all(!filesUsing))
      stop("\n No files starting with, ", objToLoad, " in \n\t", paste(as.character(BackUpDir), collapse="\n\t") , "\n",
          ifelse(CheckSubs, paste0("or any of ", ifelse(length(BackUpDir)>1,"their", "its"), " sub directories.\n"), "Try using ` CheckSubs=TRUE `.\n") )      
  # ---------------------------------------------- #


  # ------------------------------------------------- #
  # ---   PARSE OUT THE DATE FROM THE FILE NAME   --- #
  # ------------------------------------------------- #
    nc <- nchar(files)


    # check if seconds is in the file name time stamp
    #   since the seconds will take up an additional two characters (19 instead of 17)
    sc <- ifelse(grepl(h, substr(files, nc-19, nc-19)), 2, 0)   
    #TODO:  This will be thrown off if the file has seoncds stamp AND has extension '.RData' instead of '.Rda' 
    #       However, by chance (ie 2 characters), this works if the file has no seconds, but uses `.Rdata` 

    #---------------  ERROR CHECK ----------------- - - - - - 
      # 17 + sc should be a hypon, ie, just before the time stamp
      #  culp identifies which 
      if(all(culp <- !(grepl(h, substr(files, nc-(17+sc), nc-(17+sc)))), na.rm=TRUE)) {

        culp[is.na(culp)] <- FALSE

        # if there is only one candidate file, then date stamp doesnt matter
        if (length(filesAndPath)==1) {
          toLoad <- filesAndPath[1]
        
        # if only some are missing dates, then remove those from list
        } else if (!all(culp)) {
          warning("\nSome files do not have identifiable dates: \n", paste(ifelse(culp, " ==> ", "     "), files, collapse="\n"), "\n\nThose files will not be considered.")

        # otherise 
        } else
          stop("\nMultiple files found, but none have identifiable date. Please be more specific with directory.\n\n", paste("  ", filesAndPath, collapse="\n\n"), "\n")
      }
    #---------------  ERROR CHECK ----------------- - - - - - 


    # grab time stamp, without the second columns
    dates <- substr(files, nchar(files)-(16+sc), nchar(files)-(4+sc))
    dates <- as.POSIXlt(dates, format=DateFormat)
  # ---------------------------------------------- #


  # if flagged just to return time stamps of the files: 
  if (returnTimeStamps)
    return(setNames(dates, files))


  # helper.  List all files
  if(objToLoad=="?")  {
    print(cbind(imagesInDir(dir=BackUpDir, alldirs=TRUE)))
    return(cbind(imagesInDir(dir=BackUpDir, alldirs=TRUE)))
  }

  # IDENTIFY THE FILE with the most recent time stamp
  #   the ifelse is for the error check in the section with `culp`
  #   ie, if no dates found, but only one file identified, just use that one. 
  dateLoading <- max(dates, na.rm=TRUE)
  toLoad      <- ifelse(toLoad=="", filesAndPath[which(dates==dateLoading)], toLoad)

  # output the path information if flagged
  filePath <- sapply(lapply(strsplit(toLoad, "/"), head, -1), paste, collapse="/")
  if(showPath)
    cat("\n\nLoading Backup From: ",  paste(filePath, as.character(dateLoading), sep="   "), sep="\n\t" )

  # LOAD: 
  caught <- try(load(toLoad, envir=parent.frame(pos)))

  if(inherits(caught, "try-error"))
    stop("\n\nSomething went wrong, could not load the file")
  else {
    # re-load the utils, since they likely have changed since the image was last saved. 
    if (.Pfm=="Linux") {
          try(source(path.expand("~/NBS-R/Ricks/src/utils/workspace.R")), silent=TRUE)
          try(source(path.expand("~/NBS-R/Ricks/src/utilsRS.r")), silent=TRUE)
    } else {
          try(source(path.expand("~/git/misc/rscripts/utils/workspace.R")), silent=TRUE)
          try(source(path.expand("~/git/misc/rscripts/utilsRS.r")), silent=TRUE)
    }
  }

  # check if object was loaded, but only if we were not loading a backup image
  if (!exists(objToLoad) & !imgload)
    stop("\n\nSomething went wrong. File loaded but cannot find `", objToLoad, "`\n")
  
  # OTHERWISE, ALL IS WELL
  if (verbose) cat("Object `", objToLoad, "` Loaded Succesfully.\n", sep="")
  return(TRUE)
}


# example: 
#  LoadFromBackUp(objToLoad="CAconcerts.rec", BackUpDir="~/NBS-R/Ricks/data/")

checkAndLoad <- function(objToLoad, BackUpFrom, pos=1, force=FALSE) {

  # Initialize
  wasLoadedIn  <- FALSE

  # Check if object is already present.  If so, do nothing
  if(!exists(objToLoad) || force)  {

    # flag to indicate object was loaded in the call to this function (ie, it was not previously present)
    wasLoadedIn <- TRUE 

    # Load the object
    LoadFromBackUp(objToLoad=objToLoad, BackUpDir=BackUpFrom, verbose=FALSE, pos=pos+1)

    # there is an error check in LoadFromBackUp, this is just an extra double check
    stopifnot(exists(objToLoad))
  }

  # retuns whether the specific object was loaded
  return(setNames(wasLoadedIn,  objToLoad))
}

LoadFromNBSData <- function(objToLoad, pos=1, .Pfm=Sys.info()[['sysname']], force=FALSE) { 
# Wrapper function with the NBS data dir pre populated

   # set Dir to load backups from based on which system working from  
    BackUpFrom <- "~/NBS-R/Ricks/data/"
    BackUpFrom <- ifelse(.Pfm=="Linux", BackUpFrom, "/Users/ricardosaporta/gitData/nbs/data")
    return(checkAndLoad(objToLoad, BackUpFrom, pos=pos+1, force=force))
}

NBSUtils <- function()
  try(source(path.expand("~/NBS-R/utils/utils.r")))


## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% ## 
## ------------------------------------------------------------ ##
## THESE WERE MY ORIGINAL SCRIPTS, AND HAVE BEEN REPLACED WITH  ##
##    saveImageTo() and loadImageOf()                           ##
## ------------------------------------------------------------ ##
##   # save and load default
##   savdef <- savedef <- function(env=parent.frame())
##     eval(save.image(file="~/.default.RData"), envir=env)
## 
##   loadef <- function()
##     load(file="~/.default.RData", envir=.GlobalEnv)
## 
##   # save and load images specifc to NBS
##   savenbs <- function()
##     eval(save.image(file="~/gitData/nbs/.NBS_image.RData"), envir=.GlobalEnv)
## 
##   loadnbs <- function()  {
##     load(file="~/gitData/nbs/.NBS_image.RData", envir=.GlobalEnv)
##     setwd("~/git/nbs")
##   }
## ------------------------------------------------------------ ##
## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% ## 



findBackUps <- function(objName="", dir="", allDirs=TRUE, ext=c("Rda", "Rdata"), noImg=TRUE, hyphens=c("-", "_")) { 

  dir.all <-   c(getwd(), dataDir, outDir)
  
  if (allDirs)
     dir <- unique(setdiff(c(dir, dir.all), ""))

  files <- list.files(dir, recursive=TRUE, full.name=TRUE)

  # find only those extensions
  ## remove dot from extension
  ext <- gsub("\\.", "", ext)

  files <- sapply(ext, function(ex) 
      grep(paste0(ex,"$"), files, ignore.case=TRUE, value=TRUE))
  files <- unname(unlist(files))

  dates <- extractTimeStamp(files, hyphens=hyphens)
  fileNames <- sapply(strsplit(files, .Platform$file.sep), tail, 1)
  fileNames <- gsub(regOr(paste0("\\.", ext)), "", fileNames)
  h  <- regOr(h.splat)
  fileNames <- mapply(function(f, d) gsub(paste0(h, d), "", f), fileNames, dates, USE.NAMES=FALSE)
  include <- TRUE
  if (noImg)
    include <- include & !grepl("ImageSave", fileNames)

  paths <- mapply(function(p, f) gsub(paste0(f, "\\.*$"), "", p), files, fileNames, USE.NAMES=FALSE)

  ret <- unique.data.frame(data.table(fileNames, dates, paths))

  ret[!exclude, list(fileNames, dates)][order(tolower(fileNames), dates)]

}


extractTimeStamp <- function(x, hyphens=c("-", "_"), alphaHyphen=FALSE) { 
# TODO: alphahyphen

  h.splat <- unlist(strsplit(hyphens, ""))

  # h is just hyphens, hb is hyphens or word block
  hb <- regOr(c(h.splat, "\\b"))
  h  <- regOr(h.splat)

  dateFormat <- c(sec="%Y%m%d_%H%M%S", min="%Y%m%d_%H%M")
  datePattern <- paste0(hb,"[0-9]{8}", h, "[0-9]{4}([0-9]{2})?", hb)
  
  dates <- stringr::str_extract(x, datePattern)
  dates <- gsub(paste0("(^",h,"|",h,"$)"), "", dates)

  return(dates)

  # TODO:  no need to actually parse for now
  dates <- ifelse(nchar(dates)==15
          , as.Pos(dates, format=dateFormat["sec"])
          , as.Pos(dates, format=dateFormat["min"]))
}

regOr <- function(vec, brackets=TRUE) { 
  ret <- paste(c(vec), collapse="|")
  if (!brackets)
    return(ret)

  # else return
  paste0("(", ret, ")", collapse="")
}



