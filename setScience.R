
 ## FUNCTIONS IN THIS FILE

 #  setSceince ( ..., pos=1, mkdir=TRUE ) 
 #  newProject ( ..., wrkDir="~/git", pos=1, mkdir=TRUE ) 
 #  getProjectsFromFolders ( folder, recursive=FALSE ) 
 #  clearDos ( pos=1 ) 
 #  dosDir ( wrkDir , gitData=FALSE, mkdir=FALSE, pos=1, silent=FALSE, projName="", forceSubProject=FALSE ) 

 # These functions are a compliment to the "workspace.R" functions, but could be justified in their own file (for now)


    # -------------------------------------------------------------------------------------------------------- #

setSceince <- function(wrkDir, pos=1, mkdir=TRUE) { 

  if (missing(wrkDir))
    wrkDir   <- assign("wrkDir", ifelse(.Pfm=="Linux", "~/NBS-R/Ricks", "~/git/nbs"), envir=parent.frame(pos))

  newProject(wrkDir=wrkDir, pos=pos+1, mkdir=mkdir)

}


    # -------------------------------------------------------------------------------------------------------- #


newProject <- function(wrkDir="~/git", pos=1, mkdir=TRUE) {
# Note to self, setup is as follows: 
#    * wrkDir is my "root". ie   ~/git/nbs/ |  ~/NBS-R/Ricks/
# Within  wrkDir I have:   src, out, data
# Within each of those, I have folders for each Project.  
# thus: 
#        wrkDir/src/projName
#        wrkDir/out/projName
#        wrkDir/data/projName


  maxTries <- 3
  cls()
  assign(".Pfm", Sys.info()[['sysname']], envir=.GlobalEnv)

  # THIS FUNCTION SERVES TO ALLOW FOR INPUT ENDING IN "S", INDICATING TO GO INTO SUBFOLDERS
  # ------------------------------------------------------------------------------------- #
  SUBFOLDER <- FALSE; en<-environment()
  checkS <- function(sel) { 
    if (!grepl("S$", sel, ignore.case=TRUE))
      return(sel)
    #else
    assign("SUBFOLDER", TRUE, envir=en)
    return(as.numeric(substr(sel, 1, nchar(sel)-1)))
  }
  # ------------------------------------------------------------------------------------- #


  # GET LISTS OF PROJECT NAMES, ie LIST OF FOLDER NAMES AND CREATE  projectslist
  assign("Projects",   getProjectsFromFolders(wrkDir), envir=parent.frame(pos))
  projectslist <- data.table(OPT=seq_along(Projects[[1]]), as.data.table(Projects), key="OPT")


  # ------------------------------------------------------- #
  #                      SELECTION                          #
  # ------------------------------------------------------- #

  # check if projName exists. Output an introduction sentance 
  if (exists("projName")) {
    projectslist <- rbind(c(0, projName, wrkDir), projectslist)
    cat("\nCurrent projName is\n\n\t", projName, "\n\nOther project folders available in\n  wrkDir =  ",wrkDir, ": \n\t")
  } else {
    cat("\nNo projName currently set. Projects available are: \n\t")
  }

  # add option to create a new project
  projectslist <- rbind(projectslist, list(99, "[CREATE NEW PROJECT]", projectslist[, names(which.max(table(FOLDERS)))]))

  # OFFER USER OPTIONS TO SELECT
  cat("", apply(projectslist[, list(OPT, PROJECTS) ], 1, paste, collapse=":\t"), sep="\n\t")
  tries <- 1
  selection <- checkS(readline(paste0("Please select a project [ ",paste(projectslist[,range(OPT)], collapse=":")," ] >") ))

  # check for valid input. 
  while (!selection %in% projectslist$OPT && tries <= maxTries && !"x" == tolower(selection)){
    cat("\nInvalid selection")
   selection <- checkS(readline(paste0("Please select a project in the range of (",paste(projectslist[,range(OPT)], collapse=" to "),"). >") ))
   tries <- tries + 1
  }

  if (SUBFOLDER) {  
    if (selection %in% c(0, 99))
      stop("Can't use subfolder selection with 0 or 99")
    wrkDir <- as.path(wrkDir, projectslist[OPT==selection, PROJECTS])
    return(newProject(wrkDir=wrkDir, pos=pos+1, mkdir=mkdir))
  }

  ## If user selected "x" or exceeded number of attempts, then quit. 
  if("x" == tolower(selection) || tries==maxTries){
    cat("\n\nExiting without assigning `projName` or dosDir variables.\n\n")
    return(invisible(NULL))
  }

  # If selection was invalid, even after attempts, then quit and do nothing more: 
  if(!selection %in% projectslist$OPT)  {
      warning("\nNo valid project selected. Defaulting to <none>.")
      suppressWarnings(rm("projName", envir=parent.frame(pos)))
      return(invisible(NULL))  # creates dataDir
  } 

  # else: Selection WAS valid   

    ## CREATE VARIABLES
  # if user selected to create a new project: 
  if (selection==99) {
     selection2 <- readline("Please input a name for the new project. > ")
     if("x" == tolower(selection2)) {
        cat("\n\nExiting without creating new project, assigning `projName` or dosDir variables.\n\n")
        return(invisible(NULL))
     }
     projectslist[OPT==99, PROJECTS := selection2 ]
     cat("\n\nNew Project will be created in folder: \n\n\t", wrkDir, "\n\n")
  }


  # Assign the projName variable, and create the folder variables
  projName <- projectslist[OPT==selection, PROJECTS]
  assign("projName", projName, envir=parent.frame(pos))
  

  dosDir(wrkDir, pos=pos+1, gitData=(.Pfm=="Darwin"), mkdir=mkdir, silent=TRUE, projName=projName)

  # Check if there are viable images to load from back up
  if(!any(grepl(paste0("^", projName), loadImageOf("?")))) { 
    cat("\n\nNo back Images exist yet for this project.\nDon't forget: Jesus Saves!\n\n")
  } else {
    selection <- readline(paste0("\nLoad Image for { ", projName, " } ?  [y / N] >") )
    
    # if chosen yes, then load
    if ("y" == tolower(substr(selection,1,1)))
          loadImageOf(projName, verbose=FALSE, pos=pos+1)
  }

    # set working directory, data directorie variables, etc
  assign("wrkDir", wrkDir, envir=parent.frame(pos))
  setwd(wrkDir)

  cat("\n\t", pasteR("-", 20), "\n\n")
  cat("\n\n\n\t\tGo Science!!\n\n")
}


    # -------------------------------------------------------------------------------------------------------- #


getProjectsFromFolders <- function(folder, recursive=FALSE)  {
# GET LISTS OF PROJECT NAMES, IE LIST OF FOLDER NAMES MINUS ANY FOLDERS EXPLICITY NOTED AS NOT BEING A PROJECT
  
  # A running list of folders that are to be ignored as projects
  notProjects <- c("sql", "supportFns", "dicts", "-OLD?-From science", "-OLD-From science", "Zarchive", 
                    "StackExchange","misc", ".git", "data.table vs data.frame" )
  # these are prohects, but for now, no need to see them
  tmpRemove <- c("198_535_Pattern", "960_586_Inter_Data_I", "960_563_Regression")

  `%ni` <- function() negate(`%in%`)

  folders   <- list.dirs(folder, full.names = TRUE, recursive=recursive)
  projects  <- sapply(strsplit(folders, "/"), tail, 1)

  # keep only the base folder name
  folders <- unname(unlist(mapply(strsplit, folders, paste0(projects,"$"))))


  # if "src" is one of the folders, then get the projects from within that folder
  if (any(grepl("^src$", projects)) && !recursive)
    return(getProjectsFromFolders( as.path(folder, "/src")  ))
  # else: 

  indx <- projects %ni% c(notProjects, tmpRemove)
  ## Porjects = folders - notProjects.   (assigned to the parent environment)
  return(list(PROJECTS=projects[indx], FOLDERS=folders[indx]))
}



    # -------------------------------------------------------------------------------------------------------- #


clearDos <- function(pos=1) { 
  assign("wrkDir", "~/git", envir=parent.frame(pos))
  suppressWarnings(rm(list=c("gitdataDir", "dataDir", "srcDir", "outDir"), envir=parent.frame(pos)))
  setwd("~/git")
}

    # -------------------------------------------------------------------------------------------------------- #


dosDir <- function(wrkDir , gitData=FALSE, mkdir=FALSE, pos=1, silent=FALSE, projName="", forceSubProject=FALSE) {
  # makes data, out, src directory inside the directory wrkDir
  #   and creates variables with full path to these directories
  #   in the parent environment  (the environment that called this func) 
  # 
              # Note to self, setup is as follows: 
              #    * wrkDir is my "root". ie   ~/git/nbs/   ~/git/958-565 Time Series     ~/NBS-R/Ricks/
              #    * projNames are things like   Concerts              HW04                    
              # Within  wrkDir I have:   src, out, data
              # Within each of those, I have folders for each Project.  
              # thus: 
              #        wrkDir/src/projName
              #        wrkDir/out/projName
              #        wrkDir/data/projName

  # error check.  no allowance for windows platforms
  if (gitData && .Platform$file.sep != "/")
    stop("gitData only allowed for non-windows systems")

  # remove current vars, especially gitdataDir, which may not be re-created. 
  suppressWarnings(rm(list=c("gitdataDir", "dataDir", "srcDir", "outDir"), envir=parent.frame(pos)))

  if(missing("wrkDir"))
    wrkDir <- ifelse(exists("wrkDir", envir=parent.frame(pos)), get("wrkDir", envir=parent.frame(pos)), getwd())
  setwd(wrkDir)

  grp <- list("data", "out", "src")

  # create vars (+'Dir') and vals (paths)
  vars <- paste0(grp, "Dir")
  if (nchar(projName)) {
      
      # if the wrkDir has a folder called `src`, then the project goes in there, 
      #  else, the project gets its own set of `src` `out` `data` folders
      # Alternatively, user can falg to `forceSubProject`
      if(file.exists(as.path(wrkDir, "src")) || forceSubProject) {
          grp <- as.path(grp, projName)
          symdata <-  as.path(wrkDir, "data")
      } else 
          grp <- as.path(projName, grp)
          symdata <- as.path(wrkDir, projName, "data")
  }
  vals <- mapply(as.path, wrkDir, grp, MoreArgs=list(expand=FALSE), USE.NAMES=FALSE)



  ##  if gitData is TRUE AND we are making the directory, 
  ##  then we symlink from data to gitdata and the var dataDir points to data
  ##  if we gitdata and we are NOT making the directory, 
  ##  then dataDir simply points to gitdata. (note that if it is symlinked, then same effect) as changing dataDir variable. 
  ## 
  ##  1. check for both
  ##     1. create gitdata
  ##     2. if data exists and is not a symlink, issue warning
  ##     3. if data exists and is symlinked back to git data, then done
  ##  2. if gitdata, but not mkdir
  ##     1. change the value of dataDir to gitdata 
  ##  3. if mkdir but not gitdata
  ##     1. proceed as normal

  # if using gitData, create a symlink
    if (gitData && mkdir) {

      # find which of grp is the data variable
      d.indx <- grepl("(^|/)data", grp)

      # dave the original value, for symlinking
      d.virtual <- vals[d.indx]

      # replace /git/ with /gitData/
      vals[d.indx] <- sub("/git/", "/gitData/", vals[d.indx] )

      # create the dirs, including gitdata, but not data
      sapply(path.expand(vals), dir.create, showWarnings=FALSE, recursive=TRUE)

      # for symlinks, remove any trailing project names
      pat  <- paste0("(/", projName, "/?)$")
      from <- sub(pat, "", vals[d.indx] )
      to   <- sub (pat, "",  d.virtual ) 

      # for symlinks, escape any white spaces
      from <- gsub(" ", "\\\\ ", from )
      to   <- gsub(" ", "\\\\ ", to  )

      # check if plain data directory exists. If not, symlink to it. If so, warning. 
      if(file.exists(d.virtual)) { 
        
##% TODO:  Double check this test. 
##%      # check if file is a symlink, if not, issue warning
##%      if(! system(paste("test -L", to )) ) 
##%        warning("In dosDir(.), `gitData` & `mkdir` are both flagged to TRUE,\nbut regular `data` dir already exists.\nCannot symlink to it.\n")
      } else {
        # create the symlink command
        cmd <- paste("ln -s", from, to ) 

        # try to create a symlnk and issue warning if failed
        caught.symlink <- try(system(cmd), silent=TRUE)
        if(inherits(caught.symlink, "try-error") || caught.symlink==1)
           warning("Attempt to create symlink failed. Attempted symlink was\n  FROM:  ", vals[d.indx], "\n  TO  :  ", d.virtual, "\n\n")   
      }

    }  else if (mkdir) { 
        sapply(path.expand(vals), dir.create, showWarnings=FALSE, recursive=TRUE)  
    } else if (gitData) {
       d.indx <- grepl("(^|/)data", grp)
       d.virtual <- vals[d.indx]  # banking this in case creating dirs.
       vals[d.indx] <- sub("/git/", "/gitData/", vals[d.indx] )
    }

  # assign vals to appropriate var names in the calling environment                  
  output <- mapply(assign, vars, vals, MoreArgs=c(pos=parent.frame(pos)))

  # return the variables with folders they point to in a nice DF
  data.frame(FOLDER=output, stringsAsFactors=FALSE)
}


