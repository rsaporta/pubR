# Used to transfer all installed packages from one system to another.  
#  Note:  Systems must have similar OS's.   eg: cannot go from OS X to Ubuntu 


# These two functions are implementations of @DWin's suggestions on StackOverflow
#  http://stackoverflow.com/questions/7133394/migrating-r-libraries

txrL.Out <- transferLibrary.Out <- function(dir="~/git/misc/!SysAdmin", fileName="pkglist.Rdata", createIfMissing=TRUE, dx=TRUE) {
# depends on as.path()
#
# Arguments:  dx: use dropbox dir
  
  if (dx)
    dir <- "~/Dropbox/tmp" 

  dir <- path.expand(dir)

  # Check that dir exists
  if (createIfMissing) {    
    dir.create(dir, showWarnings=FALSE)
  }  else {
    if (!file.exists(dir))
      stop(dir, " NOT FOUND")
  }
 
  # in installed.packages(), the Priority column alludes to which packages come installed. 
  # Therefore, we want those with no specified priority. 
  # .... I"m not positive that this is true.  
  # We might be better off excluding priorities c("base", "recommended") 
  #   as I'm not sure if there are other ways for a package to have a different priority


  # Note: the name of object `save.pkg.list` must match with that of `transferLibrary.In`
  save.pkg.list <- installed.packages()[is.na(installed.packages()[ , "Priority"]), 1]
  save(save.pkg.list, file=as.path(dir, fileName))
}


txrL.In <-  transferLibrary.In <- function(dir="~/git/misc/!SysAdmin", fileName="pkglist.Rdata", dep=FALSE, update=TRUE, dx=TRUE) {
# depends on as.path()
# 
# Arguments:   dep:  If TRUE will also install dependencies. 
#           update:  If TRUE will run update.packages() after installation completes.
#               dx:  If TRUE will over-right the value in dir with a dropbox dir.

  if (dx)
    dir <- "~/Dropbox/tmp" 

  file <- as.path(dir, fileName)

  # Check that file exists
  if (!file.exists(file))
      stop(dir, " NOT FOUND")
  
  # load in the list of packages
  load(as.path(dir, "pkglist.Rdata"))

  # Install the packages
  # Note: the name of object `save.pkg.list` must match with that of `transferLibrary.Out`  
  install.packages(save.pkg.list, dependencies=dep)

  # update all packages, if flagged (TRUE by default)
  if (update)
    update.packages(checkBuilt=TRUE)
}
