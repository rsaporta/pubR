
  changeLevels <- function(dtable, column, newlevs, recycling=FALSE) {
    
    # get name of table
    dtName <- as.character(match.call()[[2]])

    # We are getting the table from the calling environment,  get(dtName, envir=parent.frame(1))
    #   and making changes to [[column]]

    # TODO:  Implement generic function
    # First, check to make sure it's a dt. 
    dtClass <- class(get(dtName, envir=parent.frame(1)))
    if (!"data.table" %in% dtClass) {
      stop ("dtable is not a data.table.  Please use `levels<-` for data.frame")
    }

    # Check for appropriate length on new levels
    oldLevsLength <- length(levels(get(dtName, envir=parent.frame(1))[[column]]))
    newLevsLength <- length(newlevs)

    diffs <- newLevsLength - oldLevsLength
    if (diffs > 0) {
      stop ("New levels has ", diffs, " level", ifelse(diffs==1, "", "s"), " too many." )
    } else if (diffs < 0) {
      stop ("New levels has ", -diffs, " level", ifelse(diffs==-1, "", "s"), " too few." )
    } else if (diffs != 0) {  # just in case
      stop ("Something went wrong. Unsure what.")
    } else {

      return(setattr( get(dtName, envir=parent.frame(1))[[column]], 
              "levels",newlevs))
    }
  }


 
  ##-----------------------------  EXAMPLE  -----------------------------###
  #   library(data.table)
  #   mydt <- data.table(id=1:6, value=as.factor(c("A", "A", "B", "B", "B", "C")), test=c(2, 2, 3, 4, 5, 6), key="id") 
  #
  #   newLevs.good    <- c("X", "Y", "Z")
  #   newLevs.tooFew  <- c("P", "Q")
  #   newLevs.tooMany <- c("R", "S", "T", "U", "W")
  #   originalLevs    <- c("A", "B", "C") 
  # 
  #
  #   changeLevels(mydt, "value", newLevs.good);  mydt
  #   changeLevels(mydt, "value", originalLevs);  mydt
  #   changeLevels(mydt, "value", newLevs.tooFew);  mydt
  #   changeLevels(mydt, "value", newLevs.tooMany);  mydt 
  ##-------------------------------------------------------------------###
