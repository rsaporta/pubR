#=====================================================================#
#---------------------------------------------------------------------#
#              MAIN FUNCTION:   selfAssign                            #
#---------------------------------------------------------------------#


selfAssign <- function(self, ind, val, n=1, silent=FALSE) {
## assigns val to self[ind] in environment parent.frame(n)
## self should be a vector.  Currently will not work for matricies or data frames

  ## GRAB THE CORRECT MATCH CALL
  #--------------------------------------
      # if nested function, match.call appropriately
      if (class(match.call()) == "call") {
        mc <- (match.call(call=sys.call(sys.parent(1))))
      } else {
        mc <- match.call()
      }

      # needed in case self is complex (ie df$name)
      mc2 <- paste(as.expression(mc[[2]]))


  ## CLEAN UP ARGUMENT VALUES
  #--------------------------------------
      # replace logical indecies with numeric indecies
      if (is.logical(ind))
        ind <- which(ind) 

      # if no indecies will be selected, stop here
      if(identical(ind, integer(0)) || is.null(ind)) {
        if(!silent) warning("No indecies selected")
        return(FALSE)
      }

      # if val is a string, we need to wrap it in quotes
      if (is.character(val))
        val <- paste('"', val, '"', sep="")

      # val cannot directly be NULL, must be list(NULL)
      if(is.null(val))
        val <- "list(NULL)"


  ## CREATE EXPRESSIONS AND EVAL THEM
  #--------------------------------------
     # create expressions to evaluate
     ret <- paste0("'[['(", mc2, ", ", ind, ") <- ", val)
     
     # evaluate in parent.frame(n)
     eval(parse(text=ret), envir=parent.frame(n))

     return(TRUE)
}


#=====================================================================#
#---------------------------------------------------------------------#
#                                                                     #
#             WRAPPER FUNCTIONS:                                      #
#                                                                     #
#                   NAtoNULL                                          #
#                   NAto0                                             #
#                   NAtoVal                                           #
#                   ZtoNA                                             #
#                   selfReplace                                       #
#                                                                     #
#---------------------------------------------------------------------#


NAtoNULL <- function(obj, n=1, safetyBreak=1) {
# replace NA's with NULL
  selfAssign(match.call()[[2]], is.na(obj), NULL, n=n+1)
}

NAto0 <- function(obj, n=1) {
# replace NA's with 0
  selfAssign(match.call()[[2]], is.na(obj), 0, n=n+1)
}

NAtoVal <- function(obj, val, n=1) {
  selfAssign(match.call()[[2]], is.na(obj), val, n=n+1)  
}

ZtoNA <- function(obj, n=1) {
# replace 0's with NA

  # TODO: this may have to be modified if obj is matrix
  ind <- obj == 0
  selfAssign(match.call()[[2]], ind, NA, n=n+1)
}

#=====================================================================#
#---------------------------------------------------------------------#

selfReplace <- function(obj, toReplace, val, n=1) {
## replaces occurrences of toReplace within obj with val
##  Returns: FALSE if toReplace not found in obj
##           TRUE  if replacements found and made succesfully
##           NULL  if replacening not fully succesful (warning also issued)
  
  #-------------------------------------------------------#
  #    IF OBJ IS DATA FRAME, RECURSE OVER EACH COLUMN     # 
  #                                                       #
  #     However, currently, `sapply` does not work with   #
  #        selfAssign, hence we return an error           #
  #-------------------------------------------------------#
  if (is.data.frame(obj))
   #  return(sapply(obj, selfReplace, toReplace=toReplace, val=val, n=n+1))
   stop("\n",rep("=", 50), "\n\tselfAssign and selfReplace do not\n\tcurrently work with whole data frames.\n\n\tPlease manually apply to each column\n",rep("=", 50))


  #-------------------------------------------------------#
  #    DETERMINE INDECIES TO OBJ THAT WILL BE REPLACED    # 
  #-------------------------------------------------------#

  # determine ind based on value & length of toReplace
  if (is.null(toReplace)) {
    ind <- sapply(obj, function(x) is.null(x[[1]]))
  }  else if (is.na(toReplace)) {
    ind <- is.na(obj)
  } else  {
    if (length(obj) > 1) {    # note, this wont work for data frames
          ind <- obj %in% toReplace
    } else {
      ind <- obj == toReplace
    }
  } 

  ## CHECK IF ANY REPLACEMENTS TO BE MADE
  # If not, do not continue, just return FALSE
  if (!any(ind))
    return(FALSE)

  # Othewise, make replacements
  selfAssign(match.call()[[2]], ind, val, n=n+1)

  #-------------------------------------------------------#
  #  ERROR CHECK: we will confirm that changes were made  # 
  #               and all instances of toReplace removed  #
  #-------------------------------------------------------#

  # TODO:  fix this
  # Problem:  if the name of obj in the call is also obj, the test below will
  #           produce inacurrte results.  Therefore, return NA for now. 
  #           (Note: the self-assigning should still have worked)
  if (match.call()[[2]] == "obj")
    return(NA)

  #--- TESTERS ---#
      # grab the new value of obj
      objPost <- eval(match.call()[[2]])

      # These should both be true
      changesMade <- !all(objPost %in% obj)  # some element should be different
      allGone <- !(any(objPost %in% toReplace)) # no toReplace values should remain

  #--- POSITIVE RESULTS ---# 
    if (changesMade && allGone)
      return(TRUE)

  #--- NEGATIVE RESULTS ---# 
    # Something went wrong. Issue warning and return NULL. 
    # Note: We should never reach this point. 
    #       Receiving a NULL is probably indicative of a bug in the code
    if (!allGone)
      warning("elements toReplace still remain")

    if (!changesMade)
      warning("No changes were made")
    
    return(FALSE)
}

#---------------------------------------------------------------------#
#                     END: selfReplace                                #
#---------------------------------------------------------------------#
#=====================================================================#









