getting match.call() to work with *apply & plyr? (re: Referencing a dataframe recursively)

The purpose of the following functions are to allow for self-referencing assignments more easily. (As suggested here: [Referencing a dataframe recursively](http://stackoverflow.com/questions/13615385/referencing-a-dataframe-recursively/13620608) )

So that instead of 

     # this  
     myDataFrame$variable[is.na(myDataFrame$variable)] <- 0

     # we can have this: 
     NAto0(myDataFrame$variable)


The functions work well for vectors, but less so when *ply'ing 

I'm encountering two issues with regards to the `match.call()` portion of the function `selfAssign()`. 
(1)  How can I determine from within a function if it was called from a *apply-type function?
(2)  How can I trace back up the call to the correct variable environment? 

I've included the argument `n` to `selfAssign(.)` which works well for the `eval` statement at the end. I'm wondering if I could somehow make use of `n` something akin to 

     sapply(df, NAto0, n=2)

and perhaps in selfAssign have something like `sys.parent(n)` (which I tried, and either I did not get it right, or it does not work)

Any suggestions would greatly appreciated. 


### FUNCTIONS

These functions are wrappers to selfAssign and are the ones that would be used in the `*apply` calls. 

    NAtoNULL <- function(obj, n=1) {
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

`selfAssign` is the function performing the work and where the error is coming from 
 
    selfAssign <- function(self, ind, val, n=1, silent=FALSE) {
    ## assigns val to self[ind] in environment parent.frame(n)
    ## self should be a vector.  Currently will not work for matricies or data frames

      ## GRAB THE CORRECT MATCH CALL
      #--------------------------------------
          # if nested function, match.call appropriately
          if (class(match.call()) == "call") {
            mc <- (match.call(call=sys.call(sys.parent(1))))   ## THIS LINE PROBABLY NEEDS MODIFICATION
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
            return()
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
    }
