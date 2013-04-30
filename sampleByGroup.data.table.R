sampleByGroup <- function(DT, group=key(DT), perc=0.66, replace=FALSE, size=NA, values=FALSE) { 
# Allows for sampling of a data.table by group (or multiple groups), wrapped in a 
#   simple function with additional error checks. 
# See `samplingStats()` for a complimentary summary of the group sampling
#
# Args: 
#     DT :   A data.table whose rows will be sampled by group
#  group :   A list or string of DT column names. Analagous to DT's "by"  # Note to self. Probably should have called this 'by'
#  perc  :   numeric. Percentage of elements to sample per group. Should be beteween (0, 1)
# replace:   Logical. value passed through to `sample(.)`
# size   :   numeric. number of elements to sample per group. If both size and perc are given, only perc is used
# values :   Logical. If FALSE (default) this function returns row indexes to DT. 
#                     If TRUE returns a subset of DT
#                     eg  DT[sampleByGroup(DT, values=FALSE)] == sampleByGroup(DT, values=TRUE)


  #-----------------------------------#
  # check for propper input  (pt 1)
  #-----------------------------------#
    # cannot use both size and perc. 
    if (!missing(size) && !missing(perc) && !is.na(size))
      warning("Only one of `perc` and `size` can be used. Defaulting to `perc`.")

    # "Turn off" perc, if size was set explicitly
    if(!missing(size) && missing(perc))
      perc <- NA

    # perc should be in (0, 1)
    if (!is.na(perc) && !(perc > 0 && perc < 1)) { 
      stop("perc should be between (0, 1) ", ifelse((perc>1 & perc <100), "\nDid you forget to divide by 100?", ""))
    }

    # DT should be key'd or group should be explicit
    if (missing(group) & is.null(key(DT)))
      stop ("no group specified and DT has no key.")

    # parse group, and confirm that they are proper columns of DT
    group <- parseAndCheckGroup(group, DT)
  #-----------------------------------#


  # ############################################################################################# #
  # --------------------------------------------------------------------------------------------- #
  #    THIS IS THE MEAT OF THE FUNCTION (Everything before is just checking the input)            #    
  # _____________________________________________________________________________________________ #
  # --------------------------------------------------------------------------------------------- #
  #
  # The next sections for "sample by perc" and "sample by size" are programatically 
  #   the same, the key difference of course being the input to the `sample(.)` function. 
  # They both sample _from_ the same spot, but "by size" uses `sample(.., size=size)`
  #   whereas, "by perc" uses `sample(.., size= .N * perc )`
  # Also, the section for "by size" has an additional error check to ensure 
  #   that `size` is not too large for any given group. If so, it throws an error. 
  #
  #  Explanation of the process.
  #     The sampling is three nested calls to `[.data.table`
  #     1.  create a .rowID of the entire DT ie,  1:nrow(DT)  
  #     2.  sample from the .rowID, but `by=group`  
  #     3.  return only the sampled indecies, named .rowIndex


    # SAMPLE BY EQUAL PERCENTAGE OF EACH GROUP
    if(!missing(perc) || missing(size) || is.na(size)) {
      rowIndex <- 
        DT [ , c(.SD, list(.rowID=seq(nrow(DT)))) 
          ][ , list(.rowIndex=sample(.rowID, round(.N*perc), replace=replace)), keyby=group
          ][, .rowIndex]

    # SAMPLE BY EQUAL SAMPLE SIZE PER EACH GROUP
    }  else { 

      # if we're inside this if-else-branch, size should not be NA
      if (is.na(size))
        stop("Neither `perc` nor `size` were correctly specified.")

      # Check that each group has enough rows to sample from. 
      errorCheck <- setkey(DT[, list(ec = if(.N < size) "ERROR" else "OK"), by=group], "ec")
      if(.nrows <- nrow(errorCheck[ec=="ERROR"]) )
        stop(.nrows, " groups in DT have size less than size = ", size, '.\n')

      rowIndex <- 
        DT [ , c(.SD, list(.rowID=seq(nrow(DT)))) 
          ][ , list(.rowIndex=sample(.rowID, size, replace=replace)), keyby=group
          ][, .rowIndex]
    }
 
  #_____________________________________________________________________________________________#
  # ------------------------------------------------------------------------------------------- #
  # ########################################################################################### #

  if (values)
    return(DT[rowIndex])

  # else
  return(rowIndex)
}



samplingStats <- function(DT, RowIndex, group, perc=NA)  {
# computes the sampling statistics, per group, for a given data.table and RowIndex
# If perc is given, also computes the difference from perc.

  group <- parseAndCheckGroup(group, DT)


  Stats <- DT[RowIndex, list(Sample.N=.N), keyby=group] [ DT[, list(Total.N=.N), keyby=group]  ]
  Stats[, Sample.Perc := round(Sample.N/ Total.N, 2)]
  setcolorder(Stats, c(group,  "Total.N", "Sample.N", "Sample.Perc"))

  if (!missing(perc) && !is.na(perc)){
    if (!(perc >=0  && perc <= 1 ))
      warning ("`perc` expected to be between (0, 1).\nDid you mean to divide by 100?\n")
    Stats[, SampMinusPerc := round((Sample.N / Total.N) - perc, 2)]
  }
  return(Stats)
}



parseAndCheckGroup <- function(group, DT) { 
    ## Splitting the column names by commas.  (Will have no affect if group has no commas)
    group <- strsplit(unlist(group), "(?<!\\\\),", perl=TRUE)  # split on comma, except if the comma is escaped
    group <- unlist(c(group)) 

    # Check that all elements in group are in names(DT).  (ie, their setdiff(.) should have length 0)
    if(length(  NotFound <- setdiff(group, names(DT))  )) {     
      # checking if any column names have commas in them. 
      if( length(commadNames <- grep(",", names(DT), value=TRUE)) )
        warning("A comma (',') was detected in the column names of DT: \n\t\t", paste(commadNames, collapse="  "), 
                "\nGrouping by those names is supported, but you must escape the comma with a double backslash: '\\\\,'" )
 
      stop("The following columns in `group` are not found in the column names of `DT`: \n\t\t", paste(NotFound, collapse="  "), 
           "\nAll columns in groups should be quoted strings, or a list of quoted strings (ie, do not use 'J' style) \n\n")
    }
    return(group)
}




 ## ------------------------------- ##
 ## ------------------------------- ##
 #{          USAGE  EXAMPLES        }#
 ##                                 ##
 ## ------------------------------- ##



# ### TEST DATA:
# N <- 101
# samplingGroups <- list(
#     city   = c("Bangkok", "London", "Madrid", "Melbourne", "New York")
#   , season = c("Winter", "Spring", "Summer", "Fall")
#   , test   = c("A", "B")
#   , random = rnorm(N)
#   , value  = 10*(1:N)
#   )

# DT <- as.data.table(lapply(samplingGroups, sample, N, TRUE))

# ## EXAMPLE 1
# ## -----------
# Indexes  <- sampleByGroup(DT, "season,city", perc=0.67)
# Indexes
# TestSet  <- DT[ Indexes ]
# TrainSet <- DT[-Indexes ]
# samplingStats(DT, Indexes, "season,city", perc=0.67)


# ## EXAMPLE 2
# ## -----------
# CV.folds <- 10
# set.seed(7223)
# CVIndexes <- replicate(CV.folds, sampleByGroup(DT, "season"), simplify=FALSE)
# for (i in 1:10) { 
#   TestSet  <- DT[ CVIndexes[[i]] ]
#   TrainSet <- DT[-CVIndexes[[i]] ]
    
#   # Learn Something Awesome
# }


# ## Test Cases for different syntaxes for group: 
# GROUPS.LIST <- 
#   list(
#       group0 = "season"  # one group
#     , group1 = list("season", "test")
#     , group2 = c("season", "test")
#     , group3 = "season,test"
#     , group4 = list("season", "city,test")  # Not sure why anyone would purposefully use this, but it is supported
#   )
# for (group in GROUPS.LIST) { 
#   cat("\tTesting group:\n")
#   print(group)
#   try(sampleByGroup(DT, group))
#   cat("\n\n")
# }
# cat("Testing Complete\n")


