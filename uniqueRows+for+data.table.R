#  unique.rows


  uniqueRows <- function(DT) { 
    # IF DT IS KEYED, FUNCTION ACTS SIMILAR TO unique.data.frame(.)

    # If already keyed (or not a DT), use regular unique(DT)
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
