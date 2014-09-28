as.dt_as.df_are.same <- function(x, quietly=FALSE, check.names=FALSE) {
## compares as.data.table(x) to as.data.frame(x) 
##  by wrapping the former in as.data.frame and testing with identical
## returns TRUE if identical, FALSE otherswise
##
## If value is FALSE, will also output each of x, as.data.table(x), as.data.frame(x) 
##   unless quietly flag set to FALSE

  x.dt <- try(as.data.table(x), silent=TRUE)
  x.df <- try(as.data.frame(x, stringsAsFactors=FALSE), silent=TRUE)

  ## Check for errors
  iserr.dt <- inherits(x.dt, "try-error")
  iserr.df <- inherits(x.df, "try-error")
  if (iserr.dt && iserr.df) {
    message("both of as.data.frame(x) and as.data.table(x) threw an error")
    return(invisible(TRUE))
  }
  if (iserr.dt && !iserr.df) {
    warning("DT failed but DF did not")
    return(invisible(FALSE))
  }
  if (!iserr.dt && iserr.df) {
    warning("DF failed but DT did not")
    return(invisible(FALSE))
  }


  if (!check.names) {
    setattr(x.df, "names", rep(NA_character_, ncol(x.df)))
    rownames(x.df) <- NULL
    setattr(x.dt, "names", rep(NA_character_, ncol(x.dt)))
  }

  x.dt_as.df <- as.data.frame(x.dt)
  ret <- identical(x.dt_as.df, x.df)

  ## if indetical fails, check the acutal values. 
  ## Perhaps it is just an attribtue that is different
  if (!ret && identical(dim(x.dt_as.df), dim(x.df))) {
    if (all(x.dt_as.df == x.df)) {
      warning("All values and dimensions are the same between DF and DT.\nHowever, some differences remain, perhaps in attirbute or attirbute of a column")
      return(invisible(TRUE))
    }
  }

  if (!ret && !quietly) {
    warning ("as.data.frame and as.data.tabel produced different results\n")
    out.dt <- capture.output(x.dt)
    out.df <- capture.output(x.df)
    out <- cbind_out(out.dt, out.df, "as.data.table", "as.data.frame", pre=0, gap=5)
    cat(cbind_out(x, out[-1], "input", out[1], pre=2, gap=8), sep="\n")
  }

  return(invisible(ret))
}


cbind_out <- function(out.left, out.right, header.left=NULL, header.right=NULL, gap.length=6, pregap.length=3) {
## Wrapper function for fancy output. 
## Takes two outputs and combines them side-by-side. 

  ## Check valid headers
  if ( is.null(header.left) && !is.null(header.right))
    header.left <- ""
  if (!is.null(header.left) &&  is.null(header.right))
    header.right <- ""
  if (!is.null(header.left)) {
    if ((!is.character(header.left) || length(header.left) > 1) ||  (!is.character(header.right) || length(header.right) > 1) )
      stop ("'header.left' and 'header.right' should each be a string of length 1 (or NULL)")
  }

  if (!is.character(out.left))
    out.left <- capture.output(print(out.left))
  if (!is.character(out.right))
    out.right <- capture.output(print(out.right))

  out.left  <- c(header.left,  capture.output(cat(out.left,  sep="\n")))
  out.right <- c(header.right, capture.output(cat(out.right, sep="\n")))

  ## Check that the lengths are not too wide, relative to getOption("width")
  if (max(nchar(out.left)) + quantile(nchar(out.right), .8) + gap.length + pregap.length > getOption("width", 80))
    return(c(out.left, "", out.right))


  n.l <- length(out.left)
  n.r <- length(out.right)

  if (n.l < n.r)
    out.left <- c(out.left, rep("", n.r-n.l))
  if (n.r < n.l)
    out.right <- c(out.right, rep("", n.l-n.r))

    nchar.left <- nchar(out.left)
  mxnchar.left <- max(nchar.left)

  padding.left <- sapply(mxnchar.left - nchar.left + gap.length, function(n) paste0(rep(x=" ", times=n), collapse=""))
  out.left <- paste0(out.left, padding.left)

  pre_padding <- paste0(rep(" ", pregap.length), collapse="")
  paste0(pre_padding, out.left, out.right)
}
