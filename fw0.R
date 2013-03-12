
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
