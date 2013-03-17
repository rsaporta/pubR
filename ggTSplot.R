ggTSplot <- function(TS, title=NULL, yscale=NULL, ptScale=0.8, usePoints=FALSE, appendTitle=FALSE, prefixTitle=TRUE, yl="Value", xl="Time")  {
# Plotting a `ts` object (a time series object) using ggplot2
#
# ARGS:
#            TS:  the time series to be plotted
#         title:  the first line of the title of the plot
#   appendTitle:  If T, then title will be appended with the info from str(title)
#   prefixTitle:  prepend str title with quantitative descriptor ("monthly", "quarterly", etc)
#                 Only applies when appendTitle is T or title is not explicitly given
#     usePoints:  If T:  will superimpose points on the line for each observation
#       ptScale:  Used to control size of points.  
#       yl; xl :  x & y labels to the plot


  # confirm correct objec tsent
  if (!is.ts(TS))
    stop("TS must be a `ts` object")

  # grab time information
  y <- time(TS)

  TS_df <- data.frame(time=c(y), value=c(TS)) 

  #------------------------------------------#
  #        add title if it is NULL           #
  #------------------------------------------#
    if(missing(title) || appendTitle==TRUE) 
      title <- if (appendTitle) paste(title, titleFromTS(TS, prefixTitle), sep="\n") else titleFromTS(TS, prefixTitle)
  #------------------------------------------#


  # adqust y axis. Ensure correct input, else default to range of value
  if(length(yscale) != 2 & !is.null(yscale))
    warning("yscale should be a vector of two elements. Defaulting to 'range'")
  if(is.null(yscale) || length(yscale) != 2)
    yscale <- range(TS_df[["value"]]) 

   P <- 
   ggplot(TS_df, aes(x=time, y=value)) + 
      geom_line() + 
      scale_y_continuous(limits=yscale)  + 
      ggtitle(title) + 
      xlab(xl) +
      ylab(yl) 

  if (length(TS_df[["time"]])>190)
    ptScale <- ptScale * 1/2
   # RETURN
   if (usePoints)
        P <- P + geom_point(color="black", size=2.3*ptScale, shape=20) +
                 geom_point(color="white", size=2.0*ptScale, shape=20)
    
    return(P)
} 




titleFromTS <- function(TS, prefix=TRUE) {

      title <- capture.output(str(TS))

      # count the number of observations from STR
      .bracs <- regmatches(title, regexpr("(?<=\\[).+(?=\\])", title, perl=TRUE))
      if (length(.bracs) && nchar(.bracs) > 2) {
         .obs   <- 1 + diff(as.numeric( strsplit(.bracs, ":")[[1]] ))
         .obs   <- paste0("\n(", .obs, " Observations)") 
      } else .obs <- NULL

      # remove evetyhing inside brackets
      title <- gsub("\\[(.+\\]) ", "", title)

      # Substr up until the end of the "to YYYY" portion (ie right before the colon)
      title <- substr(title, 2, regexpr(":", title) - 1)
      
      # add the observation count (will be NULL if not present)
      title <- paste0(title, .obs)

      # add Quantitleative adj to titlele
      if (prefix) 
                # switching based on the frequency attribute of the TS, then pasting the result to front of title
        title <- paste0(switch(as.character( attr(TS, "tsp")[[3]] ), 
                      '1' = "Monthly ", '4' = "Quarterly ",  '6' = "SemiAnnually ",  '12' = "Monthly ",  '7' = "Weekly" , "")
                     , title)

      return(title)
    }
