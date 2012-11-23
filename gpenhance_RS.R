## gpenhance_RS.R




##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
##
##     ggtext:   Plots a character object within plot bounds
##

ggtext <- function(txt, Title=NULL, x=50, y=50, s=3, h=0, v=0, useMono=FALSE)  {
  # Arguments: 
  #   txt:  the text to output
  #   x, y: location of where to plot. Values are 0:100
  #   s:    size of text
  #   h, v: justification for horizontal & vertical
  #   useMono: Boolean flag.  If T, use a fixed-with font.     

    ## ADJUST THE TEXT
        # Collapse if multi-lined
        txt <- paste(txt, collapse = "\n", sep = "")
        
        # Replace tabs with spaces
        # From gplots:
        # txt <- replaceTabs(txt, width = tab.width)

    # basic params used for setting plot
    dat <- data.frame(x=c(0, 100), y=c(0, 100))
    none <- element_blank()
    fam <- ifelse(useMono, "mono", "serif")   # based on flag set by call to function

    # plot
    ggplot(dat) + 
      # remove axis, and scale at [0:100]
      scale_y_continuous(limits = c(0, 100), expand = c(0, 0), breaks=NULL) +
      scale_x_continuous(limits = c(0, 100), expand = c(0, 0), breaks=NULL) +
      ylab(NULL) + xlab(NULL) + 

      # title the plot
      ggtitle(Title)  + 

      # clean up the plot background 
      theme_bw() +
      theme(panel.grid.major = none, panel.grid.minor = none) + 
      theme(legend.position = "none") +
      theme(panel.background = none) + 
      theme(panel.border = none) +

      # plot the text
      annotate("text", x=x, y=y, label=txt, family=fam, size=s, hjust=h, vjust=v) 

}





##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
##
## SOURCE:  http://stackoverflow.com/questions/4357031/qqnorm-and-qqline-in-ggplot2
##
##
qq     <- function(...) UseMethod("qqplot")
qqplot <- function(...) UseMethod("qqplot")


qqplot.lm <- function(LM) # argument: a linear model
{
    y <- quantile(LM$resid[!is.na(LM$resid)], c(0.25, 0.75))
    x <- qnorm(c(0.25, 0.75))
    slope <- diff(y)/diff(x)
    int <- y[1L] - slope * x[1L]

    ggplot(LM) +
      aes(sample=.resid) +
      geom_abline(slope=slope, intercept=int, color="red", alpha=0.85) + 
      stat_qq(alpha=0.5)
}


qqplot.data <- function (vec)  {
  # argument: vector of numbers 

  # following four lines from base R's qqline()
  y <- quantile(vec[!is.na(vec)], c(0.25, 0.75))
  x <- qnorm(c(0.25, 0.75))
  slope <- diff(y)/diff(x)
  int <- y[1L] - slope * x[1L]

  d <- data.frame(resids=vec)

  ggplot(d, aes(sample=resids)) + 
     stat_qq() + 
     geom_abline(slope=slope, intercept=int,  color="111144")
}




##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##


#``````````````````````````````````````````````````````````````````#
#
#  SOURCE:  http://wiki.stdout.org/rcookbook/Graphs/Multiple%20graphs%20on%20one%20page%20(ggplot2)/
#t
# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=2, layout=NULL) {
  require(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
