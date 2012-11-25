## ggplot2 enhancements.R


##############################################################################
##+------------------------------------------------------------------------+##
##                                                                          ##
##       These are a collection of functions/wrappers for ggplot2.          ##
##                                                                          ##
##                                                                          ##                      
##############################################################################


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
##                                                                                                       ##
##                            Viewports for layout design;  for use with print                           ##
##                                                                                                       ##
##-------------------------------------------------------------------------------------------------------##
                                                                                                        
   # q1, q2, q3, q4  are the cartesian-like quadrants for a 2x2 grid, (q1 & q4 on the right).           

   vp.q1 <- viewport(width = 0.5, height = 0.485, x = 0.5, y = 1.0, just = c("left","top"), name="vp.q2")     
   vp.q2 <- viewport(width = 0.5, height = 0.485, x = 0.0, y = 1.0, just = c("left","top"), name="vp.q1")   
 
   vp.q3 <- viewport(width = 0.5, height = 0.485, x = 0.0, y = 0.5, just = c("left","top"), name="vp.q3")
   vp.q4 <- viewport(width = 0.5, height = 0.485, x = 0.5, y = 0.5, just = c("left","top"), name="vp.q4")
##                                                                                                       ##
##-------------------------------------------------------------------------------------------------------##

  #layout 3 vertical
  l3vert <- grid.layout(ncol=3)

  vp.midUp   <- viewport(width = 1, height = 0.2, x = 0.5, y = 0.5, just = c("centre","bottom"), name="vp.midUp")    
  vp.midDown <- viewport(width = 1, height = 0.2, x = 0.5, y = 0.5, just = c("centre","top"), name="vp.midDown")   
   
  vp.midDown3 <- viewport(layout=l3vert, width = 1, height = 0.2, x = 0.5, y = 0.5, just = c("centre","top"), name="vp.midDown")   

  #---
  #  This should be able to handled with pushViewport(vpname) --- i just cant figure it out right now
  vp.midL   <- viewport(width = .33, height = 0.2, x = 0.0, y = 0.5, just = c("left","top"), name="vp.midL")    
  vp.midC   <- viewport(width = .34, height = 0.2, x = .34, y = 0.5, just = c("left","top"), name="vp.midC")    
  vp.midR   <- viewport(width = .33, height = 0.2, x = .67, y = 0.5, just = c("left","top"), name="vp.midR")    

   # # TESTING
   # grid.show.layout(grid.layout(), vp=vp.midDown3)
   # ggNewCanvass()
   # grid.rect(vp=vp.midR, gp=gpar(col="blue"))
##                                                                                                       ##
##-------------------------------------------------------------------------------------------------------##





##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
##                                                                                                       ##
##                            pgTitle & vertbar                                                          ##
##                                                                                                       ##
##-------------------------------------------------------------------------------------------------------##


pgTitle <- function(txt, bgalpha=0, bgcolor="white", border=0) {
  ## put title at top of page

        grid.rect(y = 1, height = unit(1.1, "lines"), just = c("center", "top"), 
                  gp=gpar(fill=bgcolor, alpha=bgalpha, lwd=border))

        grid.text(txt, y = unit(1, "npc") - unit(0.65, "lines"), gp = gpar(font = 2))
    }


vertbar <- function(vert=TRUE, shift=0, thick=1, color="black") {
# add horizontal bar across grid space

  # Arguments: 
  #   shift should be a percentage.  Positive values of shift go up and right. 

  val = 0.5 + (shift/100)

  if (vert) {
    grid.rect(x=unit(val, "npc"), width=unit(thick, "points"), gp=gpar(fill=color))
  } else  {
    grid.rect(y=unit(val, "npc"), height=unit(thick, "points"), gp=gpar(fill=color))
  }
}





##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##

# Some Defaults

qTit7 <- theme(plot.title = element_text(size = 7, hjust=.15, face="bold"))
qTit8 <- theme(plot.title = element_text(size = 8, hjust=.15, face="bold"))


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
##                                                                                                       ##
##                            ggtext:   Plots a character object within plot bounds                      ##
##                                                                                                       ##
##-------------------------------------------------------------------------------------------------------##


ggtextbasic <- function(txt, mono=TRUE) {
  ggtext(txt, s=2.5, x=0, y=0, useMono=mono)
}


ggtext <- function(txt, Title=NULL, x=50, y=50, s=3, h=0, v=0, mono=FALSE, useMono=mono)  {

  # TODO:  Incorporate with ggNewCanvass?

  # Arguments: 
  #   txt:  the text to output
  #   x, y: location of where to plot. Values are 0:100
  #   s:    size of text
  #   h, v: justification for horizontal & vertical
  #   useMono: Boolean flag.  If T, use a fixed-with font.     
  #   mono:  a synonym for useMono

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




# TODO: Make sure this is the best way to get a blank canvass. 
# Will it work when mixed with other plots?  Can I turn on/off 'new'?
ggNewCanvass <- function(scale=100) {
  # scale, generally, will be 100 or 1.  

  # If scale is user-given, make sure it is valid
  if (!missing(scale)) { 
    if (isNumber(scale)) {
      scale <- as.numeric(scale)      
    } else {
      warning("Invalid value '", scale, "' given for scale. Using 100.")
      scale <- 100
    }
  }
  

# OPTION 1:
  #  df <- data.frame()
  #  ggplot(df) + geom_point() + xlim(0, scale) + ylim(0, scale)

# OPTION 2:

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

      # clean up the plot background 
      theme_bw() +
      theme(panel.grid.major = none, panel.grid.minor = none) + 
      theme(legend.position = "none") +
      theme(panel.background = none) + 
      theme(panel.border = none) + 

      geom_blank()
}



##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
##                                                                                                       ##
##                            qqplot:                              ##
##                                                                                                       ##
##-------------------------------------------------------------------------------------------------------##

qq     <- function(...) UseMethod("qqplot")
qqplot <- function(...) UseMethod("qqplot")


qqplot.lm <- function(LM)  {
# argument: a linear model

    # following four lines from base R's qqline()
    y <- quantile(LM$resid[!is.na(LM$resid)], c(0.25, 0.75))
    x <- qnorm(c(0.25, 0.75))
    slope <- diff(y)/diff(x)
    int <- y[1L] - slope * x[1L]

    # Create ggplot
    ggplot(LM) +
      aes(sample=.resid) +
      geom_abline(slope=slope, intercept=int, color="red", alpha=0.85) + 
      stat_qq(alpha=0.5) + 
      ylab("Residuals") +
      xlab("Theoretical")
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
multiplot <- function(..., plotlist=NULL, file, cols=2, byrow=TRUE, layout=NULL) {
  require(grid)

  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)), byrow=byrow,
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
