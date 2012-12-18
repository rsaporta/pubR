

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

   vp.q1 <- viewport(width = 0.5, height = 0.485, x = 0.5, y = 1.0, just = c("left","top"), name="vp.q1")     
   vp.q2 <- viewport(width = 0.5, height = 0.485, x = 0.0, y = 1.0, just = c("left","top"), name="vp.q2")   
 
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
##                            pgTitle & vertbar & borderize                                              ##
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


borderize <- function(plotObj, thick=2, color="black", alpha=1, color2="grey", thick2=3) {
  # thickness should be a value between (1, 100)
  # alpha should be between (0, 1)

  # error check for correct input
  if (thick > 100 || thick < 0)
    warning("thick should be between (0, 100)")

  if (alpha > 1 || alpha < 0)
    warning("thick should be between (0, 100)")

  # these lines could be modified for separate width/height thicknesses
  wd <- ht <-  (100 - (thick+thick2/4)) / 100
  x <- (1 - wd)  / 2 
  y <- (1 - ht) / 2

  # create a solid rectangle.  The plot will go over it. 
  grid.rect(x=0, y=0, height=1, width=1, just = c("left", "bottom"), 
            gp=gpar(fill=color, alpha=alpha, col=color2, lwd=unit(thick2, "npc")))

  # create the viewport
  vp.inner <- viewport(height=unit(ht, "npc"), width=unit(wd, "npc"), just=c("left","bottom"), y=y, x=x)

  print(plotObj, vp=vp.inner)

  # note: 
  # Note that you can also modify `plot.background` and `panel.background` with theme() in ggplot2.  
  # However, this will impact your labels and legend, depending on the thickness of border, font size etc. eg: 
  #     plot.bg  <- theme(plot.background=element_rect(color="red", size=20))
  #     panel.bg <- theme(panel.background=element_rect(color="blue", size=20))
  #     plotObj + panel.bg + plot.bg    #  red border is `plot`,  blue border is `panel`

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



#------------------------------------------------ 
# density plot at edge of graph

density <- geom_rug(col=rgb(.05, .3,.2,alpha=.2))

#------------------------------------------------ 


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





#--------------------------------------------------#
#                                                  #
#   Functions to add equations to plot as texts    #
#                                                  #
#__________________________________________________#


#--------------------------------------------------#
    ## Example Usage: 

    #   gtxt_equation(model01)
    #   gtxt_equation(model01, xpos=-.5, ypos=.62)

    ## LONGER EXAMPLE

    #   x <- sample(1:30, 5)
    #   y <- (3) + (2/3 * x) + (rnorm(x, 0, 0.1))
    #   model1 <- lm(y~x)


    # Three different ways to call it

        # p2 <- qplot(y=y, x=x)
        # ptext <- gtxt_equation(model1)
        # p2 + ptext

        # p2 <- qplot(y=y, x=x)
        # p2 + gtxt_equation(model1)

        # p2 <- qplot(y=y, x=x) + 
        #       gtxt_equation(model1)

#--------------------------------------------------#



  # Generic form
  gtxt_equation <- function(m, ...)
  # m should be a model. For now, just a lm. 
     UseMethod("gtxt_equation")

    ## TODO: add other methods


  gtxt_equation.lm <- function(m, xpos=.2, ypos=.85) {
  # m is a linear model
  # xpos/ypos  should be between (-1, +1), although not required. 

    # Grab the variable information from the model. 
    vars <- m$model
    names(vars) <- names(m$model)
      
    #---------------------------#
    # CLEAN THiS UP FOR MULTIPLE VARS
      y <- vars[1]  # not using '[[' to maintain list-str & name
      x <- vars[2] 
      # modeltype is possibly a method.  ie, in this case, lm. 
    #---------------------------#


    #-----------------------------#
    ## COMPUTE XPOS, YPOS & TEXT ##
    #-----------------------------#
      ypos <- min(y) + (ypos * diff(range(y)) )
      xpos <- min(x) + (xpos * diff(range(x)) )

      labelText <- lm_eqn_to_char(m)
    #---------------------------#


        # This is what we would like to return
    dat <- data.frame(xpos, ypos, eval(labelText))  

    # as.data.frame(t(c(unlist(vars), labelText)))
    geom_text(data=dat, aes(x = xpos, y = ypos, label = labelText), parse = TRUE)
  }

  #---------------------------#
  # CREATE THE EQUATION TEXT  #
  #---------------------------#

  # source: http://stackoverflow.com/questions/7549694/ggplot2-adding-regression-line-equation-and-r2-on-graph
  # authors:  @Ramnath, with cleanup by @Jayden.

    lm_eqn_to_char = function(m) {
      # m is a model

      require(grDevices)   # plotmath

      ## TODO:  use variable names instead of "y" and "x" in eq. 
      # get variable names
      vars <- names(m$model)

      l <- list(a  = format(coef(m)[1], digits = 2),
                b  = format(abs(coef(m)[2]), digits = 2),
                r2 = format(summary(m)$r.squared, digits = 3));

      if (coef(m)[2] >= 0)  {
        eq <- substitute(italic(get(y)) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,l)
      } else {
        eq <- substitute(italic(y) == a - b %.% italic(x)*","~~italic(r)^2~"="~r2,l)    
      }

      as.character(as.expression(eq));                 
    }
