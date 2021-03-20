## ---- echo = FALSE, message = FALSE,  results = 'hide'------------------------
cat("this will be hidden; use for general initializations.")
library(superb)
library(ggplot2)

## -----------------------------------------------------------------------------
head(dataFigure2)

## -----------------------------------------------------------------------------
library(schoRsch)

t_out(t.test(dataFigure2$pre, dataFigure2$post, var.equal=T))

## ---- message=FALSE, echo=TRUE, fig.cap="Figure 1: Representation of the individual participants"----
library(reshape2)

# first transform the data in long format; the pre-post scores will go into column "variable"
dl <- melt(dataFigure2, id="id")

# make a plot, with one color for each participant
ggplot(data=dl, aes(x=variable, y=value, colour=id, group=id)) + geom_line() +
    coord_cartesian( ylim = c(70,150) ) +
    geom_abline(intercept = 102.5, slope = 0, colour = "red", size = 0.5, linetype=2)

## ---- message=FALSE, echo=TRUE, fig.cap="Figure 1: Representation of the *subject-centered* individual participants"----
# use superb:::subject_centering_transform function 
df2 <- superb:::subject_centering_transform(dataFigure2, c("pre","post"))

# tranform into long format
dl2 <- melt(df2, id="id")

# make the plot
ggplot(data=dl2, aes(x=variable, y=value, colour=id, group=id)) + geom_line()+
    coord_cartesian( ylim = c(70,150) ) +
    geom_abline(intercept = 102.5, slope = 0, colour = "red", size = 0.5, linetype=2)

## -----------------------------------------------------------------------------
t_out(t.test(dataFigure2$pre, dataFigure2$post, paired=TRUE))

## -----------------------------------------------------------------------------
cor(dataFigure2$pre, dataFigure2$post)

## ---- message=FALSE, warning=FALSE, echo=TRUE, fig.cap="Figure 3: Means and 95% confidence intervals on raw data (left) and on decorrelated data (right)"----
library(gridExtra)

## realize the plot with unadjusted (left) and ajusted (right) 95\% confidence intervals
plt2a <- superbPlot(dataFigure2, 
        WSFactor    = "Moment(2)", 
        adjustments = list(purpose = "difference"), 
        variables   = c("pre","post"), 
        plotStyle   = "line", Quiet = TRUE ) + 
    xlab("Group") + ylab("Score") + 
    labs(title="Difference-adjusted\n95% confidence intervals") +
    coord_cartesian( ylim = c(85,115) ) +
    theme_gray(base_size=10) + 
    scale_x_discrete(labels=c("1" = "Collaborative games", "2" = "Unstructured activity"))
plt2b <- superbPlot(dataFigure2, 
        WSFactor    = "Moment(2)", 
        adjustments = list(purpose = "difference", decorrelation = "CA"),  #only difference
        variables   = c("pre","post"), 
        plotStyle   = "line", Quiet = TRUE ) + 
    xlab("Group") + ylab("Score") + 
    labs(title="Correlation and difference-adjusted\n95% confidence intervals") +
    coord_cartesian( ylim = c(85,115) ) + 
    theme_gray(base_size=10) +
    scale_x_discrete(labels=c("1" = "Collaborative games", "2" = "Unstructured activity"))
plt2  <- grid.arrange(plt2a,plt2b,ncol=2)

## ---- message=FALSE, warning=FALSE, echo=FALSE, fig.cap="Figure 4: All three decorelation techniques on the same plot along with un-decorrelated error bars"----
    # using GRD to generate data with correlation of .8 and a moderate effect
    test <- GRD(WSFactor = "Moment(5)", 
                Effects = list("Moment" = extent(10) ),
                Population = list(mean = 100, stddev = 25, rho = 0.8) ) 

    # the common label to all 4 plots
    tlbl <- paste( "(red) Difference-adjusted only\n",
                "(blue) Difference adjusted and decorrelated with CM\n",
                "(green) Difference-adjusted and decorrelated with LM\n",
                "(orange) Difference-adjusted and decorrelated with CA", sep="")

    # to make the plots all identical except for the decorrelation method
    makeplot <- function(dataset, decorrelationmethod, color, nudge) {
        superbPlot(dataset, 
                WSFactor = "Moment(5)",  
                variables   = c("DV.1","DV.2","DV.3","DV.4","DV.5"), 
                Quiet = TRUE, # shut down messages
                adjustments=list(purpose = "difference", decorrelation = decorrelationmethod), 
                errorbarParams = list(color=color, width= 0.1, position = position_nudge(nudge) ),
                plotStyle="line" ) + 
            xlab("Group") + ylab("Score") + 
            labs(subtitle=tlbl) +
            coord_cartesian( ylim = c(85,115) ) +
            theme_gray(base_size=16) +
            theme(plot.subtitle=element_text(size=12, color="black"), 
                  panel.background = element_rect(fill = "transparent"),
                  plot.background = element_rect(fill = "transparent", color = "white"))
    }

    # generate the plots, nudging the error bars and using distinct colors
    pltrw <- makeplot(test, "none", "red",    -0.15)
    pltCM <- makeplot(test, "CM",   "blue",   -0.05)
    pltLM <- makeplot(test, "LM",   "chartreuse3",  +0.05)
    pltCA <- makeplot(test, "CA",   "orange", +0.15)

    # transform the ggplots into "grob" so that they can be manipulated
    pltrwg <- ggplotGrob(pltrw)
    pltCMg <- ggplotGrob(pltCM)
    pltLMg <- ggplotGrob(pltLM)
    pltCAg <- ggplotGrob(pltCA)

    # put the grobs onto an empty ggplot 
    ggplot() + 
        annotation_custom(grob=pltrwg) + 
        annotation_custom(grob=pltCMg) + 
        annotation_custom(grob=pltLMg) + 
        annotation_custom(grob=pltCAg)

## ---- message=FALSE, warning=FALSE, echo=TRUE, fig.cap="Figure 3: Means and 95% confidence intervals on raw data (left) and on decorrelated data (right)"----
superbPlot(dataFigure2, 
    WSFactor    = "Moment(2)", 
    adjustments = list(purpose = "difference", decorrelation = "CM"), 
    variables   = c("pre","post"), 
    plotStyle   = "pointindividualline", 
    Quiet       = TRUE ) + 
xlab("Group") + ylab("Score") + 
labs(subtitle="Correlation- and Difference-adjusted\n95% confidence intervals") +
coord_cartesian( ylim = c(70,150) ) +
theme_gray(base_size=16) + 
scale_x_discrete(labels=c("1" = "Collaborative games", "2" = "Unstructured activity"))

