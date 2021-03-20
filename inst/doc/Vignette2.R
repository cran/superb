## ---- echo = FALSE, message = FALSE, results = 'hide'-------------------------
cat("this will be hidden; use for general initializations.")
library(superb)
library(ggplot2)

## ---- message=FALSE, echo=FALSE, fig.cap="Figure 1: Mean scores along with 95% confidence interval for two groups of students on the quality of learning behavior."----
superbPlot(dataFigure1, 
        BSFactor = "grp", 
        variables = "score", 
        plotStyle="line" ) + 
    xlab("Group") + ylab("Score") + 
    labs(title="(stand-alone)\n95% confidence intervals") +
    coord_cartesian( ylim = c(85,115) ) +
    theme_gray(base_size=16) +
    scale_x_discrete(labels=c("1" = "Collaborative games", "2" = "Unstructured activity"))

## ---- message=FALSE, warning=FALSE, echo=TRUE---------------------------------
library(schoRsch)

t_out(t.test(dataFigure1$score[dataFigure1$grp==1], 
        dataFigure1$score[dataFigure1$grp==2],
        var.equal=T))

## ---- message=FALSE, echo=TRUE------------------------------------------------
t_out(t.test(dataFigure1$score[dataFigure1$grp==1], mu=100))

## ---- message=FALSE, echo=TRUE------------------------------------------------
t_out(t.test(dataFigure1$score[dataFigure1$grp==2], mu=105))

## ---- message=FALSE, echo=TRUE, fig.cap="Figure 2: Mean scores along with difference-adjusted 95% confidence interval for two groups of students on the quality of learning behavior."----
superbPlot(dataFigure1, 
        BSFactor = "grp", 
        adjustments=list(purpose = "difference"),  # the only new thing here
        variables = "score", 
        plotStyle="line" ) + 
    xlab("Group") + ylab("Score") + 
    labs(title="Difference-adjusted\n95% confidence intervals") +
    coord_cartesian( ylim = c(85,115) ) + 
    theme_gray(base_size=16) +
    scale_x_discrete(labels=c("1" = "Collaborative games", "2" = "Unstructured activity"))

## ---- message=FALSE, echo=TRUE, fig.cap="Figure 3: Two representation of the data with unadjusted (left) and adjusted (right) 95% confidence intervals"----
library(gridExtra)
plt1 <- superbPlot(dataFigure1, 
        BSFactor = "grp", 
        variables = "score", 
        plotStyle="line" ) + 
    xlab("Group") + ylab("Score") + 
    labs(title="(stand-alone)\n95% confidence intervals") +
    coord_cartesian( ylim = c(85,115) ) +
    theme_gray(base_size=16) +
    scale_x_discrete(labels=c("1" = "Collaborative games", "2" = "Unstructured activity")) 

plt2 <- superbPlot(dataFigure1, 
        BSFactor = "grp", 
        adjustments=list(purpose = "difference"), 
        variables = "score", 
        plotStyle="line" ) + 
    xlab("Group") + ylab("Score") + 
    labs(title="Difference-adjusted\n95% confidence intervals") +
    coord_cartesian( ylim = c(85,115) ) + 
    theme_gray(base_size=16) +
    scale_x_discrete(labels=c("1" = "Collaborative games", "2" = "Unstructured activity")) 

plt <- grid.arrange(plt1, plt2, ncol=2)

## ---- message=FALSE, echo=TRUE, fig.cap="Figure 4: Two representations of the results with adjusted and unadjusted error bars on the same plot"----
# generate the two plots, nudging the error bars, using distinct colors, and having the background transparent
plt1 <- superbPlot(dataFigure1, 
        BSFactor = "grp", 
        variables = "score", 
        errorbarParams = list(color="blue",position = position_nudge(-0.05) ),
        plotStyle="line" ) + 
    xlab("Group") + ylab("Score") + 
    labs(title="(red) Difference-adjusted 95% confidence intervals\n(blue) (stand-alone) 95% confidence intervals") +
    coord_cartesian( ylim = c(85,115) ) +
    theme_gray(base_size=16) +
    scale_x_discrete(labels=c("1" = "Collaborative games", "2" = "Unstructured activity")) +
    theme(panel.background = element_rect(fill = "transparent"),
         plot.background = element_rect(fill = "transparent", color = "white"))

plt2 <- superbPlot(dataFigure1, 
        BSFactor = "grp", 
        adjustments=list(purpose = "difference"), 
        variables = "score", 
        errorbarParams = list(color="red",position = position_nudge(0.05) ),
        plotStyle="line" ) + 
    xlab("Group") + ylab("Score") + 
    labs(title="(red) Difference-adjusted 95% confidence intervals\n(blue) (stand-alone) 95% confidence intervals") +
    coord_cartesian( ylim = c(85,115) ) + 
    theme_gray(base_size=16) +
    scale_x_discrete(labels=c("1" = "Collaborative games", "2" = "Unstructured activity")) +
    theme(panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = "white"))

# transform the ggplots into "grob" so that they can be manipulated
plt1g <- ggplotGrob(plt1)
plt2g <- ggplotGrob(plt2)

# put the two grob onto an empty ggplot (as the positions are the same, they will be overlayed)
ggplot() + 
    annotation_custom(grob=plt1g) + 
    annotation_custom(grob=plt2g)

