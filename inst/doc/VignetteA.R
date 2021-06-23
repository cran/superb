## ---- echo = FALSE, message = FALSE, results = 'hide'-------------------------
cat("this will be hidden; use for general initializations.\n")
library(superb)
library(ggplot2)
options(superb.feedback = c('design','warnings') )

## ---- message=FALSE, warning=FALSE--------------------------------------------
library(ggplot2)            # for the graphing commands
library(superb)             # for superbPlot and GRD
library(referenceIntervals) # for computing reference intervals

## ---- message=FALSE, echo=TRUE------------------------------------------------
glucoselevels <- GRD(BSFactors = "concentration(A,B,C,D,E)", 
                    SubjectsPerGroup = 100,
                    RenameDV = "gl",
                    Effects = list("concentration" = extent(10) ),
                    Population = list(mean = 100, stddev = 20) ) 

## ---- message=FALSE, echo=FALSE-----------------------------------------------
# as the package referenceIntervals cannot accommodate numbers below zero, lets remove them
glucoselevels$gl[glucoselevels$gl<10]<-10

## -----------------------------------------------------------------------------
head(glucoselevels)

## ---- message=FALSE, echo=TRUE, fig.width = 5, fig.cap="**Figure 1**. Mean glucose level as a function of concentration."----
superbPlot(glucoselevels, 
            BSFactors = "concentration",  
            variables = "gl", 
            statistic = "mean", 
            errorbar  = "CI",
            gamma     = 0.95,
            plotStyle = "line")

## -----------------------------------------------------------------------------
min(glucoselevels$gl)
max(glucoselevels$gl)

## -----------------------------------------------------------------------------
RI.mean <- function(data, gamma = 0.95) {
    refLimit(data, refConf = gamma)$Ref_Int
}

## ---- message=FALSE, echo=TRUE, fig.width = 5, fig.cap="**Figure 2**. Mean glucose level and 95% reference intervals as a function of concentration."----
superbPlot(glucoselevels, 
            BSFactors = "concentration",  
            variables = "gl", 
            statistic = "mean", # mean is what RI is attached to
            errorbar  = "RI",   # RI calls the function above
            gamma     = 0.95,   # select the coverage desired
            plotStyle = "line" )

## -----------------------------------------------------------------------------
ciloRI.mean <- function(data, gamma = c(0.95, 0.90) ) {
    refLimit(data, refConf = gamma[1], limitConf = gamma[2] )$Conf_Int[1:2]
}
cihiRI.mean <- function(data, gamma = c(0.95, 0.90) ) {
    refLimit(data, refConf = gamma[1], limitConf = gamma[2] )$Conf_Int[3:4]
}

## ---- message=FALSE, echo=TRUE, fig.width = 5, fig.cap="**Figure 3**. Mean glucose level and 90% confidence intervals of the upper RI tips."----
superbPlot(glucoselevels, 
            BSFactors = "concentration",  
            variables = "gl", 
            statistic = "mean", errorbar = "cihiRI",
            gamma     = c(0.95, 0.90),
            plotStyle = "line" ) 

## -----------------------------------------------------------------------------
ornate = list(
        labs(title =paste("(tick)     95% reference intervals (RI)",
                        "\n(red)      90% confidence intervals of upper 95% RI",
                        "\n(purple) 90% confidence intervals of lower 95% RI",
                        "\n(blue)    95% confidence intervals of the mean")),
        coord_cartesian( ylim = c(000,200) ),
        theme_light(base_size=14) # smaller font
)

## ---- message=FALSE-----------------------------------------------------------
plt1 <- superbPlot(glucoselevels, 
            BSFactors = "concentration",  
            variables = "gl", 
            statistic = "mean", 
            errorbar  = "RI",
            gamma     = 0.95,
            errorbarParams = list(width = 0.0, size = 1.5,
                                  position = position_nudge( 0.0) ),
            plotStyle = "line" ) + ornate
plt2 <- superbPlot(glucoselevels, 
            BSFactors = "concentration",  
            variables = "gl", 
            statistic = "mean", 
            errorbar  = "cihiRI",
            gamma     = c(0.95, 0.90),
            errorbarParams = list(width = 0.2, size = 0.2, color = "red",
                                  direction = "left",
                                  position = position_nudge(-0.15) ),
            plotStyle = "line" ) + ornate + makeTransparent()
plt3 <- superbPlot(glucoselevels, 
            BSFactors = "concentration",  
            variables = "gl", 
            statistic = "mean", 
            errorbar  = "ciloRI",
            gamma     = c(0.95, 0.90),
            errorbarParams = list(width = 0.2, size = 0.2, color = "purple",
                                  direction = "left",
                                  position = position_nudge(-0.15) ),
            plotStyle = "line" ) + ornate + makeTransparent()

## ---- message=FALSE, echo=TRUE, fig.width = 5, fig.cap="**Figure 3a**. Mean glucose level and 95% reference intervals with 95% confidence intervals."----
# transform the three plots into visual objects
plt1 <- ggplotGrob(plt1)
plt2 <- ggplotGrob(plt2)
plt3 <- ggplotGrob(plt3)

# superimpose the grobs onto an empty ggplot 
ggplot() + 
    annotation_custom(grob=plt1) + 
    annotation_custom(grob=plt2) + 
    annotation_custom(grob=plt3)

## ---- message=FALSE, echo=TRUE, fig.width = 5, fig.cap="**Figure 3b**. Jittered dots showing mean glucose level and 95% reference intervals with 95% confidence intervals."----
# redo plt1; the other 2 are still in memory
plt1 <- superbPlot(glucoselevels, 
            BSFactors = "concentration",  
            variables = "gl", 
            statistic = "mean", 
            errorbar  = "RI",
            gamma     = 0.95,
            errorbarParams = list(width = 0.0, size = 1.5,
                                  position = position_nudge( 0.0) ),
            plotStyle = "pointjitter" ) + ornate

# transform the new plot into a visual object
plt1 <- ggplotGrob(plt1)

# superimpose the grobs onto an empty ggplot 
ggplot() + 
    annotation_custom(grob=plt1) + 
    annotation_custom(grob=plt2) + 
    annotation_custom(grob=plt3)

## ---- message=FALSE, echo=TRUE, fig.width = 5, fig.cap="**Figure 3c**. Jittered dots and violins showing mean glucose level and 95% reference intervals with 95% confidence intervals of the tips' position."----
# redo plt1; the other 2 are still in memory
plt1 <- superbPlot(glucoselevels, 
            BSFactors = "concentration",  
            variables = "gl", 
            statistic = "mean", 
            errorbar  = "RI",
            gamma     = 0.95,
            errorbarParams = list(width = 0.0, size = 1.5,
                                  position = position_nudge( 0.0) ),
            plotStyle = "pointjitterviolin" ) + ornate

# transform the three plots into visual objects
plt1 <- ggplotGrob(plt1)

# superimpose the grobs onto an empty ggplot 
ggplot() + 
    annotation_custom(grob=plt1) + 
    annotation_custom(grob=plt2) + 
    annotation_custom(grob=plt3)

## ---- message=FALSE-----------------------------------------------------------
plt4 <- superbPlot(glucoselevels, 
            BSFactors = "concentration",  
            variables = "gl", 
            statistic = "mean", 
            errorbar  = "CI",  # just the regular CI of the mean
            errorbarParams = list(width = 0.2, size = 1.5, color = "blue",
                                  position = position_nudge( 0.00) ),
            gamma     = 0.95,
            plotStyle = "line" ) + ornate + makeTransparent()

## ---- message=FALSE, echo=TRUE, fig.width = 5, fig.cap="**Figure 3d**. Jittered dots and violins showing mean glucose level +-95% confidence intervals of the mean, and 95% reference intervals with 95% confidence intervals."----
# transform that plot too into visual objects
plt4 <- ggplotGrob(plt4)

# superimpose the grobs onto an empty ggplot 
ggplot() + 
    annotation_custom(grob=plt1) + 
    annotation_custom(grob=plt2) + 
    annotation_custom(grob=plt3) +
    annotation_custom(grob=plt4)

