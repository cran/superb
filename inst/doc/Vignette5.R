## ---- echo = FALSE, message = FALSE, results = 'hide'-------------------------
cat("this will be hidden; use for general initializations.")
library(superb)
library(ggplot2)

## -----------------------------------------------------------------------------
superb:::is.superbPlot.function("superbPlot.line")

## -----------------------------------------------------------------------------
superbPlot.foo <- function(
    summarydata,
    xvar,       
    groupingfac,
    addfactors, 
    Debug, 
    rawdata        
    # any optional argument you wish
) {
    plot <- ggplot() ## ggplot instructions...        
    return(plot)
}

## ---- message=FALSE, echo=FALSE, fig.cap="Figure 1: Mean score with 95% confidence interval using the ``simple`` plot layout."----
superbPlot.simple <- function(
    summarydata, xvar, groupingfac, addfactors, Debug, rawdata 
) {
    plot <- ggplot(
        data = summarydata, 
        mapping = aes_string( x = xvar, y = "center")
    ) +
    geom_point( ) +
    geom_errorbar( mapping = aes_string(ymin = "center + lowerwidth", ymax = "center + upperwidth")  )+ 
    facet_grid( addfactors )
        
    return(plot)
}
superbPlot(TMB1964r,
     WSFactor = "T(7)",      
     BSFactor = "Condition",
     variables = c("T1","T2","T3","T4","T5","T6","T7"),
     plotStyle = "simple", Quiet = TRUE, Debug=FALSE
)

## -----------------------------------------------------------------------------
superbPlot.simple <- function(
    summarydata, xvar, groupingfac, addfactors, Debug, rawdata 
) {
    plot <- ggplot(
        data = summarydata, 
        mapping = aes_string( x = xvar, y = "center")
    ) +
    geom_point( ) +
    geom_errorbar( mapping = aes_string(ymin = "center + lowerwidth", ymax = "center + upperwidth")  )+ 
    facet_grid( addfactors )
        
    return(plot)
}

## -----------------------------------------------------------------------------
superb:::is.superbPlot.function("superbPlot.simple")

## ---- message=FALSE, echo=TRUE, results='hide', fig.show='hide'---------------
superbPlot(TMB1964r,
     WSFactor = "T(7)",      
     BSFactor = "Condition",
     variables = c("T1","T2","T3","T4","T5","T6","T7"),
     plotStyle = "simple", Quiet = TRUE
)

## ---- eval = FALSE, message=FALSE, echo=FALSE, error=FALSE, results='hide'----
#      do.call( geom_point, modifyList(
#         list( size= 3 ##etc., the default directives##
#         ), myownParams
#      ))

## -----------------------------------------------------------------------------
superbPlot.simpleWithOptions <- function(
    summarydata, xvar, groupingfac, addfactors, Debug, rawdata,
    myownParams = list()  ## add the optional arguments to the function
) {
    plot <- ggplot(
        data = summarydata, 
        mapping = aes_string( x = xvar, y = "center")
    ) +
    do.call( geom_point, modifyList(
       list( color ="black" ),
        myownParams
    )) + 
    geom_errorbar( mapping = aes_string(ymin = "center + lowerwidth", ymax = "center + upperwidth")  )+ 
    facet_grid( addfactors )
        
    return(plot)
}
superb:::is.superbPlot.function("superbPlot.simpleWithOptions")

## ---- message=FALSE, echo=TRUE, results='hide', fig.show='show', fig.cap="Figure 2: A simple figure with optional arguments"----
superbPlot(TMB1964r,
    WSFactor = "T(7)",      
    BSFactor = "Condition",
    variables = c("T1","T2","T3","T4","T5","T6","T7"),
    plotStyle = "simpleWithOptions", Quiet = TRUE,
    myownParams = list(size=12, color="red") ## here goes the optional arguments
)

## ---- eval=FALSE, message=FALSE, echo=TRUE, results='hide'--------------------
#  runDebug( Debug, "Text to show",
#    c("variable1", "variable2", "etc"),
#    list( var1InTheFct, var2InTheFct, varetcInTheFct)
#  )

## ---- message=FALSE, echo=TRUE, results='hide', fig.show='hide'---------------
superbPlot.empty <- function(
    summarydata, xvar, groupingfac, addfactors, Debug, rawdata 
) {
    runDebug( Debug, "Getting the dataframes",
        c("summary","raw"), list(summarydata, rawdata))

    plot <- ggplot() # an empty plot        
    return(plot)
}
superbPlot(TMB1964r,
     WSFactor = "T(7)",      
     BSFactor = "Condition",
     variables = c("T1","T2","T3","T4","T5","T6","T7"),
     plotStyle = "empty", Quiet = TRUE, Debug = TRUE ## turn on Debug
)

## ---- message=FALSE, echo=TRUE, results='hide', fig.show='hide'---------------
superbPlot.simple(summary, "T", "Condition", ".~.", FALSE, raw)

## -----------------------------------------------------------------------------
# install.packages("emojifont")
library(emojifont)

## -----------------------------------------------------------------------------
superbPlot.smiley <- function(
    summarydata, xvar, groupingfac, addfactors, Debug, rawdata 
) {
    # the early part bears on summary data with variable "center"
    plot <- ggplot(
        data = summarydata, 
        mapping = aes_string(
            x = xvar, y = "center", 
            fill = groupingfac, 
            shape = groupingfac, 
            colour = groupingfac)
    ) +
    geom_point(position = position_dodge(width = .95)) +
    geom_errorbar( width = .6, position = position_dodge(.95), 
              mapping = aes_string(ymin = "center + lowerwidth", ymax = "center + upperwidth") 
    )+ 
    # this part bears on the rawdata only with variable "DV"
    geom_text(data = rawdata, 
              position = position_jitter(0.5),
              family="EmojiOne", label=emoji("smile"), size=3, 
              mapping=aes_string(x=xvar, y="DV", group = groupingfac)
    ) +
    facet_grid( addfactors )
        
    return(plot)
}

## -----------------------------------------------------------------------------
superb:::is.superbPlot.function("superbPlot.smiley")

## -----------------------------------------------------------------------------
superbPlot(TMB1964r,
     WSFactor = "T(7)",      
     BSFactor = "Condition", 
     variables = c("T1","T2","T3","T4","T5","T6","T7"),
     plotStyle = "smiley", Quiet = TRUE
)

