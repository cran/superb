## ---- echo = FALSE, message = FALSE, results = 'hide'-------------------------
cat("this will be hidden; use for general initializations.")
library(superb)
library(ggplot2)

## -----------------------------------------------------------------------------
#Motivation data for 15 participants over three weeks in wide format:
dta <- matrix( c(
    45, 50,  59,
    47, 58,  64,
    53, 63,  72,
    57, 64,  81,
    58, 67,  86,
    61, 70,  98,
    61, 75, 104,
    63, 79, 100,
    63, 79,  84,
    71, 81,  96,
    72, 83,  82,
    74, 84,  82,
    76, 86,  93,
    84, 90,  85,
    90, 96,  89
), ncol=3, byrow=T)

# put column names then convert to data.frame:
colnames(dta) <- c("Week 1", "Week 2", "Week 3")
dta           <- as.data.frame(dta)

## ---- message=FALSE, echo=FALSE, fig.cap="Figure 1: Mean scores along with 95% confidence interval per week for a program to stop smoking."----
superbPlot(dta, WSFactor="Moment(3)",
        variables = c("Week 1", "Week 2", "Week 3"),
        statistic = "mean", errorbar = "CI",
        adjustments = list(purpose = "single", decorrelation = "none"),
        plotStyle="line",
        errorbarParams = list(width = .2)
    ) + 
    coord_cartesian( ylim = c(50,100) ) +
    ylab("Mean +- 95% CI") +
    labs(title="(stand-alone)\n95% confidence interval\n")+
    theme_gray(base_size=16) +
    scale_x_discrete(labels=c("1" = "Week 1", "2" = "Week 2", "3"="Week 3"))

## ---- message=FALSE, echo=TRUE, results='hide', fig.show='hide'---------------
superbPlot(dta, 
    WSFactor="Moment(3)",
    variables = c("Week 1", "Week 2", "Week 3"),
    adjustments = list(purpose = "difference"),
    plotStyle="line"
    )

## ---- message=FALSE, echo=FALSE, , fig.cap="Figure 2: Mean scores along with difference-adjusted 95% confidence interval per week for a program to stop smoking."----
superbPlot(dta, 
    WSFactor="Moment(3)",
    variables = c("Week 1", "Week 2", "Week 3"),
    statistic = "mean", errorbar = "CI",
    adjustments = list(purpose = "difference"),
    plotStyle="line",
    errorbarParams = list(width = .2)
) + 
coord_cartesian( ylim = c(50,100) ) +
ylab("Mean +- 95% CI") +
labs(title="Difference-adjusted\n95% confidence interval\n")+
theme_gray(base_size=16) +
scale_x_discrete(labels=c("1" = "Week 1", "2" = "Week 2", "3"="Week 3"))

## -----------------------------------------------------------------------------
head(dta)

## ---- message=FALSE, echo=TRUE, results='hide', fig.show='hide'---------------
superbPlot(dta, 
    WSFactor="Moment(3)",
    variables = c("Week 1", "Week 2", "Week 3"),
    statistic = "mean", errorbar = "CI",
    adjustments = list(purpose = "difference"),
    plotStyle="line",
    errorbarParams = list(width = .2)
) + 
coord_cartesian( ylim = c(50,100) ) +
ylab("Mean +- 95% CI") +
labs(title="Difference-adjusted\n95% confidence interval\n")+
theme_gray(base_size=16) +
scale_x_discrete(labels=c("1" = "Week 1", "2" = "Week 2", "3"="Week 3"))

## ---- message=FALSE, echo=TRUE------------------------------------------------
superbPlot(dta, 
    WSFactor="Moment(3)",
    variables = c("Week 1", "Week 2", "Week 3"),
    statistic = "mean", errorbar = "CI",
    adjustments = list(purpose = "difference", decorrelation = "CM"),
    plotStyle="line",
    errorbarParams = list(width = .2)
) + 
coord_cartesian( ylim = c(50,100) ) +
ylab("Mean +- 95% CI") +
labs(title="Correlation- and Difference-adjusted\n95% confidence interval\n")+
theme_gray(base_size=16) +
scale_x_discrete(labels=c("1" = "Week 1", "2" = "Week 2", "3"="Week 3"))

## ---- message=FALSE, echo=TRUE , results='hide'-------------------------------
# add (ficticious) cluster membership for each participant in the column "cluster"
dta$cluster <- sort(rep(1:5, 3))

superbPlot(dta, 
    WSFactor="Moment(3)",
    variables = c("Week 1", "Week 2", "Week 3"),
    adjustments = list(purpose = "difference", decorrelation = "CM",
                       samplingDesign = "CRS", popSize = 100),
    plotStyle = "line",
    clusterColumn = "cluster",  # identify the column containing cluster membership
    errorbarParams = list(width = .2)
) + 
coord_cartesian( ylim = c(50,100) ) +
ylab("Mean +- 95% CI") +
labs(title="Cluster- Correlation, and Difference-adjusted\n95% confidence interval\n")+
theme_gray(base_size=16) +
scale_x_discrete(labels=c("1" = "Week 1", "2" = "Week 2", "3"="Week 3"))
