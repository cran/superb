## ---- message=FALSE, echo=TRUE, eval=TRUE-------------------------------------
## Load relevant packages
library(superb)             # for superbPlot
library(ggplot2)            # for all the graph directives
library(grid)               # for grid.arrange
library(gridExtra)          # for grid.text

## ---- message=FALSE, echo=TRUE, eval=TRUE-------------------------------------
head(dataFigure1)

## ---- message=FALSE, echo=TRUE, eval=TRUE, fig.width=3, fig.height=4, fig.cap="Figure 1a: Making left panel of Figure 1."----
plt1a = superbPlot(dataFigure1, 
            BSFactors   = "grp", 
            variables   = "score", 
            plotStyle   = "line" ) 
plt1a

## ---- message=FALSE, echo=TRUE, eval=TRUE-------------------------------------
ornateBS = list(
    xlab("Group"), 
    ylab("Attitude towards class activities"),
    scale_x_discrete(labels=c("Collaborative\ngames", "Unstructured\nactivities")), #new!
    coord_cartesian( ylim = c(75,125) ),
    geom_hline(yintercept = 100, colour = "black", size = 0.5, linetype=2),
    theme_light(base_size = 14) +
    theme( plot.subtitle = element_text(size=12))
)

## ---- message=FALSE, echo=TRUE, eval=TRUE, fig.width=3, fig.height=4, fig.cap="Figure 1b: Decorating left panel of Figure 1."----
plt1a <- plt1a + ornateBS + labs(subtitle="(stand-alone)\n95% CI")
plt1a

## ---- message=FALSE, echo=TRUE, eval=TRUE, fig.width=3, fig.height=4, fig.cap="Figure 1c: Making and decorating central panel of Figure 1."----
plt1b = superbPlot(dataFigure1, 
            BSFactors   = "grp", 
            variables   = "score", 
            adjustments = list(purpose = "difference"), #new!
            plotStyle   = "line" )
plt1b <- plt1b + ornateBS + labs(subtitle="Difference-adjusted\n95% CI") 
plt1b

## ---- message=FALSE, echo=TRUE, eval=TRUE, fig.width=3, fig.height=4, fig.cap="Figure 1c: Making and decorating right panel of Figure 1."----
    plt1c = superbPlot(dataFigure1, 
                BSFactors   = "grp", 
                variables   = "score", 
                adjustments = list(purpose = "difference"),
                plotStyle   = "raincloud",                         # new layout!
                violinParams = list(fill = "green", alpha = 0.2) ) # added some color to the violin
    plt1c <- plt1c + ornateBS + labs(subtitle="Difference-adjusted\n95% CI") 
    plt1c

## ---- message=FALSE, echo=TRUE, eval=TRUE, fig.width=9, fig.height=4, fig.cap="Figure 1: The complete Figure 1."----
grid.arrange(plt1a, plt1b, plt1c, ncol=3)

## ---- message=FALSE, echo=TRUE, eval=FALSE------------------------------------
#  png(filename = "Figure1.png", width = 640, height = 320)
#  grid.arrange(plt1a, plt1b, plt1c, ncol=3)
#  dev.off()

## ---- message=FALSE, echo=TRUE, eval=TRUE-------------------------------------
t.test(dataFigure1$score[dataFigure1$grp==1],
        dataFigure1$score[dataFigure1$grp==2], var.equal=TRUE)

## ---- message=FALSE, echo=TRUE, eval=TRUE-------------------------------------
ornateWS = list(
    xlab("Moment"),                                                #different!
    scale_x_discrete(labels=c("Pre\ntreatment", "Post\ntreatment")), #new!
    ylab("Statistics understanding"),
    coord_cartesian( ylim = c(75,125) ),
    geom_hline(yintercept = 100, colour = "black", size = 0.5, linetype=2),
    theme_light(base_size = 16) +
    theme( plot.subtitle = element_text(size=12))
)

## -----------------------------------------------------------------------------
head(dataFigure2)

## ---- message=FALSE, echo=TRUE, eval=TRUE, fig.width=3, fig.height=4, fig.cap="Figure 2a: Making left panel of Figure 2."----
plt2a <- superbPlot(dataFigure2, 
            WSFactors    = "Moment(2)", 
            variables   = c("pre","post"), 
            adjustments = list(purpose = "single"),
            plotStyle   = "line" ) 
plt2a <- plt2a + ornateWS + labs(subtitle="Stand-alone\n95% CI")
plt2a

## ---- message=TRUE, echo=TRUE, eval=TRUE, fig.width=3, fig.height=4, fig.cap="Figure 2b: Making central panel of Figure 2."----
plt2b <- superbPlot(dataFigure2, 
            WSFactors    = "Moment(2)", 
            variables   = c("pre","post"), 
            adjustments = list(purpose = "difference", decorrelation = "CA"), #new
            plotStyle   = "line" ) 
plt2b <- plt2b + ornateWS + labs(subtitle="Correlation and difference-\nadjusted 95% CI") 
plt2b

## ---- message=FALSE, echo=TRUE, eval=TRUE, fig.width=3, fig.height=4, fig.cap="Figure 2c: Making third panel of Figure 2."----
plt2c <- superbPlot(dataFigure2, 
            WSFactors    = "Moment(2)", 
            variables   = c("pre","post"), 
            adjustments = list(purpose = "difference", decorrelation = "CA"),
            plotStyle   = "pointindividualline" )   #new
plt2c <- plt2c + ornateWS + labs(subtitle="Correlation and difference-\nadjusted 95% CI")
plt2c 

## -----------------------------------------------------------------------------
ornateWS2 = list(
    xlab("Difference"),                                      #different!
    scale_x_discrete(labels=c("Post minus Pre\ntreatment")), #new!
    ylab("Statistics understanding"),
    coord_cartesian( ylim = c(-25,+25) ),
    geom_hline(yintercept = 0, colour = "black", size = 0.5, linetype=2),
    theme_light(base_size = 16) +
    theme( plot.subtitle = element_text(size=12))
)

## ---- message=FALSE, echo=TRUE, eval=TRUE, fig.width=3, fig.height=4, fig.cap="Figure 2d: Making right panel of Figure 2."----
dataFigure2$diff = dataFigure2$post - dataFigure2$pre
plt2d <- superbPlot(dataFigure2, 
            WSFactor    = "Moment(1)", 
            variables   = c("diff"), 
            adjustments = list(purpose = "single", decorrelation = "none"),
            plotStyle   = "raincloud",
            violinParams = list(fill = "green") )  #new
plt2d <- plt2d + ornateWS2 + labs(subtitle="95% CI \nof the difference")
plt2d

## ---- message=FALSE, echo=TRUE, eval=TRUE, fig.width=9, fig.height=4, fig.cap="Figure 2: The complete Figure 2."----
grid.arrange(plt2a, plt2b, plt2c, plt2d,  ncol=4)

## ---- message=FALSE, echo=TRUE, eval=FALSE------------------------------------
#  png(filename = "Figure2.png", width = 850, height = 320)
#  grid.arrange(plt2a, plt2b, plt2c, plt2d,  ncol=4)
#  dev.off()

## ---- message=FALSE, echo=TRUE, eval=TRUE-------------------------------------
t.test(dataFigure2$pre, dataFigure2$post, paired=TRUE)

## ---- message=FALSE, echo=TRUE, eval=TRUE-------------------------------------
ornateCRS = list(
    xlab("Group"), 
    ylab("Quality of policies"),
    scale_x_discrete(labels=c("From various\nfields", "From the\nsame field")), #new!
    coord_cartesian( ylim = c(75,125) ),
    geom_hline(yintercept = 100, colour = "black", size = 0.5, linetype=2),
    theme_light(base_size = 14) +
    theme( plot.subtitle = element_text(size=12))
)

## ---- message=FALSE, echo=TRUE, eval=TRUE, fig.width=8, fig.height=4, fig.cap="Figure 3a: The left panel of Figure 3."----
plt3a = superbPlot(dataFigure3, 
    BSFactors    = "grp", 
    variables   = "VD", 
    adjustments = list(purpose = "single", samplingDesign = "SRS"),
    plotStyle   = "line" )
plt3a <- plt3a + ornateCRS + labs(subtitle="Stand-alone\n95% CI") 

## ---- message=TRUE, echo=TRUE, eval=TRUE, fig.width=8, fig.height=4, fig.cap="Figure 3b: The central panel of Figure 3."----
plt3b <- superbPlot(dataFigure3, 
    BSFactors    = "grp", 
    variables   = "VD", 
    adjustments = list(purpose = "difference", samplingDesign = "CRS"), #new
    plotStyle   = "line", 
    clusterColumn = "cluster" )                                       #new
plt3b <- plt3b + ornateCRS + labs(subtitle="Cluster and difference-\nadjusted 95% CI")

## ---- message=FALSE, echo=TRUE, eval=TRUE, fig.width=8, fig.height=4, fig.cap="Figure 3c: The right panel of Figure 3."----
plt3c <- superbPlot(dataFigure3, 
    BSFactors    = "grp", 
    variables   = "VD", 
    adjustments = list(purpose = "difference", samplingDesign = "CRS"),
    plotStyle   = "raincloud", 
    violinParams = list(fill = "green", alpha = 0.2),
    clusterColumn = "cluster" )
plt3c <- plt3c + ornateCRS + labs(subtitle="Cluster and difference-\nadjusted 95% CI")

## ---- message=FALSE, echo=TRUE, eval=TRUE, fig.width=8, fig.height=4, fig.cap="Figure 3: The complete Figure 3."----
grid.arrange(plt3a, plt3b, plt3c, ncol=3)

## ---- message=FALSE, echo=TRUE, eval=FALSE------------------------------------
#  png(filename = "Figure3.png", width = 640, height = 320)
#  grid.arrange(plt3a, plt3b, plt3c, ncol=3)
#  dev.off()

## ---- message=FALSE, echo=TRUE, eval=TRUE-------------------------------------
res    <- t.test( dataFigure3$VD[dataFigure3$grp==1], 
                  dataFigure3$VD[dataFigure3$grp==2], 
                  var.equal=TRUE)
micc   <- mean(c(0.491335, 0.203857)) # mean ICCs per group, as given by superbPlot
lambda <- CousineauLaurencelleLambda(c(micc, 5, 5, 5, 5, 5, 5))
tcorr  <- res$statistic / lambda
pcorr  <- 1-pt(tcorr,4)

cat(paste("t-test corrected for cluster-randomized sampling: t(",
    2*(dim(dataFigure3)[1]-2),") = ", round(tcorr,3),
    ", p = ", round(pcorr,3),"\n", sep= ""))

## ---- message=FALSE, echo=TRUE, eval=TRUE-------------------------------------
ornateBS = list(
    xlab(""), 
    ylab("Metabolic score"),
    scale_x_discrete(labels=c("Response to treatment")), #new!
    coord_cartesian( ylim = c(75,125) ),
    geom_hline(yintercept = 100, colour = "black", size = 0.5, linetype=2),
    theme_light(base_size = 16) +
    theme( plot.subtitle = element_text(size=12))
)

## ---- message=FALSE, echo=TRUE, eval=TRUE, fig.width=3, fig.height=4, fig.cap="Figure 4a: The left panel of Figure 4."----
plt4a <- superbPlot(dataFigure4, 
    BSFactors = "group", 
    variables = "score", 
    adjustments=list(purpose = "single", popSize = Inf),
    plotStyle="line" ) 
plt4a <- plt4a + ornateBS + labs(subtitle="Stand-alone\n95% CI") 

## ---- message=FALSE, echo=TRUE, eval=TRUE, fig.width=3, fig.height=4, fig.cap="Figure 4b: The central panel of Figure 3b."----
plt4b <- superbPlot(dataFigure4, 
    BSFactors = "group",
    variables = "score", 
    adjustments=list(purpose = "single", popSize = 50 ), # new!
    plotStyle="line" ) 
plt4b <- plt4b + ornateBS + labs(subtitle="Population size-\nadjusted 95% CI") 

## ---- message=FALSE, echo=TRUE, eval=TRUE, fig.width=3, fig.height=4, fig.cap="Figure 4c: The right panel of Figure 3b."----
plt4c <- superbPlot(dataFigure4, 
    BSFactors = "group",
    variables = "score", 
    adjustments=list(purpose = "single", popSize = 50 ), # new!
    plotStyle="pointjitterviolin",
    violinParams = list(fill = "green", alpha = 0.2)  ) 
plt4c <- plt4c + ornateBS + labs(subtitle="Population size-\nadjusted 95% CI") 

## ---- message=FALSE, echo=TRUE, eval=TRUE, fig.width=9, fig.height=4, fig.cap="Figure 4: The complete Figure 4."----
plt4 = grid.arrange(plt4a, plt4b, plt4c, ncol=3)

## ---- message=FALSE, echo=TRUE, eval=FALSE------------------------------------
#  png(filename = "Figure4.png", width = 640, height = 320)
#  grid.arrange(plt4a, plt4b, plt4c, ncol=3)
#  dev.off()

## ---- message=FALSE, echo=TRUE, eval=TRUE-------------------------------------
res = t.test(dataFigure4$score, mu=100)
tcorr = res$statistic /sqrt(1-nrow(dataFigure4)/50)
pcorr = 1-pt(tcorr,24)

cat(paste("t-test corrected for finite-population size: t(",
    nrow(dataFigure4)-1,") = ", round(tcorr,3),
    ", p = ", round(pcorr,3),"\n", sep= ""))

## ---- echo = FALSE, message = FALSE, results = 'hide'-------------------------
cat("this will be hidden until I decide or not to put this here.\n")

######################################################
## Make figure 5
######################################################

plt5a = superbPlot(dataFigure1, BSFactors = "grp", 
    statistic = "var",
    adjustments=list(purpose = "difference"),
    variables = c("score"), plotStyle="line" ) + 
  xlab("Group") + ylab("Variance in scores") + 
  labs(title="Difference-adjusted 95% CI\nof the variance") +
  coord_cartesian( ylim = c(0,250) ) +
  geom_hline(yintercept = 100, colour = "black", size = 0.5, linetype=2)
plt5b = superbPlot(dataFigure1, BSFactors = "grp",
    statistic = "fisherskew", 
    adjustments=list(purpose = "difference"),
    variables = c("score"), plotStyle="line" ) + 
  xlab("Group") + ylab("Skew of the scores") + 
  labs(title="Difference-adjusted 95% CI\nof the Fisher skew") +
  coord_cartesian( ylim = c(-2,+2) ) + 
  geom_hline(yintercept = 0, colour = "black", size = 0.5, linetype=2)
plt5 = grid.arrange(plt5a,plt5b,ncol=2)


######################################################
## Make figure 6
######################################################

grp1 = dataFigure1$score[dataFigure1$grp==1] # extract group 1 for later checks
grp2 = dataFigure1$score[dataFigure1$grp==2] # extract group 2 for later checks

# defines Cohen's d_1
d1 = function(X) {
  mean(X-100)/sd(X) 
} 
# quick checks
d1(grp1)
d1(grp2)

CI.d1 <- function(X, gamma = .95) {
    n      <- length(X)
    lambda <- d1(X) * sqrt( n ) # the noncentrality parameter
    tlow   <- qt(1/2 - gamma/2, df = n-1, ncp = lambda )
    thig   <- qt(1/2 + gamma/2, df = n-1, ncp = lambda )
    dlow   <- tlow / sqrt(n)
    dhig   <- thig / sqrt(n) 
    c(dlow, dhig)
}    
# quick checks
CI.d1(grp1)  
CI.d1(grp2)


plt6a <- superbPlot(dataFigure1, BSFactors = "grp", 
  statistic = "mean", errorbar = "CI",
  showPlot=T, plotStyle="line",
  adjustments = list(purpose = "difference"),
  variable = c("score") 
)+ ylab("Mean +- 95% CI") +
labs(title="Mean with difference adjusted\n95% confidence interval")

plt6b <- superbPlot(dataFigure1, BSFactors = "grp", 
  statistic = "d1", errorbar = "CI",
  showPlot=T, plotStyle="line",
  postprocessfct = "poolSDTransform",
  adjustments = list(purpose = "difference"),
  variable = c("score") 
)+ ylab("Cohen's d_1 +- 95% CI") +
labs(title="Cohen's d_1 with difference adjusted\n95% confidence interval") + 
annotation_custom(grid.text("Cohen's d_p = 0.49\n95% CI = [-0.06, +1.09]",
    x=.66,y=.75,gp=gpar(fontsize=12, col="black"),
    rot = 0)) 
plt6 <- grid.arrange(plt6a, plt6b, ncol=2)


