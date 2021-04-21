## ---- echo = FALSE, message = FALSE, results = 'hide'-------------------------
cat("this will be hidden; use for general initializations.")
library(superb)
library(ggplot2)

## ---- message=FALSE, echo=TRUE, fig.cap="Figure 1: Various statistics and various measures of precisions"----
# shut down 'warnings', 'design' and 'summary' messages
options(superb.feedback = 'none') 

# Generate a random dataset from a (3 x 2) design, entirely within subject.
# The sample size is very small (n=5) and the correlation between scores is high (rho = .8)
dta <- GRD( 
    WSFactors  = "Moment(3): Dose(2)", 
    Effects    = list("Dose*Moment"=custom(0,0,0,1,1,3)), 
    SubjectsPerGroup = 50, 
    Population = list( mean=10, stddev = 5, rho = .80)
)

# a quick function to call superbPlot
makeplot <- function(statfct, errorbarfct, gam, rg, subttl) {
    superbPlot(dta, 
        WSFactors  = c("Moment(3)","Dose(2)"), 
        variables = c("DV.1.1","DV.2.1","DV.3.1","DV.1.2","DV.2.2","DV.3.2"), 
        statistic = statfct, 
        errorbar  = errorbarfct, 
        gamma     = gam, 
        plotStyle = "line",
        adjustments = list(purpose="difference", decorrelation="CM")
    ) + ylab(subttl) + coord_cartesian( ylim = rg )
} 

p1 <- makeplot("mean",      "CI", .95, c(6,14), "Mean +- 95% CI of the mean")
p2 <- makeplot("mean",      "SE", .00, c(6,14), "Mean +- SE of the mean")
p3 <- makeplot("median",    "CI", .95, c(6,14), "Median +- 95% CI of the median")
p4 <- makeplot("fisherskew","CI", .95, c(-2,+2), "Fisher skew +- 95% CI")

library(gridExtra)
p <- grid.arrange(p1,p2,p3,p4, ncol=2)

## -----------------------------------------------------------------------------
superb:::is.stat.function("mean")

## -----------------------------------------------------------------------------
superb:::is.errorbar.function("SE.mean")

## -----------------------------------------------------------------------------
superb:::is.gamma.required("SE.mean")

## ---- message=FALSE, echo=TRUE------------------------------------------------
    # create a descriptive statistics
    d1 <- function(X) {
      mean(X-100)/sd(X) 
    } 

    # we can test it with the data from group 1...
    grp1 <- dataFigure1$score[dataFigure1$grp==1]
    grp2 <- dataFigure1$score[dataFigure1$grp==2]
    d1(grp1)

    # or check that it is a valid statistic function
    superb:::is.stat.function("d1")

## ---- message=FALSE, echo=TRUE, fig.cap="Figure 2: ``superbPlot`` with a custom-made descriptive statistic function "----
    superbPlot(dataFigure1, 
        BSFactors = "grp", 
        statistic = "d1", errorbar = "none",
        plotStyle="line",
        adjustments = list(purpose = "difference"),
        variable = "score",
        errorbarParams = list(width=0) # so that the null-width error bar is invisible
    )+ ylab("Cohen's d_1") +
    labs(title="d_1 with no error bars") +
    coord_cartesian( ylim = c(-0.5,+1.5) ) 

## -----------------------------------------------------------------------------
    library(sadists)

    CI.d1 <- function(X, gamma = .95) {
        n    <- length(X)
        dlow <- qlambdap(1/2-gamma/2, df = n-1, t = d1(X) * sqrt(n) ) 
        dhig <- qlambdap(1/2+gamma/2, df = n-1, t = d1(X) * sqrt(n) ) 
        c(dlow, dhig) / sqrt(n)
    }

    # we test as an example the data from group 1
    CI.d1(grp1)  

    # or check that it is a valid interval function
    superb:::is.errorbar.function("CI.d1")

## ---- message=FALSE, echo=TRUE, fig.cap="Figure 3: superbPlot with a custom-made descriptive sttistic function "----
    superbPlot(dataFigure1, 
        BSFactors = "grp", 
        statistic = "d1", errorbar = "CI",
        plotStyle="line",
        adjustments = list(purpose = "single"),
        variable = "score"
    )+ ylab("Cohen's d_1") +
    labs(title="d_1 with 95% confidence interval of d_1") +
    coord_cartesian( ylim = c(-0.5,+1.5) ) 

## -----------------------------------------------------------------------------
    # compute the Cohen's dp
    dp <- (mean(grp1)-mean(grp2))/ sqrt((var(grp1)+var(grp2))/2)
    dp

    # get the confidence interval of this 
    lecoutre2007 <- function(dp, n, gamma = .95) {
        dlow <- qlambdap(1/2-gamma/2, df = 2*(n-1), t = dp * sqrt(n/2) ) 
        dhig <- qlambdap(1/2+gamma/2, df = 2*(n-1), t = dp * sqrt(n/2) ) 
        limits <- c(dlow, dhig) / sqrt(n/2)
        limits
    }
    lecoutre2007(dp, length(grp1) )

## -----------------------------------------------------------------------------
    # we define myBootstrapCI which subsample the whole sample, here called X
    myBootstrapCI.mean <- function(X, gamma = 0.95) {
      res = c()
      for (i in 1:10000) {
        res[i] <- mean(sample(X, length(X), replace = T))
      }
      quantile(res, c(1/2 - gamma/2, 1/2 + gamma/2))
    }

    # we check that it is a valid interval function
    superb:::is.errorbar.function("myBootstrapCI.mean")

## ---- message=FALSE, echo=TRUE, fig.cap="Figure 4: superbPlot with a custom-made interval function."----
    plt1 <- superbPlot(dataFigure1, 
        BSFactors = "grp", 
        variable = c("score"), 
        plotStyle="line",
        statistic = "mean", errorbar = "myBootstrapCI",
        adjustments = list(purpose = "difference")
    ) + 
    xlab("Group") + ylab("Score") + 
    labs(title="means and difference-adjusted\n95% confidence intervals") +
    coord_cartesian( ylim = c(85,115) ) +
    theme_gray(base_size=10) +
    scale_x_discrete(labels=c("1" = "Collaborative games", "2" = "Unstructured activity"))

    plt2 <- superbPlot(dataFigure1, 
        BSFactors = "grp", 
        variable = c("score"), 
        plotStyle="line",
        statistic = "mean", errorbar = "CI",
        adjustments = list(purpose = "difference")
    ) + 
    xlab("Group") + ylab("Score") + 
    labs(title="means and difference-adjusted\n95% bootstrap confidence intervals") +
    coord_cartesian( ylim = c(85,115) ) +
    theme_gray(base_size=10) +
    scale_x_discrete(labels=c("1" = "Collaborative games", "2" = "Unstructured activity"))

    library(gridExtra)
    plt <- grid.arrange(plt1, plt2, ncol=2)

