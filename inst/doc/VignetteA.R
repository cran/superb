## ---- echo = FALSE, message = FALSE, results = 'hide'-------------------------
cat("this will be hidden; use for general initializations.\n")
library(superb)
library(ggplot2)
options(superb.feedback = c('design','warnings') )

## ---- eval=FALSE, message=FALSE, warning=FALSE--------------------------------
#  library(ggplot2)            # for the graphing commands
#  library(superb)             # for superbPlot and GRD
#  library(referenceIntervals) # for computing reference intervals

## ---- eval=TRUE, message=FALSE, warning=FALSE, echo=FALSE---------------------
# the above pretend that referenceInterval was installed, but it is not installed
# because its dependencies (gWidgets2tcltk, extremeValues) crashes travis-CI.com and shinyapps.io...
# I reproduce the code here as is
library(ggplot2)            # for the graphing commands
library(superb)             # for superbPlot and GRD
library(boot)
library(car)
library(stats)
horn.outliers = function (data)
{
#   This function implements Horn's algorithm for outlier detection using
#   Tukey's interquartile fences.

	boxcox = car::powerTransform(data);
	lambda = boxcox$lambda;
	transData = data^lambda;
    descriptives = summary(transData);
    Q1 = descriptives[[2]];
    Q3 = descriptives[[5]];
    IQR = Q3 - Q1;

	out = transData[transData <= (Q1 - 1.5*IQR) | transData >= (Q3 + 1.5*IQR)];
	sub = transData[transData > (Q1 - 1.5*IQR) & transData < (Q3 + 1.5*IQR)];

    return(list(outliers = out^(1/lambda), subset = sub^(1/lambda)));
}
refLimit = function(data, out.method = "horn", out.rm = FALSE, RI = "p", CI = "p",
					refConf = 0.95, limitConf = 0.90, bootStat = "basic"){

	cl = class(data);
	if(cl == "data.frame"){
		frameLabels = colnames(data);
		dname = deparse(substitute(data));
		result = lapply(data, singleRefLimit, dname, out.method, out.rm, RI, CI, refConf, limitConf, bootStat);
		for(i in 1:length(data)){
			result[[i]]$dname = frameLabels[i];
		}
		class(result) = "interval";
	}
	else{
		frameLabels = NULL;
		dname = deparse(substitute(data));
		result = singleRefLimit(data, dname, out.method, out.rm, RI, CI, refConf, limitConf, bootStat);
	}

	return(result);
}
singleRefLimit = function(data, dname = "default", out.method = "horn", out.rm = FALSE,
						RI = "p", CI = "p", refConf = 0.95, limitConf = 0.90, bootStat = "basic")
{
#	This function determines a reference interval from a vector of data samples.
#	The default is a parametric calculation, but other options include a non-parametric
#	calculation of reference interval with bootstrapped confidence intervals around the
#	limits, and also the robust algorithm for calculating the reference interval with
#	bootstrapped confidence intervals of the limits.

	if(out.method == "dixon"){
		output = dixon.outliers(data);
	}
	else if(out.method == "cook"){
		output = cook.outliers(data);
	}
	else if(out.method == "vanderLoo"){
		output = vanderLoo.outliers(data);
	}
	else{
		output = horn.outliers(data);
	}
	if(out.rm == TRUE){
		data = output$subset;
	}

  if(!bootStat %in% c("basic", "norm", "perc", "stud", "bca")) {
		bootStat = "basic";
	}

	outliers = output$outliers;
  n = length(data);
  mean = mean(data, na.rm = TRUE);
  sd = sd(data, na.rm = TRUE);
  norm = NULL;

#	Calculate a nonparametric reference interval.
    if(RI == "n"){

    	methodRI = "Reference Interval calculated nonparametrically";

        data = sort(data);
		holder = nonparRI(data, indices = 1:length(data), refConf);
        lowerRefLimit = holder[1];
        upperRefLimit = holder[2];
        if(CI == "p"){
        	CI = "n";
        }
    }

#	Calculate a reference interval using the robust algorithm method.
    if(RI == "r"){

    	methodRI = "Reference Interval calculated using Robust algorithm";

        holder = robust(data, 1:length(data), refConf);
        lowerRefLimit = holder[1];
        upperRefLimit = holder[2];
        CI = "boot";
    }

#	Calculate a reference interval parametrically, with parametric confidence interval
#	around the limits.
    if(RI == "p"){

#		http://www.statsdirect.com/help/parametric_methods/reference_range.htm
#		https://en.wikipedia.org/wiki/Reference_range#Confidence_interval_of_limit

		methodRI = "Reference Interval calculated parametrically";
		methodCI = "Confidence Intervals calculated parametrically";

		refZ = qnorm(1 - ((1 - refConf) / 2));
		limitZ = qnorm(1 - ((1 - limitConf) / 2));

		lowerRefLimit = mean - refZ * sd;
		upperRefLimit = mean + refZ * sd;
        se = sqrt(((sd^2)/n) + (((refZ^2)*(sd^2))/(2*n)));
        lowerRefLowLimit = lowerRefLimit - limitZ * se;
        lowerRefUpperLimit = lowerRefLimit + limitZ * se;
        upperRefLowLimit = upperRefLimit - limitZ * se;
        upperRefUpperLimit = upperRefLimit + limitZ * se;

        shap_normalcy = shapiro.test(data);
        shap_output = paste(c("Shapiro-Wilk: W = ", format(shap_normalcy$statistic,
        					digits = 6), ", p-value = ", format(shap_normalcy$p.value,
        					digits = 6)), collapse = "");
        ks_normalcy = suppressWarnings(ks.test(data, "pnorm", m = mean, sd = sd));
        ks_output = paste(c("Kolmorgorov-Smirnov: D = ", format(ks_normalcy$statistic,
        					digits = 6), ", p-value = ", format(ks_normalcy$p.value,
        					digits = 6)), collapse = "");
        if(shap_normalcy$p.value < 0.05 | ks_normalcy$p.value < 0.05){
        	norm = list(shap_output, ks_output);

        }
        else{
        	norm = list(shap_output, ks_output);
        }
    }

#	Calculate confidence interval around limits nonparametrically.
    if(CI == "n"){

    	if(n < 120){
    		cat("\nSample size too small for non-parametric confidence intervals,
    		bootstrapping instead\n");
    		CI = "boot";
    	}
    	else{

    		methodCI = "Confidence Intervals calculated nonparametrically";

    		ranks = nonparRanks[which(nonparRanks$SampleSize == n),];
  		  	lowerRefLowLimit = data[ranks$Lower];
    		lowerRefUpperLimit = data[ranks$Upper];
    		upperRefLowLimit = data[(n+1) - ranks$Upper];
    		upperRefUpperLimit = data[(n+1) - ranks$Lower];
		}
    }

#	Calculate bootstrapped confidence intervals around limits.
	if(CI == "boot" & (RI == "n" | RI == "r")){

		methodCI = "Confidence Intervals calculated by bootstrapping, R = 5000";

		if(RI == "n"){
			bootresult = boot::boot(data = data, statistic = nonparRI, refConf = refConf, R = 5000);
		}
		if(RI == "r"){
			bootresult = boot::boot(data = data, statistic = robust, refConf = refConf, R = 5000);
		}
    	bootresultlower = boot::boot.ci(bootresult, conf = limitConf, type=bootStat, index = c(1,2));
    	bootresultupper = boot::boot.ci(bootresult, conf = limitConf, type=bootStat, index = c(2,2));
			bootresultlength = length(bootresultlower[[4]]);
    	lowerRefLowLimit = bootresultlower[[4]][bootresultlength - 1];
    	lowerRefUpperLimit = bootresultlower[[4]][bootresultlength];
    	upperRefLowLimit = bootresultupper[[4]][bootresultlength - 1];
    	upperRefUpperLimit = bootresultupper[[4]][bootresultlength];
    }

    RVAL = list(size = n, dname = dname, out.method = out.method, out.rm = out.rm,
    			outliers = outliers, methodRI = methodRI, methodCI = methodCI,
    			norm = norm, refConf = refConf, limitConf = limitConf,
    			Ref_Int = c(lowerRefLimit = lowerRefLimit, upperRefLimit = upperRefLimit),
    			Conf_Int = c(lowerRefLowLimit = lowerRefLowLimit,
    						lowerRefUpperLimit = lowerRefUpperLimit,
        					upperRefLowLimit = upperRefLowLimit,
        					upperRefUpperLimit = upperRefUpperLimit));
    class(RVAL) = "interval";
    return(RVAL);
}



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

