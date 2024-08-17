## ----echo = FALSE, warning=FALSE, message = FALSE, results = 'hide'-----------
cat("this will be hidden; use for general initializations.\n")
library(superb)
options("superb.feedback" = c("warnings"))
library(ggplot2)

## -----------------------------------------------------------------------------
fmri <- read.csv(url("https://raw.githubusercontent.com/mwaskom/seaborn-data/de49440879bea4d563ccefe671fd7584cba08983/fmri.csv"))

## -----------------------------------------------------------------------------
# sort the data...
fmri <- fmri[order(fmri$subject, fmri$event, fmri$region, fmri$timepoint),]

#... then convert to wide
fmriWide <- superbToWide(fmri, id="subject", 
    WSFactors = c("event","region","timepoint"), 
    variable = "signal")

## ----fig.height=4, fig.width=7, fig.cap = "**Figure 1**. Plot of the fMRI data with standalone confidence intervals."----
superbPlot( fmriWide,
  WSFactors = c("timepoint(19)","region(2)","event(2)"),
  variables = names(fmriWide)[2:77],
  plotStyle = "lineBand",
  pointParams = list(size=1,color="black"),
  lineParams = list(color="purple")
) + scale_x_discrete(name="Time", labels = 0:18) + 
scale_discrete_manual(aesthetic =c("fill","colour"), 
                      labels = c("frontal","parietal"), 
                      values = c("red","green")) +
theme_bw() 

## ----fig.height=4, fig.width=7, fig.cap = "**Figure 2**. Plot of the fMRI data with Cousineau-Morey decorrelation."----
superbPlot( fmriWide,
  WSFactors = c("timepoint(19)","region(2)","event(2)"),
  variables = names(fmriWide)[2:77],
  adjustments = list(decorrelation = "CM"), ## only new line
  plotStyle = "lineBand",
  pointParams = list(size=1,color="black"),
  lineParams = list(color="purple")
) + scale_x_discrete(name="Time", labels = 0:19) + 
scale_discrete_manual(aesthetic =c("fill","colour"), 
                      labels = c("frontal","parietal"), 
                      values = c("red","green"))+
theme_bw() + 
showSignificance(c(6,7)+1, 0.3, -0.02, "n.s.?", panel=list(event=2)) 


## ----fig.height=4, fig.width=7, fig.cap = "**Figure 3**. Plot of the fMRI data with local decorrelation."----
superbPlot( fmriWide,
  WSFactors = c("timepoint(19)","region(2)","event(2)"),
  variables = names(fmriWide)[2:77],
  adjustments = list(decorrelation = "LD2"),  ## CM replaced with LD2
  plotStyle = "lineBand",
  pointParams = list(size=1,color="black"),
  lineParams = list(color="purple")
) + scale_x_discrete(name="Time", labels = 0:19) + 
scale_discrete_manual(aesthetic =c("fill","colour"), 
                      labels = c("frontal","parietal"), 
                      values = c("red","green"))+
theme_bw() + 
showSignificance(c(6,7)+1, 0.3, -0.02, "**!", panel=list(event=2)) 


## -----------------------------------------------------------------------------
t.test(fmriWide$`signal_stim_parietal_ 6`, fmriWide$`signal_stim_parietal_ 7`, paired=TRUE)

