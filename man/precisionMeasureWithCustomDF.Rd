% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functionsStatistical.R
\name{precisionMeasureWithCustomDF}
\alias{precisionMeasureWithCustomDF}
\alias{CIwithDF.mean}
\title{Confidence intervals with custom degree of freedom}
\usage{
CIwithDF.mean(x, gamma = 0.95 )
}
\arguments{
\item{x}{a vector of numbers, the sample data (mandatory);}

\item{gamma}{a vector containing first a confidence level for CI (default 0.95) and
a custom degree of freedom (when unspecified, it uses \code{n-1} where \code{n} is the number of observations in x).}
}
\value{
the confidence interval (CI) where the \code{t} value is based on the custom-set degree of freedom.
}
\description{
The following function computes a confidence interval with
custom degree of freedom. The default is to use N-1 but this number is not
always appropriate. For example, when there are heterogeneous variances,
the confidence interval of the mean should mirror a Welsh test where the
degrees of freedom are altered based on variances. The function \code{CIwithDF.mean()}
accept an arbitrary defined degree of freedom (df).
The df must be combined to the argument \code{gamma} after the confidence level.
}
\details{
See the vignette "Unequal variances, Welch test, Tryon adjustment, and superb"
for an example of use.
}
\examples{
# this will issue a warning as no custom degree of freedom is provided
CIwithDF.mean( c(1,2,3), gamma = 0.90)          
# the confidence interval of the mean for 90\% confidence level
CIwithDF.mean( c(1,2,3), gamma = c(0.90, 1.5) ) # uses 1.5 as df instead of 2.

}
\references{
\insertAllCited{}
}
