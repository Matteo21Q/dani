\name{test.NI.continuous}
\alias{test.NI.continuous}

\title{
  Non-inferiority testing with continuous outcome.
}
\description{
  Functions for testing non-inferiority when the outcome is continuous. It allows for a number of tests on different summary measures: difference of means or ratio of means. 
}
\usage{
  test.NI.continuous(y.control=NULL, y.experim=NULL,  NI.margin, sig.level=0.025, 
                               summary.measure="mean.difference", 
                               formula=NULL, data=NULL, control.level=0,
                               print.out=TRUE, higher.better=TRUE, test.type=NULL,
                               M.boot=2000, bootCI.type="bca", sd.control=NULL, 
                               sd.experim=NULL)  
}
\arguments{
  \item{y.control}{Vector of measurements in the control arm.}
  \item{y.experim}{Vector of measurements in the experimental arm.}
  \item{NI.margin}{Non-inferiority margin, expressed as the specified summary measure.}
  \item{sig.level}{One-sided significance level for testing. Default is 0.025, i.e. 2.5\%.}
  \item{summary.measure}{The population-level summary measure to be estimated, i.e. the scale on which we define the non-inferiority margin. Can be one of "mean.difference" (Difference of Means) or "mean.ratio" (Ratio of Means).}
    \item{formula}{
  The formula for the outcome model. The variable indicating treatment has to be put within brackets and preceded by treat, e.g. treat(treatment).
}
\item{data}{
A data.frame with all data.
}
    \item{control.level}{Defines the control level in the treatment variable when using the formula and data interface. Defaults to 0.}
  \item{print.out}{Logical. If FALSE, no output is printed.}
  \item{higher.better}{Logical. If FALSE, the outcome is considered 
  unfavourable, i.e. higher scores indicate worse outcomes.
  Default is TRUE, i.e. favourable outcome, higher scores indicate better outcomes.}
  \item{test.type}{A character string defining the method to be used for calculation of the confidence interval. For the mean difference, methods available include "Z.test", "t.test" or bootstrap based on 3 different types of confidence intervals: "bootstrap.basic", "bootstrap.bca" or "bootstrap.percentile".
  For the mean ratio, methods available include "Fiellers" test, "lm" (marginalisation after using linear regression) or the three methods using bootstrap: "bootstrap.basic", "bootstrap.bca" or "bootstrap.percentile".
 }
  \item{M.boot}{ Number of bootstrap samples, e.g. for "bootstrap"" and "MUE.parametric.bootstrap" methods.}
      \item{bootCI.type}{
  Method for computing the confidence intervals if using a bootstrap method. It can be either "norm", "basic", "perc" or "bca". Default is "bca", which is the recommended option when possible. 
}
  \item{sd.control}{The assumed standard deviation of the control arm if using a Z test.}
  \item{sd.experim}{The assumed standard deviation of the experimental arm if using a Z test.}
}
\details{
  This is a function to test non-inferiority of an experimental treatment against the active control within a specific NI margin. The margin can be specified on a number of different summary measures, including mean difference, or mean ratio. It is possible to test both with favourable (e.g. cognitive score) or unfavourable (e.g. pain score) outcomes and using a multitude of methods taken from other packages. See the entry on the test.type argument for the specific methods available for each summary measure. 
}

\value{
  The output is a list, containing the estimate, standard error, cofidence interval (two-sided 2*alpha level), Z statistic and p-value. Additionally, a non-inferiority indicator is included and an indicator of whether the p-value was precise or just estimated from the confidence interval using normal approximation.
}

\examples{
  
y0<-rnorm(10,2)
y1<-rnorm(10,2)
NI.m=-0.75
alpha=0.025

set.seed(1)
out5A<-test.NI.continuous(y0, y1, NI.m, alpha, test.type="Z.test", sd.control=1, sd.experim = 1)

  
}