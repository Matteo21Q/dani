\name{power.NI.continuous}
\alias{power.NI.continuous}
\title{
  Power calculation tool for Non-Inferiority trials with a continuous outcome.
}
\description{
  A function for estimating power of a non-inferiority trial whose primary outcome is continuous. Allows for different summary measures, test types and both favourable (e.g. cure) and unfavourable (e.g. death) events. 
}
\usage{
  power.NI.continuous(mean.control, mean.experim, sd, NI.margin, sig.level = 0.025, 
                                      n.control, n.experim, summary.measure = "mean.difference", print.out = TRUE, 
                                      test.type=NULL, higher.better=T, M.boot=2000, n.rep=1000)   
}
\arguments{
  \item{mean.control}{Assumed mean in the control arm.}
  \item{mean.experim}{target mean in the experimental arm for powering the trial.}
  \item{sd}{Standard deviation in both arms.}
  \item{NI.margin}{Non-inferiority margin. Can be either efined as a mean difference, or mean ratio.}
  \item{sig.level}{One-sided significance level for testing. Default is 0.025, i.e. 2.5\%.}
\item{n.control}{The sample size in the control arm for which to estimate power.  }
\item{n.experim}{The sample size in the experimental arm for which to estimate power.  }
  \item{summary.measure}{The population-level summary measure to be estimated, i.e. the scale on which we define the non-inferiority margin. Can be one of "mean.difference" (Difference of Means) or "mean.ratio" (Ratio of Means).}
  \item{print.out}{Logical. If FALSE, no output is printed.}
  \item{higher.better}{Logical. If FALSE, the outcome is considered 
    unfavourable, i.e. higher scores indicate worse outcomes.
    Default is TRUE, i.e. favourable outcome, higher scores indicate better outcomes.}
  \item{test.type}{A character string defining the method to be used for sampel size calculations. For the mean difference, methods available include "Z.test" and "t.test".
    For the mean ratio, methods available include "Fiellers" test, "log.t.test" or "log.Z.test".
  }
 \item{n.rep}{The number of repetitions of the simulations to estimate power.}
\item{M.boot}{  Number of bootstrap samples if using a bootstrap-based analysis method.} 
}
\details{
  This is a function to estimate through simulations the power of a fixed sample size to test non-inferiority of an active treatment against the control within a specific NI margin. The margin can be specified on a number of different scales. 
}

\value{
  The output is an estimate of power, and on screen the Monte-Carlo CI for this estimate may be printed as well.
}

\examples{
  
power<-power.NI.continuous(mean.control=2, mean.experim=2, sd=1, NI.margin=-1, n.control=20, n.experim=20, n.rep=500)
  
}