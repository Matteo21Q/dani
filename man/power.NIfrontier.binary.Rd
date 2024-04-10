\name{power.NIfrontier.binary}
\alias{power.NIfrontier.binary}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Function to compute the power for a given sample size for a 2-arm non-inferiority trial designed using non-inferiority frontiers. 
}
\description{
  A function that can be used to do power calculations for a non-inferiority trial with binary outcome designed using non-inferiority frontiers. The trial can aim to use one of several possible analysis methods and summary measures. 
}
\usage{
  power.NIfrontier.binary(p.control.expected, p.experim.target=NULL, NI.frontier, sig.level=0.025,
                                         summary.measure="RD", print.out=TRUE, unfavourable=TRUE, 
                                         n.control, n.experim, n.rep=1000, M.boot=2000, BB.adj=0.0001,
                                         test.type="LRT")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{p.control.expected}{Expected event risk in the control arm.}
\item{p.experim.target}{Target event risk in the experimental arm under which to power the trial.}
  \item{NI.frontier}{Non-inferiority frontier, a function whos eonly input should be the control event risk and that returns the NI margin for that risk expressed as the specified summary measure.}
\item{sig.level}{One-sided significance level for testing. Default is 0.025, i.e. 2.5\%.}
\item{n.control}{The sample size in the control arm for which to estimate power.  }
\item{n.experim}{The sample size in the experimental arm for which to estimate power.  }
  \item{summary.measure}{The population-level summary measure to be estimated, i.e. the scale on which we define the non-inferiority margin. Can be one of "RD" (Risk Difference), "RR" (Risk Ratio), "OR" (Odds Ratio) or "AS" (Arc-Sine difference).}
\item{print.out}{Logical. If FALSE, no output is printed.}
\item{test.type}{A string that indicates the type of test to be assumed for the sample size calculation. Currently, three options are supported: "Wald", "score" and "local".}
\item{unfavourable}{A logical variable. If TRUE, the outcome is considered unfavourable. This is used to check that the NI margin specified is meaningful.}
\item{n.rep}{The number of repetitions of the simulations to estimate power.}
\item{M.boot}{  Number of bootstrap samples if using a bootstrap-based analysis method.}
\item{BB.adj}{Adjustment factor for "Berger.Boos" method.}
}

\details{
  This function estimates power of a two-arm non-inferiority trial designed using non-inferiority frontiers for a given sample size, running a certain number n.rep of simulations under the alternative hypothesis and calculating estimated success rate of the trial with the desired design and analysis methods. 
}
\value{
  The estimated power. On screen, the Monte-Carlo Confidence Interval is printed as well.  
}
\examples{
  
NI.f<-function(p) {
    marg<-ifelse(p<0.15,p, 0.15)
    return(marg)
}
power<-power.NIfrontier.binary(p.control.expected=0.2, p.experim.target=0.2, NI.frontier=NI.f, n.control=200, n.experim=200,
                               n.rep=500)
  
}
