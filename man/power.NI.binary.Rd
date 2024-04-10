\name{power.NI.binary}
\alias{power.NI.binary}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Function to compute the power for a given sample size for a standard 2-arm non-inferiority trial. 
}
\description{
  A function that can be used to do power calculations for a non-inferiority trial with binary outcome. The trial can aim to use one of several possible analysis methods and summary measures. 
}
\usage{
  power.NI.binary(p.control.expected, p.experim.target, NI.margin, sig.level = 0.025, 
                                  n.control, n.experim, summary.measure = "RD", print.out = TRUE, test.type=NULL,
                                  unfavourable=T, n.rep=1000, M.boot=2000, BB.adj=0.0001) 
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{p.control.expected}{Expected event risk in the control arm.}
\item{p.experim.target}{Target event risk in the experimental arm under which to power the trial.}
\item{NI.margin}{Non-inferiority margin. Can be either risk difference, risk ratio, odds ratio or arc-sine difference.}
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
  This function estimates power of a standard two-arm non-inferiority trial for a given sample size, running a certain number n.rep of simulations under the alternative hypothesis and calculatign estimated success rate of the trial with the desired design and analysis methods. 
}
\value{
  The estimated power. On screen, the Monte-Carlo Confidence Interval is printed as well.  
}
\examples{
  
  \donttest{power<-power.NI.binary(p.control.expected=0.2, p.experim.target=0.2, NI.margin=0.1, n.control=200, n.experim=200,
                                       n.rep=500)}
  
}
