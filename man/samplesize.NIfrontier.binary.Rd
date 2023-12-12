\name{samplesize.NIfrontier.binary}
\alias{samplesize.NIfrontier.binary}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Sample size calculator for trials with binary outcomes designed with Non-Inferiority Frontiers}
\description{
A function to estimate sample size of a trial designed with a non-inferiority frontier and analysed on either the risk difference, risk ratio, odds ratio or arc-sine difference scale.}
\usage{
samplesize.NIfrontier.binary(p.control.expected, p.experim.target=NULL, NI.frontier, sig.level=0.025,
                                     summary.measure="RD", print.out=TRUE, unfavourable=TRUE, 
                                     power=0.9, r=1, round=T, ltfu=0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{p.control.expected}{Expected event risk in the control arm.}
\item{p.experim.target}{Target event risk in the experimental arm under which to power the trial.}
  \item{NI.frontier}{Non-inferiority frontier, a function whos eonly input should be the control event risk and that returns the NI margin for that risk expressed as the specified summary measure.}
\item{sig.level}{One-sided significance level for testing. Default is 0.025, i.e. 2.5\%.}
\item{power}{Power of the trial, i.e. one minus type-II error of the study. Default is 0.9, i.e. 90\%.  }
\item{r}{Allocation ratio, i.e. ratio between sample sizes in the active and control goups. Default is 1.}
\item{summary.measure}{The population-level summary measure to be estimated, i.e. the scale on which we define the non-inferiority margin. Can be one of "RD" (Risk Difference), "RR" (Risk Ratio), "OR" (Odds Ratio) or "AS" (Arc-Sine difference).}
\item{print.out}{Logical. If FALSE, no output is printed.}
\item{round}{A logical variable. If TRUE, sample sizes are rounded to the next integer, using the ceiling function. Otherwise, they are left unrounded.}
\item{ltfu}{A numeric variable with the expected proportion of patients lost to follow-up. Default is 0.}

}
\details{
This function estimates sample size to acheiev certain power for a non-inferiority 
trial designed defining a non-inferiority frontier. The NI frontier can be defined using 
different sumamry measures. The method used for sample size calculation is "LRT".
}
\value{
A data.frame with estimated powers at each control event risk.
}

\examples{
NI.frontier.RD<-function(p) return(0.1)
out5A<-try(samplesize.NIfrontier.binary(p.control.expected=0.1, p.experim.target=0.1, 
                                        NI.frontier=NI.frontier.RD, sig.level = 0.025, 
                                        power = 0.9, r = 1, 
                                        summary.measure = "RD", print.out = TRUE, 
                                        unfavourable=T))


}
