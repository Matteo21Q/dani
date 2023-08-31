\name{compare.NIfrontier.binary}
\alias{compare.NIfrontier.binary}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Comparing Non-Inferiority Frontiers for trials with binary outcomes}
\description{
A function to compare non-inferiority frontiers corresponding to designing a trial with binary outcomes using different summary measures, i.e. risk difference, risk ratio or odds ratio. The function plots the frontiers for the specified design parameters and ranks them in terms of power for a fixed sample size.}
\usage{
compare.NIfrontier.binary(p.control.expected, p.experim.target=NULL, 
                          p.range, NI.margin, summary.measure="RD")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{p.control.expected}{
The expected control event risk.
}
 \item{p.experim.target}{
The target experimental arm event risk at which to power the trial.
}
 \item{p.range}{
The range of risks around the expected control event risk to ivestigate. These should be plausible risks in case the assumed control event risk was uncertain. 
}
 \item{NI.margin}{
The Non-Inferiority margin on the specified population-level summary measure. This is automatically converted for the specified control event risk to the other summary measures for comparison. 
}
 \item{summary.measure}{
The summary measure on which the NI margin is specified. Can be either "RD" (Risk Difference), "OR" (Odds Ratio) or "RR" (Risk Ratio).
}
}

\details{
This function compares various non-inferiority frontiers corresponding to each population-level summary measure. First, the frontiers for risk difference, risk ratio and odds ratio margins with the specified design parameters are plotted. Then, the frontiers are ranked in terms of power for a fixed sample size. This function can be used to choose a summary measure for a non-inferiority trial maximising power before performing proper sample size calculations.   

}
\value{
A data.frame with the coordinates of the NI frontiers used in the plot and the distances of each frontier from the trial expected point. A figure comparing the frontiers is plotted and the ranking of the different frontiers is printed on screen. 
}

\examples{
p0.expected<-0.05 # Expected control event risk
p1.alt<-p0.expected # Same as expected control event risk
NI.m<-0.05
range.of.p0<-c(0.005,0.10)


compare.NIfrontier.binary(p.control.expected=p0.expected, p.experim.target=p1.alt, p.range=range.of.p0, NI.margin=NI.m, summary.measure="RD")

}
