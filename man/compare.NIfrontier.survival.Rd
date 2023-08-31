\name{compare.NIfrontier.survival}
\alias{compare.NIfrontier.survival}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Comparing Non-Inferiority Frontiers for trials with survival outcomes}
\description{
A function to compare non-inferiority frontiers corresponding to designing a trial with survival outcomes using different summary measures, i.e. hazard ratio, difference in restricted mean survival time or difference in surviving proporiton. The function plots the frontiers for the specified design parameters and ranks them in terms of power for a fixed sample size.}
\usage{
compare.NIfrontier.survival(rate.control.expected=NULL, rate.experim.target=NULL, 
                  t.expected=NULL, p.control.expected=NULL, p.experim.target=NULL, 
                  rates.range=NULL, NI.margin, summary.measure="HR", tau.RMST, 
                  t.DS=tau.RMST)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
 \item{rate.control.expected}{
The expected control event rate assuming an exponential distribution of event times. Can be omitted if event risks at t.expected are passed as arguments. 
}
\item{rate.experim.target}{
The target experimental arm event rate in the alternative hypothesis used for powering the trial. This assumes an exponential distribution fo event times within arm and can be omitted if p.experim.target at t.expected are passed as arguments. 
}
\item{t.expected}{
The time at which the expected event risk in the control arm is provided, assuming exponential event distribution. Can be omitted if event rates are passed as arguments. The deafult is t.DS, the time at which the difference in surviving proportion is to be evaluated as summary measure.
}
  \item{p.control.expected}{
The expected control event risk at time t.expected. Can be omitted if event rates are passed as arguments. 
}
 \item{p.experim.target}{
The experimental arm event risk at time t.expected in the alternative hypothesis used for powering the trial. Can be omitted if event rates are passed as arguments. 
}
 \item{rates.range}{
The range of event rates to investigate around the expected control event rate. This assumes exponential distribution of event times. These should be plausible rates in case the assumed control event risk was uncertain. Defaults to 0.3 below and above rate.control.expected, or the rate corresponding to p.control.expected at t.expected.
}
 \item{NI.margin}{
The Non-Inferiority margin on the specified population-level summary measure. This is automatically converted for the specified control event rate to the other summary measures for comparison, assuming exponential distributions within arm. 
}
 \item{summary.measure}{
The summary measure on which the NI margin is specified. Can be either "HR" (Hazard Ratio), "DRMST" (Difference in Restricted Mean Survival Time) or "DS" (Difference in Surviving proportions).
}
\item{tau.RMST}{
Horizon time tau at which difference in RMST should be used as a summary measure.
}
\item{t.DS}{
Horizon time t at which difference in surviving proportion should be used as a summary measure. Defaults to same as tau.RMST.
}
}

\details{
This function compares various non-inferiority frontiers corresponding to each population-level summary measure. First, the frontiers for hazard ratio, difference in restricted mean survival time and difference in surviving proportion margins with the specified design parameters are plotted. Then, the frontiers are ranked in terms of power for a fixed sample size. This function can be used to choose a summary measure for a non-inferiority trial maximising power before performing proper sample size calculations.   
Note that, for the moment, this assumes exponential survival times within arm and proportional hazards. 
}
\value{
A data.frame with the coordinates of the NI frontiers used in the plot and the distances of each frontier from the trial expected point. A figure comparing the frontiers is plotted and the ranking of the different frontiers is printed on screen. 
}

\examples{

set.seed(1)
out1<-compare.NIfrontier.survival(rate.control.expected = 0.3, rate.experim.target=0.3, NI.margin=1.2, summary.measure="HR", t.DS=3, tau.RMST=3)
set.seed(1)
out2<-try(compare.NIfrontier.survival(p.control.expected = 0.8, t.expected=3, NI.margin=0.16, summary.measure="DS", t.DS=3, tau.RMST=3))

}
