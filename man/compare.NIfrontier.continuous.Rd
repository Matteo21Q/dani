\name{compare.NIfrontier.continuous}
\alias{compare.NIfrontier.continuous}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Comparing Non-Inferiority Frontiers for trials with continuous outcomes}
\description{
A function to compare non-inferiority frontiers corresponding to designing a trial with continuous outcomes using different summary measures, i.e. mean difference or mean ratio. The function plots the frontiers for the specified design parameters and ranks them in terms of power for a fixed sample size.}
\usage{
compare.NIfrontier.continuous(mean.control.expected, mean.experim.target=NULL,
                              mean.range, NI.margin, summary.measure="difference")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mean.control.expected}{
The assumed mean outcome in the control arm.
}
 \item{mean.experim.target}{
The target experimental arm mean in the alternative hypothesis used for powering the trial.
}
 \item{mean.range}{
The range of values around the expected control mean to investigate. These should be plausible values for the control mean in case the assumed one was uncertain. 
}
 \item{NI.margin}{
The Non-Inferiority margin on the specified population-level summary measure. This is automatically converted for the specified control mean to the other summary measures for comparison. 
}
 \item{summary.measure}{
The summary measure on which the NI margin is specified. Can be either "difference" (Mean Difference) or "ratio" (Mean Ratio).
}
}

\details{
This function compares various non-inferiority frontiers corresponding to each population-level summary measure. First, the frontiers for mean difference and ratio  margins with the specified design parameters are plotted. Then, the frontiers are ranked in terms of power for a fixed sample size. This function can be used to choose a summary measure for a non-inferiority trial maximising power before performing proper sample size calculations.   

}
\value{
A data.frame with the coordinates of the NI frontiers used in the plot and the distances of each frontier from the trial expected point. A figure comparing the frontiers is plotted and the ranking of the different frontiers is printed on screen. 
}

\examples{
mean.expected<-2 # Expected control mean
mean.alt<-mean.expected # Same as expected control mean
NI.m<-0.5
range.mean<-c(0.005,4)


compare.NIfrontier.continuous(mean.control.expected=mean.expected, 
                              mean.experim.target=mean.alt, 
                              mean.range=range.mean, NI.margin=NI.m, 
                              summary.measure="difference")

}
