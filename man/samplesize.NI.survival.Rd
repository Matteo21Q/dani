\name{samplesize.NI.survival}
\alias{samplesize.NI.survival}
\title{
  Sample size calculation tool for Non-Inferiority trials with a survival outcome.
}
\description{
A function for calculating sample size of a non-inferiority trial whose primary outcome is survival. Allows for different summary measures, test types and both favourable (e.g. cure) and unfavourable (e.g. death) events. 
}
\usage{
samplesize.NI.survival(hazard.target=NULL, p.control.expected=NULL, 
                       p.experim.target=NULL, NI.margin, sig.level = 0.025, 
                       power = 0.9, r = 1, summary.measure = "HR", 
                       print.out = TRUE, test.type="logrank.Schoenfeld",
                       unfavourable=T) 
}
\arguments{
\item{hazard.target}{The target hazard ratio between experimental and control arm at which to power the trial.}
\item{p.control.expected}{Expected event risk in the control arm.}
\item{p.experim.target}{Target event risk in the active arm at which to power the trial.}
\item{NI.margin}{Non-inferiority margin on the selected summary measure.}
\item{sig.level}{One-sided significance level for testing. Default is 0.025, i.e. 2.5\%.}
\item{power}{Power of the trial, i.e. one minus type-II error of the study. Default is 0.9, i.e. 90\%.  }
\item{r}{Allocation ratio, i.e. ratio between sample sizes in the active and control goups. Default is 1.}
\item{summary.measure}{The population-level summary measure to be estimated, i.e. the scale on which we define the non-inferiority margin. Can be one of "HR" (Hazard Ratio), "DRMST" (Difference in Resticted Mean Survival Time by time tau), or "DS" (Difference in Surviving proportion at time tau).}
\item{print.out}{Logical. If FALSE, no output is printed.}
\item{test.type}{A string that indicates the type of test to be assumed for the sample size calculation. Currently, three options are supported for "HR" only: "logrank.Freedman", "logrank.Schoenfeld", "KM".}
\item{unfavourable}{A logical variable. If TRUE, the outcome is considered unfavourable. This is used to check that the NI margin specified is meaningful.}

}
\details{
 This is a function to calculate sample size needed to test non-inferiority of an active treatment against the control within a specific NI margin. The margin can be specified on a number of different scales, though functions for sumamry measures other than HR are currently under development. 
 }
 
\value{
The output is a vector ss, containing the sample sizes for the control and active arms respectively.
}

\examples{

 out5A<-samplesize.NI.survival(hazard.target=1,p.control.expected=0.2, 
                                   p.experim.target=0.2, NI.margin=1.5)

}