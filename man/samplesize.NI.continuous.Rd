\name{samplesize.NI.continuous}
\alias{samplesize.NI.continuous}
\title{
  Sample size calculation tool for Non-Inferiority trials with a continuous outcome.
}
\description{
A function for calculating sample size of a non-inferiority trial whose primary outcome is continuous. Allows for different summary measures, test types and both favourable (e.g. cure) and unfavourable (e.g. death) events. 
}
\usage{
samplesize.NI.continuous(mean.control, mean.experim, sd, NI.margin, 
                         sig.level = 0.025, power = 0.9, r = 1, 
                         summary.measure = "mean.difference", print.out = TRUE, 
                         test.type=NULL, higher.better=T,
                         round=T, ltfu=0)   
}
\arguments{
\item{mean.control}{Assumed mean in the control arm.}
\item{mean.experim}{target mean in the experimental arm for powering the trial.}
\item{sd}{Standard deviation in both arms.}
\item{NI.margin}{Non-inferiority margin. Can be either efined as a mean difference, or mean ratio.}
\item{sig.level}{One-sided significance level for testing. Default is 0.025, i.e. 2.5\%.}
\item{power}{Power of the trial, i.e. one minus type-II error of the study. Default is 0.9, i.e. 90\%.  }
\item{r}{Allocation ratio, i.e. ratio between sample sizes in the active and control goups. Default is 1.}
  \item{summary.measure}{The population-level summary measure to be estimated, i.e. the scale on which we define the non-inferiority margin. Can be one of "mean.difference" (Difference of Means) or "mean.ratio" (Ratio of Means).}
\item{print.out}{Logical. If FALSE, no output is printed.}
  \item{higher.better}{Logical. If FALSE, the outcome is considered 
  unfavourable, i.e. higher scores indicate worse outcomes.
  Default is TRUE, i.e. favourable outcome, higher scores indicate better outcomes.}
  \item{test.type}{A character string defining the method to be used for sampel size calculations. For the mean difference, methods available include "Z.test" and "t.test".
  For the mean ratio, methods available include "Fiellers" test, "log.t.test" or "log.Z.test".
 }
 \item{round}{A logical variable. If TRUE, sample sizes are rounded to the next integer, using the ceiling function. Otherwise, they are left unrounded.}
\item{ltfu}{A numeric variable with the expected proportion of patients lost to follow-up. Default is 0.}

}
\details{
 This is a function to calculate sample size needed to test non-inferiority of an active treatment against the control within a specific NI margin. The margin can be specified on a number of different scales. 
 }
 
\value{
The output is a vector ss, containing the sample sizes for the control and active arms respectively.
}

\examples{

out<-try(samplesize.NI.continuous(20, mean.experim=20, sd=40, NI.margin=-10, 
                                    sig.level = 0.025, power = 0.9, r = 1, 
                                    summary.measure = "mean.difference", 
                                    print.out = TRUE, test.type="t.test",
                                    higher.better=T))

}