\name{test.NI.survival}
\alias{test.NI.survival}

\title{
  Non-inferiority testing with survival outcome.
}
\description{
  Functions for testing non-inferiority when the outcome is survival (time-to-event). It allows for a number of tests on different summary measures: Hazard Ratio, Difference in Restricted Mean Survival Time and Difference in Survival. 
}
\usage{
  test.NI.survival(time, event, treat, covariates=NULL, NI.margin, sig.level=0.025, summary.measure="HR", 
                   print.out=TRUE, unfavourable=TRUE, test.type=NULL,
                   M.boot=2000, bootCI.type="bca", tau=NULL, control.level=NULL)  
}
\arguments{
  \item{time}{A vector with the event (or censoring) times.}
  \item{event}{A vector with the event indicators.}
  \item{treat}{A vector with treatment indicators.}
  \item{covariates}{Optional: a data.frame including all baseline covariates to be adjusted for.}
  \item{NI.margin}{Non-inferiority margin, expressed as the specified summary measure.}
  \item{sig.level}{One-sided significance level for testing. Default is 0.025, i.e. 2.5\%.}
  \item{summary.measure}{The population-level summary measure to be estimated, i.e. the scale on which we define the non-inferiority margin. Can be one of "HR" (Hazard Ratio), "DRMST" (Difference in Resticted Mean Survival Time by time tau), or "DS" (Difference in Surviving proportion at time tau).}
  \item{print.out}{Logical. If FALSE, no output is printed.}
  \item{unfavourable}{Logical. If FALSE, the outcome is considered favourable, e.g. cure.
                  Default is TRUE, i.e. unfavourable outcome, e.g. death.}
  \item{test.type}{A character string defining the method to be used for calculation of the confidence interval. For the hazard ratio, methods available include "Cox.PH" (simple Cox proportional hazards model), "flexsurv.PH" (flexible parametric regression model) or "Cox.weighted" (Weighted Cox regression model, using Prentice weights).
  For "DRMST", possible methods are "KM" (Kaplan Meier), "Cox.PH.bootstrap" (bootstrap on estimates from Cox), "flexsurv.PH.delta" (delta method on estimates from flexible parametric survival model), "flexsurv.PH.bootstrap" (bootstrap on estimates from flexible parametric survival model).
 For "DS", methods available include "Newcombe10", "Cox.PH.bootstrap", "flexsurv.PH.delta", "flexsurv.PH.bootstrap".
 }
  \item{M.boot}{ Number of bootstrap samples.}
\item{bootCI.type}{
  Method for computing the confidence intervals if using a bootstrap method. It can be either "norm", "basic", "perc" or "bca". Default is "bca", which is the recommended option when possible. 
}
  \item{tau}{The horizon time tau if using either "DRMST" or "DS" as summary measures.}
    \item{control.level}{A character variable specifying the level of the factor variable for treatment that has to be considered as the control one. If missing, the automatic reference will be assumed to be the control and a NOTE is printed on screen specifying which one that is. }

}
\details{
  This is a function to test non-inferiority of an experimental treatment against the active control within a specific NI margin. The margin can be specified on a number of different summary measures, including HR, DRMST and difference in survival. It is possible to test both with favourable (e.g. cure) or unfavourable (e.g. death) outcomes and using different analysis methods. See the entry on the test.type argument for the specific methods available for each summary measure. 
}

\value{
  The output is a list, containing the estimate, standard error, confidence interval (two-sided 2*alpha level) and p-value. Additionally, a non-inferiority indicator is included and an indicator of whether the p-value was precise or just estimated from the confidence interval using normal approximation.
}

\examples{
  
time1<-rnorm(200,100,10)
event1<-rbinom(200,1,0.5)
treat1<-rep(0:1,each=100)
out5A<-test.NI.survival(time=time1, event=event1, treat=treat1, NI.margin=1.5)

  
}