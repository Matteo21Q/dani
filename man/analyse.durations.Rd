\name{analyse.durations}
\alias{analyse.durations}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function to analyse a DURATIONS randomised trial. 
}
\description{
A function that can be used to analyse a DURATIONS randomised trial design using one of several possible analysis methods. 
}
\usage{
analyse.durations(outcomes, durations, family="binomial", 
                  se.method="bootstrap", all.durations=unique(durations),
                  scale="RD", NI.margin, M.boot=NULL, n.cpus=1, 
                  sig.level=0.025)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{outcomes}{
  The outcome(s) of the analysis model. If the outcome is binary or continuous, this is expected to be a vector, if it is time-to-event it is a data frame with survival time and event indicator. 
}
  \item{durations}{
  A vector with the durations randomised. 
}
  \item{family}{
  The type of outcome. Can be one of "binomial", "survival" or "gaussian". 
}
  \item{se.method}{
  Method for calculating standard errors around the duration-response curve. Can be either "bootstrap" or "delta". 
}
  \item{all.durations}{
  A vector with all the durations to be investigated. Default is the durations to which patients were randomised.  
}
  \item{scale}{
  The scale on which we define the estimand. For binary outcome one of "RD" (Risk Difference), "RR" (Risk ratio), "rate" (target rate) or "AF" (Acceptability frontier). For survival dat aeither "HR" (Hazard Ratio) or "AF" (Acceptability Frontier). FOr continuous data one of "diff" (difference in mean outcome), "ratio" (ratio of mean outcome), "target" (target mean outcome) or "AF" (Acceptability frontier).  
}
  \item{NI.margin}{
 The non-inferiority margin. A function when scale="AF", a scalar in all other situations.  
}
  \item{M.boot}{
  Number of bootstrap samples if using se.method="bootstrap". 
}
  \item{n.cpus}{
  If using bootstrap, this gives the chance to parallelise over n.cpus cpus. 
}
  \item{sig.level}{
 One-sided significance level. 
}
}
\details{
This function takes data on duration arm and associated outcome and returns the estimated duration-response curve and the desired estimates. Results can then be analysed with summary.durations and plot.durations. 
}
\value{
A list including the model fit, the durations recommended (with boostrap both using the bootstrap CI and bootstrap durations CI methods), upper and lower bounds of the CI for the primary outcome at all durations, and possibly upper and lower bound of CI for optimal duration. 
}
\examples{
dur.arms=c(8,10,12,14,16,18,20)
sam.sizes=c(500)
NI.margin.RD<-0.1

durations<-sample(dur.arms,sam.sizes,rep=T)
y<-rbinom(sam.sizes,1,0.95-(20-durations)*0.01)

data.ex<-data.frame(y,durations)

res1<-analyse.durations(y, durations, family="binomial", 
                                se.method="delta", step=1, scale="RD", 
                                NI.margin.RD, M.boot=NULL, n.cpus=1)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
