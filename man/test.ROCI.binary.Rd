\name{test.ROCI.binary}
\alias{test.ROCI.binary}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function to analyse a MAMS-ROCI (aka DURATIONS) randomised trial. 
}
\description{
A function that can be used to analyse a MAMS-ROCI randomised trial with binary outcome using one of several possible analysis methods to find the minimum/maximum non-inferior arm. 
}
\usage{
test.ROCI.binary(formula=NULL, data=NULL, outcomes=NULL, treatment=NULL, NI.margin,
                 minimisation=T, se.method="bootstrap", 
                 treatment.levels=unique(treatment), summary.measure="RD",
                 tr.model="FP2.fixed", M.boot=NULL, bootCI.type="bca", 
                 parallel="no", n.cpus=1, sig.level=0.025, unfavourable=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{formula}{
  The formula for the outcome model. The variable indicating the treatment aspect to optimise has to be put within brackets and preceded by treat, e.g. treat(treatment).
}
\item{data}{
A data.frame with all data.
}
  \item{outcomes}{
  The outcome(s) of the analysis model. This is expected to be a vector. Used only when formula and data are not specified. 
}
  \item{treatment}{
  A vector with the values of the randomised treatment aspect (e.g. durations). Used only when formula and data are not specified. 
}
  \item{minimisation}{
  If TRUE, the standard level of treatment is taken as the largest and the search is for the minimum arm non-inferior to this. If false, the standard level of treatment is taken as the smallest and the search is for the maximum arm non-inferior to this. 
}
  \item{se.method}{
  Method for calculating standard errors around the treatment-response curve. Can be either "bootstrap" or "delta". 
}
  \item{treatment.levels}{
  A vector with all the treatment levels to be investigated. Default is the ones to which patients were randomised.  
}
  \item{summary.measure}{
  The population-level summary measure that defines the estimand. It can be either "RD" (Risk Difference), "RR" (Risk Ratio), "target.risk" (target probability) or "OR" (Odds Ratio).  
}
  \item{NI.margin}{
 The non-inferiority margin(s). It can be either a scalar, if the margin is the same for all treatment levels, or a vector of length equal to that of treatment.levels minus one. 
 For summary.measure="target.risk", this represents the target probability that we want to achieve, and if given as a vector it has length equal to that of treatment.levels.
}
  \item{M.boot}{
  Number of bootstrap samples if using se.method="bootstrap". 
}
  \item{bootCI.type}{
  Method for computing the confidence intervals if using se.method="bootstrap". It can be either "norm", "basic", "perc" or "bca". Default is "bca", which is the recommended option when possible. 
}
 \item{parallel}{
  If using bootstrap, this gives the chance to parallelise calculations. Use parallel="snow" with windows, or parallel="multicore" with Linux. See help page for the boot function.  
}
  \item{n.cpus}{
  If using bootstrap in parallel, this gives teh number of cpus to parallelise computations on. 
}
  \item{sig.level}{
 One-sided significance level. 
}
  \item{tr.model}{
 Model to use for the treatment-response curve. It can be one of: "FP1.classic" or "FP2.classic" (Multivariable Fractional Polynomials as in package mfp, with a maximum of either 1 or 2 powers), "FP1.fixed" or "FP2.fixed" (Fractional polynomials fixing the number of powers to exactly 1 or 2).  
}
  \item{unfavourable}{
If TRUE the outcome is taken to be an unfavourable event, e.g. death or relapse. If FALSE, the outcome is considered a positive one, like cure. 
}

}
\details{
This function takes trial data on randomised treatment arm (e.g. duration) and associated binary outcome (e.g. 1-year death, or cure) and returns the estimated treatment-response curve. It estimates the chosen population-level summary measure (RD, RR or RR) either against the largest arm ( when minimisation=T) or the shortest one (when minimisation=F). It returns what is the optimum level of treatment according to trial results as well. Results can then be explored and visualised with summary.durations and plot.durations. 
}
\value{
A list including the model fit, the optimal treatment level, mean estimates and upper and lower bounds of the CI for the summary measure on the primary outcome at all treatment levels against control. Additionally, the list includes inputs such as summary measure, analysis method, non-inferiority margin and treatment levels. 
}
\examples{
duration.arms=c(8,10,12,14,16,18,20)
sam.sizes=c(700)
NI.margin.RD<-0.1

durations<-rep(duration.arms, each=100)
y<-rbinom(sam.sizes,1,0.05+(20-durations)*0.01)

data.ex<-data.frame(y,durations)
myformula<-as.formula(y~treat(durations))

res1<-test.ROCI.binary(formula=myformula, data=data.ex, 
                        se.method="delta", treatment.levels=8:20, 
                        summary.measure="RD", NI.margin=NI.margin.RD)

}
