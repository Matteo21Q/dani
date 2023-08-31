\name{samplesize.ROCI.binary}
\alias{samplesize.ROCI.binary}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function to compute the necessary sample size for a MAMS-ROCI (aka DURATIONS) randomised trial. 
}
\description{
A function that can be used to do sample size calculations for a MAMS-ROCI randomised trial with binary outcome. The trial can aim to use one of several possible analysis methods to find the minimum/maximum non-inferior arm. 
}
\usage{
samplesize.ROCI.binary(p.expected.curve, NI.margin, minimisation=T, unfavourable=T,
                              se.method="bootstrap", treatment.levels, 
                              summary.measure="RD", tr.model="FP2.fixed", 
                              M.boot=NULL, parallel="no", n.cpus=1, 
                              bootCI.type="bca", sig.level=0.025, n.per.arm=100,
                              power=0.8, print.out=T, varest.boot="CI.bc")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{p.expected.curve}{
  The expected event risks at the specified treatment levels. It should be a numeric vector of the same length as treatment.levels. 
}

  \item{minimisation}{
  If TRUE, the standard level of treatment is the largest and the search is for the minimum arm non-inferior to this. If false, the standard level of treatment is the smallest and the search is for the maximum arm non-inferior to this. 
}
  \item{se.method}{
  Method for calculating standard errors around the treatment-response curve. Can be either "bootstrap" or "delta". 
}
  \item{treatment.levels}{
  A vector with all the treatment levels to be investigated. These will generally correspond to the treatment arms.  
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
  \item{varest.boot}{
  Way in which the variance of the estimator is estimated if using se.method="bootstrap". It can be either "direct" (i.e. take directly the variance estimate from bootstrap) or "ci.bc" (i.e. back-calculates the variance from the confidence interval and the mean estimate).
}
 \item{parallel}{
  If using bootstrap, this gives the chance to parallelise calculations. Use parallel="snow" with windows, or parallel="multicore" with Linux. See help page for the boot function.  
}
  \item{n.cpus}{
  If using bootstrap in parallel, this gives teh number of cpus to parallelise computations on. 
}
  \item{bootCI.type}{
  Method for computing the confidence intervals if using se.method="bootstrap". It can be either "norm", "basic", "perc" or "bca". Default is "bca", which is the recommended option when possible. 
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
  \item{print,out}{
If TRUE the estimated sample sizes are printed on screen. 
}
  \item{power}{
The targeted power level.
}
  \item{n.per.arm}{
The number of patients per arm used in the estimation of variance on a data set of expected outcomes. Default is 100.}
}
\details{
This function estimates the estimated sample size to target both optimal and acceptable power for a ROCI trial. It requires as input the expected treatment response curve under which to power the trial, the NI margins and a specification of all the methods. The function creates a data set of expected outcomes (e.g. if risk in one arm is 0.7, it creates 70 records with event=1 and 30 with event=0) and fits the model for the treatment-response curve on this data set. It then estimates the variance of the summary measure of interest and uses this to do a sample size calculation based on simple formula from normal theory for non-inferiority with a continuous outcome. 
The function allows for the margin to be expressed as either a risk difference, risk ratio, odds ratio or a target probability. 
}
\value{
A list including both total and arm-specific sample sizes to reach the required level of both optimal and acceptable power. Acceptable power is estimated conservatively as the minimum sample size to achieve certain power for any of the treatment levels.
The function also returns sample size to achieve same power level for all specific treatment levels. 
}
\examples{
p.expected.curve=rep(0.05,7)
NI.margin=rep(0.05,6)
minimisation=T
se.method="delta"
treatment.levels<-c(8,10,12,14,16,18,20)
summary.measure<-"RD"
tr.model="FP2.classic"
sig.level=0.05
power=0.8
unfavourable=T

\donttest{ss<-samplesize.ROCI.binary(p.expected.curve, NI.margin, minimisation, unfavourable, se.method,treatment.levels,summary.measure,tr.model, sig.level = sig.level, power=power)}

}
