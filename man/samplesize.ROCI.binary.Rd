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
samplesize.ROCI.binary(p.expected.curve, NI.margin, reference=max(treatment.levels), 
                                    power.type, power.arms, unfavourable=T, r=NULL,
                                    se.method=NULL, treatment.levels, treatment.arms=treatment.levels, 
                                    summary.measure="RD", tr.model="FP2.classic", M.boot=NULL, parallel="no", 
                                    n.cpus=1, sig.level=0.025, n.tot.start=NULL, power=0.8, print.out=T, round=T, 
                                    ltfu=0, iterative=T)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{p.expected.curve}{
  The expected event risks at the specified treatment levels. It should be a numeric vector of the same length as treatment.levels. 
}

  \item{NI.margin}{
 The non-inferiority margin(s). It can be either a scalar, if the margin is the same for all treatment levels, or a vector of length equal to that of treatment.levels minus one. 
 For summary.measure="target.risk", this represents the target probability that we want to achieve, and if given as a vector it has length equal to that of treatment.levels.
}

  \item{reference}{
  The arm that should act as a reference. It must be one of the arms in treatment.levels. Default is the largest value among treatment.levels.  
}

  \item{power.type}{
  The type of power to target. It can be one of "optimal" (power to identify exactly THE optimal arm), "acceptable" (power to find any arm to be NI to reference), "range" (power to find at least one NI level within a range) and "individual" (power for a specific arm).  
}

  \item{power.arms}{
  The arm(s) that should be targeted in the power calculation. It must be one (for "optimal" and "individual") or more (for "range" or "acceptable") of the arms in treatment levels. 
}

  \item{unfavourable}{
If TRUE the outcome is taken to be an unfavourable event, e.g. death or relapse. If FALSE, the outcome is considered a positive one, like cure. 
}
  \item{r}{
The allocation ratio. It must be a vector of same length as treatment.arms. Default is 1:1:...:1, i.e. equal allocation ratio fo r all arms.  
}

  \item{se.method}{
  Method for calculating standard errors around the treatment-response curve. Can be either "bootstrap" (default for "optimal" and "individual" power.type), "empirical.bootstrap" (default for "range" and "acceptable" power.type) or "delta" (recommended only for initial ballpark estimates). 
}

  \item{treatment.levels}{
  A vector with all the treatment levels to be investigated. These often, but not necessarily, correspond to the treatment arms.  
}
  \item{treatment.levels}{
  A vector with all the treatment arms to which we plan to randomise patients. Deafult is the same as treatment.levels.  
}

  \item{summary.measure}{
  The population-level summary measure that defines the estimand. It can be either "RD" (Risk Difference), "RR" (Risk Ratio), "target.risk" (target probability) or "OR" (Odds Ratio).  
}

  \item{tr.model}{
 Model to use for the treatment-response curve. It can be one of: "FP1.classic" or "FP2.classic" (Multivariable Fractional Polynomials as in package mfp, with a maximum of either 1 or 2 powers), "FP1.fixed" or "FP2.fixed" (Fractional polynomials fixing the number of powers to exactly 1 or 2).  
}

  \item{M.boot}{
  Number of bootstrap samples if using se.method="bootstrap". 
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

  \item{n.tot.start}{
The starting value for total sample size for which the variance of the effect measure(s) are initially calculated. Default is the equivalent of 100 patient per arm for all arms with allocation ratio 1. If using the iterative=T option (recommended) this does not have a strong impact on the results. }

  \item{power}{
The targeted power level.
}

  \item{print.out}{
If TRUE the estimated sample sizes are printed on screen. 
}

\item{round}{A logical variable. If TRUE, sample sizes are rounded to the next integer, using the ceiling function. Otherwise, they are left unrounded.}


\item{ltfu}{A numeric variable with the expected proportion of patients lost to follow-up. Default is 0.}

\item{iterative}{A logical variable. If TRUE, an initial estimate of sample size is obtained by estimating variance(s) on an expected outcome dataset with n.tot.start individuals, and then iteratively re-estimated using the previously found sample size. If set to FALSE, only n.tot.start is used. It is recommended to leave this as TRUE, the default. }


}

\details{
This function estimates the sample size to target either optimal, range, individual or acceptable power for a ROCI trial. It requires as input the expected treatment response curve under which to power the trial, the NI margins and a specification of all the methods. The function creates a data set of expected outcomes (e.g. if risk in one arm is 0.7 and n.tot.start is the default, it creates 70 records with event=1 and 30 with event=0) and fits the model for the treatment-response curve on this data set. It then estimates the variance of the summary measure of interest and uses this to do a sample size calculation based on simple formula from normal theory for non-inferiority with a continuous outcome. 
The function allows for the margin to be expressed as either a risk difference, risk ratio, odds ratio or a target probability. 
}
\value{
A list including both total and arm-specific sample sizes to reach the required level of both optimal and acceptable power. Acceptable power is estimated conservatively as the minimum sample size to achieve certain power for any of the treatment levels.
The function also returns sample size to achieve same power level for all specific treatment levels. 
}
\examples{
p.expected.curve=rep(0.05,7)
NI.margin=rep(0.05,6)
reference=20
power.type="range"
power.arms=c(8,10,12)
se.method="delta"
treatment.levels<-c(8,10,12,14,16,18,20)
summary.measure<-"RD"
tr.model="FP2.classic"
sig.level=0.05
power=0.8
unfavourable=T

\donttest{ss<-samplesize.ROCI.binary(p.expected.curve, NI.margin, reference=reference, power.type=power.type, power.arms=power.arms, unfavourable=unfavourable, 
                                     se.method=se.method,treatment.levels=treatment.levels,summary.measure=summary.measure,
                                     tr.model=tr.model, sig.level = sig.level, power=power)}

}
