\name{samplesize.NI.binary}
\alias{samplesize.NI.binary}
\title{
  Sample size calculation tool for Non-Inferiority trials with a binary outcome.
}
\description{
A function for calculating sample size of a non-inferiority trial whose primary outcome is binary. Allows for different summary measures, test types and both favourable (e.g. cure) and unfavourable (e.g. death) events. 
}
\usage{
samplesize.NI.binary(p.control.expected, p.experim.target, NI.margin, 
                     sig.level = 0.025, power = 0.9, r = 1, 
                     summary.measure = "RD", print.out = TRUE, 
                     test.type="score", unfavourable=T, cont.corr=F,
                     round=T, ltfu=0)  
}
\arguments{
\item{p.control.expected}{Expected event risk in the control arm.}
\item{p.experim.target}{Target event risk in the experimental arm under which to power the trial.}
\item{NI.margin}{Non-inferiority margin. Can be either risk difference, risk ratio, odds ratio or arc-sine difference.}
\item{sig.level}{One-sided significance level for testing. Default is 0.025, i.e. 2.5\%.}
\item{power}{Power of the trial, i.e. one minus type-II error of the study. Default is 0.9, i.e. 90\%.  }
\item{r}{Allocation ratio, i.e. ratio between sample sizes in the active and control goups. Default is 1.}
  \item{summary.measure}{The population-level summary measure to be estimated, i.e. the scale on which we define the non-inferiority margin. Can be one of "RD" (Risk Difference), "RR" (Risk Ratio), "OR" (Odds Ratio) or "AS" (Arc-Sine difference).}
\item{print.out}{Logical. If FALSE, no output is printed.}
\item{test.type}{A string that indicates the type of test to be assumed for the sample size calculation. Currently, three options are supported: "Wald", "score" and "local".}
\item{unfavourable}{A logical variable. If TRUE, the outcome is considered unfavourable. This is used to check that the NI margin specified is meaningful.}
\item{cont.corr}{A logical variable. If TRUE, a continuity correction is applied. This is currently supported for the risk difference scale only.}
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

  p.control.expected<-0.05 # Expected control event rate
  p.experim.target<-p.control.expected # Same as expected active event rate
  NI.marg.RD<-0.05  # Non-inferiority margin on risk difference scale
  NI.marg.RR<-NI.marg.OR<-2  # Non-inferiority margin on risk or odds ratio scale
  # Non-inferiority margin on arc-sine difference scale:
    NI.marg.AS<-arcsine.margin(p.experim.target+NI.marg.RD, p.control.expected) 

  r<-1       # Allocation ratio
  power<-0.9 # Power
  alph<-0.025 # Significance level

samplesize.RD<-samplesize.NI.binary(sig.level=alph, power=power, 
   p.control.expected=p.control.expected, p.experim.target=p.experim.target, 
   NI.margin=NI.marg.RD, r=r)  # Risk difference scale
samplesize.RR<-samplesize.NI.binary(sig.level=alph, power=power, 
   p.control.expected=p.control.expected, p.experim.target=p.experim.target, 
   NI.margin=NI.marg.RR, r=r, summary.measure="RR")  # Log-risk ratio scale
samplesize.AS<-samplesize.NI.binary(sig.level=alph, power=power, 
   p.control.expected=p.control.expected, p.experim.target=p.experim.target, 
   NI.margin=NI.marg.AS, r=r, summary.measure="AS")  # Arc-sine difference scale
samplesize.ORD<-samplesize.NI.binary(sig.level=alph, power=power, 
   p.control.expected=p.control.expected, p.experim.target=p.experim.target, 
   NI.margin=NI.marg.OR, r=r, summary.measure="OR")  # Log-odds ratio scale

}