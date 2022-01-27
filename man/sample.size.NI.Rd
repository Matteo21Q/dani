\name{sample.size.NI}
\alias{sample.size.NI}
\title{
  Sample size calculation tool for Non-Inferiority trials
}
\description{
A function for calculating sample size of a non-inferiority trial. 
}
\usage{
sample.size.NI(p0.expected, p1.expected, NI.margin, sig.level=0.025, 
               power=0.9, r=1, scale="RD", print.out=TRUE, var.type="AA",
               unfavourable=T)  
}
\arguments{
\item{p0.expected}{Expected event risk in the control arm.}
\item{p1.expected}{Expected event risk in the active arm.}
\item{NI.margin}{Non-inferiority margin. Can be either risk difference, risk ratio, odds ratio or arc-sine difference.}
\item{sig.level}{One-sided significance level for testing. Default is 0.025, i.e. 2.5\%.}
\item{power}{Power of the trial, i.e. one minus type-II error of the study. Default is 0.9, i.e. 90\%.  }
\item{r}{Allocation ratio, i.e. ratio between sample sizes in the active and control goups. Default is 1.}
\item{scale}{The scale on which we define the non-inferiority trial, i.e. the population-level summary measure to summarise information on the outcome. Can be one of "RD" (Risk      difference), "RR" (risk ratio), "OR" (odds ratio) or "AS" (arc-sine difference).}
\item{print.out}{Logical. If FALSE, no output is printed.}
\item{test.type}{A string that indicates the type of test to be assumed for the sample size calculation. Currently, three options are supported: "Wald", "score" and "local".}
\item{unfavourable}{A logical variable. If TRUE, the outcome is considered unfavourable. This is used to check that the NI margin specified is meaningful.}
\item{cont.corr}{A logical variable. If TRUE, a continuity correction is applied. This is currently supported for the risk difference scale only.}

}
\details{
 This is a function to calculate sample size needed to test non-inferiority of an active treatment against the control within a specific NI margin. The margin can be specified on a number of different scales. 
 }
 
\value{
The output is a vector ss, containing the sample sizes for the control and active arms respectively.
}

\examples{

  p0.expected<-0.05 # Expected control event rate
  p1.expected<-p0.expected # Same as expected active event rate
  NI.marg.RD<-0.05  # Non-inferiority margin on risk difference scale
  NI.marg.RR<-NI.marg.OR<-2  # Non-inferiority margin on risk or odds ratio scale
  # Non-inferiority margin on arc-sine difference scale:
    NI.marg.AS<-arcsine.margin(p1.expected+NI.marg.RD, p0.expected) 

  r<-1       # Allocation ratio
  power<-0.9 # Power
  alph<-0.025 # Significance level

sample.size.RD<-sample.size.NI(sig.level=alph, power=power, 
   p0.expected=p0.expected, p1.expected=p1.expected, 
   NI.margin=NI.marg.RD, r=r)  # Risk difference scale
sample.size.RR<-sample.size.NI(sig.level=alph, power=power, 
   p0.expected=p0.expected, p1.expected=p1.expected, 
   NI.margin=NI.marg.RR, r=r, scale="RR")  # Log-risk ratio scale
sample.size.AS<-sample.size.NI(sig.level=alph, power=power, 
   p0.expected=p0.expected, p1.expected=p1.expected, 
   NI.margin=NI.marg.AS, r=r, scale="AS")  # Arc-sine difference scale
sample.size.ORD<-sample.size.NI(sig.level=alph, power=power, 
   p0.expected=p0.expected, p1.expected=p1.expected, 
   NI.margin=NI.marg.OR, r=r, scale="OR")  # Log-odds ratio scale

}