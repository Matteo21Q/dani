\name{type1error.NI.frontier}
\alias{type1error.NI.frontier}
\title{
Type 1 error calculator for trials designed with Non-Inferiority Frontiers}
\description{
A function to estimate type 1 error of a trial designed with a non-inferiority frontier and analysed on either the risk difference or risk ratio scale.}
\usage{
type1error.NI.frontier(p0.expected, p1.expected, NI.frontier, sample.size, 
                              r=1, range.of.p0, threshold.modify=0,
                              sig.level=0.025,
                              scale="RD", threshold.relevance=10^(-5),
                              print.out=T, unfavourable=T)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{p0.expected}{
The expected control event risk.
}
 \item{p1.expected}{
The expected active event risk.
}
 \item{NI.frontier}{
The Non-Inferiority frontier function. This must be a function in a single variable, that is the control event risk. 
}
 \item{sample.size}{
The sample size of the trial. A vector, the first entry is the sample size of the control arm, the second entry of the active arm. 
}
 \item{r}{
The allocation ratio. Default is 1, i.e. equal allocation between arms. 
}
 \item{range.of.p0}{
The range of true control event risks for which we want to explore the type 1 error rate of the design. 
}
 \item{threshold.modify}{
The threshold for the difference considered significant enough to change the margin using the non-inferiority frontier.
}
 \item{sig.level}{
The signifincace level used for testing the null hypothesis. Either a function defining significance level to be used depending on observed control event risk, or a scalar if significance level is fixed.
}
 \item{scale}{
The analysis scale. Can be either "RD" (Risk Difference), "OR" (Odds Ratio), "AS" (Arc-Sine Difference) or "RR" (Risk Ratio).
}
 \item{threshold.relevance}{
If the density of a specific outcome is lower than this threshold, this is igored. For example, if dbinom(i,sample.size[1],p0)<threshold relevance, i is considered a very unlikely value of observed events for p0.
}
 \item{print.out}{
Logical. If FALSE no output is printed on screen.
}
\item{unfavourable}{Logical. If TRUE, the outcome is considered unfavourable, e.g. death. This is also the default.
 }
}
\details{
This function estimates type 1 error with a specific sample size for a non-inferiority trial analysed on either the risk difference or risk ratio scale, defining a non-inferiority frontier. The analysis changes the margin to that indicated by the frontier if the difference between expected and observed control risks is more than a certain threshold. 
Note this function only returns an approximation because it assumes the sample size is precisely respected, while generally the true sample size will randomly vary around the expected one. 

}
\value{
A data.frame with estimated type 1 errors at each control event risk.
}

\examples{
p0.expected<-0.05 # Expected control event rate
p1.expected<-p0.expected # Same as expected active event rate
p1.nontolerable<-0.1  # Minimum non-tolerable active event rate 
r<-1       # Allocation ratio
NI.m<-p1.nontolerable-p0.expected
range.of.p0<-seq(0.005,0.1,0.005)
ss<-sample.size.NI(p0.expected, p1.expected, NI.m)
arcsine.frontier<-function(pio0) {
  return(sin(asin(sqrt(pio0))+arcsine.margin(p1.nontolerable,p0.expected))^2-pio0)
}

# Commented out becaus eof CRAN policy on examples. 
#res7<-type1error.NI.frontier(p0.expected,p1.expected,arcsine.frontier, ss, r,range.of.p0)

res7<-type1error.NI.frontier(p0.expected,p1.expected,arcsine.frontier, ss, r,range.of.p0=c(0.045,0.05))
}
