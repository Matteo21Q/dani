\name{test.NI.binary}
\alias{test.NI.binary}

\title{
  Non-inferiority testing with binary outcome.
}
\description{
  Functions for testing non-inferiority when the outcome is binary. It allows for a number of tests on different summary measures: risk difference, risk ratio, odds ratio or arc-sine difference. 
}
\usage{
  test.NI.binary(n.control, n.experim, e.control, e.experim,  NI.margin, 
                    sig.level=0.025, summary.measure="RD", 
                    print.out=TRUE, unfavourable=TRUE, test.type=NULL,
                    M.boot=2000, BB.adj=0.0001, recursive.p.estim=FALSE)  
}
\arguments{
  \item{e.control}{Number of events in the control arm.}
  \item{e.experim}{Number of events in the active arm.}
  \item{n.control}{Total sample size of the control arm.}
  \item{n.experim}{Total sample size of the active arm.}
  \item{NI.margin}{Non-inferiority margin, expressed as the specified summary measure.}
  \item{sig.level}{One-sided significance level for testing. Default is 0.025, i.e. 2.5\%.}
  \item{summary.measure}{The population-level summary measure to be estimated, i.e. the scale on which we define the non-inferiority margin. Can be one of "RD" (Risk Difference), "RR" (Risk Ratio), "OR" (Odds Ratio) or "AS" (Arc-Sine difference).}
  \item{print.out}{Logical. If FALSE, no output is printed.}
  \item{unfavourable}{Logical. If FALSE, the outcome is considered favourable, e.g. cure.
                  Default is TRUE, i.e. unfavourable outcome, e.g. death.}
  \item{test.type}{A character string defining the method to be used for calculation of the confidence interval. For the risk difference, methods available include "Wald", "Wald.cc" (with continuity correction), "Hauck.Anderson", "Gart.Nam", "Newcombe10", "Newcombe11", "Haldane", "Jeffrey.Perks", "Agresti.Caffo", "Miettinen.Nurminen", "Farrington.Manning", "logistic" (marginalisation after fitting logistic regression model with glm), "bootstrap", "Agresti.Min", "Brown.Li.Jeffreys", "Chan.Zhang", "BLNM", "Mee", "uncond.midp", "Berger.Boos", "MUE.Lin", "MUE.parametric.bootstrap".
  For risk ratio, possible methods are "Wald.Katz", "adjusted.Wald.Katz", "inverse.hyperbolic.sine", "Koopman", "MOVER.R", "Miettinen.Nurminen", "MOVER", "Gart.Nam", "score.cc" (with continuity correction),"logregression" (binomial regression with log link), "bootstrap", "Bailey", "Noether", "Chan.Zhang", "Agresti.Min", "uncond.midp", "Berger.Boos".
 For odds ratio, methods available include "Wald.Woolf", "adjusted.Wald.Woolf", "inverse.hyperbolic.sine", "Cornfield.exact", "MOVER.R", "Miettinen.Nurminen", "MOVER", "Gart.Nam", "score.cc", "logistic" (logistic regression), "bootstrap", "Cornfield.midp", "Baptista.Pike.exact", "Baptista.Pike.midp", "Chan.Zhang", "Agresti.Min", "uncond.midp", "Berger.Boos".
 Finally, for arc-sine difference the only available emthod is "Wald".
 }
  \item{M.boot}{ Number of bootstrap samples, e.g. for "bootstrap"" and "MUE.parametric.bootstrap" methods.}
  \item{BB.adj}{Adjustment factor for "Berger.Boos" method.}
  \item{recursive.p.estim}{If TRUE, the p value is estimated by recursively running the function with varied significance level until the NI margin is crossed. If FALSE (default), the p-value is either the one calculated with standard methods or an estimate based on normal approximation.}
}
\details{
  This is a function to test non-inferiority of an experimental treatment against the active control within a specific NI margin. The margin can be specified on a number of different summary measures, including absolute risk difference, risk ratio, odds ratio and arc-sine difference. It is possible to test both with favourable (e.g. cure) or unfavourable (e.g. death) outcomes and using a multitude of methods taken from other packages. See the entry on the test.type argument for the specific methods availabel for each summary measure. 
}

\value{
  The output is a list, containing the estimate, standard error, cofidence interval (two-sided 2*alpha level), Z statistic and p-value. Additionally, a non-inferiority indicator is included and an indicator of whether the p-value was precise or just estimated from the confidence interval using normal approximation.
}

\examples{
  
n.control<-1000
n.experim<-1000
e.control<-0.05*n.control
e.experim<-0.05*n.experim
NImRD=0.05
NImRR=2
NImOR=1.7
NImAS=arcsine.margin(0.1, 0.05)
alpha=0.025

test1<-test.NI.binary(n.control=n.control, n.experim=n.experim, e.control=e.control, e.experim=e.experim, NI.margin=NImRD, sig.level=alpha, summary.measure = "RD")
test1b<-test.NI.binary(n.control=n.control, n.experim=n.experim, e.control=e.control, e.experim=e.experim, NI.margin=NImRD, sig.level=alpha, summary.measure = "RD", test.type="Miettinen.Nurminen")
test2<-test.NI.binary(n.control=n.control, n.experim=n.experim, e.control=e.control, e.experim=e.experim, NI.margin=NImRR, sig.level=alpha, summary.measure = "RR")
test3<-test.NI.binary(n.control=n.control, n.experim=n.experim, e.control=e.control, e.experim=e.experim, NI.margin=NImOR, sig.level=alpha, summary.measure = "OR", test.type="inverse.hyperbolic.sine")
test4<-test.NI.binary(n.control=n.control, n.experim=n.experim, e.control=e.control, e.experim=e.experim, NI.margin=NImAS, sig.level=alpha, summary.measure = "AS")

  
}