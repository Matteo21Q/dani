\name{test.NIfrontier.binary}
\alias{test.NIfrontier.binary}

\title{
  Non-inferiority testing with binary outcome using a non-inferiority frontier.
}
\description{
  Functions for testing non-inferiority when the outcome is binary and the goal is to use a non-inferiority frontier. It allows for a number of tests on different summary measures: risk difference, risk ratio, odds ratio or arc-sine difference. It also allows for direct Likelihood Ratio Test of the frontier.
}
\usage{
  test.NIfrontier.binary(n.control, n.experim, e.control, e.experim,  
                                    NI.frontier, sig.level, summary.measure="RD", 
                                    print.out=TRUE, unfavourable=TRUE, test.type=NULL,
                                    M.boot=2000, BB.adj=0.0001)  
}
\arguments{
  \item{e.control}{Number of events in the control arm.}
  \item{e.experim}{Number of events in the active arm.}
  \item{n.control}{Total sample size of the control arm.}
  \item{n.experim}{Total sample size of the active arm.}
  \item{NI.frontier}{Non-inferiority frontier, a function whos eonly input should be the control event risk and that returns the NI margin for that risk expressed as the specified summary measure.}
  \item{sig.level}{One-sided significance level for testing. It can be both a scalar (same significance level for any control risk) or a function in oen variable (giving significance level to be used for each observed risk). Default is 0.025, i.e. 2.5\%.}
  \item{summary.measure}{The population-level summary measure to be estimated, i.e. the scale on which we define the non-inferiority margins through the frontier. Can be one of "RD" (Risk Difference), "RR" (Risk Ratio), "OR" (Odds Ratio) or "AS" (Arc-Sine difference).}
  \item{print.out}{Logical. If FALSE, no output is printed.}
  \item{unfavourable}{Logical. If FALSE, the outcome is considered favourable, e.g. cure.
    Default is TRUE, i.e. unfavourable outcome, e.g. death.}
  \item{test.type}{A character string defining the method to be used for calculation of the confidence interval. For the risk difference, methods available include "Wald", "Wald.cc" (with continuity correction), "Hauck.Anderson", "Gart.Nam", "Newcombe10", "Newcombe11", "Haldane", "Jeffrey.Perks", "Agresti.Caffo", "Miettinen.Nurminen", "Farrington.Manning", "logistic" (marginalisation after fitting logistic regression model with glm), "bootstrap", "Agresti.Min", "Brown.Li.Jeffreys", "Chan.Zhang", "BLNM", "Mee", "uncond.midp", "Berger.Boos", "MUE.Lin", "MUE.parametric.bootstrap" and "LRT".
    For risk ratio, possible methods are "Wald.Katz", "adjusted.Wald.Katz", "inverse.hyperbolic.sine", "Koopman", "MOVER.R", "Miettinen.Nurminen", "MOVER", "Gart.Nam", "score.cc" (with continuity correction),"logregression" (binomial regression with log link), "bootstrap", "Bailey", "Noether", "Chan.Zhang", "Agresti.Min", "uncond.midp", "Berger.Boos", "LRT".
    For odds ratio, methods available include "Wald.Woolf", "adjusted.Wald.Woolf", "inverse.hyperbolic.sine", "Cornfield.exact", "MOVER.R", "Miettinen.Nurminen", "MOVER", "Gart.Nam", "score.cc", "logistic" (logistic regression), "bootstrap", "Cornfield.midp", "Baptista.Pike.exact", "Baptista.Pike.midp", "Chan.Zhang", "Agresti.Min", "uncond.midp", "Berger.Boos", "LRT".
    Finally, for arc-sine difference the only available methods are "LRT" and "Wald".
  }
  \item{M.boot}{ Number of bootstrap samples, e.g. for "bootstrap"" and "MUE.parametric.bootstrap" methods.}
  \item{BB.adj}{Adjustment factor for "Berger.Boos" method.}
}
\details{
  This is a function to test non-inferiority of an experimental treatment against the active control within a specific NI margin defined using a NI frontier. The margin can be specified on a number of different summary measures, including absolute risk difference, risk ratio, odds ratio and arc-sine difference. It is possible to test both with favourable (e.g. cure) or unfavourable (e.g. death) outcomes and using a multitude of methods taken from other packages. See the entry on the test.type argument for the specific methods available for each summary measure. The function also allows for direct test of the frontier using LRT, reporting results on a chosen summary measure. 
}

\value{
  The output is a list, containing the estimate, standard error, cofidence interval (two-sided 2*alpha level), Z statistic and p-value. Additionally, a non-inferiority indicator is included and an indicator of whether the p-value was precise or just estimated from the confidence interval using normal approximation.
}

\examples{
  
n.control<-1000
n.experim<-1000
e.control<-0.05*n.control
e.experim<-0.05*n.experim
NImRD=function(p0) {
   return(0.05)
   }
alpha=0.025

test1<-test.NIfrontier.binary(n.control=n.control, n.experim=n.experim, e.control=e.control, e.experim=e.experim, NI.frontier=NImRD, sig.level=alpha, summary.measure = "RD")

  
}