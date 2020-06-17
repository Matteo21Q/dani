\name{fracpoly2fix}
\alias{fracpoly2fix}
\title{
  Fixed-2 fractional polynomials
}
\description{
  A function to fit fixed-2 fractional polynomials. These are FP2 with the number of terms fixed at 2, rather than at a maximum of 2. 
}
\usage{
fracpoly2fix(formula, data , family="binomial")  

}

\arguments{
  \item{formula}{The formula of the model to fit. At the moment, a single covariate is allowed, which is duration (or dose/frequency/etc). This should be ideally put within brackets after fp, e.g. fp(durations).}
  \item{data}{A data.frame with outcome and durations data.}
  \item{family}{One of "binomial" (default, for logistic regression), "survival" (xoc proportional hazards model) or "gaussian" (linear regression).}
}

\details{
  This function can be used to run a fixed-2 fractional polynomial algorithm. All possible combinations of 2 polynomial terms selected from the set of possible powers P=(-2,-1,-0.5,0,0.5,1,2,3) are considered. The best fitting function (the one leading to the best likelihood) is selected. Models currently supported are logistic and linear regression and Cox proportional hazards. 
}

\value{
  The output is a list containing the fitted model and the 2 powers selected by the algorithm.
}

\examples{
  

dur.arms=c(8,10,12,14,16,18,20)
sam.sizes=c(500)
n.sim=50
family="binomial"

durations<-sample(dur.arms,sam.sizes,rep=TRUE)
y<-rbinom(sam.sizes,1,0.95)

data.ex<-data.frame(y,durations)
res<-fracpoly2fix(y~fp(durations), data.ex, family="binomial")

}