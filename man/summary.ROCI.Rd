\name{summary.ROCI}
\alias{summary.ROCI}
\title{
  Summarise the results of the analysis of a MAMS-ROCI trial
}
\description{
  A function for summarising results of the analysis of a MAMS-ROCI randomised trial that has been analysed with the test.ROCI.binary function. 
}
\usage{
  \method{summary}{ROCI}(object, ...) 
}

\arguments{
  \item{object}{A list obtained as an output from function test.ROCI.binary.}
  \item{...}{Not used.}
}
\details{
  This is a function to summarise results of a call to test.ROCI.binary. 
  
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
                        se.method="delta", treatment.levels=8:20, summary.measure="RD", 
                        NI.margin=NI.margin.RD)

summary(res1)
}