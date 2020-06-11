\name{summary.durations}
\alias{summary.durations}
\title{
  Summarise the results of the analysis of a DURATIONS trial
}
\description{
  A function for summarising results of the analysis of a DURATIONS randomsied trial that has been analysed with the analyse.durations function . 
}
\usage{
  summary.durations(list.of.results) {
}

\arguments{
  \item{list.of.results}{A list obtained as an output from function analyse.durations.}

\details{
  This is a function to summarise results of a call to analyse.durations. 
  
}


\examples{
  
dur.arms=c(8,10,12,14,16,18,20)
sam.sizes=c(500)
NI.margin.RD<-0.1

durations<-sample(dur.arms,sam.sizes,rep=T)
y<-rbinom(sam.sizes,1,0.95-(20-durations)*0.01)

data.ex<-data.frame(y,durations)

res1<-analyse.durations(y, durations, family="binomial", 
                                se.method="delta", step=1, scale="RD", 
                                NI.margin.RD, M.boot=NULL, n.cpus=1)
summary.durations(res1)
}