\name{plot.durations}
\alias{plot.durations}
\title{
  Plot duration-response curve estimated with a DURATIONS trial
}
\description{
  A function to plot the results of the analysis of a DURATIONS randomised trial analysed with analyse.durations . 
}
\usage{
  plot.durations(list.of.results, ylim=NULL, 
            xlab = "Duration", ylab=NULL, 
            lwd=3, main="Duration-Response Curve", 
            xaxt="n", yaxt="n", type="outcome", 
            predict.target=NULL)
  }
  
  \arguments{
    \item{list.of.results}{A list obtained as an output from function analyse.durations.}
    
    \item{type}{The type of plot requested. If type="outcome", this willplot CI for primary outcome at different durations. If type="duration", it will show the CI around the optimal duration}
    
    \item{predict.target}{The scale on whch we want to plot the duration-response curve. Default is "resp" for continuous and binary outcomes and "risk" for time-to-event.}
    
  }
  
  \details{
    This is a function to plot results of a call to analyse.durations. Two different types of plot are possible if se.method in analyse.durations was "bootstrap".
    
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
plot.durations(res1, type="outcome")
  }