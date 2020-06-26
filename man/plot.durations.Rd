\name{plot.durations}
\alias{plot.durations}
\title{
  Plot duration-response curve estimated with a DURATIONS trial
}
\description{
  A function to plot the results of the analysis of a DURATIONS randomised trial analysed with analyse.durations . 
}
\usage{
  \method{plot}{durations}(x, ylim=NULL, 
            xlab = "Duration", ylab=NULL, 
            lwd=3, main="Duration-Response Curve", 
            xaxt="n", yaxt="n", type="outcome", 
            predict.target=NULL, ...)
  }
  
  \arguments{
    \item{x}{A list obtained as an output from function analyse.durations.}
    
    \item{ylim}{the y limits of the plot.}
    
    \item{xlab}{a label for the x axis, defaults to a description of x.}
    
    \item{ylab}{a label for the y axis, defaults to a description of y.}
    
    \item{lwd}{A vector of line widths.}
    
    \item{main}{ main title for the plot}
    
    \item{xaxt}{xaxt="n" suppresses the x axis}
    
    \item{yaxt}{yaxt="n" suppresses the y axis}
    
    \item{type}{The type of plot requested. If type="outcome", this willplot CI for primary outcome at different durations. If type="duration", it will show the CI around the optimal duration}
    
    \item{predict.target}{The scale on whch we want to plot the duration-response curve. Default is "resp" for continuous and binary outcomes and "risk" for time-to-event.}
    \item{...}{Other graphical parameters}
    
  }
  
  \details{
    This is a function to plot results of a call to analyse.durations. Two different types of plot are possible if se.method in analyse.durations was "bootstrap".
    
  }
  
  
  \examples{
    
dur.arms=c(8,10,12,14,16,18,20)
sam.sizes=c(500)
NI.margin.RD<-0.1

durations<-sample(dur.arms,sam.sizes,rep=TRUE)
y<-rbinom(sam.sizes,1,0.95-(20-durations)*0.01)

data.ex<-data.frame(y,durations)

res1<-analyse.durations(outcomes=y, durations=durations, family="binomial", 
                                se.method="delta", all.durations=8:20, 
                                scale="RD", NI.margin=NI.margin.RD, 
                                M.boot=NULL, n.cpus=1)
plot(res1, type="outcome")
  }