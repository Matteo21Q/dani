\name{plot.ROCI}
\alias{plot.ROCI}
\title{
  Plot treatment-response curve estimated with a ROCI trial
}
\description{
  A function to plot the results of the analysis of a ROCI randomised trial analysed with test.ROCI.binary. 
}
\usage{
  \method{plot}{ROCI}(x,type="summary.measure", ylim=NULL, pch=15,
                                     xlab = "Treatment level", ylab=NULL, 
                                     lwd=3, ...)
  }
  
  \arguments{
    \item{x}{A list obtained as an output from function test.ROCI.binary.}
    
    \item{type}{ Type of plot. Can be either "tr.curve" or "summary.measure". "tr.curve" plots the treatment response }
    
    \item{ylim}{the y limits of the plot.}
    
    \item{pch}{Either an integer specifying a symbol or a single character to be used as the default in plotting points.}
    
    \item{xlab}{a label for the x axis, defaults to "Treatment level".}
    
    \item{ylab}{a label for the y axis, defaults to a description of y.}
    
    \item{lwd}{A vector of line widths. Defaults to 3.}
    
    \item{...}{Other graphical parameters}
    
  }
  
  \details{
    This is a function to plot results of a call to test.ROCI.binary. Two different types of plot are possible: with type="tr.curve", the average estimate treatment-response curve is provided, with a red line indicating the acceptability curve and a red point indicating the optimal treatment level.
    If type="summary.level", the estimated population-level summary measures are plotted, with associated confidence intervals around them. The optimal treatment level is painted red. 
    
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

plot(res1, type="tr.curve")
plot(res1, type="summary.measure")

}