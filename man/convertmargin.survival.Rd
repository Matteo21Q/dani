\name{convertmargin.survival}
\alias{convertmargin.survival}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Function to convert non-inferiority margin for survival outcome between summary measures. 
}
\description{
  Function to convert non-inferiority margin between different summary measures with a survival outcome. Supported summary measures are hazard ratio (HR), difference in restricted mean survival time (DRMST) and difference in surviving proportion (DS).
}
\usage{
  convertmargin.survival(rate.control.expected=NULL, t.expected=NULL,
  p.control.expected=NULL, NI.margin.original, 
  summary.measure.original, summary.measure.target, 
  tau.RMST=NULL, t.DS=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  
  \item{rate.control.expected}{
    The expected control event rate. This is the rate parameter of an exponential distribution for event times in the control arm. Note this can be avoided if passing t.expected and p.control.expected.  
  }
    \item{t.expected}{
    The time at which the control event risk (p.control.expected) is provided. Note this can be avoided if passing rate.control.expected.     
  }
    \item{p.control.expected}{
    The expected control event risk at t.expected. Note this can be avoided if passing rate.control.expected.  
  }
  \item{NI.margin.original}{
    The non-inferiority margin on the original scale, i.e. using the original summary measure. 
  }
  \item{summary.measure.original}{
    The original summary measure on which the non-inferiority margin is specified. One of "HR" (hazard ratio), "DRMST" (difference in restricted mean survival time) or "DS" (difference in surviving proportion). 
  }
  \item{summary.measure.target}{
    The summary measure in which we want to convert the non-inferiority margin. One of "RD" (risk difference), "RR" (risk ratio), "OR" (odds ratio) or "AS" (arcsine difference). 
  }
  
  \item{tau.RMST}{
    The horizon time tau for RMST. 
  }
    \item{t.DS}{
    The horizon time t for difference in surviving proportion. 
  }
}

\value{
  This function returns the non-inferiority margin in the target summary measure. 
}


\examples{
  # Example: HR to DRMST
  p0.expected<-0.1
  t.ex<-3
  NI.margin.HR<-2
  NI.margin.DRMST<-convertmargin.survival(t.expected=t.ex,
     p.control.expected=p0.expected, 
     NI.margin.original=NI.margin.HR, summary.measure.original="HR",
     summary.measure.target="DRMST", tau.RMST=3)
  # Example: DS to HR
  NI.margin.DS<-0.05
  NI.margin.HR<-convertmargin.survival(t.expected=t.ex,
     p.control.expected=p0.expected, 
     NI.margin.original=NI.margin.DS, summary.measure.original="DS",
     summary.measure.target="HR", t.DS=3)
  # Example: DS to DRMST
  NI.margin.DRMST<-convertmargin.survival(t.expected=t.ex,
     p.control.expected=p0.expected, 
     NI.margin.original=NI.margin.DS, summary.measure.original="DS",
     summary.measure.target="DRMST",tau.RMST=3, t.DS=3)
  
}
