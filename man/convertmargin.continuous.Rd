\name{convertmargin.continuous}
\alias{convertmargin.continuous}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Function to convert non-inferiority margin for continuous outcome between summary measures. 
}
\description{
  Function to convert non-inferiority margin between different summary measures with a continuous outcome. Supported summary measures are mean difference ("difference") and ratio ("ratio").
}
\usage{
  convertmargin.continuous(mean.control.expected, NI.margin.original, 
                       summary.measure.original, summary.measure.target)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  
  \item{mean.control.expected}{
    The assumed mean in the control arm. 
  }
  \item{NI.margin.original}{
    The non-inferiority margin on the original scale, i.e. using the original summary measure. 
  }
  \item{summary.measure.original}{
    The original summary measure on which the non-inferiority margin is specified. One of "difference" (mean difference) or "ratio" (mean ratio). 
  }
  \item{summary.measure.target}{
    The summary measure in which we want to convert the non-inferiority margin. One of "difference" (mean difference) or "ratio" (mean ratio). 
  }
  
}

\value{
  This function returns the non-inferiority margin in the target summary measure. 
}


\examples{
  # Example: difference to ratio
  m0.expected<-2
  NI.margin.diff<-1
  NI.margin.rat<-convertmargin.continuous(m0.expected, 
           NI.margin.diff, "difference", "ratio")
  # Example: ratio to difference
  NI.margin.diff<-convertmargin.continuous(m0.expected, 
           NI.margin.rat, "ratio", "difference")
  
}
