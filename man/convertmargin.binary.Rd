\name{convertmargin.binary}
\alias{convertmargin.binary}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function to convert non-inferiority margin for binary outcome between summary measures. 
}
\description{
Function to convert non-inferiority margin between different summary measures with a binary outcome. Supported summary measures are risk difference (RD), risk ratio (RR), odds ratio (OR) and arcsine difference (AS).
}
\usage{
convertmargin.binary(p.control.expected, NI.margin.original, 
                  summary.measure.original, summary.measure.target)
                  }
%- maybe also 'usage' for other objects documented here.
\arguments{

 \item{p.control.expected}{
The expected control event risk. 
}
 \item{NI.margin.original}{
The non-inferiority margin on the original scale, i.e. using the original summary measure. 
}
 \item{summary.measure.original}{
The original summary measure on which the non-inferiority margin is specified. One of "RD" (risk difference), "RR" (risk ratio), "OR" (odds ratio) or "AS" (arcsine difference). 
}
 \item{summary.measure.target}{
The summary measure in which we want to convert the non-inferiority margin. One of "RD" (risk difference), "RR" (risk ratio), "OR" (odds ratio) or "AS" (arcsine difference). 
}

}

\value{
This function returns the non-inferiority margin in the target summary measure. 
}


\examples{
# Example: RD to AS
p0.expected<-0.05
NI.margin.RD<-0.05
NI.margin.AS<-convertmargin.binary(p0.expected, NI.margin.RD,"RD","AS")
# Example: RR to RD
NI.margin.RR<-2
NI.margin.RD<-convertmargin.binary(p0.expected, NI.margin.RR,"RR","RD")
# Example: RD to OR
NI.margin.OR<-convertmargin.binary(p0.expected, NI.margin.RD,"RD","OR")

}
