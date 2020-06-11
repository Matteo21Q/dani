\name{arcsine.margin}
\alias{arcsine.margin}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function to calculate arc-sine difference margin. 
}
\description{
Function to calculate arc-sine difference margin from the expected control event risk and the non-tolerable active event risk.
}
\usage{
arcsine.margin(p1.nontolerable, p0.expected)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{p1.nontolerable}{
The null hypothesis active event risk. This is the active risk that would be considered non-tolerable despite the secondary advantages of the active treatment. 
}
 \item{p0.expected}{
The expected control event risk. 
}
}

\value{
This function returns the arc-sine difference margin to be used in the trial. 
}


\examples{
p1.nontlerable<-0.1
p0.expected<-0.05
NI.margin.AS<-arcsine.margin(p1.nontolerable, p0.expected)
}
