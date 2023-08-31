\name{arcsine.margin}
\alias{arcsine.margin}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function to calculate the arc-sine difference margin. 
}
\description{
Function to calculate the arc-sine difference margin from the expected control arm event risk and the non-tolerable experimental arm event risk.
}
\usage{
arcsine.margin(p1.nontolerable, p0.expected)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{p1.nontolerable}{
The null hypothesis experimental arm event risk. This is the minimum (with unfavourable outcomes like death) or maximum (with favourable outcomes like cure) experimental arm risk that would be considered non-tolerable despite the secondary advantages of the experimental treatment. 
}
 \item{p0.expected}{
The expected control event risk. 
}
}

\value{
This function returns the arc-sine difference margin to be used in the trial. 
}


\examples{
# Example with unfavourable events (death)
p1.nontolerable<-0.1
p0.expected<-0.05
NI.margin.AS<-arcsine.margin(p1.nontolerable, p0.expected)
# Example with favourable event (cure)
p1.nontolerable<-0.85
p0.expected<-0.9
NI.margin.AS<-arcsine.margin(p1.nontolerable, p0.expected)

}
