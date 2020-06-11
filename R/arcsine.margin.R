arcsine.margin <- function(p1.nontolerable, p0.expected) {
  NIm <- asin(sqrt(p1.nontolerable)) - asin(sqrt(p0.expected))
  return(NIm)
} 
