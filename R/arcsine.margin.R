arcsine.margin <- function(p.experim.nontolerable, p.control.expected) {
  
  stopifnot(is.numeric(p.experim.nontolerable), p.experim.nontolerable<1, p.experim.nontolerable>0)
  stopifnot(is.numeric(p.control.expected), p.control.expected<1, p.control.expected>0)
  
  NIm <- asin(sqrt(p.experim.nontolerable)) - asin(sqrt(p.control.expected))
  return(NIm)
} 
