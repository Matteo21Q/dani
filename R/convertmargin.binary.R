convertmargin.binary <- function( p.control.expected, NI.margin.original, summary.measure.original, summary.measure.target) {
  
  stopifnot(summary.measure.original%in%c("RD", "OR", "RR", "AS"))
  stopifnot(summary.measure.target%in%c("RD", "OR", "RR", "AS"))
  stopifnot(is.numeric(p.control.expected), p.control.expected<1, p.control.expected>0)
  stopifnot(is.numeric(NI.margin.original))
  
  
  if (summary.measure.original=="RD") {
    
    p.experim.nontolerable <- p.control.expected+NI.margin.original
    
  } else if  (summary.measure.original=="RR") {
    
    if (NI.margin.original<=0) stop ("Non-inferiority margin on risk ratio scale should be >0.")
    p.experim.nontolerable <- p.control.expected*NI.margin.original
    
  } else if  (summary.measure.original=="OR") {
    
    if (NI.margin.original<=0) stop ("Non-inferiority margin on odds ratio scale should be >0.")
    odds.control.expected <- p.control.expected/(1-p.control.expected)
    odds.experim.nontolerable <- odds.control.expected*NI.margin.original
    p.experim.nontolerable <- odds.experim.nontolerable/(1+odds.experim.nontolerable)
    
  } else if  (summary.measure.original=="AS") {
    
    p.experim.nontolerable <- (sin(asin(sqrt(p.control.expected))+NI.margin.original))^2
    
  } 
  
  if (!((is.numeric(p.experim.nontolerable))&(p.experim.nontolerable<1)&(p.experim.nontolerable>0))) {
    
    stop("The non-inferiority margin on the original scale implies an impossible experimental event risk. Check that this was not mis-specified.")

  }
  
  if (summary.measure.target=="RD") {
    
    NIm <- p.experim.nontolerable - p.control.expected
    
  } else if  (summary.measure.target=="RR") {
    
    NIm <- p.experim.nontolerable / p.control.expected
    
  } else if  (summary.measure.target=="OR") {
    
    odds.control.expected <- p.control.expected/(1-p.control.expected)
    odds.experim.nontolerable <- p.experim.nontolerable/(1-p.experim.nontolerable)
    NIm <- odds.experim.nontolerable / odds.control.expected
    
  } else if  (summary.measure.target=="AS") {
    
    NIm <- asin(sqrt(p.experim.nontolerable)) - asin(sqrt(p.control.expected))
    
  } 
  
  return(NIm)
} 
