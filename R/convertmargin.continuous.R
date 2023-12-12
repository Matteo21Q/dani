convertmargin.continuous <- function( mean.control.expected, NI.margin.original, summary.measure.original, summary.measure.target) {
  
  stopifnot(summary.measure.original%in%c("difference", "ratio"))
  stopifnot(summary.measure.target%in%c("difference", "ratio"))
  stopifnot(is.numeric(mean.control.expected))
  stopifnot(is.numeric(NI.margin.original))
  
  if (summary.measure.original=="difference") {
    
    mean.experim.nontolerable <- mean.control.expected+NI.margin.original
    
  } else if  (summary.measure.original=="ratio") {
    if (mean.control.expected==0) stop("Mean ratio not a valid summary measure when expected value in control arm is 0.")
    if (NI.margin.original<=0) stop("Non-inferiority margin for mean ratio should be >0.")
    mean.experim.nontolerable <- mean.control.expected*NI.margin.original
    
  }  
  
  if (!((is.numeric(mean.experim.nontolerable))&(mean.experim.nontolerable<Inf)&(mean.experim.nontolerable>-Inf))) {
    
    stop("The non-inferiority margin on the original scale implies an impossible experimental event risk. Check that this was not mis-specified.")
    
  }
  
  if (summary.measure.target=="difference") {
    
    NIm <- mean.experim.nontolerable - mean.control.expected
    
  } else if  (summary.measure.target=="ratio") {
    
    NIm <- mean.experim.nontolerable / mean.control.expected
    
  } 
  
  return(NIm)
} 
