convertmargin.survival <- function( rate.control.expected=NULL, t.expected=NULL, p.control.expected=NULL, NI.margin.original, summary.measure.original, summary.measure.target, tau.RMST=NULL, t.DS=NULL) {
  
  stopifnot(is.character(summary.measure.original)&summary.measure.original%in%c("DRMST", "HR", "DS"))
  stopifnot(is.character(summary.measure.target)&summary.measure.target%in%c("DRMST", "HR", "DS"))
  stopifnot(is.numeric(NI.margin.original))
  
  if (summary.measure.original=="DRMST"||summary.measure.target=="DRMST") {
      if (is.null(tau.RMST)||!is.numeric(tau.RMST)||tau.RMST<=0) stop("Please provide tau.RMST as a numeric, positive, horizon time for RMST.")
  }
  if (summary.measure.original=="DS"||summary.measure.target=="DS") {
    if (is.null(t.DS)||!is.numeric(t.DS)||t.DS<=0) stop("Please provide t.DS as a numeric, positive, horizon time for DS.")
  }
  
  if (is.null(p.control.expected)&&is.null(rate.control.expected)) {
    stop("Please provide either the control risk expected at t.expected (p.control.expected) or the event rate assuming exponential distribution.")
  }
  
  if (!is.null(p.control.expected)&&!is.null(rate.control.expected)) {
    stop("Please only provide one of either the control risk expected at t.expected (p.control.expected) or the event rate assuming exponential distribution.")
  }
  
  if (!is.null(p.control.expected)) {
    stopifnot(is.numeric(t.expected), t.expected>0)
    stopifnot(is.numeric(p.control.expected), p.control.expected<1, p.control.expected>0)
    if (is.null(t.expected)||!is.numeric(t.expected)||t.expected<=0) stop("When providing p.control.expected, it is necessary to also provide t.expected, i.e. the (numeric, positive) time at which the control event risk has to be evaluated.")
    
    rate.control.expected<-uniroot(fun.rate, c(1E-6, 1E6), tol = 0.000001, 
                                   target.surv=p.control.expected, time=t.expected)$root
  }
  
  if (!is.null(rate.control.expected)) {
    stopifnot(is.numeric(rate.control.expected), rate.control.expected>0)
  }
  
  
  
  if (summary.measure.original=="HR") {
    if (NI.margin.original<=0) stop("Non-inferiority margin as a hazard ratio should be >0")
    rate.experim.nontolerable <- rate.control.expected*NI.margin.original
    
  } else if  (summary.measure.original=="DRMST") {
    
    rate.experim.nontolerable <- uniroot(RMST.margin, c(1E-6, 1E6), tol = 0.000001, 
                                         lambda=rate.control.expected, target=NI.margin.original, tau=tau.RMST)$root
    
  } else if  (summary.measure.original=="DS") {
    
    rate.experim.nontolerable <- uniroot(Diff.margin, c(1E-6, 1E6), tol = 0.000001, 
                                         lambda=rate.control.expected, target=NI.margin.original, t=t.DS)$root
    
    
  } 
  
  if (!((is.numeric(rate.experim.nontolerable))&(rate.experim.nontolerable<Inf)&(rate.experim.nontolerable>0))) {
    
    stop("The non-inferiority margin on the original scale implies an impossible experimental event risk. Check that this was not mis-specified.")
    
  }
  
  if (summary.measure.target=="HR") {
    
    NIm <- rate.experim.nontolerable / rate.control.expected
    
  } else if  (summary.measure.target=="DRMST") {
    
    NIm <- RMST.margin(rate.experim.nontolerable,  rate.control.expected, tau.RMST, 0)
    
  } else if  (summary.measure.target=="DS") {
    
    NIm <- Diff.margin(rate.experim.nontolerable,  rate.control.expected, t.DS, 0)
    
  } 
  
  return(NIm)
} 
