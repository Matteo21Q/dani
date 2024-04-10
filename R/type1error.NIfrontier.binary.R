type1error.NIfrontier.binary <- function(p.control.expected, NI.frontier, sig.level=0.025,
                                    summary.measure="RD", print.out=TRUE, unfavourable=TRUE, 
                                    n.control, n.experim, n.rep=1000, M.boot=2000, BB.adj=0.0001,
                                    test.type="LRT") {
  
  stopifnot(is.numeric(p.control.expected), p.control.expected>0, p.control.expected<1)
  stopifnot((is.function(sig.level)&&length(formals(sig.level))==1)||(is.numeric(sig.level)&&sig.level < 0.5&&sig.level > 0))
  stopifnot(is.function(NI.frontier), length(formals(NI.frontier))==1, is.numeric(NI.frontier(p.control.expected)))
  stopifnot(is.character(summary.measure), summary.measure %in% c("RD", "RR", "OR", "AS" ))
  stopifnot(is.logical(print.out), !is.na(print.out))
  stopifnot(is.logical(unfavourable), !is.na(unfavourable))
  stopifnot(is.numeric(n.control), n.control > 0)
  stopifnot(is.numeric(n.experim), n.experim > 0)
  stopifnot(is.numeric(n.rep), n.rep > 0)
  
  ni.indicator<-rep(NA, n.rep)
  NI.margin<-NI.frontier(p.control.expected)
  
  if (summary.measure=="RD") {
    p.experim.null<-try(p.control.expected+NI.margin, silent=TRUE)
  } else if (summary.measure=="AS") {
    p.experim.null<-try(sin(NI.margin+asin(sqrt(p.control.expected)))^2, silent=TRUE)
  } else if (summary.measure=="RR") {
    p.experim.null<-try(p.control.expected*NI.margin, silent=TRUE)
  } else if (summary.measure=="OR") {
    p.experim.null<-p.control.expected*NI.margin/(1-p.control.expected+NI.margin*p.control.expected)
  }
  for (i in 1:n.rep) {
    
    e.control<-rbinom(1, n.control, p.control.expected)
    e.experim<-rbinom(1, n.experim, p.experim.null)
    
    fit.ed<-test.NIfrontier.binary(n.control=n.control, n.experim=n.experim, e.control=e.control, e.experim=e.experim,  
                                   NI.frontier=NI.frontier, sig.level=sig.level, summary.measure=summary.measure, 
                                   print.out=FALSE, unfavourable=unfavourable, test.type=test.type,
                                   M.boot=M.boot, BB.adj=BB.adj)
    
    ni.indicator[i]<-fit.ed$non.inferiority
    
    if (isTRUE(print.out)) {
      if (i%%50==0) cat(".")
      if (i%%1000==0) cat("\n")
    }
  }
  
  t1err <- mean(ni.indicator)*100
  MC.SE<-sqrt(t1err*(100-t1err)/n.rep)
  t1err.up<-t1err+qnorm(0.975)*MC.SE
  t1err.low<-t1err-qnorm(0.975)*MC.SE
  
  if (isTRUE(print.out)) {
    cat("The estimated type 1 error rate with the NI.frontier provided and selected analysis method is ", t1err, "%, 95% Monte-carlo CI: [", t1err.low, "%, ", t1err.up, "%]")
  }
  
  return(t1err)
}
