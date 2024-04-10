power.NI.binary <- function (p.control.expected, p.experim.target, NI.margin, sig.level = 0.025, 
                                  n.control, n.experim, summary.measure = "RD", print.out = TRUE, test.type=NULL,
                                  unfavourable=T, n.rep=1000, M.boot=2000, BB.adj=0.0001) 
{
  
  stopifnot(is.numeric(p.control.expected), p.control.expected < 1, p.control.expected > 0)
  stopifnot(is.numeric(p.experim.target), p.experim.target < 1, p.experim.target > 0)
  stopifnot(is.numeric(sig.level), sig.level < 0.5, sig.level > 0)
  stopifnot(is.numeric(n.experim), n.experim > 0)
  stopifnot(is.numeric(n.control), n.control > 0)
  stopifnot(is.numeric(n.rep), n.rep > 0)
  
  stopifnot(is.character(summary.measure), summary.measure %in%c("RD", "RR", "OR", "AS"))
  stopifnot(is.numeric(NI.margin))
  stopifnot(is.logical(print.out), !is.na(print.out))
  stopifnot(is.logical(unfavourable), !is.na(unfavourable))
  if (summary.measure%in%c("RR", "OR")&&NI.margin<=0) stop("NI margin should be >0 when summary measure is a ratio (OR or RR)")
  if (summary.measure=="RD"&&abs(NI.margin)>=1) stop("NI margin should be <1 in absolute value when summary measure is RD")
  
  p1.exp.null<-ifelse(summary.measure=="RD", p.control.expected+NI.margin, 
                      ifelse(summary.measure=="RR", p.control.expected*NI.margin,
                             ifelse(summary.measure=="OR", p.control.expected*NI.margin/(1-p.control.expected+NI.margin*p.control.expected), 
                                    sin(NI.margin+asin(sqrt(p.control.expected)))^2)))
  
  if (unfavourable==T) {
    if (p.experim.target>=p1.exp.null) stop("In the alternative hypothesis the experimental treatment is not non-inferior. p1=",
                                            p.experim.target*100, "%, which is greater or equal than the minimum non-tolerable event risk=", p1.exp.null*100,"%.\nPlease check again all parameter values. Alternatively makes sure you have specified correctly whether your outcome is favourable.")
  } else {
    if (p.experim.target<=p1.exp.null) stop("In the alternative hypothesis the experimental treatment is not non-inferior. p1=",
                                            p.experim.target*100, "%, which is lower or equal than the maximum non-tolerable event risk=", p1.exp.null*100,"%.\nPlease check again all parameter values. Alternatively makes sure you have specified correctly whether your outcome is favourable.")
    
  }
  
  ni.indicator<-rep(NA, n.rep)
  
  for (i in 1:n.rep) {
    
    e.control<-rbinom(1, n.control, p.control.expected)
    e.experim<-rbinom(1, n.experim, p.experim.target)
    
    fit.ed<-test.NI.binary(n.control=n.control, n.experim=n.experim, e.control=e.control, e.experim=e.experim,
                            NI.margin=NI.margin, sig.level=sig.level, summary.measure=summary.measure, 
                            print.out=FALSE, unfavourable=unfavourable, test.type=test.type,
                            M.boot=M.boot, BB.adj=BB.adj, recursive.p.estim=FALSE)
    
    ni.indicator[i]<-fit.ed$non.inferiority
    
    if (isTRUE(print.out)) {
      if (i%%50==0) cat(".")
      if (i%%1000==0) cat("\n")
    }
  }
  
  power <- mean(ni.indicator)*100
  MC.SE<-sqrt(power*(100-power)/n.rep)
  power.up<-power+qnorm(0.975)*MC.SE
  power.low<-power-qnorm(0.975)*MC.SE
  
  
  if (print.out == T) {
    if (isTRUE(print.out)) {
      cat("The estimated power with the parameters provided and selected analysis method is ", power, "%, 95% Monte-carlo CI: [", power.low, "%, ", power.up, "%]")
    }
  }
  return(power)
}