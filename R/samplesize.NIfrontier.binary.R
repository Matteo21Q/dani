samplesize.NIfrontier.binary <- function(p.control.expected, p.experim.target=NULL, NI.frontier, sig.level=0.025,
                                     summary.measure="RD", print.out=TRUE, unfavourable=TRUE, 
                                     power=0.9, r=1, round=T, ltfu=0) {
  
  stopifnot(is.numeric(p.control.expected), p.control.expected>0, p.control.expected<1)
  stopifnot(is.numeric(sig.level), sig.level>0, sig.level<0.5)
  stopifnot(is.function(NI.frontier), length(formals(NI.frontier))==1)
  stopifnot(is.numeric(p.experim.target), p.experim.target>0, p.experim.target<1)
  stopifnot(is.character(summary.measure), summary.measure %in% c("RD", "RR", "OR", "AS" ))
  stopifnot(is.logical(print.out), !is.na(print.out))
  stopifnot(is.logical(unfavourable), !is.na(unfavourable))
  stopifnot(is.numeric(power), power < 1, power > 0)
  stopifnot(is.numeric(r), r > 0)
  stopifnot(is.logical(round), !is.na(round))
  stopifnot(is.numeric(ltfu), ltfu < 1, ltfu >= 0)
  

  ss0<-samplesize.NI.binary(p.control.expected=p.control.expected, p.experim.target=p.experim.target,  
                                      NI.margin=NI.frontier(p.control.expected), sig.level=sig.level, 
                                      power=power, r=r, summary.measure=summary.measure, print.out=FALSE, test.type="score", 
                                      unfavourable=unfavourable, round=F, ltfu=0) 
  n.control<-ss0[1]
  n.experim<-ss0[2]
  e.control<-round(n.control*p.control.expected)
  e.experim<-round(n.experim*p.experim.target)
  
  fit.ed<-test.NIfrontier.binary(n.control=n.control, n.experim=n.experim, e.control=e.control, e.experim=e.experim,  
                                             NI.frontier=NI.frontier, sig.level=sig.level, summary.measure=summary.measure, 
                                             print.out=FALSE, unfavourable=unfavourable, test.type="LRT")
  
  p0 <-ifelse(summary.measure%in%c("RD","AS"), p.control.expected, log(p.control.expected))
  p1 <-ifelse(summary.measure%in%c("RD","AS"), p.experim.target, log(p.experim.target))
  NI.m <-ifelse(summary.measure%in%c("RD","AS"), NI.frontier(p.control.expected), log(NI.frontier(p.control.expected)))
  ss<-samplesize.NI.continuous(mean.control=p0, mean.experim=p1, sd=sqrt((fit.ed$se)^2*n.experim), 
                               NI.margin=NI.m, sig.level=sig.level, 
                               power=power, r=r, summary.measure="mean.difference", print.out=print.out, 
                               test.type=NULL, higher.better=!unfavourable, round=round, ltfu=ltfu) 
  return(ss)
}
