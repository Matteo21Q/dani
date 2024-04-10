samplesize.ROCI.binary <- function (p.expected.curve, NI.margin, reference=max(treatment.levels), 
                                    optimal=min(treatment.levels), range=optimal, unfavourable=T,
                                    se.method="bootstrap", treatment.levels, summary.measure="RD", 
                                    tr.model="FP2.classic", M.boot=NULL, parallel="no", n.cpus=1, sig.level=0.025,
                                    n.per.arm=100, power=0.8, print.out=T, round=T, ltfu=0) {
  
  stopifnot(is.numeric(p.expected.curve), all(p.expected.curve < 1), all(p.expected.curve > 0))
  stopifnot(is.numeric(treatment.levels), length(treatment.levels)==length(p.expected.curve))
  stopifnot(is.logical(unfavourable), !is.na(unfavourable))
  stopifnot(is.character(summary.measure),(( summary.measure == "RD" ) || ( summary.measure == "AS" ) || ( summary.measure == "RR" ) || ( summary.measure == "OR" ) || ( summary.measure == "target.risk" )))
  stopifnot(is.numeric(NI.margin), (length(NI.margin)==1)||(length(NI.margin)==(length(treatment.levels)-(summary.measure!="target.risk"))))
  if (length(NI.margin)==1) NI.margin<-rep(NI.margin, length(treatment.levels)-(summary.measure!="target.risk"))
  if (summary.measure%in%c("RD","AS")) {
    if ((unfavourable == T)&&any(NI.margin<=0)) stop("When outcome is unfavourable, risk difference or arc-sine difference NI margins need to all be positive.\n")
    if ((unfavourable == F)&&any(NI.margin>=0)) stop("When outcome is favourable, risk difference or arc-sine difference NI margins needs to all be negative.\n")
    if (any(NI.margin>=1)) stop("NI margins cannot be greater than 1, i.e. 100 percentage points, or otherwise the test is meaningless.\n ")
    if (any(NI.margin<=-1)) stop("NI margins cannot be lower than -1, i.e. -100 percentage points, or otherwise the test is meaningless.\n ")
  } else if (summary.measure%in%c("OR","RR")) {
    if ((unfavourable == T)&&any(NI.margin<=1)) stop("When outcome is unfavourable, NI margins on the risk ratio or odds ratio scale need to all be >1.")
    if ((unfavourable == F)&&any(NI.margin>=1)) stop("When outcome is favourable, NI margins on the risk ratio or odds ratio scale need to all be <1.")
    if (any(NI.margin<=0)) stop("A risk/odds ratio margin must be >0.\n")
  } else if (summary.measure=="target.risk") {
    if (any(NI.margin>=1)||any(NI.margin<=0)) stop("Target risks as defined in NI.margin should be numbers in the interval (0,1)")
  } 
  stopifnot(is.numeric(sig.level), sig.level < 0.5, sig.level > 0)
  stopifnot(is.numeric(power), power < 1, power > 0)
  stopifnot(is.numeric(n.per.arm), all(n.per.arm > 10), (length(n.per.arm)%in%c(1, length(treatment.levels))))
  stopifnot(is.logical(print.out), !is.na(print.out))
  stopifnot(is.logical(round), !is.na(round))
  stopifnot(is.numeric(ltfu), ltfu < 1, ltfu >= 0)
  stopifnot(is.numeric(reference), length(reference)==1, reference%in%treatment.levels)
  stopifnot(is.numeric(optimal), length(optimal)==1, optimal%in%treatment.levels)
  stopifnot(is.numeric(range), all(range%in%treatment.levels))
  ref.index<-which(treatment.levels==reference)
  experimental.arms<-treatment.levels[-ref.index]
  
  if (!is.null(M.boot)) {
    stopifnot(is.numeric(M.boot), M.boot>1)
  } else {
    M.boot=n.per.arm*length(treatment.levels)
  }
  stopifnot(is.character(se.method), se.method%in%c("bootstrap", "delta"))
  stopifnot(is.character(tr.model), tr.model%in%c("FP1.fixed","FP2.fixed", "FP1.classic", "FP2.classic", "FP01", "FP02"))
  stopifnot(is.character(parallel), parallel%in%c("no", "multicore", "snow"))

  if (tr.model%in%c("FP1.fixed","FP1.classic")) {
    if (length(treatment.levels)<3) stop ("With Fractional Polynomials with 1 power, at least 3 arms are needed.\n")
  } else {
    if (length(treatment.levels)<5) stop ("With Fractional Polynomials with 2 powers, at least 5 arms are needed.\n")
  }
  n.arms<-length(treatment.levels)
  n.tot<-ifelse(length(n.per.arm)==1,n.per.arm*n.arms, sum(n.per.arm))
  n.comparisons<-ifelse(summary.measure=="target.risk", length(treatment.levels), length(treatment.levels)-1)
  if (length(NI.margin)==1) NI.margin<-rep(NI.margin, n.comparisons)
  
  # Generate data set with exactly expected outcomes:
  if (length(n.per.arm)==1) {
    treatment<-rep(treatment.levels, each=n.per.arm)
  } else {
    treatment<-rep(treatment.levels, n.per.arm)
  }
  outcomes<-rep(0,n.tot)
  curr.k<-1
  for (nar in 1:n.arms) {
    n.per.arm.i<-ifelse(length(n.per.arm)==1,n.per.arm, n.per.arm[nar])
    outcomes[curr.k:(p.expected.curve[nar]*n.per.arm.i+curr.k-1)]<-1
    curr.k<-curr.k+n.per.arm.i
  }
  dat<-data.frame(outcomes,treatment)
  
   
    # Fit fractional polynomials on expected outcomes data set:
    myformula<-as.formula("outcomes~treat(treatment)")
    res<-test.ROCI.binary(formula=myformula, data=dat,  reference = reference, unfavourable=unfavourable,
                          se.method=se.method, treatment.levels=treatment.levels, summary.measure=summary.measure, 
                          NI.margin=NI.margin, sig.level=sig.level, parallel=parallel, n.cpus=n.cpus,
                          tr.model=tr.model, M.boot=M.boot, bootCI.type = "basic")
    
    if (summary.measure=="RD") {
      expected.sm<-p.expected.curve[-which(treatment.levels==reference)]-p.expected.curve[which(treatment.levels==reference)]
    } else if (summary.measure=="AS") {
      expected.sm<-asin(sqrt(p.expected.curve[-which(treatment.levels==reference)]))-asin(sqrt(p.expected.curve[which(treatment.levels==reference)]))
    } else if (summary.measure=="RR") {
      expected.sm<-log(p.expected.curve[-which(treatment.levels==reference)])-log(p.expected.curve[which(treatment.levels==reference)])
    } else if (summary.measure=="OR") {
      expected.sm<-log(p.expected.curve[-which(treatment.levels==reference)]/(1-p.expected.curve[-which(treatment.levels==reference)]))-log(p.expected.curve[which(treatment.levels==reference)]/(1-p.expected.curve[which(treatment.levels==reference)]))
    }
    
    
    if (summary.measure%in%c("RR","OR")) {
      NI.marg<-log(NI.margin)
      upper<- log(res$up.bounds.CI)
      lower<- log(res$low.bounds.CI)
    } else {
      NI.marg<-NI.margin
      upper<- res$up.bounds.CI
      lower<- res$low.bounds.CI
    } 
    
    if (se.method=="delta") {
      var.n.fp<-((upper-lower)/(2*qnorm(1-sig.level)))^2
    } else {
      var.n.fp<-apply(res$boot.res$t,2,var)
    }
    
  var.1<-var.n.fp*n.tot       # Estimate of variance
  
  ss <- ((qnorm(sig.level)+qnorm(1-power))^2*var.1/(expected.sm-NI.marg)^2)/(1-ltfu)
  
  if (isTRUE(round)) {
    ss.total<-ceiling(ss)         # total sample size 
  } else {
    ss.total<-ss         # total sample size 
  }
  if (summary.measure=="target.risk") experimental.arms<-treatment.levels
  ss.total.optimal<-ss.total[which(experimental.arms==optimal)]
  ss.total.range<-min(ss.total[which(experimental.arms%in%range)])
  ss.total.acceptable<-min(ss.total)

  if (print.out==T) {
    
    if (se.method=="delta") {
      cat("The total sample sizes needed (across all arms) for the specified 
        expected curves and NI margins, accounting for ", ltfu*100, "% loss 
        to follow-up, are: \nOptimal power: ", ss.total.optimal,
          "\nRange power (conservative estimate):", ss.total.range,
          "\nAcceptable power (conservative estimate): ", ss.total.acceptable, ".\n")
    } else {
      
      var.1.up<-(sqrt(var.1)+qnorm(0.975)*sqrt(var.1)/sqrt(2*(M.boot-1)))^2
      var.1.low<-(sqrt(var.1)-qnorm(0.975)*sqrt(var.1)/sqrt(2*(M.boot-1)))^2
      ss.up <- ((qnorm(sig.level)+qnorm(1-power))^2*var.1.up/(expected.sm-NI.marg)^2)/(1-ltfu)
      ss.low <- ((qnorm(sig.level)+qnorm(1-power))^2*var.1.low/(expected.sm-NI.marg)^2)/(1-ltfu)
      if (isTRUE(round)) {
        ss.up.total<-ceiling(ss.up)        
        ss.low.total<-ceiling(ss.low)         
      } else {
        ss.up.total<-ss.up  
        ss.low.total<-ss.low
      }
      ss.up.total.optimal<-ss.up.total[which(experimental.arms==optimal)]
      ss.low.total.optimal<-ss.low.total[which(experimental.arms==optimal)]
      ss.up.total.range<-min(ss.up.total[which(experimental.arms%in%range)])
      ss.low.total.range<-min(ss.low.total[which(experimental.arms%in%range)])
      ss.up.total.acceptable<-min(ss.up.total)
      ss.low.total.acceptable<-min(ss.low.total)
      
      cat("The total sample sizes needed (across all arms) for the specified 
        expected curves and NI margins, accounting for ", ltfu*100, "% loss 
        to follow-up, are: \nOptimal power: ", ss.total.optimal,
          " (95% Monte-Carlo CI: [", ss.low.total.optimal, ", ", ss.up.total.optimal,
          "])\nRange power (conservative estimate):", ss.total.range,
          " (95% Monte-Carlo CI: [", ss.low.total.range, ", ", ss.up.total.range,
          "])\nAcceptable power (conservative estimate): ", ss.total.acceptable,
          " (95% Monte-Carlo CI: [", ss.low.total.acceptable, ", ", ss.up.total.acceptable,
          "]).\n")
      
    }
    
     
  }
  results<-list( ss.total=ss.total,
                 ss.total.optimal = ss.total.optimal,
                 ss.total.range = ss.total.range,
                 ss.total.acceptable = ss.total.acceptable,
                 res=res$boot.res)
  return(results)
  
}