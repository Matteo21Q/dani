power.ROCI.binary <- function (p.expected.curve, NI.margin, reference=max(treatment.levels),
                               optimal=min(treatment.levels), range=optimal, unfavourable=T,
                               se.method="bootstrap", treatment.levels, summary.measure="RD",
                               tr.model="FP2.classic", M.boot=NULL, parallel="no", n.cpus=1, sig.level=0.025,
                               n.per.arm, print.out=T, simulations=FALSE, n.rep=1000, bootCI.type="bca") {
  stopifnot(is.numeric(p.expected.curve), all(p.expected.curve < 1), all(p.expected.curve > 0))
  stopifnot(is.numeric(treatment.levels), length(treatment.levels)==length(p.expected.curve))
  stopifnot(is.logical(unfavourable), !is.na(unfavourable))
  stopifnot(is.logical(simulations), !is.na(simulations))
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
  stopifnot(is.numeric(n.per.arm), all(n.per.arm > 10), (length(n.per.arm)%in%c(1, length(treatment.levels))))
  stopifnot(is.numeric(n.rep), n.rep > 0)
  stopifnot(is.logical(print.out), !is.na(print.out))
  stopifnot(is.numeric(reference), length(reference)==1, reference%in%treatment.levels)
  stopifnot(is.numeric(optimal), length(optimal)==1, optimal%in%treatment.levels)
  stopifnot(is.numeric(range), all(range%in%treatment.levels))
  ref.index<-which(treatment.levels==reference)
  experimental.arms<-treatment.levels[-ref.index]
  if (!is.null(M.boot)) stopifnot(is.numeric(M.boot), M.boot>1)
  stopifnot(is.character(se.method), se.method%in%c("bootstrap", "delta"))
  stopifnot(is.character(bootCI.type), bootCI.type%in%c("norm","perc","bca","basic"))
  stopifnot(is.character(tr.model), tr.model%in%c("FP1.fixed","FP2.fixed", "FP1.classic", "FP2.classic", "FP01", "FP02"))
  stopifnot(is.character(parallel), parallel%in%c("no", "multicore", "snow"))
  if (tr.model%in%c("FP1.fixed","FP1.classic")) {
    if (length(treatment.levels)<3) stop ("With Fractional Polynomials with 1 power, at least 3 arms are needed.\n")
  } else {
    if (length(treatment.levels)<5) stop ("With Fractional Polynomials with 2 powers, at least 5 arms are needed.\n")
  }
  n.arms<-length(treatment.levels)
  n.tot<-ifelse(length(n.per.arm)==1,n.per.arm*n.arms, sum(n.per.arm))
  if (se.method=="bootstrap"&is.null(M.boot)) M.boot=n.tot
  n.comparisons<-ifelse(summary.measure=="target.risk", length(treatment.levels), length(treatment.levels)-1)
  if (length(NI.margin)==1) NI.margin<-rep(NI.margin, n.comparisons)
  # Generate data set with exactly expected outcomes:
  if (length(n.per.arm)==1) {
    treatment<-rep(treatment.levels, each=n.per.arm)
  } else {
    treatment<-rep(treatment.levels, n.per.arm)
  }  
  if (!isTRUE(simulations)) {
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
    power <- pnorm(sqrt(n.tot*(expected.sm-NI.marg)^2/var.1)-qnorm(1-sig.level))*100
    if (summary.measure=="target.risk") experimental.arms<-treatment.levels
    power.optimal<-power[which(experimental.arms==optimal)]
    power.range<-max(power[which(experimental.arms%in%range)])
    power.acceptable<-max(power)
    
    if (print.out==T) {
      if (se.method=="delta") {
        cat("The power with a total sample size of ", n.tot, " and  for the specified
expected curves and NI margins is: \nOptimal power: ", power.optimal,
            "% \nRange power (conservative estimate):", power.range,
            "% \nAcceptable power (conservative estimate): ", power.acceptable, "%.\n")
      } else {
        var.1.up<-(sqrt(var.1)+qnorm(0.975)*sqrt(var.1)/sqrt(2*(M.boot-1)))^2
        var.1.low<-(sqrt(var.1)-qnorm(0.975)*sqrt(var.1)/sqrt(2*(M.boot-1)))^2
        power.up <- 100*pnorm(sqrt(n.tot*(expected.sm-NI.marg)^2/var.1.low)-qnorm(1-sig.level))
        power.low <- 100*pnorm(sqrt(n.tot*(expected.sm-NI.marg)^2/var.1.up)-qnorm(1-sig.level))
        power.up.optimal<-power.up[which(experimental.arms==optimal)]
        power.low.optimal<-power.low[which(experimental.arms==optimal)]
        power.up.range<-max(power.up[which(experimental.arms%in%range)])
        power.low.range<-max(power.low[which(experimental.arms%in%range)])
        power.up.acceptable<-max(power.up)
        power.low.acceptable<-max(power.low)
        cat("The power with a total sample size of ", n.tot, " and  for the specified
expected curves and NI margins is: \nOptimal power: ", power.optimal,
            "% (95% Monte-Carlo CI: [", power.low.optimal, "%, ", power.up.optimal,
            "%])\nRange power (conservative estimate):", power.range,
            "% (95% Monte-Carlo CI: [", power.low.range, "%, ", power.up.range,
            "%])\nAcceptable power (conservative estimate): ", power.acceptable,
            "% (95% Monte-Carlo CI: [", power.low.acceptable, "%, ", power.up.acceptable,
            "%]).\n")
      }
    }
    
  } else {
    
    if (summary.measure=="target.risk") experimental.arms<-treatment.levels
    n.ea<-length(experimental.arms)
    ni.indicator<-matrix(NA, n.rep, n.ea)
    
    for (i in 1:n.rep) {
      
      outcomes<-NULL
      for (tl in 1:n.arms) {
        
        n.per.arm.i<-ifelse(length(n.per.arm)==1,n.per.arm, n.per.arm[tl])
        outcomes<-c(outcomes, rbinom(n.per.arm.i,1,p.expected.curve[tl]))
        
      }
      
      dat<-data.frame(outcomes,treatment)
      # Fit fractional polynomials on expected outcomes data set:
      myformula<-as.formula("outcomes~treat(treatment)")
      res<-test.ROCI.binary(formula=myformula, data=dat,  reference = reference, unfavourable=unfavourable,
                            se.method=se.method, treatment.levels=treatment.levels, summary.measure=summary.measure,
                            NI.margin=NI.margin, sig.level=sig.level, parallel=parallel, n.cpus=n.cpus,
                            tr.model=tr.model, M.boot=M.boot, bootCI.type = bootCI.type)
      if (isTRUE(unfavourable)) {
        
        ni.indicator[i,]<-((res$up.bounds.CI-NI.margin)<0)
        
      } else {
        
        ni.indicator[i,]<-((res$low.bounds.CI-NI.margin)>0)
        
      }
      
      if (isTRUE(print.out)) {
        if (i%%50==0) cat(".")
        if (i%%1000==0) cat("\n")
      }
      
    }
    
    power<-apply(ni.indicator,2,mean)*100
    if (summary.measure=="target.risk") experimental.arms<-treatment.levels
    power.optimal<-power[which(experimental.arms==optimal)]
    power.range<-max(power[which(experimental.arms%in%range)])
    power.acceptable<-max(power)
    
    if (print.out==T) {
      
        MC.SE<-sqrt(power*(100-power)/n.rep)
        power.up <- power+MC.SE*qnorm(0.975)
        power.low <- power-MC.SE*qnorm(0.975)
        power.up.optimal<-power.up[which(experimental.arms==optimal)]
        power.low.optimal<-power.low[which(experimental.arms==optimal)]
        power.up.range<-max(power.up[which(experimental.arms%in%range)])
        power.low.range<-max(power.low[which(experimental.arms%in%range)])
        power.up.acceptable<-max(power.up)
        power.low.acceptable<-max(power.low)
        cat("The power with a total sample size of ", n.tot, " and  for the specified
expected curves and NI margins is: \nOptimal power: ", power.optimal,
            "% (95% Monte-Carlo CI: [", power.low.optimal, "%, ", power.up.optimal,
            "%])\nRange power (conservative estimate):", power.range,
            "% (95% Monte-Carlo CI: [", power.low.range, "%, ", power.up.range,
            "%])\nAcceptable power (conservative estimate): ", power.acceptable,
            "% (95% Monte-Carlo CI: [", power.low.acceptable, "%, ", power.up.acceptable,
            "%]).\n")
      
    }
  }
  

  results<-list( power=power,
                 power.optimal = power.optimal,
                 power.range = power.range,
                 power.acceptable = power.acceptable)
  return(results)
}