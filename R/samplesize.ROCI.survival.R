samplesize.ROCI.survival <- function (rates, shapes, NI.margin, reference=max(treatment.levels), 
                                    power.type, power.arms, unfavourable=T, r=NULL,
                                    se.method=NULL, treatment.levels, treatment.arms=treatment.levels, 
                                    summary.measure="HR", tr.model="FP2.select", M.boot=NULL, parallel="no", cl=NULL,
                                    n.cpus=1, sig.level=0.025, n.tot.start=NULL, power=0.8, print.out=T, round=T,
                                    k=2, knots=NULL, bknots=NULL, tau=NULL, 
                                    rate.censor=0, follow.up=NULL, iterative=T, recruitment=NULL) {
  
  stopifnot(is.numeric(rates), all(rates > 0))
  stopifnot(is.numeric(shapes), all(shapes > 0))
  
  stopifnot(is.numeric(treatment.levels), length(treatment.levels)==length(shapes), length(treatment.levels)==length(rates))
  stopifnot(is.numeric(treatment.arms), length(treatment.levels)>=length(treatment.arms), all(treatment.arms%in%treatment.levels))
  n.arms<-length(treatment.arms)
  if (is.null(r)) {
    r<-rep(1,n.arms)
  } else {
    stopifnot(is.numeric(r), length(r)==length(treatment.arms), all(r>0))
    
  }
  stopifnot(is.logical(unfavourable), !is.na(unfavourable))
  stopifnot(is.character(summary.measure),(( summary.measure == "HR" ) || ( summary.measure == "DS" ) || ( summary.measure == "DRMST" ) || ( summary.measure == "RS" )))
  stopifnot(is.numeric(NI.margin), (length(NI.margin)==1)||(length(NI.margin)==(length(treatment.levels)-1)))
  if (length(NI.margin)==1) NI.margin<-rep(NI.margin, length(treatment.levels)-1)
  stopifnot(is.numeric(sig.level), sig.level < 0.5, sig.level > 0)
  stopifnot(is.numeric(power), power < 1, power > 0)
  if (is.null(n.tot.start)) {
    n.tot.start<-ceiling(sum(r)*100)
  } else {
    stopifnot(is.numeric(n.tot.start), n.tot.start > sum(r*2), length(n.tot.start)==1)
  }
  stopifnot(is.logical(print.out), !is.na(print.out))
  stopifnot(is.logical(round), !is.na(round))
  stopifnot(is.numeric(rate.censor), rate.censor >= 0)
  stopifnot(is.numeric(reference), length(reference)==1, reference%in%treatment.levels)
  stopifnot(is.character(power.type), power.type%in%c("optimal", "acceptable"))
  stopifnot(is.numeric(power.arms), all(power.arms%in%treatment.levels), all(power.arms!=reference))
  if (power.type=="optimal") {
    stopifnot(length(power.arms)==1)
  } 
  ref.index<-which(treatment.levels==reference)
  experimental.levels<-treatment.levels[-ref.index]
  
  if (!is.null(M.boot)) {
    stopifnot(is.numeric(M.boot), M.boot>1)
  } else {
    M.boot=n.tot.start
  }
  if (is.null(se.method)) {
    se.method<-ifelse(power.type=="acceptable", "empirical.bootstrap", "bootstrap")
  } else {
    stopifnot(is.character(se.method), se.method%in%c("bootstrap", "delta", "empirical.bootstrap"))
  }
  stopifnot(is.character(tr.model), tr.model%in%c("FP1.fixed","FP2.fixed", "FP1.select", "FP2.select", "FP01", "FP02"))
  stopifnot(is.character(parallel), parallel%in%c("no", "multicore", "snow"))
  stopifnot(is.logical(iterative), !is.na(iterative))
  
  if (is.null(recruitment)) {
    recruitment<-function(x) return(0)
  } else {
    stopifnot(is.function(recruitment))
  }
  
  stopifnot(is.numeric(follow.up), follow.up>0)
  
  if (is.null(tau)&summary.measure=="HR") tau<-1
  stopifnot(is.numeric(tau), tau>0)
  
  if (tr.model%in%c("FP1.fixed","FP1.select")) {
    if (length(treatment.arms)<3) stop ("With Fractional Polynomials with 1 power, at least 3 arms are needed.\n")
  } else {
    if (length(treatment.arms)<5) stop ("With Fractional Polynomials with 2 powers, at least 5 arms are needed.\n")
  }
  unit.per.arm<-ceiling(n.tot.start/sum(r))
  n.per.arm<-r*unit.per.arm
  n.tot.start<-sum(n.per.arm)
  n.comparisons<- length(treatment.levels)-1
  if (length(NI.margin)==1) NI.margin<-rep(NI.margin, n.comparisons)

  if (isTRUE(iterative)) {
    
    # Get first estimate of sample size:
    
    n.tot<-samplesize.ROCI.survival(rates=rates, shapes=shapes, NI.margin=NI.margin, reference=reference, 
                                    power.type=power.type, power.arms=power.arms, unfavourable=unfavourable, r=r,
                                    se.method="delta", treatment.levels=treatment.levels, treatment.arms=treatment.arms, 
                                    summary.measure=summary.measure, tr.model=tr.model, M.boot=M.boot, parallel=parallel, 
                                    cl=cl, n.cpus=n.cpus, sig.level=sig.level, n.tot.start=n.tot.start, power=power, 
                                    print.out=FALSE, round=FALSE,
                                    k=k, knots=knots, bknots=bknots, tau=tau, 
                                    rate.censor=rate.censor, follow.up=follow.up, iterative=F, recruitment=recruitment)$ss.total
    
  } else {
    n.tot<-n.tot.start
  }
  
  # Generate data set with exactly expected outcomes:
  unit.per.arm<-ceiling(n.tot/sum(r))
  n.per.arm<-r*unit.per.arm
  n.tot<-sum(n.per.arm)
  treatment<-rep(treatment.arms, n.per.arm)
  event.time<-rep(NA,n.tot)
  curr.k<-1
  for (nar in 1:n.arms) {
    n.per.arm.i<-ifelse(length(n.per.arm)==1,n.per.arm, n.per.arm[nar])
    event.time[curr.k:(n.per.arm.i+curr.k-1)]<-qweibull(seq(0.001,0.999,length.out=n.per.arm.i), shapes[nar],1/rates[nar])
    curr.k<-curr.k+n.per.arm.i
  }
  event.status<-event.time<follow.up
  dat<-data.frame(event.time, event.status,treatment)
  
  list.ss<-list(recruitment, follow.up, rate.censor)
  
  # Fit fractional polynomials on expected outcomes data set:
  myformula<-as.formula("Surv(event.time, event.status)~treat(treatment)")
  se.method.analysis<-ifelse(se.method=="empirical.bootstrap", "bootstrap", se.method)
  res<-test.ROCI.survival(formula=myformula, data=dat,  reference = reference, unfavourable=unfavourable,
                        se.method=se.method.analysis, treatment.levels=treatment.levels, summary.measure=summary.measure, 
                        NI.margin=NI.margin, sig.level=sig.level, parallel=parallel, n.cpus=n.cpus, cl=cl,
                        tr.model=tr.model, M.boot=M.boot, bootCI.type = "basic",
                        k=2, knots=NULL, bknots=NULL, tau=tau, list.ss=list.ss)
  
  survs<-1-pweibull(tau,shapes,1/rates)
  if (summary.measure=="HR") {
    expected.sm<-log(rates[-which(treatment.levels==reference)]/rates[which(treatment.levels==reference)])
  } else if (summary.measure=="DS") {
    expected.sm<-survs[-which(treatment.levels==reference)]-survs[which(treatment.levels==reference)]
  } else if (summary.measure=="RS") {
    expected.sm<-log(survs[-which(treatment.levels==reference)]/survs[which(treatment.levels==reference)])
  } else if (summary.measure=="DRMST") {
    expected.sm<-res$estimates
  }
  
  
  if (summary.measure%in%c("HR","RS")) {
    NI.marg<-log(NI.margin)
    upper<- log(res$up.bounds.CI)
    lower<- log(res$low.bounds.CI)
    if (se.method!="delta") ests<-log(res$boot.res$t)
  } else {
    NI.marg<-NI.margin
    upper<- res$up.bounds.CI
    lower<- res$low.bounds.CI
    if (se.method!="delta") ests<-res$boot.res$t
  } 
  
  if (se.method=="delta") {
    var.n.fp<-((upper-lower)/(2*qnorm(1-sig.level)))^2
  } else {
    var.n.fp<-apply(ests,2,var, na.rm=T)
  }
  
  var.1<-var.n.fp*n.tot       # Estimate of variance
  if (se.method!="delta") {
    var.1.up<-(sqrt(var.1)+qnorm(0.975)*sqrt(var.1)/sqrt(2*(M.boot-1)))^2
    var.1.low<-(sqrt(var.1)-qnorm(0.975)*sqrt(var.1)/sqrt(2*(M.boot-1)))^2
  }
  if (se.method!="empirical.bootstrap") {
    ss <- ((qnorm(sig.level)+qnorm(1-power))^2*var.1/(expected.sm-NI.marg)^2)
    if (se.method=="bootstrap") {
      ss.up <- ((qnorm(sig.level)+qnorm(1-power))^2*var.1.up/(expected.sm-NI.marg)^2)
      ss.low <- ((qnorm(sig.level)+qnorm(1-power))^2*var.1.low/(expected.sm-NI.marg)^2)
    } 
  }
  
  if (se.method=="empirical.bootstrap") {
    
    v<-which(experimental.levels%in%power.arms)
    
    if (power.type=="optimal") {
      
      if (isTRUE(unfavourable)) {
        sol.finder<-function(x, which.bound, var.curr) {
          pow.est<-mean((ests[,v]/(sqrt(x/n.tot))+qnorm(1-sig.level)*sqrt(var.curr[v]/x))<NI.marg)
          pow.est<-pow.est+which.bound*qnorm(0.975)*sqrt(pow.est*(1-pow.est)/M.boot)
          pow.est-power
        }
      } else {
        sol.finder<-function(x, which.bound, var.curr) {
          pow.est<-mean((ests[,v]/(sqrt(x/n.tot))-qnorm(1-sig.level)*sqrt(var.curr[v]/x))>NI.marg)
          pow.est<-pow.est+which.bound*qnorm(0.975)*sqrt(pow.est*(1-pow.est)/M.boot)
          pow.est-power
        }
      }
    } else if (power.type=="acceptable") {
      
      if (isTRUE(unfavourable)) {
        sol.finder<-function(x, which.bound, var.curr) {
          pow.est<-mean(apply(t(t(ests[,v]/(sqrt(x/n.tot)))+qnorm(1-sig.level)*sqrt(var.curr[v]/x))<NI.marg[v],1,any))
          pow.est<-pow.est+which.bound*qnorm(0.975)*sqrt(pow.est*(1-pow.est)/M.boot)
          pow.est-power
        }
      } else {
        sol.finder<-function(x, which.bound, var.curr) {
          pow.est<-mean(apply(t(t(ests[,v]/(sqrt(x/n.tot)))-qnorm(1-sig.level)*sqrt(var.curr[v]/x))>NI.marg[v],1,any))
          pow.est<-pow.est+which.bound*qnorm(0.975)*sqrt(pow.est*(1-pow.est)/M.boot)
          pow.est-power
        }
      } 
      
    }
    
    ss<-uniroot(sol.finder, c(1, 10^8), tol = 0.0001, which.bound=0, var.curr=var.1)$root
    ss.up<-uniroot(sol.finder, c(1, 10^8), tol = 0.0001, which.bound=-1, var.curr=var.1.up)$root
    ss.low<-uniroot(sol.finder, c(1, 10^8), tol = 0.0001, which.bound=1, var.curr=var.1.low)$root
    
  }
  
  if (isTRUE(round)) {
    ss<-ceiling(ss)         # total sample size 
  } 
  
  if (se.method!="empirical.bootstrap") {
    ss.total<-min(ss[which(experimental.levels%in%power.arms)])
  } else {
    ss.total=ss
  }
  if (se.method!="delta") {
    if (isTRUE(round)) {
      ss.up<-ceiling(ss.up)        
      ss.low<-ceiling(ss.low)         
    } 
    if (se.method=="bootstrap") {
      ss.total.up<-ss.up[which(ss==ss.total)]
      ss.total.low<-ss.low[which(ss==ss.total)]
    } else {
      ss.total.up<-ss.up
      ss.total.low<-ss.low
    }
    ss.total.CI<-c(ss.total.low, ss.total.up)
    
  } else {
    ss.total.CI<-NULL
  }
  
  
  if (print.out==T) {
    
    if (se.method=="delta") {
      cat("Reference arm: ", reference, "\nPower type: ", power.type, 
          " power for the following arm(s): ", power.arms, "\nTotal sample size needed (across all arms): ", ss.total, ".\n")
    } else {
      
      
      
      cat("Reference arm: ", reference, "\nPower type: ", power.type, 
          " power for the following arm(s): ", power.arms, "\nTotal sample size (across all arms): ", ss.total,
          " (95% Monte-Carlo CI: [", ss.total.low, ", ", ss.total.up,
          "]).\n")
      
    }
    
    
  }
  results<-list( ss.total=ss.total,
                 ss.total.CI=ss.total.CI,
                 power.type=power.type,
                 power.arms=power.arms,
                 res=res$boot.res)
  return(results)
  
}