samplesize.ROCI.binary <- function (p.expected.curve, NI.margin, minimisation=T, unfavourable=T,
                              se.method="bootstrap", treatment.levels, summary.measure="RD", 
                              tr.model="FP2.fixed", M.boot=NULL, parallel="no", n.cpus=1, bootCI.type="bca", sig.level=0.025,
                              n.per.arm=100, power=0.8, print.out=T, varest.boot="CI.bc") {
  
  stopifnot(is.numeric(p.expected.curve), all(p.expected.curve < 1), all(p.expected.curve > 0))
  stopifnot(is.numeric(treatment.levels), length(treatment.levels)==length(p.expected.curve))
  stopifnot(is.logical(unfavourable), !is.na(unfavourable))
  stopifnot(is.numeric(NI.margin), (length(NI.margin)==1)||(length(NI.margin)==(length(treatment.levels)-(summary.measure!="target.risk"))))
  if (length(NI.margin)==1) NI.margin<-rep(NI.margin, length(treatment.levels)-(summary.measure!="target.risk"))
  if (summary.measure=="RD") {
    if ((unfavourable == T)&&any(NI.margin<=0)) stop("When outcome is unfavourable, risk difference NI margins need to all be positive.\n")
    if ((unfavourable == F)&&any(NI.margin>=0)) stop("When outcome is favourable, risk difference NI margins needs to all be negative.\n")
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
  stopifnot(is.numeric(n.per.arm), n.per.arm > 10)
  stopifnot(is.character(summary.measure),(( summary.measure == "RD" ) || ( summary.measure == "RR" ) || ( summary.measure == "OR" ) || ( summary.measure == "target.risk" )))
  stopifnot(is.logical(print.out), !is.na(print.out))
  stopifnot(is.logical(minimisation), !is.na(minimisation))
  if (!is.null(M.boot)) stopifnot(is.numeric(M.boot), M.boot>1)
  stopifnot(is.character(se.method), se.method%in%c("bootstrap", "delta"))
  stopifnot(is.character(bootCI.type), bootCI.type%in%c("norm","perc","bca","basic"))
  stopifnot(is.character(tr.model), tr.model%in%c("FP1.fixed","FP2.fixed", "FP1.classic", "FP2.classic", "FP01", "FP02"))
  stopifnot(is.character(parallel), parallel%in%c("no", "multicore", "snow"))
  stopifnot(is.character(varest.boot), varest.boot%in%c("direct", "CI.bc"))
  
  if (tr.model%in%c("FP1.fixed","FP1.classic")) {
    if (length(treatment.levels)<3) stop ("With Fractional Polynomials with 1 power, at least 3 arms are needed.\n")
  } else {
    if (length(treatment.levels)<5) stop ("With Fractional Polynomials with 2 powers, at least 5 arms are needed.\n")
  }
  n.arms<-length(treatment.levels)
  n.tot<-n.arms*n.per.arm
  n.comparisons<-ifelse(summary.measure=="probability", length(treatment.levels), length(treatment.levels)-1)
  if (length(NI.margin)==1) NI.margin<-rep(NI.margin, n.comparisons)
  
  # Generate data set with exactly expected outcomes:
  outcomes<-rep(0,n.arms*n.per.arm)
  for (nar in 1:n.arms) {
    outcomes[(1+(nar-1)*n.per.arm):(p.expected.curve[nar]*n.per.arm+(nar-1)*n.per.arm)]<-1
  }
  treatment<-c(rep(treatment.levels, each=n.per.arm))
  dat<-data.frame(outcomes,treatment)
  
  # Fit fractional polynomials on expected outcomes data set:
  myformula<-as.formula("outcomes~treat(treatment)")
  res<-test.ROCI.binary(formula=myformula, data=dat,  minimisation = minimisation, unfavourable=unfavourable,
                         se.method=se.method, treatment.levels=treatment.levels, summary.measure=summary.measure, 
                         NI.margin=NI.margin, sig.level=sig.level, parallel=parallel, n.cpus=n.cpus,
                        tr.model=tr.model, M.boot=M.boot, bootCI.type=bootCI.type)

  if (minimisation==T) {
    if (summary.measure=="RD") {
      expected.sm<-p.expected.curve[length(p.expected.curve)]-p.expected.curve[1:n.comparisons]
    } else if (summary.measure=="RR") {
      expected.sm<-log(p.expected.curve[length(p.expected.curve)])-log(p.expected.curve[1:n.comparisons])
    } else if (summary.measure=="OR") {
      expected.sm<-log(p.expected.curve[length(p.expected.curve)]/(1-p.expected.curve[length(p.expected.curve)]))-log(p.expected.curve[1:n.comparisons]/(1-p.expected.curve[1:n.comparisons]))
    }
  } else {
    if (summary.measure=="RD") {
      expected.sm<-p.expected.curve[1]-p.expected.curve[2:(n.comparisons+1)]
    } else if (summary.measure=="RR") {
      expected.sm<-log(p.expected.curve[1])-log(p.expected.curve[2:(n.comparisons+1)])
    } else if (summary.measure=="OR") {
      expected.sm<-log(p.expected.curve[1]/(1-p.expected.curve[1]))-log(p.expected.curve[2:(n.comparisons+1)]/(1-p.expected.curve[2:(n.comparisons+1)]))
    }
    
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
  if (isTRUE(unfavourable)) {
    NI<-(expected.sm-NI.marg<0)
  } else {
    NI<-(expected.sm-NI.marg>0)
  }

  if (minimisation==T) {
    upper<-upper[1:n.comparisons]
    lower<-lower[1:n.comparisons]
    optimal.i<-min(which(NI==T))
  } else {
    upper<-upper[2:(n.comparisons+1)]
    lower<-lower[2:(n.comparisons+1)]
    optimal.i<-max(which(NI==T))
  }

  if (varest.boot=="CI.bc") {
    var.n.fp<-((upper-lower)/(2*qnorm(1-sig.level)))^2
  } else {
    var.n.fp<-apply(res$boot.res$t,2,var)
  }
  var.1<-var.n.fp*n.tot       # Estimate of variance using delta method
  
  ss.total<-ceiling((qnorm(sig.level)+qnorm(1-power))^2*var.1/(expected.sm-NI.marg)^2)         # total sample size using delta
  ss.perarm<-ceiling(ss.total/n.arms)            # sample size per arm using delta
  ss.total.optimal<-ss.total[optimal.i]
  ss.perarm.optimal<-ss.perarm[optimal.i]
  ss.total.acceptable<-min(ss.total)
  ss.perarm.acceptable<-min(ss.perarm)

  if (print.out==T) {
    
    cat("The total sample sizes needed (across all arms) for the specified expected curves and NI margins are: \nOptimal power: ", ss.total.optimal,
        "\nAcceptable power (conservative estimate): ", ss.total.acceptable, ".\n")
    
  }
  results<-list( ss.total=ss.total,
                 ss.perarm = ss.perarm,
                 ss.total.optimal = ss.total.optimal,
                 ss.total.acceptable = ss.total.acceptable,
                 ss.perarm.optimal = ss.perarm.optimal,
                 ss.perarm.acceptable = ss.perarm.acceptable,
                 res=res$boot.res)
  return(results)
  
}