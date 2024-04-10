samplesize.NI.continuous <- function (mean.control, mean.experim, sd, NI.margin, sig.level = 0.025, 
                                   power = 0.9, r = 1, summary.measure = "mean.difference", print.out = TRUE, 
                                   test.type=NULL, higher.better=T, round=T, ltfu=0) 
{
  
  stopifnot(is.numeric(mean.control))
  stopifnot(is.numeric(mean.experim))
  stopifnot(is.numeric(sd), sd > 0)
  stopifnot(is.numeric(sig.level), sig.level < 0.5, sig.level > 0)
  stopifnot(is.numeric(power), power < 1, power > 0)
  stopifnot(is.numeric(r), r > 0)
  stopifnot(is.logical(round), !is.na(round))
  stopifnot(is.numeric(ltfu), ltfu < 1, ltfu >= 0)
  stopifnot(is.character(summary.measure), summary.measure %in%c("mean.difference", "mean.ratio"))
  if (is.null(test.type)) {
    test.type<-ifelse(summary.measure=="mean.difference", "t.test", "Fiellers" )
  }
  stopifnot(is.numeric(NI.margin))
  stopifnot(is.logical(print.out), !is.na(print.out))
  if (summary.measure=="mean.difference") stopifnot(is.character(test.type), test.type %in% c("t.test", "Z.test"))
  if (summary.measure=="mean.ratio") stopifnot(is.character(test.type), test.type %in% c("log.t.test", "log.Z.test", "Fiellers"))
  stopifnot(is.logical(higher.better), !is.na(higher.better))
  if ((summary.measure=="mean.ratio")&&(mean.control==0)) stop("The ratio of means is not an appropriate summary measure when the expected control mean is 0. Hence only the difference in means is appropriate.\n")
  if ((NI.margin==0)&&(summary.measure=="mean.difference")) stop ("A Non-inferiority margin of 0 for the mean difference means this is a superiority trial.")
  if ((NI.margin==1)&&(summary.measure=="mean.ratio")) stop ("A Non-inferiority margin of 1 for the mean ratio means this is a superiority trial.")
  
  mean.exp.null<-ifelse(summary.measure=="mean.difference", mean.control+NI.margin, mean.control*NI.margin)
  
  if (higher.better==F) {
    if (mean.experim>=mean.exp.null) stop("In the alternative hypothesis the experimental treatment is not non-inferior. Mean outcome in experimental arm=",
                                       mean.experim, ", which is greater or equal than the minimum non-tolerable mean outcome=", mean.exp.null,".\nPlease check again all parameter values. Alternatively makes sure you have specified correctly whether your outcome is such that higher values are better.")
  } else {
    if (mean.experim<=mean.exp.null) stop("In the alternative hypothesis the experimental treatment is not non-inferior. Mean outcome in experimental arm=",
                                       mean.experim, ", which is lower or equal than the maximum non-tolerable mean outcome=", mean.exp.null,".\nPlease check again all parameter values. Alternatively makes sure you have specified correctly whether your outcome is such that higher values are better.")
    
  }
  
  r<-1/r  #easier to write formula for control/active allocation ratio
  
  if (summary.measure == "mean.difference") {
    
    mean.alt <- mean.experim - mean.control - NI.margin
    
  } else {
    
    NI.margin.log <- log(NI.margin)
    mean.alt <- log(mean.experim/mean.control) - NI.margin.log
    
  } 
  
  if (test.type!="Fiellers") {
    n = ( qnorm(1 - sig.level) + qnorm(power))^2 *2* (sd^2/(mean.alt)^2)
    
    if (test.type=="log.t.test"||test.type=="t.test") {
      check<-0
      n.try<-ifelse(isTRUE(round),ceiling(n),n)
      while ((check==0)&&(n.try<ceiling(n)+10000)) {
        power.est<-1-pt(qt(1-sig.level, n.try-2), n.try-2, abs((mean.alt*sqrt(n.try))/(sd)))
        if (power.est>=power) {
          check<-1
          n<-n.try
        } else {
          n.try<-n.try+1
          if (n.try==ceiling(n)+1000) warning("Wasn't able to solve for t distribution, the sample size calculation is for the Z test and therefore assumes known variance.\n")
        }
      }
    }
    
    n = n/(1-ltfu)
    if (isTRUE(round)) {
      ss <- c(nC <- ceiling(ceiling(n* r) ), nE <- ceiling(n))
    } else {
      ss <- c(nC <- n* r , nE <- n)
    }
    
  } else {
    if (r!=1) stop("Fiellers method currently supported for equal allocation only (r=1).\n")
    output<-capture.output(n.ratio(m=1, rho=NI.margin, Power=power, CV0=sd/mean.control, rho.star=mean.experim/mean.control,
                                        alpha=2*sig.level))
    ss<-rep(as.numeric(substr(output[2], 42,nchar(output[2])-1 )),2)
    ss<-ceiling(ss/(1-ltfu))
    
    if (!isTRUE(round)) warning("round=F not allowed for method Fiellers. Rounding estimated sample sizes instead. ")
    }
  
 
  if (print.out == T) {
    cat("Power:", (power) * 100, "%\nOne-sided significance level:", 
        sig.level * 100, "%.\nMean in control arm =", 
        mean.control, "\nMean in experimental arm =", 
        mean.experim, "\nNon-acceptable mean in experimental arm (null H) =",
        mean.exp.null,"\nExpected loss to follow-up: ",
        ltfu*100, "%\n"
    )
    if (summary.measure == "mean.difference") {
      cat("The sample size required to test non-inferiority within a", 
          NI.margin, " mean difference NI margin is:")
    }
    else if (summary.measure == "mean.ratio") {
      cat("The sample size required to test non-inferiority within a", 
          NI.margin, " mean ratio margin is:")
    }
    cat("\n", ss[1], " individuals in the control group.\n", 
        ss[2], " individuals in the experimental group.\n")
  }
  return(ss)
}