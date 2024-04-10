power.NI.continuous <- function (mean.control, mean.experim, sd, NI.margin, sig.level = 0.025, 
                                      n.control, n.experim, summary.measure = "mean.difference", print.out = TRUE, 
                                      test.type=NULL, higher.better=T, M.boot=2000, n.rep=1000, bootCI.type="norm") 
{
  
  stopifnot(is.numeric(mean.control))
  stopifnot(is.numeric(mean.experim))
  stopifnot(is.numeric(sd), sd > 0)
  stopifnot(is.numeric(sig.level), sig.level < 0.5, sig.level > 0)
  stopifnot(is.numeric(n.control), n.control > 0)
  stopifnot(is.numeric(n.experim), n.experim > 0)
  stopifnot(is.numeric(n.rep), n.rep > 0)
  stopifnot(is.numeric(M.boot), M.boot > 0)
  stopifnot(is.character(summary.measure), summary.measure %in%c("mean.difference", "mean.ratio"))
  stopifnot(is.numeric(NI.margin))
  stopifnot(is.logical(print.out), !is.na(print.out))
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
  
  ni.indicator<-rep(NA, n.rep)
  
  for (i in 1:n.rep) {
    
    y.control<-rnorm(n.control, mean.control, sd)
    y.experim<-rnorm(n.experim, mean.experim, sd)
    
    fit.ed<-test.NI.continuous(y.control=y.control, y.experim=y.experim,  
                               NI.margin=NI.margin, sig.level=sig.level, summary.measure=summary.measure, 
                               print.out=FALSE, higher.better=higher.better, test.type=test.type,
                               M.boot=M.boot, sd.control=sd, sd.experim=sd, bootCI.type = bootCI.type)
    
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