samplesize.NI.survival <- function (HR.target=NULL, p.control.expected=NULL, p.experim.target=NULL, t.expected,
                                     NI.margin, sig.level = 0.025, power = 0.9, r = 1, 
                                     summary.measure = "HR", print.out = TRUE, 
                                       test.type="logrank.Schoenfeld", unfavourable=T, round=T, ltfu=0) 
{
  
  if (!is.null(HR.target)) stopifnot(is.numeric(HR.target), HR.target>0)
  if (!is.null(p.control.expected)) stopifnot(is.numeric(p.control.expected), p.control.expected>0, p.control.expected<=1)
  if (!is.null(p.experim.target)) stopifnot(is.numeric(p.experim.target), p.experim.target>0, p.experim.target<=1)
  if (!is.null(p.experim.target)&&!is.null(HR.target)) stop("Please only provide one of either p.experim.target or HR.target.\n")
  
  stopifnot(is.numeric(sig.level), sig.level < 0.5, sig.level > 0)
  stopifnot(is.numeric(power), power < 1, power > 0)
  stopifnot(is.numeric(r), r > 0)
  stopifnot(is.logical(round), !is.na(round))
  stopifnot(is.numeric(ltfu), ltfu < 1, ltfu >= 0)
  stopifnot(is.character(summary.measure), summary.measure %in%c("HR", "DRMST"))
  stopifnot(is.numeric(NI.margin))
  stopifnot(is.numeric(t.expected), t.expected>0)
  if (summary.measure=="HR") stopifnot(NI.margin>0)
  stopifnot(is.logical(print.out), !is.na(print.out))
  stopifnot(is.character(test.type), test.type %in% c("logrank.Freedman", "logrank.Schoenfeld", "KM"))
  stopifnot(is.logical(unfavourable), !is.na(unfavourable))
  if ((summary.measure=="DRMST")&&(test.type!="KM")) stop("Only a test based on Kaplan Meier is currently supported for DRMST.\n")
  if ((summary.measure=="HR")&&(test.type=="KM")) stop("Only logrank test formulae (logrank.Schoenfeld and logrank.Freedman) are currently supported for HR.\n")
  if ((NI.margin==0)&&(summary.measure=="DRMST")) stop ("A Non-inferiority margin of 0 for DRMST means this is a superiority trial.")
  if ((NI.margin==1)&&(summary.measure=="HR")) stop ("A Non-inferiority margin of 1 for the hazard ratio means this is a superiority trial.")
  if (unfavourable==T&&summary.measure=="HR"&&NI.margin<1) stop("With an unfavourable outcome, the NI margin for HR should be >1.")
  if (unfavourable==T&&summary.measure=="DRMST"&&NI.margin>0) stop("With an unfavourable outcome, the NI margin for DRMST should be <0.")
  if (unfavourable==F&&summary.measure=="HR"&&NI.margin>1) stop("With a favourable outcome, the NI margin for HR should be <1.")
  if (unfavourable==F&&summary.measure=="DRMST"&&NI.margin<0) stop("With a favourable outcome, the NI margin for DRMST should be >0.")
  
  if (unfavourable==F&&summary.measure=="HR") {
    if (HR.target<=NI.margin) stop("In the alternative hypothesis the experimental treatment is not non-inferior. Expected hazard ratio=",
                                               HR.target, ", which is lower or equal to the maximum non-tolerable hazard ratio =", NI.margin,".\nPlease check again all parameter values. Alternatively make sure you have specified correctly whether your outcome is favourable (i.e. events are positive).")
  } else if (unfavourable==T&&summary.measure=="HR") {
    if (HR.target>=NI.margin) stop("In the alternative hypothesis the experimental treatment is not non-inferior. Expected hazard ratio=",
                                         HR.target, ", which is greater or equal to the minimum non-tolerable hazard ratio =", NI.margin,".\nPlease check again all parameter values. Alternatively make sure you have specified correctly whether your outcome is unfavourable (i.e. events are negative).")
  }
  
  rate.control.expected<-uniroot(fun.rate, c(1E-6, 1E6), tol = 0.000001, 
                                 target.surv=(p.control.expected), time=t.expected)$root
  if (!is.null(HR.target)) {
    rate.experim.target<-rate.control.expected*HR.target
    p.experim.target <- pexp( t.expected, rate.experim.target )
    
  } else {
    rate.experim.target<-uniroot(fun.rate, c(1E-6, 1E6), tol = 0.000001, 
                                   target.surv=(1-p.experim.target), time=t.expected)$root
    HR.target <- rate.experim.target / rate.control.expected
  }
  
  prop.control<-1/(1+r) 
  prop.exp<-1-prop.control
  
  if (summary.measure == "HR") {
    
    if (test.type=="logrank.Schoenfeld") {
      n.ev <- (qnorm(1 - sig.level) + qnorm(power))^2/((log(HR.target) - log(NI.margin))^2 * 
                                                 prop.control * prop.exp)  
      n <- n.ev*(1+1/r)/((p.control.expected)/r+(p.experim.target))
      
    } else if (test.type=="logrank.Freedman") {
      n.ev <- (qnorm(1 - sig.level) + qnorm(power))^2*(1+(HR.target/NI.margin)/r)^2/((1-HR.target/NI.margin)^2 * (1/r))
      n <- n.ev*(1+1/r)/((p.control.expected)/r+(p.experim.target))
    }
    
  } else {
    
   stop("Function to calculate sample size for DRMST is under development, and not yet available.\n")
    
  } 
  
  
  n.ev <- n.ev/(1-ltfu)
  n <- n/(1-ltfu)
  
  if (isTRUE(round)) {
    ss.ev <- ceiling(n.ev) 
    ss <- c(nC <- ceiling(n*prop.control) , nE <- ceiling(ceiling(n* prop.exp)))
  } else {
    ss.ev <- n.ev 
    ss <- c(nC <- n*prop.control , nE <- n* prop.exp)
  }
  
  
  if (print.out == T) {
    cat("Power:", (power) * 100, "%\nOne-sided significance level:", 
        sig.level * 100, "%.\nEvent risk in control arm over follow-up=", 
        p.control.expected * 100, "%\nEvent risk in experimental arm over follow-up =", 
        p.experim.target * 100, "%\nTrue hazard ratio =",
        HR.target,"\nExpected loss to follow-up: ",
        ltfu*100, "%\n"
    )
    if (summary.measure == "HR") {
      cat("The sample size required to test non-inferiority within a", 
          NI.margin, " Hazard Ratio NI margin is:")
    }
    else if (summary.measure == "DRMST") {
      cat("The sample size required to test non-inferiority within a", 
          NI.margin, " DRMST NI margin is:")
    }
    cat("\n", ss.ev, " events overall, achieved by:\n", ss[1], " individuals in the control group.\n", 
        ss[2], " individuals in the experimental group.\n")
  }
  return(ss)
}