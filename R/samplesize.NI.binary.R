samplesize.NI.binary <- function (p.control.expected, p.experim.target, NI.margin, sig.level = 0.025, 
                            power = 0.9, r = 1, summary.measure = "RD", print.out = TRUE, test.type="score",
                            unfavourable=T, cont.corr=F) 
{
  
  stopifnot(is.numeric(p.control.expected), p.control.expected < 1, p.control.expected > 0)
  stopifnot(is.numeric(p.experim.target), p.experim.target < 1, p.experim.target > 0)
  stopifnot(is.numeric(sig.level), sig.level < 0.5, sig.level > 0)
  stopifnot(is.numeric(power), power < 1, power > 0)
  stopifnot(is.numeric(r), r > 0)
  stopifnot(is.character(summary.measure), summary.measure %in%c("RD", "RR", "OR", "AS"))
  stopifnot(is.numeric(NI.margin))
  stopifnot(is.logical(print.out), !is.na(print.out))
  stopifnot(is.character(test.type), test.type %in% c("Wald", "score", "local"))
  stopifnot(is.logical(unfavourable), !is.na(unfavourable))
  stopifnot(is.logical(cont.corr), !is.na(cont.corr))
  if (summary.measure%in%c("RR", "OR")&&NI.margin<=0) stop("NI margin should be >0 when summary measure is a ratio (OR or RR)")
  if (summary.measure=="RD"&&abs(NI.margin)>=1) stop("NI margin should be <1 in absolute value when summary measure is RD")
  
  var.type<-ifelse(test.type=="Wald", "AA", ifelse(test.type=="score","NA","NN"))
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
  
  r<-1/r  #easier to write formula for control/experimental allocation ratio
  
  if (summary.measure == "RD") {
    
    mean.alt <- p.experim.target - p.control.expected - NI.margin
    var.alt <-(p.control.expected * (1 - p.control.expected)/r + 
                 p.experim.target * (1 - p.experim.target))
    a<-1+r
    b<-(-(1+r+p.experim.target+r*p.control.expected+NI.margin*(r+2)))
    cc<-NI.margin^2+NI.margin*(2*p.experim.target+r+1)+p.experim.target+r*p.control.expected
    d<-(-p.experim.target*NI.margin*(1+NI.margin))
    v<-b^3/(3*a)^3-b*cc/(6*a^2)+d/(2*a)
    u<-sign(v)*sqrt((b^2)/(3*a)^2-cc/(3*a))
    w<-1/3*(pi+acos(v/u^3))
    p1.null<-2*u*cos(w)-b/(3*a)
    p0.null<-p1.null-NI.margin
    var.null <-(p0.null * (1 - p0.null)/r + 
                  p1.null * (1 - p1.null))
    
  } else if (summary.measure == "RR") {
    
    NI.margin.log <- log(NI.margin)
    mean.alt <- log(p.experim.target/p.control.expected) - NI.margin.log
    var.alt <- ((1 - p.control.expected)/(r*p.control.expected) + 
                  (1 - p.experim.target)/(p.experim.target))
    a<-1+r
    b<-(-(r+p.experim.target+NI.margin*(r*p.control.expected+1)))
    cc<-NI.margin*(p.experim.target+r*p.control.expected)
    p1.null<-(-b-sqrt(b^2-4*a*cc))/(2*a)
    p0.null<-p1.null/NI.margin
    var.null <- ((1 - p0.null)/(p0.null) + 
                   (1 - p1.null)/(r *p1.null))
    
  } else if (summary.measure == "OR") {
    
    NI.margin.log <- log(NI.margin)
    mean.alt<-log((p.experim.target * (1 - p.control.expected))/(p.control.expected *(1 - p.experim.target))) + 
      - NI.margin.log
    var.alt<-(1/(r*(1 - p.control.expected) * p.control.expected) + 
                1/((1 - p.experim.target) *p.experim.target))
    a<-NI.margin-1
    b<-1 + r * NI.margin + (1 - NI.margin) * (r * p.control.expected + p.experim.target)
    cc<-(-(r * p.control.expected + p.experim.target))
    p1.null<-(-b+sqrt(b^2-4*a*cc))/(2*a)
    p0.null<- NI.margin * p1.null/(1 + p1.null * (NI.margin - 1))
    var.null<-(1/(r*(1 - p0.null) * p0.null) + 
                 1/((1 - p1.null) * p1.null))
    
  } else if (summary.measure == "AS") {
    
    mean.alt<-asin(sqrt(p.experim.target)) - asin(sqrt(p.control.expected)) - NI.margin
    var.alt <- var.null <- ((1/(4 * r) + 1/4))
    
  }
  
  var.1<-ifelse (substr(var.type,1,1)=="A", var.alt, var.null)
  var.2<-ifelse (substr(var.type,2,2)=="A", var.alt, var.null)
  
  n = (qnorm(1 - sig.level)*sqrt(var.1) + qnorm(power)*sqrt(var.2))^2 * (1/(mean.alt)^2)

    ss <- c(nC <- ceiling(ceiling(n* r) ), nE <- ceiling(n))
  
  if (cont.corr==T) {
    if (summary.measure=="RD") {
      r<-1/r
      ss[1] <- 0.25*(1+sqrt(1+2/(ss[1]*(r/(1+r))*NI.margin)))^2*ss[1]
      ss[2] <- 0.25*(1+sqrt(1+2/(ss[2]*(1/(1+r))*NI.margin)))^2*ss[2]
    } else {
      warning("Continuity correction currently supported for risk difference summary.measure only.
              Results are for sample size without continuity correction.\n")
    }
  }
  
  ss<-ceiling(ss)
  
  if (print.out == T) {
    cat("Method: ", test.type,
        "\nPower:", (power) * 100, "%\nOne-sided significance level:", 
        sig.level * 100, "%.\nExpected control event risk =", 
        p.control.expected * 100, "%\nExpected experimental arm event risk (alternative H) =", 
        p.experim.target * 100, "%\nNon-acceptable experimental arm event risk (null H) =",
        p1.exp.null*100,"%\n"
        )
    if (summary.measure == "RD") {
      cat("The sample size required to test non-inferiority within a", 
          NI.margin * 100, "% risk difference NI margin is:")
    }
    else if (summary.measure == "RR") {
      cat("The sample size required to test non-inferiority within a", 
          NI.margin, "risk ratio margin (", NI.margin.log, "log-risk ratio margin) is:")
    }
    else if (summary.measure == "OR") {
      cat("The sample size required to test non-inferiority within a", 
          NI.margin, "odds ratio margin (", NI.margin.log, "log-odds ratio margin) is:")
    }
    else if (summary.measure == "AS") {
      cat("The sample size required to test non-inferiority within a", 
          NI.margin, "arc-sine difference margin is:")
    }
    cat("\n", ss[1], " individuals in the control group.\n", 
        ss[2], " individuals in the experimental treatment group.\n")
  }
  return(ss)
}