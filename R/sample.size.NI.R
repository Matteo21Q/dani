sample.size.NI <- function (p0.expected, p1.expected, NI.margin, sig.level = 0.025, 
                            power = 0.9, r = 1, scale = "RD", print.out = TRUE, test.type="Wald",
                            unfavourable=T, cont.corr=F) 
{
  
  stopifnot(is.numeric(p0.expected), p0.expected <= 1, p0.expected >= 0)
  stopifnot(is.numeric(p1.expected), p1.expected <= 1, p1.expected >= 0)
  stopifnot(is.numeric(NI.margin))
  stopifnot(is.numeric(sig.level), sig.level < 1, sig.level > 0)
  stopifnot(is.numeric(power), power < 1, power > 0)
  stopifnot(is.numeric(r), r > 0)
  stopifnot(is.character(scale), ((scale == "RD") || (scale == "RR") || 
                                    (scale == "OR") || (scale == "AS")))
  stopifnot(is.logical(print.out), !is.na(print.out))
  stopifnot(is.character(test.type), ((test.type == "Wald") || (test.type == "score") || 
                                    (test.type == "local")))
  stopifnot(is.logical(unfavourable), !is.na(unfavourable))
  stopifnot(is.logical(cont.corr), !is.na(cont.corr))
  
  if ((scale=="AS")&&(test.type!="Wald")) {
    stop("Only Wald test available for AS scale.\n")
  }
  var.type<-ifelse(test.type=="Wald", "AA", ifelse(test.type=="score","NA","NN"))
  p1.exp.null<-ifelse(scale=="RD", p0.expected+NI.margin, 
                      ifelse(scale=="RR", p0.expected*NI.margin,
                             ifelse(scale=="OR", p0.expected*NI.margin/(1-p0.expected+NI.margin*p0.expected), 
                                    sin(NI.margin+asin(sqrt(p0.expected)))^2)))
  
  if (unfavourable==T) {
    if (p1.expected>=p1.exp.null) stop("In the alternative hypothesis the active treatment is not non-inferior. p1=",
                                                    p1.expected*100, "%, which is greater or equal than the minimum non-tolerable event risk=", p1.exp.null*100,"%.\nPlease check again all parameter values. Alternatively makes sure you have specified correctly whether your outcome is favourable.")
  } else {
    if (p1.expected<=p1.exp.null) stop("In the alternative hypothesis the active treatment is not non-inferior. p1=",
                                       p1.expected*100, "%, which is lower or equal than the maximum non-tolerable event risk=", p1.exp.null*100,"%.\nPlease check again all parameter values. Alternatively makes sure you have specified correctly whether your outcome is favourable.")
    
  }
  
  r<-1/r  #easier to write formula for control/active allocation ratio
  
  if (scale == "RD") {
    
    mean.alt <- p1.expected - p0.expected - NI.margin
    var.alt <-(p0.expected * (1 - p0.expected)/r + 
                 p1.expected * (1 - p1.expected))
    a<-1+r
    b<-(-(1+r+p1.expected+r*p0.expected+NI.margin*(r+2)))
    cc<-NI.margin^2+NI.margin*(2*p1.expected+r+1)+p1.expected+r*p0.expected
    d<-(-p1.expected*NI.margin*(1+NI.margin))
    v<-b^3/(3*a)^3-b*cc/(6*a^2)+d/(2*a)
    u<-sign(v)*sqrt((b^2)/(3*a)^2-cc/(3*a))
    w<-1/3*(pi+acos(v/u^3))
    p1.null<-2*u*cos(w)-b/(3*a)
    p0.null<-p1.null-NI.margin
    var.null <-(p0.null * (1 - p0.null)/r + 
                  p1.null * (1 - p1.null))
    
  } else if (scale == "RR") {
    
    NI.margin.log <- log(NI.margin)
    mean.alt <- log(p1.expected/p0.expected) - NI.margin.log
    var.alt <- ((1 - p0.expected)/(r*p0.expected) + 
                  (1 - p1.expected)/(p1.expected))
    a<-1+r
    b<-(-(r+p1.expected+NI.margin*(r*p0.expected+1)))
    cc<-NI.margin*(p1.expected+r*p0.expected)
    p1.null<-(-b-sqrt(b^2-4*a*cc))/(2*a)
    p0.null<-p1.null/NI.margin
    var.null <- ((1 - p0.null)/(p0.null) + 
                   (1 - p1.null)/(r *p1.null))
    
  } else if (scale == "OR") {
    
    NI.margin.log <- log(NI.margin)
    mean.alt<-log((p1.expected * (1 - p0.expected))/(p0.expected *(1 - p1.expected))) + 
      - NI.margin.log
    var.alt<-(1/(r*(1 - p0.expected) * p0.expected) + 
                1/((1 - p1.expected) *p1.expected))
    a<-NI.margin-1
    b<-1 + r * NI.margin + (1 - NI.margin) * (r * p0.expected + p1.expected)
    cc<-(-(r * p0.expected + p1.expected))
    p1.null<-(-b+sqrt(b^2-4*a*cc))/(2*a)
    p0.null<- NI.margin * p1.null/(1 + p1.null * (NI.margin - 1))
    var.null<-(1/(r*(1 - p0.null) * p0.null) + 
                 1/((1 - p1.null) * p1.null))
    
  } else if (scale == "AS") {
    
    mean.alt<-asin(sqrt(p1.expected)) - asin(sqrt(p0.expected)) - NI.margin
    var.alt <- var.null <- ((1/(4 * r) + 1/4))
    
  }
  
  var.1<-ifelse (substr(var.type,1,1)=="A", var.alt, var.null)
  var.2<-ifelse (substr(var.type,2,2)=="A", var.alt, var.null)
  
  n = (qnorm(1 - sig.level)*sqrt(var.1) + qnorm(power)*sqrt(var.2))^2 * (1/(mean.alt)^2)
  ss <- c(nC <- ceiling(ceiling(n) * r), nA <- ceiling(n))
  
  if (cont.corr==T) {
    if (scale=="RD") {
      r<-1/r
      ss[1] <- 0.25*(1+sqrt(1+2/(ss[1]*(r/(1+r))*NI.margin)))^2*ss[1]
      ss[2] <- 0.25*(1+sqrt(1+2/(ss[2]*(1/(1+r))*NI.margin)))^2*ss[2]
    } else {
      warning("Continuity correction currently supported for risk difference scale only.
              Results are for sample size without continuity correction.\n")
    }
  }
  
  ss<-ceiling(ss)
  
  if (print.out == T) {
    cat("Method: ", test.type,
        "\nPower:", (power) * 100, "%\nOne-sided significance level:", 
        sig.level * 100, "%.\nExpected control event risk =", 
        p0.expected * 100, "%\nExpected active event risk (alternative H) =", 
        p1.expected * 100, "%\nNon-acceptable active event risk (null H) =",
        p1.exp.null*100,"%\n"
        )
    if (scale == "RD") {
      cat("The sample size required to test non-inferiority within a", 
          NI.margin * 100, "% risk difference NI margin is:")
    }
    else if (scale == "RR") {
      cat("The sample size required to test non-inferiority within a", 
          NI.margin, "risk ratio margin (", NI.margin.log, "log-risk ratio margin) is:")
    }
    else if (scale == "OR") {
      cat("The sample size required to test non-inferiority within a", 
          NI.margin, "odds ratio margin (", NI.margin.log, "log-odds ratio margin) is:")
    }
    else if (scale == "AS") {
      cat("The sample size required to test non-inferiority within a", 
          NI.margin, "arc-sine difference margin is:")
    }
    cat("\n", ss[1], " individuals in the control group.\n", 
        ss[2], " individuals in the active group.\n")
  }
  return(ss)
}