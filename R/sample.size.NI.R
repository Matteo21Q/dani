sample.size.NI <- function (p0.expected, p1.expected, NI.margin, sig.level = 0.025, 
                            power = 0.9, r = 1, scale = "RD", print.out = TRUE, var.type="AA") 
{
  stopifnot(((scale == "RD") || (scale == "RR") || 
               (scale == "OR") || (scale == "AS")), p0.expected <= 
              1, p1.expected <= 1, p0.expected >= 0, p1.expected >= 
              0, sig.level < 1, power < 1, sig.level > 0, power > 0, 
            r > 0,((var.type == "AA") || (var.type == "NA") || 
                     (var.type == "NN")))
  
  if (scale == "RD") {
    
    mean.alt <- p1.expected - p0.expected - NI.margin
    var.alt <-(p0.expected * (1 - p0.expected) + 
                 p1.expected * (1 - p1.expected)/r)
    p1.null<-(NI.margin+2*p0.expected)/(1+r)
    p0.null<-2*p0.expected-r*p1.null
    var.null <-(p0.null * (1 - p0.null) + 
                  p1.null * (1 - p1.null)/r)
    
  } else if (scale == "RR") {
    
    NI.margin.log <- log(NI.margin)
    mean.alt <- log(p1.expected/p0.expected) - NI.margin.log
    var.alt <- ((1 - p0.expected)/(p0.expected) + 
                  (1 - p1.expected)/(r *p1.expected))
    p1.null<-(NI.margin*2*p0.expected)/(1+r*NI.margin)
    p0.null<-2*p0.expected-r*p1.null
    var.null <- ((1 - p0.null)/(p0.null) + 
                   (1 - p1.null)/(r *p1.null))
    
  } else if (scale == "OR") {
    
    NI.margin.log <- log(NI.margin)
    mean.alt<-log((p1.expected * (1 - p0.expected))/(p0.expected *(1 - p1.expected))) + 
      - NI.margin.log
    var.alt<-(1/((1 - p0.expected) * p0.expected) + 
                1/(r * (1 - p1.expected) *p1.expected))
    roots<-as.numeric(polyroot(c(-2*NI.margin*p0.expected,
                                 1-2*p0.expected+NI.margin*r+2*NI.margin*p0.expected,
                                 (1-NI.margin)*r)))
    p1.null<-ifelse(roots[1]<=1&roots[1]>=0,roots[1],roots[2])
    p0.null<-2*p0.expected-r*p1.null
    var.null<-(1/((1 - p0.null) * p0.null) + 
                 1/(r * (1 - p1.null) * p1.null))
    
  } else if (scale == "AS") {
    
    mean.alt<-asin(sqrt(p1.expected)) - asin(sqrt(p0.expected)) - NI.margin
    var.alt <- var.null <- ((1/(4 * r) + 1/4))
    
  }
  
  var.1<-ifelse (substr(var.type,1,1)=="A", var.alt, var.null)
  var.2<-ifelse (substr(var.type,2,2)=="A", var.alt, var.null)
  
  n = (qnorm(1 - sig.level)*sqrt(var.1) + qnorm(power)*sqrt(var.2))^2 * (1/(mean.alt)^2)
  ss <- c(nC <- ceiling(n), nA <- ceiling(ceiling(n) * r))
  
  
  if (print.out == T) {
    cat("Power:", (power) * 100, "%\nOne-sided significance level:", 
        sig.level * 100, "%.\nExpected control event risk =", 
        p0.expected * 100, "%\nExpected active event risk =", 
        p1.expected * 100, "%\n")
    if (scale == "RD") {
      cat("The sample size required to test non-inferiority within a", 
          NI.margin * 100, "% risk difference NI margin is:")
    }
    else if (scale == "RR") {
      cat("The sample size required to test non-inferiority within a", 
          NI.margin.log, "log-risk ratio margin is:")
    }
    else if (scale == "OR") {
      cat("The sample size required to test non-inferiority within a", 
          NI.margin.log, "log-odds ratio margin is:")
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