sample.size.NI <-
  function(p0.expected, p1.expected, NI.margin, sig.level=0.025, power=0.9, r=1, scale="RD", print.out=TRUE, favourable=FALSE) {
    stopifnot(((scale == "RD") || (scale == "RR") || (scale == "OR")|| 
                 (scale == "AS")), p0.expected <= 1, p1.expected <= 
                1, p0.expected >= 0, p1.expected >= 0, sig.level < 1, 
              power < 1, sig.level > 0, power > 0, r > 0)
    
    if (favourable == T) {
      p0.expected <- 1-p0.expected
      p1.expected <- 1-p1.expected
    }
    if (scale == "RD") {
      NI.margin.unf <- (1-2*(NI.margin<0))*NI.margin
      n <- (qnorm(1 - sig.level) + qnorm(power))^2 * ((p0.expected * 
                                                         (1 - p0.expected) + p1.expected * (1 - p1.expected)/r)/(p1.expected - 
                                                                                                                   p0.expected - NI.margin.unf)^2)
      ss <- c(nC <- ceiling(n), nA <- ceiling(ceiling(n) * 
                                                r))
    }
    else if (scale == "RR") {
      NI.margin.unf <- log(NI.margin^(1-2*(NI.margin<1)))
      n = (qnorm(1 - sig.level) + qnorm(power))^2 * (((1 - 
                                                         p0.expected)/(p0.expected) + (1 - p1.expected)/(r * 
                                                                                                           p1.expected))/(log(p1.expected/p0.expected) - NI.margin.unf)^2)
      ss <- c(nC <- ceiling(n), nA <- ceiling(ceiling(n) * 
                                                r))
    }
    else if (scale == "OR") {
      NI.margin.unf <- log(NI.margin^(1-2*(NI.margin<1)))
      n = (qnorm(1 - sig.level) + qnorm(power))^2 * ((1/((1 -p0.expected)*p0.expected) +
                                                        1/(r * (1 - p1.expected)*  p1.expected))/(log((p1.expected*(1 - p0.expected))/(p0.expected*(1 - p1.expected))) - NI.margin.unf)^2)
      ss <- c(nC <- ceiling(n), nA <- ceiling(ceiling(n) * 
                                                r))

    } else if (scale == "AS") {
      NI.margin.unf <- (1-2*(NI.margin<0))*NI.margin
      n = (qnorm(1 - sig.level) + qnorm(power))^2 * (((1/(4 * 
                                                            r) + 1/4))/(asin(sqrt(p1.expected)) - asin(sqrt(p0.expected)) - 
                                                                          NI.margin.unf)^2)
      ss <- c(nC <- ceiling(n), nA <- ceiling(ceiling(n) * 
                                                r))
    }
    
    if (favourable == T) {
      p0.expected <- 1-p0.expected
      p1.expected <- 1-p1.expected
    }
    if (print.out == T) {
      cat("Power:", (power) * 100, "%\nOne-sided significance level:", 
          sig.level * 100, "%.\nExpected control event risk =", 
          p0.expected * 100, "%\nExpected active event risk =", 
          p1.expected * 100, "%\n")
      if (scale == "RD") {
          cat("The sample size required to test non-inferiority within a", 
              NI.margin * 100, "% risk difference NI margin is:")
      } else if (scale == "RR"){
        cat("The sample size required to test non-inferiority within a", 
            log(NI.margin) , "log-risk ratio margin is:")
      } else if ( scale == "OR") {
        cat("The sample size required to test non-inferiority within a", 
            log(NI.margin) , "log-odds ratio margin is:")
      } else if (scale == "AS") {
        cat("The sample size required to test non-inferiority within a", 
            NI.margin , "arc-sine difference margin is:")
      }
      cat("\n", ss[1], " individuals in the control group.\n", 
          ss[2], " individuals in the active group.\n")
    }
    return(ss)
  }