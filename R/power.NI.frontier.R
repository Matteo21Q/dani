power.NI.frontier <- function(p0.expected, p1.expected, NI.frontier, sample.size, 
                                   r=1, range.of.p0, threshold.modify=0,
                                   sig.level=0.025,
                                   scale="RD", threshold.relevance=10^(-5),
                                   alt.hyp.frontier=NULL,
                                   print.out=T, unfavourable=T) {
  
  stopifnot(((scale == "RD") || (scale == "RR") || 
               (scale == "OR") || (scale == "AS")), p0.expected <= 
              1, p1.expected <= 1, p0.expected >= 0, p1.expected >= 0,
            r > 0, is.function(NI.frontier), length(formals(NI.frontier))==1,
            is.vector(sample.size),sample.size[1]>0, sample.size[2]>0,
            threshold.modify>=0, threshold.relevance>0)
  if (is.null(alt.hyp.frontier)) {
    alt.hyp.frontier<-function(p) {
      return(p)
    }
  }
  CI.bound<-ifelse(isTRUE(unfavourable),2,1)
  if (is.numeric(sig.level.analysis)) {
    sig.level.analysis<-function(x) {
      return(0*x+sig.level)
    }
  } else {
    sig.level.analysis<-sig.level
  }
  power<-NULL
  for (p0 in range.of.p0) {
    p1 <- alt.hyp.frontier(p0) 
    probs0<-rep(NA,sample.size[1]+1)
    probs1<-rep(NA,sample.size[2]+1)
    p.est<-0
    for (i in 0:sample.size[1]) {
      probs0[i+1]<-dbinom(i,sample.size[1],p0)
      if (probs0[i+1]>threshold.relevance) {
        pio0<-i/sample.size[1]
        NI.marg2 <- ifelse(abs(pio0-p0.expected)>threshold.modify,NI.frontier(pio0),NI.frontier(p0.expected))
        p.est1<-0
        for (j in 0:sample.size[2]) {
          probs1[j+1]<-dbinom(j,sample.size[2],p1)
          res.test<-test.NI(n0 = sample.size[1], 
                            n1 = sample.size[2], e0 = i, e1 = j, NI.margin = NI.marg2, 
                            sig.level = sig.level.analysis(pio0), scale = scale, 
                            print.out = FALSE, unfavourable=unfavourable)$CI[CI.bound]
          if (is.nan(res.test)) res.test<-9999
          if (isTRUE(unfavourable)) {
            p.est1<-p.est1+probs1[j+1]*(res.test<NI.marg2)
          } else {
            p.est1<-p.est1+probs1[j+1]*(res.test>NI.marg2)
          }
        }
        p.est<-p.est+probs0[i+1]*p.est1
      }
      
    }
    cat(".")
    power<-c(power,p.est)
  }
  cat("Completed\n")
  results<-data.frame(range.of.p0,power)
  return(results)
}
