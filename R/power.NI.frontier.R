power.NI.frontier <- function(p0.expected, NI.frontier, sample.size, 
                                   r=1, range.of.p0, threshold.modify=0,
                                   sig.level.analysis=0.025,
                                   scale="RD", threshold.relevance=10^(-5),
                                   print.out=T,favourable=F) {
  
  power<-NULL
  for (p0 in range.of.p0) {
    p1 <- p0 
    probs0<-rep(NA,sample.size[1]+1)
    probs1<-rep(NA,sample.size[2]+1)
    p.est<-0
    for (i in 0:sample.size[1]) {
      probs0[i+1]<-dbinom(i,sample.size[1],p0)
      if (probs0[i+1]>threshold.relevance) {
        pio0<-i/sample.size[1]
        if (scale=="RD"){
          NI.marg2 <- ifelse(abs(pio0-p0.expected)>threshold.modify,NI.frontier(pio0),NI.frontier(p0.expected))
        } else if (scale=="RR") {
          NI.marg2 <- ifelse(abs(log(pio0/p0.expected))>log(threshold.modify),NI.frontier(pio0),NI.frontier(p0.expected))
        }
        p.est1<-0
        for (j in 0:sample.size[2]) {
          probs1[j+1]<-dbinom(j,sample.size[2],p1)
          res.test<-test.NI(n0 = sample.size[1], 
                            n1 = sample.size[2], e0 = i, e1 = j, NI.margin = NI.marg2, 
                            sig.level = sig.level.analysis, scale = scale, 
                            print.out = FALSE, favourable=favourable)$CI[2]
          if (is.nan(res.test)) res.test<-9999
          p.est1<-p.est1+probs1[j+1]*(res.test<NI.marg2)
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
