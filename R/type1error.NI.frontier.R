type1error.NI.frontier <- function(p0.expected, NI.frontier, sample.size, 
                              r=1, range.of.p0, threshold.modify=0,
                              sig.level.analysis=0.025,
                              scale="RD", threshold.relevance=10^(-5),
                              print.out=T,favourable=F) {
  
  type1error<-NULL
  for (p0 in range.of.p0) {
    if (scale=="RD") {
      p1 <- ifelse(favourable==FALSE,p0+NI.frontier(p0), p0-NI.frontier(p0)) 
    } else if (scale=="RR") {
      p1 <- ifelse(favourable==FALSE,p0*NI.frontier(p0), p0/NI.frontier(p0)) 
    }
    probs0<-rep(NA,sample.size+1)
    probs1<-rep(NA,sample.size+1)
    p.est<-0
    for (i in 0:sample.size) {
      probs0[i+1]<-dbinom(i,sample.size,p0)
      if (probs0[i+1]>threshold.relevance) {
        pio0<-i/sample.size
        if (scale=="RD"){
          NI.marg2 <- ifelse(abs(pio0-p0.expected)>threshold.modify,NI.frontier(pio0),NI.frontier(p0.expected))
        } else if (scale=="RR") {
          NI.marg2 <- ifelse(abs(log(pio0/p0.expected))>log(threshold.modify),NI.frontier(pio0),NI.frontier(p0.expected))
        }
        p.est1<-0
        for (j in 0:sample.size) {
          probs1[j+1]<-dbinom(j,sample.size,p1)
          res.test<-test.NI(n0 = sample.size, 
                            n1 = sample.size, e0 = i, e1 = j, NI.margin = NI.marg2, 
                            sig.level = sig.level.analysis, scale = scale, 
                            print.out = FALSE, favourable=favourable)$CI[2]
          if (is.nan(res.test)) res.test<-9999
          p.est1<-p.est1+probs1[j+1]*(res.test<NI.marg2)
        }
        p.est<-p.est+probs0[i+1]*p.est1
      }
      
    }
    cat(".")
    type1error<-c(type1error,p.est)
  }
  cat("Sample size ", sample.size, " and significance level ", sig.level.analysis, " completed\n")
  results<-data.frame(range.of.p0,type1error)
  return(results)
}
