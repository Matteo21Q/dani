compare.NIfrontier.survival<-function(rate.control.expected=NULL, rate.experim.target=NULL, t.expected=NULL, p.control.expected=NULL, p.experim.target=NULL, rates.range=NULL, NI.margin, summary.measure="HR", tau.RMST, t.DS=tau.RMST) {
  
  if ((summary.measure!="HR")&&(summary.measure!="DRMST")&&(summary.measure!="DS")) stop("summary.measure should be one of either DS (difference in survival at specific time point), HR (hazard ratio) or DRMST (difference in restricted mean survival time).\n")
  if ((!is.null(rate.control.expected)&&!is.numeric(rate.control.expected))||(!is.null(rate.experim.target)&&!is.numeric(rate.experim.target))) stop("rates should be specified as numeric.\n")
  if (is.null(rate.control.expected)&&(is.null(t.expected)|is.null(p.control.expected))) stop("Either the control event rate or the expected control risk at time t.expected have to be specified\n")
  if (!is.null(rate.control.expected)&&(!is.null(p.control.expected))) stop("Please only specify either the control event rate or the expected control risk at time t.expected. The other one will be back calculted assuming constant baseline hazard.\n")
  if ((!is.null(rate.control.expected)&&(rate.control.expected<=0))||(!is.null(rate.experim.target)&&(rate.experim.target<=0))) stop("Event rates should be positive.\n")
  if (!is.numeric(t.DS)||!is.numeric(tau.RMST)) stop("Horizon time should be defined (as a numeric) for both RMST and DS.\n")
  if ((tau.RMST<=0)||(t.DS<=0)) stop("Horizon time should be positive.\n")
  if (is.null(t.expected)) t.expected<-t.DS
  if ((!is.null(NI.margin)&&!is.numeric(NI.margin))) stop("NI margins should be of numeric class.\n")
  if ((summary.measure=="HR")&&(NI.margin<=0)) stop("NI margin should be >0 when defined as a HR.\n")
  if (!is.numeric(t.expected)||t.expected<=0) stop("t.expected should be a positive number.\n")
  if (!is.null(p.control.expected)&&(!is.numeric(p.control.expected)||p.control.expected<=0||p.control.expected>=1)) stop("p.control.expected should be a numeric >0 and <1.\n")
  if (!is.null(p.experim.target)&&(!is.numeric(p.experim.target)||p.experim.target<=0||p.experim.target>=1)) stop("p.experim.target should be a numeric >0 and <1.\n")
  if ((NI.margin==1)&&(summary.measure=="HR")) stop ("A Non-inferiority margin of 1 for the hazard ratio means this is a superiority trial. Hence, all summary measures will have same power.\n")
  if ((NI.margin==0)&&(summary.measure=="DRMST"||summary.measure=="DS")) stop ("A Non-inferiority margin of 0 for a difference (DS or DRMST) means this is a superiority trial. Hence, all summary measures will have same power.\n")
  
  if (is.null(rate.control.expected)) {
    rate.control.expected<-uniroot(fun.rate, c(1E-6, 1E6), tol = 0.000001, 
                              target.surv=p.control.expected, time=t.expected)$root
    if (!is.null(p.experim.target)) {
      rate.experim.target<-uniroot(fun.rate, c(1E-6, 1E6), tol = 0.000001, 
                              target.surv=p.experim.target, time=t.expected)$root
    }
  }
  if (is.null(p.control.expected)) {
    p.control.expected<-pexp(t.expected,rate.control.expected)
  }
  
  if (summary.measure=="HR") {
    HR.margin<-NI.margin
    rate.experim.null<-HR.margin*rate.control.expected
  } else if (summary.measure=="DRMST") {
    DRMST.margin<-NI.margin
    rate.experim.null<-uniroot(RMST.margin, c(1E-6, 20), tol = 0.000001, 
            lambda=rate.control.expected, tau=tau.RMST, 
            target=DRMST.margin)$root
  } else {
    DS.margin<-NI.margin
    rate.experim.null<-uniroot(Diff.margin, c(1E-6, 20), tol = 0.000001, 
            lambda=rate.control.expected, t=t.DS, 
            target=DS.margin)$root
  }
  
  if (summary.measure!="DRMST") {
    DRMST.margin<-RMST.margin(rate.experim.null, rate.control.expected, tau.RMST,0)
  }
  if (summary.measure!="DS") {
    DS.margin<-Diff.margin(rate.experim.null, rate.control.expected, t.DS,0)
  }
  if (summary.measure!="HR") {
    HR.margin<-rate.experim.null/rate.control.expected
  }
  
  if (is.null(rates.range)) {
    rates.range<-c(max(0.000001,rate.control.expected-0.3), rate.control.expected+0.3)
  }
  if (is.null(rate.experim.target)) rate.experim.target<-rate.control.expected
  if (rate.experim.target>=rate.experim.null) stop("The alternative hypothesis does not imply non-inferiority of the experimental treatment")
  
  if ((!is.vector(rates.range))||(!is.numeric(rates.range))||(length(rates.range)!=2)||(sum(rates.range<=0)>0)) stop("rates.range should be a numeric vector of length 2, with rates>0.\n")
  if (rates.range[1]>rates.range[2]) rates.range<-rates.range[2:1]
  if (rates.range[1]==rates.range[2]) stop("This function should be used to evaluate frontiers. If the range is a single point, then any summary measure will have same power if the margins are matched.")
  if ((rate.control.expected>rates.range[2])||(rate.control.expected<rates.range[1])) stop("Range of risks should include the expected value.\n")
  
  length.vec<-20
  rates.vec<-seq(rates.range[1], rates.range[2], length.out=length.vec)
  experim.rates.HR<-experim.rates.DRMST<-experim.rates.DS<-rep(NA,length.vec)
  for (i in 1:length.vec) {
    
    experim.rates.HR[i]<-rates.vec[i]*HR.margin
    
    res1<-try(uniroot(RMST.margin, c(1E-6, 20), tol = 0.000001, 
                      lambda=rates.vec[i], tau=tau.RMST, 
                      target=DRMST.margin)$root, silent = T)
    if (class(res1)!="try-error") experim.rates.DRMST[i]<-res1
    
    res2<-try(uniroot(Diff.margin, c(1E-6, 20), tol = 0.000001, 
                      lambda=rates.vec[i], t=t.DS, 
                      target=DS.margin)$root, silent=T)
    if (class(res2)!="try-error") experim.rates.DS[i]<-res2
    
    cat(".")
  }
  cat("\n")
  
  palette("Okabe-Ito")
  par(mar = c(4.1, 4.1, 1.1, 1.1))
  plot(rates.vec,rates.vec, type="l", ylim=c(0,max(experim.rates.HR,experim.rates.DRMST,experim.rates.DS, na.rm=T)), lty=2,
       main = "", xlab = "Control event rate",
       ylab="Experimental event rate", las=1)
  lines(rates.vec, experim.rates.HR, type="l", col="#56B4E9", lwd=2)
  lines(rates.vec, experim.rates.DRMST, type="l", lwd=2)
  lines(rates.vec, experim.rates.DS, type="l", col="#009E73", lwd=2)
  points(rate.control.expected, rate.experim.target, lwd=2, pch=1)
  points(rate.control.expected, rate.experim.null, lwd=2, pch=3)
  
  euclidean <- function(a, b) sqrt(sum((a - b)^2))
  
  dist.HR<-dist.DS<-dist.DRMST<-rep(NA,length.vec)
  for (jj in 1:length.vec) {
    dist.HR[jj]<-euclidean(c(rate.control.expected,rate.control.expected),
                       c(rates.vec[jj],experim.rates.HR[jj]))
    dist.DS[jj]<-euclidean(c(rate.control.expected,rate.control.expected),
                       c(rates.vec[jj],experim.rates.DS[jj]))
    dist.DRMST[jj]<-euclidean(c(rate.control.expected,rate.control.expected),
                       c(rates.vec[jj],experim.rates.DRMST[jj]))
  }
  min.HR<-min(dist.HR, na.rm = T)
  min.DS<-min(dist.DS, na.rm = T)
  min.DRMST<-min(dist.DRMST, na.rm = T)
  
  which.min.dist<-order(c(min.HR, min.DRMST, min.DS))
  method<-c("HR", "DRMST", "DS")
  cat("Assumed event distribution within arm: exponential (fixed rate).\nHR Margin = ", HR.margin, ".\nDRMST Margin = ", DRMST.margin, 
  "\nDS Margin = ", DS.margin, "\nControl event rate: ", rate.control.expected, 
  "\nExpected ", t.expected, "-year Control event risk = ", p.control.expected, 
  "\nThe most powerful summary measure for testing non-inferiority is ", method[which.min.dist[3]], ". Second best is ",
      method[which.min.dist[2]], ". The least powerful is ",
      method[which.min.dist[1]], ".\n The non-inferiority frontier plot shows the distance of various frontiers from the expected point (solid circle).\nBlack line: DRMST frontier.\nGreen line: DS frontier.\nBlue line: HR frontier.\nThe black cross represents the frontier point.\n"
      ,sep="")
  results<-data.frame(rates.range, experim.rates.HR, experim.rates.DS, experim.rates.DRMST, 
                      dist.HR, dist.DS, dist.DRMST)
  return(results)
}


