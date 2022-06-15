frontier.comparison<-function(rate.control.expected=NULL, rate.active.alternative=NULL, t.control=NULL, control.risk=NULL, control.rates=NULL, HR.margin=NULL, DRMST.margin=NULL, DS.margin=NULL, tau.RMST, t.DS) {
  
  if (is.null(rate.control.expected)&&(is.null(t.control)|is.null(control.risk))) stop("Either the control event rate or the expected control risk at time t.control have to be specified\n")
  if (!is.null(rate.control.expected)&&(!is.null(t.control)&!is.null(control.risk))) stop("Please only specify either the control event rate or the expected control risk at time t.control. The other one will be back calculted assuming constant baseline hazard.\n")
  if ((is.null(HR.margin))&(is.null(DRMST.margin))&(is.null(DS.margin))) stop("The Non-Inferiority margin should be specified in at least one of the three possible summary measures.\n")
  if (sum(is.null(HR.margin),is.null(DRMST.margin),is.null(DS.margin))<2) stop("Please only specify the Non-Inferiority margin in one of the three possible summary measures. The others will be back-calculated automatically.\n")
  if ((tau.RMST<=0)||(t.DS<=0)) stop("Horizon time should be positive.\n")
  if (is.null(tau.RMST)||is.null(t.DS<=0)) stop("Horizon time should be defined for both RMST and DS.\n")
  if (is.null(t.control)) t.control<-t.DS
  
  fun.rate <- function(rate,n,target.surv, tau) {
    
    test<-rexp(n,rate)
    res<-tau-quantile(test,c(target.surv))
    
    return(res)
  }
  
  if (is.null(rate.control.expected)) {
    rate.control.expected<-uniroot(fun.rate, c(1E-6, 1E6), tol = 0.000001, 
                              n=1000000, target.surv=control.risk, tau=t.control)$root
  }
  if (is.null(control.risk)) {
    test<-rexp(100000,rate.control.expected)
    control.risk<-mean(test<t.control)
  }
  
  RMST.margin<-function(lambda2,lambda, tau, target) {
    return(exp(-lambda2*tau)/lambda2-exp(0)/lambda2-(exp(-lambda*tau)/lambda-exp(0)/lambda)-target)
  }
  Diff.margin<-function(lambda2,lambda, t, target) {
    return(mean(rexp(1000000,lambda2)<t)-mean(rexp(1000000,lambda)<t)-target)
  }
  
  if (!is.null(HR.margin)) {
    rate.active.null<-HR.margin*rate.control.expected
  } else if (!is.null(DRMST.margin)) {
    rate.active.null<-uniroot(RMST.margin, c(1E-6, 20), tol = 0.000001, 
            lambda=rate.control.expected, tau=tau.RMST, 
            target=DRMST.margin)$root
  } else {
    rate.active.null<-uniroot(Diff.margin, c(1E-6, 20), tol = 0.000001, 
            lambda=rate.control.expected, t=t.DS, 
            target=DS.margin)$root
  }
  
  if (is.null(DRMST.margin)) {
    DRMST.margin<-RMST.margin(rate.active.null, rate.control.expected, tau.RMST,0)
  }
  if (is.null(DS.margin)) {
    DS.margin<-Diff.margin(rate.active.null, rate.control.expected, t.DS,0)
  }
  if (is.null(HR.margin)){
    HR.margin<-rate.active.null/rate.control.expected
  }
  
  if (is.null(control.rates)) {
    control.rates<-seq(max(0,rate.control.expected-0.3), rate.control.expected+0.3,0.02)
  }
  if (is.null(rate.active.alternative)) rate.active.alternative<-rate.control.expected
  
  ac.rates.HR<-ac.rates.DRMST<-ac.rates.DS<-rep(NA,length(control.rates))
  for (i in 1:length(control.rates)) {
    
    ac.rates.HR[i]<-control.rates[i]*HR.margin
    
    res1<-try(uniroot(RMST.margin, c(1E-6, 20), tol = 0.000001, 
                      lambda=control.rates[i], tau=tau.RMST, 
                      target=DRMST.margin)$root, silent = T)
    if (class(res1)!="try-error") ac.rates.DRMST[i]<-res1
    
    res2<-try(uniroot(Diff.margin, c(1E-6, 20), tol = 0.000001, 
                      lambda=control.rates[i], t=t.DS, 
                      target=DS.margin)$root, silent=T)
    if (class(res2)!="try-error") ac.rates.DS[i]<-res2
    
    cat(".")
  }
  cat("\n")
  
  palette("Okabe-Ito")
  
  par(mar = c(4.1, 4.1, 1.1, 1.1))
  plot(control.rates,control.rates, type="l", ylim=c(0,max(ac.rates.HR)), lty=2,
       main = "", xlab = "Control event rate",
       ylab="Active event rate", las=1)
  lines(control.rates, ac.rates.HR, type="l", col="#56B4E9", lwd=2)
  lines(control.rates, ac.rates.DRMST, type="l", lwd=2)
  lines(control.rates, ac.rates.DS, type="l", col="#009E73", lwd=2)
  points(rate.control.expected, rate.active.alternative, lwd=2, pch=1)
  points(rate.control.expected, rate.active.null, lwd=2, pch=3)
  
  euclidean <- function(a, b) sqrt(sum((a - b)^2))
  
  dist.HR<-dist.DS<-dist.DRMST<-rep(NA,length(control.rates))
  for (jj in 1:length(control.rates)) {
    dist.HR[jj]<-euclidean(c(rate.control.expected,rate.control.expected),
                       c(control.rates[jj],ac.rates.HR[jj]))
    dist.DS[jj]<-euclidean(c(rate.control.expected,rate.control.expected),
                       c(control.rates[jj],ac.rates.DS[jj]))
    dist.DRMST[jj]<-euclidean(c(rate.control.expected,rate.control.expected),
                       c(control.rates[jj],ac.rates.DRMST[jj]))
  }
  min.HR<-min(dist.HR, na.rm = T)
  min.DS<-min(dist.DS, na.rm = T)
  min.DRMST<-min(dist.DRMST, na.rm = T)
  
  which.min.dist<-order(c(min.HR, min.DRMST, min.DS))
  method<-c("HR", "DRMST", "DS")
  cat("HR Margin = ", HR.margin, ".\nDRMST Margin = ", DRMST.margin, 
  "\nDS Margin = ", DS.margin, "\nControl event rate: ", rate.control.expected, 
  "\nExpected ", t.control, "-year Control event risk = ", control.risk, 
  "\nThe most powerful summary measure for testing non-inferiority is ", method[which.min.dist[3]], ". Second best is ",
      method[which.min.dist[2]], ". The least powerful is ",
      method[which.min.dist[1]], ".\n The non-inferiority frontier plot shows the distance of various frontiers from the expected point (solid circle).\nBlack line: DRMST frontier.\nGreen line: DS frontier.\nBlue line: HR frontier.\nThe black cross represents the frontier point."
      ,sep="")
  results<-data.frame(control.rates, ac.rates.HR, ac.rates.DS, ac.rates.DRMST, 
                      dist.HR, dist.DS, dist.DRMST)
  return(results)
}


