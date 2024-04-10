RMST_Cox <- function(survf, tau){
  int <- (survf$time[1] - 0)*1 #1 is the initial survival number: 1
  for(i in 2:(length(survf$time))){
    if(survf$time[i] < tau){
      int <- int + (survf$time[i] - survf$time[i-1])*survf$surv[i-1]
    }
    else{
      int <- int + (tau - survf$time[i-1])*survf$surv[i-1]
      break
    }
  }
  return(int)
}

RMST.diff<- function(data, index, tau) {
  
  datai<-data[index,]
  datai<-datai[order(datai$time),]
  
  fit.cox.i<-coxph(Surv(time,event)~treat, data=datai)
  newd.cont<-data.frame(treat=0)
  newd.cont$treat<-as.factor(newd.cont$treat)
  survCont<-survfit(fit.cox.i, newdata = newd.cont)
  newd.act<-data.frame(treat=1)
  newd.act$treat<-as.factor(newd.act$treat)
  survAct<-survfit(fit.cox.i, newdata = newd.act)
  RMST_Cont<-RMST_Cox(survCont,tau)
  RMST_Act<-RMST_Cox(survAct,tau)
  DRMST_Est<-RMST_Act-RMST_Cont
  return(c(DRMST_Est))
}

surv.diff<- function(data, index, tau) {
  
  datai<-data[index,]
  
  fit.cox.i<-coxph(Surv(time,event)~treat, data=datai)
  survCont<-survfit(fit.cox.i, newdata = data.frame(treat=0))
  survAct<-survfit(fit.cox.i, newdata = data.frame(treat=1))
  
  Diff_est<-summary(survAct, times=tau, extend = TRUE)$surv-summary(survCont, times=tau, extend = TRUE)$surv
  
  return(c(Diff_est))
}


RMST.diff.flexsurv<- function(data, index, tau) {
  
  datai<-data[index,]
  datai<-datai[order(datai$time),]
  
  fit.flexsurv.boot<-flexsurvspline(Surv(time,event)~treat, k=2, data=datai)
  beta1boot<-coef(fit.flexsurv.boot)[5]
  s0boot<-coef(fit.flexsurv.boot)[1]
  s1boot<-coef(fit.flexsurv.boot)[2]
  s2boot<-coef(fit.flexsurv.boot)[3]
  s3boot<-coef(fit.flexsurv.boot)[4]
  k1boot<-fit.flexsurv.boot$knots[1]
  k2boot<-fit.flexsurv.boot$knots[2]
  k3boot<-fit.flexsurv.boot$knots[3]
  k4boot<-fit.flexsurv.boot$knots[4]
  
  DRMST_Est<-DRMST.est(beta1boot, s0boot, s1boot, s2boot, s3boot, tau, datai, k1boot, k2boot, k3boot, k4boot)
  return(c(DRMST_Est))
}

Surv.diff.flexsurv<- function(data, index, tau) {
  
  datai<-data[index,]
  datai<-datai[order(datai$time),]
  
  fit.flexsurv<-flexsurvspline(Surv(time,event)~treat, k=2, data=datai)
  beta1<-coef(fit.flexsurv)[5]
  s0<-coef(fit.flexsurv)[1]
  s1<-coef(fit.flexsurv)[2]
  s2<-coef(fit.flexsurv)[3]
  s3<-coef(fit.flexsurv)[4]
  k1<-fit.flexsurv$knots[1]
  k2<-fit.flexsurv$knots[2]
  k3<-fit.flexsurv$knots[3]
  k4<-fit.flexsurv$knots[4]
  
  DS_Est<-DS.est(beta1, s0, s1, s2, s3, k1,k2,k3,k4, tau)
  return(c(DS_Est))
}


# Function to estimate RMST from Royston-Parmar

RMST_RP.Beta <- function(beta, sp0, sp1, sp2, sp3, tau, active, dat, k1, k2, k3, k4){
  int <- (dat$time[1] - 0)*1 #1 is the initial survival number: 1
  for(ii in 2:(length(dat$time))){
    if(dat$time[ii] < tau){
      int <- int + (dat$time[ii] - dat$time[ii-1])*surv.est(beta, sp0, sp1, sp2, sp3,active,dat$time[ii-1], k1, k2, k3, k4)
    }
    else{
      int <- int + (tau - dat$time[ii-1])*surv.est(beta, sp0, sp1, sp2, sp3,active,dat$time[ii-1], k1, k2, k3, k4)
      break
    }
  }
  return(int)
}


# Function to estimate difference in RMST:

DRMST.est<-function(beta, sp0, sp1, sp2, sp3, tau, data, k1, k2, k3, k4) {
  res<-RMST_RP.Beta(beta, sp0, sp1, sp2, sp3,tau,1,data, k1, k2, k3, k4)-RMST_RP.Beta(beta, sp0, sp1, sp2, sp3,tau,0, data, k1, k2, k3, k4)
  return(res)
}

# Function to estimate 5 partial derivatives
Deriv1.DRMST<-function( sp0, dat,beta1, s1, s2, s3, k1, k2, k3, k4, tau ) {
  return(DRMST.est(beta1, sp0, s1, s2, s3, tau, dat, k1, k2, k3, k4))
}
Deriv2.DRMST<-function( sp1,dat,beta1, s0, s2, s3, k1, k2, k3, k4, tau) {
  return(DRMST.est(beta1, s0, sp1, s2, s3, tau, dat, k1, k2, k3, k4))
}
Deriv3.DRMST<-function( sp2,dat,beta1, s1, s0, s3, k1, k2, k3, k4, tau) {
  return(DRMST.est(beta1, s0, s1, sp2, s3, tau, dat, k1, k2, k3, k4))
}
Deriv4.DRMST<-function( sp3,dat,beta1, s1, s2, s0, k1, k2, k3, k4,tau) {
  return(DRMST.est(beta1, s0, s1, s2, sp3, tau, dat, k1, k2, k3, k4))
}
Deriv5.DRMST<-function(beta,dat,s0, s1, s2, s3, k1, k2, k3, k4, tau) {
  return(DRMST.est(beta, s0, s1, s2, s3, tau, dat, k1, k2, k3, k4))
}

# Function to estimate survival prob at time tt given Royston-Parmar parameter estimates

surv.est<-function(beta, sp0, sp1, sp2, sp3, active, tt, k1, k2, k3, k4) {
  S0<-sp0+sp1*log(tt)+sp2*(max(0,(log(tt)-k2)^3)-(k4-k2)/(k4-k1)*max(0,(log(tt)-k1)^3)-
                             (1-(k4-k2)/(k4-k1))*max(0,(log(tt)-k4)^3))+
    sp3*(max(0,(log(tt)-k3)^3)-(k4-k3)/(k4-k1)*max(0,(log(tt)-k1)^3)-
           (1-(k4-k3)/(k4-k1))*max(0,(log(tt)-k4)^3))
  logres<-as.numeric(S0+active*beta)
  
  return(exp(-exp(logres)))
}

# Function to estimate risk difference at time tau:

DS.est<-function(beta, sp0, sp1, sp2, sp3, k1, k2, k3, k4, tau) {
  surv.est(beta, sp0, sp1, sp2, sp3,1,tau, k1, k2, k3, k4)-surv.est(beta, sp0, sp1, sp2, sp3,0,tau, k1, k2, k3, k4)
}


# Function to estimate 5 partial derivatives
Deriv1.DS<-function( sp0, beta1, s1, s2, s3, k1, k2, k3, k4, tau) {
  return(DS.est(beta1, sp0, s1, s2, s3, k1, k2, k3, k4, tau))
}
Deriv2.DS<-function( sp1, beta1, s0, s2, s3, k1, k2, k3, k4, tau) {
  return(DS.est(beta1, s0, sp1, s2, s3, k1, k2, k3, k4, tau))
}
Deriv3.DS<-function( sp2, beta1, s0, s1, s3, k1, k2, k3, k4, tau) {
  return(DS.est(beta1, s0, s1, sp2, s3, k1, k2, k3, k4, tau))
}
Deriv4.DS<-function( sp3, beta1, s0, s1, s2, k1, k2, k3, k4, tau) {
  return(DS.est(beta1, s0, s1, s2, sp3, k1, k2, k3, k4, tau))
}
Deriv5.DS<-function( beta, s0, s1, s2, s3, k1, k2, k3, k4, tau) {
  return(DS.est(beta, s0, s1, s2, s3, k1, k2, k3, k4, tau))
}

# Functions to convert NI margins on survival outcome:
fun.rate <- function(rate,target.surv, time, dist="exp") {
  if (dist=="exp") {
    test<-qexp(target.surv, rate)
  } else if (dist=="weib2") {
    test<-qweibull(target.surv,2,rate)
  } else if (dist=="weib05") {
    test<-qweibull(target.surv,0.5,rate)
  }
  res<-time-test
  
  return(res)
}

RMST.margin<-function(lambda2,lambda, tau, target) {
  return(exp(-lambda2*tau)/lambda2-exp(0)/lambda2-(exp(-lambda*tau)/lambda-exp(0)/lambda)-target)
}

Diff.margin<-function(lambda2,lambda, t, target) {
  return(pexp(t,lambda2)-pexp(t,lambda)-target)
}

## Function to recursively estimate p in test.NI.binary

func.to.opt<-function(x,n.control, n.experim, e.control, e.experim,
                      NI.margin, summary.measure, 
                      unfavourable, test.type,
                      M.boot, BB.adj) {
  out<-test.NI.binary(n.control=n.control, n.experim=n.experim, e.control=e.control, e.experim=e.experim,
                      NI.margin=NI.margin, sig.level=x, summary.measure=summary.measure, 
                      print.out=FALSE, unfavourable=unfavourable, test.type=test.type,
                      M.boot=M.boot, BB.adj=BB.adj, recursive.p.estim=FALSE)
  if (isTRUE(unfavourable)) {
    extreme<-ifelse(out$estimate>NI.margin,
                    out$CI[1]-NI.margin, out$CI[2]-NI.margin)
  } else {
    extreme<-ifelse(out$estimate<NI.margin,
                    out$CI[2]-NI.margin, out$CI[1]-NI.margin)
  }
  return(extreme)
}

# function to bootstrap mean ratio:
ratios <- function(dat, indices) {
  d <- dat[indices,] # allows boot to select sample
  rat <- mean(d[d$treat==1,"y.comb"])/mean(d[d$treat==0,"y.comb"])
  return(rat)
} 