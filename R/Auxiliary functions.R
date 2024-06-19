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


RMST.diff.flexsurv<- function(data, index, tau, k, knots, bknots) {
  
  datai<-data[index,]
  datai<-datai[order(datai$time),]
  
  fit.flexsurv.boot<-flexsurvspline(Surv(time,event)~treat, k=k, knots=knots, bknots=bknots, data=datai)
  n.parboot<-length(coef(fit.flexsurv.boot))
  betaboot<-coef(fit.flexsurv.boot)[n.parboot]
  sboot<-NULL
  kboot<-NULL
  for (indx in 1:(n.parboot-1)) {
    
    sboot<-c(sboot,coef(fit.flexsurv.boot)[indx])
    kboot<-c(kboot,fit.flexsurv.boot$knots[indx])

  }
  parameters<-c(tau, betaboot, sboot, kboot)
  
  DRMST_Est<-DRMST.estimator(parameters)
  return(c(DRMST_Est))
}

Surv.diff.flexsurv<- function(data, index, tau, k, knots, bknots) {
  
  datai<-data[index,]
  datai<-datai[order(datai$time),]
  
  fit.flexsurv.boot<-flexsurvspline(Surv(time,event)~treat, k=k, knots=knots, bknots=bknots, data=datai)
  n.parboot<-length(coef(fit.flexsurv.boot))
  betaboot<-coef(fit.flexsurv.boot)[n.parboot]
  sboot<-NULL
  kboot<-NULL
  for (indx in 1:(n.parboot-1)) {
    
    sboot<-c(sboot,coef(fit.flexsurv.boot)[indx])
    kboot<-c(kboot,fit.flexsurv.boot$knots[indx])
    
  }
  parameters<-c(tau, betaboot, sboot, kboot)
  
  DS_Est<-DS.estimator(parameters)
  return(c(DS_Est))
}

# Function to estimate survival from flexsurv fit:

surv.est<-function(tt, betas, sp, gammas, active, kn) {
  
  S0<-sp[1]+(sp[2]+gammas[1]*active)*log(tt)
  l.k<-length(kn)
  
  if (l.k>2) {
    for (indx in 1:(l.k-2)) {
      
      nu.indx<-max(0,(log(tt)-kn[indx+1])^3)-(kn[l.k]-kn[indx+1])/(kn[l.k]-kn[1])*max(0,(log(tt)-kn[1])^3)-
        (1-(kn[l.k]-kn[indx+1])/(kn[l.k]-kn[1]))*max(0,(log(tt)-kn[l.k])^3)
      S0<-S0+(sp[indx+2]+gammas[indx+1]*active)*nu.indx
      
    }
  }
 

  logres<-as.numeric(S0+active*betas[1])
  
  
  return(exp(-exp(logres)))
}

# Function to estimate DS from flexsurv fit:

DS.estimator<-function(args) {
  
  x=as.numeric(args[1]) 
  beta=as.numeric(args[2]) 
  n.par.sp<-length(args)/2-1
  sp<-NULL
  for (indx in 1:(n.par.sp)) {
    sp<-c(sp,as.numeric(args[indx+2]))
  }
  kn<-NULL
  for (indx in 1:(n.par.sp)) {
    kn<-c(kn,as.numeric(args[indx+2+n.par.sp]))
  }
  
  surv.est(x,beta, sp, 1, kn)-surv.est(x,beta, sp, 0, kn)
}

# Function to estimate DS from flexsurv fit within adaptive quadrature:

DS.integrate<-function(x, beta, sp, kn, gammas) {
  
  surv.est(x,beta, sp, gammas, 1, kn)-surv.est(x,beta, sp, gammas, 0, kn)
  
}

# Function to estimate DRMST from flexsurv fit:

DRMST.estimator<-function(args) {
  tt=as.numeric(args[1]) 
  n.par.sp<-as.numeric(args[length(args)])
  n.cov<-as.numeric(args[length(args)-1])
  beta=as.numeric(args[2:(n.cov+1)])
  sp<-NULL
  for (indx in 1:(n.par.sp)) {
    sp<-c(sp,as.numeric(args[indx+1+n.cov]))
  }
  kn<-NULL
  for (indx in 1:(n.par.sp)) {
    kn<-c(kn,as.numeric(args[indx+1+n.cov+n.par.sp]))
  }
  gammas<-NULL
  for (indx in 1:(n.par.sp-1)) {
    gammas<-c(gammas,as.numeric(args[indx+1+n.cov+2*n.par.sp]))
  }
  
  return(suppressWarnings(quad(DS.integrate,xa=0, xb=tt,beta=beta, sp=sp, kn=kn, gammas=gammas)))
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
  return(-(exp(-lambda2*tau)/lambda2-exp(0)/lambda2-(exp(-lambda*tau)/lambda-exp(0)/lambda))-target)
}

Diff.margin<-function(lambda2,lambda, t, target) {
  return(-pexp(t,lambda2)+pexp(t,lambda)-target)
}

RMST.margin.flex<-function(S.control, HR, tau, target) {
  diff.S<-function(t) {
    S.control(t)^HR-S.control(t)
  }
  NIm <- suppressWarnings(quad(diff.S,xa=0, xb=tau))
  
  return(NIm-target)
}

Diff.margin.flex<-function(S.control, HR, t, target) {
  return(S.control(t)^HR-S.control(t)-target)
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