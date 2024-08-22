### Functions to run bootstrap in test.NI.survival

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



### Functions to convert NI margins on survival outcome:
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


### Function to recursively estimate p in test.NI.binary

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

### function to bootstrap mean ratio:
ratios <- function(dat, indices) {
  d <- dat[indices,] # allows boot to select sample
  rat <- mean(d[d$treat==1,"y.comb"])/mean(d[d$treat==0,"y.comb"])
  return(rat)
} 