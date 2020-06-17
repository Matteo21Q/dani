analyse.durations <- function (outcomes, durations, family="binomial", 
                               se.method="bootstrap", all.durations=unique(durations), scale="RD", 
                               NI.margin, M.boot=NULL, n.cpus=1, sig.level=0.025) {
  min.dur<-min(durations)
  n.dur<-length(all.durations)
  max.dur<-max(durations)
  x.dur<-seq(min.dur, max.dur, length.out = 100)
  poss.durations <- all.durations[-n.dur]
  if (is.null(M.boot)) M.boot<-length(durations)
  data.mfp <- data.frame(outcomes, durations)
  if (family=="survival"){
    myformula<-as.formula(paste("Surv(", colnames(outcomes)[1], ",",
              colnames(outcomes)[2], ") ~ fp(durations)", sep=""))
  } else {
    myformula<-as.formula("outcomes~fp(durations)")
  }
  
  
  # Fit Fractional Polynomial regression model
  fit.fp<-fracpoly2fix(myformula, data.mfp, family=family)
  powers.fp<-fit.fp$selected.powers # these are the selected powers
  fit<-fit.fp$fit
  if (powers.fp[1]==0) {
    all.dur.p1<-log(all.durations-min.dur+0.5)
  } else {
    all.dur.p1<-(all.durations-min.dur+0.5)^powers.fp[1]
  }
  if (powers.fp[1]==powers.fp[2]) {
    all.dur.p2<-all.dur.p1*log(all.durations-min.dur+0.5)
  } else {
    if (powers.fp[2]==0) {
      all.dur.p2<-log(all.durations-min.dur+0.5) 
    } else {
      all.dur.p2<-(all.durations-min.dur+0.5)^powers.fp[2]
    }
  }
 
  if (family=="binomial") {
 
    if (se.method=="delta") {
      
      # Store predictions on the logodds scale at each of the n.arms durations:
      preds.logodds.fp<-predict(fit, newdata=data.frame(durations=all.durations))
      vcov.fit.fp<-vcov(fit)   # Store variance covariance matrix of parameters
      up.bounds.CI<-rep(NA,length(poss.durations))
      low.bounds.CI<-rep(NA,length(poss.durations))
      
      for (j in 1:(length(poss.durations))) {
        
        if (scale=="RD"||scale=="AF") {
          
          # Need to calculate the Jacobian for f= invlogit(a+d1x^p1+d1x^p2)-inv.logit(a+d2x^p1+d2x^p2)
          Jacob.fp<-c((exp(preds.logodds.fp[n.dur])/(1+exp(preds.logodds.fp[n.dur]))^2)-(exp(preds.logodds.fp[j])/(1+exp(preds.logodds.fp[j]))^2), 
                      (all.dur.p1[n.dur]*exp(preds.logodds.fp[n.dur])/(1+exp(preds.logodds.fp[n.dur]))^2)-(all.dur.p1[j]*exp(preds.logodds.fp[j])/(1+exp(preds.logodds.fp[j]))^2),
                      (all.dur.p2[n.dur]*exp(preds.logodds.fp[n.dur])/(1+exp(preds.logodds.fp[n.dur]))^2)-(all.dur.p2[j]*exp(preds.logodds.fp[j])/(1+exp(preds.logodds.fp[j]))^2))
          
          # As usual, thanks to delta method:
          var.diff.fp <- t(Jacob.fp)%*%vcov.fit.fp%*%Jacob.fp 
          se.diff.fp<-sqrt(var.diff.fp)
          mean.diff.fp<-inv.logit(preds.logodds.fp[n.dur])-inv.logit(preds.logodds.fp[j])
          CI.diff<-c(mean.diff.fp-qnorm(1-sig.level)*se.diff.fp, mean.diff.fp+qnorm(1-sig.level)*se.diff.fp)
          
          up.bounds.CI[j]<-CI.diff[2]
          low.bounds.CI[j]<-CI.diff[1]
          
        } else if (scale=="RR") {
          
          # Need to calculate the Jacobian for f= log(invlogit(a+d1x^p1+d1x^p2)/inv.logit(a+d2x^p1+d2x^p2))
          Jacob.fp<-c(inv.logit(preds.logodds.fp[n.dur])-inv.logit(preds.logodds.fp[j]), 
                      all.dur.p1[j]-all.dur.p1[n.dur]+all.dur.p1[n.dur]*inv.logit(preds.logodds.fp[n.dur])-all.dur.p1[j]*inv.logit(preds.logodds.fp[j]),
                      all.dur.p2[j]-all.dur.p2[n.dur]+all.dur.p2[n.dur]*inv.logit(preds.logodds.fp[n.dur])-all.dur.p2[j]*inv.logit(preds.logodds.fp[j]))
          
          # As usual, thanks to delta method:
          var.diff.fp <- t(Jacob.fp)%*%vcov.fit.fp%*%Jacob.fp 
          se.diff.fp<-sqrt(var.diff.fp)
          mean.diff.fp<-log(inv.logit(preds.logodds.fp[j])/inv.logit(preds.logodds.fp[n.dur]))
          CI.diff<-c(mean.diff.fp-qnorm(1-sig.level)*se.diff.fp, mean.diff.fp+qnorm(1-sig.level)*se.diff.fp)
          
          up.bounds.CI[j]<-exp(CI.diff[2])
          low.bounds.CI[j]<-exp(CI.diff[1])
          
        } else if (scale=="rate") {
          
          mean.diff.fp<-predict(fit,data.frame(durations=all.durations[j]), type="resp")
          se.diff.fp<-predict(fit,data.frame(durations=all.durations[j]), se.fit=T, type="resp")$se.fit     
          CI.diff<-c(mean.diff.fp-qnorm(1-sig.level)*se.diff.fp, mean.diff.fp+qnorm(1-sig.level)*se.diff.fp)
          
          up.bounds.CI[j]<-CI.diff[2]
          low.bounds.CI[j]<-CI.diff[1]
          
        }
        
      }
      
    } else if (se.method=="bootstrap") {
      
      # Function to bootstrap:
      
      min.duration.stack<-NULL
      find.min.dur<- function (data.mfp, indices) {
        # Select bootstrap sample:
        da <- data.mfp[indices,]
        
        # Fit Fractional Polynomial regression model
        fit.n.power<-fracpoly2fix(myformula, data=da, family = "binomial")
        fit.i<-fit.n.power$fit
        
        # Predict duration-response curve and associated pointwise CI
        min.dur<-min(all.durations)
        invisible(capture.output(y.dur.est<-predict(object=fit.i,newdata=data.frame(durations=x.dur), type="resp")))
        if ((scale=="RD")||(scale=="AF")) {
          invisible(capture.output(y.durations<-predict(fit.i, newdata=data.frame(durations=all.durations[n.dur]), type="resp")-predict(fit.i, newdata=data.frame(durations=poss.durations), type="resp")))
        } else if (scale=="RR") {
          invisible(capture.output(y.durations<-predict(fit.i, newdata=data.frame(durations=poss.durations), type="resp")/predict(fit.i, newdata=data.frame(durations=all.durations[n.dur]), type="resp")))
        } else if (scale=="rate") {
          invisible(capture.output(y.durations<-predict(fit.i, newdata=data.frame(durations=poss.durations), type="resp")))
        }
        
        #Define acceptability curve. 
        
        acceptability<-function(x) {
          
          if (scale=="RD") {
            return(y.dur.est[100]-NI.margin+0*x)
          } else if (scale=="RR") {
            return(y.dur.est[100]*NI.margin+0*x)
          } else if (scale=="rate") {
            return(NI.margin+0*x)
          } else if (scale=="AF") {
            return(y.dur.est[100]-NI.margin(x))
          }
        }
        
        y.accept<-acceptability(x.dur)
        
        # What is point where predicted lower CI first crosses acceptability curve?
        flag=t=1
        min.duration<-max.dur
        while ((t<length(x.dur))&(flag==1)) {
          if ((y.dur.est[t]-y.accept[t])>0) {
            flag=0
            min.duration<-x.dur[t]
          }
          t=t+1
        }
        output<-c(min.duration, y.durations)
        return(output)
      }
      
      results<-boot(data.mfp,find.min.dur,M.boot, parallel = "multicore", ncpus=n.cpus)
      if (length(unique(results$t[,1])) > 1) {
        res.ci<-boot.ci(results, conf=1-sig.level*2, type="perc", index=1)
        min.duration<- res.ci$perc[5]
        low.bound.dur<-res.ci$perc[4]
      } else {
        min.duration<-low.bound.dur<-results$t0[1]
      }
      up.bounds.CI<-NULL
      low.bounds.CI<-NULL
      for (indx in 2:length(results$t0)) {
        res.ci2<-boot.ci(results, conf=1-sig.level*2, type="bca", index=indx)
        up.bounds.CI<-c(up.bounds.CI, res.ci2$bca[5])
        low.bounds.CI<-c(low.bounds.CI, res.ci2$bca[4])
        
      }
      
      
      
    }
    
    # What is point where predicted lower CI first crosses acceptability curve?
    flag=t=1
    min.duration.i<-max.dur
    while ((t<length(all.durations))&(flag==1)) {
      if ((scale=="RD")) {
        if ((up.bounds.CI[t]-NI.margin)<0) {
          flag=0
          min.duration.i<-poss.durations[t]
        }
      } else if ((scale=="RR")|(scale=="rate")) {
        if ((low.bounds.CI[t]-NI.margin)>0) {
          flag=0
          min.duration.i<-poss.durations[t]
        }
      }else if (scale=="AF") {
        if ((up.bounds.CI[t]-NI.margin(poss.durations[t]))<0) {
          flag=0
          min.duration.i<-poss.durations[t]
        }
      }
      t=t+1
    }

    } else if (family=="survival") {
    if (se.method=="delta") {
      
      # Store predictions on the logodds scale at each of the n.arms durations:
      preds.hr.fp<-predict(fit, newdata=data.frame(durations=all.durations), type = "risk")
      vcov.fit.fp<-vcov(fit)   # Store variance covariance matrix of parameters
      up.bounds.CI<-rep(NA,length(poss.durations))
      low.bounds.CI<-rep(NA,length(poss.durations))
      
      for (j in 1:(length(poss.durations))) {
        
        if (scale=="HR"||scale=="AF") {
          
          # Need to calculate the Jacobian for f= invlogit(a+d1x^p1+d1x^p2)-inv.logit(a+d2x^p1+d2x^p2)
          Jacob.fp<-c(((all.dur.p1[j]-all.dur.p1[n.dur])*(preds.hr.fp[j]/preds.hr.fp[n.dur])),
                      ((all.dur.p2[j]-all.dur.p2[n.dur])*(preds.hr.fp[j]/preds.hr.fp[n.dur])))
          
          # As usual, thanks to delta method:
          var.diff.fp <- t(Jacob.fp)%*%vcov.fit.fp%*%Jacob.fp 
          se.diff.fp<-sqrt(var.diff.fp)
          mean.diff.fp<-log(preds.hr.fp[j]/preds.hr.fp[n.dur])
          CI.diff<-c(mean.diff.fp-qnorm(1-sig.level)*se.diff.fp, mean.diff.fp+qnorm(1-sig.level)*se.diff.fp)
          
          up.bounds.CI[j]<-exp(CI.diff[2])
          low.bounds.CI[j]<-exp(CI.diff[1])
        } 
        
      }
      
    } else if (se.method=="bootstrap") {
      
      # Function to bootstrap:
      
      min.duration.stack<-NULL
      find.min.dur<- function (data.mfp, indices) {
        # Select bootstrap sample:
        da <- data.mfp[indices,]
        
        # Fit Fractional Polynomial regression model
        fit.n.power<-fracpoly2fix(myformula, data=da, family = "survival")
        fit.i<-fit.n.power$fit
        
        # Predict duration-response curve and associated pointwise CI
        invisible(capture.output(y.dur.est<-predict(object=fit.i,newdata=data.frame(durations=x.dur), type="risk")))
        if ((scale=="HR")||(scale=="AF")) {
          invisible(capture.output(y.durations<-predict(fit.i, newdata=data.frame(durations=poss.durations), type="risk")/predict(fit.i, newdata=data.frame(durations=all.durations[n.dur]), type="risk")))
        } 
        
        acceptability<-function(x) {
          if (scale=="HR") {
            return(y.dur.est[100]*NI.margin+0*x)
          } else if (scale=="AF") {
            return(y.dur.est[100]*NI.margin(x))
          }
        }
        
        y.accept<-acceptability(x.dur)
        
        # What is point where predicted lower CI first crosses acceptability curve?
        flag=t=1
        min.duration<-max.dur
        while ((t<length(x.dur))&(flag==1)) {
          if ((y.dur.est[t]-y.accept[t])<0) {
            flag=0
            min.duration<-x.dur[t]
          }
          t=t+1
        }
        output<-c(min.duration, y.durations)
        return(output)
      }
      
      results<-boot(data.mfp,find.min.dur,M.boot, parallel = "multicore", ncpus=n.cpus)
      if (length(unique(results$t[,1])) > 1) {
        res.ci<-boot.ci(results, conf=1-sig.level*2, type="perc", index=1)
        min.duration<- res.ci$perc[5]
        low.bound.dur<-res.ci$perc[4]
      } else {
        min.duration<-low.bound.dur<-results$t0[1]
      }
      up.bounds.CI<-NULL
      low.bounds.CI<-NULL
      for (indx in 2:length(results$t0)) {
        res.ci2<-boot.ci(results, conf=1-sig.level*2, type="bca", index=indx)
        up.bounds.CI<-c(up.bounds.CI, res.ci2$bca[5])
        low.bounds.CI<-c(low.bounds.CI, res.ci2$bca[4])
        
      }
    }
      
      # What is point where predicted lower CI first crosses acceptability curve?
      flag=t=1
      min.duration.i<-max.dur
      while ((t<length(all.durations))&(flag==1)) {
        if (scale=="HR") {
          if ((up.bounds.CI[t]-NI.margin)<0) {
            flag=0
            min.duration.i<-poss.durations[t]
          }
        }else if (scale=="AF") {
          if ((up.bounds.CI[t]-NI.margin(poss.durations[t]))<0) {
            flag=0
            min.duration.i<-poss.durations[t]
          }
        }
        t=t+1
      }
      
    } else if (family=="gaussian") {
      if (se.method=="delta") {
        
        # Store predictions on the logodds scale at each of the n.arms durations:
        preds.fp<-predict(fit, newdata=data.frame(durations=all.durations))
        vcov.fit.fp<-vcov(fit)   # Store variance covariance matrix of parameters
        up.bounds.CI<-rep(NA,length(poss.durations))
        low.bounds.CI<-rep(NA,length(poss.durations))
        
        for (j in 1:(length(poss.durations))) {
          
          if (scale=="diff"||scale=="AF") {
            
            # Need to calculate the Jacobian for f= invlogit(a+d1x^p1+d1x^p2)-inv.logit(a+d2x^p1+d2x^p2)
            Jacob.fp<-c(0, 
                        (all.dur.p1[n.dur]-all.dur.p1[j]),
                        (all.dur.p2[n.dur]-all.dur.p2[j]))
            
            # As usual, thanks to delta method:
            var.diff.fp <- t(Jacob.fp)%*%vcov.fit.fp%*%Jacob.fp 
            se.diff.fp<-sqrt(var.diff.fp)
            mean.diff.fp<-preds.fp[n.dur]-preds.fp[j]
            CI.diff<-c(mean.diff.fp-qnorm(1-sig.level)*se.diff.fp, mean.diff.fp+qnorm(1-sig.level)*se.diff.fp)
            
            up.bounds.CI[j]<-CI.diff[2]
            low.bounds.CI[j]<-CI.diff[1]
            
          } else if (scale=="ratio") {
            
            # Need to calculate the Jacobian for f= log(invlogit(a+d1x^p1+d1x^p2)/inv.logit(a+d2x^p1+d2x^p2))
            Jacob.fp<-c((preds.fp[n.dur]-preds.fp[j])/(preds.fp[n.dur]^2), 
                        (all.dur.p1[j]*preds.fp[n.dur]-all.dur.p1[n.dur]*preds.fp[j])/(preds.fp[n.dur]^2),
                        (all.dur.p2[j]*preds.fp[n.dur]-all.dur.p2[n.dur]*preds.fp[j])/(preds.fp[n.dur]^2))
            
            # As usual, thanks to delta method:
            var.diff.fp <- t(Jacob.fp)%*%vcov.fit.fp%*%Jacob.fp 
            se.diff.fp<-sqrt(var.diff.fp)
            mean.diff.fp<-preds.fp[j]/preds.fp[n.dur]
            CI.diff<-c(mean.diff.fp-qnorm(1-sig.level)*se.diff.fp, mean.diff.fp+qnorm(1-sig.level)*se.diff.fp)
            
            up.bounds.CI[j]<-CI.diff[2]
            low.bounds.CI[j]<-CI.diff[1]
            
          } else if (scale=="target") {
            
            mean.diff.fp<-predict(fit,data.frame(durations=all.durations[j]))
            se.diff.fp<-predict(fit,data.frame(durations=all.durations[j]), se.fit=T)$se.fit     
            CI.diff<-c(mean.diff.fp-qnorm(1-sig.level)*se.diff.fp, mean.diff.fp+qnorm(1-sig.level)*se.diff.fp)
            
            up.bounds.CI[j]<-CI.diff[2]
            low.bounds.CI[j]<-CI.diff[1]
            
          }
          
        }
        
      } else if (se.method=="bootstrap") {
        
        # Function to bootstrap:
        
        min.duration.stack<-NULL
        find.min.dur<- function (data.mfp, indices) {
          # Select bootstrap sample:
          da <- data.mfp[indices,]
          
          # Fit Fractional Polynomial regression model
          fit.n.power<-fracpoly2fix(myformula, data=da, family = "gaussian")
          fit.i<-fit.n.power$fit
          
          # Predict duration-response curve and associated pointwise CI
          invisible(capture.output(y.dur.est<-predict(object=fit.i,newdata=data.frame(durations=x.dur))))
          if ((scale=="diff")||(scale=="AF")) {
            invisible(capture.output(y.durations<-predict(fit.i, newdata=data.frame(durations=all.durations[n.dur]))-predict(fit.i,newdata=data.frame(durations=poss.durations))))
          } else if (scale=="ratio") {
            invisible(capture.output(y.durations<-predict(fit.i,newdata=data.frame(durations=poss.durations))/predict(fit.i,newdata=data.frame(durations=all.durations[n.dur]))))
          } else if (scale=="target") {
            invisible(capture.output(y.durations<-predict(fit.i,newdata=data.frame(durations=poss.durations))))
          }
          
          acceptability<-function(x) {
            if (scale=="diff") {
              return(y.dur.est[100]-NI.margin+0*x)
            } else if (scale=="ratio") {
              return(y.dur.est[100]*NI.margin+0*x)
            } else if (scale=="target") {
              return(NI.margin+0*x)
            } else if (scale=="AF") {
              return(y.dur.est[100]-NI.margin(x))
            }
          }
          y.accept<-acceptability(x.dur)
          
          # What is point where predicted lower CI first crosses acceptability curve?
          flag=t=1
          min.duration<-max.dur
          while ((t<length(x.dur))&(flag==1)) {
            if ((y.dur.est[t]-y.accept[t])>0) {
              flag=0
              min.duration<-x.dur[t]
            }
            t=t+1
          }
          output<-c(min.duration, y.durations)
          return(output)
        }
        
        results<-boot(data.mfp,find.min.dur,M.boot, parallel = "multicore", ncpus=n.cpus)
        if (length(unique(results$t[,1])) > 1) {
          res.ci<-boot.ci(results, conf=1-sig.level*2, type="perc", index=1)
          min.duration<- res.ci$perc[5]
          low.bound.dur<-res.ci$perc[4]
        } else {
          min.duration<-low.bound.dur<-results$t0[1]
        }
        up.bounds.CI<-NULL
        low.bounds.CI<-NULL
        for (indx in 2:length(results$t0)) {
          res.ci2<-boot.ci(results, conf=1-sig.level*2, type="bca", index=indx)
          up.bounds.CI<-c(up.bounds.CI, res.ci2$bca[5])
          low.bounds.CI<-c(low.bounds.CI, res.ci2$bca[4])
          
        }
        
        
        
      }
      
      # What is point where predicted lower CI first crosses acceptability curve?
      flag=t=1
      min.duration.i<-max.dur
      while ((t<length(poss.durations))&(flag==1)) {
        if ((scale=="diff")) {
          if ((up.bounds.CI[t]-NI.margin)<0) {
            flag=0
            min.duration.i<-poss.durations[t]
          }
        } else if ((scale=="ratio")|(scale=="target")) {
          if ((low.bounds.CI[t]-NI.margin)>0) {
            flag=0
            min.duration.i<-poss.durations[t]
          }
        }else if (scale=="AF") {
          if ((up.bounds.CI[t]-NI.margin(poss.durations[t]))<0) {
            flag=0
            min.duration.i<-poss.durations[t]
          }
        }
        t=t+1
      }
      
  }
  
   
  if (se.method=="bootstrap") {
    
    # Now second bootstrap method: What is optimal duration arm?
    flag=t=1
    while ((t<(length(all.durations)+1))&(flag==1)) {
      if ((min.duration)<=all.durations[t]) {
        flag=0
        up.bound.dur<-min.duration
        min.duration<-all.durations[t]
        
      }
      t=t+1
      
    }
    
    
    
  } else {
    min.duration<-NULL
    low.bound.dur<-up.bound.dur<-NULL
  }
  
  results<-list( model.fit = fit, min.duration = min.duration.i, 
                min.dur.boot = min.duration, up.bounds.CI = up.bounds.CI, 
                low.bounds.CI = low.bounds.CI, all.durations = all.durations,
                NI.margin = NI.margin, se.method = se.method, family = family,
                scale = scale, low.bound.dur = low.bound.dur, 
                up.bound.dur = up.bound.dur )
  class(results)<-c("durations", "list")
  return(results)
  
}