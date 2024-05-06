test.NI.survival <- function(time, event, treat, covariates=NULL, NI.margin, sig.level=0.025, summary.measure="HR", 
                               print.out=TRUE, unfavourable=TRUE, test.type=NULL,
                               M.boot=2000, bootCI.type="bca", tau=NULL, control.level=NULL) {
  
  stopifnot(is.numeric(time), is.vector(time), length(time)>2, all(time>0))
  stopifnot(is.vector(event), nlevels(as.factor(event))%in%c(1,2), length(time)==length(event))
  stopifnot(is.vector(treat), nlevels(as.factor(treat))==2, length(time)==length(treat))
  treat<-as.factor(treat)
  if (!is.null(control.level)) stopifnot(control.level%in%levels(treat))
  treat<-relevel(treat, ref = control.level)
  if (!is.null(covariates)) {
    stopifnot(is.data.frame(covariates))
    if (is_tibble(covariates)) covariates<-as.data.frame(covariates)
  }
  covariate.formula<-NULL
  if (length(covariates)!=0) {
    covariate.formula<-"+"
    for (cc in 1:length(covariates)) {
      covariate.formula <- paste(covariate.formula, colnames(covariates)[cc])
      if (cc!=length(covariates)) covariate.formula<-paste(covariate.formula, "+")
    }
  }
  myformula<-as.formula(paste("Surv(time, event)~treat", covariate.formula))
  if (!is.null(covariates))  {
    mydata <- data.frame(time, event, treat, covariates)
    colnames(mydata)[4:ncol(mydata)]<-colnames(covariates)
  } else {
    mydata <- data.frame(time, event, treat)
  }
  assign("mydata", mydata, envir = .GlobalEnv)
  
  stopifnot(is.numeric(NI.margin))
  stopifnot(is.numeric(sig.level), sig.level < 0.5, sig.level > 0)
  stopifnot(is.character(summary.measure), summary.measure %in% c("HR", "DRMST", "DS"))
  stopifnot(is.logical(print.out), !is.na(print.out))
  stopifnot(is.character(bootCI.type), bootCI.type%in%c("norm","perc","bca","basic"))
  stopifnot(is.logical(unfavourable), !is.na(unfavourable))
  stopifnot(is.numeric(M.boot), M.boot>1)
  if (is.null(test.type)) {
    if (summary.measure=="HR") {
      test.type<-"Cox.PH"
    } else {
      test.type<-"flexsurv.PH.delta"
    } 
  }
  adjusted<-(!is.null(covariates)&&(ncol(covariates)>0))
  stopifnot(is.character(test.type))
  if (summary.measure=="HR") {
    stopifnot(test.type%in%c("Cox.PH", "flexsurv.PH", "Cox.weighted"))
  } else if (summary.measure=="DRMST") {
    if (!adjusted) {
      stopifnot(test.type%in%c("KM", "Cox.PH.bootstrap", "flexsurv.PH.delta", "flexsurv.PH.bootstrap"))
    } else {
      stopifnot(test.type%in%c( "Cox.PH.bootstrap", "flexsurv.PH.delta", "flexsurv.PH.bootstrap"))
    }
  } else {
    if (!adjusted) {
      stopifnot(test.type%in%c("Newcombe10", "Cox.PH.bootstrap", "flexsurv.PH.delta", "flexsurv.PH.bootstrap", "Cox.weighted"))
    } else {
      stop("Covariate adjustment not currently supported for summary measures different from HR.\n")
    }
  }
  
  estimate<-se<-Z<-p<-NULL
  dd<-data.frame(time, treat, event)
  dd<-dd[order(dd$time),]
  
  if (summary.measure=="HR") {
    stopifnot(NI.margin>0)
    if ((unfavourable == T)&&(NI.margin<=1)) stop("When events are unfavourable (e.g. deaths), a HR NI margin needs to be >1.\n")
    if ((unfavourable == F)&&(NI.margin>=1)) stop("When events are favourable (e.g. cure), a HR NI margin needs to be <1.\n")
    
    if (test.type=="Cox.PH") {
      fit<-coxph(myformula, mydata)
      estimate<-as.numeric(exp(fit$coefficients["treat1"]))
      CI<-as.numeric(exp(confint(fit, level=1-2*sig.level)["treat1",]))
    } else if (test.type=="flexsurv.PH") {
      fit<-flexsurvspline(myformula, k=2, data=mydata)
      estimate<-as.numeric(exp(fit$coefficients["treat1"]))
      CI<-as.numeric(exp(confint(fit, level=1-2*sig.level)["treat1",]))
    } else if (test.type=="Cox.weighted") {
      fit<-coxphw(myformula, data=mydata)
      estimate<-as.numeric(exp(fit$coefficients["treat1"]))
      CI<-as.numeric(exp(confint(fit, level=1-2*sig.level)["treat1",]))
    }  
    
    estimate.n<-mean(CI)
    if (is.null(se)) {
      se<-(CI[2]-CI[1])/(2*qnorm(1-sig.level))
      is.se.est<-T
    } else {
      is.se.est<-F
    }
    if (is.null(p)) {
      Z <- ifelse(unfavourable==T,(estimate.n - NI.margin)/se,(-estimate.n + NI.margin)/se)
      p <- pnorm(Z)
      is.p.est<-T
    } else {
      is.p.est<-F
    }
    if ((unfavourable==F&&CI[1]>NI.margin)||(unfavourable==T&&CI[2]<NI.margin)) { 
      non.inferiority<-T
    } else {
      non.inferiority<-F
    }
    if (print.out==T) {
      cat("Testing for non-inferiority.\nSummary measure: Hazard Ratio.\nNon-inferiority margin = ", NI.margin, ".\nMethod: ",test.type,
          ".\nEstimate = ", estimate, 
          "\nConfidence interval (Two-sided ", (1-sig.level*2)*100,"%): (", CI[1], ",", CI[2], 
          ")\np-value = ", p, ".\n" , sep="")
      if ((unfavourable==F&&CI[1]>NI.margin)||(unfavourable==T&&CI[2]<NI.margin)) { 
        cat("The confidence interval does not cross the null ( Hazard Ratio = ", NI.margin, " ), and hence we have evidence of non-inferiority.\n", sep="")
      } else {
        cat("The confidence interval crosses the null ( Hazard Ratio = ", NI.margin, " ), and hence we have NO evidence of non-inferiority.\n", sep="")
      }
      if (is.p.est==T) {
        if (is.se.est==T) {
          cat("Note: with the test = ",test.type, " for summary measure = ", summary.measure,", p-value and standard error are only approximations based on a Z test for given estimate and CI.\n")
        } else {
          cat("Note: with the test = ",test.type, " for summary measure = ", summary.measure,", p-value is only an approximation based on a Z test with given estimate and CI.\n")
        }
      }
      
    }
    
  } else if (summary.measure == "DRMST") {
    stopifnot(is.numeric(tau), tau>0)
    if ((unfavourable == T)&&(NI.margin>=0)) stop("When events are unfavourable (e.g. death), a NI margin as a difference in RMST needs to be <0.")
    if ((unfavourable == F)&&(NI.margin<=0)) stop("When events are favourable (e.g. cure), a NI margin as a difference in RMST needs to be >0.")
    if (test.type=="KM") {
      fit.RMST<-rmst2(dd$time, dd$event, as.numeric(dd$treat!=control.level), tau=tau, alpha=sig.level*2)
      CI<-as.numeric(fit.RMST$unadjusted.result[1,2:3])
      estimate<-as.numeric(fit.RMST$unadjusted.result[1,1])
    } else if (test.type=="Cox.PH.bootstrap") {
      res<-boot(dd, RMST.diff, tau=tau, R=M.boot)
      CI<-boot.ci(res, index=1, type=bootCI.type, conf=1-sig.level*2)[[4]][4:5-2*(bootCI.type=="norm")]
      estimate<-res$t0     
    } else if (test.type=="flexsurv.PH.delta") {
      fit.flexsurv<-flexsurvspline(Surv(time,event)~treat, k=2, data=dd)
      varcov<-vcov(fit.flexsurv)
      beta1<-coef(fit.flexsurv)[5]
      s0<-coef(fit.flexsurv)[1]
      s1<-coef(fit.flexsurv)[2]
      s2<-coef(fit.flexsurv)[3]
      s3<-coef(fit.flexsurv)[4]
      k1<-fit.flexsurv$knots[1]
      k2<-fit.flexsurv$knots[2]
      k3<-fit.flexsurv$knots[3]
      k4<-fit.flexsurv$knots[4]

      gradsb<-c(grad(Deriv1.DRMST,s0, dat=dd,s1=s1,s2=s2,s3=s3,beta1=beta1, k1=k1, k2=k2, k3=k3, k4=k4, tau=tau),
                grad(Deriv2.DRMST,s1, dat=dd,s0=s0,s2=s2,s3=s3,beta1=beta1, k1=k1, k2=k2, k3=k3, k4=k4, tau=tau),
                grad(Deriv3.DRMST,s2, dat=dd,s0=s0,s1=s1,s3=s3,beta1=beta1, k1=k1, k2=k2, k3=k3, k4=k4, tau=tau), 
                grad(Deriv4.DRMST,s3, dat=dd,s0=s0,s1=s1,s2=s2,beta1=beta1, k1=k1, k2=k2, k3=k3, k4=k4, tau=tau),
                grad(Deriv5.DRMST,beta1, dat=dd,s0=s0,s1=s1,s3=s3,s2=s2, k1=k1, k2=k2, k3=k3, k4=k4, tau=tau))
      estimate<-DRMST.est(beta1, s0, s1, s2, s3,tau,dd,k1,k2,k3,k4)
      se<-sqrt(t(gradsb)%*%varcov%*%gradsb)
      low.bndb<-(estimate-qnorm(1-sig.level)*se)
      up.bndb<-(estimate+qnorm(1-sig.level)*se)
      CI<-c(low.bndb, up.bndb)
    } else if (test.type=="flexsurv.PH.bootstrap") {
      res<-boot(dd, RMST.diff.flexsurv, tau=tau, R=M.boot)
      CI<-boot.ci(res, index=1, type=bootCI.type, conf=1-sig.level*2)[[4]][4:5-2*(bootCI.type=="norm")]
      estimate<-as.numeric(res$t0)    
    } 
    if (is.null(se)) {
      se<-(CI[2]-CI[1])/(2*qnorm(1-sig.level))
      is.se.est<-T
    } else {
      is.se.est<-F
    }
    if (is.null(p)) {
      Z <- ifelse( unfavourable==T, -(estimate - NI.margin)/se, (estimate - NI.margin)/se)
      p <- pnorm(Z)
      is.p.est<-T
    } else {
      is.p.est<-F
    }
    if ((unfavourable==T&&CI[1]>NI.margin)||(unfavourable==F&&CI[2]<NI.margin)) { 
      non.inferiority<-T
    } else {
      non.inferiority<-F
    }
    if (print.out==T) {
      cat("Testing for non-inferiority.\nSummary measure: Difference in RMST.\nNon-inferiority margin = ", NI.margin, ".\nMethod: ",test.type,
          ".\nEstimate = ", estimate, 
          "\nConfidence interval (Two-sided ", (1-sig.level*2)*100,"%): (", CI[1], ",", CI[2], 
          ")\np-value = ", p, ".\n" , sep="")
      if ((unfavourable==T&&CI[1]>NI.margin)||(unfavourable==F&&CI[2]<NI.margin)) { 
        cat("The confidence interval does not cross the null ( DRMST = ", NI.margin, " ), and hence we have evidence of non-inferiority.\n", sep="")
      } else {
        cat("The confidence interval crosses the null ( DRMST = ", NI.margin, " ), and hence we have NO evidence of non-inferiority.\n", sep="")
      }
      if (is.p.est==T) {
        if (is.se.est==T) {
          cat("Note: with the test = ",test.type, " for summary measure = ", summary.measure,", p-value and standard error are only approximations based on a Z test with given estimate and CI.\n")
        } else {
          cat("Note: with the test = ",test.type, " for summary measure = ", summary.measure,", p-value is only an approximation based on a Z test with given estimate and CI.\n")
        }
      }
      
    }
    
  } else if (summary.measure=="DS") {
    stopifnot(is.numeric(tau), tau>0)
    if ((unfavourable == T)&&(NI.margin>=0)) stop("When events are unfavourable (e.g. death), a NI margin as a difference in survival needs to be <0.")
    if ((unfavourable == F)&&(NI.margin<=0)) stop("When events are favourable (e.g. cure), a NI margin as a difference in survival needs to be >0.")
    if (test.type=="Newcombe10") {
      n0<-sum(treat==levels(factor(treat))[1])
      n1<-sum(treat==levels(factor(treat))[2])
      e0<-sum(((dd[dd$treat==levels(factor(dd$treat))[1],"event"]==1)&(dd[dd$treat==levels(factor(dd$treat))[1],"time"]<tau)))
      e1<-sum(((dd[dd$treat==levels(factor(dd$treat))[2],"event"]==1)&(dd[dd$treat==levels(factor(dd$treat))[2],"time"]<tau)))
      test<-BinomDiffCI(n1-e1, n1, n0-e0, n0, conf.level = (1-sig.level*2), method = "score")
      CI <- test[2:3]
      estimate<-test[1]
    } else if (test.type=="Cox.PH.bootstrap") {
      res<-boot(dd, surv.diff, tau=tau, R=M.boot)
      CI<-boot.ci(res, index=1, type=bootCI.type, conf=1-sig.level*2)[[4]][4:5-2*(bootCI.type=="norm")]
      estimate<-res$t0     
    } else if (test.type=="flexsurv.PH.delta") {
      fit.flexsurv<-flexsurvspline(Surv(time,event)~treat, k=2, data=dd)
      varcov<-vcov(fit.flexsurv)
      beta1<-coef(fit.flexsurv)[5]
      s0<-coef(fit.flexsurv)[1]
      s1<-coef(fit.flexsurv)[2]
      s2<-coef(fit.flexsurv)[3]
      s3<-coef(fit.flexsurv)[4]
      k1<-fit.flexsurv$knots[1]
      k2<-fit.flexsurv$knots[2]
      k3<-fit.flexsurv$knots[3]
      k4<-fit.flexsurv$knots[4]
      grads<-c(grad(Deriv1.DS,s0, s1=s1,s2=s2,s3=s3,beta1=beta1, k1=k1, k2=k2, k3=k3, k4=k4, tau=tau),
                grad(Deriv2.DS,s1, s0=s0,s2=s2,s3=s3,beta1=beta1, k1=k1, k2=k2, k3=k3, k4=k4, tau=tau),
                grad(Deriv3.DS,s2, s0=s0,s1=s1,s3=s3,beta1=beta1, k1=k1, k2=k2, k3=k3, k4=k4, tau=tau), 
                grad(Deriv4.DS,s3, s0=s0,s1=s1,s2=s2,beta1=beta1, k1=k1, k2=k2, k3=k3, k4=k4, tau=tau),
                grad(Deriv5.DS,beta1, s0=s0,s1=s1,s3=s3,s2=s2, k1=k1, k2=k2, k3=k3, k4=k4, tau=tau))
      estimate<-DS.est(beta1, s0, s1, s2, s3,k1,k2,k3,k4,tau)
      se<-sqrt(t(grads)%*%varcov%*%grads)
      low.bndb<-(estimate-qnorm(1-sig.level)*se)
      up.bndb<-(estimate+qnorm(1-sig.level)*se)
      CI<-c(low.bndb, up.bndb)
    } else if (test.type=="flexsurv.PH.bootstrap") {
      res<-boot(dd, Surv.diff.flexsurv, tau=tau, R=M.boot)
      CI<-boot.ci(res, index=1, type=bootCI.type, conf=1-sig.level*2)[[4]][4:5-2*(bootCI.type=="norm")]
      estimate<-as.numeric(res$t0)    
    } 
    if (is.null(se)) {
      se<-(CI[2]-CI[1])/(2*qnorm(1-sig.level))
      is.se.est<-T
    } else {
      is.se.est<-F
    }
    if (is.null(p)) {
      Z <- ifelse( unfavourable==T, -(estimate - NI.margin)/se, (estimate - NI.margin)/se)
      p <- pnorm(Z)
      is.p.est<-T
    } else {
      is.p.est<-F
    }
    if ((unfavourable==T&&CI[1]>NI.margin)||(unfavourable==F&&CI[2]<NI.margin)) { 
      non.inferiority<-T
    } else {
      non.inferiority<-F
    }
    if (print.out==T) {
      cat("Testing for non-inferiority.\nSummary measure: Difference in Survival.\nNon-inferiority margin = ", NI.margin, ".\nMethod: ",test.type,
          ".\nEstimate = ", estimate, 
          "\nConfidence interval (Two-sided ", (1-sig.level*2)*100,"%): (", CI[1], ",", CI[2], 
          ")\np-value = ", p, ".\n" , sep="")
      if ((unfavourable==T&&CI[1]>NI.margin)||(unfavourable==F&&CI[2]<NI.margin)) { 
        cat("The confidence interval does not cross the null ( DS = ", NI.margin, " ), and hence we have evidence of non-inferiority.\n", sep="")
      } else {
        cat("The confidence interval crosses the null ( DS = ", NI.margin, " ), and hence we have NO clear evidence of non-inferiority.\n", sep="")
      }
      if (is.p.est==T) {
        if (is.se.est==T) {
          cat("Note: with the test = ",test.type, " for summary measure = ", summary.measure,", p-value and standard error are only approximations based on a Z test with given estimate and CI.\n")
        } else {
          cat("Note: with the test = ",test.type, " for summary measure = ", summary.measure,", p-value is only an approximation based on a Z test with given estimate and CI.\n")
        }
      }
      
    }
  } 
  
  results <- list(estimate, se, p, CI, test.type, summary.measure, is.p.est, sig.level, non.inferiority)
  names(results)<-c("estimate", "se", "p", "CI", "test.type", "summary.measure", "is.p.est", "sig.level", "non.inferiority")
  return(results)
}