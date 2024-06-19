test.NI.survival <- function(time, event, treat, covariates=NULL, NI.margin, sig.level=0.025, summary.measure="HR", 
                             print.out=TRUE, unfavourable=TRUE, test.type=NULL,
                             M.boot=2000, bootCI.type="bca", tau=NULL, control.level=NULL,
                             k=2, knots=NULL, bknots=NULL) {
  
  stopifnot(is.numeric(time), is.vector(time), length(time)>2, all(time>0))
  stopifnot(is.vector(event), nlevels(as.factor(event))%in%c(1,2), length(time)==length(event))
  stopifnot(is.vector(treat), nlevels(as.factor(treat))==2, length(time)==length(treat))
  treat<-as.factor(treat)
  if (!is.null(control.level)) {
    stopifnot(is.character(control.level)&(control.level%in%levels(treat)))
  } else {
    control.level<-levels(treat)[1]
    if (isTRUE(print.out)) cat("NOTE: control level for treatment variable was not specified. It was assumed to be: ", control.level, ".\n")
  }
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
  myformula<-paste("Surv(time, event)~treat", covariate.formula)
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
      stopifnot(test.type%in%c("KM", "Cox.PH.bootstrap", "flexsurv.nonPH.delta", "flexsurv.PH.delta", "flexsurv.PH.bootstrap"))
    } else {
      stopifnot(test.type%in%c( "Cox.PH.bootstrap", "flexsurv.nonPH.delta", "flexsurv.PH.delta", "flexsurv.PH.bootstrap"))
    }
  } else {
    if (!adjusted) {
      stopifnot(test.type%in%c("KM", "Cox.PH.bootstrap", "flexsurv.PH.delta", "flexsurv.PH.bootstrap", "Cox.weighted"))
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
      fit<-coxph(as.formula(myformula), mydata)
      estimate<-as.numeric(exp(fit$coefficients[paste("treat",levels(treat)[levels(treat)!=control.level], sep="")]))
      CI<-as.numeric(exp(confint(fit, level=1-2*sig.level)[paste("treat",levels(treat)[levels(treat)!=control.level], sep=""),]))
    } else if (test.type=="flexsurv.PH") {
      fit<-flexsurvspline(as.formula(myformula), k=k, knots=knots, bknots=bknots, data=mydata)
      estimate<-as.numeric(exp(fit$coefficients[paste("treat",levels(treat)[levels(treat)!=control.level], sep="")]))
      CI<-as.numeric(exp(confint(fit, level=1-2*sig.level)[paste("treat",levels(treat)[levels(treat)!=control.level], sep=""),]))
    } else if (test.type=="Cox.weighted") {
      fit<-coxphw(as.formula(myformula), data=mydata)
      estimate<-as.numeric(exp(fit$coefficients[paste("treat",levels(treat)[levels(treat)!=control.level], sep="")]))
      CI<-as.numeric(exp(confint(fit, level=1-2*sig.level)[paste("treat",levels(treat)[levels(treat)!=control.level], sep=""),]))
    }  
    
    if (is.null(se)) {
      se<-(log(CI[2])-log(CI[1]))/(2*qnorm(1-sig.level))
      is.se.est<-T
    } else {
      is.se.est<-F
    }
    if (is.null(p)) {
      Z <- ifelse(unfavourable==T,(log(estimate) - log(NI.margin))/se,(-log(estimate) + log(NI.margin))/se)
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
      fit.flexsurv<-flexsurvspline(as.formula(myformula), k=k, knots=knots, bknots=bknots, data=dd)
      varcov<-vcov(fit.flexsurv)
      n.par<-k+2
      betas<-coef(fit.flexsurv)[(n.par+1):length(coef(fit.flexsurv))]
      n.cov<-1+length(covariates)
      parameters<-c(tau,betas)
      for (indx in 1:n.par) {
        parameters<-c(parameters,coef(fit.flexsurv)[indx])
      }
      for (indx in 1:n.par) {
        parameters<-c(parameters,fit.flexsurv$knots[indx])
      }
      parameters<-c(parameters, rep(0,n.par-1), n.cov, n.par)
      grads.comp<-numDeriv:::grad(DRMST.estimator, parameters)
      gradsb<-NULL
      for (indx in 1:n.par) {
        gradsb<-c(gradsb, grads.comp[indx+1+n.cov])
      }
      gradsb<-c(gradsb, grads.comp[2:(n.cov+1)])
      estimate <- DRMST.estimator(parameters)
      se<-sqrt(t(gradsb)%*%varcov%*%gradsb)
      low.bndb<-(estimate-qnorm(1-sig.level)*se)
      up.bndb<-(estimate+qnorm(1-sig.level)*se)
      CI<-c(low.bndb, up.bndb)
    } else if (test.type=="flexsurv.nonPH.delta") {
      formula.nonPH<-paste(myformula,"+gamma1(treat)", sep="")
      if (k>0) {
        for (indx in 1:k) {
          formula.nonPH<-paste(formula.nonPH, "+gamma", indx+1, "(treat)", sep="")
        }
      }
      fit.flexsurv<-flexsurvspline(as.formula(formula.nonPH), k=k, knots=knots, bknots=bknots, data=dd)
      varcov<-vcov(fit.flexsurv)
      n.par<-k+2
      n.cov<-1+length(covariates)
      betas<-coef(fit.flexsurv)[(n.par+1):(n.par+n.cov)]
      parameters<-c(tau,betas)
      for (indx in 1:(n.par)) {
        parameters<-c(parameters,coef(fit.flexsurv)[indx])
      }
      for (indx in 1:(n.par)) {
        parameters<-c(parameters,fit.flexsurv$knots[indx])
      }
      for (indx in 1:(n.par-1)) {
        parameters<-c(parameters,coef(fit.flexsurv)[indx+n.par+n.cov])
      }   
      parameters<-c(parameters, n.cov, n.par)
      grads.comp<-numDeriv:::grad(DRMST.estimator, parameters)
      gradsb<-NULL
      for (indx in 1:n.par) {
        gradsb<-c(gradsb, grads.comp[indx+1+n.cov])
      }
      gradsb<-c(gradsb, grads.comp[2:(1+n.cov)])
      for (indx in 1:(n.par-1)) {
        gradsb<-c(gradsb, grads.comp[indx+n.par*2+1+n.cov])
      }
      estimate <- DRMST.estimator(parameters)
      se<-sqrt(t(gradsb)%*%varcov%*%gradsb)
      low.bndb<-(estimate-qnorm(1-sig.level)*se)
      up.bndb<-(estimate+qnorm(1-sig.level)*se)
      CI<-c(low.bndb, up.bndb)
    } else if (test.type=="flexsurv.PH.bootstrap") {
      res<-boot(dd, RMST.diff.flexsurv, tau=tau, k=k, knots=knots, bknots=bknots, R=M.boot)
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
    if (test.type=="KM") {
     
      fit.KM<-survfit(Surv(time,event)~treat, data=dd)
      sum.KM<-summary(fit.KM, t=tau)
      estimate<-sum.KM$surv[2]-sum.KM$surv[1]
      se<-sqrt(sum.KM$std.err[1]^2+sum.KM$std.err[2]^2)
      low.bndb<-(estimate-qnorm(1-sig.level)*se)
      up.bndb<-(estimate+qnorm(1-sig.level)*se)
      CI<-c(low.bndb, up.bndb)
      
      } else if (test.type=="Cox.PH.bootstrap") {
      res<-boot(dd, surv.diff, tau=tau, R=M.boot)
      CI<-boot.ci(res, index=1, type=bootCI.type, conf=1-sig.level*2)[[4]][4:5-2*(bootCI.type=="norm")]
      estimate<-res$t0     
    } else if (test.type=="flexsurv.PH.delta") {
      fit.flexsurv<-flexsurvspline(Surv(time,event)~treat, k=k, knots=knots, bknots=bknots, data=dd)
      varcov<-vcov(fit.flexsurv)
      n.par<-length(coef(fit.flexsurv))
      beta1<-coef(fit.flexsurv)[n.par]
      parameters<-c(tau,beta1)
      for (indx in 1:(n.par-1)) {
        parameters<-c(parameters,coef(fit.flexsurv)[indx])
      }
      for (indx in 1:(n.par-1)) {
        parameters<-c(parameters,fit.flexsurv$knots[indx])
      }
      grads.comp<-numDeriv:::grad(DS.estimator, parameters)
      gradsb<-NULL
      for (indx in 1:(n.par-1)) {
        gradsb<-c(gradsb, grads.comp[indx+2])
      }
      gradsb<-c(gradsb, grads.comp[2])
      estimate <- DS.estimator(parameters)
      se<-sqrt(t(gradsb)%*%varcov%*%gradsb)
      low.bndb<-(estimate-qnorm(1-sig.level)*se)
      up.bndb<-(estimate+qnorm(1-sig.level)*se)
      CI<-c(low.bndb, up.bndb)
    } else if (test.type=="flexsurv.PH.bootstrap") {
      res<-boot(dd, Surv.diff.flexsurv, tau=tau, k=k, knots=knots, bknots=bknots, R=M.boot)
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