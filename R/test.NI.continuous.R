test.NI.continuous <- function(y.control=NULL, y.experim=NULL,  NI.margin, sig.level=0.025, 
                               summary.measure="mean.difference", 
                               formula=NULL, data=NULL, control.level=0,
                               print.out=TRUE, higher.better=TRUE, test.type=NULL,
                               M.boot=2000, bootCI.type="bca", sd.control=NULL, 
                               sd.experim=NULL) {
  
  covariates<-NULL
  if (any(is.null(y.control), is.null(y.experim))&&any(is.null(data), is.null(formula))) {
    stop("Either y.control and y.experim or formula+data must be provided.\n")
  }
  if (!is.null(formula)) {
    if (is.character(formula)) formula<-as.formula(formula)
    stopifnot(is.data.frame(data), inherits(formula,"formula"))
    if (is_tibble(data)) data<-as.data.frame(data)
    terms.form <- attr(terms(formula), "term.labels")
    treat.index <- which(grepl("treat\\(", terms.form))
    if (length(treat.index)==0) stop("Treatment variable in the formula must be provided within brackets and preceded by treat, e.g. treat(treatment).\n")
    treatment <- factor(data[,all.vars(formula[[3]])[treat.index]])
    stopifnot(any(treatment==control.level), nlevels(treatment)==2)
    treatment<-relevel(treatment, ref=as.character(control.level))
    covariates <- terms.form[-treat.index]
    outcomes <- data[,all.vars(formula[[2]])]
    y.control<-outcomes[treatment==control.level]
    y.experim<-outcomes[treatment!=control.level]
    covariate.formula<-NULL
    if (length(covariates)!=0) {
      covariate.formula<-"+"
      for (cc in 1:length(covariates)) {
        covariate.formula <- paste(covariate.formula, covariates[cc])
        if (cc!=length(covariates)) covariate.formula<-paste(covariate.formula, "+")
      }
    }
    myformula<-as.formula(paste("outcomes~treatment", covariate.formula))
    mydata <- data.frame(outcomes, treatment, data[,covariates])
    if (length(covariates)>0) colnames(mydata)[3:ncol(mydata)]<-covariates
    assign("mydata", mydata, envir = .GlobalEnv)
    
  } 
  
  stopifnot(is.numeric(y.control), is.vector(y.control), length(y.control)>1)
  stopifnot(is.vector(y.experim), is.numeric(y.experim), length(y.experim)>1)
  stopifnot(is.numeric(NI.margin))
  stopifnot(is.numeric(sig.level), sig.level < 0.5, sig.level > 0)
  stopifnot(is.character(summary.measure),(( summary.measure == "mean.difference" ) || ( summary.measure == "mean.ratio" )))
  stopifnot(is.logical(print.out), !is.na(print.out))
  stopifnot(is.logical(higher.better), !is.na(higher.better))
  stopifnot(is.numeric(M.boot), M.boot>1)
  stopifnot(is.character(bootCI.type), bootCI.type%in%c("norm","perc","bca","basic"))
  
  if (is.null(test.type)) {
    if (summary.measure=="mean.difference") {
      test.type<-"t.test"
    } else {
      test.type<-"Fiellers"
    } 
  }
  stopifnot(is.character(test.type))
  adjusted<-(!is.null(covariates)&&any(dim(covariates)>0))
  if (is.null(formula)) {
    outcomes<-c(y.experim, y.control)
    treatment<-factor(c(rep(1,length(y.experim)), rep(0, length(y.control))))
    mydata<-data.frame(outcomes,treatment)
    myformula<-as.formula("outcomes~treatment")
  }
  
  if (!adjusted) {
    if (summary.measure=="mean.difference") {
      stopifnot(test.type%in%c("Z.test", "t.test", "bootstrap", "lm"))
    } else {
      stopifnot(test.type%in%c("Fiellers", "lm", "bootstrap"))
    } 
  } else {
    if (summary.measure=="mean.difference") {
      stopifnot(test.type%in%c("lm", "bootstrap"))
    } else {
      stopifnot(test.type%in%c("lm", "bootstrap"))
    } 
  }

  if (test.type=="Z.test") {
    stopifnot(is.numeric(sd.control), sd.control>0)
    stopifnot(is.numeric(sd.experim), sd.experim>0)
  }
  
  estimate<-se<-Z<-p<-NULL

  if (summary.measure=="mean.difference") {
    if ((higher.better == T)&&(NI.margin>=0)) stop("When higher values of the outcome are better, a mean difference NI margin needs to be negative.\n")
    if ((higher.better == F)&&(NI.margin<=0)) stop("When lower values of the outcome are better, a mean difference NI margin needs to be positive.\n")

    if (test.type=="Z.test") {
      test.CI<-z.test(y.experim, y.control,"two.sided",NI.margin,sd.control, sd.experim, 1-2*sig.level)
      CI<-as.numeric(test.CI$conf.int)
      estimate<-as.numeric(-diff(test.CI$estimate))
      se<-(CI[2]-estimate)/qnorm(1-sig.level)
      direction<-ifelse(higher.better==F, "less", "greater")
      test<-z.test(y.experim, y.control,direction,NI.margin,sd.control, sd.experim, 1-sig.level)
      p <- test$p.value
    } else if (test.type=="t.test") {
      test.CI<-t.test(y.experim, y.control,"two.sided",NI.margin,conf.level=1-2*sig.level)
      CI<-as.numeric(test.CI$conf.int)
      estimate<-as.numeric(-diff(test.CI$estimate))
      se<-(CI[2]-estimate)/qnorm(1-sig.level)
      direction<-ifelse(higher.better==F, "less", "greater")
      test<-t.test(y.experim, y.control,direction,NI.margin,conf.level=1-sig.level)
      p <- test$p.value
    } else if (test.type=="bootstrap") {
      dif <- function(dat, indices) {
        d <- dat[indices,] # allows boot to select sample
        mdif <- mean(d[d$treatment!=control.level,"outcomes"]) - mean(d[d$treatment==control.level,"outcomes"])
        return(mdif)
      } 
      res.b<-boot(mydata, dif, R=M.boot)
      CI<-boot.ci(res.b, type=bootCI.type, conf=1-sig.level*2)[[4]][4:5-2*(bootCI.type=="norm")]
      estimate<-res.b$t0
    } else if (test.type=="lm") {
      fit<-lm(myformula, mydata)
      fit.std<-avg_comparisons(fit, variables="treatment")

      CI <- c(fit.std[fit.std$term=="treatment","conf.low"], fit.std[fit.std$term=="treatment","conf.high"])
      estimate<-fit.std[fit.std$term=="treatment","estimate"]
      p<-fit.std[fit.std$term=="treatment","p.value"]
      
    } 
    estimate.n<-mean(CI)
    if (is.null(se)) {
      se<-(CI[2]-CI[1])/(2*qnorm(1-sig.level))
      is.se.est<-T
    } else {
      is.se.est<-F
    }
    if (is.null(p)) {
      Z <- ifelse(higher.better==F,(estimate.n - NI.margin)/se,(-estimate.n + NI.margin)/se)
      p <- pnorm(Z)
      is.p.est<-T
    } else {
      is.p.est<-F
    }
    if ((higher.better==F&&CI[2]<NI.margin)||(higher.better==T&&CI[1]>NI.margin)) { 
      non.inferiority<-T
    } else {
      non.inferiority<-F
    }
    if (print.out==T) {
      cat("Testing for non-inferiority.\nSummary measure: Mean difference.\nNon-inferiority margin = ", NI.margin, ".\nMethod: ",test.type,
          ".\nEstimate = ", estimate, 
          "\nConfidence interval (Two-sided ", (1-sig.level*2)*100,"%): (", CI[1], ",", CI[2], 
          ")\np-value = ", p, ".\n" , sep="")
      if ((higher.better==F&&CI[2]<NI.margin)||(higher.better==T&&CI[1]>NI.margin)) { 
        cat("The confidence interval does not cross the null ( Mean Difference = ", NI.margin, " ), and hence we have evidence of non-inferiority.\n", sep="")
      } else {
        cat("The confidence interval crosses the null ( Mean DIfference = ", NI.margin, " ), and hence we have NO evidence of non-inferiority.\n", sep="")
      }
      if (is.p.est==T) {
        if (is.se.est==T) {
          cat("Note: with the test = ",test.type, " for summary measure = ", summary.measure,", p-value and standard error are only approximations based on a Z test with given logarithm of estimate and CI.\n")
        } else {
          cat("Note: with the test = ",test.type, " for summary measure = ", summary.measure,", p-value is only an approximation based on a Z test with given logarithm of estimate and CI.\n")
        }
      }
      
    }
    
  } else if (summary.measure == "mean.ratio") {
    if ((higher.better == T)&&(NI.margin>=1)) stop("When outcome is such that higher values are better, a NI margin on the mean ratio scale needs to be <1.")
    if ((higher.better == F)&&(NI.margin<=1)) stop("When outcome is such that higher values are worse, a NI margin on the mean ratio scale needs to be >1.")
    if (NI.margin<=0) stop("A mean ratio margin must be >0.\n")
    if (test.type=="Fiellers") {
      test.CI<-ttestratio(y.experim, y.control,"two.sided",NI.margin,conf.level=1-2*sig.level)
      CI<-as.numeric(test.CI$conf.int)
      estimate<-as.numeric(test.CI$estimate)[3]
      se<-(CI[2]-estimate)/qnorm(1-sig.level)
      direction<-ifelse(higher.better==F, "less", "greater")
      test<-ttestratio(y.experim, y.control,direction,NI.margin,conf.level=1-sig.level)
      p <- test$p.value
    } else if (test.type=="lm") {
      fit<-lm(myformula, mydata)
      mf<-avg_comparisons(fit, variables="treatment", comparison="lnratioavg", transform = exp)
      estimate<-mf$estimate    
      p <- mf$p.value
      CI <- c(mf$conf.low, mf$conf.high)
    } else if (test.type=="bootstrap") {
      rat <- function(dat, indices) {
        d <- dat[indices,] # allows boot to select sample
        mrat <- log(mean(d[d$treatment!=control.level,"outcomes"])/mean(d[d$treatment==control.level,"outcomes"]))
        return(mrat)
      } 
      res.b<-boot(mydata, rat, R=M.boot)
      CI<-exp(boot.ci(res.b,  type=bootCI.type, conf=1-sig.level*2)[[4]][4:5-2*(bootCI.type=="norm")])
      estimate<-exp(res.b$t0)
    } 
    if (is.null(se)) {
      se<-(log(CI[2])-log(CI[1]))/(2*qnorm(1-sig.level))
      is.se.est<-T
    } else {
      is.se.est<-F
    }
    if (is.null(p)) {
      Z <- ifelse( higher.better==T, (estimate - NI.margin)/se, -(estimate - NI.margin)/se)
      p <- pnorm(Z)
      is.p.est<-T
    } else {
      is.p.est<-F
    }
    if ((higher.better==T&&CI[1]>NI.margin)||(higher.better==F&&CI[2]<NI.margin)) { 
      non.inferiority<-T
    } else {
      non.inferiority<-F
    }
    if (print.out==T) {
      cat("Testing for non-inferiority.\nSummary measure: Ratio of means.\nNon-inferiority margin = ", NI.margin, ".\nMethod: ",test.type,
          ".\nEstimate = ", estimate, 
          "\nConfidence interval (Two-sided ", (1-sig.level*2)*100,"%): (", CI[1], ",", CI[2], 
          ")\np-value = ", p, ".\n" , sep="")
      if ((higher.better==T&&CI[1]>NI.margin)||(higher.better==F&&CI[2]<NI.margin)) { 
        cat("The confidence interval does not cross the null ( Mean Ratio = ", NI.margin, " ), and hence we have evidence of non-inferiority.\n", sep="")
      } else {
        cat("The confidence interval crosses the null ( Mean Ratio = ", NI.margin, " ), and hence we have NO clear evidence of non-inferiority.\n", sep="")
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