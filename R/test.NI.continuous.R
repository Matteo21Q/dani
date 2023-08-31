test.NI.continuous <- function(y.control, y.experim,  NI.margin, sig.level=0.025, summary.measure="mean.difference", 
                           print.out=TRUE, higher.better=TRUE, test.type=NULL,
                           M.boot=2000, sd.control=NULL, sd.experim=NULL) {
  
  stopifnot(is.numeric(y.control), is.vector(y.control), length(y.control)>1)
  stopifnot(is.vector(y.experim), is.numeric(y.experim), length(y.experim)>1)
  stopifnot(is.numeric(NI.margin))
  stopifnot(is.numeric(sig.level), sig.level < 0.5, sig.level > 0)
  stopifnot(is.character(summary.measure),(( summary.measure == "mean.difference" ) || ( summary.measure == "mean.ratio" )))
  stopifnot(is.logical(print.out), !is.na(print.out))
  stopifnot(is.logical(higher.better), !is.na(higher.better))
  stopifnot(is.numeric(M.boot), M.boot>1)
  if (is.null(test.type)) {
    if (summary.measure=="mean.difference") {
      test.type<-"t.test"
    } else {
      test.type<-"Fiellers"
    } 
  }
  stopifnot(is.character(test.type))
  if (summary.measure=="mean.difference") {
    stopifnot(test.type%in%c("Z.test", "t.test", "bootstrap.bca", "bootstrap.basic", "bootstrap.percentile"))
  } else {
    stopifnot(test.type%in%c("Fiellers", "lm", "bootstrap.bca", "bootstrap.basic", "bootstrap.percentile"))
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
    } else if (test.type=="bootstrap.bca") {
      test<-MeanDiffCI(y.experim, y.control, conf.level = (1-sig.level*2), method = "bca", R=M.boot)
      CI <- as.numeric(test[2:3])
      estimate<-as.numeric(test[1])
    } else if (test.type=="bootstrap.basic") {
      test<-MeanDiffCI(y.experim, y.control, conf.level = (1-sig.level*2), method = "basic", R=M.boot)
      CI <- as.numeric(test[2:3])
      estimate<-as.numeric(test[1])
    } else if (test.type=="bootstrap.percentile") {
      test<-MeanDiffCI(y.experim, y.control, conf.level = (1-sig.level*2), method = "perc", R=M.boot)
      CI <- as.numeric(test[2:3])
      estimate<-as.numeric(test[1])
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
    
    if (print.out==T) {
      cat("Testing for non-inferiority.\nSummary measure: Mean difference.\nNon-inferiority margin = ", NI.margin, ".\nMethod: ",test.type,
          ".\nEstimate = ", estimate, 
          "\nConfidence interval (Two-sided ", (1-sig.level*2)*100,"%): (", CI[1], ",", CI[2], 
          ")\np-value = ", p, ".\n" , sep="")
      if ((higher.better==F&&CI[2]<NI.margin)||(higher.better==T&&CI[1]>NI.margin)) { 
        cat("The confidence interval does not cross the null ( Mean Difference = ", NI.margin, " ), and hence we have evidence of non-inferiority.\n", sep="")
        non.inferiority<-T
      } else {
        cat("The confidence interval crosses the null ( Mean DIfference = ", NI.margin, " ), and hence we have NO evidence of non-inferiority.\n", sep="")
        non.inferiority<-F
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
    y.comb<-c(y.control, y.experim)
    treat<-c(rep(0, length(y.control)),rep(1, length(y.control)))
    dd<-data.frame(y.comb, treat)
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
      fit<-lm(y.comb~treat)
      mf<-avg_comparisons(fit, variables="treat", comparison="lnratioavg", transform = exp)
      estimate<-mf$estimate    
      p <- mf$p.value
      CI <- c(mf$conf.low, mf$conf.high)
    } else if (test.type=="bootstrap.percentile") {
      res.b<-boot(dd, ratios, R=M.boot)
      CI<-boot.ci(res.b, type="perc")$percent[4:5]
      estimate<-res.b$t0
    } else if (test.type=="bootstrap.basic") {
      res.b<-boot(dd, ratios, R=M.boot)
      CI<-boot.ci(res.b, type="basic")$basic[4:5]
      estimate<-res.b$t0
    } else if (test.type=="bootstrap.bca") {
      res.b<-boot(dd, ratios, R=M.boot)
      CI<-boot.ci(res.b, type="bca")$bca[4:5]
      estimate<-res.b$t0
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
    if (print.out==T) {
      cat("Testing for non-inferiority.\nSummary measure: Ratio of means.\nNon-inferiority margin = ", NI.margin, ".\nMethod: ",test.type,
          ".\nEstimate = ", estimate, 
          "\nConfidence interval (Two-sided ", (1-sig.level*2)*100,"%): (", CI[1], ",", CI[2], 
          ")\np-value = ", p, ".\n" , sep="")
      if ((higher.better==T&&CI[1]>NI.margin)||(higher.better==F&&CI[2]<NI.margin)) { 
        non.inferiority<-T
        cat("The confidence interval does not cross the null ( Mean Ratio = ", NI.margin, " ), and hence we have evidence of non-inferiority.\n", sep="")
      } else {
        non.inferiority<-F
        cat("The confidence interval crosses the null ( Mean Ratio = ", NI.margin, " ), and hence we have NO evidence of non-inferiority.\n", sep="")
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