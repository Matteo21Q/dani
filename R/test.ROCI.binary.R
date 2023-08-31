test.ROCI.binary <- function (formula=NULL, data=NULL, outcomes=NULL, treatment=NULL, NI.margin, minimisation=T,
                               se.method="bootstrap", treatment.levels=unique(treatment), summary.measure="RD", 
                               tr.model="FP2.fixed", M.boot=NULL, bootCI.type="bca", parallel="no", n.cpus=1, sig.level=0.025,
                               unfavourable=TRUE) {
  
  if ((is.null(formula)|is.null(data))&(is.null(outcomes)|is.null(treatment))) {
    stop("One of formula+data or outcomes+treatment necessary\n")
  }
  if ((!is.null(formula)&!is.null(data))&(!is.null(outcomes)|!is.null(treatment))) {
    message("Ignoring outcomes+treatment and using formula+data interface\n")
  }
  
  if (is.character(formula)) formula<-as.formula(formula)
  if (!is.null(data)) stopifnot(is.data.frame(data))
  if (is.null(treatment)) {
    treatment <- data[,all.vars(formula[[3]])]
  } 
  stopifnot(is.numeric(treatment))
  
  if (is.null(outcomes)) {
    outcomes <- data[,all.vars(formula[[2]])]
  } 
  stopifnot(is.numeric(outcomes), length(outcomes)==length(treatment), nlevels(factor(outcomes))==2)
  stopifnot(is.numeric(treatment.levels), length(treatment.levels)>2)
  stopifnot(is.logical(unfavourable), !is.na(unfavourable))
  stopifnot(is.numeric(NI.margin), (length(NI.margin)==1)||(length(NI.margin)==(length(treatment.levels)-(summary.measure!="target.risk"))))
  if (length(NI.margin)==1) NI.margin<-rep(NI.margin, length(treatment.levels)-(summary.measure!="target.risk"))
  if (summary.measure=="RD") {
    if ((unfavourable == T)&&any(NI.margin<=0)) stop("When outcome is unfavourable, risk difference NI margins need to all be positive.\n")
    if ((unfavourable == F)&&any(NI.margin>=0)) stop("When outcome is favourable, risk difference NI margins need to all be negative.\n")
    if (any(NI.margin>=1)) stop("NI margins cannot be greater than 1, i.e. 100 percentage points, or otherwise the test is meaningless.\n ")
    if (any(NI.margin<=-1)) stop("NI margins cannot be lower than -1, i.e. -100 percentage points, or otherwise the test is meaningless.\n ")
  } else if (summary.measure%in%c("OR","RR")) {
    if ((unfavourable == T)&&any(NI.margin<=1)) stop("When outcome is unfavourable, NI margins on the risk ratio or odds ratio scale need to all be >1.")
    if ((unfavourable == F)&&any(NI.margin>=1)) stop("When outcome is favourable, NI margins on the risk ratio or odds ratio scale need to all be <1.")
    if (any(NI.margin<=0)) stop("A risk/odds ratio margin must be >0.\n")
  } else if (summary.measure=="target.risk") {
    if (any(NI.margin>=1)||any(NI.margin<=0)) stop("Target risks as defined in NI.margin should be numbers in the interval (0,1)")
  }
  stopifnot(is.numeric(sig.level), sig.level < 0.5, sig.level > 0)
  stopifnot(is.character(summary.measure),(( summary.measure == "RD" ) || ( summary.measure == "RR" ) || ( summary.measure == "OR" ) || ( summary.measure == "target.risk" )))
  stopifnot(is.logical(minimisation), !is.na(minimisation))
  if (!is.null(M.boot)) stopifnot(is.numeric(M.boot), M.boot>1)
  stopifnot(is.character(se.method), se.method%in%c("bootstrap", "delta"))
  stopifnot(is.character(bootCI.type), bootCI.type%in%c("norm","perc","bca","basic"))
  stopifnot(is.character(tr.model), tr.model%in%c("FP1.fixed","FP2.fixed", "FP1.classic", "FP2.classic"))
  min.treat<-min(treatment)
  n.treat<-length(treatment.levels)
  max.treat<-max(treatment)
  x.treat<-seq(min.treat, max.treat, length.out = 100)
  reference<-ifelse(isTRUE(minimisation),max.treat,min.treat)
  ref.index<-which(treatment.levels==reference)
  experimental.arms<-treatment.levels[-ref.index]
  
  if (is.null(M.boot)) M.boot<-length(treatment)
  data.mfp <- data.frame(outcomes, treatment, tr.model)
  assign("data.mfp", data.mfp, envir = .GlobalEnv)
  myformula<-as.formula("outcomes~fp(treatment)")
  
  # Fit Fractional Polynomial regression model
  fp<-mfp::fp
  if (tr.model=="FP2.fixed") {
    myformula<-as.formula("outcomes~fp(treatment,df=4, select=1, alpha=1)")
    fit<-mfp(myformula, data.mfp, family="binomial")
  } else if (tr.model=="FP1.fixed") {
    myformula<-as.formula("outcomes~fp(treatment,df=2, select=1, alpha=1)")
    fit<-mfp(myformula, data.mfp, family="binomial")
  } else if (tr.model=="FP2.classic") {
    myformula<-as.formula("outcomes~fp(treatment,df=4)")
    fit<-mfp(myformula, data.mfp, family="binomial")
  } else if (tr.model=="FP1.classic") {
    myformula<-as.formula("outcomes~fp(treatment,df=2)")
    fit<-mfp(myformula, data.mfp, family="binomial")
  } 
  glm.fit<-glm(fit$fit$formula, data=data.mfp, family="binomial")
  
  if (se.method=="delta") {
    
    up.bounds.CI<-rep(NA,length(treatment.levels))
    low.bounds.CI<-rep(NA,length(treatment.levels))
    mean.est<-rep(NA,length(treatment.levels))
    
    for (j in 1:(length(treatment.levels))) {
      
      if (summary.measure=="RD") {
        
        if (isTRUE(minimisation)) {
          mef<-avg_comparisons(glm.fit, variables = list(treatment = c(treatment.levels[j], reference)), comparison=function(hi, lo) lo-hi, conf_level = (1-sig.level*2))
        } else {
          mef<-avg_comparisons(fit$fit, variables = list(treatment = c(treatment.levels[j], reference)), comparison=function(hi, lo) hi-lo, conf_level = (1-sig.level*2))
        }
        
        
      } else if (summary.measure=="RR") {
        
        if (isTRUE(minimisation)) {
          mef<-avg_comparisons(fit$fit, variables = list(treatment = c(treatment.levels[j], reference)), comparison=function(hi, lo) log(lo / hi), transform=exp, conf_level = (1-sig.level*2))
        } else {
          mef<-avg_comparisons(fit$fit, variables = list(treatment = c(treatment.levels[j], reference)), comparison=function(hi, lo) log(hi / lo), transform=exp, conf_level = (1-sig.level*2))
        }
        
      } else if (summary.measure=="target.risk") {
        
        mef<-avg_comparisons(fit$fit, variables = list(treatment = c(treatment.levels[j], 0.0001)), comparison=function(hi, lo) hi, conf_level = (1-sig.level*2))
        
        
      } else if (summary.measure=="OR") {
        
        
        if (isTRUE(minimisation)) {
          mef<-avg_comparisons(fit$fit, variables = list(treatment = c(treatment.levels[j], reference)), comparison=function(hi, lo) log((lo/(1-lo)) / (hi/(1-hi))), transform=exp, conf_level = (1-sig.level*2))
        } else {
          mef<-avg_comparisons(fit$fit, variables = list(treatment = c(treatment.levels[j], reference)), comparison=function(hi, lo) log((hi/(1-hi)) / (lo/(1-lo))), transform=exp, conf_level = (1-sig.level*2))
        }
        
      }
      up.bounds.CI[j]<-mef$conf.high
      low.bounds.CI[j]<-mef$conf.low
      mean.est[j]<-mef$estimate
      
    }
    
  } else if (se.method=="bootstrap") {
    
    # Function to bootstrap:
    
    min.treat.stack<-NULL
    find.min.treat<- function (data.mfp, indices) {
      # Select bootstrap sample:
      da <- data.mfp[indices,]
      
      # Fit Fractional Polynomial regression model
      if (da$tr.model[1]=="FP2.fixed") {
        myformula<-as.formula("outcomes~fp(treatment,df=4, select=1, alpha=1)")
        fit.i<-mfp(myformula, da, family="binomial")
      } else if (da$tr.model[1]=="FP1.fixed") {
        myformula<-as.formula("outcomes~fp(treatment,df=2, select=1, alpha=1)")
        fit.i<-mfp(myformula, da, family="binomial")
      } else if (da$tr.model[1]=="FP2.classic") {
        myformula<-as.formula("outcomes~fp(treatment,df=4)")
        fit.i<-mfp(myformula, da, family="binomial")
      } else if (da$tr.model[1]=="FP1.classic") {
        myformula<-as.formula("outcomes~fp(treatment,df=2)")
        fit.i<-mfp(myformula, da, family="binomial")
      }  
      
      # Predict treat-response curve and associated pointwise CI
      if ((summary.measure=="RD")) {
        invisible(capture.output(y.treatment<-predict(fit.i, newdata=data.frame(treatment=experimental.arms), type="resp")-predict(fit.i, newdata=data.frame(treatment=reference), type="resp")))
      } else if (summary.measure=="RR") {
        invisible(capture.output(y.treatment<-predict(fit.i, newdata=data.frame(treatment=experimental.arms), type="resp")/predict(fit.i, newdata=data.frame(treatment=reference), type="resp")))
      } else if (summary.measure=="target.risk") {
        invisible(capture.output(y.treatment<-predict(fit.i, newdata=data.frame(treatment=treatment.levels), type="resp")))
      } else if (summary.measure=="OR") {
        invisible(capture.output(y.treatment<-exp(predict(fit.i, newdata=data.frame(treatment=experimental.arms), type="link")-predict(fit.i, newdata=data.frame(treatment=reference), type="link"))))
      } 
      
      
      output<-c(y.treatment)
      return(output)
    }
    
    boot.res<-boot(data.mfp,find.min.treat,M.boot, parallel = parallel, ncpus=n.cpus)
    up.bounds.CI<-NULL
    low.bounds.CI<-NULL
    for (indx in 1:(length(boot.res$t0))) {
      res.ci2<-boot.ci(boot.res, conf=1-sig.level*2, type=bootCI.type, index=indx)
      up.bounds.CI<-c(up.bounds.CI, res.ci2[[4]][5-2*(bootCI.type=="norm")])
      low.bounds.CI<-c(low.bounds.CI, res.ci2[[4]][4-2*(bootCI.type=="norm")])
      
    }
    if (summary.measure=="target.risk") {
      res.ci2<-boot.ci(boot.res, conf=1-sig.level*2, type=bootCI.type, index=length(boot.res$t0))
      up.bounds.CI<-c(up.bounds.CI, res.ci2[[4]][5-2*(bootCI.type=="norm")])
      low.bounds.CI<-c(low.bounds.CI, res.ci2[[4]][4-2*(bootCI.type=="norm")])
    } else if (summary.measure=="RR"||summary.measure=="OR") {
      if (isTRUE(minimisation)) {
        up.bounds.CI<-c(up.bounds.CI, 1)
        low.bounds.CI<-c(low.bounds.CI, 1)
      } else {
        up.bounds.CI<-c(1,up.bounds.CI)
        low.bounds.CI<-c(1, low.bounds.CI)
      }
    } else {
      if (isTRUE(minimisation)) {
        up.bounds.CI<-c(up.bounds.CI, 0)
        low.bounds.CI<-c(low.bounds.CI, 0)
      } else {
        up.bounds.CI<-c(0,up.bounds.CI)
        low.bounds.CI<-c(0, low.bounds.CI)
      }      
    }
    
    
    mean.est<-boot.res$t0
  }
  
  # What is point where predicted lower CI first crosses acceptability curve?
  flag=t=1
  optimal.treat<-reference
  if (isTRUE(minimisation)) {
    while ((t<(length(treatment.levels)+(summary.measure=="target.risk")))&(flag==1)) {
      if (((isTRUE(unfavourable))&(up.bounds.CI[t]-NI.margin[t])<0)||((unfavourable==F)&(low.bounds.CI[t]-NI.margin[t])>0)) {
        flag=0
        optimal.treat<-treatment.levels[t]
      }
      t=t+1
    }
    
  } else {
    t<-length(treatment.levels)
    while ((t>(1-(summary.measure=="target.risk")))&(flag==1)) {
      if (((isTRUE(unfavourable))&(up.bounds.CI[t]-NI.margin[t-(summary.measure!="target.risk")])<0)||((unfavourable==F)&(low.bounds.CI[t]-NI.margin[t-(summary.measure!="target.risk")])>0)) {
        flag=0
        optimal.treat<-treatment.levels[t]
      }
      t=t-1
    }
    
  }
  if (se.method!="bootstrap") boot.res<-NULL
  
  results<-list( model.fit = fit, optimal.treat = optimal.treat, 
                 up.bounds.CI = up.bounds.CI, boot.res=boot.res,
                 low.bounds.CI = low.bounds.CI, treatment.levels = treatment.levels,
                 NI.margin = NI.margin, se.method = se.method, 
                 summary.measure = summary.measure, family="binomial",
                 reference=reference, estimates=mean.est)
  class(results)<-c("ROCI", "list")
  return(results)
  
}
