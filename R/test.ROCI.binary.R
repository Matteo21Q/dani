test.ROCI.binary <- function (formula=NULL, data=NULL, NI.margin, reference=max(treatment.levels),
                               se.method="bootstrap", treatment.levels=unique(treatment), summary.measure="RD", 
                               tr.model="FP2.fixed", M.boot=NULL, bootCI.type="bca", parallel="no", n.cpus=1, sig.level=0.025,
                               unfavourable=TRUE) {

  stopifnot(!is.null(formula), !is.null(data))
  
  if (is.character(formula)) formula<-as.formula(formula)
  stopifnot(is.data.frame(data))
  if (is_tibble(data)) data<-as.data.frame(data)
  terms.form <- attr(terms(formula), "term.labels")
  treat.index <- which(grepl("treat\\(", terms.form))
  if (length(treat.index)==0) stop("Treatment variable in the formula must be provided within brackets and preceded by treat, e.g. treat(treatment).\n")
  treatment <- data[,all.vars(formula[[3]])[treat.index]]
  covariates <- terms.form[-treat.index]
  outcomes <- data[,all.vars(formula[[2]])]
  
  stopifnot(is.numeric(treatment))
  stopifnot(is.numeric(outcomes), length(outcomes)==length(treatment), nlevels(factor(outcomes))==2)
  stopifnot(is.numeric(treatment.levels), length(treatment.levels)>2)
  stopifnot(is.numeric(reference), length(reference)==1, reference%in%treatment.levels)
  stopifnot(is.logical(unfavourable), !is.na(unfavourable))
  stopifnot(is.character(summary.measure),(( summary.measure == "RD" ) || ( summary.measure == "AS" )|| ( summary.measure == "RR" ) || ( summary.measure == "OR" ) || ( summary.measure == "target.risk" )))
  stopifnot(is.numeric(NI.margin), (length(NI.margin)==1)||(length(NI.margin)==(length(treatment.levels)-(summary.measure!="target.risk"))))
  if (length(NI.margin)==1) NI.margin<-rep(NI.margin, length(treatment.levels)-(summary.measure!="target.risk"))
  if (summary.measure%in%c("AS","RD")) {
    if ((unfavourable == T)&&any(NI.margin<=0)) stop("When outcome is unfavourable, risk difference or arc-sine difference NI margins need to all be positive.\n")
    if ((unfavourable == F)&&any(NI.margin>=0)) stop("When outcome is favourable, risk difference or arc-sine difference NI margins need to all be negative.\n")
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
  if (!is.null(M.boot)) stopifnot(is.numeric(M.boot), M.boot>1)
  stopifnot(is.character(se.method), se.method%in%c("bootstrap", "delta"))
  stopifnot(is.character(bootCI.type), bootCI.type%in%c("norm","perc","bca","basic"))
  stopifnot(is.character(tr.model), tr.model%in%c("FP1.fixed","FP2.fixed", "FP1.classic", "FP2.classic"))
  min.treat<-min(treatment)
  n.treat<-length(treatment.levels)
  max.treat<-max(treatment)
  x.treat<-seq(min.treat, max.treat, length.out = 100)
  ref.index<-which(treatment.levels==reference)
  if (summary.measure=="target.risk") {
    experimental.arms<- treatment.levels
  } else {
    experimental.arms<- treatment.levels[-ref.index]
  }
  
  if (is.null(M.boot)) M.boot<-length(treatment)
  data.mfp <- data.frame(outcomes, treatment, tr.model, data[,covariates])
  if (length(covariates)>0) colnames(data.mfp)[4:ncol(data.mfp)]<-covariates
  assign("data.mfp", data.mfp, envir = .GlobalEnv)
  covariate.formula<-NULL
  if (length(covariates)!=0) {
    covariate.formula<-"+"
    for (cc in 1:length(covariates)) {
      covariate.formula <- paste(covariate.formula, covariates[cc])
      if (cc!=length(covariates)) covariate.formula<-paste(covariate.formula, "+")
    }
  }
  
  # Fit Fractional Polynomial regression model
  fp<-mfp::fp
  if (tr.model=="FP2.fixed") {
    myformula<-as.formula(paste("outcomes~fp(treatment,df=4, select=1, alpha=1)", covariate.formula))
    fit<-mfp(myformula, data.mfp, family="binomial")
  } else if (tr.model=="FP1.fixed") {
    myformula<-as.formula(paste("outcomes~fp(treatment,df=2, select=1, alpha=1)", covariate.formula))
    fit<-mfp(myformula, data.mfp, family="binomial")
  } else if (tr.model=="FP2.classic") {
    myformula<-as.formula(paste("outcomes~fp(treatment,df=4)", covariate.formula))
    fit<-mfp(myformula, data.mfp, family="binomial")
  } else if (tr.model=="FP1.classic") {
    myformula<-as.formula(paste("outcomes~fp(treatment,df=2)", covariate.formula))
    fit<-mfp(myformula, data.mfp, family="binomial")
  } 
  glm.fit<-glm(fit$fit$formula, data=data.mfp, family="binomial")
  
  if (se.method=="delta") {
    
    up.bounds.CI<-rep(NA,length(experimental.arms))
    low.bounds.CI<-rep(NA,length(experimental.arms))
    mean.est<-rep(NA,length(experimental.arms))
    
    for (j in 1:(length(experimental.arms))) {
      
      if (summary.measure=="RD") {
        
        if (experimental.arms[j]<reference) {
          mef<-avg_comparisons(glm.fit, variables = list(treatment = c(experimental.arms[j], reference)), comparison=function(hi, lo) lo-hi, conf_level = (1-sig.level*2))
        } else {
          mef<-avg_comparisons(glm.fit, variables = list(treatment = c(experimental.arms[j], reference)), comparison=function(hi, lo) hi-lo, conf_level = (1-sig.level*2))
        }
        
        
      } else if (summary.measure=="RR") {
        
        if (experimental.arms[j]<reference) {
          mef<-avg_comparisons(glm.fit, variables = list(treatment = c(experimental.arms[j], reference)), comparison=function(hi, lo) log(lo / hi), transform=exp, conf_level = (1-sig.level*2))
        } else {
          mef<-avg_comparisons(glm.fit, variables = list(treatment = c(experimental.arms[j], reference)), comparison=function(hi, lo) log(hi / lo), transform=exp, conf_level = (1-sig.level*2))
        }
        
      } else if (summary.measure=="AS") {
        
        if (experimental.arms[j]<reference) {
          mef<-avg_comparisons(glm.fit, variables = list(treatment = c(experimental.arms[j], reference)), comparison=function(hi, lo) asin(sqrt(lo))-asin(sqrt(hi)), conf_level = (1-sig.level*2))
        } else {
          mef<-avg_comparisons(glm.fit, variables = list(treatment = c(experimental.arms[j], reference)), comparison=function(hi, lo) asin(sqrt(hi))-asin(sqrt(lo)), conf_level = (1-sig.level*2))
        }
        
      } else if (summary.measure=="target.risk") {
        
        mef<-avg_comparisons(glm.fit, variables = list(treatment = c(treatment.levels[j], -999999)), comparison=function(hi, lo) hi, conf_level = (1-sig.level*2))
        
        
      } else if (summary.measure=="OR") {
        
        
        if (experimental.arms[j]<reference) {
          mef<-avg_comparisons(fit$fit, variables = list(treatment = c(experimental.arms[j], reference)), comparison=function(hi, lo) log((lo/(1-lo)) / (hi/(1-hi))), transform=exp, conf_level = (1-sig.level*2))
        } else {
          mef<-avg_comparisons(fit$fit, variables = list(treatment = c(experimental.arms[j], reference)), comparison=function(hi, lo) log((hi/(1-hi)) / (lo/(1-lo))), transform=exp, conf_level = (1-sig.level*2))
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
      assign("da", data.mfp[indices,], envir = .GlobalEnv)
      
      # Fit Fractional Polynomial regression model
      if (da$tr.model[1]=="FP2.fixed") {
        myformula<-as.formula(paste("outcomes~fp(treatment,df=4, select=1, alpha=1)", covariate.formula))
        fit.i<-mfp(myformula, da, family="binomial")
      } else if (da$tr.model[1]=="FP1.fixed") {
        myformula<-as.formula(paste("outcomes~fp(treatment,df=2, select=1, alpha=1)", covariate.formula))
        fit.i<-mfp(myformula, da, family="binomial")
      } else if (da$tr.model[1]=="FP2.classic") {
        myformula<-as.formula(paste("outcomes~fp(treatment,df=4)", covariate.formula))
        fit.i<-mfp(myformula, da, family="binomial")
      } else if (da$tr.model[1]=="FP1.classic") {
        myformula<-as.formula(paste("outcomes~fp(treatment,df=2)", covariate.formula))
        fit.i<-mfp(myformula, da, family="binomial")
      }  
      
      # Predict treat-response curve 
      
      mean.est<-rep(NA,length(experimental.arms))
      
      for (j in 1:(length(experimental.arms))) {
        
          if (summary.measure=="RD") {
            
            if (experimental.arms[j]<reference) {
              mef<-avg_comparisons(fit.i, vcov=FALSE, variables = list(treatment = c(experimental.arms[j], reference)), comparison=function(hi, lo) lo-hi, conf_level = (1-sig.level*2))
            } else {
              mef<-avg_comparisons(fit.i, vcov=FALSE, variables = list(treatment = c(experimental.arms[j], reference)), comparison=function(hi, lo) hi-lo, conf_level = (1-sig.level*2))
            }
            
            
          } else if (summary.measure=="RR") {
            
            if (experimental.arms[j]<reference) {
              mef<-avg_comparisons(fit.i, vcov=FALSE, variables = list(treatment = c(experimental.arms[j], reference)), comparison=function(hi, lo) log(lo / hi), transform=exp, conf_level = (1-sig.level*2))
            } else {
              mef<-avg_comparisons(fit.i, vcov=FALSE, variables = list(treatment = c(experimental.arms[j], reference)), comparison=function(hi, lo) log(hi / lo), transform=exp, conf_level = (1-sig.level*2))
            }
            
          }  else if (summary.measure=="AS") {
            
            if (experimental.arms[j]<reference) {
              mef<-avg_comparisons(glm.fit, vcov=FALSE, variables = list(treatment = c(experimental.arms[j], reference)), comparison=function(hi, lo) asin(sqrt(lo))-asin(sqrt(hi)), conf_level = (1-sig.level*2))
            } else {
              mef<-avg_comparisons(glm.fit, vcov=FALSE, variables = list(treatment = c(experimental.arms[j], reference)), comparison=function(hi, lo) asin(sqrt(hi))-asin(sqrt(lo)), conf_level = (1-sig.level*2))
            }
            
          } else if (summary.measure=="target.risk") {
            
            mef<-avg_predictions(model=fit.i, by=F, vcov=FALSE, variables = list(treatment = c(treatment.levels[j])), conf_level = (1-sig.level*2))
            
            
          } else if (summary.measure=="OR") {
            
            
            if (experimental.arms[j]<reference) {
              mef<-avg_comparisons(fit.i, vcov=FALSE, variables = list(treatment = c(experimental.arms[j], reference)), comparison=function(hi, lo) log((lo/(1-lo)) / (hi/(1-hi))), transform=exp, conf_level = (1-sig.level*2))
            } else {
              mef<-avg_comparisons(fit.i, vcov=FALSE, variables = list(treatment = c(experimental.arms[j], reference)), comparison=function(hi, lo) log((hi/(1-hi)) / (lo/(1-lo))), transform=exp, conf_level = (1-sig.level*2))
            }
            
          }
        mean.est[j]<-mef$estimate
      }
      
      
      output<-c(mean.est)
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
    mean.est<-boot.res$t0
  }
  
  # What is point furthest from reference that implies non-inferiority
  optimal.treat<-reference
  if (isTRUE(unfavourable)) {
    acc.index<-((up.bounds.CI-NI.margin)<0)
  } else {
    acc.index<-((low.bounds.CI-NI.margin)>0)
  }
  
  acc.vec<-experimental.arms[acc.index]
  optimal.treat <- ifelse (length(acc.vec)>0, 
                           acc.vec[which.max(abs(acc.vec-reference))],
                           reference)  

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
