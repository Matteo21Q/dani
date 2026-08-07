test.ROCI.survival <- function (formula=NULL, data=NULL, NI.margin, reference=max(treatment.levels),
                              se.method="bootstrap", treatment.levels=NULL, summary.measure="HR", 
                              tr.model="FP2.fixed", M.boot=NULL, bootCI.type="bca", parallel="no", n.cpus=1, cl=NULL, sig.level=0.025,
                              unfavourable=TRUE, k=2, knots=NULL, bknots=NULL, tau=NULL, list.ss=NULL) {
  
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
  stopifnot(is.numeric(outcomes[,1]), nrow(outcomes)==length(treatment), nlevels(factor(outcomes[,2]))==2)
  if (is.null(treatment.levels))  treatment.levels=unique(treatment)
  stopifnot(is.numeric(treatment.levels), length(treatment.levels)>2)
  stopifnot(is.numeric(reference), length(reference)==1, reference%in%treatment.levels)
  stopifnot(is.logical(unfavourable), !is.na(unfavourable))
  stopifnot(is.character(summary.measure),(( summary.measure == "HR" ) || ( summary.measure == "DS" )|| ( summary.measure == "RS" )|| ( summary.measure == "DRMST" )))
  stopifnot(is.numeric(NI.margin), (length(NI.margin)==1)||(length(NI.margin)==(length(treatment.levels)-1)))
  if (length(NI.margin)==1) NI.margin<-rep(NI.margin, length(treatment.levels)-1)
  if (summary.measure%in%c("DS","DRMST")) {
    if ((unfavourable == T)&&any(NI.margin>=0)) stop("When outcome is unfavourable, difference in survival or RMST NI margins need to all be negative.\n")
    if ((unfavourable == F)&&any(NI.margin<=0)) stop("When outcome is favourable, difference in survival or RMST  NI margins need to all be positive.\n")
    if (summary.measure=="DS") {
      if (any(NI.margin>=1)) stop("NI margins cannot be greater than 1, i.e. 100 percentage points, or otherwise the test is meaningless.\n ")
      if (any(NI.margin<=-1)) stop("NI margins cannot be lower than -1, i.e. -100 percentage points, or otherwise the test is meaningless.\n ")
    }
  } else if (summary.measure%in%c("HR")) {
    if ((unfavourable == T)&&any(NI.margin<=1)) stop("When outcome is unfavourable, NI margins on the hazard ratio scale need to all be >1.")
    if ((unfavourable == F)&&any(NI.margin>=1)) stop("When outcome is favourable, NI margins on the hazard ratio scale need to all be <1.")
    if (any(NI.margin<=0)) stop("A hazard ratio margin must be >0.\n")
  } else if (summary.measure%in%c( "RS")) {
    if ((unfavourable == T)&&any(NI.margin>=1)) stop("When outcome is unfavourable, NI margins on the ratio of survival scale need to all be <1.")
    if ((unfavourable == F)&&any(NI.margin<=1)) stop("When outcome is favourable, NI margins on the ratio of survival scale need to all be >1.")
    if (any(NI.margin<=0)) stop("A ratio of survival margin must be >0.\n")
  } 
  stopifnot(is.numeric(sig.level), sig.level < 0.5, sig.level > 0)
  if (!is.null(M.boot)) stopifnot(is.numeric(M.boot), M.boot>1)
  stopifnot(is.character(se.method), se.method%in%c("bootstrap", "delta"))
  stopifnot(is.character(bootCI.type), bootCI.type%in%c("norm","perc","bca","basic"))
  stopifnot(is.character(tr.model), tr.model%in%c("FP1.fixed","FP2.fixed", "FP1.select", "FP2.select"))
  min.treat<-min(treatment)
  n.treat<-length(treatment.levels)
  max.treat<-max(treatment)
  x.treat<-seq(min.treat, max.treat, length.out = 100)
  ref.index<-which(treatment.levels==reference)
  experimental.arms<- treatment.levels[-ref.index]
  
  if (is.null(M.boot)) M.boot<-length(treatment)
  data.mfp <- data.frame(outcomes, treatment, tr.model, data[,covariates])
  if (length(covariates)>0) colnames(data.mfp)[5:ncol(data.mfp)]<-covariates
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
  if (is.null(tau)&summary.measure=="HR") tau<-1
  stopifnot(is.numeric(tau), tau>0)
  at.list<-vector("list",length(treatment.levels))
  at.list[[1]]<-list(treatment=reference)
  for (j in 1:(length(experimental.arms))) {
    at.list[[j+1]]<-list(treatment=experimental.arms[j])
  }
  
  if (!is.null(list.ss)) {
    n.tot<-nrow(data.mfp)
    recruitment<-Vectorize(list.ss[[1]])
    recs<-recruitment(1:nrow(data.mfp))
    follow.up<-list.ss[[2]]+recs
    follow.up<-follow.up[sample(1:n.tot, n.tot)]
    suppressWarnings(censor.time<-rexp(n.tot,list.ss[[3]]))
    data.mfp$event.status<-rep(NA,n.tot)
    for (iii in 1:n.tot) {
      follow.up[iii]<-ifelse(is.na(censor.time[iii]), follow.up[iii],min(follow.up[iii], censor.time[iii]))
      data.mfp$event.status[iii]<-data.mfp$event.time[iii]<follow.up[iii]
      data.mfp$event.time[iii]<-Winsorize(data.mfp$event.time[iii],c(0,follow.up[iii]))
    }
    
  }
  if (tr.model=="FP2.fixed") {
    myformula<-as.formula(paste(as.character(formula)[2],"~fp(treatment,df=4, select=1, alpha=1)", covariate.formula))
    fit<-mfp(myformula, data.mfp, family="cox")
  } else if (tr.model=="FP1.fixed") {
    myformula<-as.formula(paste(as.character(formula)[2],"~fp(treatment,df=2, select=1, alpha=1)", covariate.formula))
    fit<-mfp(myformula, data.mfp, family="cox")
  } else if (tr.model=="FP2.select") {
    myformula<-as.formula(paste(as.character(formula)[2],"~fp(treatment,df=4)", covariate.formula))
    fit<-mfp(myformula, data.mfp, family="cox")
  } else if (tr.model=="FP1.select") {
    myformula<-as.formula(paste(as.character(formula)[2],"~fp(treatment,df=2)", covariate.formula))
    fit<-mfp(myformula, data.mfp, family="cox")
  } 
  assign("myformula2unique", fit$fit$formula, envir = .GlobalEnv)
  flexsurv.fit<-flexsurvspline(myformula2unique, data=data.mfp, k=k, knots=knots, bknots=bknots)
  
  if (se.method=="delta") {
    
    
    up.bounds.CI<-rep(NA,length(experimental.arms))
    low.bounds.CI<-rep(NA,length(experimental.arms))
    mean.est<-rep(NA,length(experimental.arms))
    
    if (summary.measure=="DS") {
        
        invisible(capture.output(try(standsurv_fit <- standsurv(flexsurv.fit, newdata=data.mfp, at = at.list, se=TRUE, t=tau, type="survival", contrast = "difference", ci=TRUE, cl = (1-sig.level*2))), type="message"))
        
      } else if (summary.measure=="RS") {
        
        invisible(capture.output(try(standsurv_fit <- standsurv(flexsurv.fit, newdata=data.mfp, at = at.list, se=TRUE, t=tau, type="survival", contrast = "ratio", ci=TRUE, cl = (1-sig.level*2))), type="message"))
        
      } else if (summary.measure=="DRMST") {
        
        invisible(capture.output(try(standsurv_fit <- standsurv(flexsurv.fit, newdata=data.mfp, at = at.list, se=TRUE, t=tau, type="rmst", contrast = "difference", ci=TRUE, cl = (1-sig.level*2))), type="message"))
        
      } else if (summary.measure=="HR") {
        
        invisible(capture.output(try(standsurv_fit <- standsurv(flexsurv.fit, newdata=data.mfp, at = at.list, se=TRUE, t=tau, type="hazard", contrast = "ratio", ci=TRUE, cl = (1-sig.level*2))), type="message"))

      } 
    if (inherits(standsurv_fit,"try-error")) {
        stop("Error while marginalising results of model.\n")
      } else {
        
        for (j in 1:(length(experimental.arms))) {
          
          up.bounds.CI[j]<-as.numeric(standsurv_fit[1+4*length(treatment.levels)+4*(j-1)+4])
          low.bounds.CI[j]<-as.numeric(standsurv_fit[1+4*length(treatment.levels)+4*(j-1)+3])
          mean.est[j]<-as.numeric(standsurv_fit[1+4*length(treatment.levels)+4*(j-1)+1])  
          
          }
 
    }
      
    
  } else if (se.method=="bootstrap") {
    
    # Function to bootstrap:
    
    min.treat.stack<-NULL
    find.min.treat<- function (data.mfp, indices, list.samsiz=list.ss) {
      # Select bootstrap sample:
      if (!is.null(list.samsiz)) {
        recruitment<-Vectorize(list.samsiz[[1]])
        recs<-recruitment(1:nrow(data.mfp))
        follow.up<-list.samsiz[[2]]+recs
        follow.up<-follow.up[sample(1:nrow(data.mfp), nrow(data.mfp))]
        suppressWarnings(censor.time<-rexp(nrow(data.mfp),list.samsiz[[3]]))
        data.mfp$event.status<-rep(NA,nrow(data.mfp))
        for (iii in 1:nrow(data.mfp)) {
          follow.up[iii]<-ifelse(is.na(censor.time[iii]), follow.up[iii],min(follow.up[iii], censor.time[iii]))
          data.mfp$event.status[iii]<-data.mfp$event.time[iii]<follow.up[iii]
          data.mfp$event.time[iii]<-Winsorize(data.mfp$event.time[iii],c(0,follow.up[iii]))
        }
        
      }
      assign("da2unique", data.mfp[indices,], envir = .GlobalEnv)
      
      # Fit Fractional Polynomial regression model
      if (da2unique$tr.model[1]=="FP2.fixed") {
        myformula<-as.formula(paste(as.character(formula)[2],"~fp(treatment,df=4, select=1, alpha=1)", covariate.formula))
        fit.i<-mfp(myformula, da2unique, family="cox")
      } else if (da2unique$tr.model[1]=="FP1.fixed") {
        myformula<-as.formula(paste(as.character(formula)[2],"~fp(treatment,df=2, select=1, alpha=1)", covariate.formula))
        fit.i<-mfp(myformula, da2unique, family="cox")
      } else if (da2unique$tr.model[1]=="FP2.select") {
        myformula<-as.formula(paste(as.character(formula)[2],"~fp(treatment,df=4)", covariate.formula))
        fit.i<-mfp(myformula, da2unique, family="cox")
      } else if (da2unique$tr.model[1]=="FP1.select") {
        myformula<-as.formula(paste(as.character(formula)[2],"~fp(treatment,df=2)", covariate.formula))
        fit.i<-mfp(myformula, da2unique, family="cox")
      }  
      assign("myformula2unique", fit.i$fit$formula, envir = .GlobalEnv)
      flexsurv.fit.i<-try(flexsurvspline(myformula2unique, data=da2unique, k=k, knots=knots, bknots=bknots))
      
      # Predict treat-response curve 
      
      mean.est<-rep(NA,length(experimental.arms))
      
      if (!inherits(flexsurv.fit.i, "try-error")) {
        for (j in 1:(length(experimental.arms))) {
          
          if (summary.measure=="DS") {
            
            invisible(capture.output(try(standsurv_fit.i <- standsurv(flexsurv.fit.i, newdata=da2unique, at = at.list, se=FALSE, t=tau, type="survival", contrast = "difference", ci=FALSE)), type="message"))
            
          } else if (summary.measure=="RS") {
            
            invisible(capture.output(try(standsurv_fit.i <- standsurv(flexsurv.fit.i, newdata=da2unique, at = at.list, se=FALSE, t=tau, type="survival", contrast = "ratio", ci=FALSE)), type="message"))
            
          } else if (summary.measure=="DRMST") {
            
            invisible(capture.output(try(standsurv_fit.i <- standsurv(flexsurv.fit.i, newdata=da2unique, at = at.list, se=FALSE, t=tau, type="rmst", contrast = "difference", ci=FALSE)), type="message"))
            
          } else if (summary.measure=="HR") {
            
            invisible(capture.output(try(standsurv_fit.i <- standsurv(flexsurv.fit.i, newdata=da2unique, at = at.list, se=FALSE, t=tau, type="hazard", contrast = "ratio", ci=FALSE)), type="message"))
            
          } 
          
          if (inherits(standsurv_fit.i,"try-error")) {
            
            stop("Error while marginalising results of model.\n")
            
          } else {
            
            for (j in 1:(length(experimental.arms))) {
              
              mean.est[j]<-as.numeric(standsurv_fit.i[1+length(treatment.levels)+j])  
              
            }
            
          }
        }
      }

      
      
      output<-c(mean.est)
      return(output)
    }
    
    boot.res<-boot(data.mfp,find.min.treat,M.boot, parallel = parallel, ncpus=n.cpus, cl=cl)
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
  if ((isTRUE(unfavourable)&summary.measure=="HR")||(!isTRUE(unfavourable)&summary.measure!="HR")) {
    acc.index<-((up.bounds.CI-NI.margin)<0)
  } else {
    acc.index<-((low.bounds.CI-NI.margin)>0)
  }
  
  acc.vec<-experimental.arms[acc.index]
  optimal.treat <- ifelse (length(acc.vec)>0, 
                           acc.vec[which.max(abs(acc.vec-reference))],
                           reference)  
  
  if (se.method!="bootstrap") {
    boot.res<-NULL
  } else {
    fit=NULL
  }
  
  results<-list( model.fit = flexsurv.fit, optimal.treat = optimal.treat, 
                 up.bounds.CI = up.bounds.CI, boot.res=boot.res,
                 low.bounds.CI = low.bounds.CI, treatment.levels = treatment.levels,
                 NI.margin = NI.margin, se.method = se.method, 
                 summary.measure = summary.measure, family="survival",
                 reference=reference, estimates=mean.est, tau=tau)
  class(results)<-c("ROCI", "list")
  
  rm(myformula2unique, envir=.GlobalEnv)
  if (se.method=="bootstrap") rm(da2unique, envir=.GlobalEnv)
  
  return(results)
  
}
