test.NI.binary <- function(n.control=NULL, n.experim=NULL, e.control=NULL, e.experim=NULL,
                           formula=NULL, data=NULL, control.level=0,
                           NI.margin, sig.level=0.025, summary.measure="RD", 
                           print.out=TRUE, unfavourable=TRUE, test.type=NULL,
                           M.boot=2000, bootCI.type="bca", BB.adj=0.0001, 
                           recursive.p.estim=FALSE) {
  covariates<-NULL
  if (any(is.null(n.control), is.null(n.experim), is.null(e.control), is.null(e.experim))&&any(is.null(data), is.null(formula))) {
    stop("Either counts of events and participants or formula+data must be provided.\n")
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
    n.control<-sum(treatment==control.level)
    n.experim<-length(treatment)-n.control
    e.control<-sum(outcomes[treatment==control.level]==1)
    e.experim<-sum(outcomes[treatment!=control.level]==1)
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
  stopifnot(is.numeric(n.control), n.control>0)
  stopifnot(is.numeric(n.experim), n.experim>0)
  stopifnot(is.numeric(e.control), e.control>=0, n.control>=e.control)
  stopifnot(is.numeric(e.experim), e.experim>=0, n.experim>=e.experim)
  stopifnot(is.numeric(NI.margin))
  stopifnot(is.numeric(sig.level), sig.level < 0.5, sig.level > 0)
  stopifnot(is.character(summary.measure),(( summary.measure == "RD" ) || ( summary.measure == "RR" ) || ( summary.measure == "OR" ) || ( summary.measure == "AS" )))
  stopifnot(is.logical(print.out), !is.na(print.out))
  stopifnot(is.logical(unfavourable), !is.na(unfavourable))
  stopifnot(is.logical(recursive.p.estim), !is.na(recursive.p.estim))
  stopifnot(is.numeric(M.boot), M.boot>1)
  stopifnot(is.character(bootCI.type), bootCI.type%in%c("norm","perc","bca","basic"))
  stopifnot(is.numeric(BB.adj), BB.adj>0)
  adjusted<-(!is.null(covariates)&&any(dim(covariates)>0))
  if (is.null(formula)) {
    outcomes<-c(rep(1, e.experim),rep(0, n.experim-e.experim),rep(1, e.control), rep(0, n.control-e.control))
    treatment<-factor(c(rep(1,n.experim), rep(0, n.control)))
    treat.index<-1
    mydata<-data.frame(outcomes,treatment)
    myformula<-as.formula("outcomes~treatment")
  }
  if (is.null(test.type)) {
    if (!adjusted) {
      if (summary.measure=="RD") {
        test.type<-"Newcombe10"
      } else if (summary.measure=="RR") {
        test.type<-"Koopman"
      } else if (summary.measure=="OR") {
        test.type<-"Baptista.Pike.midp"
      } else {
        test.type<-"Wald"
      } 
    } else {
        test.type<-"logistic"
    }
    
  }
  stopifnot(is.character(test.type))
  if (summary.measure=="RD") {
    if (!adjusted) {
      stopifnot(test.type%in%c("Wald", "Wald.cc", "Hauck.Anderson", "Gart.Nam",
                               "Newcombe10", "Newcombe11", "Haldane", "Jeffreys.Perks",
                               "Agresti.Caffo", "Miettinen.Nurminen", "Farrington.Manning",
                               "logistic", "binreg", "bootstrap", "Agresti.Min", "Brown.Li.Jeffreys", 
                               "Chan.Zhang", "BLNM", "Mee", "uncond.midp", "Berger.Boos",
                               "MUE.Lin", "MUE.parametric.bootstrap"))
      
    } else {
      stopifnot(test.type%in%c("logistic", "binreg", "bootstrap"))
      
    }
   } else if (summary.measure=="RR") {
     if (!adjusted) {
       stopifnot(test.type%in%c("Wald.Katz", "adjusted.Wald.Katz", "inverse.hyperbolic.sine", "Koopman",
                                "MOVER.R", "Miettinen.Nurminen", "MOVER", "Gart.Nam", "score.cc",
                                "logregression", "logistic", "bootstrap", "Bailey", "Noether", 
                                "Chan.Zhang", "Agresti.Min", "uncond.midp", "Berger.Boos"))
     } else {
       stopifnot(test.type%in%c("logregression", "logistic", "bootstrap"))
       
     }
  } else if (summary.measure=="OR") {
    if (!adjusted) {
      stopifnot(test.type%in%c("Wald.Woolf", "adjusted.Wald.Woolf", "inverse.hyperbolic.sine", "Cornfield.exact",
                               "MOVER.R", "Miettinen.Nurminen", "MOVER", "Gart.Nam", "score.cc",
                               "logistic", "bootstrap", "Cornfield.midp", "Baptista.Pike.exact", "Baptista.Pike.midp", 
                               "Chan.Zhang", "Agresti.Min", "uncond.midp", "Berger.Boos"))
    } else {
      stopifnot(test.type%in%c("logistic", "bootstrap"))
      
    }
  } else {
    if (!adjusted) {
      stopifnot(test.type%in%c("Wald","logistic", "bootstrap"))
    } else {
      stopifnot(test.type%in%c("logistic", "bootstrap"))
    }
  }

  estimate<-se<-Z<-p<-NULL
  
  if (summary.measure=="RD") {
    t2x2<-matrix(c(e.control, n.control-e.control, e.experim, n.experim-e.experim), nrow = 2, byrow = TRUE)
    if ((unfavourable == T)&&(NI.margin<=0)) stop("When outcome is unfavourable, a risk difference NI margin needs to be positive.\n")
    if ((unfavourable == F)&&(NI.margin>=0)) stop("When outcome is favourable, a risk difference NI margin needs to be negative.\n")
    if (NI.margin>=1) stop("NI.margin cannot be greater than 1, i.e. 100 percentage points, or otherwise the test is meaningless.\n ")
    if (NI.margin<=-1) stop("NI.margin cannot be lower than -1, i.e. -100 percentage points, or otherwise the test is meaningless.\n ")
    NIm<- NI.margin
    if (test.type=="Wald") {
      se <- sqrt(e.experim/n.experim*(1-e.experim/n.experim)/n.experim+e.control/n.control*(1-e.control/n.control)/n.control)
      estimate <- e.experim/n.experim-e.control/n.control
      Z <- ifelse( unfavourable==T, (estimate - NIm)/se, -(estimate - NIm)/se)
      p <- pnorm(Z)
      CI <- c(estimate-qnorm(1-sig.level)*se,estimate+qnorm(1-sig.level)*se)
    } else if (test.type=="Wald.cc") {
      se <- sqrt(e.experim/n.experim*(1-e.experim/n.experim)/n.experim+e.control/n.control*(1-e.control/n.control)/n.control)
      estimate <- e.experim/n.experim-e.control/n.control
      Z <- ifelse( unfavourable==T, (estimate - NIm)/se, -(estimate - NIm)/se)
      Z<-Z+(1/n.control+1/n.experim)/2*(1-2*(unfavourable==F))
      p <- pnorm(Z)
      CI <- c(estimate-(qnorm(1-sig.level)*se+(1/n.control+1/n.experim)/2),
              estimate+(qnorm(1-sig.level)*se+(1/n.control+1/n.experim)/2))
    } else if (test.type=="Newcombe10") {
        test<-BinomDiffCI(e.experim, n.experim, e.control, n.control, conf.level = (1-sig.level*2), method = "score")
        CI <- test[2:3]
        estimate<-test[1]
    } else if (test.type=="Newcombe11") {
      test<-BinomDiffCI(e.experim, n.experim, e.control, n.control, conf.level = (1-sig.level*2), method = "scorecc")
      CI <- test[2:3]
      estimate<-test[1]
    } else if (test.type=="Gart.Nam") {
      test<-scasci(e.experim,n.experim,e.control,n.control, level=(1-sig.level*2))$estimates
      CI <-test[c(1,3)]
      estimate<-test[2]
    } else if (test.type=="Agresti.Caffo") {
      se <- sqrt((e.experim+1)/(n.experim+2)*(1-(e.experim+1)/(n.experim+2))/(n.experim+2)+(e.control+1)/(n.control+2)*(1-(e.control+1)/(n.control+2))/(n.control+2))
      estimate <- (e.experim+1)/(n.experim+2)-(e.control+1)/(n.control+2)
      Z <- ifelse( unfavourable==T, (estimate - NIm)/se, -(estimate - NIm)/se)
      p <- pnorm(Z)
      CI <- c(estimate-qnorm(1-sig.level)*se,estimate+qnorm(1-sig.level)*se)
    } else if (test.type=="Haldane") {
      test<-BinomDiffCI(e.experim, n.experim, e.control, n.control, conf.level = (1-sig.level*2), method = "hal")
      CI <- test[2:3]
      estimate<-test[1]
    } else if (test.type=="Hauck.Anderson") {
      test<-BinomDiffCI(e.experim, n.experim, e.control, n.control, conf.level = (1-sig.level*2), method = "ha")
      CI <- test[2:3]
      estimate<-test[1]
    } else if (test.type=="Jeffreys.Perks") {
      test<-BinomDiffCI(e.experim, n.experim, e.control, n.control, conf.level = (1-sig.level*2), method = "jp")
      CI <- test[2:3]
      estimate<-test[1]
    } else if (test.type=="Miettinen.Nurminen") {
      test<-BinomDiffCI(e.experim, n.experim, e.control, n.control, conf.level = (1-sig.level*2), method = "mn")
      CI <- test[2:3]
      estimate<-test[1]
    } else if (test.type=="Mee") {
      test<-BinomDiffCI(e.experim, n.experim, e.control, n.control, conf.level = (1-sig.level*2), method = "mee")
      CI <- test[2:3]
      estimate<-test[1]
    } else if (test.type=="Farrington.Manning") {
      alt.type<-ifelse(isTRUE(unfavourable), "less", "greater")
      test <- farrington.manning(c(rep(TRUE, e.experim), rep(FALSE, n.experim-e.experim)),c(rep(TRUE, e.control), rep(FALSE, n.control-e.control)), delta=NI.margin, alpha = sig.level, alternative = alt.type)
      CI<-test$conf.int
      estimate<-test$estimate
      Z<-test$statistic
      p<-test$p.value
      se<-(estimate - NIm)/Z
    } else if (test.type=="Brown.Li.Jeffreys") {
      test<-BinomDiffCI(e.experim, n.experim, e.control, n.control, conf.level = (1-sig.level*2), method = "blj")
      CI <- test[2:3]
      estimate<-test[1]
    } else if (test.type=="BLNM") {
        test1 <- BinomDiffCI(e.experim, n.experim, e.control, n.control, conf.level = (1-sig.level*2), method = "blj")
        test2 <- BinomDiffCI(e.experim, n.experim, e.control, n.control, conf.level = (1-sig.level*2), method = "mn")
        CI <- test1[2:3]/3 + 2 * test2[2:3]/3
        estimate<-test1[1]/3 + 2 * test2[1]/3
      } else if (test.type=="logistic") {
        
        fit<-glm(myformula, data=mydata, family = binomial)
        fit.std<-avg_comparisons(fit, conf_level = (1-sig.level*2))
        CI <- c(fit.std[fit.std$term=="treatment","conf.low"], fit.std[fit.std$term=="treatment","conf.high"])
        estimate<-fit.std[fit.std$term=="treatment","estimate"]
        se<-fit.std[fit.std$term=="treatment","std.error"]
        
      } else if (test.type=="binreg") {
        
        fit<-glm(myformula, data=mydata, family = binomial(link = "identity"))
        estimate<-coef(summary(fit))[treat.index+1,"Estimate"]
        se<-coef(summary(fit))[treat.index+1,"Std. Error"]
        CI <- c(estimate-qnorm(1-sig.level)*se,estimate+qnorm(1-sig.level)*se)
        
      } else if ( test.type == "bootstrap") {
  
        rdif <- function(dat, indices) {
          d <- dat[indices,] # allows boot to select sample
          rd <- mean(d[d$treatment!=control.level,"outcomes"]) - mean(d[d$treatment==control.level,"outcomes"])
          return(rd)
        } 
        res.b<-boot(mydata, rdif, R=M.boot)
        CI<-boot.ci(res.b, type=bootCI.type, conf=1-sig.level*2)[[4]][4:5-2*(bootCI.type=="norm")]
        estimate<-res.b$t0
        
      } else if (test.type == "Agresti.Min") {
        fit<-uncondExact2x2(e.control,n.control,e.experim,n.experim, method="score", tsmethod = "square", conf.int = T)
        CI <- as.numeric(fit$conf.int)
        estimate<-as.numeric(fit$estimate)
      } else if (test.type == "Chan.Zhang") {
        fit<-uncondExact2x2(e.control,n.control,e.experim,n.experim, method="score", tsmethod = "central", conf.int = T)
        CI <- as.numeric(fit$conf.int)
        estimate<-as.numeric(fit$estimate)
      } else if (test.type == "uncond.midp") {
        fit<-uncondExact2x2(e.control,n.control,e.experim,n.experim, method="score", tsmethod = "square", midp=T, conf.int = T)
        CI <- as.numeric(fit$conf.int)
        estimate<-as.numeric(fit$estimate)
      } else if (test.type == "Berger.Boos") {
        fit<-uncondExact2x2(e.control,n.control,e.experim,n.experim, method="score", tsmethod = "square", gamma=BB.adj, conf.int = T)
        CI <- as.numeric(fit$conf.int)
        estimate<-as.numeric(fit$estimate)
      } else if (test.type == "MUE.Lin") {
        p0.tilde<-ifelse(((e.control>0)&&(e.control<n.control)),(qbeta(0.5,e.control,n.control-e.control+1)+qbeta(0.5,e.control+1,n.control-e.control))/2,
                           ifelse(e.control==0,(1-0.5^(1/n.control))/2,(1+0.5^(1/n.control))/2))
        p1.tilde<-ifelse(((e.experim>0)&&(e.experim<n.experim)),(qbeta(0.5,e.experim,n.experim-e.experim+1)+qbeta(0.5,e.experim+1,n.experim-e.experim))/2,
                         ifelse(e.experim==0,(1-0.5^(1/n.experim))/2,(1+0.5^(1/n.experim))/2))
        probs.mat<-matrix(NA,(n.control+1)*(n.experim+1),7)
        kk=1
        for (ii in 0:n.control) {
          for (jj in 0:n.experim) {
            probs.mat[kk,6]<-ii
            probs.mat[kk,7]<-jj
            probs.mat[kk,2]<-ifelse(((ii>0)&&(ii<n.control)),(qbeta(0.5,ii,n.control-ii+1)+qbeta(0.5,ii+1,n.control-ii))/2,
                                    ifelse(ii==0,(1-0.5^(1/n.control))/2,(1+0.5^(1/n.control))/2))
            
            probs.mat[kk,3]<-ifelse(((jj>0)&&(jj<n.experim)),(qbeta(0.5,jj,n.experim-jj+1)+qbeta(0.5,jj+1,n.experim-jj))/2,
                                    ifelse(jj==0,(1-0.5^(1/n.experim))/2,(1+0.5^(1/n.experim))/2))
            
            probs.mat[kk,4]<-probs.mat[kk,3]-probs.mat[kk,2]
            probs.mat[kk,1]<-exp(lchoose(n.control,ii)+lchoose(n.experim,jj)+log(p0.tilde)*ii+log(p1.tilde)*jj+log(1-p0.tilde)*(n.control-ii)+log(1-p1.tilde)*(n.experim-jj))
            kk=kk+1
          }
        }
        probs.mat<-probs.mat[order(probs.mat[,4]),]
        probs.mat[,5]<-cumsum(probs.mat[,1])
        
        ci.l.l<-max(which(probs.mat[,5]<=sig.level))
        ci.l.u<-min(which(probs.mat[,5]>=sig.level))
        ci.l<-(probs.mat[ci.l.l,4]*(probs.mat[ci.l.u,5]-sig.level)+probs.mat[ci.l.u,4]*(-probs.mat[ci.l.l,5]+sig.level))/(probs.mat[ci.l.u,5]-probs.mat[ci.l.l,5])
        ci.u.l<-max(which(probs.mat[,5]<=(1-sig.level)))
        ci.u.u<-min(which(probs.mat[,5]>=(1-sig.level)))
        ci.u<-(probs.mat[ci.u.l,4]*(probs.mat[ci.u.u,5]-(1-sig.level))+probs.mat[ci.u.u,4]*(-probs.mat[ci.u.l,5]+1-sig.level))/(probs.mat[ci.u.u,5]-probs.mat[ci.u.l,5])
        CI<-c(ci.l,ci.u)
        estimate<-mean(c(ci.l, ci.u))
      } else if (test.type == "MUE.parametric.bootstrap") {
        p0.tilde<-ifelse(((e.control>0)&&(e.control<n.control)),(qbeta(0.5,e.control,n.control-e.control+1)+qbeta(0.5,e.control+1,n.control-e.control))/2,
                         ifelse(e.control==0,(1-0.5^(1/n.control))/2,(1+0.5^(1/n.control))/2))
        p1.tilde<-ifelse(((e.experim>0)&&(e.experim<n.experim)),(qbeta(0.5,e.experim,n.experim-e.experim+1)+qbeta(0.5,e.experim+1,n.experim-e.experim))/2,
                         ifelse(e.experim==0,(1-0.5^(1/n.experim))/2,(1+0.5^(1/n.experim))/2))
        y<-c(rep(1, e.experim),rep(0, n.experim-e.experim),rep(1, e.control), rep(0, n.control-e.control))
        treat<-factor(c(rep(1,n.experim), rep(0, n.control)))
        dd<-data.frame(y,treat)
        rdif <- function(dat) {
          p0.tilde.i<-ifelse(((as.numeric(dat[1,1])>0)&&(as.numeric(dat[1,1])<n.control)),(qbeta(0.5,as.numeric(dat[1,1]),n.control-as.numeric(dat[1,1])+1)+qbeta(0.5,as.numeric(dat[1,1])+1,n.control-as.numeric(dat[1,1])))/2,
                             ifelse(as.numeric(dat[1,1])==0,(1-0.5^(1/n.control))/2,(1+0.5^(1/n.control))/2))
          p1.tilde.i<-ifelse(((as.numeric(dat[1,2])>0)&&(as.numeric(dat[1,2])<n.experim)),(qbeta(0.5,as.numeric(dat[1,2]),n.experim-as.numeric(dat[1,2])+1)+qbeta(0.5,as.numeric(dat[1,2])+1,n.experim-as.numeric(dat[1,2])))/2,
                             ifelse(as.numeric(dat[1,2])==0,(1-0.5^(1/n.experim))/2,(1+0.5^(1/n.experim))/2))
          
          rd <- p1.tilde.i-p0.tilde.i
          return(rd)
        } 
        rg <- function(dat, mle) {
          # Function to generate random exponential variates.
          # mle will contain the mean of the original data
          out <- data.frame(matrix(NA,1,2))
          out[1,1] <- rbinom(1,mle[2], mle[1])
          out[1,2] <- rbinom(1,mle[4], mle[3])
          out
        }
        res.b<-boot(dd, rdif, R = M.boot, sim = "parametric",
                    ran.gen = rg, mle = c(p0.tilde,n.control,p1.tilde,n.experim))
        CI<-boot.ci(res.b, type=bootCI.type, conf=1-sig.level*2)[[4]][4:5-2*(bootCI.type=="norm")]
        estimate<-res.b$t0
    }
    estimate.n<-mean(CI)
    if (is.null(se)) {
      se<-(CI[2]-CI[1])/(2*qnorm(1-sig.level))
      is.se.est<-T
    } else {
      is.se.est<-F
    }
    if (isTRUE(recursive.p.estim)) {
      
      if (estimate==NI.margin) {
        p=0.5
      } else {
        sig.lev.est<-try(uniroot(func.to.opt, c(10^(-15), 0.5-10^(-15)), 
                                 n.control=n.control, n.experim=n.experim, 
                                 e.control=e.control, e.experim=e.experim,
                                 NI.margin=NI.margin, summary.measure=summary.measure, 
                                 unfavourable=unfavourable, test.type=test.type,
                                 M.boot=M.boot, BB.adj=BB.adj,
                                 tol = 0.000001)$root, silent=TRUE)
        if (!inherits(sig.lev.est,"try-error")) {
          p<-ifelse(((unfavourable==T)&&(estimate>NI.margin))||
                      ((unfavourable==F)&&(estimate<NI.margin)),
                    1-sig.lev.est, sig.lev.est)
        } else {
          p<-NULL
          warning("recursive estimation of p failed. Providing standard estimate.\n")
        }

      }
      
      
    }
    if (is.null(p)) {
      Z <- ifelse(unfavourable==T,(estimate.n - NIm)/se,(-estimate.n + NIm)/se)
      p <- pnorm(Z)
      is.p.est<-T
    } else {
      is.p.est<-F
    }
    if ((unfavourable==T&&CI[2]<NI.margin)||(unfavourable==F&&CI[1]>NI.margin)) { 
      non.inferiority<-T
    } else {
      non.inferiority<-F
    }
    
    if (print.out==T) {
      cat("Testing for non-inferiority.\nSummary measure: Risk difference.\nNon-inferiority margin = ", NI.margin, ".\nMethod: ",test.type,
          ".\nEstimate = ", estimate, 
          "\nConfidence interval (Two-sided ", (1-sig.level*2)*100,"%): (", CI[1], ",", CI[2], 
          ")\np-value = ", p, ".\n" , sep="")
      if ((unfavourable==T&&CI[2]<NI.margin)||(unfavourable==F&&CI[1]>NI.margin)) { 
        cat("The confidence interval does not cross the null ( RD = ", NI.margin, " ), and hence we have evidence of non-inferiority.\n", sep="")
      } else {
        cat("The confidence interval crosses the null ( RD = ", NI.margin, " ), and hence we have NO evidence of non-inferiority.\n", sep="")
      }
      if (is.p.est==T) {
        if (is.se.est==T) {
          cat("Note: with the test = ",test.type, " for summary measure = ", summary.measure,", p-value and standard error are only approximations based on a Z test with given logarithm of estimate and CI.\n")
        } else {
          cat("Note: with the test = ",test.type, " for summary measure = ", summary.measure,", p-value is only an approximation based on a Z test with given logarithm of estimate and CI.\n")
        }
      }
      
    }
    
  } else if (summary.measure == "RR") {
    t2x2<-matrix(c(e.experim, n.experim-e.experim, e.control, n.control-e.control), nrow = 2, byrow = TRUE)
    if ((unfavourable == T)&&(NI.margin<=1)) stop("When outcome is unfavourable, a NI margin on the risk ratio scale needs to be >1.")
    if ((unfavourable == F)&&(NI.margin>=1)) stop("When outcome is favourable, a NI margin on the risk ratio scale needs to be <1.")
    if (NI.margin<=0) stop("A risk ratio margin must be >0.\n")
    NIm<- log(NI.margin)
    if (test.type=="Wald.Katz") {
      if ((e.control==0)||(e.experim==0)) stop("Wald.Katz interval not computable in presence of zero cell counts (e.control=0 or e.experim=0).\n")
      se <- sqrt(1/e.control-1/n.control+1/e.experim-1/n.experim)
      estimate <- log((e.experim/n.experim)/(e.control/n.control))
      Z <- ifelse( unfavourable==T, (estimate - NIm)/se, -(estimate - NIm)/se)
      p <- pnorm(Z)
      CI <- exp(c(estimate-qnorm(1-sig.level)*se,estimate+qnorm(1-sig.level)*se))
      estimate <- exp(estimate)
    } else if (test.type=="adjusted.Wald.Katz") {
      se <- sqrt(1/(e.control+0.5)-1/(n.control+0.5)+1/(e.experim+0.5)-1/(n.experim+0.5))
      estimate <- log(((e.experim+0.5)/(n.experim+0.5))/((e.control+0.5)/(n.control+0.5)))
      Z <- ifelse( unfavourable==T, (estimate - NIm)/se, -(estimate - NIm)/se)
      p <- pnorm(Z)
      CI <- exp(c(estimate-qnorm(1-sig.level)*se,estimate+qnorm(1-sig.level)*se))
      estimate <- exp(estimate)
    } else if (test.type=="inverse.hyperbolic.sine") {
      if (e.experim==0) t2x2[1,1]<-round(qnorm(1-sig.level)^2)
      if (e.control==0) t2x2[2,1]<-round(qnorm(1-sig.level)^2)
      test<-Inv_sinh_CI_ratio_2x2(t2x2, alpha = 2*sig.level)
      CI<-as.numeric(test[1:2])
      estimate<-as.numeric(test[3])
    } else if (test.type=="Koopman") {
      test<-Koopman_asymptotic_score_CI_2x2(t2x2, alpha = 2*sig.level)
      CI<-as.numeric(test[1:2])
      estimate<-as.numeric(test[3])
    } else if (test.type=="Miettinen.Nurminen") {
      test<-MiettinenNurminen_asymptotic_score_CI_ratio_2x2(t2x2, alpha = 2*sig.level)
      CI<-as.numeric(test[1:2])
      estimate<-as.numeric(test[3])
    } else if (test.type=="MOVER.R") {
      test<-MOVER_R_Wilson_CI_ratio_2x2(t2x2, alpha = 2*sig.level)
      CI<-as.numeric(test[1:2])
      estimate<-as.numeric(test[3])
    } else if (test.type=="MOVER") {
      test<-moverci(e.experim,n.experim,e.control,n.control,contrast = "RR", level = 1-2*sig.level)
      CI<-test[c(1,3)]
      estimate<-test[2]
    } else if (test.type=="Gart.Nam") {
      test<-scasci(e.experim,n.experim,e.control,n.control,contrast = "RR", level = 1-2*sig.level)$estimates
      CI<-test[c(1,3)]
      estimate<-test[2]
    } else if (test.type=="score.cc") {
      test<-ratesci::scoreci(e.experim,n.experim,e.control,n.control,contrast = "RR", level = 1-2*sig.level, cc=T)$estimates
      CI<-test[c(1,3)]
      estimate<-test[2]
    } else if (test.type=="Bailey") {
      test<-BinomRatioCI(x1 = e.experim, n1 = n.experim, x2 = e.control, n2 = n.control, method = "bailey")
      CI<-test[2:3]
      estimate<-test[1]
    } else if (test.type=="Noether") {
      test<-BinomRatioCI(x1 = e.experim, n1 = n.experim, x2 = e.control, n2 = n.control, method = "noether")
      CI<-test[2:3]
      estimate<-test[1]
    } else if (test.type=="bootstrap") {
      
      rrat <- function(dat, indices) {
        d <- dat[indices,] # allows boot to select sample
        rr <- log(mean(d[d$treatment!=control.level,"outcomes"])) - log(mean(d[d$treatment==control.level,"outcomes"]))
        return(rr)
      } 
      res.b<-boot(mydata, rrat, R=M.boot)
      CI<-exp(boot.ci(res.b, type=bootCI.type, conf=1-sig.level*2)[[4]][4:5-2*(bootCI.type=="norm")])
      estimate<-exp(res.b$t0)
      
    } else if (test.type == "Agresti.Min") {
      fit<-uncondExact2x2(e.control,n.control,e.experim,n.experim, method="score", tsmethod = "square", conf.int = T, parmtype="ratio")
      CI <- as.numeric(fit$conf.int)
      estimate<-as.numeric(fit$estimate)
    } else if (test.type == "Chan.Zhang") {
      fit<-uncondExact2x2(e.control,n.control,e.experim,n.experim, method="score", tsmethod = "central", conf.int = T, parmtype="ratio")
      CI <- as.numeric(fit$conf.int)
      estimate<-as.numeric(fit$estimate)
    } else if (test.type == "uncond.midp") {
      fit<-uncondExact2x2(e.control,n.control,e.experim,n.experim, method="score", tsmethod = "square", midp=T, conf.int = T, parmtype="ratio")
      CI <- as.numeric(fit$conf.int)
      estimate<-as.numeric(fit$estimate)
    } else if (test.type == "Berger.Boos") {
      fit<-uncondExact2x2(e.control,n.control,e.experim,n.experim, method="score", tsmethod = "square", gamma=BB.adj, conf.int = T, parmtype="ratio")
      CI <- as.numeric(fit$conf.int)
      estimate<-as.numeric(fit$estimate)
    } else if (test.type=="logistic") {
      fit<-glm(myformula, data=mydata, family = binomial)
      fit.std<-avg_comparisons(fit, conf_level = (1-sig.level*2), comparison="lnratio", transform = exp)
      CI <- c(fit.std[fit.std$term=="treatment","conf.low"], fit.std[fit.std$term=="treatment","conf.high"])
      estimate<-fit.std[fit.std$term=="treatment","estimate"]

    } else if (test.type=="logregression") {
      if (e.experim==0||e.control==0) stop("logregression not possible with zero cell counts (e.control=0 or e.experim=0).\n")
      fit<-glm(myformula, data=mydata, family = binomial(link="log"))
      row.treat<-which(grepl("treatment", row.names(coef(summary(fit)))))
      estimate<-exp(coef(summary(fit)))[row.treat,"Estimate"]
      CI <- exp(suppressMessages(confint(fit, level = 1-sig.level*2))[row.treat,])
    } 
    estimate.n<-mean(log(CI))
    if (is.null(se)) {
      se<-(log(CI[2])-log(CI[1]))/(2*qnorm(1-sig.level))
      is.se.est<-T
    } else {
      is.se.est<-F
    }
    if (isTRUE(recursive.p.estim)) {
      
      if (estimate==NI.margin) {
        p=0.5
      } else {
        sig.lev.est<-try(uniroot(func.to.opt, c(10^(-15), 0.5-10^(-15)), 
                                 n.control=n.control, n.experim=n.experim, 
                                 e.control=e.control, e.experim=e.experim,
                                 NI.margin=NI.margin, summary.measure=summary.measure, 
                                 unfavourable=unfavourable, test.type=test.type,
                                 M.boot=M.boot, BB.adj=BB.adj,
                                 tol = 0.000001)$root, silent=TRUE)
        if (!inherits(sig.lev.est,"try-error")) {
          p<-ifelse(((unfavourable==T)&&(estimate>NI.margin))||
                      ((unfavourable==F)&&(estimate<NI.margin)),
                    1-sig.lev.est, sig.lev.est)
        } else {
          p<-NULL
          warning("Recursive estimation of p failed. Providing standard estimate.\n")
        }
      }
    }
    if (is.null(p)) {
      Z <- ifelse(unfavourable==T,(estimate.n - NIm)/se,(-estimate.n + NIm)/se)
      p <- pnorm(Z)
      is.p.est<-T
    } else {
      is.p.est<-F
    }
    if ((unfavourable==T&&CI[2]<NI.margin)||(unfavourable==F&&CI[1]>NI.margin)) { 
      non.inferiority<-T
    } else {
      non.inferiority<-F
    }
    if (print.out==T) {
      cat("Testing for non-inferiority.\nSummary measure: Risk ratio.\nNon-inferiority margin = ", NI.margin, ".\nMethod: ",test.type,
          ".\nEstimate = ", estimate, 
          "\nConfidence interval (Two-sided ", (1-sig.level*2)*100,"%): (", CI[1], ",", CI[2], 
          ")\np-value = ", p, ".\n" , sep="")
      if ((unfavourable==T&&CI[2]<NI.margin)||(unfavourable==F&&CI[1]>NI.margin)) { 
        cat("The confidence interval does not cross the null ( RR = ", NI.margin, " ), and hence we have evidence of non-inferiority.\n", sep="")
      } else {
        cat("The confidence interval crosses the null ( RR = ", NI.margin, " ), and hence we have NO evidence of non-inferiority.\n", sep="")
      }
      if (is.p.est==T) {
        if (is.se.est==T) {
          cat("Note: with the test = ",test.type, " for summary measure = ", summary.measure,", p-value and standard error are only approximations based on a Z test with given logarithm of estimate and CI.\n")
        } else {
          cat("Note: with the test = ",test.type, " for summary measure = ", summary.measure,", p-value is only an approximation based on a Z test with given logarithm of estimate and CI.\n")
        }
      }
      
    }
    
  } else if (summary.measure == "AS") {
    is.p.est<-is.se.est<-F
    NIm<-NI.margin
    if ((unfavourable == T)&&(NI.margin<=0)) stop("When outcome is unfavourable, a NI margin on the arc-sine difference scale needs to be >0.")
    if ((unfavourable == F)&&(NI.margin>=0)) stop("When outcome is favourable, a NI margin on the arc-sine difference scale needs to be <0.")
    if (test.type=="Wald") {
      se <- sqrt(1/(4*n.control)+1/(4*n.experim))
      estimate <- asin(sqrt(e.experim/n.experim))-asin(sqrt(e.control/n.control))
      Z <- (estimate - NIm)/se
      p <- pnorm(Z)
      CI <- c(estimate-qnorm(1-sig.level)*se,estimate+qnorm(1-sig.level)*se)
    } else if (test.type=="logistic") {
      fit<-glm(myformula, data=mydata, family = binomial)
      fit.std<-avg_comparisons(fit, conf_level = (1-sig.level*2), comparison=function(hi, lo) asin(sqrt(hi))-asin(sqrt(lo)))
      CI <- c(fit.std[fit.std$term=="treatment","conf.low"], fit.std[fit.std$term=="treatment","conf.high"])
      estimate<-fit.std[fit.std$term=="treatment","estimate"]
      se<-fit.std[fit.std$term=="treatment","std.error"]
      p<-NULL
    } else if (test.type=="bootstrap") {
      
      asdif <- function(dat, indices) {
        d <- dat[indices,] # allows boot to select sample
        as<-asin(sqrt(mean(d[d$treatment!=control.level,"outcomes"])))-asin(sqrt(mean(d[d$treatment==control.level,"outcomes"])))
        return(as)
      } 
      res.b<-boot(mydata, asdif, R=M.boot)
      CI<-boot.ci(res.b, type=bootCI.type, conf=1-sig.level*2)[[4]][4:5-2*(bootCI.type=="norm")]
      estimate<-res.b$t0
      
    }
    estimate.n<-mean(CI)
    if (is.null(p)) {
      Z <- ifelse(unfavourable==T,(estimate.n - NIm)/se,(-estimate.n + NIm)/se)
      p <- pnorm(Z)
      is.p.est<-T
    } else {
      is.p.est<-F
    }
    if (isTRUE(recursive.p.estim)) {
      
      if (estimate==NI.margin) {
        p=0.5
      } else {
        sig.lev.est<-try(uniroot(func.to.opt, c(10^(-15), 0.5-10^(-15)), 
                                 n.control=n.control, n.experim=n.experim, 
                                 e.control=e.control, e.experim=e.experim,
                                 NI.margin=NI.margin, summary.measure=summary.measure, 
                                 unfavourable=unfavourable, test.type=test.type,
                                 M.boot=M.boot, BB.adj=BB.adj,
                                 tol = 0.000001)$root, silent=TRUE)
        if (!inherits(sig.lev.est, "try-error")) {
          p<-ifelse(((unfavourable==T)&&(estimate>NI.margin))||
                      ((unfavourable==F)&&(estimate<NI.margin)),
                    1-sig.lev.est, sig.lev.est)
        } else {
          warning("Recursive estimation of p failed. Providing standard estimate.\n")
        }
      }
      
      
    }
    
    if ((unfavourable==T&&CI[2]<NI.margin)||(unfavourable==F&&CI[1]>NI.margin)) { 
      non.inferiority<-T
    } else {
      non.inferiority<-F
    }
    if (print.out==T) {
      cat("Testing for non-inferiority.\nSummary measure: Arc-sine difference.\nNon-inferiority margin = ", NI.margin, ".\nMethod: ",test.type,
          ".\nEstimate = ", estimate, 
          "\nConfidence interval (Two-sided ", (1-sig.level*2)*100,"%): (", CI[1], ",", CI[2], 
          ")\np-value = ", p, ".\n" , sep="")
      if ((unfavourable==T&&CI[2]<NI.margin)||(unfavourable==F&&CI[1]>NI.margin)) { 
        cat("The confidence interval does not cross the null ( ASD = ", NI.margin, " ), and hence we have evidence of non-inferiority.\n", sep="")
      } else {
        cat("The confidence interval crosses the null ( ASD = ", NI.margin, " ), and hence we have NO evidence of non-inferiority.\n", sep="")
        }
      } 
    } else if (summary.measure == "OR") {
      t2x2<-matrix(c(e.experim, n.experim-e.experim, e.control, n.control-e.control), nrow = 2, byrow = TRUE)
    if ((unfavourable == T)&&(NI.margin<=1)) stop("When outcome is unfavourable, a NI margin on the odds ratio scale needs to be >1.")
    if ((unfavourable == F)&&(NI.margin>=1)) stop("When outcome is favourable, a NI margin on the odds ratio scale needs to be <1.")
    if (NI.margin<=0) stop("A odds ratio margin must be >0.\n")
    NIm<- log(NI.margin)
    if (test.type=="Wald.Woolf") {
      if (e.experim==0||e.control==0) stop("Wald.Woolf not informative with zero cell counts (e.control=0 or e.experim=0).\n")
      se <- sqrt(1/e.control+1/(n.control-e.control)+1/e.experim+1/(n.experim-e.experim))
      estimate <- log((e.experim/(n.experim-e.experim))/(e.control/(n.control-e.control)))
      Z <- ifelse( unfavourable==T, (estimate - NIm)/se, -(estimate - NIm)/se)
      p <- pnorm(Z)
      CI <- exp(c(estimate-qnorm(1-sig.level)*se,estimate+qnorm(1-sig.level)*se))
      estimate <- exp(estimate)
    } else if (test.type=="adjusted.Wald.Woolf") {
      se <- sqrt(1/(e.control+0.5)+1/(n.control-e.control+0.5)+1/(e.experim+0.5)+1/(n.experim-e.experim+0.5))
      estimate <- log(((e.experim+0.5)/(n.experim-e.experim+0.5))/((e.control+0.5)/(n.control-e.control+0.5)))
      Z <- ifelse( unfavourable==T, (estimate - NIm)/se, -(estimate - NIm)/se)
      p <- pnorm(Z)
      CI <- exp(c(estimate-qnorm(1-sig.level)*se,estimate+qnorm(1-sig.level)*se))
      estimate <- exp(estimate)
    } else if (test.type=="IndSmooth.logit") {
      test<-Independence_smoothed_logit_CI_2x2(t2x2, alpha = 2*sig.level)
      CI<-as.numeric(test[1:2])
      estimate<-as.numeric(test[3])
    } else if (test.type=="Cornfield.exact") {
      test<-Cornfield_exact_conditional_CI_2x2(t2x2, alpha = 2*sig.level)
      CI<-as.numeric(test[1:2])
      estimate<-as.numeric(test[3])
    } else if (test.type=="Baptista.Pike.exact") {
      test<-BaptistaPike_exact_conditional_CI_2x2(t2x2, alpha = 2*sig.level)
      CI<-as.numeric(test[1:2])
      estimate<-as.numeric(test[3])
    } else if (test.type=="Cornfield.midp") {
      test<-Cornfield_midP_CI_2x2(t2x2, alpha = 2*sig.level)
      CI<-as.numeric(test[1:2])
      estimate<-as.numeric(test[3])
    } else if (test.type=="Baptista.Pike.midp") {
      test<-BaptistaPike_midP_CI_2x2(t2x2, alpha = 2*sig.level)
      CI<-as.numeric(test[1:2])
      estimate<-as.numeric(test[3])
    } else if (test.type=="inverse.hyperbolic.sine") {
      test<-Inv_sinh_CI_OR_2x2(t2x2, alpha = 2*sig.level)
      CI<-as.numeric(test[1:2])
      estimate<-as.numeric(test[3])
    } else if (test.type=="Miettinen.Nurminen") {
      test<-MiettinenNurminen_asymptotic_score_CI_OR_2x2(t2x2, alpha = 2*sig.level)
      CI<-as.numeric(test[1:2])
      estimate<-as.numeric(test[3])
    } else if (test.type=="MOVER.R") {
      test<-MOVER_R_Wilson_CI_OR_2x2(t2x2, alpha = 2*sig.level)
      CI<-as.numeric(test[1:2])
      estimate<-as.numeric(test[3])
    } else if (test.type=="MOVER") {
      test<-moverci(e.experim,n.experim,e.control,n.control,contrast = "OR", level = 1-2*sig.level)
      CI<-as.numeric(test[c(1,3)])
      estimate<-as.numeric(test[2])
    } else if (test.type=="Gart.Nam") {
      test<-scasci(e.experim,n.experim,e.control,n.control,contrast = "OR", level = 1-2*sig.level)$estimates
      CI<-as.numeric(test[c(1,3)])
      estimate<-as.numeric(test[2])
    } else if (test.type=="score.cc") {
      test<-ratesci::scoreci(e.experim,n.experim,e.control,n.control,contrast = "OR", level = 1-2*sig.level, cc=T)$estimates
      CI<-as.numeric(test[c(1,3)])
      estimate<-as.numeric(test[2])
    }  else if (test.type=="MLE") {
      test<-OddsRatio(t2x2, method = "mle", conf.level = 1-sig.level*2)
      CI<-as.numeric(test[c(2,3)])
      estimate<-as.numeric(test[1])
    } else if ( test.type == "bootstrap") {
      if (e.control==0||e.experim==0) stop("bootstrap method not appropriate for zero cell counts (e.control=0 or e.experim=0).\n")
      oddsr <- function(dat, indices) {
        d <- dat[indices,] # allows boot to select sample
        or <- log((sum(d[d$treatment!=control.level,"outcomes"], na.rm=T)/(sum(d$treatment!=control.level, na.rm=T)-sum(d[d$treatment!=control.level,"outcomes"], na.rm=T)))/(sum(d[d$treatment==control.level,"outcomes"], na.rm=T)/(sum(d$treatment==control.level, na.rm=T)-sum(d[d$treatment==control.level,"outcomes"], na.rm=T))))
        return(or)
      } 
      
      res.b<-boot(mydata, oddsr, R=M.boot)
      CI<-exp(boot.ci(res.b, type=bootCI.type, conf=1-sig.level*2)[[4]][4:5-2*(bootCI.type=="norm")])
      estimate<-exp(res.b$t0)
    } else if (test.type == "Agresti.Min") {
      fit<-uncondExact2x2(e.control,n.control,e.experim,n.experim, method="score", tsmethod = "square", conf.int = T, parmtype="oddsratio")
      CI <- as.numeric(fit$conf.int)
      estimate<-as.numeric(fit$estimate)
    } else if (test.type == "Chan.Zhang") {
      fit<-uncondExact2x2(e.control,n.control,e.experim,n.experim, method="score", tsmethod = "central", conf.int = T, parmtype="oddsratio")
      CI <- as.numeric(fit$conf.int)
      estimate<-as.numeric(fit$estimate)
    } else if (test.type == "uncond.midp") {
      fit<-uncondExact2x2(e.control,n.control,e.experim,n.experim, method="score", tsmethod = "square", midp=T, conf.int = T, parmtype="oddsratio")
      CI <- as.numeric(fit$conf.int)
      estimate<-as.numeric(fit$estimate)
    } else if (test.type == "Berger.Boos") {
      fit<-uncondExact2x2(e.control,n.control,e.experim,n.experim, method="score", tsmethod = "square", gamma=BB.adj, conf.int = T, parmtype="oddsratio")
      CI <- as.numeric(fit$conf.int)
      estimate<-as.numeric(fit$estimate)
    } else if (test.type=="logistic") {
      if (e.experim==0||e.control==0) stop("With zero cell counts (e.control=0 or e.experim=0) logistic method not informative.\n")
      fit<-glm(myformula, data=mydata, family = binomial)
      row.treat<-which(grepl("treatment", row.names(coef(summary(fit)))))
      estimate<-exp(coef(summary(fit)))[row.treat,"Estimate"]
      CI <- exp(suppressMessages(confint(fit, level = 1-sig.level*2))[row.treat,])
    }  
    estimate.n<-mean(log(CI))
    if (is.null(se)) {
      se<-(log(CI[2])-log(CI[1]))/(2*qnorm(1-sig.level))
      is.se.est<-T
    } else {
      is.se.est<-F
    }

    if (isTRUE(recursive.p.estim)) {
      
      if (estimate==NI.margin) {
        p=0.5
      } else {
        sig.lev.est<-try(uniroot(func.to.opt, c(10^(-15), 0.5-10^(-15)), 
                                 n.control=n.control, n.experim=n.experim, 
                                 e.control=e.control, e.experim=e.experim,
                                 NI.margin=NI.margin, summary.measure=summary.measure, 
                                 unfavourable=unfavourable, test.type=test.type,
                                 M.boot=M.boot, BB.adj=BB.adj,
                                 tol = 0.000001)$root, silent=TRUE)
        if (!inherits(sig.lev.est, "try-error")) {
          p<-ifelse(((unfavourable==T)&&(estimate>NI.margin))||
                    ((unfavourable==F)&&(estimate<NI.margin)),
                  1-sig.lev.est, sig.lev.est)
        } else {
          p<-NULL
          warning("Recursive estimation of p failed. Providing standard estimate.\n")
        }
      }
      
      
    }
    if (is.null(p)) {
      Z <- ifelse(unfavourable==T,(estimate.n - NIm)/se,(-estimate.n + NIm)/se)
      p <- pnorm(Z)
      is.p.est<-T
      
    } else {
      is.p.est<-F
    }
    if ((unfavourable==T&&CI[2]<NI.margin)||(unfavourable==F&&CI[1]>NI.margin)) { 
      non.inferiority<-T
    } else {
      non.inferiority<-F
    }
    if (print.out==T) {
      cat("Testing for non-inferiority.\nSummary measure: Odds ratio.\nNon-inferiority margin = ", NI.margin, ".\nMethod: ",test.type,
          ".\nEstimate = ", estimate, 
          "\nConfidence interval (Two-sided ", (1-sig.level*2)*100,"%): (", CI[1], ",", CI[2], 
          ")\np-value = ", p, ".\n" , sep="")
      if ((unfavourable==T&&CI[2]<NI.margin)||(unfavourable==F&&CI[1]>NI.margin)) { 
        cat("The confidence interval does not cross the null ( OR = ", NI.margin, " ), and hence we have evidence of non-inferiority.\n", sep="")
      } else {
        cat("The confidence interval crosses the null ( OR = ", NI.margin, " ), and hence we have NO clear evidence of non-inferiority.\n", sep="")
      }
      if (is.p.est==T) {
        if (is.se.est==T) {
          cat("Note: with the test = ",test.type, " for summary measure = ", summary.measure,", p-value and standard error are only approximations based on a Z test with given logarithm of estimate and CI.\n")
        } else {
          cat("Note: with the test = ",test.type, " for summary measure = ", summary.measure,", p-value is only an approximation based on a Z test with given logarithm of estimate and CI.\n")
        }
      }
      
    }    
  }
  results <- list(estimate, se, p, CI, test.type, summary.measure, is.p.est, sig.level, non.inferiority)
  names(results)<-c("estimate", "se", "p", "CI", "test.type", "summary.measure", "is.p.est", "sig.level", "non.inferiority")
  return(results)
}