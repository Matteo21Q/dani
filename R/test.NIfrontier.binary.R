test.NIfrontier.binary <- function(n.control, n.experim, e.control, e.experim,  
                                    NI.frontier, sig.level, summary.measure="RD", 
                                    print.out=TRUE, unfavourable=TRUE, test.type=NULL,
                                    M.boot=2000, bootCI.type="bca", BB.adj=0.0001) {
  
  stopifnot(is.numeric(n.control), n.control>0)
  stopifnot(is.numeric(n.experim), n.experim>0)
  stopifnot(is.numeric(e.control), e.control>=0, n.control>=e.control)
  stopifnot(is.numeric(e.experim), e.experim>=0, n.experim>=e.experim)
  stopifnot(is.function(NI.frontier), length(formals(NI.frontier))==1)
  stopifnot((is.function(sig.level)&&length(formals(sig.level))==1)||(is.numeric(sig.level)&&sig.level < 0.5&&sig.level > 0))
  stopifnot(is.character(summary.measure),(( summary.measure == "RD" ) || ( summary.measure == "RR" ) || ( summary.measure == "OR" ) || ( summary.measure == "AS" )))
  stopifnot(is.logical(print.out), !is.na(print.out))
  stopifnot(is.logical(unfavourable), !is.na(unfavourable))
  stopifnot(is.numeric(M.boot), M.boot>1)
  stopifnot(is.character(bootCI.type), bootCI.type%in%c("norm","perc","bca","basic"))
  stopifnot(is.numeric(BB.adj), BB.adj>0)
  if (is.null(test.type)) {
    if (summary.measure=="RD") {
      test.type<-"Newcombe10"
    } else if (summary.measure=="RR") {
      test.type<-"Koopman"
    } else if (summary.measure=="OR") {
      test.type<-"Baptista.Pike.midp"
    } else {
      test.type<-"Wald"
    } 
  }
  stopifnot(is.character(test.type))
  if (summary.measure=="RD") {
    stopifnot(test.type%in%c("Wald", "Wald.cc", "Hauck.Anderson", "Gart.Nam",
                             "Newcombe10", "Newcombe11", "Haldane", "Jeffreys.Perks",
                             "Agresti.Caffo", "Miettinen.Nurminen", "Farrington.Manning",
                             "logistic", "binreg", "bootstrap", "Agresti.Min", "Brown.Li.Jeffreys", 
                             "Chan.Zhang", "BLNM", "Mee", "uncond.midp", "Berger.Boos",
                             "MUE.Lin", "MUE.parametric.bootstrap", "LRT"))
  } else if (summary.measure=="RR") {
    stopifnot(test.type%in%c("Wald.Katz", "adjusted.Wald.Katz", "inverse.hyperbolic.sine", "Koopman",
                             "MOVER.R", "Miettinen.Nurminen", "MOVER", "Gart.Nam", "score.cc",
                             "logregression", "logistic", "bootstrap", "Bailey", "Noether", 
                             "Chan.Zhang", "Agresti.Min", "uncond.midp", "Berger.Boos", "LRT"))
  } else if (summary.measure=="OR") {
    stopifnot(test.type%in%c("Wald.Woolf", "adjusted.Wald.Woolf", "inverse.hyperbolic.sine", "Cornfield.exact",
                             "MOVER.R", "Miettinen.Nurminen", "MOVER", "Gart.Nam", "score.cc",
                             "logistic", "bootstrap", "Cornfield.midp", "Baptista.Pike.exact", "Baptista.Pike.midp", 
                             "Chan.Zhang", "Agresti.Min", "uncond.midp", "Berger.Boos", "LRT"))
  } else {
    stopifnot(test.type%in%c("Wald","logistic", "LRT"))
  }
  
  estimate<-se<-Z<-p<-NULL
  
  p0.obs<-e.control/n.control
  NI.margin<-NI.frontier(p0.obs)
  if (is.function(sig.level)) {
    alpha<-sig.level(p0.obs) 
  } else{
    alpha<-sig.level
  }
  stopifnot(is.numeric(alpha), alpha < 0.5, alpha > 0)
  stopifnot(is.numeric(NI.margin))
  if (summary.measure=="RD") {
    if ((unfavourable == T)&&(NI.margin<=0)) stop("When outcome is unfavourable, a risk difference NI margin needs to be positive.\n")
    if ((unfavourable == F)&&(NI.margin>=0)) stop("When outcome is favourable, a risk difference NI margin needs to be negative.\n")
    if (NI.margin>=1) stop("NI.margin cannot be greater than 1, i.e. 100 percentage points, or otherwise the test is meaningless.\n ")
    if (NI.margin<=-1) stop("NI.margin cannot be lower than -1, i.e. -100 percentage points, or otherwise the test is meaningless.\n ")
  } else if (summary.measure=="RR") {
    if ((unfavourable == T)&&(NI.margin<=1)) stop("When outcome is unfavourable, a NI margin on the risk ratio scale needs to be >1.")
    if ((unfavourable == F)&&(NI.margin>=1)) stop("When outcome is favourable, a NI margin on the risk ratio scale needs to be <1.")
    if (NI.margin<=0) stop("A risk ratio margin must be >0.\n")
  } else if (summary.measure=="OR") {
    if ((unfavourable == T)&&(NI.margin<=1)) stop("When outcome is unfavourable, a NI margin on the odds ratio scale needs to be >1.")
    if ((unfavourable == F)&&(NI.margin>=1)) stop("When outcome is favourable, a NI margin on the odds ratio scale needs to be <1.")
    if (NI.margin<=0) stop("A odds ratio margin must be >0.\n")
  } else if (summary.measure=="AS") {
    if ((unfavourable == T)&&(NI.margin<=0)) stop("When outcome is unfavourable, a NI margin on the arc-sine difference scale needs to be >0.")
    if ((unfavourable == F)&&(NI.margin>=0)) stop("When outcome is favourable, a NI margin on the arc-sine difference scale needs to be <0.")
  }
  if (test.type!="LRT") {
    results<-test.NI.binary(n.control=n.control, n.experim=n.experim, e.control=e.control, e.experim=e.experim,  NI.margin=NI.margin, 
                            sig.level=alpha, summary.measure=summary.measure, 
                            print.out=print.out, unfavourable=unfavourable, test.type=test.type,
                            M.boot=M.boot, bootCI.type=bootCI.type, BB.adj=BB.adj)
  } else {
    
    p0.unconstr<-p0.constr<-p0.obs
    p1.unconstr<-p1.constr<-e.experim/n.experim
    
    if (summary.measure=="RD") {
      
      estimate<-p1.unconstr-p0.unconstr
      p1.func<-function(p0) {
        p1f<-min(p0+NI.frontier(p0),1)
        p1f<-max(0,p1f)
        return(p1f)
      }
      if (unfavourable==T) {
        logLik.c<-function(probabs) {
          p0<-probabs[1]
          p1<-probabs[2]
          if (p0<=10^(-100) ) p0=10^(-100) 
          if (p0>=1-10^(-10) ) p0=1-10^(-10) 
          if (p1<=10^(-100) ) p1=10^(-100) 
          if (p1>=1-10^(-10) ) p1=1-10^(-10) 
          if (p1<p0+NI.frontier(p0)) p1<-p0+NI.frontier(p0)
          l<-log(p0)*e.control+log(1-p0)*(n.control-e.control)+log(p1)*e.experim+log(1-p1)*(n.experim-e.experim)
          return(l)
        }
      } else {
        logLik.c<-function(probabs) {
          p0<-probabs[1]
          p1<-probabs[2]
          if (p1>p0+NI.frontier(p0)) p1<-p0+NI.frontier(p0)
          if (p0<=10^(-100) ) p0=10^(-100) 
          if (p0>=1-10^(-10) ) p0=1-10^(-10) 
          if (p1<=10^(-100) ) p1=10^(-100) 
          if (p1>=1-10^(-10) ) p1=1-10^(-10) 
          l<-log(p0)*e.control+log(1-p0)*(n.control-e.control)+log(p1)*e.experim+log(1-p1)*(n.experim-e.experim)
          return(l)
        }
      }
      ests.constr<-optim(c(p0.unconstr,p1.unconstr), logLik.c, control=list(fnscale=-1, reltol=1e-50))$par
      p0.constr<-ests.constr[1]
      if (unfavourable==TRUE) {
        p1.constr<-ifelse(ests.constr[2]<p0.constr+NI.frontier(p0.constr), p0.constr+NI.frontier(p0.constr), ests.constr[2])
      } else {
        p1.constr<-ifelse(ests.constr[2]>p0.constr+NI.frontier(p0.constr), p0.constr+NI.frontier(p0.constr), ests.constr[2])
      }
      
    } else if (summary.measure=="RR") {
      
      estimate<-p1.unconstr/p0.unconstr
      p1.func<-function(p0) {
        p1f<-min(p0*NI.frontier(p0),1)
        p1f<-max(0,p1f)
        return(p1f)
      }
      if (unfavourable==T) {
        logLik.c<-function(probabs) {
          p0<-probabs[1]
          p1<-probabs[2]
          if (p0<=10^(-100) ) p0=10^(-100) 
          if (p0>=1-10^(-10) ) p0=1-10^(-10) 
          if (p1<=10^(-100) ) p1=10^(-100) 
          if (p1>=1-10^(-10) ) p1=1-10^(-10) 
          if (p1<p0*NI.frontier(p0)) p1<-p0*NI.frontier(p0)
          l<-log(p0)*e.control+log(1-p0)*(n.control-e.control)+log(p1)*e.experim+log(1-p1)*(n.experim-e.experim)
          return(l)
        }
      } else {
        logLik.c<-function(probabs) {
          p0<-probabs[1]
          p1<-probabs[2]
          if (p0<=10^(-100) ) p0=10^(-100) 
          if (p0>=1-10^(-10) ) p0=1-10^(-10) 
          if (p1<=10^(-100) ) p1=10^(-100) 
          if (p1>=1-10^(-10) ) p1=1-10^(-10) 
          if (p1>p0*NI.frontier(p0)) p1<-p0*NI.frontier(p0)
          l<-log(p0)*e.control+log(1-p0)*(n.control-e.control)+log(p1)*e.experim+log(1-p1)*(n.experim-e.experim)
          return(l)
        }
      }
      
      ests.constr<-optim(c(p0.unconstr,p1.unconstr), logLik.c, control=list(fnscale=-1, reltol=1e-50))$par
      p0.constr<-ests.constr[1]
      if (unfavourable==TRUE) {
        p1.constr<-ifelse(ests.constr[2]<p0.constr*NI.frontier(p0.constr), p0.constr*NI.frontier(p0.constr), ests.constr[2])
      } else {
        p1.constr<-ifelse(ests.constr[2]>p0.constr*NI.frontier(p0.constr), p0.constr*NI.frontier(p0.constr), ests.constr[2])
      }
      
    } else if (summary.measure=="OR") {
      
      odds0<-p0.obs/(1-p0.obs)
      odds1<-p1.unconstr/(1-p1.unconstr)
      estimate<-odds1/odds0
      p1.func<-function(p0) {
        o0<-p0/(1-p0)
        o1<-o0*NI.frontier(p0)
        p1f<-min(o1/(1+o1),1)
        p1f<-max(0,p1f)
        return(p1f)
      }
      if (unfavourable==T) {
        logLik.c<-function(probabs) {
          p0<-probabs[1]
          p1<-probabs[2]
          if (p0<=10^(-100) ) p0=10^(-100) 
          if (p0>=1-10^(-10) ) p0=1-10^(-10) 
          if (p1<=10^(-100) ) p1=10^(-100) 
          if (p1>=1-10^(-10) ) p1=1-10^(-10) 
          o0<-p0/(1-p0)
          o1<-p1/(1-p1)
          if (o1<o0*NI.frontier(p0)) p1<-(o0*NI.frontier(p0))/(1+o0*NI.frontier(p0))
          l<-log(p0)*e.control+log(1-p0)*(n.control-e.control)+log(p1)*e.experim+log(1-p1)*(n.experim-e.experim)
          return(l)
        }
      } else {
        logLik.c<-function(probabs) {
          p0<-probabs[1]
          p1<-probabs[2]
          if (p0<=10^(-100) ) p0=10^(-100) 
          if (p0>=1-10^(-10) ) p0=1-10^(-10) 
          if (p1<=10^(-100) ) p1=10^(-100) 
          if (p1>=1-10^(-10) ) p1=1-10^(-10) 
          o0<-p0/(1-p0)
          o1<-p1/(1-p1)
          if (o1>o0*NI.frontier(p0)) p1<-(o0*NI.frontier(p0))/(1+o0*NI.frontier(p0))
          l<-log(p0)*e.control+log(1-p0)*(n.control-e.control)+log(p1)*e.experim+log(1-p1)*(n.experim-e.experim)
          return(l)
        }
      }
      
      ests.constr<-optim(c(p0.unconstr,p1.unconstr), logLik.c, control=list(fnscale=-1, reltol=1e-50))$par
      p0.constr<-ests.constr[1]
      if (unfavourable==TRUE) {
        p1.constr<-ifelse(ests.constr[2]/(1-ests.constr[2])<NI.frontier(p0.constr)*p0.constr/(1-p0.constr), (NI.frontier(p0.constr)*p0.constr/(1-p0.constr))/(1+NI.frontier(p0.constr)*p0.constr/(1-p0.constr)), ests.constr[2])
      } else {
        p1.constr<-ifelse(ests.constr[2]/(1-ests.constr[2])>NI.frontier(p0.constr)*p0.constr/(1-p0.constr), (NI.frontier(p0.constr)*p0.constr/(1-p0.constr))/(1+NI.frontier(p0.constr)*p0.constr/(1-p0.constr)), ests.constr[2])
      }
      
    } else if (summary.measure=="AS") {
      
      estimate<-asin(sqrt(p1.unconstr))-asin(sqrt(p0.unconstr))
      p1.func<-function(p0) {
        p1f<-min(sin(NI.frontier(p0)+asin(sqrt(p0)))^2,1)
        p1f<-max(0,p1f)
        return(p1f)
      }
      if (unfavourable==T) {
        logLik.c<-function(probabs) {
          p0<-probabs[1]
          p1<-probabs[2]
          if (p0<=10^(-100) ) p0=10^(-100) 
          if (p0>=1-10^(-10) ) p0=1-10^(-10) 
          if (p1<=10^(-100) ) p1=10^(-100) 
          if (p1>=1-10^(-10) ) p1=1-10^(-10) 
          if (p1<sin(NI.frontier(p0)+asin(sqrt(p0)))^2) p1<-sin(NI.frontier(p0)+asin(sqrt(p0)))^2
          l<-log(p0)*e.control+log(1-p0)*(n.control-e.control)+log(p1)*e.experim+log(1-p1)*(n.experim-e.experim)
          return(l)
        }
      } else {
        logLik.c<-function(probabs) {
          p0<-probabs[1]
          p1<-probabs[2]
          if (p0<=10^(-100) ) p0=10^(-100) 
          if (p0>=1-10^(-10) ) p0=1-10^(-10) 
          if (p1<=10^(-100) ) p1=10^(-100) 
          if (p1>=1-10^(-10) ) p1=1-10^(-10) 
          if (p1>sin(NI.frontier(p0)+asin(sqrt(p0)))^2) p1<-sin(NI.frontier(p0)+asin(sqrt(p0)))^2
          l<-log(p0)*e.control+log(1-p0)*(n.control-e.control)+log(p1)*e.experim+log(1-p1)*(n.experim-e.experim)
          return(l)
        }
      }
      
      ests.constr<-optim(c(p0.unconstr,p1.unconstr), logLik.c, control=list(fnscale=-1, reltol=1e-50))$par
      p0.constr<-ests.constr[1]
      if (unfavourable==TRUE) {
        p1.constr<-ifelse(ests.constr[2]<sin(NI.frontier(p0.constr)+asin(sqrt(p0.constr)))^2, sin(NI.frontier(p0.constr)+asin(sqrt(p0.constr)))^2, ests.constr[2])
      } else {
        p1.constr<-ifelse(ests.constr[2]>sin(NI.frontier(p0.constr)+asin(sqrt(p0.constr)))^2, sin(NI.frontier(p0.constr)+asin(sqrt(p0.constr)))^2, ests.constr[2])
      }
      
      
    }
    
    
    # Uncostrained likelihood
    logLik.u<-function(p0,p1) {
      l<-log(p0)*e.control+log(1-p0)*(n.control-e.control)+log(p1)*e.experim+log(1-p1)*(n.experim-e.experim)
      return(l)
    }
    
    max.logLik.c<-logLik.u(p0.constr,p1.constr)
    max.logLik.u<-logLik.u(p0.unconstr,p1.unconstr)
    ml.stat<-(-2*(max.logLik.c-max.logLik.u))
    
    p1.func.vec<-Vectorize(p1.func)
    dfs<-integrate(p1.func.vec,lower=0,upper=1)$value
    if (unfavourable==FALSE) dfs<-1-dfs
    p<-(1-pchisq(ml.stat,dfs))
    is.p.est<-FALSE
    non.inferiority<-ifelse(p<alpha, T, F)
    Z<-qnorm(1-p)
    if (summary.measure=="RD") {
      estimate<-p1.unconstr-p0.unconstr
      se <- abs(ifelse(unfavourable==T,(estimate - NI.margin)/Z,(-estimate + NI.margin)/Z))
      CI<-c(estimate-se*qnorm(1-alpha), estimate+se*qnorm(1-alpha))
    } else if (summary.measure=="RR") {
      estimate<-p1.unconstr/p0.unconstr
      se <- abs(ifelse(unfavourable==T,(log(estimate) - log(NI.margin))/Z,(-log(estimate) + log(NI.margin))/Z))
      CI<-exp(c(log(estimate)-se*qnorm(1-alpha), log(estimate)+se*qnorm(1-alpha)))
    } else if (summary.measure=="OR") {
      estimate<-odds1/odds0
      se <- abs(ifelse(unfavourable==T,(log(estimate) - log(NI.margin))/Z,(-log(estimate) + log(NI.margin))/Z))
      CI<-exp(c(log(estimate)-se*qnorm(1-alpha), log(estimate)+se*qnorm(1-alpha)))
      
    } else if (summary.measure=="AS") {
      estimate<-asin(sqrt(p1.unconstr))-asin(sqrt(p0.unconstr))
      se <- abs(ifelse(unfavourable==T,(estimate - NI.margin)/Z,(-estimate + NI.margin)/Z))
      CI<-c(estimate-se*qnorm(1-alpha), estimate+se*qnorm(1-alpha))
    }
    results <- list(estimate, se, p, CI, test.type, summary.measure, is.p.est, alpha, non.inferiority)
    names(results)<-c("estimate", "se", "p", "CI", "test.type", "summary.measure", "is.p.est", "sig.level", "non.inferiority")
    non.inferiority<-ifelse(p<alpha,T,F)
    if (print.out==T) {
      cat("Testing for non-inferiority.\nSummary measure: ",summary.measure, ".\nNon-inferiority margin = ", NI.margin, ".\nMethod: ",test.type,
          ".\nEstimate = ", estimate, 
          "\nConfidence interval (Two-sided ", (1-alpha*2)*100,"%): (", CI[1], ",", CI[2], 
          ")\np-value = ", p, ".\n" , sep="")
      if (p<alpha) { 
        non.inferiority<-T
        cat("p<alpha = ", alpha, " ), and hence we have evidence of non-inferiority.\n", sep="")
      } else {
        non.inferiority<-F
        cat("p>=alpha = ", alpha, ", and hence we have NO clear evidence of non-inferiority.\n", sep="")
      }
      
      cat("Note: with the test = ",test.type, " the confidence interval is test-based.\n")
      
    }
  }
  
  
  return(results)
}