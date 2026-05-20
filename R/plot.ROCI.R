plot.ROCI <- function (x, type="summary.measure", ylim=NULL, pch=15,
                                     xlab = "Treatment level", ylab=NULL, 
                                     lwd=3, ...) {
  stopifnot(type %in% c("tr.curve", "summary.measure"))
  NI.margin<-x$NI.margin
  x.treat<- x$treatment.levels
  max.treat<-max(x.treat)
  min.treat<-min(x.treat)
  x.treatall<-seq(min.treat,max.treat, length.out=100)

  
  if (x$family=="binomial") {
    
    y.treat.est<-try(predict(x$model.fit, 
                             newdata=data.frame(treatment=x.treat), 
                             type="resp"),
                     silent=T)
    if (inherits(y.treat.est, "try-error")&&type=="tr.curve") {
      stop("In presence of covariates, only the summary measure plot is currently available.\n")
    }
    
    if (x$summary.measure=="RD") {
      acceptability<-try(y.treat.est[which(x.treat==x$reference)]+NI.margin, silent=TRUE)
      experimental.arms<-x.treat[-which(x.treat==x$reference)]
    } else if (x$summary.measure=="AS") {
      acceptability<-try(sin(NI.margin+asin(sqrt(y.treat.est[which(x.treat==x$reference)])))^2, silent=TRUE)
      experimental.arms<-x.treat[-which(x.treat==x$reference)]
    } else if (x$summary.measure=="RR") {
      acceptability<-try(y.treat.est[which(x.treat==x$reference)]*NI.margin, silent=TRUE)
      experimental.arms<-x.treat[-which(x.treat==x$reference)]
    } else if (x$summary.measure=="target.risk") {
      acceptability<-NI.margin
      experimental.arms<-x.treat
    } else if (x$summary.measure=="OR") {
      odds.treat.est<-try(exp(predict(x$model.fit, 
                           newdata=data.frame(treatment=x.treat), 
                           type="link")), silent=TRUE)
      acceptability.odds<-try(odds.treat.est[which(x.treat==x$reference)]*NI.margin, silent=TRUE)
      acceptability<-try(acceptability.odds/(1+acceptability.odds), silent=TRUE)
      experimental.arms<-x.treat[-which(x.treat==x$reference)]
    }
    
  } else if (x$family=="survival") {
    
    
    if (x$summary.measure=="DS") {
      
      y.treat.est<-try(predict(x$model.fit, 
                               newdata=data.frame(treatment=x.treat), 
                               type="survival", times=x$tau)$.pred_survival,
                       silent=T)
      
      acceptability<-try(y.treat.est[which(x.treat==x$reference)]+NI.margin, silent=TRUE)
      experimental.arms<-x.treat[-which(x.treat==x$reference)]
      
    } else if (x$summary.measure=="DRMST") {
      
      y.treat.est<-try(predict(x$model.fit, 
                               newdata=data.frame(treatment=x.treat), 
                               type="rmst", times=x$tau)$.pred_rmst,
                       silent=T)
      
      acceptability<-try(y.treat.est[which(x.treat==x$reference)]+NI.margin, silent=TRUE)
      experimental.arms<-x.treat[-which(x.treat==x$reference)]
      
    } else if (x$summary.measure=="RS") {
      
      y.treat.est<-try(predict(x$model.fit, 
                               newdata=data.frame(treatment=x.treat), 
                               type="survival", times=x$tau)$.pred_survival,
                       silent=T)
      
      acceptability<-try(y.treat.est[which(x.treat==x$reference)]*NI.margin, silent=TRUE)
      experimental.arms<-x.treat[-which(x.treat==x$reference)]
      
    } else if (x$summary.measure=="HR") {
      
      y.treat.est<-try(predict(x$model.fit, 
                               newdata=data.frame(treatment=x.treat), 
                               type="hazard", times=x$tau)$.pred_hazard,
                       silent=T)
      
      acceptability<-try(y.treat.est[which(x.treat==x$reference)]*NI.margin, silent=TRUE)
      experimental.arms<-x.treat[-which(x.treat==x$reference)]
      
    }
    
    
  }
  
  
  if (type=="tr.curve") {
    
    if (x$family=="binomial") {
      
      if (is.null(ylim)) ylim=c(0,1)
      if (is.null(ylab)) ylab="Outcome risk"
      
    } else if (x$family=="survival") {
      
      if (x$summary.measure=="DS"||x$summary.measure=="RS") {
        
        if (is.null(ylim)) ylim=c(0,1)
        if (is.null(ylab)) ylab="Survival probability"
        
      } else if (x$summary.measure=="DRMST") {
        
        if (is.null(ylim)) ylim=c(min(y.treat.est)*0.9,max(y.treat.est)*1.1)
        if (is.null(ylab)) ylab="RMST"
        
      } else if (x$summary.measure=="HR") {
        
        if (is.null(ylim)) ylim=c(min(y.treat.est)*0.9,max(y.treat.est)*1.1)
        if (is.null(ylab)) ylab="Hazard"
        
      }
      
      
    }
    
    flag=t=1
    est.opt.treat<-x$optimal.treat
    est.opt.y<-y.treat.est[which(x.treat==est.opt.treat)]
    
    if (x$family=="binomial") {
      
      y.treatall<-predict(x$model.fit, newdata=data.frame(treatment=x.treatall), 
                          type="resp")
      labs<-paste(round(100*seq(ylim[1], ylim[2], length.out=11)),"%", sep="")
      
    } else if (x$family=="survival") {
      
      if (x$summary.measure%in%c("RS", "DS")) {
        
        y.treatall<-predict(x$model.fit, newdata=data.frame(treatment=x.treatall), 
                            type="survival", times=x$tau)$.pred_survival
        labs<-paste(round(100*seq(ylim[1], ylim[2], length.out=11)),"%", sep="")
        
        
      } else if (x$summary.measure=="DRMST") {
        
        y.treatall<-predict(x$model.fit, newdata=data.frame(treatment=x.treatall), 
                            type="rmst", times=x$tau)$.pred_rmst
        labs<-paste(round(seq(ylim[1], ylim[2], length.out=11), digits=1), sep="")
        
        
        
      } else if (x$summary.measure=="HR") {
        
        y.treatall<-predict(x$model.fit, newdata=data.frame(treatment=x.treatall), 
                            type="hazard", times=x$tau)$.pred_hazard
        labs<-paste(round(seq(ylim[1], ylim[2], length.out=11), digits = 3), sep="")
        
        
      }
      
    }
    
     
    plot(x.treatall, y.treatall, 
         xlim=c(min.treat,max.treat), 
          ylim=ylim, xlab = xlab, ylab=ylab, lwd=lwd,  
          xaxt="n", yaxt="n", type="l", ...)
    axis(side=1, at=x.treat, 
         labels=x.treat)
    axis(side=1, at=est.opt.treat, labels=est.opt.treat,
         col.axis="red", col.ticks = "red")
    axis(side=2, at=seq(ylim[1], ylim[2], length.out=11), 
         labels=labs, las=2)
    lines(experimental.arms,acceptability, type="l", col="red")
    segments(est.opt.treat,est.opt.y,
             est.opt.treat,ylim[1]+0.005, lwd=1, col="grey", lty=2)
    points(x$optimal.treat, est.opt.y,
           col="red", pch=8)
    
    
  } else if (type=="summary.measure") {
    
    if (is.null(ylim)) ylim=c(min(x$low.bounds.CI, na.rm = TRUE),max(x$up.bounds.CI, na.rm = TRUE))
    
    if (x$summary.measure=="target.risk") {
      if (is.null(ylab)) ylab="Outcome risk"
      plot(x.treatall, predict(x$model.fit, newdata=data.frame(treatment=x.treatall), type="resp"), xlim=c(min.treat,max.treat), ylim=ylim,
            xaxt="n", yaxt="n", xlab = xlab, ylab=ylab, type="l", pch=pch, ...)
    } else {
      if (is.null(ylab)) ylab=ifelse(x$summary.measure=="RD", "Risk Difference vs reference", 
                                     ifelse(x$summary.measure=="RR", "Risk Ratio vs reference",
                                            ifelse(x$summary.measure=="AS", "Arc-sine difference vs reference",
                                            ifelse(x$summary.measure=="OR", "Odds Ratio vs reference",
                                            ifelse(x$summary.measure=="DS", "Difference in survival vs reference",
                                            ifelse(x$summary.measure=="DRMST", "Difference in RMST vs reference",
                                            ifelse(x$summary.measure=="RS", "Ratio of survival vs reference",
                                            "Hazard ratio vs reference"))))))) 
      plot(experimental.arms, x$estimates[-which(x.treat==x$reference)], xlim=c(min(x.treat), max(x.treat)), 
            xaxt="n", yaxt="n", type="p", ylim=ylim, pch=pch, xlab=xlab, ylab=ylab, ...)
    }
    
    lines(experimental.arms, NI.margin, col="red")
    
    
    axis(side=1, at=x$treatment.levels, labels=x$treatment.levels)
    axis(side=1, at=x$optimal.treat, labels=x$optimal.treat, col.axis="red", col.ticks = "red")
    if (x$summary.measure%in%c("RD", "target.risk", "DS")) {
      axis(side=2, at=seq(ylim[1], ylim[2], length.out=11), 
           labels=paste(round(100*seq(ylim[1], ylim[2], length.out=11)),"%", sep=""), las=2)      
    } else {
      axis(side=2, at=seq(ylim[1], ylim[2], length.out=6), 
           labels=round(seq(ylim[1], ylim[2], length.out=6), digits=2), las=2)      
    }
    
    n.treat<-length(experimental.arms)
    for (d in (1:n.treat)) {
      if (d==which(experimental.arms==x$optimal.treat)) color.plot<-"red" else color.plot<-"black"
      segments(experimental.arms[d]-0.1,x$low.bounds.CI[d], experimental.arms[d]+0.1, x$low.bounds.CI[d], lwd=2, col=color.plot)
      segments(experimental.arms[d]-0.1,x$up.bounds.CI[d],experimental.arms[d]+0.1,x$up.bounds.CI[d], lwd=2, col=color.plot)
      segments(experimental.arms[d],x$up.bounds.CI[d],experimental.arms[d],x$low.bounds.CI[d], lwd=2, col=color.plot)
      points(experimental.arms[d], x$estimates[d], pch=pch, col=color.plot)
    }

  }

}
