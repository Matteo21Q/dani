plot.ROCI <- function (x, type="summary.measure", ylim=NULL, pch=15,
                                     xlab = "Treatment level", ylab=NULL, 
                                     lwd=3, ...) {
  stopifnot(type %in% c("tr.curve", "summary.measure"))
  NI.margin<-x$NI.margin
  x.treat<- x$treatment.levels
  max.treat<-max(x.treat)
  min.treat<-min(x.treat)
  x.treatall<-seq(min.treat,max.treat, length.out=100)
  y.treat.est<-predict(x$model.fit, 
                     newdata=data.frame(treatment=x.treat), 
                                                      type="resp")
  if (x$family=="binomial") {
    if (x$summary.measure=="RD") {
      acceptability<-y.treat.est[which(x.treat==x$reference)]+NI.margin
      experimental.arms<-x.treat[-which(x.treat==x$reference)]
    } else if (x$summary.measure=="RR") {
      acceptability<-y.treat.est[which(x.treat==x$reference)]*NI.margin
      experimental.arms<-x.treat[-which(x.treat==x$reference)]
    } else if (x$summary.measure=="target.risk") {
      acceptability<-NI.margin
      experimental.arms<-x.treat
    } else if (x$summary.measure=="OR") {
      odds.treat.est<-exp(predict(x$model.fit, 
                           newdata=data.frame(treatment=x.treat), 
                           type="link"))
      acceptability.odds<-odds.treat.est[which(x.treat==x$reference)]*NI.margin
      acceptability<-acceptability.odds/(1+acceptability.odds)
      experimental.arms<-x.treat[-which(x.treat==x$reference)]
    }
  } 
  
  
  if (type=="tr.curve") {
    
    if (is.null(ylim)) ylim=c(0,1)
    if (is.null(ylab)) ylab="Outcome risk"
    flag=t=1
    est.opt.treat<-x$optimal.treat
    est.opt.y<-y.treat.est[which(x.treat==est.opt.treat)]
    
     
    plot(x.treatall, predict(x$model.fit, newdata=data.frame(treatment=x.treatall), type="resp"), 
         xlim=c(min.treat,max.treat), 
          ylim=ylim, xlab = xlab, ylab=ylab, lwd=lwd,  
          xaxt="n", yaxt="n", type="l", ...)
    axis(side=1, at=x.treat, 
         labels=x.treat)
    axis(side=1, at=est.opt.treat, labels=est.opt.treat,
         col.axis="red", col.ticks = "red")
    axis(side=2, at=seq(ylim[1], ylim[2], length.out=11), 
         labels=paste(round(100*seq(ylim[1], ylim[2], length.out=11)),"%", sep=""), las=2)
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
                                            "Odds Ratio vs reference")) 
      plot(experimental.arms, x$estimates[-which(x.treat==x$reference)], xlim=c(min(x.treat), max(x.treat)), 
            xaxt="n", yaxt="n", type="p", ylim=ylim, pch=pch, xlab=xlab, ylab=ylab, ...)
    }
    
    lines(experimental.arms, NI.margin, col="red")
    
    
    axis(side=1, at=x$treatment.levels, labels=x$treatment.levels)
    axis(side=1, at=x$optimal.treat, labels=x$optimal.treat, col.axis="red", col.ticks = "red")
    if (x$summary.measure%in%c("RD", "target.risk")) {
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
