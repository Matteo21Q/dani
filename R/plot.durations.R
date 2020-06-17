plot.durations <- function (x, ylim=NULL, 
                                     xlab = "Duration", ylab=NULL, 
                                     lwd=3, main="Duration-Response Curve", 
                                     xaxt="n", yaxt="n", type="outcome", 
                                     predict.target=NULL, ...) {
  if(is.null(predict.target)){
    predict.target<-ifelse(x$family=="survival", "risk", "resp")
  }
  if (x$se.method=="delta"&type=="duration") {
    type="outcome"
    cat("Only type=outcome available for results from delta-method.\n")
  }
  x.dur<- seq(min(x$all.durations),
      max(x$all.durations), 
      length.out = 100)
  y.dur.est<-predict(x$model.fit, 
                     newdata=data.frame(durations=x.dur), 
                                                      type=predict.target)
  max.dur<-max(x$all.durations)
  if (x$family=="binomial") {
    if (x$scale=="RD") {
      acceptability<- function(x) {
        return(0*x+y.dur.est[length(y.dur.est)]-x$NI.margin)
      }
    } else if (x$scale=="RR") {
      acceptability<- function(x) {
        return(0*x+y.dur.est[length(y.dur.est)]*x$NI.margin)
      }
    } else if (x$scale=="rate") {
      acceptability<- function(x) {
        return(0*x+x$NI.margin)
      }
    } else if (x$scale=="AF") {
      acceptability<- function(x) {
        return(y.dur.est[length(y.dur.est)]-x$NI.margin(x))
      }
    }
  } else if (x$family=="gaussian") {
    if (x$scale=="diff") {
      acceptability<- function(x) {
        return(0*x+y.dur.est[length(y.dur.est)]-x$NI.margin)
      }
    } else if (x$scale=="ratio") {
      acceptability<- function(x) {
        return(0*x+y.dur.est[length(y.dur.est)]*x$NI.margin)
      }
    } else if (x$scale=="target") {
      acceptability<- function(x) {
        return(0*x+x$NI.margin)
      }
    } else if (x$scale=="AF") {
      acceptability<- function(x) {
        return(y.dur.est[length(y.dur.est)]-x$NI.margin(x))
      }
    }
  } else if (x$family=="survival") {
      if (x$scale=="HR") {
      acceptability<- function(x) {
        return(0*x+y.dur.est[length(y.dur.est)]*x$NI.margin)
      }
    } else if (x$scale=="AF") {
      acceptability<- function(x) {
        return(y.dur.est[length(y.dur.est)]*x$NI.margin(x))
      }
    }
  }
  
  
  if (type=="duration") {
    
    if (is.null(ylim)) ylim=c(min(y.dur.est)-0.05*abs(min(y.dur.est)),
                              max(y.dur.est)+0.005*abs(max(y.dur.est)))
    
    flag=t=1
    est.opt.dur<-max.dur
    while ((t<length(x.dur))&(flag==1)) {
      if ((y.dur.est[t]-acceptability(x.dur[t]))>0) {
        flag=0
        est.opt.dur<-x.dur[t]
        est.opt.cure<-y.dur.est[t]
      }
      t=t+1
    }
    
     
    plot(x.dur, predict(x$model.fit, newdata=data.frame(durations=x.dur), type=predict.target), 
         xlim=c(min(x$all.durations),max(x$all.durations)), 
          ylim=ylim, xlab = xlab, ylab=ylab, lwd=lwd, main=main, 
          xaxt=xaxt, type="l", ...)
    axis(side=1, at=x$all.durations, 
         labels=x$all.durations)
    axis(side=1, at=x$min.dur.boot, labels=x$min.dur.boot,
         col.axis="red", col.ticks = "red")
    plot(x.dur,acceptability(x.dur), type="l", add=T, col="red")
    segments(x$low.bound.dur,ylim[1], 
             x$up.bound.dur, ylim[1], lwd=2)
    segments(x$up.bound.dur,ylim[1], 
             x$min.dur.boot, ylim[1],
             lwd=1, col="red", lty=2)
    points(x$min.dur.boot, ylim[1],
           col="red", pch=8)
    segments(x$min.dur.boot, ylim[1],
             x$min.dur.boot, 0, lwd=1, col="red", lty=2)
    segments(x$low.bound.dur,ylim[1]-0.01,
             x$low.bound.dur,ylim[1]+0.01, lwd=2)
    segments(x$up.bound.dur,ylim[1]-0.01,
             x$up.bound.dur,ylim[1]+0.01, lwd=2)
    segments(est.opt.dur,ylim[1]-0.005,
             est.opt.dur,ylim[1]+0.005, lwd=2)
    segments(est.opt.dur,est.opt.cure,
             est.opt.dur,ylim[1]+0.005, lwd=1, col="grey", lty=2)
    
  } else if (type=="outcome") {
    
    if (is.null(ylab)&x$family=="binomial") ylab="% events"
    if (is.null(ylab)&x$family=="gaussian") ylab="Outcome"
    if (is.null(ylab)&x$family=="survival") ylab="HR vs control"
    if (is.null(ylim)) ylim=c(min(x$low.bounds.CI)-0.05*abs(min(x$low.bounds.CI)),
                              max(x$up.bounds.CI)+0.05*abs(max(x$up.bounds.CI)))
    if (x$scale=="AF"){
      NI.marg<-x$NI.margin
      plot(x.dur,NI.marg(x.dur), xlim=c(min(x.dur), max(x.dur)), ylim=ylim,
            xaxt="n", yaxt="n", xlab = "Duration", ylab=ylab, type="l", 
            lty=1, col="red", main="Difference from longest duration", ...)
    } else {
      plot(NULL, xlim=c(min(x.dur), max(x.dur)), ylim=ylim,
            xaxt="n", yaxt="n", xlab = "Duration", ylab=ylab, 
            lty=1, col="red", main="Difference from longest duration", ...)
      abline(h=x$NI.margin, col="red")
    }
    
    axis(side=1, at=x$all.durations, labels=x$all.durations)
    axis(side=1, at=x$min.duration, labels=x$min.duration, col.axis="red", col.ticks = "red")
    if (x$family=="binomial") {
      axis(side=2, at=seq(round(ylim[1]*100),round(ylim[2]*100),length.out = 5)/100, seq(round(ylim[1]*100),round(ylim[2]*100),length.out = 5))
      
    } else {
      axis(side=2, at=seq(round(ylim[1]),round(ylim[2]),length.out = 5), seq(round(ylim[1]),round(ylim[2]),length.out = 5))
      
    }
    
    n.dur<-length(x$all.durations)
    poss.durations<-x$all.durations[-n.dur]
    for (d in (1:(n.dur-1))) {
      if (d==which(x$all.durations==x$min.duration)) color.plot<-"red" else color.plot<-"black"
      segments(poss.durations[d]-0.1,x$low.bounds.CI[d], poss.durations[d]+0.1, x$low.bounds.CI[d], lwd=2, col=color.plot)
      segments(poss.durations[d]-0.1,x$up.bounds.CI[d],poss.durations[d]+0.1,x$up.bounds.CI[d], lwd=2, col=color.plot)
      segments(poss.durations[d],x$up.bounds.CI[d],poss.durations[d],x$low.bounds.CI[d], lwd=2, col=color.plot)
    }

  }

}
