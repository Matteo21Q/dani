plot.durations <- function (list.of.results, ylim=NULL, 
                                     xlab = "Duration", ylab=NULL, 
                                     lwd=3, main="Duration-Response Curve", 
                                     xaxt="n", yaxt="n", type="outcome", 
                                     predict.target=NULL) {
  if(is.null(predict.target)){
    predict.target<-ifelse(list.of.results$family=="survival", "risk", "resp")
  }
  if (list.of.results$se.method=="delta"&type=="duration") {
    type="outcome"
    cat("Only type=outcome available for results from delta-method.\n")
  }
  x.dur<- seq(min(list.of.results$all.durations),
      max(list.of.results$all.durations), 
      length.out = 100)
  y.dur.est<-predict(list.of.results$model.fit, 
                     newdata=data.frame(durations=x.dur), 
                                                      type=predict.target)
  max.dur<-max(list.of.results$all.durations)
  if (list.of.results$family=="binomial") {
    if (list.of.results$scale=="RD") {
      acceptability<- function(x) {
        return(0*x+y.dur.est[length(y.dur.est)]-list.of.results$NI.margin)
      }
    } else if (list.of.results$scale=="RR") {
      acceptability<- function(x) {
        return(0*x+y.dur.est[length(y.dur.est)]*list.of.results$NI.margin)
      }
    } else if (list.of.results$scale=="rate") {
      acceptability<- function(x) {
        return(0*x+list.of.results$NI.margin)
      }
    } else if (list.of.results$scale=="AF") {
      acceptability<- function(x) {
        return(y.dur.est[length(y.dur.est)]-list.of.results$NI.margin(x))
      }
    }
  } else if (list.of.results$family=="gaussian") {
    if (list.of.results$scale=="diff") {
      acceptability<- function(x) {
        return(0*x+y.dur.est[length(y.dur.est)]-list.of.results$NI.margin)
      }
    } else if (list.of.results$scale=="ratio") {
      acceptability<- function(x) {
        return(0*x+y.dur.est[length(y.dur.est)]*list.of.results$NI.margin)
      }
    } else if (list.of.results$scale=="target") {
      acceptability<- function(x) {
        return(0*x+list.of.results$NI.margin)
      }
    } else if (list.of.results$scale=="AF") {
      acceptability<- function(x) {
        return(y.dur.est[length(y.dur.est)]-list.of.results$NI.margin(x))
      }
    }
  } else if (list.of.results$family=="survival") {
      if (list.of.results$scale=="HR") {
      acceptability<- function(x) {
        return(0*x+y.dur.est[length(y.dur.est)]*list.of.results$NI.margin)
      }
    } else if (list.of.results$scale=="AF") {
      acceptability<- function(x) {
        return(y.dur.est[length(y.dur.est)]*list.of.results$NI.margin(x))
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
    
     
    curve(predict(list.of.results$model.fit, newdata=data.frame(durations=x), type=predict.target), 
          xlim=c(min(list.of.results$all.durations),max(list.of.results$all.durations)), 
          ylim=ylim, xlab = xlab, ylab=ylab, lwd=lwd, main=main, 
          xaxt=xaxt)
    axis(side=1, at=list.of.results$all.durations, 
         labels=list.of.results$all.durations)
    axis(side=1, at=list.of.results$min.dur.boot, labels=list.of.results$min.dur.boot,
         col.axis="red", col.ticks = "red")
    curve(acceptability(x), add=T, col="red")
    segments(list.of.results$low.bound.dur,ylim[1], 
             list.of.results$up.bound.dur, ylim[1], lwd=2)
    segments(list.of.results$up.bound.dur,ylim[1], 
             list.of.results$min.dur.boot, ylim[1],
             lwd=1, col="red", lty=2)
    points(list.of.results$min.dur.boot, ylim[1],
           col="red", pch=8)
    segments(list.of.results$min.dur.boot, ylim[1],
             list.of.results$min.dur.boot, 0, lwd=1, col="red", lty=2)
    segments(list.of.results$low.bound.dur,ylim[1]-0.01,
             list.of.results$low.bound.dur,ylim[1]+0.01, lwd=2)
    segments(list.of.results$up.bound.dur,ylim[1]-0.01,
             list.of.results$up.bound.dur,ylim[1]+0.01, lwd=2)
    segments(est.opt.dur,ylim[1]-0.005,
             est.opt.dur,ylim[1]+0.005, lwd=2)
    segments(est.opt.dur,est.opt.cure,
             est.opt.dur,ylim[1]+0.005, lwd=1, col="grey", lty=2)
    
  } else if (type=="outcome") {
    
    if (is.null(ylab)&list.of.results$family=="binomial") ylab="% events"
    if (is.null(ylab)&list.of.results$family=="gaussian") ylab="Outcome"
    if (is.null(ylab)&list.of.results$family=="survival") ylab="HR vs control"
    if (is.null(ylim)) ylim=c(min(list.of.results$low.bounds.CI)-0.05*abs(min(list.of.results$low.bounds.CI)),
                              max(list.of.results$up.bounds.CI)+0.05*abs(max(list.of.results$up.bounds.CI)))
    if (list.of.results$scale=="AF"){
      NI.marg<-list.of.results$NI.margin
      curve(NI.marg, xlim=c(min(x.dur), max(x.dur)), ylim=ylim,
            xaxt="n", yaxt="n", xlab = "Duration", ylab=ylab, 
            lty=1, col="red", main="Difference from longest duration")
    } else {
      curve(rep(list.of.results$NI.margin, length(x)), xlim=c(min(x.dur), max(x.dur)), ylim=ylim,
            xaxt="n", yaxt="n", xlab = "Duration", ylab=ylab, 
            lty=1, col="red", main="Difference from longest duration")
    }
    
    axis(side=1, at=list.of.results$all.durations, labels=list.of.results$all.durations)
    axis(side=1, at=list.of.results$min.duration, labels=list.of.results$min.duration, col.axis="red", col.ticks = "red")
    if (list.of.results$family=="binomial") {
      axis(side=2, at=seq(round(ylim[1]*100),round(ylim[2]*100),length.out = 5)/100, seq(round(ylim[1]*100),round(ylim[2]*100),length.out = 5))
      
    } else {
      axis(side=2, at=seq(round(ylim[1]),round(ylim[2]),length.out = 5), seq(round(ylim[1]),round(ylim[2]),length.out = 5))
      
    }
    
    n.dur<-length(list.of.results$all.durations)
    poss.durations<-list.of.results$all.durations[-n.dur]
    for (d in (1:(n.dur-1))) {
      if (d==which(list.of.results$all.durations==list.of.results$min.duration)) color.plot<-"red" else color.plot<-"black"
      segments(poss.durations[d]-0.1,list.of.results$low.bounds.CI[d], poss.durations[d]+0.1, list.of.results$low.bounds.CI[d], lwd=2, col=color.plot)
      segments(poss.durations[d]-0.1,list.of.results$up.bounds.CI[d],poss.durations[d]+0.1,list.of.results$up.bounds.CI[d], lwd=2, col=color.plot)
      segments(poss.durations[d],list.of.results$up.bounds.CI[d],poss.durations[d],list.of.results$low.bounds.CI[d], lwd=2, col=color.plot)
    }

  }

}
