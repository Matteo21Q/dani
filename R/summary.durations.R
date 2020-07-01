summary.durations <- function (object, ...) {
  
  type.p<-ifelse(object$family=="survival", "risk", "resp") 
  preds<-predict(object$model.fit, newdata=data.frame(durations=object$all.durations), type=type.p)
  
  
  cat("Family = ", object$family, "\n Model fit: \n")
  print(summary(object$model.fit))
  if (object$scale=="RD"|object$scale=="diff"|
      (object$family!="survival"&object$scale=="AF")) {
    cat("Difference from control duration (", max(object$all.durations), "):\n")
    arms<-paste( max(object$all.durations)," - ", object$all.durations )
    best.est<-preds[length(preds)]-preds
  } else if (object$scale=="RR") {
    cat("Risk Ratio against control duration (", max(object$all.durations), "):\n")
    arms<-paste( object$all.durations," / ", max(object$all.durations) )
    best.est<-preds/preds[length(preds)]
  } else if (object$scale=="HR"|
             (object$family=="survival"&object$scale=="AF")) {
    cat("Hazard Ratio against control duration (", max(object$all.durations), "):\n")
    arms<-paste(  object$all.durations ," / ", max(object$all.durations))
    best.est<-preds/preds[length(preds)]
    
  } else if (object$scale=="ratio") {
    cat("Ratio against control duration (", max(object$all.durations), "):\n")
    arms<-paste(  object$all.durations," / ", max(object$all.durations) )
    best.est<-preds/preds[length(preds)]
  } else if (object$scale=="rate") {
    cat("Estimated event rate:\n")
    arms<-paste( object$all.durations )
    best.est<-preds
  } else if (object$scale=="target") {
    cat("Estimated mean outcome:\n")
    arms<-paste( object$all.durations )
    best.est<-preds
  } 

  for (i in (length(object$all.durations)):1) {
    cat(arms[i], ": ", round(best.est[i],3)  ,"(",
        round(object$low.bounds.CI[i],3), ", ", round(object$up.bounds.CI[i],3), ")\n")
  }
  cat("Recommended duration with selected NI margin: ", object$min.duration, "\n")
  if (!is.null(object$min.dur.boot)) cat("Or, using Bootstrap Duration CI: ",
                                                  object$min.dur.boot, "\n")
  results<-data.frame(arms,best.est,object$low.bounds.CI,object$up.bounds.CI)
  opt.dur<-object$min.dur.boot
  output<-list(results=results, opt.dur=opt.dur)
  invisible(output)
}

  