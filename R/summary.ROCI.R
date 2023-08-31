summary.ROCI <- function (object, ...) {
  
  type.p<-ifelse(object$family=="survival", "risk", "resp") 
  preds<-predict(object$model.fit, newdata=data.frame(treatment=object$treatment.levels), type=type.p)
  
  cat("Family = ", object$family, "\n Model fit: \n")
  print(summary(object$model.fit))
  if (object$summary.measure=="RD") {
    cat("Difference from control treatment level (", object$reference, "):\n")
    arms<-paste( object$reference," - ", object$treatment.levels )
    best.est<-object$estimates
  } else if (object$summary.measure=="RR") {
    cat("Risk Ratio against control duration (", object$reference, "):\n")
    arms<-paste( object$reference," / ", object$treatment.levels )
    best.est<-object$estimates
  } else if (object$summary.measure=="OR") {
    cat("Odds Ratio against control duration (", object$reference, "):\n")
    arms<-paste( object$reference," / ", object$treatment.levels )
    best.est<-object$estimates
  } else if (object$summary.measure=="target.risk") {
    cat("Estimated outcome probabilities:\n")
    arms<-paste( object$treatment.levels )
    best.est<-object$estimates
  } 

  for (i in (length(object$treatment.levels)):1) {
    cat(arms[i], ": ", round(best.est[i],3)  ,"(",
        round(object$low.bounds.CI[i],3), ", ", round(object$up.bounds.CI[i],3), ")\n")
  }
  cat("Recommended treatment level with selected NI margin: ", object$optimal.treat, ".\n")
  results<-data.frame(arms,best.est,object$low.bounds.CI,object$up.bounds.CI)
  opt.treat<-object$optimal.treat
  output<-list(results=results, opt.treat=opt.treat)
  invisible(output)
}

  