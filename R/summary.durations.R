summary.durations <- function (object, ...) {
  
  cat("Family = ", object$family, "\n Model fit: \n")
  print(summary(object$model.fit))
  cat("Difference from control duration (", max(object$all.durations), "):\n")
  for (i in (length(object$all.durations)-1):1) {
    cat(object$all.durations[i]," vs ", max(object$all.durations), ": (",
        object$low.bounds.CI[i], ", ", object$up.bounds.CI[i], ")\n")
  }
  cat("Recommended duration with selected NI margin: ", object$min.duration, "\n")
  if (!is.null(object$min.dur.boot)) cat("Or, using Bootstrap Duration CI: ",
                                                  object$min.dur.boot, "\n")
}

  