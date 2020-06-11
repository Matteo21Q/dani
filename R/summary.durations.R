summary.durations <- function (list.of.results) {
  
  cat("Family = ", list.of.results$family, "\n Model fit: \n")
  print(summary(list.of.results$model.fit))
  cat("Difference from control duration (", max(list.of.results$all.durations), "):\n")
  for (i in (length(list.of.results$all.durations)-1):1) {
    cat(list.of.results$all.durations[i]," vs ", max(list.of.results$all.durations), ":",
        p," (", 
        list.of.results$low.bounds.CI[i], ", ", list.of.results$up.bounds.CI[i], ")\n")
  }
  cat("Recommended duration with selected NI margin: ", list.of.results$min.duration, "\n")
  if (!is.null(list.of.results$min.dur.boot)) cat("Or, using Bootstrap Duration CI: ",
                                                  list.of.results$min.dur.boot, "\n")
}

  