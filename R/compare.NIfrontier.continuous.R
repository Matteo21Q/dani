compare.NIfrontier.continuous<-function(mean.control.expected, mean.experim.target=NULL, mean.range, NI.margin, summary.measure="difference") {
  
  if (!is.numeric(mean.control.expected)||(!is.numeric(mean.experim.target)&&!is.null(mean.experim.target))||!is.numeric(NI.margin)) stop ("mean.control.expected, mean.experim.target and NI.margin should all be numeric.\n")
  if ((summary.measure!="difference")&&(summary.measure!="ratio")) stop("summary.measure should be one of either difference or ratio.\n")
  if ((!is.vector(mean.range))||(!is.numeric(mean.range))||(length(mean.range)!=2)) stop("mean.range should be a numeric vector of length 2.\n")
  if (mean.range[2]<mean.range[1]) {
    mean.range<-mean.range[c(2,1)]
  }
  if (mean.range[1]==mean.range[2]) stop("This function should be used to evaluate frontiers. If the range is a single point, then any summary measure will have same power if the margins are matched.")
  if ((mean.control.expected>mean.range[2])||(mean.control.expected<mean.range[1])) stop("Range of means should include the expected value.\n")
  if ((mean.control.expected==0)) stop("The ratio of means is not an appropriate summary measure when the expected control mean is 0. Hence only the difference in means is available.\n")
  if ((NI.margin==0)&&(summary.measure=="difference")) stop ("A Non-inferiority margin of 0 for the mean difference means this is a superiority trial. Hence, all summary measures will have same power.")
  if ((NI.margin==1)&&(summary.measure=="ratio")) stop ("A Non-inferiority margin of 1 for the mean ratio means this is a superiority trial. Hence, all summary measures will have same power.")

  ratio.margin<-convertmargin.continuous(mean.control.expected, NI.margin, summary.measure, "ratio")
  difference.margin<-convertmargin.continuous(mean.control.expected, NI.margin, summary.measure, "difference")
  
  mean.experim.null<-difference.margin+mean.control.expected
  
  if (is.null(mean.experim.target)) mean.experim.target<-mean.control.expected
  if (!(((mean.experim.target>mean.experim.null)&&(mean.control.expected>=mean.experim.null))||((mean.experim.target<mean.experim.null)&&(mean.control.expected<=mean.experim.null)))) stop ("The alternative hypothesis does not imply non-inferiority of the experimental treatment")
  
  length.vec<-100
  means.vec<-seq(mean.range[1], mean.range[2], length.out=length.vec)
  experim.means.diff<-experim.means.ratio<-rep(NA,length.vec)
  
  for (i in 1:length.vec) {
    
    experim.means.ratio[i]<-means.vec[i]*ratio.margin
    experim.means.diff[i]<-means.vec[i]+difference.margin
    cat(".")
    
  }
  cat("\n")
  
  palette("Okabe-Ito")
  
  par(mar = c(4.1, 4.1, 1.1, 1.1))
  plot(means.vec,means.vec, type="l", ylim=c(min(experim.means.diff),max(experim.means.diff)), lty=2,
       main = "", xlab = "Control arm mean",
       ylab="Experimental arm mean", las=1)
  lines(means.vec, experim.means.ratio, type="l", col="#56B4E9", lwd=2)
  lines(means.vec, experim.means.diff, type="l", col="#009E73", lwd=2)
  points(mean.control.expected, mean.experim.target, lwd=2, pch=1)
  points(mean.control.expected, mean.experim.null, lwd=2, pch=3)
  
  euclidean <- function(a, b) sqrt(sum((a - b)^2))
  
  dist.diff<-dist.ratio<-rep(NA,length(means.vec))
  for (jj in 1:length.vec) {
    dist.diff[jj]<-euclidean(c(mean.control.expected,mean.control.expected),
                           c(means.vec[jj],experim.means.diff[jj]))
    dist.ratio[jj]<-euclidean(c(mean.control.expected,mean.control.expected),
                           c(means.vec[jj],experim.means.ratio[jj]))
  }
  min.diff<-min(dist.diff, na.rm = T)
  min.ratio<-min(dist.ratio, na.rm = T)

  which.min.dist<-order(c(min.diff, min.ratio))
  method<-c("difference", "ratio")
  cat("Mean difference Margin = ", difference.margin, ".\nMean ratio Margin = ", ratio.margin, 
      "\nExpected mean in control arm: ", mean.control.expected, 
      "\nThe mean ", method[which.min.dist[2]], " summary measure for testing non-inferiority is more powerful than the mean ",
      method[which.min.dist[1]], " summary measure.\nThe non-inferiority frontier plot shows the distance of various frontiers from the expected point (solid circle).\nGreen line: mean difference frontier.\nBlue line: mean ratio frontier.\nThe black cross represents the frontier point, i.e. the point that defines the null at the expected control mean."
      ,sep="")
  results<-data.frame(means.vec, experim.means.diff, experim.means.ratio, 
                      dist.diff, dist.ratio)
}


