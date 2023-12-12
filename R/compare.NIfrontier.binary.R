compare.NIfrontier.binary<-function(p.control.expected, p.experim.target=NULL, p.range, NI.margin, summary.measure="RD") {
  
  if (!is.numeric(p.control.expected)||(!is.numeric(p.experim.target)&&!is.null(p.experim.target))||!is.numeric(NI.margin)||(p.control.expected<=0)||
      (p.experim.target<=0)||(p.control.expected>=1)||(p.experim.target>=1)) stop ("p.control.expected, p.experim.target and NI.margin should all be numeric. p.control.expected and p.experim.target should range between 0 and 1 (excluded).\n")
  if ((summary.measure!="RD")&&(summary.measure!="RR")&&(summary.measure!="OR")) stop("summary.measure should be one of either RD (risk difference), RR (risk ratio), AS (arcsine difference) or OR (odds ratio).\n")
  if ((summary.measure=="RR"||summary.measure=="OR")&&(NI.margin<=0)) stop("Ratio (RR or OR) NI margins cannot be negative.\n")
  if ((!is.vector(p.range))||(!is.numeric(p.range))||(length(p.range)!=2)||(sum(p.range<=0)>0)||(sum(p.range>=1)>0)) stop("p.range should be a numeric vector of length 2, with values in the (0,1) range.\n")
  if (p.range[2]<p.range[1]) {
    p.range<-p.range[c(2,1)]
  }
  if (p.range[1]==p.range[2]) stop("This function should be used to evaluate frontiers. If the range is a single point, then any summary measure will have same power if the margins are matched.")
  if ((p.control.expected>p.range[2])||(p.control.expected<p.range[1])) stop("Range of risks should include the expected value.\n")
  if ((NI.margin==0)&&(summary.measure=="RD"||summary.measure=="AS")) stop ("A Non-inferiority margin of 0 for the risk (or arcsine) difference means this is a superiority trial. Hence, all summary measures will have same power.")
  if ((NI.margin==1)&&(summary.measure=="RR"||summary.measure=="OR")) stop ("A Non-inferiority margin of 1 for a ratio means this is a superiority trial. Hence, all summary measures will have same power.")
  
  RD.margin<-convertmargin.binary(p.control.expected, NI.margin, summary.measure, "RD")
  RR.margin<-convertmargin.binary(p.control.expected, NI.margin, summary.measure, "RR")
  OR.margin<-convertmargin.binary(p.control.expected, NI.margin, summary.measure, "OR")
  AS.margin<-convertmargin.binary(p.control.expected, NI.margin, summary.measure, "AS")

  p.experim.null<-RD.margin+p.control.expected
  
  if (is.null(p.experim.target)) p.experim.target<-p.control.expected
  if (!(((p.experim.target>p.experim.null)&&(p.control.expected>=p.experim.null))||((p.experim.target<p.experim.null)&&(p.control.expected<=p.experim.null)))) stop ("The alternative hypothesis does not imply non-inferiority of the experimental treatment")
  
  length.vec<-100
  p.vec<-seq(p.range[1], p.range[2], length.out=length.vec)
  experim.p.RD<-experim.p.RR<-experim.p.OR<-experim.p.AS<-rep(NA,length.vec)
  
  for (i in 1:length.vec) {
    
    experim.p.RR[i]<-p.vec[i]*RR.margin
    experim.p.RD[i]<-p.vec[i]+RD.margin
    odds.vec<-p.vec[i]/(1-p.vec[i])
    odds.experim.null<-OR.margin*odds.vec
    experim.p.OR[i]<-odds.experim.null/(1+odds.experim.null)
    experim.p.AS[i]<-(sin(asin(sqrt(p.vec[i]))+AS.margin))^2
    
    cat(".")
    
  }
  cat("\n")
  
  palette("Okabe-Ito")
  
  par(mar = c(4.1, 4.1, 1.1, 1.1))
  plot(p.vec,p.vec, type="l", ylim=c(min(experim.p.RD,experim.p.RR,experim.p.OR,experim.p.AS, p.experim.null, na.rm=T),max(experim.p.RD,experim.p.RR,experim.p.OR,experim.p.AS, p.experim.null, na.rm=T)), lty=2,
       main = "", xlab = "Control arm risk",
       ylab="Experimental arm risk", las=1)
  lines(p.vec, experim.p.RR, type="l", lwd=2)
  lines(p.vec, experim.p.OR, type="l", col="#56B4E9", lwd=2)
  lines(p.vec, experim.p.RD, type="l", col="#009E73", lwd=2)
  lines(p.vec, experim.p.AS, type="l", col="orange", lwd=2)
  points(p.control.expected, p.experim.target, lwd=2, pch=1)
  points(p.control.expected, p.experim.null, lwd=2, pch=3)
  
  euclidean <- function(a, b) sqrt(sum((a - b)^2))
  
  dist.RD<-dist.RR<-dist.OR<-dist.AS<-rep(NA,length(p.vec))
  for (jj in 1:length.vec) {
    dist.RD[jj]<-euclidean(c(p.control.expected,p.control.expected),
                             c(p.vec[jj],experim.p.RD[jj]))
    dist.RR[jj]<-euclidean(c(p.control.expected,p.control.expected),
                              c(p.vec[jj],experim.p.RR[jj]))
    dist.OR[jj]<-euclidean(c(p.control.expected,p.control.expected),
                           c(p.vec[jj],experim.p.OR[jj]))
    dist.AS[jj]<-euclidean(c(p.control.expected,p.control.expected),
                           c(p.vec[jj],experim.p.AS[jj]))
  }
  min.RD<-min(dist.RD, na.rm = T)
  min.RR<-min(dist.RR, na.rm = T)
  min.OR<-min(dist.OR, na.rm = T)
  min.AS<-min(dist.AS, na.rm = T)
  
  which.min.dist<-order(c(min.RD, min.RR, min.OR, min.AS))
  method<-c("Risk Difference", "Risk Ratio", "Odds Ratio", "Arcsine Difference")
  cat("Risk difference Margin = ", RD.margin, ".\nRisk ratio Margin = ", RR.margin, ".\nOdds ratio Margin = ", OR.margin, ".\nArcsine Difference Margin = ", AS.margin,
      "\nExpected risk in control arm: ", p.control.expected, 
      "\nThe ", method[which.min.dist[4]], " summary measure for testing non-inferiority is the most powerful. The ",
      method[which.min.dist[3]], " summary measure comes second followed by the ", method[which.min.dist[2]]," summary measure. The ", method[which.min.dist[1]] , " is the least powerful summary measure.
      \nThe non-inferiority frontier plot shows the distance of various frontiers from the expected point (solid circle).\n
Green line: Risk Difference frontier.\nBlue line: Odds Ratio frontier.\nBlack line: Risk Ratio frontier.\nOrange line: Arcsine difference frontier.\nThe black cross represents the frontier point, i.e. the point that defines the point null at the expected control risk.\nThe dashed line represents the line of equality, i.e. the line where control and experimental risks are the same.\n"
      ,sep="")
  results<-data.frame(p.vec, experim.p.RD, experim.p.RR,experim.p.OR,experim.p.AS,
                      dist.RD, dist.RR, dist.OR, dist.AS)
}


