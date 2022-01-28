test.NI <- function(n0, n1, e0, e1, NI.margin, sig.level=0.025, scale="RD", 
                    print.out=TRUE, unfavourable=TRUE, test.type="Wald",
                    M.boot=2000, BB.adj=0.0001) {
  
  stopifnot(is.numeric(n0), n0>0)
  stopifnot(is.numeric(n1), n1>0)
  stopifnot(is.numeric(e0), is.numeric(e1))
  stopifnot(is.numeric(NI.margin))
  stopifnot(is.numeric(sig.level), sig.level < 1, sig.level > 0)
  stopifnot(is.character(scale),(( scale == "RD" ) || ( scale == "RR" ) || ( scale == "OR" ) || ( scale == "AS" )))
  stopifnot(is.logical(print.out), !is.na(print.out))
  stopifnot(is.character(test.type), ((test.type == "Wald") || (test.type == "Wald.cc") ||  
                                      (test.type=="Hauck.Anderson") || (test.type=="Gart.Nam") ||
                                      (test.type == "Newcombe10") || (test.type == "Newcombe11") ||
                                      (test.type == "Haldane") || (test.type == "Jeffreys.Perks") ||
                                      (test.type == "Agresti.Caffo") || (test.type == "Miettinen.Nurminen") ||
                                      (test.type == "Farrington.Manning") || (test.type == "logistic") ||
                                      (test.type == "bootstrap") || (test.type=="Agresti.Min") ||
                                      (test.type == "Brown.Li.Jeffreys") || (test.type=="Chan.Zhang") ||
                                      (test.type == "BLNM") ||
                                      (test.type == "midp") || (test.type=="Berger.Boos") ||
                                      (test.type == "MUE.Lin") || (test.type == "MUE.parametric.bootstrap")))
  stopifnot(is.logical(unfavourable), !is.na(unfavourable))
  
  if (scale=="RD") {
    if ((unfavourable == T)&&(NI.margin<=0)) stop("When outcome is unfavourable, a NI margin on the risk difference scale needs to be positive.")
    if ((unfavourable == F)&&(NI.margin>=0)) stop("When outcome is favourable, a NI margin on the risk difference scale needs to be negative.")
    NIm<- NI.margin
    se <- se.n <- sqrt(e1/n1*(1-e1/n1)/n1+e0/n0*(1-e0/n0)/n0)
    estimate <- estimate.n <- e1/n1-e0/n0
    Z <- (estimate - NIm)/se
    if (test.type=="Wald") {
      CI <- c(estimate-qnorm(1-sig.level)*se,estimate+qnorm(1-sig.level)*se)
    } else if (test.type=="Wald.cc") {
      Z<-Z+(1/n0+1/n1)/2*(1-2*(unfavourable==F))
      CI <- c(estimate-(qnorm(1-sig.level)*se+(1/n0+1/n1)/2),
              estimate+(qnorm(1-sig.level)*se+(1/n0+1/n1)/2))
      } else if (test.type=="Newcombe10") {
      CI <- BinomDiffCI(e1, n1, e0, n0, conf.level = (1-sig.level*2), method = "score")[2:3]

      } else if (test.type=="Newcombe11") {
        CI <- BinomDiffCI(e1, n1, e0, n0, conf.level = (1-sig.level*2), method = "scorecc")[2:3]

      } else if (test.type=="Gart.Nam") {
        CI <- scasci(e1,n1,e0,n0, level=(1-sig.level*2))$estimates[c(1,3)]

      } else if (test.type=="Agresti.Caffo") {
        CI <- BinomDiffCI(e1, n1, e0, n0, conf.level = (1-sig.level*2), method = "ac")[2:3]

      } else if (test.type=="Haldane") {
        CI <- BinomDiffCI(e1, n1, e0, n0, conf.level = (1-sig.level*2), method = "hal")[2:3]

      } else if (test.type=="Hauck.Anderson") {
        CI <- BinomDiffCI(e1, n1, e0, n0, conf.level = (1-sig.level*2), method = "ha")[2:3]

      } else if (test.type=="Jeffreys.Perks") {
        CI <- BinomDiffCI(e1, n1, e0, n0, conf.level = (1-sig.level*2), method = "jp")[2:3]

      } else if (test.type=="Miettinen.Nurminen") {
        CI <- BinomDiffCI(e1, n1, e0, n0, conf.level = (1-sig.level*2), method = "mn")[2:3]

      } else if (test.type=="Farrington.Manning") {
        CI <- BinomDiffCI(e1, n1, e0, n0, conf.level = (1-sig.level*2), method = "mee")[2:3]

      } else if (test.type=="Brown.Li.Jeffreys") {
        CI <- BinomDiffCI(e1, n1, e0, n0, conf.level = (1-sig.level*2), method = "blj")[2:3]

      } else if (test.type=="BLNM") {
        CI1 <- BinomDiffCI(e1, n1, e0, n0, conf.level = (1-sig.level*2), method = "blj")[2:3]
        CI2 <- BinomDiffCI(e1, n1, e0, n0, conf.level = (1-sig.level*2), method = "mn")[2:3]
        CI <- CI1/3 + 2 * CI2/3

      } else if (test.type=="logistic") {
        y<-c(rep(1, e1),rep(0, n1-e1),rep(1, e0), rep(0, n0-e0))
        treat<-factor(c(rep(1,n1), rep(0, n0)))
        dd<-data.frame(y,treat)
        fit<-glm(y~treat, data=dd, family = binomial)
        fit.std <- summary(marginaleffects(fit))
        CI <- as.numeric(fit.std[7:8])

      } else if ( test.type == "bootstrap") {
        y<-c(rep(1, e1),rep(0, n1-e1),rep(1, e0), rep(0, n0-e0))
        treat<-factor(c(rep(1,n1), rep(0, n0)))
        dd<-data.frame(y,treat)
        rdif <- function(dat, indices) {
          d <- dat[indices,] # allows boot to select sample
          rd <- mean(d[d[,2]==1,1]) - mean(d[d[,2]==0,1])
          return(rd)
        } 
        res.b<-boot(dd, rdif, R=M.boot)
        CI<-boot.ci(res.b, type="perc")$percent[4:5]

      } else if (test.type == "Agresti.Min") {
        fit<-uncondExact2x2(e0,n0,e1,n1, method="score", tsmethod = "square", conf.int = T)
        CI <- as.numeric(fit$conf.int)

      } else if (test.type == "Chan.Zhang") {
        fit<-uncondExact2x2(e0,n0,e1,n1, method="score", tsmethod = "central", conf.int = T)
        CI <- as.numeric(fit$conf.int)

      } else if (test.type == "midp") {
        fit<-uncondExact2x2(e0,n0,e1,n1, method="score", tsmethod = "square", midp=T, conf.int = T)
        CI <- as.numeric(fit$conf.int)

      } else if (test.type == "Berger.Boos") {
        fit<-uncondExact2x2(e0,n0,e1,n1, method="score", tsmethod = "square", gamma=BB.adj, conf.int = T)
        CI <- as.numeric(fit$conf.int)

      } else if (test.type == "MUE.Lin") {
        p0.tilde<-ifelse(((e0>0)&&(e0<n0)),(qbeta(0.5,e0,n0-e0+1)+qbeta(0.5,e0+1,n0-e0))/2,
                           ifelse(e0==0,(1-0.5^(1/n0))/2,(1+0.5^(1/n0))/2))
        p1.tilde<-ifelse(((e1>0)&&(e1<n1)),(qbeta(0.5,e1,n1-e1+1)+qbeta(0.5,e1+1,n1-e1))/2,
                         ifelse(e1==0,(1-0.5^(1/n1))/2,(1+0.5^(1/n1))/2))
        probs.mat<-matrix(NA,(n0+1)*(n1+1),7)
        kk=1
        for (ii in 0:n0) {
          for (jj in 0:n1) {
            probs.mat[kk,6]<-ii
            probs.mat[kk,7]<-jj
            probs.mat[kk,2]<-ifelse(((ii>0)&&(ii<n0)),(qbeta(0.5,ii,n0-ii+1)+qbeta(0.5,ii+1,n0-ii))/2,
                                    ifelse(ii==0,(1-0.5^(1/n0))/2,(1+0.5^(1/n0))/2))
            
            probs.mat[kk,3]<-ifelse(((jj>0)&&(jj<n1)),(qbeta(0.5,jj,n1-jj+1)+qbeta(0.5,jj+1,n1-jj))/2,
                                    ifelse(jj==0,(1-0.5^(1/n1))/2,(1+0.5^(1/n1))/2))
            
            probs.mat[kk,4]<-probs.mat[kk,3]-probs.mat[kk,2]
            probs.mat[kk,1]<-choose(n0,ii)*choose(n1,jj)*p0.tilde^ii*p1.tilde^jj*(1-p0.tilde)^(n0-ii)*(1-p1.tilde)^(n1-jj)
            kk=kk+1
          }
        }
        probs.mat<-probs.mat[order(probs.mat[,4]),]
        probs.mat[,5]<-cumsum(probs.mat[,1])
        
        ci.l.l<-max(which(probs.mat[,5]<=sig.level))
        ci.l.u<-min(which(probs.mat[,5]>=sig.level))
        ci.l<-(probs.mat[ci.l.l,4]*(probs.mat[ci.l.u,5]-sig.level)+probs.mat[ci.l.u,4]*(-probs.mat[ci.l.l,5]+sig.level))/(probs.mat[ci.l.u,5]-probs.mat[ci.l.l,5])
        ci.u.l<-max(which(probs.mat[,5]<=(1-sig.level)))
        ci.u.u<-min(which(probs.mat[,5]>=(1-sig.level)))
        ci.u<-(probs.mat[ci.u.l,4]*(probs.mat[ci.u.u,5]-(1-sig.level))+probs.mat[ci.u.u,4]*(-probs.mat[ci.u.l,5]+1-sig.level))/(probs.mat[ci.u.u,5]-probs.mat[ci.u.l,5])
        CI<-c(ci.l,ci.u)
  
              } else if (test.type == "MUE.parametric.bootstrap") {
        p0.tilde<-ifelse(((e0>0)&&(e0<n0)),(qbeta(0.5,e0,n0-e0+1)+qbeta(0.5,e0+1,n0-e0))/2,
                         ifelse(e0==0,(1-0.5^(1/n0))/2,(1+0.5^(1/n0))/2))
        p1.tilde<-ifelse(((e1>0)&&(e1<n1)),(qbeta(0.5,e1,n1-e1+1)+qbeta(0.5,e1+1,n1-e1))/2,
                         ifelse(e1==0,(1-0.5^(1/n1))/2,(1+0.5^(1/n1))/2))
        y<-c(rep(1, e1),rep(0, n1-e1),rep(1, e0), rep(0, n0-e0))
        treat<-factor(c(rep(1,n1), rep(0, n0)))
        dd<-data.frame(y,treat)
        rdif <- function(dat) {
          p0.tilde.i<-ifelse(((as.numeric(dat[1,1])>0)&&(as.numeric(dat[1,1])<n0)),(qbeta(0.5,as.numeric(dat[1,1]),n0-as.numeric(dat[1,1])+1)+qbeta(0.5,as.numeric(dat[1,1])+1,n0-as.numeric(dat[1,1])))/2,
                             ifelse(as.numeric(dat[1,1])==0,(1-0.5^(1/n0))/2,(1+0.5^(1/n0))/2))
          p1.tilde.i<-ifelse(((as.numeric(dat[1,2])>0)&&(as.numeric(dat[1,2])<n1)),(qbeta(0.5,as.numeric(dat[1,2]),n1-as.numeric(dat[1,2])+1)+qbeta(0.5,as.numeric(dat[1,2])+1,n1-as.numeric(dat[1,2])))/2,
                             ifelse(as.numeric(dat[1,2])==0,(1-0.5^(1/n1))/2,(1+0.5^(1/n1))/2))
          
          rd <- p1.tilde.i-p0.tilde.i
          return(rd)
        } 
        rg <- function(dat, mle) {
          # Function to generate random exponential variates.
          # mle will contain the mean of the original data
          out <- data.frame(matrix(NA,1,2))
          out[1,1] <- rbinom(1,mle[2], mle[1])
          out[1,2] <- rbinom(1,mle[4], mle[3])
          out
        }
        res.b<-boot(dd, rdif, R = M.boot, sim = "parametric",
                    ran.gen = rg, mle = c(p0.tilde,n0,p1.tilde,n1))
        CI<-boot.ci(res.b, type="perc")$percent[4:5]
        
              }
    estimate.n<-sum(CI)/2
    se.n<-(CI[2]-CI[1])/(2*qnorm(1-sig.level))
    Z <- ifelse(unfavourable==T,(estimate.n - NIm)/se.n,(-estimate.n + NIm)/se.n)
    p <- pnorm(Z)
    
    if (print.out==T) cat("Risk difference:\nMethod = ",test.type,",\n Estimate: ", estimate.n, "\nStandard error: ", se.n, "\nConfidence interval (Two-sided ", (1-sig.level*2)*100,"%): (", CI[1], ",", CI[2], ")\np-value:", p, ".\n" )
    
  } else if (scale == "RR") {
    if ((unfavourable == T)&&(NI.margin<=1)) stop("When outcome is unfavourable, a NI margin on the risk ratio scale needs to be >1.")
    if ((unfavourable == F)&&(NI.margin>=1)) stop("When outcome is favourable, a NI margin on the risk ratio scale needs to be <1.")
    NIm<- log(NI.margin)
    se <- sqrt(1/e0-1/n0+1/e1-1/n1)
    estimate <- log((e1/n1)/(e0/n0))
    Z <- (estimate - NIm)/se
    p <- pnorm(Z)
    CI.norm <- CI <- exp(c(estimate-qnorm(1-sig.level)*se,estimate+qnorm(1-sig.level)*se))
    if (print.out==T) cat("Risk ratio:\nEstimate: ", exp(estimate), "\nlog(RR):", estimate, "\nStandard error (log(RR)): ", se, "\nConfidence interval (one-sided ", sig.level*100,"%): (", CI[1], ",", CI[2], ")\np-value:", p, ".\n" )
    
  } else if (scale == "AS") {
    NIm<-NI.margin
    if ((unfavourable == T)&&(NI.margin<=0)) stop("When outcome is unfavourable, a NI margin on the arc-sine difference scale needs to be >0.")
    if ((unfavourable == F)&&(NI.margin>=0)) stop("When outcome is favourable, a NI margin on the arc-sine difference scale needs to be <0.")
    se <- sqrt(1/(4*n0)+1/(4*n1))
    estimate <- asin(sqrt(e1/n1))-asin(sqrt(e0/n0))
    Z <- (estimate - NIm)/se
    p <- pnorm(Z)
    CI.norm <- CI <- c(estimate-qnorm(1-sig.level)*se,estimate+qnorm(1-sig.level)*se)
    if (print.out==T) cat("Arc-sine difference:\nEstimate: ", estimate, "\nStandard error: ", se, "\nConfidence interval (two-sided ", sig.level*200,"%): (", CI[1], ",", CI[2], ")\np-value:", p, ".\n" )
  } else if (scale == "OR") {
    if ((unfavourable == T)&&(NI.margin<=1)) stop("When outcome is unfavourable, a NI margin on the odds ratio scale needs to be >1.")
    if ((unfavourable == F)&&(NI.margin>=1)) stop("When outcome is favourable, a NI margin on the odds ratio scale needs to be <1.")
    NIm<- log(NI.margin)
    se <- sqrt(1/e0+1/(n0-e0)+1/e1+1/(n1-e1))
    estimate <- log((e1/(n1-e1))/(e0/(n0-e0)))
    Z <- (estimate - NIm)/se
    p <- pnorm(Z)
    CI.norm <- CI <- exp(c(estimate-qnorm(1-sig.level)*se,estimate+qnorm(1-sig.level)*se))
    if (print.out==T) cat("Odds ratio:\nEstimate: ", exp(estimate), "\nlog(OR):", estimate, "\nStandard error (log(OR)): ", se, "\nConfidence interval (one-sided ", sig.level*100,"%): (", CI[1], ",", CI[2], ")\np-value:", p, ".\n" )
    
  }
  results <- list(estimate, se, Z, p, CI)
  names(results)<-c("estimate", "se", "Z", "p", "CI")
  return(results)
}