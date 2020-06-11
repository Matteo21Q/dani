fracpoly2fix <- function (formula, data , family="binomial") {

  poss.powers<-c(-2,-1,-0.5, 0,0.5,1,2,3)
  power1 <- c(poss.powers, poss.powers[-1], poss.powers[-c(1:2)], poss.powers[-c(1:3)],
              poss.powers[-c(1:4)], poss.powers[-c(1:5)], poss.powers[-c(1:6)], 
              poss.powers[-c(1:7)])
  power2 <- c(poss.powers, rep(poss.powers[1],7), rep(poss.powers[2],6),
              rep(poss.powers[3],5), rep(poss.powers[4],4),
              rep(poss.powers[5],3), rep(poss.powers[6],2),poss.powers[7])
  durations<-all.vars(formula[[3]])
  min.dur<-min(data[,durations])
  datamod <- data 
  datamod[,durations] <- datamod[,durations]-min.dur+0.5

  for (i in 1:36) {
    datai<-datamod
    if (power1[i]==0) {
      datai$dur1<-log(datai[,durations])
    } else {
      datai$dur1<-datai[,durations]^power1[i]
    }
    if (power1[i]==power2[i]){
      datai$dur2<-datai$dur1*log(datai[,durations])
    } else {
      if (power2[i]==0) {
        datai$dur2<-log(datai[,durations])
      } else {
        datai$dur2<-datai[,durations]^power1[i]
      }
    }
    if (family=="binomial") {
      fit<-glm(paste(formula[[2]],"~ dur1 + dur2"), datai, family=binomial)
      
    } else if (family=="survival") {
      fit<-coxph(as.formula(paste("Surv(", all.vars(formula[[2]])[1], ",",
                       all.vars(formula[[2]])[2], ") ~ dur1 + dur2", sep="")), datai)
    } else if (family=="gaussian") {
      fit<-lm(paste(formula[[2]],"~ dur1 + dur2"), datai)
    }
    
    if (i == 1) {
      max.ll<-logLik(fit)
      selected.powers<-c(power1[i], power2[i])
    } else {
      if (logLik(fit)>max.ll) {
        max.ll<-logLik(fit)
        selected.powers<-c(power1[i], power2[i])
      }
    }
  }
  
  if (selected.powers[1]==0) {
    powdur1<-paste("I(log(", durations,"-",min.dur,"+0.5))", sep="")
  } else {
    powdur1<-paste("I((", durations,"-",min.dur,"+0.5)","^", selected.powers[1], ")", sep="")
  }
  if (selected.powers[1]==selected.powers[2]){
    if (selected.powers[1]==0) {
      powdur2<-paste("I(log(", durations,"-",min.dur,"+0.5)^2)", sep="")
    } else {
      powdur2<-paste(powdur1,":I(log(", durations,"-",min.dur,"+0.5))", sep="")
    }
  } else {
    if (selected.powers[2]==0) {
      powdur2<-paste("I(log(", durations,"-",min.dur,"+0.5))", sep="")
    } else {
      powdur2<-paste("I((", durations,"-",min.dur,"+0.5)","^", selected.powers[2],")", sep="")
    }
  }
  if (family=="binomial") {
    fit<-glm(paste(formula[[2]],"~", powdur1, "+", powdur2, sep=""), data, family=binomial)
    
  } else if (family=="survival"){
    fit<-coxph(as.formula(paste("Surv(", all.vars(formula[[2]])[1], ",",
                     all.vars(formula[[2]])[2], ") ~ ", powdur1, "+", powdur2, sep="")), data)
  } else if (family=="gaussian") {
    fit<-lm(paste(formula[[2]],"~", powdur1, "+", powdur2, sep=""), data)
  }
  
  list.of.results<-list(fit = fit,selected.powers = selected.powers)
  return(list.of.results)
}