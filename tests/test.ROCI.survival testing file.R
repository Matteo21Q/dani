###################################################
### test.ROCI.survival testing file             #####
### 28-02-2025                                #####
###################################################

# Load dani:
# library(dani)
library(boot)
library(mfp)
library(marginaleffects)
library(flexsurv)
library(tibble)

#Initialise vector of outputs 
correct<-list(NULL)
n.t<-1

#####################################################
# First set of checks:
# Check that it stops for non acceptable values of formula and data

out1A<-try(test.ROCI.survival(data="gigi", NI.margin=0.1))
correct[[n.t]]<-ifelse((inherits(out1A, "try-error"))&&(grepl("!is.null(formula) is not TRUE", out1A[1], fixed=T  )),1,0) 
names(correct)[[n.t]]<-"out1A"
n.t=n.t+1
out1B<-try(test.ROCI.survival( formula=as.formula("Y~x"), NI.margin=0.1))
correct[[n.t]]<-ifelse((inherits(out1B, "try-error"))&&(grepl("!is.null(data) is not TRUE", out1B[1], fixed=T  )),1,0) 
names(correct)[[n.t]]<-"out1B"
n.t=n.t+1
out1C<-try(test.ROCI.survival(data="gigi", formula=as.formula("Y~x"), NI.margin=0.1))
correct[[n.t]]<-ifelse((inherits(out1C, "try-error"))&&(grepl("is.data.frame(data) is not TRUE", out1C[1], fixed=T )),1,0) 
names(correct)[[n.t]]<-"out1C"
n.t=n.t+1
out1D<-try(test.ROCI.survival(formula=as.formula("Surv(time, status)~treat(treat)"), data = data.frame(status=rbinom(100,1,0.5)), NI.margin=0.1))
correct[[n.t]]<-ifelse((inherits(out1D, "try-error"))&&(grepl("undefined columns selected", out1D[1], fixed=T  )),1,0) 
names(correct)[[n.t]]<-"out1D"
n.t=n.t+1
out1E<-try(test.ROCI.survival(formula=as.formula(Surv(time, status)~treat(treatment)), data=data.frame(status = rep(c(1,2,3,4),25), time = rnorm(100,100,1), treatment = sample(7,100,rep=TRUE)), NI.margin=0.1))
correct[[n.t]]<-ifelse((inherits(out1E, "try-error"))&&(grepl("nlevels(factor(outcomes[, 2])) == 2 is not TRUE", out1E[1], fixed=T  )),1,0) 
names(correct)[[n.t]]<-"out1E"
n.t=n.t+1

#####################################################
# Second set of checks:
# Check that it stops for non-acceptable margins:

out2A<-try(test.ROCI.survival(formula=as.formula(Surv(time, status)~treat(treatment)), data=data.frame(status = rep(c(1,0),50), time = rnorm(100,100,1), treatment = sample(7,100,rep=TRUE)), NI.margin="0.1"))
correct[[n.t]]<-ifelse((inherits(out2A, "try-error"))&&(grepl("is.numeric(NI.margin) is not TRUE", out2A[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out2A"
n.t=n.t+1
out2B<-try(test.ROCI.survival(formula=as.formula(Surv(time, status)~treat(treatment)), data=data.frame(status = rep(c(1,0),50), time = rnorm(100,100,1), treatment = sample(7,100,rep=TRUE)), NI.margin=0.1))
correct[[n.t]]<-ifelse((inherits(out2B, "try-error"))&&(grepl("When outcome is unfavourable, NI ", out2B[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out2B"
n.t=n.t+1
out2C<-try(test.ROCI.survival(formula=as.formula(Surv(time, status)~treat(treatment)), data=data.frame(status = rep(c(1,0),50), time = rnorm(100,100,1), treatment = sample(7,100,rep=TRUE)), unfavourable=FALSE, NI.margin=1.1))
correct[[n.t]]<-ifelse((inherits(out2C, "try-error"))&&(grepl("When outcome is favourable, NI ", out2C[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out2C"
n.t=n.t+1
out2D<-try(test.ROCI.survival(formula=as.formula(Surv(time, status)~treat(treatment)), data=data.frame(status = rep(c(1,0),50), time = rnorm(100,100,1), treatment = sample(7,100,rep=TRUE)), unfavourable=FALSE, NI.margin=-0.1))
correct[[n.t]]<-ifelse((inherits(out2D, "try-error"))&&(grepl("A hazard ratio margin", out2D[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out2D"
n.t=n.t+1
out2E<-try(test.ROCI.survival(formula=as.formula(Surv(time, status)~treat(treatment)), data=data.frame(status = rep(c(1,0),50), time = rnorm(100,100,1), treatment = sample(7,100,rep=TRUE)), NI.margin=0.1, summary.measure="DS"))
correct[[n.t]]<-ifelse((inherits(out2E, "try-error"))&&(grepl("When outcome is unfavourable", out2E[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out2E"
n.t=n.t+1
out2F<-try(test.ROCI.survival(formula=as.formula(Surv(time, status)~treat(treatment)), data=data.frame(status = rep(c(1,0),50), time = rnorm(100,100,1), treatment = sample(7,100,rep=TRUE)), NI.margin=-1.1, summary.measure="DS"))
correct[[n.t]]<-ifelse((inherits(out2F, "try-error"))&&(grepl("NI margins cannot be lower than -1", out2F[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out2F"
n.t=n.t+1
out2G<-try(test.ROCI.survival(formula=as.formula(Surv(time, status)~treat(treatment)), data=data.frame(status = rep(c(1,0),50), time = rnorm(100,100,1), treatment = sample(7,100,rep=TRUE)), NI.margin=0.1, unfavourable=FALSE, summary.measure = "RS"))
correct[[n.t]]<-ifelse((inherits(out2G, "try-error"))&&(grepl("When outcome is favourable, NI margins on the ratio", out2G[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out2G"
n.t=n.t+1
out2H<-try(test.ROCI.survival(formula=as.formula(Surv(time, status)~treat(treatment)), data=data.frame(status = rep(c(1,0),50), time = rnorm(100,100,1), treatment = sample(7,100,rep=TRUE)), NI.margin=0.2, summary.measure="DRMST"))
correct[[n.t]]<-ifelse((inherits(out2H, "try-error"))&&(grepl("When outcome is unfavourable, difference", out2H[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out2H"
n.t=n.t+1
out2I<-try(test.ROCI.survival(formula=as.formula(Surv(time, status)~treat(treatment)), data=data.frame(status = rep(c(1,0),50), time = rnorm(100,100,1), treatment = sample(7,100,rep=TRUE)), NI.margin=-0.1, summary.measure="DRMST", unfavourable = FALSE))
correct[[n.t]]<-ifelse((inherits(out2I, "try-error"))&&(grepl("When outcome is favourable, difference ", out2I[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out2I"
n.t=n.t+1
out2J<-try(test.ROCI.survival(formula=as.formula(Surv(time, status)~treat(treatment)), data=data.frame(status = rep(c(1,0),50), time = rnorm(100,100,1), treatment = sample(7,100,rep=TRUE)), NI.margin=c(-0.1,-0.1,-0.1,-0.1,-0.1)))
correct[[n.t]]<-ifelse((inherits(out2J, "try-error"))&&(grepl("length(NI.margin) == (length(treatment.levels)", out2J[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out2J"
n.t=n.t+1

#####################################################
# Third set of checks:
# Check that it stops for unacceptable values of significance level:

out3A<-try(test.ROCI.survival(formula=as.formula(Surv(time, status)~treat(treatment)), data=data.frame(status = rep(c(1,0),50), time = rnorm(100,100,1), treatment = sample(7,100,rep=TRUE)), NI.margin=1.1, sig.level="0.025"))
correct[[n.t]]<-ifelse((inherits(out3A, "try-error"))&&(grepl("is.numeric(sig.level) is not TRUE", out3A[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out3A"
n.t=n.t+1
out3B<-try(test.ROCI.survival(formula=as.formula(Surv(time, status)~treat(treatment)), data=data.frame(status = rep(c(1,0),50), time = rnorm(100,100,1), treatment = sample(7,100,rep=TRUE)), NI.margin=1.1, sig.level=-0.025))
correct[[n.t]]<-ifelse((inherits(out3B, "try-error"))&&(grepl("sig.level > 0 is not TRUE", out3B[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out3B"
n.t=n.t+1
out3C<-try(test.ROCI.survival(formula=as.formula(Surv(time, status)~treat(treatment)), data=data.frame(status = rep(c(1,0),50), time = rnorm(100,100,1), treatment = sample(7,100,rep=TRUE)), NI.margin=1.1, sig.level=1.1))
correct[[n.t]]<-ifelse((inherits(out3C, "try-error"))&&(grepl("sig.level < 0.5 is not TRUE", out3C[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out3C"
n.t=n.t+1

#####################################################
# Fourth set of checks:
# Check with other wrong arguments:

out4A<-try(test.ROCI.survival(formula=as.formula(Surv(time, status)~treat(treatment)), data=data.frame(status = rep(c(1,0),50), time = rnorm(100,100,1), treatment = sample(7,100,rep=TRUE)), NI.margin=1.1, summary.measure = "pippo"))
correct[[n.t]]<-ifelse((inherits(out4A, "try-error"))&&(grepl("summary.measure ==", out4A[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4A"
n.t=n.t+1

# Check that it works when reference incorrectly specified:
out4B<-try(test.ROCI.survival(formula=as.formula(Surv(time, status)~treat(treatment)), data=data.frame(status = rep(c(1,0),50), time = rnorm(100,100,1), treatment = sample(7,100,rep=TRUE)), NI.margin=1.1, reference=NA))
correct[[n.t]]<-ifelse((inherits(out4B, "try-error"))&&(grepl("is.numeric(reference) is not TRUE", out4B[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4B"
n.t=n.t+1

# Check that it works when se.method incorrectly specified:
out4C<-try(test.ROCI.survival(formula=as.formula(Surv(time, status)~treat(treatment)), data=data.frame(status = rep(c(1,0),50), time = rnorm(100,100,1), treatment = sample(7,100,rep=TRUE)), NI.margin=1.1, se.method=2))
correct[[n.t]]<-ifelse((inherits(out4C, "try-error"))&&(grepl("is.character(se.method) is not TRUE", out4C[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4C"
n.t=n.t+1
out4D<-try(test.ROCI.survival(formula=as.formula(Surv(time, status)~treat(treatment)), data=data.frame(status = rep(c(1,0),50), time = rnorm(100,100,1), treatment = sample(7,100,rep=TRUE)), NI.margin=1.1, se.method="pippo"))
correct[[n.t]]<-ifelse((inherits(out4D, "try-error"))&&(grepl("se.method %in% c", out4D[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4D"
n.t=n.t+1

# Check that it works when unfavourable incorrectly specified:
out4E<-try(test.ROCI.survival(formula=as.formula(Surv(time, status)~treat(treatment)), data=data.frame(status = rep(c(1,0),50), time = rnorm(100,100,1), treatment = sample(7,100,rep=TRUE)), NI.margin=1.1, unfavourable = 3))
correct[[n.t]]<-ifelse((inherits(out4E, "try-error"))&&(grepl("is.logical(unfavourable) is not TRUE", out4E[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4E"
n.t=n.t+1
out4F<-try(test.ROCI.survival(formula=as.formula(Surv(time, status)~treat(treatment)), data=data.frame(status = rep(c(1,0),50), time = rnorm(100,100,1), treatment = sample(7,100,rep=TRUE)), NI.margin=1.1, unfavourable=NA))
correct[[n.t]]<-ifelse((inherits(out4F, "try-error"))&&(grepl("!is.na(unfavourable) is not TRUE", out4F[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4F"
n.t=n.t+1

# Check that M.boot has acceptable value:
out4G<-try(test.ROCI.survival(formula=as.formula(Surv(time, status)~treat(treatment)), data=data.frame(status = rep(c(1,0),50), time = rnorm(100,100,1), treatment = sample(7,100,rep=TRUE)), NI.margin=1.1, M.boot="0.025"))
correct[[n.t]]<-ifelse((inherits(out4G, "try-error"))&&(grepl("is.numeric(M.boot) is not TRUE", out4G[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4G"
n.t=n.t+1
out4H<-try(test.ROCI.survival(formula=as.formula(Surv(time, status)~treat(treatment)), data=data.frame(status = rep(c(1,0),50), time = rnorm(100,100,1), treatment = sample(7,100,rep=TRUE)), NI.margin=1.1, M.boot=0))
correct[[n.t]]<-ifelse((inherits(out4H, "try-error"))&&(grepl("M.boot > 1 is not TRUE", out4H[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4H"
n.t=n.t+1

# Check that tr.model has acceptable value:
out4I<-try(test.ROCI.survival(formula=as.formula(Surv(time, status)~treat(treatment)), data=data.frame(status = rep(c(1,0),50), time = rnorm(100,100,1), treatment = sample(7,100,rep=TRUE)), NI.margin=1.1, tr.model=NA))
correct[[n.t]]<-ifelse((inherits(out4I, "try-error"))&&(grepl("is.character(tr.model) is not TRUE", out4I[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4I"
n.t=n.t+1
out4J<-try(test.ROCI.survival(formula=as.formula(Surv(time, status)~treat(treatment)), data=data.frame(status = rep(c(1,0),50), time = rnorm(100,100,1), treatment = sample(7,100,rep=TRUE)), NI.margin=1.1, tr.model="0.025"))
correct[[n.t]]<-ifelse((inherits(out4J, "try-error"))&&(grepl("tr.model %in% c", out4J[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4J"
n.t=n.t+1

# Check that it works when treatment.levels incorrectly specified:
out4K<-try(test.ROCI.survival(formula=as.formula(Surv(time, status)~treat(treatment)), data=data.frame(status = rep(c(1,0),50), time = rnorm(100,100,1), treatment = sample(7,100,rep=TRUE)), NI.margin=1.1, treatment.levels="0.025"))
correct[[n.t]]<-ifelse((inherits(out4K, "try-error"))&&(grepl("is.numeric(treatment.levels) is not TRUE", out4K[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4K"
n.t=n.t+1
out4L<-try(test.ROCI.survival(formula=as.formula(Surv(time, status)~treat(treatment)), data=data.frame(status = rep(c(1,0),50), time = rnorm(100,100,1), treatment = sample(7,100,rep=TRUE)), NI.margin=1.1, treatment.levels=c(1,2)))
correct[[n.t]]<-ifelse((inherits(out4L, "try-error"))&&(grepl("length(treatment.levels) > 2 is not TRUE", out4L[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4L"
n.t=n.t+1

# Check that it works when bootCI.type incorrectly specified:
out4M<-try(test.ROCI.survival(formula=as.formula(Surv(time, status)~treat(treatment)), data=data.frame(status = rep(c(1,0),50), time = rnorm(100,100,1), treatment = sample(7,100,rep=TRUE)), NI.margin=1.1, se.method="bootstrap", bootCI.type = 2))
correct[[n.t]]<-ifelse((inherits(out4M, "try-error"))&&(grepl("is.character(bootCI.type) is not TRUE", out4M[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4M"
n.t=n.t+1
out4N<-try(test.ROCI.survival(formula=as.formula(Surv(time, status)~treat(treatment)), data=data.frame(status = rep(c(1,0),50), time = rnorm(100,100,1), treatment = sample(7,100,rep=TRUE)), NI.margin=1.1, se.method="bootstrap", bootCI.type = "pippo"))
correct[[n.t]]<-ifelse((inherits(out4N, "try-error"))&&(grepl("bootCI.type %in% c", out4N[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4N"
n.t=n.t+1

# Check that it works when parallel incorrectly specified:
out4O<-try(test.ROCI.survival(formula=as.formula(Surv(time, status)~treat(treatment)), data=data.frame(status = rep(c(1,0),50), time = rnorm(100,100,10), treatment = sample(7,100,rep=TRUE)), NI.margin=1.1, se.method="bootstrap", parallel=NA))
correct[[n.t]]<-ifelse((inherits(out4O, "try-error"))&&(grepl("'arg' must be NULL or a character vector", out4O[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4O"
n.t=n.t+1
out4P<-try(test.ROCI.survival(formula=as.formula(Surv(time, status)~treat(treatment)), data=data.frame(status = rep(c(1,0),50), time = rnorm(100,100,10), treatment = sample(7,100,rep=TRUE)), NI.margin=1.1, se.method="bootstrap", parallel="pippo"))
correct[[n.t]]<-ifelse((inherits(out4P, "try-error"))&&(grepl("should be one of", out4P[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4P"
n.t=n.t+1

# Check behavior with wrong n.cpus
out4Q<-try(test.ROCI.survival(formula=as.formula(Surv(time, status)~treat(treatment)), data=data.frame(status = rep(c(1,0),50), time = rnorm(100,100,10), treatment = sample(7,100,rep=TRUE)), NI.margin=1.1, se.method="bootstrap", parallel="snow", n.cpus="pippo"))
correct[[n.t]]<-ifelse((inherits(out4Q, "try-error"))&&(grepl("invalid 'times' argument", out4Q[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4Q"
n.t=n.t+1

#####################################################
# Fifth set of checks:
# Now check test for certain values on DS scale. 

set.seed(1)
status1 = rep(c(1,0),50)
time1 = rnorm(100,100,10)
treatment1 = sample(7,100,rep=TRUE)
data1<-data.frame(status1, time1, treatment1)
out5A<-try(test.ROCI.survival(formula=as.formula(Surv(time1, status1)~treat(treatment1)), data=data1, 
                            NI.margin=-0.1, reference=7, se.method="delta", 
                            treatment.levels=1:7, summary.measure="DS", 
                            tr.model="FP2.select",sig.level=0.025,
                            unfavourable=TRUE, tau=101))
correct[[n.t]]<-ifelse((inherits(out5A,"list"))&&(all.equal(out5A$up.bounds.CI[2],0.2416069, tolerance=10^(-4)))&&out5A$optimal.treat==3,1,0)  
names(correct)[[n.t]]<-"out5A"
n.t=n.t+1
out5B<-try(test.ROCI.survival(formula=as.formula(Surv(time1, status1)~treat(treatment1)), data=data1, 
                              NI.margin=-0.1, reference=7, se.method="delta", 
                              treatment.levels=1:7, summary.measure="DS", 
                              tr.model="FP2.select",sig.level=0.05,
                              unfavourable=TRUE, tau=101))
correct[[n.t]]<-ifelse((inherits(out5B,"list"))&&(all.equal(out5B$up.bounds.CI[2],0.2140328, tolerance=10^(-4)))&&out5B$optimal.treat==1,1,0)  
names(correct)[[n.t]]<-"out5B"
n.t=n.t+1
out5C<-try(test.ROCI.survival(formula=as.formula(Surv(time1, status1)~treat(treatment1)), data=data1, 
                              NI.margin=c(-0.1, -0.095,-0.09,-0.085,-0.08,-0.075), reference=7, se.method="delta", 
                              treatment.levels=1:7, summary.measure="DS", 
                              tr.model="FP2.select",sig.level=0.025,
                              unfavourable=TRUE, tau=101))
correct[[n.t]]<-ifelse((inherits(out5C,"list"))&&(all.equal(out5C$up.bounds.CI[2],0.2416069, tolerance=10^(-4)))&&out5C$optimal.treat==3,1,0)  
names(correct)[[n.t]]<-"out5C"
n.t=n.t+1
out5D<-try(test.ROCI.survival(formula=as.formula(Surv(time1, status1)~treat(treatment1)), data=data1, 
                              NI.margin=-0.1, reference=7, se.method="delta", 
                              treatment.levels=1:7, summary.measure="DS", 
                              tr.model="FP2.fixed",sig.level=0.025,
                              unfavourable=TRUE, tau=101))
correct[[n.t]]<-ifelse((inherits(out5D,"list"))&&(all.equal(out5D$up.bounds.CI[6],0.002243763, tolerance=10^(-4)))&&out5D$optimal.treat==7,1,0)  
names(correct)[[n.t]]<-"out5D"
n.t=n.t+1
set.seed(1)
out5E<-try(test.ROCI.survival(formula=as.formula(Surv(time1, status1)~treat(treatment1)), data=data1, 
                              NI.margin=-0.1, reference=7, se.method="bootstrap", 
                              M.boot=10, bootCI.type = "basic",
                              treatment.levels=1:7, summary.measure="DS", 
                              tr.model="FP2.select",sig.level=0.025,
                              unfavourable=TRUE, tau=101))
correct[[n.t]]<-ifelse((inherits(out5E,"list"))&&(all.equal(out5E$up.bounds.CI[2],0.2599099, tolerance=10^(-6)))&&out5E$optimal.treat==2,1,0)  
names(correct)[[n.t]]<-"out5E"
n.t=n.t+1
set.seed(1)
out5F<-try(test.ROCI.survival(formula=as.formula(Surv(time1, status1)~treat(treatment1)), data=data1, 
                              NI.margin=-0.1, reference=7, se.method="bootstrap", 
                              M.boot=10, bootCI.type = "perc",
                              treatment.levels=1:7, summary.measure="DS", 
                              tr.model="FP2.select",sig.level=0.025,
                              unfavourable=TRUE, tau=101))
correct[[n.t]]<-ifelse((inherits(out5F,"list"))&&(all.equal(out5F$up.bounds.CI[2],0.1051021, tolerance=10^(-6)))&&out5F$optimal.treat==3,1,0)  
names(correct)[[n.t]]<-"out5F"
n.t=n.t+1
out5G<-try(test.ROCI.survival(formula=as.formula(Surv(time1, status1)~treat(treatment1)), data=data1, 
                              NI.margin=-0.1, reference=7, se.method="delta", 
                              treatment.levels=c(1.5,5.5,7), summary.measure="DS", 
                              tr.model="FP2.select",sig.level=0.025,
                              unfavourable=TRUE, tau=101))
correct[[n.t]]<-ifelse((inherits(out5G,"list"))&&(all.equal(out5G$up.bounds.CI[2], 0.08078335, tolerance=10^(-4)))&&out5G$optimal.treat==5.5,1,0)  
names(correct)[[n.t]]<-"out5G"
n.t=n.t+1
out5H<-try(test.ROCI.survival(formula=as.formula(Surv(time1, status1)~treat(treatment1)), data=data1, 
                              NI.margin=-0.5, reference=7, se.method="delta", 
                              treatment.levels=1:7, summary.measure="DS", 
                              tr.model="FP2.select",sig.level=0.025,
                              unfavourable=TRUE, tau=101))
correct[[n.t]]<-ifelse((inherits(out5H,"list"))&&(all.equal(out5H$up.bounds.CI[2],0.2416069, tolerance=10^(-4)))&&out5H$optimal.treat==1,1,0)  
names(correct)[[n.t]]<-"out5H"
n.t=n.t+1
out5I<-try(test.ROCI.survival(formula=as.formula(Surv(time1, status1)~treat(treatment1)), data=data1, 
                                                 NI.margin=-0.1, reference=7, se.method="delta", 
                                                 treatment.levels=1:7, summary.measure="DS", 
                                                 tr.model="FP1.fixed",sig.level=0.025,
                                                 unfavourable=TRUE, tau=101))
correct[[n.t]]<-ifelse((inherits(out5I,"list"))&&(all.equal(out5I$up.bounds.CI[2],0.4520623, tolerance=10^(-4)))&&out5I$optimal.treat==4,1,0)  
names(correct)[[n.t]]<-"out5I"
n.t=n.t+1
out5J<-try(test.ROCI.survival(formula=as.formula(Surv(time1, status1)~treat(treatment1)), data=data1, 
                                                 NI.margin=-0.1, reference=7, se.method="delta", 
                                                 treatment.levels=1:7, summary.measure="DS", 
                                                 tr.model="FP1.select",sig.level=0.025,
                                                 unfavourable=TRUE, tau=101))
correct[[n.t]]<-ifelse((inherits(out5J,"list"))&&(all.equal(out5J$up.bounds.CI[2],0.2416069, tolerance=10^(-4)))&&out5J$optimal.treat==3,1,0)  
names(correct)[[n.t]]<-"out5J"
n.t=n.t+1
out5K<-try(test.ROCI.survival(formula=as.formula(Surv(time1, status1)~treat(treatment1)), data=data1, 
                              NI.margin=-0.1, reference=6, se.method="delta", 
                              treatment.levels=1:7, summary.measure="DS", 
                              tr.model="FP2.select",sig.level=0.025,
                              unfavourable=TRUE, tau=101))
correct[[n.t]]<-ifelse((inherits(out5K,"list"))&&(all.equal(out5K$up.bounds.CI[2],0.1869576, tolerance=10^(-4)))&&out5K$optimal.treat==1,1,0)  
names(correct)[[n.t]]<-"out5K"
n.t=n.t+1
out5L<-try(test.ROCI.survival(formula=as.formula(Surv(time1, status1)~treat(treatment1)), data=data1, 
                              NI.margin=0.1, reference=7, se.method="delta", 
                              treatment.levels=1:7, summary.measure="DS", 
                              tr.model="FP2.select",sig.level=0.025,
                              unfavourable=FALSE, tau=101))
correct[[n.t]]<-ifelse((inherits(out5L,"list"))&&(all.equal(out5L$low.bounds.CI[2],-0.1014109, tolerance=10^(-3)))&&out5L$optimal.treat==6,1,0)  
names(correct)[[n.t]]<-"out5L"
n.t=n.t+1
set.seed(1)
status2 = rep(c(1,0),250)
time2 = rnorm(500,100,10)
treatment2 = sample(7,500,rep=TRUE)
age2<-rnorm(500,20,2)
data2<-data.frame(status2, time2, treatment2, age2)
out5M<-try(test.ROCI.survival(formula=as.formula(Surv(time2, status2)~treat(treatment2)+age2), data=data2, 
                              NI.margin=-0.1, reference=7, se.method="delta", 
                              treatment.levels=1:7, summary.measure="DS", 
                              tr.model="FP2.select",sig.level=0.025,
                              unfavourable=TRUE, tau=101))
correct[[n.t]]<-ifelse((inherits(out5M,"list"))&&(all.equal(out5M$up.bounds.CI[2],0.0695364, tolerance=10^(-4)))&&out5M$optimal.treat==2,1,0)  
names(correct)[[n.t]]<-"out5M"
n.t=n.t+1

#####################################################
# Sixth set of checks:
# Now check sample size calculations for certain values on RS scale. 

out6A<-try(test.ROCI.survival(formula=as.formula(Surv(time1, status1)~treat(treatment1)), data=data1, 
                                                 NI.margin=0.6, reference=7, se.method="delta", 
                                                 treatment.levels=1:7, summary.measure="RS", 
                                                 tr.model="FP2.select",sig.level=0.025,
                                                 unfavourable=TRUE, tau=101))
correct[[n.t]]<-ifelse((inherits(out6A,"list"))&&(all.equal(out6A$up.bounds.CI[2],1.410905, tolerance=10^(-4)))&&out6A$optimal.treat==1,1,0)  
names(correct)[[n.t]]<-"out6A"
n.t=n.t+1
out6B<-try(test.ROCI.survival(formula=as.formula(Surv(time1, status1)~treat(treatment1)), data=data1, 
                              NI.margin=0.6, reference=1, se.method="delta", 
                              treatment.levels=1:7, summary.measure="RS", 
                              tr.model="FP2.select",sig.level=0.025,
                              unfavourable=TRUE, tau=101))
correct[[n.t]]<-ifelse((inherits(out6B,"list"))&&(all.equal(out6B$up.bounds.CI[2],1.039852, tolerance=10^(-4)))&&out6B$optimal.treat==7,1,0)  
names(correct)[[n.t]]<-"out6B"
n.t=n.t+1
set.seed(1)
out6C<-try(test.ROCI.survival(formula=as.formula(Surv(time1, status1)~treat(treatment1)), data=data1, 
                              NI.margin=0.6, reference=1, se.method="bootstrap",
                              M.boot=10, bootCI.type = "basic",
                              treatment.levels=1:7, summary.measure="RS", 
                              tr.model="FP2.select",sig.level=0.025,
                              unfavourable=TRUE, tau=101))
correct[[n.t]]<-ifelse((inherits(out6C,"list"))&&(all.equal(out6C$up.bounds.CI[2],1.198023, tolerance=10^(-6)))&&out6C$optimal.treat==6,1,0)  
names(correct)[[n.t]]<-"out6C"
n.t=n.t+1
out6D<-try(test.ROCI.survival(formula=as.formula(Surv(time1, status1)~treat(treatment1)), data=data1, 
                              NI.margin=0.6, reference=1, se.method="delta",
                              treatment.levels=1:7, summary.measure="RS", 
                              tr.model="FP1.fixed",sig.level=0.025,
                              unfavourable=TRUE, tau=101))
correct[[n.t]]<-ifelse((inherits(out6D,"list"))&&(all.equal(out6D$up.bounds.CI[2],2.574475, tolerance=10^(-4)))&&out6D$optimal.treat==1,1,0)  
names(correct)[[n.t]]<-"out6D"
n.t=n.t+1
out6E<-try(test.ROCI.survival(formula=as.formula(Surv(time1, status1)~treat(treatment1)), data=data1, 
                              NI.margin=1.6, reference=1, se.method="delta",
                              treatment.levels=1:7, summary.measure="RS", 
                              tr.model="FP2.select",sig.level=0.025,
                              unfavourable=FALSE, tau=101))
correct[[n.t]]<-ifelse((inherits(out6E,"list"))&&(all.equal(out6E$low.bounds.CI[2],0.8989938, tolerance=10^(-4)))&&out6E$optimal.treat==7,1,0)  
names(correct)[[n.t]]<-"out6E"
n.t=n.t+1

#####################################################
# Seventh set of checks:
# Now check sample size calculations for certain values on DRMST scale. 

out7A<-try(test.ROCI.survival(formula=as.formula(Surv(time1, status1)~treat(treatment1)), data=data1, 
                              NI.margin=-0.6, reference=7, se.method="delta", 
                              treatment.levels=1:7, summary.measure="DRMST", 
                              tr.model="FP2.select",sig.level=0.025,
                              unfavourable=TRUE, tau=101))
correct[[n.t]]<-ifelse((inherits(out7A,"list"))&&(all.equal(out7A$up.bounds.CI[2],1.682833, tolerance=10^(-6)))&&out7A$optimal.treat==4,1,0)  
names(correct)[[n.t]]<-"out7A"
n.t=n.t+1
out7B<-try(test.ROCI.survival(formula=as.formula(Surv(time1, status1)~treat(treatment1)), data=data1, 
                              NI.margin=-0.6, reference=1, se.method="delta", 
                              treatment.levels=1:7, summary.measure="DRMST", 
                              tr.model="FP2.select",sig.level=0.025,
                              unfavourable=TRUE, tau=101))
correct[[n.t]]<-ifelse((inherits(out7B,"list"))&&(all.equal(out7B$up.bounds.CI[2],0.2032345, tolerance=10^(-6)))&&out7B$optimal.treat==3,1,0)  
names(correct)[[n.t]]<-"out7B"
n.t=n.t+1
set.seed(1)
out7C<-try(test.ROCI.survival(formula=as.formula(Surv(time1, status1)~treat(treatment1)), data=data1, 
                              NI.margin=-0.6, reference=7, se.method="delta", 
                              treatment.levels=1:7, summary.measure="DRMST", 
                              tr.model="FP1.fixed",sig.level=0.025,
                              unfavourable=TRUE, tau=101))
correct[[n.t]]<-ifelse((inherits(out7C,"list"))&&(all.equal(out7C$up.bounds.CI[2],3.240699, tolerance=10^(-6)))&&out7C$optimal.treat==4,1,0)  
names(correct)[[n.t]]<-"out7C"
n.t=n.t+1
out7D<-try(test.ROCI.survival(formula=as.formula(Surv(time1, status1)~treat(treatment1)), data=data1, 
                              NI.margin=0.6, reference=7, se.method="delta", 
                              treatment.levels=1:7, summary.measure="DRMST", 
                              tr.model="FP2.select",sig.level=0.025,
                              unfavourable=FALSE, tau=101))
correct[[n.t]]<-ifelse((inherits(out7D,"list"))&&(all.equal(out7D$up.bounds.CI[2],1.682833, tolerance=10^(-6)))&&out7D$optimal.treat==6,1,0)  
names(correct)[[n.t]]<-"out7D"
n.t=n.t+1
set.seed(1)
out7E<-try(test.ROCI.survival(formula=as.formula(Surv(time1, status1)~treat(treatment1)), data=data1, 
                              NI.margin=-0.6, reference=7, se.method="bootstrap",
                              M.boot=10, bootCI.type = "norm",
                              treatment.levels=1:7, summary.measure="DRMST", 
                              tr.model="FP2.select",sig.level=0.025,
                              unfavourable=TRUE, tau=101))
correct[[n.t]]<-ifelse((inherits(out7E,"list"))&&(all.equal(out7E$low.bounds.CI[2],-0.2421177, tolerance=10^(-6)))&&out7E$optimal.treat==2,1,0)  
names(correct)[[n.t]]<-"out7E"
n.t=n.t+1

#####################################################
# Eigth set of checks:
# Now check sample size calculations for certain values on HR scale. 

out8A<-try(test.ROCI.survival(formula=as.formula(Surv(time1, status1)~treat(treatment1)), data=data1, 
                              NI.margin=1.6, reference=7, se.method="delta", 
                              treatment.levels=1:7, summary.measure="HR", 
                              tr.model="FP2.select",sig.level=0.025,
                              unfavourable=TRUE, tau=101))
correct[[n.t]]<-ifelse((inherits(out8A,"list"))&&(all.equal(out8A$up.bounds.CI[2],1.495758, tolerance=10^(-4)))&&out8A$optimal.treat==2,1,0)  
names(correct)[[n.t]]<-"out8A"
n.t=n.t+1
out8B<-try(test.ROCI.survival(formula=as.formula(Surv(time1, status1)~treat(treatment1)), data=data1, 
                              NI.margin=1.6, reference=1, se.method="delta", 
                              treatment.levels=1:7, summary.measure="HR", 
                              tr.model="FP2.select",sig.level=0.025,
                              unfavourable=TRUE, tau=101))
correct[[n.t]]<-ifelse((inherits(out8B,"list"))&&(all.equal(out8B$up.bounds.CI[2],1.487068, tolerance=10^(-4)))&&out8B$optimal.treat==3,1,0)  
names(correct)[[n.t]]<-"out8B"
set.seed(1)
n.t=n.t+1
out8C<-try(test.ROCI.survival(formula=as.formula(Surv(time1, status1)~treat(treatment1)), data=data1, 
                              NI.margin=1.6, reference=7, se.method="delta", 
                              treatment.levels=1:7, summary.measure="HR", 
                              tr.model="FP1.fixed",sig.level=0.025,
                              unfavourable=TRUE, tau=101))
correct[[n.t]]<-ifelse((inherits(out8C,"list"))&&(all.equal(out8C$up.bounds.CI[2], 5.291627, tolerance=10^(-6)))&&out8C$optimal.treat==4,1,0)  
names(correct)[[n.t]]<-"out8C"
n.t=n.t+1
out8D<-try(test.ROCI.survival(formula=as.formula(Surv(time1, status1)~treat(treatment1)), data=data1, 
                              NI.margin=0.6, reference=7, se.method="delta", 
                              treatment.levels=1:7, summary.measure="HR", 
                              tr.model="FP2.select",sig.level=0.025,
                              unfavourable=FALSE, tau=101))
correct[[n.t]]<-ifelse((inherits(out8D,"list"))&&(all.equal(out8D$up.bounds.CI[2], 1.495758, tolerance=10^(-4)))&&out8D$optimal.treat==5,1,0)  
names(correct)[[n.t]]<-"out8D"
n.t=n.t+1
set.seed(1)
out8E<-try(test.ROCI.survival(formula=as.formula(Surv(time1, status1)~treat(treatment1)), data=data1, 
                              NI.margin=1.6, reference=7, se.method="bootstrap",
                              M.boot=10, bootCI.type = "perc",
                              treatment.levels=1:7, summary.measure="HR", 
                              tr.model="FP2.select",sig.level=0.025,
                              unfavourable=TRUE, tau=101))
correct[[n.t]]<-ifelse((inherits(out8E,"list"))&&(all.equal(out8E$low.bounds.CI[2],0.6447801, tolerance=10^(-4)))&&out8E$optimal.treat==4,1,0)  
names(correct)[[n.t]]<-"out8E"
n.t=n.t+1


#####################################################
# Ninth set of checks:
# Now check plot and summary functions:

out9A<-try(plot(out5A, type = "summary.measure"))
correct[[n.t]]<-ifelse(!(inherits(out9A,"try-error")),1,0)  
names(correct)[[n.t]]<-"out9A"
n.t=n.t+1
out9B<-try(plot(out5A, type = "tr.curve"))
correct[[n.t]]<-ifelse(!(inherits(out9B,"try-error")),1,0)  
names(correct)[[n.t]]<-"out9B"
n.t=n.t+1
out9C<-try(plot(out5A, type = "pippo"))
correct[[n.t]]<-ifelse((inherits(out9C,"try-error")),1,0)  
names(correct)[[n.t]]<-"out9C"
n.t=n.t+1
out9D<-try(plot(out6A, type = "summary.measure"))
correct[[n.t]]<-ifelse(!(inherits(out9D,"try-error")),1,0)  
names(correct)[[n.t]]<-"out9D"
n.t=n.t+1
out9E<-try(plot(out6A, type = "tr.curve"))
correct[[n.t]]<-ifelse(!(inherits(out9E,"try-error")),1,0)  
names(correct)[[n.t]]<-"out9E"
n.t=n.t+1
out9F<-try(plot(out7A, type = "summary.measure"))
correct[[n.t]]<-ifelse(!(inherits(out9F,"try-error")),1,0)  
names(correct)[[n.t]]<-"out9F"
n.t=n.t+1
out9G<-try(plot(out7A, type = "tr.curve"))
correct[[n.t]]<-ifelse(!(inherits(out9G,"try-error")),1,0)  
names(correct)[[n.t]]<-"out9G"
n.t=n.t+1
out9H<-try(plot(out8A, type = "summary.measure"))
correct[[n.t]]<-ifelse(!(inherits(out9H,"try-error")),1,0)  
names(correct)[[n.t]]<-"out9H"
n.t=n.t+1
out9I<-try(plot(out8A, type = "tr.curve"))
correct[[n.t]]<-ifelse(!(inherits(out9I,"try-error")),1,0)  
names(correct)[[n.t]]<-"out9I"
n.t=n.t+1

# Now summary function:
out9J<-try(summary(out5A))
correct[[n.t]]<-ifelse(!(inherits(out9J,"try-error"))&&out9J$opt.treat==3,1,0)  
names(correct)[[n.t]]<-"out9J"
n.t=n.t+1
out9K<-try(summary(out6A))
correct[[n.t]]<-ifelse(!(inherits(out9K,"try-error"))&&out9K$opt.treat==1,1,0)  
names(correct)[[n.t]]<-"out9K"
n.t=n.t+1
out9L<-try(summary(out7A))
correct[[n.t]]<-ifelse(!(inherits(out9L,"try-error"))&&out9L$opt.treat==4,1,0)  
names(correct)[[n.t]]<-"out9L"
n.t=n.t+1
out9M<-try(summary(out8A))
correct[[n.t]]<-ifelse(!(inherits(out9M,"try-error"))&&out9M$opt.treat==2,1,0)  
names(correct)[[n.t]]<-"out9M"
n.t=n.t+1

##################################################
#### Now summarise results

vec.correct<-unlist(correct)  # Create vector from list
print(vec.correct)
number.of.tests<-n.t-1   # How many tests did we do?
tot.correct<-sum(vec.correct==1, na.rm = T) # How many tests gave correct result?
tot.incorrect<-sum(vec.correct==0, na.rm = T) # How many test gave wrong result?
tot.NA<-sum(is.na(vec.correct))              # How many test generated an NA?

cat("Testing completed. ", tot.correct, " tests out of ", number.of.tests, " behaved correctly.\n",
    tot.incorrect, " tests out of ", number.of.tests, " behaved incorrectly.\n",
    "An NA was produced for ", tot.NA, " tests out of ", number.of.tests, ".\n")

# Now list incorrect tests
if(tot.incorrect>0) {
  cat("Incorrect tests:\n")
  names(correct)[which(vec.correct==0)]
}
# Now list NA tests
if (tot.NA>0) {
  cat("Tests returning NAs:\n")
  names(correct)[which(is.na(vec.correct))]
}

t.ROCI.s<-(tot.correct==number.of.tests) 


