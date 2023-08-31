###################################################
### test.ROCI.binary testing file             #####
### 28-07-2023                                #####
###################################################

# Load dani:
# library(dani)
library(boot)
library(mfp)
library(marginaleffects)

#Initialise vector of outputs 
correct<-list(NULL)
n.t<-1

#####################################################
# First set of checks:
# Check that it stops for non acceptable values of treatment and outcome

out1A<-try(test.ROCI.binary(outcomes=rbinom(100,1,0.5), NI.margin=0.1))
correct[[n.t]]<-ifelse((inherits(out1A, "try-error"))&&(grepl("One of formula+data or outcomes+treatment necessary", out1A[1], fixed=T  )),1,0) 
names(correct)[[n.t]]<-"out1A"
n.t=n.t+1
out1B<-try(test.ROCI.binary(formula=as.formula("Y~treat"), NI.margin=0.1))
correct[[n.t]]<-ifelse((inherits(out1B, "try-error"))&&(grepl("One of formula+data or outcomes+treatment necessary", out1B[1], fixed=T  )),1,0) 
names(correct)[[n.t]]<-"out1B"
n.t=n.t+1
out1C<-try(test.ROCI.binary(formula=as.formula("Y~treat"), data=matrix(rnorm(100),10,10), NI.margin=0.1))
correct[[n.t]]<-ifelse((inherits(out1C, "try-error"))&&(grepl("is.data.frame(data) is not TRUE", out1C[1], fixed=T )),1,0) 
names(correct)[[n.t]]<-"out1C"
n.t=n.t+1
out1D<-try(test.ROCI.binary(treatment = "prova", outcomes = rbinom(100,1,0.5), NI.margin=0.1))
correct[[n.t]]<-ifelse((inherits(out1D, "try-error"))&&(grepl("is.numeric(treatment) is not TRUE", out1D[1], fixed=T  )),1,0) 
names(correct)[[n.t]]<-"out1D"
n.t=n.t+1
out1E<-try(test.ROCI.binary(outcomes = "prova", treatment = sample(7,100,rep=TRUE), NI.margin=0.1))
correct[[n.t]]<-ifelse((inherits(out1E, "try-error"))&&(grepl("is.numeric(outcomes) is not TRUE", out1E[1], fixed=T  )),1,0) 
names(correct)[[n.t]]<-"out1E"
n.t=n.t+1
out1F<-try(test.ROCI.binary(outcomes = rbinom(10,1,0.5), treatment = sample(7,100,rep=TRUE),  NI.margin=0.1))
correct[[n.t]]<-ifelse((inherits(out1F, "try-error"))&&(grepl("length(outcomes) == length(treatment) is not TRUE", out1F[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out1F"
n.t=n.t+1
out1G<-try(test.ROCI.binary(formula=as.formula("Y~treat"), data = data.frame(Y=rbinom(100,1,0.5)), NI.margin=0.1))
correct[[n.t]]<-ifelse((inherits(out1G, "try-error"))&&(grepl("undefined columns selected", out1G[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out1G"
n.t=n.t+1
out1H<-try(test.ROCI.binary(outcomes = rep(1,100), treatment = sample(7,100,rep=TRUE), NI.margin=0.1))
correct[[n.t]]<-ifelse((inherits(out1H, "try-error"))&&(grepl("nlevels(factor(outcomes)) == 2 is not TRUE", out1H[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out1H"
n.t=n.t+1

#####################################################
# Second set of checks:
# Check that it stops for non-acceptable margins:

out2A<-try(test.ROCI.binary(outcomes = rbinom(100,1,0.5), treatment = sample(7,100,rep=TRUE), NI.margin="0.1"))
correct[[n.t]]<-ifelse((inherits(out2A, "try-error"))&&(grepl("is.numeric(NI.margin) is not TRUE", out2A[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out2A"
n.t=n.t+1
out2B<-try(test.ROCI.binary(outcomes = rbinom(100,1,0.5), treatment = sample(7,100,rep=TRUE), NI.margin=-0.1))
correct[[n.t]]<-ifelse((inherits(out2B, "try-error"))&&(grepl("When outcome is unfavourable, risk difference NI margins need to all be positive", out2B[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out2B"
n.t=n.t+1
out2C<-try(test.ROCI.binary(outcomes = rbinom(100,1,0.5), treatment = sample(7,100,rep=TRUE), NI.margin=0.1, unfavourable = FALSE))
correct[[n.t]]<-ifelse((inherits(out2C, "try-error"))&&(grepl("When outcome is favourable, risk difference NI margins need to all be negative", out2C[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out2C"
n.t=n.t+1
out2D<-try(test.ROCI.binary(outcomes = rbinom(100,1,0.5), treatment = sample(7,100,rep=TRUE), NI.margin=1.1))
correct[[n.t]]<-ifelse((inherits(out2D, "try-error"))&&(grepl("NI margins cannot be greater than 1", out2D[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out2D"
n.t=n.t+1
out2E<-try(test.ROCI.binary(outcomes = rbinom(100,1,0.5), treatment = sample(7,100,rep=TRUE), NI.margin=-1.1, unfavourable=FALSE))
correct[[n.t]]<-ifelse((inherits(out2E, "try-error"))&&(grepl("NI margins cannot be lower than -1", out2E[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out2E"
n.t=n.t+1
out2F<-try(test.ROCI.binary(outcomes = rbinom(100,1,0.5), treatment = sample(7,100,rep=TRUE), NI.margin=0.1, summary.measure = "RR"))
correct[[n.t]]<-ifelse((inherits(out2F, "try-error"))&&(grepl("When outcome is unfavourable, NI margins on the risk ratio or odds ratio scale need to all be >1.", out2F[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out2F"
n.t=n.t+1
out2G<-try(test.ROCI.binary(outcomes = rbinom(100,1,0.5), treatment = sample(7,100,rep=TRUE), NI.margin=1.1, unfavourable=FALSE, summary.measure = "RR"))
correct[[n.t]]<-ifelse((inherits(out2G, "try-error"))&&(grepl("When outcome is favourable, NI margins on the risk ratio or odds ratio scale need to all be <1.", out2G[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out2G"
n.t=n.t+1
out2H<-try(test.ROCI.binary(outcomes = rbinom(100,1,0.5), treatment = sample(7,100,rep=TRUE), NI.margin=0.1, summary.measure = "OR"))
correct[[n.t]]<-ifelse((inherits(out2H, "try-error"))&&(grepl("When outcome is unfavourable, NI margins on the risk ratio or odds ratio scale need to all be >1.", out2H[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out2H"
n.t=n.t+1
out2I<-try(test.ROCI.binary(outcomes = rbinom(100,1,0.5), treatment = sample(7,100,rep=TRUE), NI.margin=1.1, unfavourable=FALSE, summary.measure = "OR"))
correct[[n.t]]<-ifelse((inherits(out2I, "try-error"))&&(grepl("When outcome is favourable, NI margins on the risk ratio or odds ratio scale need to all be <1.", out2I[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out2I"
n.t=n.t+1
out2J<-try(test.ROCI.binary(outcomes = rbinom(100,1,0.5), treatment = sample(7,100,rep=TRUE), NI.margin=c(0.1, 0.12)))
correct[[n.t]]<-ifelse((inherits(out2J, "try-error"))&&(grepl("length(NI.margin) == (length(treatment.levels)", out2J[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out2J"
n.t=n.t+1
out2K<-try(test.ROCI.binary(outcomes = rbinom(100,1,0.5), treatment = sample(7,100,rep=TRUE), NI.margin=c(0.1, 0.09,0.08,0.07,0.06,0.05,0.04)))
correct[[n.t]]<-ifelse((inherits(out2K, "try-error"))&&(grepl("length(NI.margin) == (length(treatment.levels)", out2K[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out2K"
n.t=n.t+1
out2L<-try(test.ROCI.binary(outcomes = rbinom(100,1,0.5), treatment = sample(7,100,rep=TRUE), NI.margin=c(0.1, 0.09,0.08,0.07,0.06,0.05), summary.measure = "target.risk"))
correct[[n.t]]<-ifelse((inherits(out2L, "try-error"))&&(grepl("length(NI.margin) == (length(treatment.levels)", out2L[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out2L"
n.t=n.t+1
out2M<-try(test.ROCI.binary(outcomes = rbinom(100,1,0.5), treatment = sample(7,100,rep=TRUE), NI.margin=-0.1, summary.measure = "RR", unfavourable=FALSE))
correct[[n.t]]<-ifelse((inherits(out2M, "try-error"))&&(grepl("A risk/odds ratio margin must be >0.", out2M[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out2M"
n.t=n.t+1

#####################################################
# Third set of checks:
# Check that it stops for unacceptable values of significance level:

out3A<-try(test.ROCI.binary(outcomes = rbinom(100,1,0.5), treatment = sample(7,100,rep=TRUE), NI.margin=0.1, sig.level = "0.025"))
correct[[n.t]]<-ifelse((inherits(out3A, "try-error"))&&(grepl("is.numeric(sig.level) is not TRUE", out3A[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out3A"
n.t=n.t+1
out3B<-try(test.ROCI.binary(outcomes = rbinom(100,1,0.5), treatment = sample(7,100,rep=TRUE), NI.margin=0.1, sig.level = 0))
correct[[n.t]]<-ifelse((inherits(out3B, "try-error"))&&(grepl("sig.level > 0 is not TRUE", out3B[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out3B"
n.t=n.t+1
out3C<-try(test.ROCI.binary(outcomes = rbinom(100,1,0.5), treatment = sample(7,100,rep=TRUE), NI.margin=0.1, sig.level = 1))
correct[[n.t]]<-ifelse((inherits(out3C, "try-error"))&&(grepl("sig.level < 0.5 is not TRUE", out3C[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out3C"
n.t=n.t+1

#####################################################
# Fourth set of checks:
# Check with other wrong arguments:

out4A<-try(test.ROCI.binary(outcomes = rbinom(100,1,0.5), treatment = sample(7,100,rep=TRUE), NI.margin=0.1, summary.measure = "pippo"))
correct[[n.t]]<-ifelse((inherits(out4A, "try-error"))&&(grepl("summary.measure ==", out4A[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4A"
n.t=n.t+1

# Check that it works when minimisation incorrectly specified:
out4B<-try(test.ROCI.binary(outcomes = rbinom(100,1,0.5), treatment = sample(7,100,rep=TRUE), NI.margin=0.1, minimisation=NA))
correct[[n.t]]<-ifelse((inherits(out4B, "try-error"))&&(grepl("!is.na(minimisation) is not TRUE", out4B[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4B"
n.t=n.t+1

# Check that it works when se.method incorrectly specified:
out4C<-try(test.ROCI.binary(outcomes = rbinom(100,1,0.5), treatment = sample(7,100,rep=TRUE), NI.margin=0.1, se.method=NA))
correct[[n.t]]<-ifelse((inherits(out4C, "try-error"))&&(grepl("is.character(se.method) is not TRUE", out4C[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4C"
n.t=n.t+1
out4D<-try(test.ROCI.binary(outcomes = rbinom(100,1,0.5), treatment = sample(7,100,rep=TRUE), NI.margin=0.1, se.method="pippo"))
correct[[n.t]]<-ifelse((inherits(out4D, "try-error"))&&(grepl("se.method %in% c", out4D[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4D"
n.t=n.t+1

# Check that it works when unfavourable incorrectly specified:
out4E<-try(test.ROCI.binary(outcomes = rbinom(100,1,0.5), treatment = sample(7,100,rep=TRUE), NI.margin=0.1, unfavourable = "pippo"))
correct[[n.t]]<-ifelse((inherits(out4E, "try-error"))&&(grepl("is.logical(unfavourable) is not TRUE", out4E[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4E"
n.t=n.t+1
out4F<-try(test.ROCI.binary(outcomes = rbinom(100,1,0.5), treatment = sample(7,100,rep=TRUE), NI.margin=0.1, unfavourable = NA))
correct[[n.t]]<-ifelse((inherits(out4F, "try-error"))&&(grepl("!is.na(unfavourable) is not TRUE", out4F[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4F"
n.t=n.t+1

# Check that M.boot has acceptable value:
out4G<-try(test.ROCI.binary(outcomes = rbinom(100,1,0.5), treatment = sample(7,100,rep=TRUE), NI.margin=0.1, se.method = "bootstrap", M.boot = "100"))
correct[[n.t]]<-ifelse((inherits(out4G, "try-error"))&&(grepl("is.numeric(M.boot) is not TRUE", out4G[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4G"
n.t=n.t+1
out4H<-try(test.ROCI.binary(outcomes = rbinom(100,1,0.5), treatment = sample(7,100,rep=TRUE), NI.margin=0.1, se.method = "bootstrap", M.boot = 0))
correct[[n.t]]<-ifelse((inherits(out4H, "try-error"))&&(grepl("M.boot > 1 is not TRUE", out4H[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4H"
n.t=n.t+1

# Check that tr.model has acceptable value:
out4I<-try(test.ROCI.binary(outcomes = rbinom(100,1,0.5), treatment = sample(7,100,rep=TRUE), NI.margin=0.1, tr.model = NA))
correct[[n.t]]<-ifelse((inherits(out4I, "try-error"))&&(grepl("is.character(tr.model) is not TRUE", out4I[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4I"
n.t=n.t+1
out4J<-try(test.ROCI.binary(outcomes = rbinom(100,1,0.5), treatment = sample(7,100,rep=TRUE), NI.margin=0.1, tr.model = "pippo"))
correct[[n.t]]<-ifelse((inherits(out4J, "try-error"))&&(grepl("tr.model %in% c", out4J[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4J"
n.t=n.t+1

# Check that it works when treatment.levels incorrectly specified:
out4K<-try(test.ROCI.binary(outcomes = rbinom(100,1,0.5), treatment = sample(7,100,rep=TRUE), NI.margin=0.1, treatment.levels = c("1", "3", "5", "7")))
correct[[n.t]]<-ifelse((inherits(out4K, "try-error"))&&(grepl("is.numeric(treatment.levels) is not TRUE", out4K[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4K"
n.t=n.t+1
out4L<-try(test.ROCI.binary(outcomes = rbinom(100,1,0.5), treatment = sample(7,100,rep=TRUE), NI.margin=0.1, treatment.levels = c(1,7)))
correct[[n.t]]<-ifelse((inherits(out4L, "try-error"))&&(grepl("length(treatment.levels) > 2 is not TRUE", out4L[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4L"
n.t=n.t+1

# Check that it works when bootCI.type incorrectly specified:
out4M<-try(test.ROCI.binary(outcomes = rbinom(100,1,0.5), treatment = sample(7,100,rep=TRUE), NI.margin=0.1, se.method="bootstrap", bootCI.type = 2))
correct[[n.t]]<-ifelse((inherits(out4M, "try-error"))&&(grepl("is.character(bootCI.type) is not TRUE", out4M[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4M"
n.t=n.t+1
out4N<-try(test.ROCI.binary(outcomes = rbinom(100,1,0.5), treatment = sample(7,100,rep=TRUE), NI.margin=0.1, se.method="bootstrap", bootCI.type = "pippo"))
correct[[n.t]]<-ifelse((inherits(out4N, "try-error"))&&(grepl("bootCI.type %in% c", out4N[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4N"
n.t=n.t+1

# Check that it works when parallel incorrectly specified:
out4O<-try(test.ROCI.binary(outcomes = rbinom(100,1,0.5), treatment = sample(7,100,rep=TRUE), NI.margin=0.1, se.method="bootstrap", parallel=NA))
correct[[n.t]]<-ifelse((inherits(out4O, "try-error"))&&(grepl("'arg' must be NULL or a character vector", out4O[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4O"
n.t=n.t+1
out4P<-try(test.ROCI.binary(outcomes = rbinom(100,1,0.5), treatment = sample(7,100,rep=TRUE), NI.margin=0.1, se.method="bootstrap", parallel="pippo"))
correct[[n.t]]<-ifelse((inherits(out4P, "try-error"))&&(grepl("should be one of", out4P[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4P"
n.t=n.t+1

# Check behavior with wrong n.cpus
out4Q<-try(test.ROCI.binary(outcomes = rbinom(100,1,0.5), treatment = sample(7,100,rep=TRUE), NI.margin=0.1, se.method = "bootstrap", parallel="snow", n.cpus = "pippo"))
correct[[n.t]]<-ifelse((inherits(out4Q, "try-error"))&&(grepl("invalid 'times' argument", out4Q[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4Q"
n.t=n.t+1

#####################################################
# Fifth set of checks:
# Now check test for certain values on RD scale. 

set.seed(1)
outcomes1<-rbinom(500,1,0.8)
treatment1<-sample(7,500, rep=TRUE)
data1<-data.frame(outcomes1, treatment1)
out5A<-try(test.ROCI.binary(formula="outcomes1~treat(treatment1)", data=data1, 
                            NI.margin=0.1, minimisation=T, se.method="delta", 
                            treatment.levels=1:7, summary.measure="RD", 
                            tr.model="FP2.classic",sig.level=0.025,
                            unfavourable=TRUE))
correct[[n.t]]<-ifelse((inherits(out5A,"list"))&&(all.equal(out5A$up.bounds.CI[2],0.1805203, tolerance=10^(-6)))&&out5A$optimal.treat==5,1,0)  
names(correct)[[n.t]]<-"out5A"
n.t=n.t+1
out5B<-try(test.ROCI.binary(treatment=treatment1, outcomes = outcomes1, 
                            NI.margin=0.1, minimisation=T, se.method="delta", 
                            treatment.levels=1:7, summary.measure="RD", 
                            tr.model="FP2.classic",sig.level=0.025,
                            unfavourable=TRUE))
correct[[n.t]]<-ifelse((inherits(out5B,"list"))&&(all.equal(out5B$up.bounds.CI[2],0.1805203, tolerance=10^(-6)))&&out5B$optimal.treat==5,1,0)  
names(correct)[[n.t]]<-"out5B"
n.t=n.t+1
out5C<-try(test.ROCI.binary(formula="outcomes1~treat(treatment1)", data=data1, 
                            NI.margin=c(0.1, 0.095,0.09,0.085,0.08,0.075), minimisation=T, se.method="delta", 
                            treatment.levels=1:7, summary.measure="RD", 
                            tr.model="FP2.classic",sig.level=0.025,
                            unfavourable=TRUE))
correct[[n.t]]<-ifelse((inherits(out5C,"list"))&&(all.equal(out5C$up.bounds.CI[2],0.1805203, tolerance=10^(-6)))&&out5C$optimal.treat==6,1,0)  
names(correct)[[n.t]]<-"out5C"
n.t=n.t+1
out5D<-try(test.ROCI.binary(formula="outcomes1~treat(treatment1)", data=data1, 
                            NI.margin=0.1, minimisation=F, se.method="delta", 
                            treatment.levels=1:7, summary.measure="RD", 
                            tr.model="FP2.classic",sig.level=0.025,
                            unfavourable=TRUE))
correct[[n.t]]<-ifelse((inherits(out5D,"list"))&&(all.equal(out5D$up.bounds.CI[2],-0.002217021, tolerance=10^(-6)))&&out5D$optimal.treat==7,1,0)  
names(correct)[[n.t]]<-"out5D"
n.t=n.t+1
set.seed(1)
out5E<-try(test.ROCI.binary(formula="outcomes1~treat(treatment1)", data=data1, 
                            NI.margin=0.1, minimisation=T, se.method="bootstrap",
                            M.boot=100, bootCI.type = "basic",
                            treatment.levels=1:7, summary.measure="RD", 
                            tr.model="FP2.classic",sig.level=0.025,
                            unfavourable=TRUE))
correct[[n.t]]<-ifelse((inherits(out5E,"list"))&&(all.equal(out5E$up.bounds.CI[2],0.173634, tolerance=10^(-6)))&&out5E$optimal.treat==6,1,0)  
names(correct)[[n.t]]<-"out5E"
n.t=n.t+1
set.seed(1)
out5F<-try(test.ROCI.binary(formula="outcomes1~treat(treatment1)", data=data1, 
                            NI.margin=0.1, minimisation=T, se.method="bootstrap",
                            M.boot=100, bootCI.type = "perc",
                            treatment.levels=1:7, summary.measure="RD", 
                            tr.model="FP2.classic",sig.level=0.025,
                            unfavourable=TRUE))
correct[[n.t]]<-ifelse((inherits(out5F,"list"))&&(all.equal(out5F$up.bounds.CI[2],0.2201005, tolerance=10^(-6)))&&out5F$optimal.treat==6,1,0)  
names(correct)[[n.t]]<-"out5F"
n.t=n.t+1
out5G<-try(test.ROCI.binary(formula="outcomes1~treat(treatment1)", data=data1, 
                            NI.margin=0.1, minimisation=T, se.method="delta", 
                            treatment.levels=c(1.5,5.5,7), summary.measure="RD", 
                            tr.model="FP2.classic",sig.level=0.025,
                            unfavourable=TRUE))
correct[[n.t]]<-ifelse((inherits(out5G,"list"))&&(all.equal(out5G$up.bounds.CI[2], 0.06354068, tolerance=10^(-6)))&&out5G$optimal.treat==5.5,1,0)  
names(correct)[[n.t]]<-"out5G"
n.t=n.t+1
out5H<-try(test.ROCI.binary(formula="outcomes1~treat(treatment1)", data=data1, 
                            NI.margin=0.1, minimisation=T, se.method="delta", 
                            treatment.levels=1:7, summary.measure="RD", 
                            tr.model="FP2.fixed",sig.level=0.025,
                            unfavourable=TRUE))
correct[[n.t]]<-ifelse((inherits(out5H,"list"))&&(all.equal(out5H$up.bounds.CI[2],0.2273355, tolerance=10^(-6)))&&out5H$optimal.treat==5,1,0)  
names(correct)[[n.t]]<-"out5H"
n.t=n.t+1
out5I<-try(test.ROCI.binary(formula="outcomes1~treat(treatment1)", data=data1, 
                            NI.margin=0.1, minimisation=T, se.method="delta", 
                            treatment.levels=1:7, summary.measure="RD", 
                            tr.model="FP1.fixed",sig.level=0.025,
                            unfavourable=TRUE))
correct[[n.t]]<-ifelse((inherits(out5I,"list"))&&(all.equal(out5I$up.bounds.CI[2],0.2020502, tolerance=10^(-6)))&&out5I$optimal.treat==6,1,0)  
names(correct)[[n.t]]<-"out5I"
n.t=n.t+1
out5J<-try(test.ROCI.binary(formula="outcomes1~treat(treatment1)", data=data1, 
                            NI.margin=0.1, minimisation=T, se.method="delta", 
                            treatment.levels=1:7, summary.measure="RD", 
                            tr.model="FP1.classic",sig.level=0.025,
                            unfavourable=TRUE))
correct[[n.t]]<-ifelse((inherits(out5J,"list"))&&(all.equal(out5J$up.bounds.CI[2],0.1805203, tolerance=10^(-6)))&&out5J$optimal.treat==5,1,0)  
names(correct)[[n.t]]<-"out5J"
n.t=n.t+1
out5K<-try(test.ROCI.binary(formula="outcomes1~treat(treatment1)", data=data1, 
                            NI.margin=0.1, minimisation=T, se.method="delta", 
                            treatment.levels=1:7, summary.measure="RD", 
                            tr.model="FP2.classic",sig.level=0.05,
                            unfavourable=TRUE))
correct[[n.t]]<-ifelse((inherits(out5K,"list"))&&(all.equal(out5K$up.bounds.CI[2],0.1654666, tolerance=10^(-6)))&&out5K$optimal.treat==5,1,0)  
names(correct)[[n.t]]<-"out5K"
n.t=n.t+1
out5L<-try(test.ROCI.binary(formula="outcomes1~treat(treatment1)", data=data1, 
                            NI.margin=-0.1, minimisation=T, se.method="delta", 
                            treatment.levels=1:7, summary.measure="RD", 
                            tr.model="FP2.classic",sig.level=0.025,
                            unfavourable=FALSE))
correct[[n.t]]<-ifelse((inherits(out5L,"list"))&&(all.equal(out5L$low.bounds.CI[2],-0.006744666, tolerance=10^(-6)))&&out5L$optimal.treat==1,1,0)  
names(correct)[[n.t]]<-"out5L"
n.t=n.t+1

#####################################################
# Sixth set of checks:
# Now check sample size calculations for certain values on RR scale. 

out6A<-try(test.ROCI.binary(formula="outcomes1~treat(treatment1)", data=data1, 
                            NI.margin=1.5, minimisation=T, se.method="delta", 
                            treatment.levels=1:7, summary.measure="RR", 
                            tr.model="FP2.classic",sig.level=0.025,
                            unfavourable=TRUE))
correct[[n.t]]<-ifelse((inherits(out6A,"list"))&&(all.equal(out6A$up.bounds.CI[2],1.256398, tolerance=10^(-6)))&&out6A$optimal.treat==1,1,0)  
names(correct)[[n.t]]<-"out6A"
n.t=n.t+1
out6B<-try(test.ROCI.binary(formula="outcomes1~treat(treatment1)", data=data1, 
                            NI.margin=1.5, minimisation=F, se.method="delta", 
                            treatment.levels=1:7, summary.measure="RR", 
                            tr.model="FP2.classic",sig.level=0.025,
                            unfavourable=TRUE))
correct[[n.t]]<-ifelse((inherits(out6B,"list"))&&(all.equal(out6B$up.bounds.CI[2],0.99675, tolerance=10^(-6)))&&out6B$optimal.treat==7,1,0)  
names(correct)[[n.t]]<-"out6B"
n.t=n.t+1
set.seed(1)
out6C<-try(test.ROCI.binary(formula="outcomes1~treat(treatment1)", data=data1, 
                            NI.margin=1.5, minimisation=T, se.method="bootstrap", 
                            M.boot=100, bootCI.type = "perc",
                            treatment.levels=1:7, summary.measure="RR", 
                            tr.model="FP2.classic",sig.level=0.025,
                            unfavourable=TRUE))
correct[[n.t]]<-ifelse((inherits(out6C,"list"))&&(all.equal(out6C$up.bounds.CI[2],1.328252, tolerance=10^(-6)))&&out6C$optimal.treat==1,1,0)  
names(correct)[[n.t]]<-"out6C"
n.t=n.t+1
out6D<-try(test.ROCI.binary(formula="outcomes1~treat(treatment1)", data=data1, 
                            NI.margin=1.5, minimisation=T, se.method="delta", 
                            treatment.levels=1:7, summary.measure="RR", 
                            tr.model="FP1.fixed",sig.level=0.025,
                            unfavourable=TRUE))
correct[[n.t]]<-ifelse((inherits(out6D,"list"))&&(all.equal(out6D$up.bounds.CI[2],1.295357, tolerance=10^(-6)))&&out6D$optimal.treat==1,1,0)  
names(correct)[[n.t]]<-"out6D"
n.t=n.t+1
out6E<-try(test.ROCI.binary(formula="outcomes1~treat(treatment1)", data=data1, 
                            NI.margin=0.75, minimisation=T, se.method="delta", 
                            treatment.levels=1:7, summary.measure="RR", 
                            tr.model="FP2.classic",sig.level=0.025,
                            unfavourable=FALSE))
correct[[n.t]]<-ifelse((inherits(out6E,"list"))&&(all.equal(out6E$low.bounds.CI[2],0.9891393, tolerance=10^(-6)))&&out6E$optimal.treat==1,1,0)  
names(correct)[[n.t]]<-"out6E"
n.t=n.t+1

#####################################################
# Seventh set of checks:
# Now check sample size calculations for certain values on OR scale. 

out7A<-try(test.ROCI.binary(formula="outcomes1~treat(treatment1)", data=data1, 
                            NI.margin=1.5, minimisation=T, se.method="delta", 
                            treatment.levels=1:7, summary.measure="OR", 
                            tr.model="FP2.classic",sig.level=0.025,
                            unfavourable=TRUE))
correct[[n.t]]<-ifelse((inherits(out7A,"list"))&&(all.equal(out7A$up.bounds.CI[2],3.09873, tolerance=10^(-6)))&&out7A$optimal.treat==6,1,0)  
names(correct)[[n.t]]<-"out7A"
n.t=n.t+1
out7B<-try(test.ROCI.binary(formula="outcomes1~treat(treatment1)", data=data1, 
                            NI.margin=1.5, minimisation=F, se.method="delta", 
                            treatment.levels=1:7, summary.measure="OR", 
                            tr.model="FP2.classic",sig.level=0.025,
                            unfavourable=TRUE))
correct[[n.t]]<-ifelse((inherits(out7B,"list"))&&(all.equal(out7B$up.bounds.CI[2],1.00581, tolerance=10^(-6)))&&out7B$optimal.treat==7,1,0)  
names(correct)[[n.t]]<-"out7B"
n.t=n.t+1
set.seed(1)
out7C<-try(test.ROCI.binary(formula="outcomes1~treat(treatment1)", data=data1, 
                            NI.margin=1.5, minimisation=T, se.method="bootstrap", 
                            M.boot=100, bootCI.type = "perc",
                            treatment.levels=1:7, summary.measure="OR", 
                            tr.model="FP2.classic",sig.level=0.025,
                            unfavourable=TRUE))
correct[[n.t]]<-ifelse((inherits(out7C,"list"))&&(all.equal(out7C$up.bounds.CI[2],5.739898, tolerance=10^(-6)))&&out7C$optimal.treat==6,1,0)  
names(correct)[[n.t]]<-"out7C"
n.t=n.t+1
out7D<-try(test.ROCI.binary(formula="outcomes1~treat(treatment1)", data=data1, 
                            NI.margin=1.5, minimisation=T, se.method="delta", 
                            treatment.levels=1:7, summary.measure="OR", 
                            tr.model="FP1.fixed",sig.level=0.025,
                            unfavourable=TRUE))
correct[[n.t]]<-ifelse((inherits(out7D,"list"))&&(all.equal(out7D$up.bounds.CI[2],3.413271, tolerance=10^(-6)))&&out7D$optimal.treat==6,1,0)  
names(correct)[[n.t]]<-"out7D"
n.t=n.t+1
out7E<-try(test.ROCI.binary(formula="outcomes1~treat(treatment1)", data=data1, 
                            NI.margin=0.75, minimisation=T, se.method="delta", 
                            treatment.levels=1:7, summary.measure="OR", 
                            tr.model="FP2.classic",sig.level=0.025,
                            unfavourable=FALSE))
correct[[n.t]]<-ifelse((inherits(out7E,"list"))&&(all.equal(out7E$low.bounds.CI[2],0.9714512, tolerance=10^(-6)))&&out7E$optimal.treat==1,1,0)  
names(correct)[[n.t]]<-"out7E"
n.t=n.t+1

#####################################################
# Eigth set of checks:
# Now check sample size calculations for certain values on target risk scale. 

out8A<-try(test.ROCI.binary(formula="outcomes1~treat(treatment1)", data=data1, 
                            NI.margin=0.8, minimisation=T, se.method="delta", 
                            treatment.levels=1:7, summary.measure="target.risk", 
                            tr.model="FP2.classic",sig.level=0.025,
                            unfavourable=TRUE))
correct[[n.t]]<-ifelse((inherits(out8A,"list"))&&(all.equal(out8A$up.bounds.CI[2],0.8895169, tolerance=10^(-6)))&&out8A$optimal.treat==7,1,0)  
names(correct)[[n.t]]<-"out8A"
n.t=n.t+1
out8B<-try(test.ROCI.binary(formula="outcomes1~treat(treatment1)", data=data1, 
                            NI.margin=0.8, minimisation=F, se.method="delta", 
                            treatment.levels=1:7, summary.measure="target.risk", 
                            tr.model="FP2.classic",sig.level=0.025,
                            unfavourable=TRUE))
correct[[n.t]]<-ifelse((inherits(out8B,"list"))&&(all.equal(out8B$up.bounds.CI[2],0.8895169, tolerance=10^(-6)))&&out8B$optimal.treat==1,1,0)  
names(correct)[[n.t]]<-"out8B"
set.seed(1)
n.t=n.t+1
out8C<-try(test.ROCI.binary(formula="outcomes1~treat(treatment1)", data=data1, 
                            NI.margin=0.8, minimisation=T, se.method="bootstrap", 
                            M.boot=100, bootCI.type = "perc",
                            treatment.levels=1:7, summary.measure="target.risk", 
                            tr.model="FP2.classic",sig.level=0.025,
                            unfavourable=TRUE))
correct[[n.t]]<-ifelse((inherits(out8C,"list"))&&(all.equal(out8C$up.bounds.CI[2], 0.9367394, tolerance=10^(-6)))&&out8C$optimal.treat==7,1,0)  
names(correct)[[n.t]]<-"out8C"
n.t=n.t+1
out8D<-try(test.ROCI.binary(formula="outcomes1~treat(treatment1)", data=data1, 
                            NI.margin=0.8, minimisation=T, se.method="delta", 
                            treatment.levels=1:7, summary.measure="target.risk", 
                            tr.model="FP1.fixed",sig.level=0.025,
                            unfavourable=TRUE))
correct[[n.t]]<-ifelse((inherits(out8D,"list"))&&(all.equal(out8D$up.bounds.CI[2], 0.8873118, tolerance=10^(-6)))&&out8D$optimal.treat==7,1,0)  
names(correct)[[n.t]]<-"out8D"
n.t=n.t+1
out8E<-try(test.ROCI.binary(formula="outcomes1~treat(treatment1)", data=data1, 
                            NI.margin=0.75, minimisation=T, se.method="delta", 
                            treatment.levels=1:7, summary.measure="target.risk", 
                            tr.model="FP2.classic",sig.level=0.025,
                            unfavourable=FALSE))
correct[[n.t]]<-ifelse((inherits(out8E,"list"))&&(all.equal(out8E$low.bounds.CI[2],0.7981392, tolerance=10^(-6)))&&out8E$optimal.treat==1,1,0)  
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
correct[[n.t]]<-ifelse(!(inherits(out9J,"try-error"))&&out9J$opt.treat==5,1,0)  
names(correct)[[n.t]]<-"out9J"
n.t=n.t+1
out9K<-try(summary(out6A))
correct[[n.t]]<-ifelse(!(inherits(out9K,"try-error"))&&out9K$opt.treat==1,1,0)  
names(correct)[[n.t]]<-"out9K"
n.t=n.t+1
out9L<-try(summary(out7A))
correct[[n.t]]<-ifelse(!(inherits(out9L,"try-error"))&&out9L$opt.treat==6,1,0)  
names(correct)[[n.t]]<-"out9L"
n.t=n.t+1
out9M<-try(summary(out8A))
correct[[n.t]]<-ifelse(!(inherits(out9M,"try-error"))&&out9M$opt.treat==7,1,0)  
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

t.ROCI.b<-(tot.correct==number.of.tests) 


