###################################################
### test.NI.survival testing file             #####
### 26-07-2023                                #####
###################################################

# Load dani:
# library(dani)
library(survival)
library(coxphw)
library(survRM2)
library(DescTools)
library(boot)
library(flexsurv)
library(numDeriv)
library(tibble)
library(pracma)

###############################################################################################################################################
### Here we test the test.NI.survival function

#Initialise vector of outputs 
correct<-list(NULL)
n.t<-1

#####################################################
# First set of checks:
# Non-acceptable values of treatment and outcome
# Check that it stops for non acceptable values of time:
out1A<-try(test.NI.survival("100", rbinom(100,1,0.5), rep(0:1,each=50), NI.margin=1.5))
correct[[n.t]]<-ifelse((inherits(out1A, "try-error"))&&(grepl("is.numeric(time) is not TRUE", out1A[1], fixed=T  )),1,0) 
names(correct)[[n.t]]<-"out1A"
n.t=n.t+1
out1B<-try(test.NI.survival(100, rbinom(100,1,0.5), rep(0:1,each=50), NI.margin=1.5))
correct[[n.t]]<-ifelse((inherits(out1B, "try-error"))&&(grepl("length(time) > 2 is not TRUE", out1B[1], fixed=T  )),1,0) 
names(correct)[[n.t]]<-"out1B"
n.t=n.t+1
out1C<-try(test.NI.survival(rnorm(10,100,0.5), rbinom(100,1,0.5), rep(0:1,each=50), NI.margin=1.5))
correct[[n.t]]<-ifelse((inherits(out1C, "try-error"))&&(grepl("length(time) == length(event) is not TRUE", out1C[1], fixed=T )),1,0) 
names(correct)[[n.t]]<-"out1C"
n.t=n.t+1
out1D<-try(test.NI.survival(rnorm(100,0,0.5), rbinom(100,1,0.5), rep(0:1,each=50), NI.margin= 1.5))
correct[[n.t]]<-ifelse((inherits(out1D, "try-error"))&&(grepl("all(time > 0) is not TRUE", out1D[1], fixed=T  )),1,0) 
names(correct)[[n.t]]<-"out1D"
n.t=n.t+1

# Check that it stops for non acceptable values of event indicator:
out1E<-try(test.NI.survival(rnorm(100,100,1), "100", rep(0:1,each=50), NI.margin= 1.5))
correct[[n.t]]<-ifelse((inherits(out1E, "try-error"))&&(grepl("length(time) == length(event) is not TRUE", out1E[1], fixed=T  )),1,0) 
names(correct)[[n.t]]<-"out1E"
n.t=n.t+1
out1F<-try(test.NI.survival(rnorm(100,100,1), c(0,1), rep(0:1,each=50), NI.margin= 1.5))
correct[[n.t]]<-ifelse((inherits(out1F, "try-error"))&&(grepl("length(time) == length(event) is not TRUE", out1F[1], fixed=T  )),1,0) 
names(correct)[[n.t]]<-"out1F"
n.t=n.t+1

# Check that it stops for non acceptable values of treatment indicator:
out1G<-try(test.NI.survival(rnorm(100,100,1), rbinom(100,1,0.5), "pippo", NI.margin= 1.5))
correct[[n.t]]<-ifelse((inherits(out1G, "try-error"))&&(grepl("nlevels(as.factor(treat)) == 2 is not TRUE", out1G[1], fixed=T  )),1,0) 
names(correct)[[n.t]]<-"out1G"
n.t=n.t+1
out1H<-try(test.NI.survival(rnorm(100,100,1), rbinom(100,1,0.5), rep(0:1,each=250), NI.margin=1.5))
correct[[n.t]]<-ifelse((inherits(out1H, "try-error"))&&(grepl("length(time) == length(treat) is not TRUE", out1H[1], fixed=T  )),1,0) 
names(correct)[[n.t]]<-"out1H"
n.t=n.t+1

#####################################################
# Second set of checks:
# Check that it stops for non acceptable values of NI margins:

out2A<-try(test.NI.survival(rnorm(100,100,1), rbinom(100,1,0.5), rep(0:1,each=50), NI.margin= "1.5"))
correct[[n.t]]<-ifelse((inherits(out2A, "try-error"))&&(grepl("is.numeric(NI.margin) is not TRUE", out2A[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out2A"
n.t=n.t+1
out2B<-try(test.NI.survival(rnorm(100,100,1), rbinom(100,1,0.5), rep(0:1,each=50), NI.margin= 1))
correct[[n.t]]<-ifelse((inherits(out2B, "try-error"))&&(grepl("When events are unfavourable (e.g. deaths), a HR NI margin needs to be >1.", out2B[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out2B"
n.t=n.t+1
out2C<-try(test.NI.survival(rnorm(100,100,1), rbinom(100,1,0.5), rep(0:1,each=50), NI.margin= 1.5, unfavourable = F))
correct[[n.t]]<-ifelse((inherits(out2C, "try-error"))&&(grepl("When events are favourable (e.g. cure), a HR NI margin needs to be <1.", out2C[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out2C"
n.t=n.t+1
out2D<-try(test.NI.survival(rnorm(100,100,1), rbinom(100,1,0.5), rep(0:1,each=50), unfavourable=F, NI.margin= -1))
correct[[n.t]]<-ifelse((inherits(out2D, "try-error"))&&(grepl("NI.margin > 0 is not TRUE", out2D[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out2D"
n.t=n.t+1
out2E<-try(test.NI.survival(rnorm(100,100,1), rbinom(100,1,0.5), rep(0:1,each=50), NI.margin= 1, summary.measure="DRMST", tau=5))
correct[[n.t]]<-ifelse((inherits(out2E, "try-error"))&&(grepl("When events are unfavourable (e.g. death), a NI margin as a difference in RMST needs to be <0.", out2E[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out2E"
n.t=n.t+1
out2F<-try(test.NI.survival(rnorm(100,100,1), rbinom(100,1,0.5), rep(0:1,each=50), NI.margin= -1, summary.measure="DRMST", unfavourable=F, tau=5))
correct[[n.t]]<-ifelse((inherits(out2F, "try-error"))&&(grepl("When events are favourable (e.g. cure), a NI margin as a difference in RMST needs to be >0.", out2F[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out2F"
n.t=n.t+1
out2G<-try(test.NI.survival(rnorm(100,100,1), rbinom(100,1,0.5), rep(0:1,each=50), NI.margin= 0.1, summary.measure="DS", tau=5))
correct[[n.t]]<-ifelse((inherits(out2G, "try-error"))&&(grepl("When events are unfavourable (e.g. death), a NI margin as a difference in survival needs to be <0.", out2G[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out2G"
n.t=n.t+1
out2H<-try(test.NI.survival(rnorm(100,100,1), rbinom(100,1,0.5), rep(0:1,each=50), NI.margin= -0.1, summary.measure="DS", unfavourable=F, tau=5))
correct[[n.t]]<-ifelse((inherits(out2H, "try-error"))&&(grepl("When events are favourable (e.g. cure), a NI margin as a difference in survival needs to be >0.", out2H[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out2H"
n.t=n.t+1

#####################################################
# Third set of checks:
# Check that it stops for unacceptable values of significance level:

out3A<-try(test.NI.survival(rnorm(100,100,1), rbinom(100,1,0.5), rep(0:1,each=50), NI.margin= 1.5, sig.level="0.025"))
correct[[n.t]]<-ifelse((inherits(out3A, "try-error"))&&(grepl("is.numeric(sig.level) is not TRUE", out3A[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out3A"
n.t=n.t+1
out3B<-try(test.NI.survival(rnorm(100,100,1), rbinom(100,1,0.5), rep(0:1,each=50), NI.margin= 1.5, sig.level=-0.1))
correct[[n.t]]<-ifelse((inherits(out3B, "try-error"))&&(grepl("sig.level > 0 is not TRUE", out3B[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out3B"
n.t=n.t+1
out3C<-try(test.NI.survival(rnorm(100,100,1), rbinom(100,1,0.5), rep(0:1,each=50), NI.margin= 1.5, sig.level=0.7))
correct[[n.t]]<-ifelse((inherits(out3C, "try-error"))&&(grepl("sig.level < 0.5 is not TRUE", out3C[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out3C"
n.t=n.t+1

#####################################################
# Fourth set of checks:
# Check with other wrong arguments:

# Check that it works for wrong summary measure value:
out4A<-try(test.NI.survival(rnorm(100,100,1), rbinom(100,1,0.5), rep(0:1,each=50), NI.margin= 1.5, summary.measure = "pippo"))
correct[[n.t]]<-ifelse((inherits(out4A, "try-error"))&&(grepl("summary.measure %in%", out4A[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4A"
n.t=n.t+1

# Check that it works when print.out incorrectly specified:
out4B<-try(test.NI.survival(rnorm(100,100,1), rbinom(100,1,0.5), rep(0:1,each=50), NI.margin= 1.5,  print.out = NA))
correct[[n.t]]<-ifelse((inherits(out4B, "try-error"))&&(grepl("!is.na(print.out) is not TRUE", out4B[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4B"
n.t=n.t+1

# Check that it works when test.type incorrectly specified:
out4C<-try(test.NI.survival(rnorm(100,100,1), rbinom(100,1,0.5), rep(0:1,each=50), NI.margin= 1.5,  test.type = "pippo"))
correct[[n.t]]<-ifelse((inherits(out4C, "try-error"))&&(grepl("test.type %in%", out4C[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4C"
n.t=n.t+1

# Check that it works when unfavourable incorrectly specified:
out4D<-try(test.NI.survival(rnorm(100,100,1), rbinom(100,1,0.5), rep(0:1,each=50), NI.margin= 1.5,  unfavourable = "pippo"))
correct[[n.t]]<-ifelse((inherits(out4D, "try-error"))&&(grepl("is.logical(unfavourable) is not TRUE", out4D[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4D"
n.t=n.t+1

# Check that M.boot has acceptable value:
out4E<-try(test.NI.survival(rnorm(100,100,1), rbinom(100,1,0.5), rep(0:1,each=50), NI.margin= 1.5, test.type="Cox.PH.bootstrap", M.boot="2000"))
correct[[n.t]]<-ifelse((inherits(out4E, "try-error"))&&(grepl("is.numeric(M.boot) is not TRUE", out4E[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4E"
n.t=n.t+1
out4F<-try(test.NI.survival(rnorm(100,100,1), rbinom(100,1,0.5), rep(0:1,each=50), NI.margin= 1.5, test.type="Cox.PH.bootstrap", M.boot=1))
correct[[n.t]]<-ifelse((inherits(out4F, "try-error"))&&(grepl("M.boot > 1 is not TRUE", out4F[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4F"
n.t=n.t+1

# Check that tau have acceptable value:
out4G<-try(test.NI.survival(rnorm(100,100,1), rbinom(100,1,0.5), rep(0:1,each=50), NI.margin= 1.5, summary.measure="DRMST"))
correct[[n.t]]<-ifelse((inherits(out4G, "try-error"))&&(grepl("is.numeric(tau) is not TRUE", out4G[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4G"
n.t=n.t+1
out4H<-try(test.NI.survival(rnorm(100,100,1), rbinom(100,1,0.5), rep(0:1,each=50), NI.margin= 1.5, summary.measure="DRMST", tau=-1))
correct[[n.t]]<-ifelse((inherits(out4H, "try-error"))&&(grepl("tau > 0 is not TRUE", out4H[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4H"
n.t=n.t+1

# Check that control.level have acceptable value:
out4I<-try(test.NI.survival(rnorm(100,100,1), rbinom(100,1,0.5), rep(0:1,each=50), NI.margin= 1.5, summary.measure="DRMST", tau=1, control.level=TRUE))
correct[[n.t]]<-ifelse((inherits(out4I, "try-error"))&&(grepl("is.character(control.level)", out4I[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4I"
n.t=n.t+1
out4J<-try(test.NI.survival(rnorm(100,100,1), rbinom(100,1,0.5), rep(0:1,each=50), NI.margin= 1.5, summary.measure="DRMST", tau=1, control.level="pippo"))
correct[[n.t]]<-ifelse((inherits(out4J, "try-error"))&&(grepl("is.character(control.level)", out4J[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4J"
n.t=n.t+1

# Check that flexsurv parameters have acceptable value:
out4K<-try(test.NI.survival(rnorm(100,100,1), rbinom(100,1,0.5), rep(0:1,each=50), NI.margin= -1.5, summary.measure="DRMST", tau=1, k="pippo"))
correct[[n.t]]<-ifelse((inherits(out4K, "try-error"))&&(grepl("k must be numeric", out4K[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4K"
n.t=n.t+1
out4L<-try(test.NI.survival(rnorm(100,100,1), rbinom(100,1,0.5), rep(0:1,each=50), NI.margin= -1.5, summary.measure="DRMST", tau=1, knots="pippo"))
correct[[n.t]]<-ifelse((inherits(out4L, "try-error"))&&(grepl("must be a numeric vector", out4L[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4L"
n.t=n.t+1
out4M<-try(test.NI.survival(rnorm(100,100,1), rbinom(100,1,0.5), rep(0:1,each=50), NI.margin= -1.5, summary.measure="DRMST", tau=1, bknots="pippo"))
correct[[n.t]]<-ifelse((inherits(out4M, "try-error"))&&(grepl("boundary knots should be a numeric vector of length 2", out4M[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4M"
n.t=n.t+1
#####################################################
# Fifth set of checks:
# Now check sample size calculations for certain values on HR scale. There are generally no comparators

set.seed(1)
time1<-rnorm(200,100,10)
event1<-rbinom(200,1,0.5)
treat1<-rep(0:1,each=100)
out5A<-try(test.NI.survival(time1, event1, treat1, NI.margin=1.5))
correct[[n.t]]<-ifelse((inherits(out5A,"list"))&&(all.equal(out5A$CI[2],1.874577, tolerance = 10^(-5)))&&out5A$non.inferiority==F,1,0) 
names(correct)[[n.t]]<-"out5A"
n.t=n.t+1
out5B<-try(test.NI.survival(time1, event1, treat1, NI.margin= 2))
correct[[n.t]]<-ifelse((inherits(out5B,"list"))&&(all.equal(out5B$CI[2],1.874577, tolerance=10^(-6))),1,0) 
names(correct)[[n.t]]<-"out5B"
n.t=n.t+1
out5C<-try(test.NI.survival(time1, event1, treat1, NI.margin= 1.5, test.type="flexsurv.PH"))
correct[[n.t]]<-ifelse((inherits(out5C,"list"))&&(all.equal(out5C$CI[2],1.337755, tolerance=10^(-6)))&&out5C$non.inferiority==T,1,0) 
names(correct)[[n.t]]<-"out5C"
n.t=n.t+1
out5D<-try(test.NI.survival(time1, event1, treat1, NI.margin=1.5, test.type="Cox.weighted"))
correct[[n.t]]<-ifelse((inherits(out5D,"list"))&&(all.equal(out5D$CI[2],1.852567, tolerance=10^(-6))),1,0) 
names(correct)[[n.t]]<-"out5D"
n.t=n.t+1
out5E<-try(test.NI.survival(time1, event1, treat1, NI.margin=0.75, unfavourable=F))
correct[[n.t]] <- ifelse((inherits(out5E, "list")) && (all.equal(out5E$CI[2], 1.874577, tolerance=10^(-6))), 1, 0) 
names(correct)[[n.t]] <- "out5E"
n.t <- n.t + 1
out5F<-try(test.NI.survival(time1, event1, treat1, NI.margin=1.5, sig.level=0.05))
correct[[n.t]] <- ifelse((inherits(out5F, "list")) && (all.equal(out5F$CI[2], 1.760655, tolerance=10^(-6))), 1, 0) 
names(correct)[[n.t]] <- "out5F"
n.t <- n.t + 1
set.seed(1)
time1<-rnorm(200,100,10)
event1<-rbinom(200,1,0.5)
treat1<-rep(0:1,each=100)
covs<-data.frame(age=rnorm(200,30,5))
out5G<-try(test.NI.survival(time1, event1, treat1,covs, NI.margin=1.5))
correct[[n.t]]<-ifelse((inherits(out5G,"list"))&&(all.equal(out5G$CI[2],1.855229, tolerance = 10^(-5)))&&out5G$non.inferiority==F,1,0) 
names(correct)[[n.t]]<-"out5G"
n.t=n.t+1

#####################################################
# Sixth set of checks:
# Now check sample size calculations for certain values on DRMST scale.

out6A<-try(test.NI.survival(time1, event1, treat1, NI.margin= -1, summary.measure="DRMST", tau=100))
correct[[n.t]] <- ifelse((is.list(out6A)) && (all.equal(out6A$CI[2], -0.3322545, tolerance=10^(-5))) && out6A$non.inferiority, 1, 0) 
names(correct)[[n.t]] <- "out6A"
n.t <- n.t + 1
out6B<-try(test.NI.survival(time1, event1, treat1, NI.margin=-0.5, summary.measure="DRMST", tau=100))
correct[[n.t]] <- ifelse((is.list(out6B)) && (all.equal(out6B$CI[2], -0.3322545, tolerance=10^(-5))) && !out6B$non.inferiority, 1, 0)
names(correct)[[n.t]] <- "out6B"
n.t <- n.t + 1
out6C<-try(test.NI.survival(time1, event1, treat1, NI.margin= 1, unfavourable=F, summary.measure="DRMST", tau=100))
correct[[n.t]] <- ifelse((is.list(out6C)) && (all.equal(out6C$CI[2], -0.3322545, tolerance=10^(-5))) && out6C$non.inferiority, 1, 0)
names(correct)[[n.t]] <- "out6C"
n.t <- n.t + 1
out6D<-try(test.NI.survival(time1, event1, treat1, NI.margin= -1, sig.level=0.05, summary.measure="DRMST", tau=100))
correct[[n.t]] <- ifelse((is.list(out6D)) && (all.equal(out6D$CI[2], -0.3547087, tolerance=10^(-6))), 1, 0)
names(correct)[[n.t]] <- "out6D"
n.t <- n.t + 1
out6E<-try(test.NI.survival(time1, event1, treat1, NI.margin= -1, summary.measure="DRMST", test.type="KM", tau=100))
correct[[n.t]] <- ifelse((is.list(out6E)) && (all.equal(out6E$CI[2], 0.6416595, tolerance=10^(-6))), 1, 0)
names(correct)[[n.t]] <- "out6E"
n.t <- n.t + 1
set.seed(1)
out6F<-try(test.NI.survival(time1, event1, treat1, NI.margin= -1, summary.measure="DRMST", test.type="Cox.PH.bootstrap", tau=100, bootCI.type = "norm"))
correct[[n.t]] <- ifelse((is.list(out6F)) && (all.equal(out6F$CI[2], 0.3362708, tolerance=10^(-6))), 1, 0)
names(correct)[[n.t]] <- "out6F"
n.t <- n.t + 1
set.seed(1)
out6G<-try(test.NI.survival(time1, event1, treat1, NI.margin= -1, summary.measure="DRMST", test.type="flexsurv.PH.bootstrap", tau=100, M.boot = 20, bootCI.type = "norm"))
correct[[n.t]] <- ifelse((is.list(out6G)) && (all.equal(out6G$CI[2], 0.3567705, tolerance=10^(-6))), 1, 0) 
names(correct)[[n.t]] <- "out6G"
n.t <- n.t + 1

#####################################################
# Seventh set of checks:
# Now check sample size calculations for certain values on DS scale. 

out7A<-try(test.NI.survival(time1, event1, treat1, NI.margin= -0.1, summary.measure="DS", tau=100))
correct[[n.t]] <- ifelse((is.list(out7A)) && (all.equal(out7A$CI[2], -0.04740067, tolerance=10^(-5))) && out7A$non.inferiority == TRUE, 1, 0) 
names(correct)[[n.t]] <- "out7A"
n.t <- n.t + 1
out7B<-try(test.NI.survival(time1, event1, treat1, NI.margin= -0.05, summary.measure="DS", tau=100))
correct[[n.t]] <- ifelse((is.list(out7B)) && (all.equal(out7A$CI[2], -0.04740067, tolerance=10^(-5))) && out7B$non.inferiority == FALSE, 1, 0) 
names(correct)[[n.t]] <- "out7B"
n.t <- n.t + 1
out7C<-try(test.NI.survival(time1, event1, treat1, NI.margin= -0.1, sig.level=0.05, summary.measure="DS", tau=100))
correct[[n.t]] <- ifelse((is.list(out7C)) && (all.equal(out7C$CI[2], -0.04978398, tolerance=10^(-5))) && out7C$non.inferiority == TRUE, 1, 0) 
names(correct)[[n.t]] <- "out7C"
n.t <- n.t + 1
out7D<-try(test.NI.survival(time1, event1, treat1, NI.margin= -0.1, summary.measure="DS", test.type="KM", tau=100))
correct[[n.t]] <- ifelse((is.list(out7D)) && (all.equal(out7D$CI[2], -0.01721723, tolerance=10^(-6))), 1, 0) 
names(correct)[[n.t]] <- "out7D"
n.t <- n.t + 1
set.seed(1)
out7E<-try(test.NI.survival(time1, event1, treat1, NI.margin= -0.1, summary.measure="DS", test.type="Cox.PH.bootstrap", tau=100, bootCI.type = "norm"))
correct[[n.t]] <- ifelse((is.list(out7E)) && (all.equal(out7E$CI[2], 0.04578162, tolerance=10^(-6))), 1, 0) 
names(correct)[[n.t]] <- "out7E"
n.t <- n.t + 1
out7F<-try(test.NI.survival(time1, event1, treat1, NI.margin= 0.1, unfavourable=F, summary.measure="DS", tau=100))
correct[[n.t]] <- ifelse((is.list(out7F)) && (all.equal(out7F$CI[2], -0.04740067, tolerance=10^(-6))), 1, 0) 
names(correct)[[n.t]] <- "out7F"
n.t <- n.t + 1
set.seed(1)
out7G<-try(test.NI.survival(time1, event1, treat1, NI.margin= -0.1, summary.measure="DS", test.type="flexsurv.PH.bootstrap", tau=100, M.boot=20, bootCI.type = "norm"))
correct[[n.t]] <- ifelse((is.list(out7G)) && (all.equal(out7G$CI[2], 0.04695612, tolerance=10^(-6))), 1, 0) 
names(correct)[[n.t]] <- "out7G"
n.t <- n.t + 1



##################################################
#### Now summarise results

vec.correct<-unlist(correct)  # Create vector from list
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
  names(correct)[which(correct==0)]
}
# Now list NA tests
if (tot.NA>0) {
  cat("Tests returning NAs:\n")
  names(correct)[which(is.na(correct))]
}

t.NI.s<-(tot.correct==number.of.tests) 


