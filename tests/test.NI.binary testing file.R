###################################################
### test.NI.binary testing file               #####
### 21-07-2023                                #####
###################################################

# Load dani:
# library(dani)
library(PropCIs)
library(DescTools)
library(boot)
library(ratesci)
library(DescrTab2)
library(marginaleffects)
library(exact2x2)
library(contingencytables)

#Initialise vector of outputs 
correct<-list(NULL)
n.t<-1

#####################################################
# First set of checks:
# Check that it stops for non acceptable values of n.control, n.experim, e.control, e.experim:

out1A<-try(test.NI.binary(n.control="100", n.experim=100, e.control=15, e.experim=15,  NI.margin=0.05))
correct[[n.t]]<-ifelse((inherits(out1A, "try-error"))&&(grepl("is.numeric(n.control) is not TRUE", out1A[1], fixed=T  )),1,0) 
names(correct)[[n.t]]<-"out1A"
n.t=n.t+1
out1B<-try(test.NI.binary(n.control=100, n.experim="100", e.control=15, e.experim=15,  NI.margin=0.05))
correct[[n.t]]<-ifelse((inherits(out1B, "try-error"))&&(grepl("is.numeric(n.experim) is not TRUE", out1B[1], fixed=T  )),1,0) 
names(correct)[[n.t]]<-"out1B"
n.t=n.t+1
out1C<-try(test.NI.binary(n.control=100, n.experim=100, e.control="15", e.experim=15,  NI.margin=0.05))
correct[[n.t]]<-ifelse((inherits(out1C, "try-error"))&&(grepl("is.numeric(e.control) is not TRUE", out1C[1], fixed=T )),1,0) 
names(correct)[[n.t]]<-"out1C"
n.t=n.t+1
out1D<-try(test.NI.binary(n.control=100, n.experim=100, e.control=15, e.experim="15",  NI.margin=0.05))
correct[[n.t]]<-ifelse((inherits(out1D, "try-error"))&&(grepl("is.numeric(e.experim) is not TRUE", out1D[1], fixed=T  )),1,0) 
names(correct)[[n.t]]<-"out1D"
n.t=n.t+1
out1E<-try(test.NI.binary(n.control=0, n.experim=100, e.control=15, e.experim=15,  NI.margin=0.05))
correct[[n.t]]<-ifelse((inherits(out1E, "try-error"))&&(grepl("n.control > 0 is not TRUE", out1E[1], fixed=T  )),1,0) 
names(correct)[[n.t]]<-"out1E"
n.t=n.t+1
out1F<-try(test.NI.binary(n.control=100, n.experim=0, e.control=15, e.experim=15,  NI.margin=0.05))
correct[[n.t]]<-ifelse((inherits(out1F, "try-error"))&&(grepl("n.experim > 0 is not TRUE", out1F[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out1F"
n.t=n.t+1
out1G<-try(test.NI.binary(n.control=100, n.experim=100, e.control=-15, e.experim=15,  NI.margin=0.05))
correct[[n.t]]<-ifelse((inherits(out1G, "try-error"))&&(grepl("e.control >= 0 is not TRUE", out1G[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out1G"
n.t=n.t+1
out1H<-try(test.NI.binary(n.control=100, n.experim=100, e.control=15, e.experim=-15,  NI.margin=0.05))
correct[[n.t]]<-ifelse((inherits(out1H, "try-error"))&&(grepl("e.experim >= 0 is not TRUE", out1H[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out1H"
n.t=n.t+1
out1I<-try(test.NI.binary(n.control=10, n.experim=100, e.control=15, e.experim=15,  NI.margin=0.05))
correct[[n.t]]<-ifelse((inherits(out1I, "try-error"))&&(grepl("n.control >= e.control is not TRUE", out1I[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out1I"
n.t=n.t+1
out1J<-try(test.NI.binary(n.control=100, n.experim=10, e.control=15, e.experim=15,  NI.margin=0.05))
correct[[n.t]]<-ifelse((inherits(out1J, "try-error"))&&(grepl("n.experim >= e.experim is not TRUE", out1J[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out1J"
n.t=n.t+1

#####################################################
# Second set of checks:
# Check that it stops for non-acceptable margins:

out2A<-try(test.NI.binary(n.control=100, n.experim=100, e.control=15, e.experim=15,  NI.margin="0.05"))
correct[[n.t]]<-ifelse((inherits(out2A, "try-error"))&&(grepl("is.numeric(NI.margin) is not TRUE", out2A[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out2A"
n.t=n.t+1
out2B<-try(test.NI.binary(n.control=100, n.experim=100, e.control=15, e.experim=15,  NI.margin=-0.05))
correct[[n.t]]<-ifelse((inherits(out2B, "try-error"))&&(grepl("When outcome is unfavourable, a risk difference NI margin needs to be positive.", out2B[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out2B"
n.t=n.t+1
out2C<-try(test.NI.binary(n.control=100, n.experim=100, e.control=15, e.experim=15,  NI.margin=0.05, unfavourable = F))
correct[[n.t]]<-ifelse((inherits(out2C, "try-error"))&&(grepl("When outcome is favourable, a risk difference NI margin needs to be negative.", out2C[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out2C"
n.t=n.t+1
out2D<-try(test.NI.binary(n.control=100, n.experim=100, e.control=15, e.experim=15,  NI.margin=1.05))
correct[[n.t]]<-ifelse((inherits(out2D, "try-error"))&&(grepl("NI.margin cannot be greater than 1", out2D[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out2D"
n.t=n.t+1
out2E<-try(test.NI.binary(n.control=100, n.experim=100, e.control=15, e.experim=15,  NI.margin=-1.05, unfavourable=F))
correct[[n.t]]<-ifelse((inherits(out2E, "try-error"))&&(grepl("NI.margin cannot be lower than -1", out2E[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out2E"
n.t=n.t+1
out2F<-try(test.NI.binary(n.control=100, n.experim=100, e.control=15, e.experim=15,  NI.margin=0.2, summary.measure = "RR"))
correct[[n.t]]<-ifelse((inherits(out2F, "try-error"))&&(grepl("When outcome is unfavourable, a NI margin on the risk ratio scale needs to be >1.", out2F[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out2F"
n.t=n.t+1
out2G<-try(test.NI.binary(n.control=100, n.experim=100, e.control=15, e.experim=15,  NI.margin=1.2, summary.measure = "RR", unfavourable = F))
correct[[n.t]]<-ifelse((inherits(out2G, "try-error"))&&(grepl("When outcome is favourable, a NI margin on the risk ratio scale needs to be <1.", out2G[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out2G"
n.t=n.t+1
out2H<-try(test.NI.binary(n.control=100, n.experim=100, e.control=15, e.experim=15,  NI.margin=0.2, summary.measure = "OR"))
correct[[n.t]]<-ifelse((inherits(out2H, "try-error"))&&(grepl("When outcome is unfavourable, a NI margin on the odds ratio scale needs to be >1.", out2H[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out2H"
n.t=n.t+1
out2I<-try(test.NI.binary(n.control=100, n.experim=100, e.control=15, e.experim=15,  NI.margin=1.2, summary.measure = "OR", unfavourable = F))
correct[[n.t]]<-ifelse((inherits(out2I, "try-error"))&&(grepl("When outcome is favourable, a NI margin on the odds ratio scale needs to be <1.", out2I[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out2I"
n.t=n.t+1
out2J<-try(test.NI.binary(n.control=100, n.experim=100, e.control=15, e.experim=15,  NI.margin=-0.05, summary.measure = "AS"))
correct[[n.t]]<-ifelse((inherits(out2J, "try-error"))&&(grepl("When outcome is unfavourable, a NI margin on the arc-sine difference scale needs to be >0.", out2J[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out2J"
n.t=n.t+1
out2K<-try(test.NI.binary(n.control=100, n.experim=100, e.control=15, e.experim=15,  NI.margin=0.05, summary.measure = "AS", unfavourable = F))
correct[[n.t]]<-ifelse((inherits(out2K, "try-error"))&&(grepl("When outcome is favourable, a NI margin on the arc-sine difference scale needs to be <0.", out2K[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out2K"
n.t=n.t+1
out2L<-try(test.NI.binary(n.control=100, n.experim=100, e.control=15, e.experim=15,  NI.margin=0, summary.measure = "RR", unfavourable = F))
correct[[n.t]]<-ifelse((inherits(out2L, "try-error"))&&(grepl("A risk ratio margin must be >0.", out2L[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out2L"
n.t=n.t+1
out2M<-try(test.NI.binary(n.control=100, n.experim=100, e.control=15, e.experim=15,  NI.margin=0, summary.measure = "OR", unfavourable = F))
correct[[n.t]]<-ifelse((inherits(out2M, "try-error"))&&(grepl("A odds ratio margin must be >0.", out2M[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out2M"
n.t=n.t+1

#####################################################
# Third set of checks:
# Check that it stops for unacceptable values of significance level:

out3A<-try(test.NI.binary(n.control=100, n.experim=100, e.control=15, e.experim=15,  NI.margin=0.05, sig.level="0.025"))
correct[[n.t]]<-ifelse((inherits(out3A, "try-error"))&&(grepl("is.numeric(sig.level) is not TRUE", out3A[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out3A"
n.t=n.t+1
out3B<-try(test.NI.binary(n.control=100, n.experim=100, e.control=15, e.experim=15,  NI.margin=0.05,  sig.level=-0.1))
correct[[n.t]]<-ifelse((inherits(out3B, "try-error"))&&(grepl("sig.level > 0 is not TRUE", out3B[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out3B"
n.t=n.t+1
out3C<-try(test.NI.binary(n.control=100, n.experim=100, e.control=15, e.experim=15,  NI.margin=0.05,  sig.level=0.7))
correct[[n.t]]<-ifelse((inherits(out3C, "try-error"))&&(grepl("sig.level < 0.5 is not TRUE", out3C[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out3C"
n.t=n.t+1

#####################################################
# Fourth set of checks:
# Check with other wrong arguments:

out4A<-try(test.NI.binary(n.control=100, n.experim=100, e.control=15, e.experim=15,  NI.margin=0.05, summary.measure = "pippo"))
correct[[n.t]]<-ifelse((inherits(out4A, "try-error"))&&(grepl("summary.measure ==", out4A[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4A"
n.t=n.t+1

# Check that it works when print.out incorrectly specified:
out4B<-try(test.NI.binary(n.control=100, n.experim=100, e.control=15, e.experim=15,  NI.margin=0.05,  print.out = NA))
correct[[n.t]]<-ifelse((inherits(out4B, "try-error"))&&(grepl("!is.na(print.out) is not TRUE", out4B[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4B"
n.t=n.t+1

# Check that it works when test.type incorrectly specified:
out4C<-try(test.NI.binary(n.control=100, n.experim=100, e.control=15, e.experim=15,  NI.margin=0.05, test.type = NA))
correct[[n.t]]<-ifelse((inherits(out4C, "try-error"))&&(grepl("is.character(test.type) is not TRUE", out4C[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4C"
n.t=n.t+1
out4D<-try(test.NI.binary(n.control=100, n.experim=100, e.control=15, e.experim=15,  NI.margin=0.05,  test.type = "pippo"))
correct[[n.t]]<-ifelse((inherits(out4D, "try-error"))&&(grepl("test.type %in% c", out4D[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4D"
n.t=n.t+1

# Check that it works when unfavourable incorrectly specified:
out4E<-try(test.NI.binary(n.control=100, n.experim=100, e.control=15, e.experim=15,  NI.margin=0.05,  unfavourable = "pippo"))
correct[[n.t]]<-ifelse((inherits(out4E, "try-error"))&&(grepl("is.logical(unfavourable) is not TRUE", out4E[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4E"
n.t=n.t+1
out4F<-try(test.NI.binary(n.control=100, n.experim=100, e.control=15, e.experim=15,  NI.margin=0.05, unfavourable = NA))
correct[[n.t]]<-ifelse((inherits(out4F, "try-error"))&&(grepl("!is.na(unfavourable) is not TRUE", out4F[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4F"
n.t=n.t+1

# Check that M.boot has acceptable value:
out4G<-try(test.NI.binary(n.control=100, n.experim=100, e.control=15, e.experim=15,  NI.margin=0.05, test.type="bootstrap", M.boot="2000"))
correct[[n.t]]<-ifelse((inherits(out4G, "try-error"))&&(grepl("is.numeric(M.boot) is not TRUE", out4G[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4G"
n.t=n.t+1
out4H<-try(test.NI.binary(n.control=100, n.experim=100, e.control=15, e.experim=15,  NI.margin=0.05, test.type="bootstrap", M.boot=1))
correct[[n.t]]<-ifelse((inherits(out4H, "try-error"))&&(grepl("M.boot > 1 is not TRUE", out4H[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4H"
n.t=n.t+1

# Check that BB.adj has acceptable value:
out4I<-try(test.NI.binary(n.control=100, n.experim=100, e.control=15, e.experim=15,  NI.margin=0.05, test.type="Berger.Boos", BB.adj="0.00001"))
correct[[n.t]]<-ifelse((inherits(out4I, "try-error"))&&(grepl("is.numeric(BB.adj) is not TRUE", out4I[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4I"
n.t=n.t+1
out4J<-try(test.NI.binary(n.control=100, n.experim=100, e.control=15, e.experim=15,  NI.margin=0.05,test.type="Berger.Boos", BB.adj=-0.00001))
correct[[n.t]]<-ifelse((inherits(out4J, "try-error"))&&(grepl("BB.adj > 0 is not TRUE", out4J[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4J"
n.t=n.t+1

# Check that it works when recursive.p.estim incorrectly specified:
out4K<-try(test.NI.binary(n.control=100, n.experim=100, e.control=15, e.experim=15,  NI.margin=0.05,  recursive.p.estim = "pippo"))
correct[[n.t]]<-ifelse((inherits(out4K, "try-error"))&&(grepl("is.logical(recursive.p.estim) is not TRUE", out4K[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4K"
n.t=n.t+1
out4L<-try(test.NI.binary(n.control=100, n.experim=100, e.control=15, e.experim=15,  NI.margin=0.05, recursive.p.estim = NA))
correct[[n.t]]<-ifelse((inherits(out4L, "try-error"))&&(grepl("!is.na(recursive.p.estim) is not TRUE", out4L[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4L"
n.t=n.t+1

# Check what happens for wrongly specified formula/data:
dat<-data.frame(y=rbinom(100,1,0.5), treat=rbinom(100,1,0.5))
out4M<-try(test.NI.binary(formula=3, data=dat,  NI.margin=0.05))
correct[[n.t]]<-ifelse((inherits(out4M, "try-error"))&&(grepl("inherits(formula", out4M[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4M"
n.t=n.t+1
out4N<-try(test.NI.binary(formula="y~treat(treat)", data=3, NI.margin=0.05))
correct[[n.t]]<-ifelse((inherits(out4N, "try-error"))&&(grepl("is.data.frame(data) is not TRUE", out4N[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4N"
n.t=n.t+1
dat2<-data.frame(y=rbinom(100,1,0.5), treat=sample(0:3,100,T))
out4O<-try(test.NI.binary(formula="y~treat(treat)", data=dat2, NI.margin=0.05))
correct[[n.t]]<-ifelse((inherits(out4O, "try-error"))&&(grepl(" nlevels(treatment) == 2 is not TRUE", out4O[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4O"
n.t=n.t+1
dat3<-data.frame(y=rbinom(100,1,0.5), treat=sample(2:3,100,T))
out4P<-try(test.NI.binary(formula="y~treat(treat)", data=dat3, NI.margin=0.05))
correct[[n.t]]<-ifelse((inherits(out4P, "try-error"))&&(grepl("any(treatment == control.level)", out4P[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4P"
n.t=n.t+1
#####################################################
# Fifth set of checks:
# Now check test for certain values on RD scale. These are compared against various comparators

out5A<-try(test.NI.binary(n.control=100, n.experim=100, e.control=15, e.experim=15,  NI.margin=0.05, sig.level=0.025, summary.measure="RD", 
                          print.out=TRUE, unfavourable=TRUE, test.type="Wald",
                          M.boot=2000, BB.adj=0.0001))
correct[[n.t]]<-ifelse((inherits(out5A,"list"))&&(all.equal(out5A$CI[2],0.09897333))&&out5A$non.inferiority==F,1,0) # Checked against prop.test
names(correct)[[n.t]]<-"out5A"
n.t=n.t+1
out5B<-try(test.NI.binary(n.control=1000, n.experim=1001, e.control=12, e.experim=0,  NI.margin=0.05, sig.level=0.025, summary.measure="RD", 
                          print.out=TRUE, unfavourable=TRUE, test.type="Wald",
                          M.boot=2000, BB.adj=0.0001))
correct[[n.t]]<-ifelse((inherits(out5B,"list"))&&(all.equal(out5B$CI[2],-0.005251346, tolerance=10^(-6))),1,0) # Checked against prop.test
names(correct)[[n.t]]<-"out5B"
n.t=n.t+1
out5C<-try(test.NI.binary(n.control=100, n.experim=100, e.control=15, e.experim=15,  NI.margin=0.1, sig.level=0.025, summary.measure="RD", 
                          print.out=TRUE, unfavourable=TRUE, test.type="Wald",
                          M.boot=2000, BB.adj=0.0001))
correct[[n.t]]<-ifelse((inherits(out5C,"list"))&&(all.equal(out5C$CI[2],0.09897333))&&out5C$non.inferiority==T,1,0) # Checked against prop.test
names(correct)[[n.t]]<-"out5C"
n.t=n.t+1
out5D<-try(test.NI.binary(n.control=1000, n.experim=1001, e.control=12, e.experim=0,  NI.margin=0.05, sig.level=0.05, summary.measure="RD", 
                          print.out=TRUE, unfavourable=TRUE, test.type="Wald",
                          M.boot=2000, BB.adj=0.0001))
correct[[n.t]]<-ifelse((inherits(out5D,"list"))&&(all.equal(out5D$CI[2],-0.006336351, tolerance=10^(-6))),1,0) # Checked against prop.test
names(correct)[[n.t]]<-"out5D"
n.t=n.t+1
out5E <- try(test.NI.binary(n.control=1000, n.experim=1001, e.control=12, e.experim=0,  NI.margin=-0.05, sig.level=0.05, summary.measure="RD", 
                            print.out=TRUE, unfavourable=FALSE, test.type="Wald",
                            M.boot=2000, BB.adj=0.0001))

correct[[n.t]] <- ifelse((inherits(out5E, "list")) && (all.equal(out5E$CI[2], -0.006336351, tolerance=10^(-6))), 1, 0) # Checked against prop.test
names(correct)[[n.t]] <- "out5E"
n.t <- n.t + 1
out5F <- try(test.NI.binary(n.control=100, n.experim=101, e.control=12, e.experim=0,  NI.margin=0.05, sig.level=0.025, summary.measure="RD", 
                            print.out=TRUE, unfavourable=TRUE, test.type="Wald.cc",
                            M.boot=2000, BB.adj=0.0001))
correct[[n.t]] <- ifelse((inherits(out5F, "list")) && (all.equal(out5F$CI[2], -0.04635821, tolerance=10^(-6))), 1, 0) # Checked against prop.test
names(correct)[[n.t]] <- "out5F"
n.t <- n.t + 1
out5G <- try(test.NI.binary(n.control=1000, n.experim=1001, e.control=120, e.experim=10,  NI.margin=0.05, sig.level=0.025, summary.measure="RD", 
                            print.out=TRUE, unfavourable=T, test.type="Wald.cc",
                            M.boot=2000, BB.adj=0.0001))
correct[[n.t]] <- ifelse((inherits(out5G, "list")) && (all.equal(out5G$CI[2], -0.08794837, tolerance=10^(-6))), 1, 0) # Checked against prop.test
names(correct)[[n.t]] <- "out5G"
n.t <- n.t + 1
out5H <- try(test.NI.binary(n.control=100, n.experim=101, e.control=12, e.experim=0,  NI.margin=0.05, sig.level=0.025, summary.measure="RD", 
                            print.out=TRUE, unfavourable=TRUE, test.type="Hauck.Anderson",
                            M.boot=2000, BB.adj=0.0001))
correct[[n.t]] <- ifelse((inherits(out5H, "list")) && (all.equal(out5H$CI[2], -0.05098784, tolerance=10^(-6))), 1, 0) # Checked against BinomDiffCI
names(correct)[[n.t]] <- "out5H"
n.t <- n.t + 1
out5I <- try(test.NI.binary(n.control=1000, n.experim=1001, e.control=120, e.experim=10,  NI.margin=0.05, sig.level=0.025, summary.measure="RD", 
                            print.out=TRUE, unfavourable=T, test.type="Hauck.Anderson",
                            M.boot=2000, BB.adj=0.0001))
correct[[n.t]] <- ifelse((inherits(out5I, "list")) && (all.equal(out5I$CI[2], -0.08843733, tolerance=10^(-6))), 1, 0) # Checked against BinomDiffCI
names(correct)[[n.t]] <- "out5I"
n.t <- n.t + 1
out5J <- try(test.NI.binary(n.control=100, n.experim=101, e.control=12, e.experim=0,  NI.margin=0.05, sig.level=0.025, summary.measure="RD", 
                            print.out=TRUE, unfavourable=TRUE, test.type="Gart.Nam",
                            M.boot=2000, BB.adj=0.0001))
correct[[n.t]] <- ifelse((inherits(out5J, "list")) && (all.equal(out5J$CI[2], -0.066845, tolerance=10^(-6))), 1, 0) # Checked against scasci
names(correct)[[n.t]] <- "out5J"
n.t <- n.t + 1
out5K <- try(test.NI.binary(n.control=1000, n.experim=1001, e.control=120, e.experim=10,  NI.margin=0.05, sig.level=0.025, summary.measure="RD", 
                            print.out=TRUE, unfavourable=T, test.type="Gart.Nam",
                            M.boot=2000, BB.adj=0.0001))
correct[[n.t]] <- ifelse((inherits(out5K, "list")) && (all.equal(out5K$CI[2], -0.089749, tolerance=10^(-6))), 1, 0) # Checked against scasci
names(correct)[[n.t]] <- "out5K"
n.t <- n.t + 1
out5L <- try(test.NI.binary(n.control=100, n.experim=101, e.control=12, e.experim=0,  NI.margin=0.05, sig.level=0.025, summary.measure="RD", 
                            print.out=TRUE, unfavourable=TRUE, test.type="Newcombe10",
                            M.boot=2000, BB.adj=0.0001))
correct[[n.t]] <- ifelse((inherits(out5L, "list")) && (all.equal(out5L$CI[2], -0.05800701, tolerance=10^(-6))), 1, 0) # Checked against contingencytables
names(correct)[[n.t]] <- "out5L"
n.t <- n.t + 1
out5M <- try(test.NI.binary(n.control=1000, n.experim=1001, e.control=120, e.experim=10,  NI.margin=0.05, sig.level=0.025, summary.measure="RD", 
                            print.out=TRUE, unfavourable=T, test.type="Newcombe10",
                            M.boot=2000, BB.adj=0.0001))
correct[[n.t]] <- ifelse((inherits(out5M, "list")) && (all.equal(out5M$CI[2], -0.08954957, tolerance=10^(-6))), 1, 0) # Checked against contingencytables
names(correct)[[n.t]] <- "out5M"
n.t <- n.t + 1
out5N <- try(test.NI.binary(n.control=100, n.experim=101, e.control=12, e.experim=0,  NI.margin=0.05, sig.level=0.025, summary.measure="RD", 
                            print.out=TRUE, unfavourable=TRUE, test.type="Newcombe11",
                            M.boot=2000, BB.adj=0.0001))
correct[[n.t]] <- ifelse((inherits(out5N, "list")) && (all.equal(out5N$CI[2], -0.04947413, tolerance=10^(-6))), 1, 0) # Checked against BinomDiffCI
names(correct)[[n.t]] <- "out5N"
n.t <- n.t + 1
out5O <- try(test.NI.binary(n.control=1000, n.experim=1001, e.control=120, e.experim=10,  NI.margin=0.05, sig.level=0.025, summary.measure="RD", 
                            print.out=TRUE, unfavourable=T, test.type="Newcombe11",
                            M.boot=2000, BB.adj=0.0001))
correct[[n.t]] <- ifelse((inherits(out5O, "list")) && (all.equal(out5O$CI[2], -0.08886343, tolerance=10^(-6))), 1, 0) # Checked against BiomDiffCI
names(correct)[[n.t]] <- "out5O"
n.t <- n.t + 1
out5P <- try(test.NI.binary(n.control=100, n.experim=101, e.control=12, e.experim=0,  NI.margin=0.05, sig.level=0.025, summary.measure="RD", 
                            print.out=TRUE, unfavourable=TRUE, test.type="Jeffreys.Perks",
                            M.boot=2000, BB.adj=0.0001))
correct[[n.t]] <- ifelse((inherits(out5P, "list")) && (all.equal(out5P$CI[2], -0.05248876, tolerance=10^(-6))), 1, 0) # Checked against BiomDiffCI
names(correct)[[n.t]] <- "out5P"
n.t <- n.t + 1
out5Q <- try(test.NI.binary(n.control=1000, n.experim=1001, e.control=120, e.experim=10,  NI.margin=0.05, sig.level=0.025, summary.measure="RD", 
                            print.out=TRUE, unfavourable=T, test.type="Jeffreys.Perks",
                            M.boot=2000, BB.adj=0.0001))
correct[[n.t]] <- ifelse((inherits(out5Q, "list")) && (all.equal(out5Q$CI[2], -0.08868839, tolerance=10^(-6))), 1, 0) # Checked against BiomDiffCI
names(correct)[[n.t]] <- "out5Q"
n.t <- n.t + 1
out5R <- try(test.NI.binary(n.control=100, n.experim=101, e.control=12, e.experim=0,  NI.margin=0.05, sig.level=0.025, summary.measure="RD", 
                            print.out=TRUE, unfavourable=TRUE, test.type="Haldane",
                            M.boot=2000, BB.adj=0.0001))
correct[[n.t]] <- ifelse((inherits(out5R, "list")) && (all.equal(out5R$CI[2], -0.05470316, tolerance=10^(-6))), 1, 0) # Checked against BinomDiffCI
names(correct)[[n.t]] <- "out5R"
n.t <- n.t + 1
out5S <- try(test.NI.binary(n.control=1000, n.experim=1001, e.control=120, e.experim=10,  NI.margin=0.05, sig.level=0.025, summary.measure="RD", 
                            print.out=TRUE, unfavourable=T, test.type="Haldane",
                            M.boot=2000, BB.adj=0.0001))
correct[[n.t]] <- ifelse((inherits(out5S, "list")) && (all.equal(out5S$CI[2], -0.08875705, tolerance=10^(-6))), 1, 0) # Checked against BiomDiffCI
names(correct)[[n.t]] <- "out5S"
n.t <- n.t + 1
out5T <- try(test.NI.binary(n.control=100, n.experim=101, e.control=12, e.experim=0,  NI.margin=0.05, sig.level=0.025, summary.measure="RD", 
                            print.out=TRUE, unfavourable=TRUE, test.type="Mee",
                            M.boot=2000, BB.adj=0.0001))
correct[[n.t]] <- ifelse((inherits(out5T, "list")) && (all.equal(out5T$CI[2], -0.06999872, tolerance=10^(-6))), 1, 0) # Checked against BiomDiffCI
names(correct)[[n.t]] <- "out5T"
n.t <- n.t + 1
out5U <- try(test.NI.binary(n.control=1000, n.experim=1001, e.control=120, e.experim=10,  NI.margin=0.05, sig.level=0.025, summary.measure="RD", 
                            print.out=TRUE, unfavourable=T, test.type="Mee",
                            M.boot=2000, BB.adj=0.0001))
correct[[n.t]] <- ifelse((inherits(out5U, "list")) && (all.equal(out5U$CI[2], -0.09004525, tolerance=10^(-6))), 1, 0) # Checked against BiomDiffCI
names(correct)[[n.t]] <- "out5U"
n.t <- n.t + 1
out5V <- try(test.NI.binary(n.control=100, n.experim=101, e.control=12, e.experim=0,  NI.margin=0.05, sig.level=0.025, summary.measure="RD", 
                            print.out=TRUE, unfavourable=TRUE, test.type="Agresti.Caffo",
                            M.boot=2000, BB.adj=0.0001))
correct[[n.t]] <- ifelse((inherits(out5V, "list")) && (all.equal(out5V$CI[2], -0.05031232, tolerance=10^(-6))), 1, 0) # Checked against contingencytables
names(correct)[[n.t]] <- "out5V"
n.t <- n.t + 1
out5W <- try(test.NI.binary(n.control=1000, n.experim=1001, e.control=120, e.experim=10,  NI.margin=0.05, sig.level=0.025, summary.measure="RD", 
                            print.out=TRUE, unfavourable=T, test.type="Agresti.Caffo",
                            M.boot=2000, BB.adj=0.0001))
correct[[n.t]] <- ifelse((inherits(out5W, "list")) && (all.equal(out5W$CI[2], -0.08861123, tolerance=10^(-6))), 1, 0) # Checked against contingencytables
names(correct)[[n.t]] <- "out5W"
n.t <- n.t + 1
out5X <- try(test.NI.binary(n.control=100, n.experim=101, e.control=12, e.experim=0,  NI.margin=0.05, sig.level=0.025, summary.measure="RD", 
                            print.out=TRUE, unfavourable=TRUE, test.type="Miettinen.Nurminen",
                            M.boot=2000, BB.adj=0.0001))
correct[[n.t]] <- ifelse((inherits(out5X, "list")) && (all.equal(out5X$CI[2], -0.06990028, tolerance=10^(-6))), 1, 0) # Checked against contingencytables
names(correct)[[n.t]] <- "out5X"
n.t <- n.t
out5Y <- try(test.NI.binary(n.control=1000, n.experim=1001, e.control=120, e.experim=10,  NI.margin=0.05, sig.level=0.025, summary.measure="RD", 
                            print.out=TRUE, unfavourable=T, test.type="Miettinen.Nurminen",
                            M.boot=2000, BB.adj=0.0001))
correct[[n.t]] <- ifelse((inherits(out5Y, "list")) && (all.equal(out5Y$CI[2], -0.09001412, tolerance=10^(-6))), 1, 0) # Checked against contingencytables
names(correct)[[n.t]] <- "out5Y"
n.t <- n.t + 1
out5Z <- try(test.NI.binary(n.control=100, n.experim=101, e.control=12, e.experim=0,  NI.margin=0.05, sig.level=0.025, summary.measure="RD", 
                            print.out=TRUE, unfavourable=TRUE, test.type="Farrington.Manning",
                            M.boot=2000, BB.adj=0.0001))
correct[[n.t]] <- ifelse((inherits(out5Z, "list")) && (all.equal(out5Z$CI[2], -0.06999406, tolerance=10^(-6))), 1, 0) # Checked against descrtab2
names(correct)[[n.t]] <- "out5Z"
n.t <- n.t + 1
out5AA <- try(test.NI.binary(n.control=1000, n.experim=1001, e.control=120, e.experim=10,  NI.margin=0.05, sig.level=0.025, summary.measure="RD", 
                             print.out=TRUE, unfavourable=T, test.type="Farrington.Manning",
                             M.boot=2000, BB.adj=0.0001))
correct[[n.t]] <- ifelse((inherits(out5AA, "list")) && (all.equal(out5AA$CI[2], -0.09001888, tolerance=10^(-6))), 1, 0) # Checked against descrtab2
names(correct)[[n.t]] <- "out5AA"
n.t <- n.t + 1
out5AB <- try(test.NI.binary(n.control=100, n.experim=101, e.control=12, e.experim=0,  NI.margin=0.05, sig.level=0.025, summary.measure="RD", 
                             print.out=TRUE, unfavourable=TRUE, test.type="logistic",
                             M.boot=2000, BB.adj=0.0001))
correct[[n.t]] <- ifelse((inherits(out5AB, "list")) && (all.equal(out5AB$CI[2], -0.05630389, tolerance=10^(-4))), 1, 0) # No comparator
names(correct)[[n.t]] <- "out5AB"
n.t <- n.t + 1
out5AC <- try(test.NI.binary(n.control=1000, n.experim=1001, e.control=120, e.experim=10,  NI.margin=0.05, sig.level=0.025, summary.measure="RD", 
                             print.out=TRUE, unfavourable=T, test.type="logistic",
                             M.boot=2000, BB.adj=0.0001))
correct[[n.t]] <- ifelse((inherits(out5AC, "list")) && (all.equal(out5AC$CI[2], -0.08894788, tolerance=10^(-4))), 1, 0) # No comparator
names(correct)[[n.t]] <- "out5AC"
n.t <- n.t + 1
set.seed(1)
out5AD <- try(test.NI.binary(n.control=100, n.experim=101, e.control=12, e.experim=0,  NI.margin=0.05, sig.level=0.025, summary.measure="RD", 
                             print.out=TRUE, unfavourable=TRUE, test.type="bootstrap",
                             M.boot=2000, BB.adj=0.0001))
correct[[n.t]] <- ifelse((inherits(out5AD, "list")) && (all.equal(out5AD$CI[2], -0.06382979, tolerance=10^(-6))), 1, 0) # No comparator
names(correct)[[n.t]] <- "out5AD"
n.t <- n.t + 1
set.seed(1)
out5AE <- try(test.NI.binary(n.control=1000, n.experim=1001, e.control=120, e.experim=10,  NI.margin=0.05, sig.level=0.025, summary.measure="RD", 
                             print.out=TRUE, unfavourable=T, test.type="bootstrap",
                             M.boot=2005, BB.adj=0.0001))
correct[[n.t]] <- ifelse((inherits(out5AE, "list")) && (all.equal(out5AE$CI[2], -0.08968069, tolerance=10^(-6))), 1, 0) # No comparator
names(correct)[[n.t]] <- "out5AE"
n.t <- n.t + 1
out5AF <- try(test.NI.binary(n.control=10, n.experim=11, e.control=2, e.experim=0,  NI.margin=0.05, sig.level=0.025, summary.measure="RD", 
                             print.out=TRUE, unfavourable=TRUE, test.type="Agresti.Min",
                             M.boot=2000, BB.adj=0.0001))
correct[[n.t]] <- ifelse((inherits(out5AF, "list")) && (all.equal(out5AF$CI[2], 0.1032273, tolerance=10^(-6))), 1, 0) # Checked against exact2x2
names(correct)[[n.t]] <- "out5AF"
n.t <- n.t + 1
out5AG <- try(test.NI.binary(n.control=10, n.experim=11, e.control=2, e.experim=0,  NI.margin=0.05, sig.level=0.025, summary.measure="RD", 
                             print.out=TRUE, unfavourable=TRUE, test.type="Chan.Zhang",
                             M.boot=2000, BB.adj=0.0001))
correct[[n.t]] <- ifelse((inherits(out5AG, "list")) && (all.equal(out5AG$CI[2], 0.1166923, tolerance=10^(-6))), 1, 0) # Checked against exact2x2
names(correct)[[n.t]] <- "out5AG"
n.t <- n.t + 1
out5AH <- try(test.NI.binary(n.control=10, n.experim=11, e.control=2, e.experim=0,  NI.margin=0.05, sig.level=0.025, summary.measure="RD", 
                             print.out=TRUE, unfavourable=TRUE, test.type="uncond.midp",
                             M.boot=2000, BB.adj=0.0001))
correct[[n.t]] <- ifelse((inherits(out5AH, "list")) && (all.equal(out5AH$CI[2], 0.1032273, tolerance=10^(-6))), 1, 0) # Checked against exact2x2
names(correct)[[n.t]] <- "out5AH"
n.t <- n.t + 1
out5AI <- try(test.NI.binary(n.control=10, n.experim=11, e.control=2, e.experim=0,  NI.margin=0.05, sig.level=0.025, summary.measure="RD", 
                             print.out=TRUE, unfavourable=TRUE, test.type="Berger.Boos",
                             M.boot=2000, BB.adj=0.01))
correct[[n.t]] <- ifelse((inherits(out5AI, "list")) && (all.equal(out5AI$CI[2], 0.1080679, tolerance=10^(-6))), 1, 0) # Checked against exact2x2
names(correct)[[n.t]] <- "out5AI"
n.t <- n.t + 1
out5AJ <- try(test.NI.binary(n.control=100, n.experim=101, e.control=12, e.experim=0,  NI.margin=0.05, sig.level=0.025, summary.measure="RD", 
                             print.out=TRUE, unfavourable=TRUE, test.type="BLNM",
                             M.boot=2000, BB.adj=0.0001))
correct[[n.t]] <- ifelse((inherits(out5AJ, "list")) && (all.equal(out5AJ$CI[2], -0.06423191, tolerance=10^(-6))), 1, 0) # No comparator
names(correct)[[n.t]] <- "out5AJ"
n.t <- n.t + 1
out5AK <- try(test.NI.binary(n.control=1000, n.experim=1001, e.control=120, e.experim=10,  NI.margin=0.05, sig.level=0.025, summary.measure="RD", 
                             print.out=TRUE, unfavourable=T, test.type="BLNM",
                             M.boot=2000, BB.adj=0.0001))
correct[[n.t]] <- ifelse((inherits(out5AK, "list")) && (all.equal(out5AK$CI[2], -0.08959896, tolerance=10^(-6))), 1, 0) # No comparator
names(correct)[[n.t]] <- "out5AK"
n.t <- n.t + 1
out5AL <- try(test.NI.binary(n.control=100, n.experim=101, e.control=12, e.experim=0,  NI.margin=0.05, sig.level=0.025, summary.measure="RD", 
                             print.out=TRUE, unfavourable=TRUE, test.type="Brown.Li.Jeffreys",
                             M.boot=2000, BB.adj=0.0001))
correct[[n.t]] <- ifelse((inherits(out5AL, "list")) && (all.equal(out5AL$CI[2], -0.05289521, tolerance=10^(-6))), 1, 0) # Checked against BiomDiffCI
names(correct)[[n.t]] <- "out5AL"
n.t <- n.t + 1
out5AM <- try(test.NI.binary(n.control=1000, n.experim=1001, e.control=120, e.experim=10,  NI.margin=0.05, sig.level=0.025, summary.measure="RD", 
                             print.out=TRUE, unfavourable=T, test.type="Brown.Li.Jeffreys",
                             M.boot=2000, BB.adj=0.0001))
correct[[n.t]] <- ifelse((inherits(out5AM, "list")) && (all.equal(out5AM$CI[2], -0.08876864, tolerance=10^(-6))), 1, 0) # Checked against BiomDiffCI
names(correct)[[n.t]] <- "out5AM"
n.t <- n.t + 1
out5AN <- try(test.NI.binary(n.control=100, n.experim=101, e.control=12, e.experim=0,  NI.margin=0.05, sig.level=0.025, summary.measure="RD", 
                             print.out=TRUE, unfavourable=TRUE, test.type="MUE.Lin",
                             M.boot=2000, BB.adj=0.0001))
correct[[n.t]] <- ifelse((inherits(out5AN, "list")) && (all.equal(out5AN$CI[2], -0.05808034, tolerance=10^(-6))), 1, 0) # Checked against BiomDiffCI
names(correct)[[n.t]] <- "out5AN"
n.t <- n.t + 1
out5AO <- try(test.NI.binary(n.control=1000, n.experim=1001, e.control=120, e.experim=10,  NI.margin=0.05, sig.level=0.025, summary.measure="RD", 
                             print.out=TRUE, unfavourable=T, test.type="MUE.Lin",
                             M.boot=2000, BB.adj=0.0001))
correct[[n.t]] <- ifelse((inherits(out5AO, "list")) && (all.equal(out5AO$CI[2], -0.08898313, tolerance=10^(-6))), 1, 0) # Checked against BiomDiffCI
names(correct)[[n.t]] <- "out5AO"
n.t <- n.t + 1
set.seed(1)
out5AP <- try(test.NI.binary(n.control=100, n.experim=101, e.control=12, e.experim=0,  NI.margin=0.05, sig.level=0.025, summary.measure="RD", 
                             print.out=TRUE, unfavourable=TRUE, test.type="MUE.parametric.bootstrap",
                             M.boot=2000, BB.adj=0.0001, bootCI.type = "perc"))
correct[[n.t]] <- ifelse((inherits(out5AP, "list")) && (all.equal(out5AP$CI[2], -0.0497923, tolerance=10^(-6))), 1, 0) # Checked against BiomDiffCI
names(correct)[[n.t]] <- "out5AP"
n.t <- n.t + 1
set.seed(1)
out5AQ <- try(test.NI.binary(n.control=1000, n.experim=1001, e.control=120, e.experim=10,  NI.margin=0.05, sig.level=0.025, summary.measure="RD", 
                             print.out=TRUE, unfavourable=T, test.type="MUE.parametric.bootstrap",
                             M.boot=2000, bootCI.type="perc", BB.adj=0.0001))
correct[[n.t]] <- ifelse((inherits(out5AQ, "list")) && (all.equal(out5AQ$CI[2], -0.08898217, tolerance=10^(-6))), 1, 0) # Checked against BiomDiffCI
names(correct)[[n.t]] <- "out5AQ"
n.t <- n.t + 1
dat4<-data.frame(y=rep(c(1,0,1,0),c(15,85,15,85)), treat=rep(c(1,0), each=100))
out5AR<-try(test.NI.binary(formula="y~treat(treat)", data=dat4,  NI.margin=0.05, sig.level=0.025, summary.measure="RD", 
                          print.out=TRUE, unfavourable=TRUE, test.type="Wald",
                          M.boot=2000, BB.adj=0.0001))
correct[[n.t]]<-ifelse((inherits(out5AR,"list"))&&(all.equal(out5AR$CI[2],0.09897333))&&out5AR$non.inferiority==F,1,0) # Checked against prop.test
names(correct)[[n.t]]<-"out5AR"
n.t=n.t+1
dataf<-data.frame(y<-rep(c(1,0,1,0),c(15,85,15,85)), treat<-rep(c(1,0), each=100))
colnames(dataf)<-c("y", "treat")
out5AS<-try(test.NI.binary(data=dataf, formula=as.formula("y~treat(treat)"),  NI.margin=0.05, sig.level=0.025, summary.measure="RD", 
                          print.out=TRUE, unfavourable=TRUE, test.type="Wald",
                          M.boot=2000, BB.adj=0.0001))
correct[[n.t]]<-ifelse((inherits(out5AS,"list"))&&(all.equal(out5AS$CI[2],0.09897333))&&out5AS$non.inferiority==F,1,0) # Checked against prop.test
names(correct)[[n.t]]<-"out5AS"
n.t=n.t+1
dataf2<-data.frame(y<-rep(c(1,0,1,0),c(15,85,15,85)), treat<-rep(c(1,0), each=100), age<-rep(c(20,30,40,50), 50))
colnames(dataf2)<-c("y", "treat", "age")
out5AT<-try(test.NI.binary(data=dataf2, formula=as.formula("y~treat(treat)+age"),  NI.margin=0.05, sig.level=0.025, summary.measure="RD", 
                           print.out=TRUE, unfavourable=TRUE, test.type="logistic",
                           M.boot=2000, BB.adj=0.0001))
correct[[n.t]]<-ifelse((inherits(out5AT,"list"))&&(all.equal(out5AT$CI[2],0.09890323))&&out5AT$non.inferiority==F,1,0) # Checked against prop.test
names(correct)[[n.t]]<-"out5AT"
n.t=n.t+1

#####################################################
# Sixth set of checks:
# Now check sample size calculations for certain values on RR scale. These are compared against various comparators

out6A <- try(test.NI.binary(n.control=100, n.experim=100, e.control=15, e.experim=15, NI.margin=1.5, sig.level=0.025, summary.measure="RR", 
                            print.out=TRUE, unfavourable=TRUE, test.type="Wald.Katz",
                            M.boot=2000, BB.adj=0.0001))
correct[[n.t]] <- ifelse((is.list(out6A)) && (all.equal(out6A$CI[2], 1.934448, tolerance=10^(-5))) && !out6A$non.inferiority, 1, 0) # Checked against riskratio epitools
names(correct)[[n.t]] <- "out6A"
n.t <- n.t + 1
out6B <- try(test.NI.binary(n.control=1000, n.experim=1001, e.control=12, e.experim=0, NI.margin=1.5, sig.level=0.025, summary.measure="RR", 
                            print.out=TRUE, unfavourable=TRUE, test.type="Wald.Katz",
                            M.boot=2000, BB.adj=0.0001))
correct[[n.t]] <- ifelse((class(out6B) == "try-error") && (substr(out6B[1], 83, 100) == "Wald.Katz interval"), 1, 0)
names(correct)[[n.t]] <- "out6B"
n.t <- n.t + 1
out6C <- try(test.NI.binary(n.control=100, n.experim=100, e.control=15, e.experim=15, NI.margin=2, sig.level=0.025, summary.measure="RR", 
                            print.out=TRUE, unfavourable=TRUE, test.type="Wald.Katz",
                            M.boot=2000, BB.adj=0.0001))
correct[[n.t]] <- ifelse((is.list(out6C)) && (all.equal(out6C$CI[2], 1.934448, tolerance=10^(-5))) && out6C$non.inferiority, 1, 0)
names(correct)[[n.t]] <- "out6C"
n.t <- n.t + 1
out6D <- try(test.NI.binary(n.control=100, n.experim=100, e.control=15, e.experim=15, NI.margin=1.5, sig.level=0.05, summary.measure="RR", 
                            print.out=TRUE, unfavourable=TRUE, test.type="Wald.Katz",
                            M.boot=2000, BB.adj=0.0001))
correct[[n.t]] <- ifelse((is.list(out6D)) && (all.equal(out6D$CI[2], 1.739748, tolerance=10^(-6))), 1, 0)
names(correct)[[n.t]] <- "out6D"
n.t <- n.t + 1
out6E <- try(test.NI.binary(n.control=100, n.experim=100, e.control=15, e.experim=15, NI.margin=0.75, sig.level=0.05, summary.measure="RR", 
                            print.out=TRUE, unfavourable=FALSE, test.type="Wald.Katz",
                            M.boot=2000, BB.adj=0.0001))
correct[[n.t]] <- ifelse((is.list(out6E)) && (all.equal(out6E$CI[2], 1.739748, tolerance=10^(-6))), 1, 0)
names(correct)[[n.t]] <- "out6E"
n.t <- n.t + 1
out6F <- try(test.NI.binary(n.control=100, n.experim=101, e.control=12, e.experim=0, NI.margin=1.5, sig.level=0.025, summary.measure="RR", 
                            print.out=TRUE, unfavourable=TRUE, test.type="adjusted.Wald.Katz",
                            M.boot=2000, BB.adj=0.0001))
correct[[n.t]] <- ifelse((is.list(out6F)) && (all.equal(out6F$CI[2], 0.6599676, tolerance=10^(-6))), 1, 0)
names(correct)[[n.t]] <- "out6F"
n.t <- n.t + 1
out6G <- try(test.NI.binary(n.control=1000, n.experim=1001, e.control=120, e.experim=10, NI.margin=1.5, sig.level=0.025, summary.measure="RR", 
                            print.out=TRUE, unfavourable=TRUE, test.type="adjusted.Wald.Katz",
                            M.boot=2000, BB.adj=0.0001))
correct[[n.t]] <- ifelse((is.list(out6G)) && (all.equal(out6G$CI[2], 0.1625568, tolerance=10^(-6))), 1, 0) # Checked against contingencytables
names(correct)[[n.t]] <- "out6G"
n.t <- n.t + 1
out6H <- try(test.NI.binary(n.control=100, n.experim=101, e.control=12, e.experim=1,  NI.margin=1.5, sig.level=0.025, summary.measure="RR", 
                            print.out=TRUE, unfavourable=TRUE, test.type="inverse.hyperbolic.sine",
                            M.boot=2000, BB.adj=0.0001))
correct[[n.t]] <- ifelse((is.list(out6H)) && (all.equal(out6H$CI[2], 0.4881272, tolerance=10^(-6))), 1, 0) # Checked against contingencytables
names(correct)[[n.t]] <- "out6H"
n.t <- n.t + 1
out6I <- try(test.NI.binary(n.control=1000, n.experim=1001, e.control=120, e.experim=10, NI.margin=1.5, sig.level=0.025, summary.measure="RR", 
                            print.out=TRUE, unfavourable=TRUE, test.type="inverse.hyperbolic.sine",
                            M.boot=2000, BB.adj=0.0001))
correct[[n.t]] <- ifelse((is.list(out6I)) && (all.equal(out6I$CI[2], 0.1561108, tolerance=10^(-6))), 1, 0) # Checked against contingencytables
names(correct)[[n.t]] <- "out6I"
n.t <- n.t + 1
out6J <- try(test.NI.binary(n.control=100, n.experim=101, e.control=12, e.experim=0,  NI.margin=1.5, sig.level=0.025, summary.measure="RR", 
                            print.out=TRUE, unfavourable=TRUE, test.type="Koopman",
                            M.boot=2000, BB.adj=0.0001))
correct[[n.t]] <- ifelse((is.list(out6J)) && (all.equal(out6J$CI[2], 0.3078771, tolerance=10^(-6))), 1, 0) # Checked against desctools
names(correct)[[n.t]] <- "out6J"
n.t <- n.t + 1
out6K <- try(test.NI.binary(n.control=1000, n.experim=1001, e.control=120, e.experim=10, NI.margin=1.5, sig.level=0.025, summary.measure="RR", 
                            print.out=TRUE, unfavourable=TRUE, test.type="Koopman",
                            M.boot=2000, BB.adj=0.0001))
correct[[n.t]] <- ifelse((is.list(out6K)) && (all.equal(out6K$CI[2], 0.1557953, tolerance=10^(-6))), 1, 0) # Checked against desctools
names(correct)[[n.t]] <- "out6K"
n.t <- n.t + 1
out6L <- try(test.NI.binary(n.control=100, n.experim=101, e.control=12, e.experim=0,  NI.margin=1.5, sig.level=0.025, summary.measure="RR", 
                            print.out=TRUE, unfavourable=TRUE, test.type="MOVER.R",
                            M.boot=2000, BB.adj=0.0001))
correct[[n.t]] <- ifelse((is.list(out6L)) && (all.equal(out6L$CI[2], 0.3358926, tolerance=10^(-6))), 1, 0) # Checked against contingencytables
names(correct)[[n.t]] <- "out6L"
n.t <- n.t + 1
out6M <- try(test.NI.binary(n.control=1000, n.experim=1001, e.control=120, e.experim=10, NI.margin=1.5, sig.level=0.025, summary.measure="RR", 
                            print.out=TRUE, unfavourable=TRUE, test.type="MOVER.R",
                            M.boot=2000, BB.adj=0.0001))
correct[[n.t]] <- ifelse((is.list(out6M)) && (all.equal(out6M$CI[2], 0.1566062, tolerance=10^(-6))), 1, 0) # Checked against contingencytables
names(correct)[[n.t]] <- "out6M"
n.t <- n.t + 1
out6N <- try(test.NI.binary(n.control=100, n.experim=101, e.control=12, e.experim=0,  NI.margin=1.5, sig.level=0.025, summary.measure="RR", 
                            print.out=TRUE, unfavourable=TRUE, test.type="Miettinen.Nurminen",
                            M.boot=2000, BB.adj=0.0001))
correct[[n.t]] <- ifelse((is.list(out6N)) && (all.equal(out6N$CI[2], 0.3093824, tolerance=10^(-6))), 1, 0) # Checked against contingencytables
names(correct)[[n.t]] <- "out6N"
n.t <- n.t + 1
out6O <- try(test.NI.binary(n.control=1000, n.experim=1001, e.control=120, e.experim=10, NI.margin=1.5, sig.level=0.025, summary.measure="RR", 
                            print.out=TRUE, unfavourable=TRUE, test.type="Miettinen.Nurminen",
                            M.boot=2000, BB.adj=0.0001))
correct[[n.t]] <- ifelse((is.list(out6O)) && (all.equal(out6O$CI[2], 0.1558188, tolerance=10^(-6))), 1, 0) # Checked against contingencytables
names(correct)[[n.t]] <- "out6O"
n.t <- n.t + 1
out6P <- try(test.NI.binary(n.control=100, n.experim=101, e.control=12, e.experim=0,  NI.margin=1.5, sig.level=0.025, summary.measure="RR", 
                            print.out=TRUE, unfavourable=TRUE, test.type="MOVER",
                            M.boot=2000, BB.adj=0.0001))
correct[[n.t]] <- ifelse((is.list(out6P)) && (all.equal(out6P$CI[2], 0.2282918, tolerance=10^(-6))), 1, 0) # Checked against ratesci
names(correct)[[n.t]] <- "out6P"
n.t <- n.t + 1
out6Q <- try(test.NI.binary(n.control=1000, n.experim=1001, e.control=120, e.experim=10, NI.margin=1.5, sig.level=0.025, summary.measure="RR", 
                            print.out=TRUE, unfavourable=TRUE, test.type="MOVER",
                            M.boot=2000, BB.adj=0.0001))
correct[[n.t]] <- ifelse((is.list(out6Q)) && (all.equal(out6Q$CI[2], 0.1514325, tolerance=10^(-6))), 1, 0) # Checked against ratesci
names(correct)[[n.t]] <- "out6Q"
n.t <- n.t + 1
out6R <- try(test.NI.binary(n.control=100, n.experim=101, e.control=12, e.experim=0,  NI.margin=1.5, sig.level=0.025, summary.measure="RR", 
                            print.out=TRUE, unfavourable=TRUE, test.type="Gart.Nam",
                            M.boot=2000, BB.adj=0.0001))
correct[[n.t]] <- ifelse((is.list(out6R)) && (all.equal(out6R$CI[2], 0.25062, tolerance=10^(-6))), 1, 0) # Checked against ratesci
names(correct)[[n.t]] <- "out6R"
n.t <- n.t + 1
out6S <- try(test.NI.binary(n.control=1000, n.experim=1001, e.control=120, e.experim=10, NI.margin=1.5, sig.level=0.025, summary.measure="RR", 
                            print.out=TRUE, unfavourable=TRUE, test.type="Gart.Nam",
                            M.boot=2000, BB.adj=0.0001))
correct[[n.t]] <- ifelse((is.list(out6S)) && (all.equal(out6S$CI[2], 0.151537, tolerance=10^(-6))), 1, 0) # Checked against ratesci
names(correct)[[n.t]] <- "out6S"
n.t <- n.t + 1
out6T <- try(test.NI.binary(n.control=100, n.experim=101, e.control=12, e.experim=0,  NI.margin=1.5, sig.level=0.025, summary.measure="RR", 
                            print.out=TRUE, unfavourable=TRUE, test.type="score.cc",
                            M.boot=2000, BB.adj=0.0001))
correct[[n.t]] <- ifelse((is.list(out6T)) && (all.equal(out6T$CI[2], 0.371133, tolerance=10^(-6))), 1, 0) # Checked against ratesci
names(correct)[[n.t]] <- "out6T"
n.t <- n.t + 1
out6U <- try(test.NI.binary(n.control=1000, n.experim=1001, e.control=120, e.experim=10, NI.margin=1.5, sig.level=0.025, summary.measure="RR", 
                            print.out=TRUE, unfavourable=TRUE, test.type="score.cc",
                            M.boot=2000, BB.adj=0.0001))
correct[[n.t]] <- ifelse((is.list(out6U)) && (all.equal(out6U$CI[2], 0.157835, tolerance=10^(-6))), 1, 0) # Checked against ratesci
names(correct)[[n.t]] <- "out6U"
n.t <- n.t + 1
out6V <- try(test.NI.binary(n.control=100, n.experim=101, e.control=12, e.experim=0,  NI.margin=1.5, sig.level=0.025, summary.measure="RR", 
                            print.out=TRUE, unfavourable=TRUE, test.type="logregression",
                            M.boot=2000, BB.adj=0.0001))
correct[[n.t]] <- ifelse((class(out6V) == "try-error") && (substr(out6V[1], 81, 93) == "logregression"), 1, 0) 
names(correct)[[n.t]] <- "out6V"
n.t <- n.t + 1
out6W <- try(test.NI.binary(n.control=1000, n.experim=1001, e.control=120, e.experim=10, NI.margin=1.5, sig.level=0.025, summary.measure="RR", 
                            print.out=TRUE, unfavourable=TRUE, test.type="logregression",
                            M.boot=2000, BB.adj=0.0001))
correct[[n.t]] <- ifelse((class(out6W) == "list") && (all.equal(as.numeric(out6W$CI[2]), 0.1495272, tolerance=10^(-4))), 1, 0)
names(correct)[[n.t]] <- "out6W"
n.t <- n.t + 1
set.seed(1)
out6X <- try(test.NI.binary(n.control=100, n.experim=101, e.control=12, e.experim=10,  NI.margin=1.5, sig.level=0.025, summary.measure="RR", 
                            print.out=TRUE, unfavourable=TRUE, test.type="bootstrap",
                            M.boot=2000, BB.adj=0.0001))
correct[[n.t]] <- ifelse((is.list(out6X)) && (all.equal(out6X$CI[2], 2.057539, tolerance=10^(-6))), 1, 0) # Checked against desctools
names(correct)[[n.t]] <- "out6X"
n.t <- n.t + 1
set.seed(1)
out6Y <- try(test.NI.binary(n.control=1000, n.experim=1001, e.control=120, e.experim=10, NI.margin=1.5, sig.level=0.025, summary.measure="RR", 
                            print.out=TRUE, unfavourable=TRUE, test.type="bootstrap",
                            M.boot=2005, BB.adj=0.0001))
correct[[n.t]] <- ifelse((is.list(out6Y)) && (all.equal(out6Y$CI[2], 0.1492745, tolerance=10^(-6))), 1, 0) # Checked against desctools
names(correct)[[n.t]] <- "out6Y"
n.t <- n.t + 1
out6Z <- try(test.NI.binary(n.control=100, n.experim=101, e.control=12, e.experim=0,  NI.margin=1.5, sig.level=0.025, summary.measure="RR", 
                            print.out=TRUE, unfavourable=TRUE, test.type="Bailey",
                            M.boot=2000, BB.adj=0.0001))
correct[[n.t]] <- ifelse((is.list(out6Z)) && (all.equal(out6Z$CI[2], 0.3233335, tolerance=10^(-6))), 1, 0) # Checked against desctools
names(correct)[[n.t]] <- "out6Z"
n.t <- n.t + 1
out6AA <- try(test.NI.binary(n.control=1000, n.experim=1001, e.control=120, e.experim=10, NI.margin=1.5, sig.level=0.025, summary.measure="RR", 
                             print.out=TRUE, unfavourable=TRUE, test.type="Bailey",
                             M.boot=2000, BB.adj=0.0001))
correct[[n.t]] <- ifelse((is.list(out6AA)) && (all.equal(out6AA$CI[2], 0.1498864, tolerance=10^(-6))), 1, 0) # Checked against desctools
names(correct)[[n.t]] <- "out6AA"
n.t <- n.t + 1
out6AB <- try(test.NI.binary(n.control=100, n.experim=101, e.control=12, e.experim=0,  NI.margin=1.5, sig.level=0.025, summary.measure="RR", 
                             print.out=TRUE, unfavourable=TRUE, test.type="Noether",
                             M.boot=2000, BB.adj=0.0001))
correct[[n.t]] <- ifelse((is.list(out6AB)) && (all.equal(out6AB$CI[2], 0.1574018, tolerance=10^(-6))), 1, 0) # Checked against desctools
names(correct)[[n.t]] <- "out6AB"
n.t <- n.t + 1
out6AC <- try(test.NI.binary(n.control=1000, n.experim=1001, e.control=120, e.experim=10, NI.margin=1.5, sig.level=0.025, summary.measure="RR", 
                             print.out=TRUE, unfavourable=TRUE, test.type="Noether",
                             M.boot=2000, BB.adj=0.0001))
correct[[n.t]] <- ifelse((is.list(out6AC)) && (all.equal(out6AC$CI[2], 0.1364572, tolerance=10^(-6))), 1, 0) # Checked against desctools
names(correct)[[n.t]] <- "out6AC"
n.t <- n.t + 1
out6AD <- try(test.NI.binary(n.control=10, n.experim=11, e.control=2, e.experim=0,  NI.margin=1.5, sig.level=0.025, summary.measure="RR", 
                             print.out=TRUE, unfavourable=TRUE, test.type="Agresti.Min",
                             M.boot=2000, BB.adj=0.0001))
correct[[n.t]] <- ifelse((is.list(out6AD)) && (all.equal(out6AD$CI[2], 1.855417, tolerance=10^(-6))), 1, 0) # Checked against desctools
names(correct)[[n.t]] <- "out6AD"
n.t <- n.t + 1
out6AE <- try(test.NI.binary(n.control=10, n.experim=11, e.control=2, e.experim=0,  NI.margin=1.5, sig.level=0.025, summary.measure="RR", 
                             print.out=TRUE, unfavourable=TRUE, test.type="uncond.midp",
                             M.boot=2000, BB.adj=0.0001))
correct[[n.t]] <- ifelse((is.list(out6AE)) && (all.equal(out6AE$CI[2], 1.855417, tolerance=10^(-6))), 1, 0) # Checked against desctools
names(correct)[[n.t]] <- "out6AE"
n.t <- n.t + 1
out6AF <- try(test.NI.binary(n.control=10, n.experim=11, e.control=2, e.experim=0,  NI.margin=1.5, sig.level=0.025, summary.measure="RR", 
                             print.out=TRUE, unfavourable=TRUE, test.type="Berger.Boos",
                             M.boot=2000, BB.adj=0.001))
correct[[n.t]] <- ifelse((is.list(out6AF)) && (all.equal(out6AF$CI[2], 1.600651, tolerance=10^(-6))), 1, 0) # Checked against desctools
names(correct)[[n.t]] <- "out6AF"
n.t <- n.t + 1
out6AG <- try(test.NI.binary(n.control=10, n.experim=11, e.control=2, e.experim=0,  NI.margin=1.5, sig.level=0.025, summary.measure="RR", 
                             print.out=TRUE, unfavourable=TRUE, test.type="Chan.Zhang",
                             M.boot=2000, BB.adj=0.0001))
correct[[n.t]] <- ifelse((is.list(out6AG)) && (all.equal(out6AG$CI[2], 2.265278, tolerance=10^(-6))), 1, 0) # Checked against desctools
names(correct)[[n.t]] <- "out6AG"
n.t <- n.t + 1

#####################################################
# Seventh set of checks:
# Now check sample size calculations for certain values on OR scale. These are compared against various comparators

out7A <- try(test.NI.binary(n.control=100, n.experim=100, e.control=15, e.experim=15,  NI.margin=1.5, sig.level=0.025, summary.measure="OR", 
                            print.out=TRUE, unfavourable=TRUE, test.type="Wald.Woolf",
                            M.boot=2000, BB.adj=0.0001))
correct[[n.t]] <- ifelse((is.list(out7A)) && (all.equal(out7A$CI[2], 2.173332, tolerance=10^(-5))) && out7A$non.inferiority == FALSE, 1, 0) # Checked against riskratio epitools
names(correct)[[n.t]] <- "out7A"
n.t <- n.t + 1
out7B <- try(test.NI.binary(n.control=1000, n.experim=1001, e.control=12, e.experim=0,  NI.margin=1.5, sig.level=0.025, summary.measure="OR", 
                            print.out=TRUE, unfavourable=TRUE, test.type="Wald.Woolf",
                            M.boot=2000, BB.adj=0.0001))
correct[[n.t]] <- ifelse((class(out7B) == "try-error") && (substr(out7B[1], 83, 100) == "Wald.Woolf not inf"), 1, 0) 
names(correct)[[n.t]] <- "out7B"
n.t <- n.t + 1
out7C <- try(test.NI.binary(n.control=100, n.experim=100, e.control=15, e.experim=15,  NI.margin=2.2, sig.level=0.025, summary.measure="OR", 
                            print.out=TRUE, unfavourable=TRUE, test.type="Wald.Woolf",
                            M.boot=2000, BB.adj=0.0001))
correct[[n.t]] <- ifelse((is.list(out7C)) && (all.equal(out7C$CI[2], 2.173332, tolerance=10^(-5))) && out7C$non.inferiority == TRUE, 1, 0) # Checked against riskratio epitools
names(correct)[[n.t]] <- "out7C"
n.t <- n.t + 1
out7D <- try(test.NI.binary(n.control=100, n.experim=100, e.control=15, e.experim=15,  NI.margin=1.5, sig.level=0.05, summary.measure="OR", 
                            print.out=TRUE, unfavourable=TRUE, test.type="Wald.Woolf",
                            M.boot=2000, BB.adj=0.0001))
correct[[n.t]] <- ifelse((is.list(out7D)) && (all.equal(out7D$CI[2], 1.918338, tolerance=10^(-6))), 1, 0) # Checked against riskratio epitools
names(correct)[[n.t]] <- "out7D"
n.t <- n.t + 1
out7E <- try(test.NI.binary(n.control=100, n.experim=100, e.control=15, e.experim=15,  NI.margin=0.75, sig.level=0.05, summary.measure="OR", 
                            print.out=TRUE, unfavourable=FALSE, test.type="Wald.Woolf",
                            M.boot=2000, BB.adj=0.0001))
correct[[n.t]] <- ifelse((is.list(out7E)) && (all.equal(out7E$CI[2], 1.918338, tolerance=10^(-6))), 1, 0) # Checked against riskratio epitools
names(correct)[[n.t]] <- "out7E"
n.t <- n.t + 1
out7F <- try(test.NI.binary(n.control=100, n.experim=101, e.control=12, e.experim=0,  NI.margin=1.5, sig.level=0.025, summary.measure="OR", 
                            print.out=TRUE, unfavourable=TRUE, test.type="adjusted.Wald.Woolf",
                            M.boot=2000, BB.adj=0.0001))
correct[[n.t]] <- ifelse((is.list(out7F)) && (all.equal(out7F$CI[2], 0.5975629, tolerance=10^(-6))), 1, 0) # Checked against contingencytables
names(correct)[[n.t]] <- "out7F"
n.t <- n.t + 1
out7G <- try(test.NI.binary(n.control=1000, n.experim=1001, e.control=120, e.experim=10, NI.margin=1.5, sig.level=0.025, summary.measure="OR", 
                            print.out=TRUE, unfavourable=TRUE, test.type="adjusted.Wald.Woolf",
                            M.boot=2000, BB.adj=0.0001))
correct[[n.t]] <- ifelse((is.list(out7G)) && (all.equal(out7G$CI[2], 0.1463366, tolerance=10^(-6))), 1, 0) # Checked against contingencytables
names(correct)[[n.t]] <- "out7G"
n.t <- n.t + 1
out7H <- try(test.NI.binary(n.control=100, n.experim=101, e.control=12, e.experim=0,  NI.margin=1.5, sig.level=0.025, summary.measure="OR", 
                            print.out=TRUE, unfavourable=TRUE, test.type="inverse.hyperbolic.sine",
                            M.boot=2000, BB.adj=0.0001))
correct[[n.t]] <- ifelse((is.list(out7H)) && (all.equal(out7H$CI[2], 0.2789178, tolerance=10^(-6))), 1, 0) # Checked against contingencytables
names(correct)[[n.t]] <- "out7H"
n.t <- n.t + 1
out7I <- try(test.NI.binary(n.control=1000, n.experim=1001, e.control=120, e.experim=10, NI.margin=1.5, sig.level=0.025, summary.measure="OR", 
                            print.out=TRUE, unfavourable=TRUE, test.type="inverse.hyperbolic.sine",
                            M.boot=2000, BB.adj=0.0001))
correct[[n.t]] <- ifelse((is.list(out7I)) && (all.equal(out7I$CI[2], 0.1404025, tolerance=10^(-6))), 1, 0) # Checked against contingencytables
names(correct)[[n.t]] <- "out7I"
n.t <- n.t + 1
out7J <- try(test.NI.binary(n.control=100, n.experim=101, e.control=12, e.experim=0,  NI.margin=1.5, sig.level=0.025, summary.measure="OR", 
                            print.out=TRUE, unfavourable=TRUE, test.type="Cornfield.exact",
                            M.boot=2000, BB.adj=0.0001))
correct[[n.t]] <- ifelse((is.list(out7J)) && (all.equal(out7J$CI[2], 0.2562646, tolerance=10^(-6))), 1, 0) # Checked against desctools
names(correct)[[n.t]] <- "out7J"
n.t <- n.t + 1
out7K <- try(test.NI.binary(n.control=1000, n.experim=1001, e.control=120, e.experim=10, NI.margin=1.5, sig.level=0.025, summary.measure="OR", 
                            print.out=TRUE, unfavourable=TRUE, test.type="Cornfield.exact",
                            M.boot=2000, BB.adj=0.0001))
correct[[n.t]] <- ifelse((is.list(out7K)) && (all.equal(out7K$CI[2], 0.1371403, tolerance=10^(-6))), 1, 0) # Checked against desctools
names(correct)[[n.t]] <- "out7K"
n.t <- n.t + 1
out7L <- try(test.NI.binary(n.control=100, n.experim=101, e.control=12, e.experim=0,  NI.margin=1.5, sig.level=0.025, summary.measure="OR", 
                            print.out=TRUE, unfavourable=TRUE, test.type="Cornfield.midp",
                            M.boot=2000, BB.adj=0.0001))
correct[[n.t]] <- ifelse((is.list(out7L)) && (all.equal(out7L$CI[2], 0.2562646, tolerance=10^(-6))), 1, 0) # Checked against contingencytables
names(correct)[[n.t]] <- "out7L"
n.t <- n.t + 1
out7M <- try(test.NI.binary(n.control=1000, n.experim=1001, e.control=120, e.experim=10, NI.margin=1.5, sig.level=0.025, summary.measure="OR", 
                            print.out=TRUE, unfavourable=TRUE, test.type="Cornfield.midp",
                            M.boot=2000, BB.adj=0.0001))
correct[[n.t]] <- ifelse((is.list(out7M)) && (all.equal(out7M$CI[2], 0.1371403, tolerance=10^(-6))), 1, 0) # Checked against contingencytables
names(correct)[[n.t]] <- "out7M"
n.t <- n.t + 1
out7N <- try(test.NI.binary(n.control=100, n.experim=101, e.control=12, e.experim=0,  NI.margin=1.5, sig.level=0.025, summary.measure="OR", 
                            print.out=TRUE, unfavourable=TRUE, test.type="Miettinen.Nurminen",
                            M.boot=2000, BB.adj=0.0001))
correct[[n.t]] <- ifelse((is.list(out7N)) && (all.equal(out7N$CI[2], 0.2846007, tolerance=10^(-6))), 1, 0) # Checked against contingencytables
names(correct)[[n.t]] <- "out7N"
n.t <- n.t + 1
out7AO <- try(test.NI.binary(n.control=1000, n.experim=1001, e.control=120, e.experim=10, NI.margin=1.5, sig.level=0.025, summary.measure="OR", 
                             print.out=TRUE, unfavourable=TRUE, test.type="Miettinen.Nurminen",
                             M.boot=2000, BB.adj=0.0001))
correct[[n.t]] <- ifelse((is.list(out7AO)) && (all.equal(out7AO$CI[2], 0.1404683, tolerance=10^(-6))), 1, 0) # Checked against contingencytables
names(correct)[[n.t]] <- "out7AO"
n.t <- n.t + 1
out7P <- try(test.NI.binary(n.control=100, n.experim=101, e.control=12, e.experim=0,  NI.margin=1.5, sig.level=0.025, summary.measure="OR", 
                            print.out=TRUE, unfavourable=TRUE, test.type="MOVER",
                            M.boot=2000, BB.adj=0.0001))
correct[[n.t]] <- ifelse((is.list(out7P)) && (all.equal(out7P$CI[2], 0.2099266, tolerance=10^(-6))), 1, 0) # Checked against contingencytables
names(correct)[[n.t]] <- "out7P"
n.t <- n.t + 1
out7Q <- try(test.NI.binary(n.control=1000, n.experim=1001, e.control=120, e.experim=10, NI.margin=1.5, sig.level=0.025, summary.measure="OR", 
                            print.out=TRUE, unfavourable=TRUE, test.type="MOVER",
                            M.boot=2000, BB.adj=0.0001))
correct[[n.t]] <- ifelse((is.list(out7Q)) && (all.equal(out7Q$CI[2], 0.1365741, tolerance=10^(-6))), 1, 0) # Checked against contingencytables
names(correct)[[n.t]] <- "out7Q"
n.t <- n.t + 1
out7R <- try(test.NI.binary(n.control=100, n.experim=101, e.control=12, e.experim=0,  NI.margin=1.5, sig.level=0.025, summary.measure="OR", 
                            print.out=TRUE, unfavourable=TRUE, test.type="Gart.Nam",
                            M.boot=2000, BB.adj=0.0001))
correct[[n.t]] <- ifelse((is.list(out7R)) && (all.equal(out7R$CI[2], 0.233931, tolerance=10^(-6))), 1, 0) # Checked against ratesci
names(correct)[[n.t]] <- "out7R"
n.t <- n.t + 1
out7S <- try(test.NI.binary(n.control=1000, n.experim=1001, e.control=120, e.experim=10, NI.margin=1.5, sig.level=0.025, summary.measure="OR", 
                            print.out=TRUE, unfavourable=TRUE, test.type="Gart.Nam",
                            M.boot=2000, BB.adj=0.0001))
correct[[n.t]] <- ifelse((is.list(out7S)) && (all.equal(out7S$CI[2], 0.136812, tolerance=10^(-6))), 1, 0) # Checked against ratesci
names(correct)[[n.t]] <- "out7S"
n.t <- n.t + 1
out7T <- try(test.NI.binary(n.control=100, n.experim=101, e.control=12, e.experim=0,  NI.margin=1.5, sig.level=0.025, summary.measure="OR", 
                            print.out=TRUE, unfavourable=TRUE, test.type="score.cc",
                            M.boot=2000, BB.adj=0.0001))
correct[[n.t]] <- ifelse((is.list(out7T)) && (all.equal(out7T$CI[2], 0.350693, tolerance=10^(-6))), 1, 0) # Checked against ratesci
names(correct)[[n.t]] <- "out7T"
n.t <- n.t + 1
out7U <- try(test.NI.binary(n.control=1000, n.experim=1001, e.control=120, e.experim=10, NI.margin=1.5, sig.level=0.025, summary.measure="OR", 
                            print.out=TRUE, unfavourable=TRUE, test.type="score.cc",
                            M.boot=2000, BB.adj=0.0001))
correct[[n.t]] <- ifelse((is.list(out7U)) && (all.equal(out7U$CI[2], 0.142669, tolerance=10^(-6))), 1, 0) # Checked against ratesci
names(correct)[[n.t]] <- "out7U"
n.t <- n.t + 1
out7V <- try(test.NI.binary(n.control=100, n.experim=101, e.control=12, e.experim=0,  NI.margin=1.5, sig.level=0.025, summary.measure="OR", 
                            print.out=TRUE, unfavourable=TRUE, test.type="logistic",
                            M.boot=2000, BB.adj=0.0001))
correct[[n.t]] <- ifelse((class(out7V) == "try-error") && (substr(out7V[1], 81, 94) == "With zero cell"), 1, 0) 
names(correct)[[n.t]] <- "out7V"
n.t <- n.t + 1
out7W <- try(test.NI.binary(n.control=1000, n.experim=1001, e.control=120, e.experim=10, NI.margin=1.5, sig.level=0.025, summary.measure="OR", 
                            print.out=TRUE, unfavourable=TRUE, test.type="logistic",
                            M.boot=2000, BB.adj=0.0001))
correct[[n.t]] <- ifelse((is.list(out7W)) && (all.equal(as.numeric(out7W$CI[2]), 0.1348625, tolerance=10^(-5))), 1, 0) # no comparator
names(correct)[[n.t]] <- "out7W"
n.t <- n.t + 1
set.seed(1)
out7X <- try(test.NI.binary(n.control=100, n.experim=101, e.control=12, e.experim=0,  NI.margin=1.5, sig.level=0.025, summary.measure="OR", 
                            print.out=TRUE, unfavourable=TRUE, test.type="bootstrap",
                            M.boot=2000, BB.adj=0.0001))
correct[[n.t]] <- ifelse((class(out7X) == "try-error") && (substr(out7X[1], 81, 96) == "bootstrap method"), 1, 0) 
names(correct)[[n.t]] <- "out7X"
n.t <- n.t + 1
set.seed(1)
out7Y <- try(test.NI.binary(n.control=1000, n.experim=1001, e.control=120, e.experim=20, NI.margin=1.5, sig.level=0.025, summary.measure="OR", 
                            print.out=TRUE, unfavourable=TRUE, test.type="bootstrap",
                            M.boot=2005, BB.adj=0.0001))
correct[[n.t]] <- ifelse((is.list(out7Y)) && (all.equal(out7Y$CI[2], 0.2364948, tolerance=10^(-6))), 1, 0) # No comparators
names(correct)[[n.t]] <- "out7Y"
n.t <- n.t + 1
out7Z <- try(test.NI.binary(n.control=100, n.experim=101, e.control=12, e.experim=0,  NI.margin=1.5, sig.level=0.025, summary.measure="OR", 
                            print.out=TRUE, unfavourable=TRUE, test.type="Baptista.Pike.midp",
                            M.boot=2000, BB.adj=0.0001))
correct[[n.t]] <- ifelse((is.list(out7Z)) && (all.equal(out7Z$CI[2], 0.2419096, tolerance=10^(-6))), 1, 0) # Checked against contingencytables
names(correct)[[n.t]] <- "out7Z"
n.t <- n.t + 1
set.seed(1)
out7AA <- try(test.NI.binary(n.control=1000, n.experim=1001, e.control=120, e.experim=10, NI.margin=1.5, sig.level=0.025, summary.measure="OR", 
                             print.out=TRUE, unfavourable=TRUE, test.type="Baptista.Pike.midp",
                             M.boot=2000, BB.adj=0.0001))
correct[[n.t]] <- ifelse((is.list(out7AA)) && (all.equal(out7AA$CI[2], 0.1395453, tolerance=10^(-6))), 1, 0) # Checked against contingencytables
names(correct)[[n.t]] <- "out7AA"
n.t <- n.t + 1
out7AB <- try(test.NI.binary(n.control=100, n.experim=101, e.control=12, e.experim=0,  NI.margin=1.5, sig.level=0.025, summary.measure="OR", 
                             print.out=TRUE, unfavourable=TRUE, test.type="Baptista.Pike.exact",
                             M.boot=2000, BB.adj=0.0001))
correct[[n.t]] <- ifelse((is.list(out7AB)) && (all.equal(out7AB$CI[2], 0.2419096, tolerance=10^(-6))), 1, 0) # Checked against contingencytables
names(correct)[[n.t]] <- "out7AB"
n.t <- n.t + 1
out7AC <- try(test.NI.binary(n.control=1000, n.experim=1001, e.control=120, e.experim=10, NI.margin=1.5, sig.level=0.025, summary.measure="OR", 
                             print.out=TRUE, unfavourable=TRUE, test.type="Baptista.Pike.exact",
                             M.boot=2000, BB.adj=0.0001))
correct[[n.t]] <- ifelse((is.list(out7AC)) && (all.equal(out7AC$CI[2], 0.1395453, tolerance=10^(-6))), 1, 0) # Checked against contingencytables
names(correct)[[n.t]] <- "out7AC"
n.t <- n.t + 1
out7AD <- try(test.NI.binary(n.control=10, n.experim=11, e.control=2, e.experim=0,  NI.margin=1.5, sig.level=0.025, summary.measure="OR", 
                             print.out=TRUE, unfavourable=TRUE, test.type="Agresti.Min",
                             M.boot=2000, BB.adj=0.0001))
correct[[n.t]] <- ifelse((is.list(out7AD)) && (all.equal(out7AD$CI[2], 1.702387, tolerance=10^(-6))), 1, 0) # Checked against exact2x2
names(correct)[[n.t]] <- "out7AD"
n.t <- n.t + 1
out7AE <- try(test.NI.binary(n.control=10, n.experim=11, e.control=2, e.experim=0,  NI.margin=1.5, sig.level=0.025, summary.measure="OR", 
                             print.out=TRUE, unfavourable=TRUE, test.type="uncond.midp",
                             M.boot=2000, BB.adj=0.0001))
correct[[n.t]] <- ifelse((is.list(out7AE)) && (all.equal(out7AE$CI[2], 1.702387, tolerance=10^(-6))), 1, 0) # Checked against exact2x2
names(correct)[[n.t]] <- "out7AE"
n.t <- n.t + 1
out7AF <- try(test.NI.binary(n.control=10, n.experim=11, e.control=2, e.experim=0,  NI.margin=1.5, sig.level=0.025, summary.measure="OR", 
                             print.out=TRUE, unfavourable=TRUE, test.type="Berger.Boos",
                             M.boot=2000, BB.adj=0.001))
correct[[n.t]] <- ifelse((is.list(out7AF)) && (all.equal(out7AF$CI[2], 1.702387, tolerance=10^(-6))), 1, 0) # Checked against exact2x2
names(correct)[[n.t]] <- "out7AF"
n.t <- n.t + 1
out7AG <- try(test.NI.binary(n.control=10, n.experim=11, e.control=2, e.experim=0,  NI.margin=1.5, sig.level=0.025, summary.measure="OR", 
                             print.out=TRUE, unfavourable=TRUE, test.type="Chan.Zhang",
                             M.boot=2000, BB.adj=0.0001))
correct[[n.t]] <- ifelse((is.list(out7AG)) && (all.equal(out7AG$CI[2], 2.437464, tolerance=10^(-6))), 1, 0) # Checked against exact2x2
names(correct)[[n.t]] <- "out7AG"
n.t <- n.t + 1
out7AH <- try(test.NI.binary(n.control=100, n.experim=101, e.control=12, e.experim=0, NI.margin=1.5, sig.level=0.025, summary.measure="OR",
                             print.out=TRUE, unfavourable=TRUE, test.type="MOVER.R",
                             M.boot=2000, BB.adj=0.0001))
correct[[n.t]] <- ifelse((is.list(out7AH)) && (all.equal(out7AH$CI[2], 0.3075885, tolerance=10^(-6))), 1, 0) # Checked against contingencytables
names(correct)[[n.t]] <- "out7AH"
n.t <- n.t + 1
out7AI <- try(test.NI.binary(n.control=1000, n.experim=1001, e.control=120, e.experim=10, NI.margin=1.5, sig.level=0.025, summary.measure="OR",
                             print.out=TRUE, unfavourable=TRUE, test.type="MOVER.R",
                             M.boot=2000, BB.adj=0.0001))
correct[[n.t]] <- ifelse((is.list(out7AI)) && (all.equal(out7AI$CI[2], 0.1447186, tolerance=10^(-6))), 1, 0) # Checked against contingencytables
names(correct)[[n.t]] <- "out7AI"
n.t <- n.t + 1

#####################################################
# Eight set of checks:
# Check results for AS. These are not compared against any comparator

out8A <- try(test.NI.binary(n.control=100, n.experim=100, e.control=15, e.experim=15,  NI.margin=0.1, sig.level=0.025, summary.measure="AS", 
                            print.out=TRUE, unfavourable=TRUE, test.type="Wald",
                            M.boot=2000, BB.adj=0.0001))
correct[[n.t]] <- ifelse((is.list(out8A)) && (all.equal(out8A$CI[2], 0.1385904, tolerance=10^(-6))) && out8A$non.inferiority==F, 1, 0) 
names(correct)[[n.t]] <- "out8A"
n.t <- n.t + 1
out8B <- try(test.NI.binary(n.control=1000, n.experim=1001, e.control=12, e.experim=0,  NI.margin=0.1, sig.level=0.025, summary.measure="AS", 
                            print.out=TRUE, unfavourable=TRUE, test.type="Wald",
                            M.boot=2000, BB.adj=0.0001))
correct[[n.t]] <- ifelse((is.list(out8B)) && (all.equal(out8B$CI[2], -0.06594961, tolerance=10^(-6))), 1, 0) 
names(correct)[[n.t]] <- "out8B"
n.t <- n.t + 1
out8C <- try(test.NI.binary(n.control=100, n.experim=100, e.control=15, e.experim=15,  NI.margin=0.15, sig.level=0.025, summary.measure="AS", 
                            print.out=TRUE, unfavourable=TRUE, test.type="Wald",
                            M.boot=2000, BB.adj=0.0001))
correct[[n.t]] <- ifelse((is.list(out8C)) && (all.equal(out8C$CI[2], 0.1385904, tolerance=10^(-6))) && out8C$non.inferiority==T, 1, 0) 
names(correct)[[n.t]] <- "out8C"
n.t <- n.t + 1
out8D <- try(test.NI.binary(n.control=1000, n.experim=1001, e.control=12, e.experim=0,  NI.margin=0.1, sig.level=0.05, summary.measure="AS", 
                            print.out=TRUE, unfavourable=TRUE, test.type="Wald",
                            M.boot=2000, BB.adj=0.0001))
correct[[n.t]] <- ifelse((is.list(out8D)) && (all.equal(out8D$CI[2], -0.07299393, tolerance=10^(-6))), 1, 0) 
names(correct)[[n.t]] <- "out8D"
n.t <- n.t + 1
out8E <- try(test.NI.binary(n.control=1000, n.experim=1001, e.control=12, e.experim=0,  NI.margin=-0.1, sig.level=0.05, summary.measure="AS", 
                            print.out=TRUE, unfavourable=F, test.type="Wald",
                            M.boot=2000, BB.adj=0.0001))
correct[[n.t]] <- ifelse((is.list(out8E)) && (all.equal(out8E$CI[2], -0.07299393, tolerance=10^(-6))), 1, 0) 
names(correct)[[n.t]] <- "out8E"
n.t <- n.t + 1

#####################################################
# Ninth set of checks:
# Check results for recursive p estimation:

out9A<-try(test.NI.binary(n.control=100, n.experim=100, e.control=15, e.experim=15,  NI.margin=0.05, sig.level=0.025, summary.measure="RD", 
                          print.out=TRUE, unfavourable=TRUE, test.type="Wald",
                          M.boot=2000, BB.adj=0.0001, recursive.p.estim = TRUE))
correct[[n.t]]<-ifelse((inherits(out9A,"list"))&&(all.equal(out9A$p,0.161051, tolerance=10^(-5)))&&out9A$non.inferiority==F,1,0) # Checked against prop.test
names(correct)[[n.t]]<-"out9A"
n.t=n.t+1
out9B <- try(test.NI.binary(n.control=100, n.experim=100, e.control=15, e.experim=15, NI.margin=1.5, sig.level=0.025, summary.measure="RR", 
                            print.out=TRUE, unfavourable=TRUE, test.type="Wald.Katz",
                            M.boot=2000, BB.adj=0.0001, recursive.p.estim = TRUE))
correct[[n.t]] <- ifelse((is.list(out9B)) && (all.equal(out9B$p, 0.1142154, tolerance=10^(-5))) && !out9B$non.inferiority, 1, 0) # Checked against riskratio epitools
names(correct)[[n.t]] <- "out9B"
n.t <- n.t + 1
out9C <- try(test.NI.binary(n.control=100, n.experim=100, e.control=15, e.experim=15, NI.margin=1.5, sig.level=0.025, summary.measure="OR", 
                            print.out=TRUE, unfavourable=TRUE, test.type="Wald.Woolf",
                            M.boot=2000, BB.adj=0.0001, recursive.p.estim = TRUE))
correct[[n.t]] <- ifelse((is.list(out9C)) && (all.equal(out9C$p, 0.1529769, tolerance=10^(-5))) && !out9C$non.inferiority, 1, 0) # Checked against riskratio epitools
names(correct)[[n.t]] <- "out9C"
n.t <- n.t + 1
out9D <- try(test.NI.binary(n.control=100, n.experim=100, e.control=15, e.experim=15, NI.margin=0.1, sig.level=0.025, summary.measure="AS", 
                            print.out=TRUE, unfavourable=TRUE, test.type="Wald",
                            M.boot=2000, BB.adj=0.0001, recursive.p.estim = TRUE))
correct[[n.t]] <- ifelse((is.list(out9D)) && (all.equal(out9D$p, 0.0786496, tolerance=10^(-5))) && !out9D$non.inferiority, 1, 0) # Checked against riskratio epitools
names(correct)[[n.t]] <- "out9D"
n.t <- n.t + 1
out9E<-try(test.NI.binary(n.control=100, n.experim=100, e.control=15, e.experim=5,  NI.margin=0.05, sig.level=0.025, summary.measure="RD", 
                          print.out=TRUE, unfavourable=TRUE, test.type="Wald",
                          M.boot=2000, BB.adj=0.0001, recursive.p.estim = TRUE))
correct[[n.t]]<-ifelse((inherits(out9E,"list"))&&(all.equal(out9E$p,0.0001680954, tolerance=10^(-5)))&&out9E$non.inferiority==T,1,0) # Checked against prop.test
names(correct)[[n.t]]<-"out9E"
n.t=n.t+1
out9F<-try(test.NI.binary(n.control=100, n.experim=100, e.control=5, e.experim=15,  NI.margin=0.05, sig.level=0.025, summary.measure="RD", 
                          print.out=TRUE, unfavourable=TRUE, test.type="Wald",
                          M.boot=2000, BB.adj=0.0001, recursive.p.estim = TRUE))
correct[[n.t]]<-ifelse((inherits(out9F,"list"))&&(all.equal(out9F$p,.8840011, tolerance=10^(-5)))&&out9F$non.inferiority==F,1,0) # Checked against prop.test
names(correct)[[n.t]]<-"out9F"
n.t=n.t+1
out9G<-try(test.NI.binary(n.control=100, n.experim=100, e.control=15, e.experim=15,  NI.margin=-0.05, sig.level=0.025, summary.measure="RD", 
                          print.out=TRUE, unfavourable=FALSE, test.type="Wald",
                          M.boot=2000, BB.adj=0.0001, recursive.p.estim = TRUE))
correct[[n.t]]<-ifelse((inherits(out9G,"list"))&&(all.equal(out9G$p,0.161051, tolerance=10^(-5)))&&out9G$non.inferiority==F,1,0) # Checked against prop.test
names(correct)[[n.t]]<-"out9G"
n.t=n.t+1
out9H<-try(test.NI.binary(n.control=100, n.experim=100, e.control=15, e.experim=5,  NI.margin=-0.05, sig.level=0.025, summary.measure="RD", 
                          print.out=TRUE, unfavourable=FALSE, test.type="Wald",
                          M.boot=2000, BB.adj=0.0001, recursive.p.estim = TRUE))
correct[[n.t]]<-ifelse((inherits(out9H,"list"))&&(all.equal(out9H$p,0.8840011, tolerance=10^(-5)))&&out9H$non.inferiority==F,1,0) # Checked against prop.test
names(correct)[[n.t]]<-"out9H"
n.t=n.t+1
out9I<-try(test.NI.binary(n.control=100, n.experim=100, e.control=5, e.experim=15,  NI.margin=-0.05, sig.level=0.025, summary.measure="RD", 
                          print.out=TRUE, unfavourable=FALSE, test.type="Wald",
                          M.boot=2000, BB.adj=0.0001, recursive.p.estim = TRUE))
correct[[n.t]]<-ifelse((inherits(out9I,"list"))&&(all.equal(out9I$p,.0001680954, tolerance=10^(-5)))&&out9I$non.inferiority==T,1,0) # Checked against prop.test
names(correct)[[n.t]]<-"out9I"
n.t=n.t+1

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
  names(correct)[which(vec.correct==0)]
}
# Now list NA tests
if (tot.NA>0) {
  cat("Tests returning NAs:\n")
  names(correct)[which(is.na(vec.correct))]
}

t.NI.b<-(tot.correct==number.of.tests) 


