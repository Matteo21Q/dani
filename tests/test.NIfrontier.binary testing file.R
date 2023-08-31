###################################################
### test.NIfrontier.binary testing file       #####
### 27-07-2023                                #####
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

NI.f<-function(p0) {
  NI.m<-0.1+0.01*(p0-0.1)
  return(NI.m)
}
sig.l<-function(p0) {
  sl<-ifelse((p0<0.05||p0>0.15),0.005,0.025)
  return(sl)
}
out1A<-try(test.NIfrontier.binary("100", 100, 15, 15,  NI.m, sig.l))
correct[[n.t]]<-ifelse((inherits(out1A, "try-error"))&&(grepl("is.numeric(n.control) is not TRUE", out1A[1], fixed=T  )),1,0) 
names(correct)[[n.t]]<-"out1A"
n.t=n.t+1
out1B<-try(test.NIfrontier.binary(100, "100", 15, 15,  NI.m, sig.l))
correct[[n.t]]<-ifelse((inherits(out1B, "try-error"))&&(grepl("is.numeric(n.experim) is not TRUE", out1B[1], fixed=T  )),1,0) 
names(correct)[[n.t]]<-"out1B"
n.t=n.t+1
out1C<-try(test.NIfrontier.binary(100, 100, "15", 15,  NI.m, sig.l))
correct[[n.t]]<-ifelse((inherits(out1C, "try-error"))&&(grepl("is.numeric(e.control) is not TRUE", out1C[1], fixed=T )),1,0) 
names(correct)[[n.t]]<-"out1C"
n.t=n.t+1
out1D<-try(test.NIfrontier.binary(100, 100, 15, "15",  NI.m, sig.l))
correct[[n.t]]<-ifelse((inherits(out1D, "try-error"))&&(grepl("is.numeric(e.experim) is not TRUE", out1D[1], fixed=T  )),1,0) 
names(correct)[[n.t]]<-"out1D"
n.t=n.t+1
out1E<-try(test.NIfrontier.binary(0, 100, 15, 15,  NI.m, sig.l))
correct[[n.t]]<-ifelse((inherits(out1E, "try-error"))&&(grepl("n.control > 0 is not TRUE", out1E[1], fixed=T  )),1,0) 
names(correct)[[n.t]]<-"out1E"
n.t=n.t+1
out1F<-try(test.NIfrontier.binary(100, 0, 15, 15,  NI.m, sig.l))
correct[[n.t]]<-ifelse((inherits(out1F, "try-error"))&&(grepl("n.experim > 0 is not TRUE", out1F[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out1F"
n.t=n.t+1
out1G<-try(test.NIfrontier.binary(100, 100, -1, 15,  NI.m, sig.l))
correct[[n.t]]<-ifelse((inherits(out1G, "try-error"))&&(grepl("e.control >= 0 is not TRUE", out1G[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out1G"
n.t=n.t+1
out1H<-try(test.NIfrontier.binary(100, 100, 15, -1,  NI.m, sig.l))
correct[[n.t]]<-ifelse((inherits(out1H, "try-error"))&&(grepl("e.experim >= 0 is not TRUE", out1H[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out1H"
n.t=n.t+1
out1I<-try(test.NIfrontier.binary(10, 100, 15, 15,  NI.m, sig.l))
correct[[n.t]]<-ifelse((inherits(out1I, "try-error"))&&(grepl("n.control >= e.control is not TRUE", out1I[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out1I"
n.t=n.t+1
out1J<-try(test.NIfrontier.binary(100, 10, 15, 15,  NI.m, sig.l))
correct[[n.t]]<-ifelse((inherits(out1J, "try-error"))&&(grepl("n.experim >= e.experim is not TRUE", out1J[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out1J"
n.t=n.t+1

#####################################################
# Second set of checks:
# Check that it stops for non acceptable values of NI frontier:
out2A<-try(test.NIfrontier.binary(100, 100, 15, 15,  0.05, sig.l))
correct[[n.t]]<-ifelse((inherits(out2A, "try-error"))&&(grepl("is.function(NI.frontier) is not TRUE", out2A[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out2A"
n.t=n.t+1
NI.f2<-function(p0,gigi) {
  NI.m<-0.1+0.01*(p0-0.1)+gigi
  return(NI.m)
}
out2B<-try(test.NIfrontier.binary(100, 100, 15, 15,  NI.f2, sig.l ))
correct[[n.t]]<-ifelse((inherits(out2B, "try-error"))&&(grepl("length(formals(NI.frontier)) == 1 is not TRUE", out2B[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out2B"
n.t=n.t+1

#####################################################
# Third set of checks:
# Check that it stops for unacceptable values of significance level:
out3A<-try(test.NIfrontier.binary(100, 100, 15, 15,  NI.f, "sig.l"))
correct[[n.t]]<-ifelse((inherits(out3A, "try-error"))&&(grepl("is.function(sig.level)", out3A[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out3A"
n.t=n.t+1
sig.l2<-function(p0,gigi) {
  sl<-0.025+0.01(p0-0.1)+gigi
  return(sl)
}
out3B<-try(test.NIfrontier.binary(100, 100, 15, 15,  NI.f, sig.l2))
correct[[n.t]]<-ifelse((inherits(out3B, "try-error"))&&(grepl("is.function(sig.level)", out3B[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out3B"
n.t=n.t+1
sig.l3<-function(p0) {
  sl<-0.025+(p0>0.1)
  return(sl)
}
out3C<-try(test.NIfrontier.binary(100, 100, 15, 15,  NI.f, sig.l3))
correct[[n.t]]<-ifelse((inherits(out3C, "try-error"))&&(grepl("alpha < 0.5 is not TRUE", out3C[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out3C"
n.t=n.t+1

#####################################################
# Fourth set of checks:
# Check with other wrong arguments:

out4A<-try(test.NIfrontier.binary(100, 100, 15, 15,  NI.f, sig.l, summary.measure="pippo"))
correct[[n.t]]<-ifelse((inherits(out4A, "try-error"))&&(grepl("summary.measure ==", out4A[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4A"
n.t=n.t+1

# Check that it works when print.out incorrectly specified:
out4B<-try(test.NIfrontier.binary(100, 100, 15, 15,  NI.f, sig.l, print.out = NA))
correct[[n.t]]<-ifelse((inherits(out4B, "try-error"))&&(grepl("!is.na(print.out) is not TRUE", out4B[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4B"
n.t=n.t+1

# Check that it works when test.type incorrectly specified:
out4C<-try(test.NIfrontier.binary(100, 100, 15, 15,  NI.f, sig.l, test.type = NA))
correct[[n.t]]<-ifelse((inherits(out4C, "try-error"))&&(grepl("is.character(test.type) is not TRUE", out4C[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4C"
n.t=n.t+1
out4D<-try(test.NIfrontier.binary(100, 100, 15, 15,  NI.f, sig.l,  test.type = "pippo"))
correct[[n.t]]<-ifelse((inherits(out4D, "try-error"))&&(grepl("test.type %in% c", out4D[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4D"
n.t=n.t+1

# Check that it works when unfavourable incorrectly specified:
out4E<-try(test.NIfrontier.binary(100, 100, 15, 15,  NI.f, sig.l,  unfavourable = "pippo"))
correct[[n.t]]<-ifelse((inherits(out4E, "try-error"))&&(grepl("is.logical(unfavourable) is not TRUE", out4E[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4E"
n.t=n.t+1
out4F<-try(test.NIfrontier.binary(100, 100, 15, 15,  NI.f, sig.l, unfavourable = NA))
correct[[n.t]]<-ifelse((inherits(out4F, "try-error"))&&(grepl("!is.na(unfavourable) is not TRUE", out4F[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4F"
n.t=n.t+1

# Check that M.boot has acceptable value:
out4G<-try(test.NIfrontier.binary(100, 100, 15, 15,  NI.f, sig.l, test.type="bootstrap", M.boot="2000"))
correct[[n.t]]<-ifelse((inherits(out4G, "try-error"))&&(grepl("is.numeric(M.boot) is not TRUE", out4G[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4G"
n.t=n.t+1
out4H<-try(test.NIfrontier.binary(100, 100, 15, 15,  NI.f, sig.l, test.type="bootstrap", M.boot=1))
correct[[n.t]]<-ifelse((inherits(out4H, "try-error"))&&(grepl("M.boot > 1 is not TRUE", out4H[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4H"
n.t=n.t+1

# Check that BB.adj has acceptable value:
out4I<-try(test.NIfrontier.binary(100, 100, 15, 15,  NI.f, sig.l, test.type="Berger.Boos", BB.adj="0.00001"))
correct[[n.t]]<-ifelse((inherits(out4I, "try-error"))&&(grepl("is.numeric(BB.adj) is not TRUE", out4I[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4I"
n.t=n.t+1
out4J<-try(test.NIfrontier.binary(100, 100, 15, 15,  NI.f, sig.l,  test.type="Berger.Boos", BB.adj=-0.00001))
correct[[n.t]]<-ifelse((inherits(out4J, "try-error"))&&(grepl("BB.adj > 0 is not TRUE", out4J[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4J"
n.t=n.t+1

#####################################################
# Fifth set of checks:
# Now check tests for certain values on RD scale. 

out5A<-try(test.NIfrontier.binary(n.control=100, n.experim=100, e.control=15, e.experim=15,  
                                  NI.frontier=NI.f, sig.level=sig.l, summary.measure="RD", 
                                  print.out=TRUE, unfavourable=TRUE, test.type="Wald",
                                  M.boot=2000, BB.adj=0.0001))
correct[[n.t]]<-ifelse((inherits(out5A,"list"))&&(all.equal(out5A$CI[2],0.09897333))&&out5A$non.inferiority==T,1,0)  
names(correct)[[n.t]]<-"out5A"
n.t=n.t+1
out5B<-try(test.NIfrontier.binary(n.control=100, n.experim=100, e.control=15, e.experim=15,  
                                  NI.frontier=NI.f, sig.level=sig.l, summary.measure="RD", 
                                   print.out=TRUE, unfavourable=TRUE, test.type="LRT",
                                   M.boot=2000, BB.adj=0.0001))
correct[[n.t]]<-ifelse((inherits(out5B,"list"))&&(all.equal(out5B$CI[2],0.09993408, tolerance=10^(-6))),1,0)  
names(correct)[[n.t]]<-"out5B"
n.t=n.t+1
out5C<-try(test.NIfrontier.binary(n.control=90, n.experim=100, e.control=15, e.experim=15,  
                                  NI.frontier=NI.f, sig.level=sig.l, summary.measure="RD", 
                                  print.out=TRUE, unfavourable=TRUE, test.type="LRT",
                                  M.boot=2000, BB.adj=0.0001))
correct[[n.t]]<-ifelse((inherits(out5C,"list"))&&(all.equal(out5C$CI[2],0.119435, tolerance = 10^(-6)))&&out5C$non.inferiority==F,1,0)  
names(correct)[[n.t]]<-"out5C"
n.t=n.t+1
out5D<-try(test.NIfrontier.binary(n.control=100, n.experim=100, e.control=15, e.experim=15,  
                                  NI.frontier=NI.f, sig.level=0.1, summary.measure="RD", 
                                  print.out=TRUE, unfavourable=TRUE, test.type="LRT",
                                  M.boot=2000, BB.adj=0.0001))
correct[[n.t]]<-ifelse((inherits(out5D,"list"))&&(all.equal(out5D$CI[2],0.06534338, tolerance=10^(-6))),1,0)  
names(correct)[[n.t]]<-"out5D"
n.t=n.t+1
NI.f2<-function(p0) {
  NI.m<-2+0.01*(p0-0.1)
  return(NI.m)
}
out5E<-try(test.NIfrontier.binary(n.control=100, n.experim=100, e.control=15, e.experim=15,  
                                  NI.frontier=NI.f2, sig.level=sig.l, summary.measure="RR", 
                                  print.out=TRUE, unfavourable=TRUE, test.type="LRT",
                                  M.boot=2000, BB.adj=0.0001))
correct[[n.t]] <- ifelse((inherits(out5E, "list")) && (all.equal(out5E$CI[2], 2.036753, tolerance=10^(-6))), 1, 0)  
names(correct)[[n.t]] <- "out5E"
n.t <- n.t + 1
out5F<-try(test.NIfrontier.binary(n.control=100, n.experim=100, e.control=15, e.experim=15,  
                                  NI.frontier=NI.f2, sig.level=sig.l, summary.measure="OR", 
                                  print.out=TRUE, unfavourable=TRUE, test.type="LRT",
                                  M.boot=2000, BB.adj=0.0001))
correct[[n.t]] <- ifelse((inherits(out5F, "list")) && (all.equal(out5F$CI[2], 2.202807, tolerance=10^(-6))), 1, 0)  
names(correct)[[n.t]] <- "out5F"
n.t <- n.t + 1
out5G<-try(test.NIfrontier.binary(n.control=100, n.experim=100, e.control=15, e.experim=15,  
                                  NI.frontier=NI.f2, sig.level=sig.l, summary.measure="AS", 
                                  print.out=TRUE, unfavourable=TRUE, test.type="LRT",
                                  M.boot=2000, BB.adj=0.0001))
correct[[n.t]] <- ifelse((inherits(out5G, "list")) && (all.equal(out5G$CI[2], 0.7733731, tolerance=10^(-6))), 1, 0)  
names(correct)[[n.t]] <- "out5G"
n.t <- n.t + 1
NI.f3<-function(p0) {
  NI.m<--0.1-0.01*(p0-0.1)
  return(NI.m)
}
out5H<-try(test.NIfrontier.binary(n.control=100, n.experim=100, e.control=15, e.experim=15,  
                                  NI.frontier=NI.f3, sig.level=sig.l, summary.measure="RD", 
                                  print.out=TRUE, unfavourable=F, test.type="LRT",
                                  M.boot=2000, BB.adj=0.0001))
correct[[n.t]] <- ifelse((inherits(out5H, "list")) && (all.equal(out5H$CI[2], 0.09895053, tolerance=10^(-6))), 1, 0)  
names(correct)[[n.t]] <- "out5H"
n.t <- n.t + 1
out5I<-try(test.NIfrontier.binary(n.control=100, n.experim=100, e.control=15, e.experim=15,  
                                  NI.frontier=NI.f, sig.level=sig.l, summary.measure="RD", 
                                   print.out=TRUE, unfavourable=TRUE, test.type="Newcombe10",
                                   M.boot=2000, BB.adj=0.0001))
correct[[n.t]] <- ifelse((inherits(out5I, "list")) && (all.equal(out5I$CI[2], 0.1005182, tolerance=10^(-6))), 1, 0)  
names(correct)[[n.t]] <- "out5I"
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
  names(correct)[which(vec.correct==0)]
}
# Now list NA tests
if (tot.NA>0) {
  cat("Tests returning NAs:\n")
  names(correct)[which(is.na(vec.correct))]
}

t.NIf.b<-(tot.correct==number.of.tests) 


