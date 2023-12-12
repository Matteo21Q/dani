###################################################
### samplesize.ROCI.binary  testing file      #####
### 23-08-2023                                #####
###################################################

# Load dani:
# library(dani)

#Initialise vector of outputs 
correct<-list(NULL)
n.t<-1

#####################################################
# First set of checks:
# Check that it stops for non acceptable values of expected probabilities:

out1A<-try(samplesize.ROCI.binary(p.expected.curve=c("0.1",0.1,0.1,0.1,0.1),NI.margin=0.1, se.method="delta", treatment.levels = c(1,2,3,4,5)))
correct[[n.t]]<-ifelse((inherits(out1A, "try-error"))&&(grepl("is.numeric(p.expected.curve) is not TRUE", out1A[1], fixed=T  )),1,0) 
names(correct)[[n.t]]<-"out1A"
n.t=n.t+1
out1B<-try(samplesize.ROCI.binary(p.expected.curve=c(-0.1,0.1,0.1,0.1,0.1),NI.margin=0.1, se.method="delta", treatment.levels = c(1,2,3,4,5)))
correct[[n.t]]<-ifelse((inherits(out1B, "try-error"))&&(grepl("all(p.expected.curve > 0) is not TRUE", out1B[1], fixed=T  )),1,0) 
names(correct)[[n.t]]<-"out1B"
n.t=n.t+1
out1C<-try(samplesize.ROCI.binary(p.expected.curve=c(1.1,0.1,0.1,0.1,0.1),NI.margin=0.1, se.method="delta", treatment.levels = c(1,2,3,4,5)))
correct[[n.t]]<-ifelse((inherits(out1C, "try-error"))&&(grepl("all(p.expected.curve < 1) is not TRUE", out1C[1], fixed=T )),1,0) 
names(correct)[[n.t]]<-"out1C"
n.t=n.t+1

#####################################################
# Second set of checks:
# Check that it stops for non-acceptable margins:

out2A<-try(samplesize.ROCI.binary(p.expected.curve=c(0.1,0.1,0.1,0.1,0.1),NI.margin="0.1", se.method="delta", treatment.levels = c(1,2,3,4,5)))
correct[[n.t]]<-ifelse((inherits(out2A, "try-error"))&&(grepl("is.numeric(NI.margin) is not TRUE", out2A[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out2A"
n.t=n.t+1
out2B<-try(samplesize.ROCI.binary(p.expected.curve=c(0.1,0.1,0.1,0.1,0.1),NI.margin=-0.1, summary.measure="RR", se.method="delta", treatment.levels = c(1,2,3,4,5)))
correct[[n.t]]<-ifelse((inherits(out2B, "try-error"))&&(grepl("When outcome is unfavourable, NI margins on the risk ratio or odds ratio scale need to all be >1.", out2B[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out2B"
n.t=n.t+1
out2C<-try(samplesize.ROCI.binary(p.expected.curve=c(0.1,0.1,0.1,0.1,0.1),NI.margin=-0.1, summary.measure="OR", se.method="delta", treatment.levels = c(1,2,3,4,5)))
correct[[n.t]]<-ifelse((inherits(out2C, "try-error"))&&(grepl("When outcome is unfavourable, NI margins on the risk ratio or odds ratio scale need to all be >1.", out2C[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out2C"
n.t=n.t+1
out2D<-try(samplesize.ROCI.binary(p.expected.curve=c(0.1,0.1,0.1,0.1,0.1),NI.margin=1.2, se.method="delta", treatment.levels = c(1,2,3,4,5)))
correct[[n.t]]<-ifelse((inherits(out2D, "try-error"))&&(grepl("NI margins cannot be greater than 1, i.e. 100 percentage points, or otherwise the test is meaningless.", out2D[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out2D"
n.t=n.t+1
out2E<-try(samplesize.ROCI.binary(p.expected.curve=c(0.1,0.1,0.1,0.1,0.1),NI.margin=-1.2, se.method="delta", unfavourable=F, treatment.levels = c(1,2,3,4,5)))
correct[[n.t]]<-ifelse((inherits(out2E, "try-error"))&&(grepl("NI margins cannot be lower than -1, i.e. -100 percentage points, or otherwise the test is meaningless.", out2E[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out2E"
n.t=n.t+1

#####################################################
# Third set of checks:
# Check that it stops for unacceptable values of significance level:

out3A<-try(samplesize.ROCI.binary(p.expected.curve=c(0.1,0.1,0.1,0.1,0.1),NI.margin=0.1, se.method="delta", sig.level="0.025", treatment.levels = c(1,2,3,4,5)))
correct[[n.t]]<-ifelse((inherits(out3A, "try-error"))&&(grepl("is.numeric(sig.level) is not TRUE", out3A[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out3A"
n.t=n.t+1
out3B<-try(samplesize.ROCI.binary(p.expected.curve=c(0.1,0.1,0.1,0.1,0.1),NI.margin=0.1, se.method="delta", sig.level=0, treatment.levels = c(1,2,3,4,5)))
correct[[n.t]]<-ifelse((inherits(out3B, "try-error"))&&(grepl("sig.level > 0 is not TRUE", out3B[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out3B"
n.t=n.t+1
out3C<-try(samplesize.ROCI.binary(p.expected.curve=c(0.1,0.1,0.1,0.1,0.1),NI.margin=0.1, se.method="delta", sig.level=1, treatment.levels = c(1,2,3,4,5)))
correct[[n.t]]<-ifelse((inherits(out3C, "try-error"))&&(grepl("sig.level < 0.5 is not TRUE", out3C[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out3C"
n.t=n.t+1

#####################################################
# Fourth set of checks:
# Check with other wrong arguments:

# Check that it stops for unacceptable values of power:
out4A<-try(samplesize.ROCI.binary(p.expected.curve=c(0.1,0.1,0.1,0.1,0.1),NI.margin=0.1, se.method="delta", power="0.9", treatment.levels = c(1,2,3,4,5)))
correct[[n.t]]<-ifelse((inherits(out4A, "try-error"))&&(grepl("is.numeric(power) is not TRUE", out4A[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4A"
n.t=n.t+1
out4B<-try(samplesize.ROCI.binary(p.expected.curve=c(0.1,0.1,0.1,0.1,0.1),NI.margin=0.1, se.method="delta", power=0, treatment.levels = c(1,2,3,4,5)))
correct[[n.t]]<-ifelse((inherits(out4B, "try-error"))&&(grepl("power > 0 is not TRUE", out4B[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4B"
n.t=n.t+1
out4C<-try(samplesize.ROCI.binary(p.expected.curve=c(0.1,0.1,0.1,0.1,0.1),NI.margin=0.1, se.method="delta", power=1, treatment.levels = c(1,2,3,4,5)))
correct[[n.t]]<-ifelse((inherits(out4C, "try-error"))&&(grepl("power < 1 is not TRUE", out4C[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4C"
n.t=n.t+1

# Check that it works for wrong summary measure value:
out4D<-try(samplesize.ROCI.binary(p.expected.curve=c(0.1,0.1,0.1,0.1,0.1),NI.margin=0.1, se.method="delta", summary.measure="pippo", treatment.levels = c(1,2,3,4,5)))
correct[[n.t]]<-ifelse((inherits(out4D, "try-error"))&&(grepl("summary.measure ==", out4D[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4D"
n.t=n.t+1

# Check that it works when print.out incorrectly specified:
out4E<-try(samplesize.ROCI.binary(p.expected.curve=c(0.1,0.1,0.1,0.1,0.1),NI.margin=0.1, se.method="delta", print.out=NA, treatment.levels = c(1,2,3,4,5)))
correct[[n.t]]<-ifelse((inherits(out4E, "try-error"))&&(grepl("!is.na(print.out) is not TRUE", out4E[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4E"
n.t=n.t+1

# Check that it works when se.method incorrectly specified:
out4F<-try(samplesize.ROCI.binary(p.expected.curve=c(0.1,0.1,0.1,0.1,0.1),NI.margin=0.1, se.method=1, treatment.levels = c(1,2,3,4,5)))
correct[[n.t]]<-ifelse((inherits(out4F, "try-error"))&&(grepl("is.character(se.method) is not TRUE", out4F[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4F"
n.t=n.t+1
out4G<-try(samplesize.ROCI.binary(p.expected.curve=c(0.1,0.1,0.1,0.1,0.1),NI.margin=0.1, se.method="pippo", treatment.levels = c(1,2,3,4,5)))
correct[[n.t]]<-ifelse((inherits(out4G, "try-error"))&&(grepl("se.method %in%", out4G[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4G"
n.t=n.t+1

# Check that it works when tr.model incorrectly specified:
out4H<-try(samplesize.ROCI.binary(p.expected.curve=c(0.1,0.1,0.1,0.1,0.1),NI.margin=0.1, se.method="delta", tr.model=1, treatment.levels = c(1,2,3,4,5)))
correct[[n.t]]<-ifelse((inherits(out4H, "try-error"))&&(grepl("is.character(tr.model) is not TRUE", out4H[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4H"
n.t=n.t+1
out4I<-try(samplesize.ROCI.binary(p.expected.curve=c(0.1,0.1,0.1,0.1,0.1),NI.margin=0.1, se.method="delta", tr.model="pippo", treatment.levels = c(1,2,3,4,5)))
correct[[n.t]]<-ifelse((inherits(out4I, "try-error"))&&(grepl("tr.model %in%", out4I[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4I"
n.t=n.t+1

# Check that it works when unfavourable incorrectly specified:
out4J<-try(samplesize.ROCI.binary(p.expected.curve=c(0.1,0.1,0.1,0.1,0.1),NI.margin=0.1, se.method="delta", unfavourable=NA, treatment.levels = c(1,2,3,4,5)))
correct[[n.t]]<-ifelse((inherits(out4J, "try-error"))&&(grepl("!is.na(unfavourable) is not TRUE", out4J[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4J"
n.t=n.t+1
out4K<-try(samplesize.ROCI.binary(p.expected.curve=c(0.1,0.1,0.1,0.1,0.1),NI.margin=0.1, se.method="delta", unfavourable="pippo", treatment.levels = c(1,2,3,4,5)))
correct[[n.t]]<-ifelse((inherits(out4K, "try-error"))&&(grepl("is.logical(unfavourable) is not TRUE", out4K[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4K"
n.t=n.t+1

# Check that it works when minimisation incorrectly specified:
out4L<-try(samplesize.ROCI.binary(p.expected.curve=c(0.1,0.1,0.1,0.1,0.1),NI.margin=0.1, se.method="delta", minimisation=NA, treatment.levels = c(1,2,3,4,5)))
correct[[n.t]]<-ifelse((inherits(out4L, "try-error"))&&(grepl("!is.na(minimisation) is not TRUE", out4L[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4L"
n.t=n.t+1
out4M<-try(samplesize.ROCI.binary(p.expected.curve=c(0.1,0.1,0.1,0.1,0.1),NI.margin=0.1, se.method="delta", minimisation="pippo", treatment.levels = c(1,2,3,4,5)))
correct[[n.t]]<-ifelse((inherits(out4M, "try-error"))&&(grepl("is.logical(minimisation) is not TRUE", out4M[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4M"
n.t=n.t+1

# Check that it works when treatment levels incorrectly specified:
out4N<-try(samplesize.ROCI.binary(p.expected.curve=c(0.1,0.1,0.1,0.1,0.1),NI.margin=0.1, se.method="delta", treatment.levels = c("1",2,3,4,5)))
correct[[n.t]]<-ifelse((inherits(out4N, "try-error"))&&(grepl("is.numeric(treatment.levels) is not TRUE", out4N[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4N"
n.t=n.t+1
out4O<-try(samplesize.ROCI.binary(p.expected.curve=c(0.1,0.1,0.1,0.1,0.1),NI.margin=0.1, se.method="delta", treatment.levels = c(2,3,4,5)))
correct[[n.t]]<-ifelse((inherits(out4O, "try-error"))&&(grepl("length(treatment.levels) == length(p.expected.curve) is not TRUE", out4O[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4O"
n.t=n.t+1

# Check that it works when M.boot incorrectly specified:
out4P<-try(samplesize.ROCI.binary(p.expected.curve=c(0.1,0.1,0.1,0.1,0.1),NI.margin=0.1, se.method="bootstrap", M.boot=NA, treatment.levels = c(1,2,3,4,5)))
correct[[n.t]]<-ifelse((inherits(out4P, "try-error"))&&(grepl("is.numeric(M.boot) is not TRUE", out4P[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4P"
n.t=n.t+1
out4Q<-try(samplesize.ROCI.binary(p.expected.curve=c(0.1,0.1,0.1,0.1,0.1),NI.margin=0.1, se.method="bootstrap", M.boot=0, treatment.levels = c(1,2,3,4,5)))
correct[[n.t]]<-ifelse((inherits(out4Q, "try-error"))&&(grepl("M.boot > 1 is not TRUE", out4Q[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4Q"
n.t=n.t+1

# Check that it works when parallel incorrectly specified:
out4R<-try(samplesize.ROCI.binary(p.expected.curve=c(0.1,0.1,0.1,0.1,0.1),NI.margin=0.1, se.method="bootstrap", parallel=NA, treatment.levels = c(1,2,3,4,5)))
correct[[n.t]]<-ifelse((inherits(out4R, "try-error"))&&(grepl("is.character(parallel) is not TRUE", out4R[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4R"
n.t=n.t+1
out4S<-try(samplesize.ROCI.binary(p.expected.curve=c(0.1,0.1,0.1,0.1,0.1),NI.margin=0.1, se.method="bootstrap", parallel="pippo", treatment.levels = c(1,2,3,4,5)))
correct[[n.t]]<-ifelse((inherits(out4S, "try-error"))&&(grepl("parallel %in%", out4S[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4S"
n.t=n.t+1

# Check that it works when boot.CI.type incorrectly specified:
out4T<-try(samplesize.ROCI.binary(p.expected.curve=c(0.1,0.1,0.1,0.1,0.1),NI.margin=0.1, se.method="bootstrap", bootCI.type=2, treatment.levels = c(1,2,3,4,5)))
correct[[n.t]]<-ifelse((inherits(out4T, "try-error"))&&(grepl("is.character(bootCI.type) is not TRUE", out4T[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4T"
n.t=n.t+1
out4U<-try(samplesize.ROCI.binary(p.expected.curve=c(0.1,0.1,0.1,0.1,0.1),NI.margin=0.1, se.method="bootstrap", bootCI.type="pippo", treatment.levels = c(1,2,3,4,5)))
correct[[n.t]]<-ifelse((inherits(out4U, "try-error"))&&(grepl("bootCI.type %in%", out4U[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4U"
n.t=n.t+1

# Check that it works when boot.CI.type incorrectly specified:
out4V<-try(samplesize.ROCI.binary(p.expected.curve=c(0.1,0.1,0.1,0.1,0.1),NI.margin=0.1, se.method="bootstrap", varest.boot = 2, treatment.levels = c(1,2,3,4,5)))
correct[[n.t]]<-ifelse((inherits(out4V, "try-error"))&&(grepl("is.character(varest.boot) is not TRUE", out4V[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4V"
n.t=n.t+1
out4W<-try(samplesize.ROCI.binary(p.expected.curve=c(0.1,0.1,0.1,0.1,0.1),NI.margin=0.1, se.method="bootstrap", varest.boot = "pippo", treatment.levels = c(1,2,3,4,5)))
correct[[n.t]]<-ifelse((inherits(out4W, "try-error"))&&(grepl("varest.boot %in%", out4W[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4W"
n.t=n.t+1

# Check that it works when round incorrectly specified:
out4X<-try(samplesize.ROCI.binary(p.expected.curve=c(0.1,0.1,0.1,0.1,0.1),NI.margin=0.1, se.method="delta", power=0.9, treatment.levels = c(1,2,3,4,5), round = NA))
correct[[n.t]]<-ifelse((inherits(out4X, "try-error"))&&(grepl("!is.na(round) is not TRUE", out4X[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4X"
n.t=n.t+1
out4O<-try(samplesize.ROCI.binary(p.expected.curve=c(0.1,0.1,0.1,0.1,0.1),NI.margin=0.1, se.method="delta", power=0.9, treatment.levels = c(1,2,3,4,5), round = "pippo"))
correct[[n.t]]<-ifelse((inherits(out4O, "try-error"))&&(grepl("is.logical(round) is not TRUE", out4O[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4O"
n.t=n.t+1

# Check that it stops for unacceptable values of loss to follow up:
out4P<-try(samplesize.ROCI.binary(p.expected.curve=c(0.1,0.1,0.1,0.1,0.1),NI.margin=0.1, se.method="delta", power=0.9, treatment.levels = c(1,2,3,4,5), ltfu = "0.9"))
correct[[n.t]]<-ifelse((inherits(out4P, "try-error"))&&(grepl("is.numeric(ltfu) is not TRUE", out4P[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4P"
n.t=n.t+1
out4Q<-try(samplesize.ROCI.binary(p.expected.curve=c(0.1,0.1,0.1,0.1,0.1),NI.margin=0.1, se.method="delta", power=0.9, treatment.levels = c(1,2,3,4,5), ltfu = -0.9))
correct[[n.t]]<-ifelse((inherits(out4Q, "try-error"))&&(grepl("ltfu >= 0 is not TRUE", out4Q[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4Q"
n.t=n.t+1
out4R<-try(samplesize.ROCI.binary(p.expected.curve=c(0.1,0.1,0.1,0.1,0.1),NI.margin=0.1, se.method="delta", power=0.9, treatment.levels = c(1,2,3,4,5), ltfu = 1))
correct[[n.t]]<-ifelse((inherits(out4R, "try-error"))&&(grepl("ltfu < 1 is not TRUE", out4R[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4R"
n.t=n.t+1


#####################################################
# Fifth set of checks:
# Now check sample size calculations for certain values on RD scale. 

out5A<-try(samplesize.ROCI.binary(p.expected.curve=c(0.1,0.1,0.1,0.1,0.1),NI.margin=0.1, se.method="delta", treatment.levels = c(1,2,3,4,5)))
correct[[n.t]]<-ifelse((inherits(out5A,"list"))&&(all.equal(out5A$ss.total.optimal,537)),1,0) 
names(correct)[[n.t]]<-"out5A"
n.t=n.t+1
out5B<-try(samplesize.ROCI.binary(p.expected.curve=c(0.7,0.7,0.7,0.7,0.7),NI.margin=0.88, se.method="delta", treatment.levels = c(6,9,12,15,18), summary.measure="RR", minimisation = F, unfavourable = F))
correct[[n.t]]<-ifelse((inherits(out5B,"list"))&&(all.equal(out5B$ss.total.optimal,1638)),1,0)
names(correct)[[n.t]]<-"out5B"
n.t=n.t+1
set.seed(1)
out5C<-try(samplesize.ROCI.binary(p.expected.curve=c(0.7,0.7,0.7,0.7,0.7),NI.margin=0.88, se.method="bootstrap", treatment.levels = c(6,9,12,15,18), summary.measure="RR", minimisation = F, unfavourable = F))
correct[[n.t]]<-ifelse((inherits(out5C,"list"))&&(all.equal(out5C$ss.total.optimal,1940)),1,0) 
names(correct)[[n.t]]<-"out5C"
n.t=n.t+1
out5D<-try(samplesize.ROCI.binary(p.expected.curve=c(0.7,0.7,0.7,0.7,0.7),NI.margin=c(0.91,0.9,0.89,0.88), se.method="delta", treatment.levels = c(6,9,12,15,18), summary.measure="RR", minimisation = F, unfavourable = F))
correct[[n.t]]<-ifelse((inherits(out5D,"list"))&&(all.equal(out5D$ss.total.optimal,1638)),1,0) 
names(correct)[[n.t]]<-"out5D"
n.t=n.t+1
out5E<-try(samplesize.ROCI.binary(p.expected.curve=c(0.12,0.12,0.1,0.1,0.1,0.1,0.1),NI.margin=0.1, se.method="delta", treatment.levels = c(1,2,3,4,5,6,7)))
correct[[n.t]]<-ifelse((inherits(out5E,"list"))&&(all.equal(out5E$ss.total.optimal,503)),1,0) 
names(correct)[[n.t]]<-"out5E"
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

ss.ROCI.b<-(tot.correct==number.of.tests) 


