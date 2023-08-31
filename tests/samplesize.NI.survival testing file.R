###################################################
### samplesize.NI.survival testing file       #####
### 14-08-2023                                #####
###################################################

# Load dani:
# library(dani)

###############################################################################################################################################
### Here we test the samplesize function

### survival data: ###########################################################

#Initialise vector of outputs 
correct<-list(NULL)
n.t<-1

#####################################################
# First set of checks:
# Check that it stops for non acceptable values of hazards and risks:

out1A<-try(samplesize.NI.survival("1.2",0.2,0.2,1.5))
correct[[n.t]]<-ifelse((inherits(out1A, "try-error"))&&(grepl("is.numeric(hazard.target) is not TRUE", out1A[1], fixed=T  )),1,0) 
names(correct)[[n.t]]<-"out1A"
n.t=n.t+1
out1B<-try(samplesize.NI.survival(-1.2,0.2,0.2,1.5))
correct[[n.t]]<-ifelse((inherits(out1B, "try-error"))&&(grepl("hazard.target > 0 is not TRUE", out1B[1], fixed=T  )),1,0) 
names(correct)[[n.t]]<-"out1B"
n.t=n.t+1
out1C<-try(samplesize.NI.survival(1.2,"0.2",0.2,1.5))
correct[[n.t]]<-ifelse((inherits(out1C, "try-error"))&&(grepl("is.numeric(p.control.expected) is not TRUE", out1C[1], fixed=T )),1,0) 
names(correct)[[n.t]]<-"out1C"
n.t=n.t+1
out1D<-try(samplesize.NI.survival(1.2,-0.2,0.2,1.5))
correct[[n.t]]<-ifelse((inherits(out1D, "try-error"))&&(grepl("p.control.expected > 0 is not TRUE", out1D[1], fixed=T  )),1,0) 
names(correct)[[n.t]]<-"out1D"
n.t=n.t+1
out1E<-try(samplesize.NI.survival(1.2,1.2,0.2,1.5))
correct[[n.t]]<-ifelse((inherits(out1E, "try-error"))&&(grepl("p.control.expected <= 1 is not TRUE", out1E[1], fixed=T  )),1,0) 
names(correct)[[n.t]]<-"out1E"
n.t=n.t+1
out1F<-try(samplesize.NI.survival(1.2,0.2,"0.2",1.5))
correct[[n.t]]<-ifelse((inherits(out1F, "try-error"))&&(grepl("is.numeric(p.experim.target) is not TRUE", out1F[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out1F"
n.t=n.t+1
out1G<-try(samplesize.NI.survival(1.2,0.2,-0.2,1.5))
correct[[n.t]]<-ifelse((inherits(out1G, "try-error"))&&(grepl("p.experim.target > 0 is not TRUE", out1G[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out1G"
n.t=n.t+1
out1H<-try(samplesize.NI.survival(1.2,0.2,1.2,1.5))
correct[[n.t]]<-ifelse((inherits(out1H, "try-error"))&&(grepl("p.experim.target <= 1 is not TRUE", out1H[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out1H"
n.t=n.t+1

#####################################################
# Second set of checks:
# Check that it stops for non acceptable values of NI margins:

out2A<-try(samplesize.NI.survival(1.2,0.2,0.2,"1.5"))
correct[[n.t]]<-ifelse((inherits(out2A, "try-error"))&&(grepl("is.numeric(NI.margin) is not TRUE", out2A[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out2A"
n.t=n.t+1
out2B<-try(samplesize.NI.survival(1.2,0.2,0.2,-1))
correct[[n.t]]<-ifelse((inherits(out2B, "try-error"))&&(grepl("NI.margin > 0 is not TRUE", out2B[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out2B"
n.t=n.t+1
out2C<-try(samplesize.NI.survival(1.2,0.2,0.2,0.3))
correct[[n.t]]<-ifelse((inherits(out2C, "try-error"))&&(grepl("With an unfavourable outcome, the NI margin for HR should be >1.", out2C[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out2C"
n.t=n.t+1
out2D<-try(samplesize.NI.survival(1.2,0.2,0.2,1.3, unfavourable = F))
correct[[n.t]]<-ifelse((inherits(out2D, "try-error"))&&(grepl("With a favourable outcome, the NI margin for HR should be <1.", out2D[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out2D"
n.t=n.t+1

# Check that it stops if hazard implies inferiority with current margin:
out2E<-try(samplesize.NI.survival(1.2,0.2,0.2,1.1))
correct[[n.t]]<-ifelse((inherits(out2E, "try-error"))&&(grepl("In the alternative hypothesis the experimental treatment is not non-inferior.", out2E[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out2E"
n.t=n.t+1
out2F<-try(samplesize.NI.survival(0.7,0.2,0.2,0.8, unfavourable = F))
correct[[n.t]]<-ifelse((inherits(out2F, "try-error"))&&(grepl("In the alternative hypothesis the experimental treatment is not non-inferior.", out2F[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out2F"
n.t=n.t+1

#####################################################
# Third set of checks:
# Check that it stops for unacceptable values of significance level:

out3A<-try(samplesize.NI.survival(1.2,0.2,0.2,1.5, sig.level="0.025"))
correct[[n.t]]<-ifelse((inherits(out3A, "try-error"))&&(grepl("is.numeric(sig.level) is not TRUE", out3A[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out3A"
n.t=n.t+1
out3B<-try(samplesize.NI.survival(1.2,0.2,0.2,1.5, sig.level=-0.4))
correct[[n.t]]<-ifelse((inherits(out3B, "try-error"))&&(grepl("sig.level > 0 is not TRUE", out3B[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out3B"
n.t=n.t+1
out3C<-try(samplesize.NI.survival(1.2,0.2,0.2,1.5, sig.level=1))
correct[[n.t]]<-ifelse((inherits(out3C, "try-error"))&&(grepl("sig.level < 0.5 is not TRUE", out3C[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out3C"
n.t=n.t+1

#####################################################
# Fourth set of checks:
# Check that it stops for unacceptable values of power:

out4A<-try(samplesize.NI.survival(1.2,0.2,0.2,1.5, power="0.025"))
correct[[n.t]]<-ifelse((inherits(out4A, "try-error"))&&(grepl("is.numeric(power) is not TRUE", out4A[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4A"
n.t=n.t+1
out4B<-try(samplesize.NI.survival(1.2,0.2,0.2,1.5, power=-0.4))
correct[[n.t]]<-ifelse((inherits(out4B, "try-error"))&&(grepl("power > 0 is not TRUE", out4B[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4B"
n.t=n.t+1
out4C<-try(samplesize.NI.survival(1.2,0.2,0.2,1.5, power=1))
correct[[n.t]]<-ifelse((inherits(out4C, "try-error"))&&(grepl("power < 1 is not TRUE", out4C[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4C"
n.t=n.t+1

# Check that it stops for unacceptable values of allocation ratio:
out4D<-try(samplesize.NI.survival(1.2,0.2,0.2,1.5, r="0.025"))
correct[[n.t]]<-ifelse((inherits(out4D, "try-error"))&&(grepl("is.numeric(r) is not TRUE", out4D[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4D"
n.t=n.t+1
out4E<-try(samplesize.NI.survival(1.2,0.2,0.2,1.5, r=-0.4))
correct[[n.t]]<-ifelse((inherits(out4E, "try-error"))&&(grepl("r > 0 is not TRUE", out4E[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4E"
n.t=n.t+1

# Check that it works for wrong summary measure value:
out4F<-try(samplesize.NI.survival(1.2,0.2,0.2,1.5, summary.measure="pippo"))
correct[[n.t]]<-ifelse((inherits(out4F, "try-error"))&&(grepl("summary.measure %in%", out4F[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4F"
n.t=n.t+1

# Check that it works when print.out incorrectly specified:
out4G<-try(samplesize.NI.survival(1.2,0.2,0.2,1.5, print.out=NA))
correct[[n.t]]<-ifelse((inherits(out4G, "try-error"))&&(grepl("!is.na(print.out) is not TRUE", out4G[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4G"
n.t=n.t+1

# Check that it works when test.type incorrectly specified:
out4H<-try(samplesize.NI.survival(1.2,0.2,0.2,1.5, test.type=NA))
correct[[n.t]]<-ifelse((inherits(out4H, "try-error"))&&(grepl("is.character(test.type) is not TRUE", out4H[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4H"
n.t=n.t+1
out4I<-try(samplesize.NI.survival(1.2,0.2,0.2,1.5, test.type="pippo"))
correct[[n.t]]<-ifelse((inherits(out4I, "try-error"))&&(grepl("test.type %in%", out4I[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4I"
n.t=n.t+1

# Check that it works when unfavourable incorrectly specified:
out4J<-try(samplesize.NI.survival(1.2,0.2,0.2,1.5, unfavourable=NA))
correct[[n.t]]<-ifelse((inherits(out4J, "try-error"))&&(grepl("!is.na(unfavourable) is not TRUE", out4J[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4J"
n.t=n.t+1
out4K<-try(samplesize.NI.survival(1.2,0.2,0.2,1.5, unfavourable="pippo"))
correct[[n.t]]<-ifelse((inherits(out4K, "try-error"))&&(grepl("is.logical(unfavourable) is not TRUE", out4K[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4K"
n.t=n.t+1

#####################################################
# Fifth set of checks:
# Now check sample size calculations for certain values on HR scale using Schoenfeld method. These are compared against results from function Cox.NIS in package TrialSize

out5A<-try(samplesize.NI.survival(1.2,0.2,0.2,1.5))
correct[[n.t]]<-ifelse((inherits(out5A,"numeric"))&&(all.equal(out5A[2],2111)),1,0)  
names(correct)[[n.t]]<-"out5A"
n.t=n.t+1
out5B<-try(samplesize.NI.survival(0.9,0.2,0.2,1.5))
correct[[n.t]]<-ifelse((inherits(out5B,"numeric"))&&(all.equal(out5B[2],403)),1,0)  
names(correct)[[n.t]]<-"out5B"
n.t=n.t+1
out5C<-try(samplesize.NI.survival(1.2,1,1,1.5))
correct[[n.t]]<-ifelse((inherits(out5C,"numeric"))&&(all.equal(out5C[2],423)),1,0)  
names(correct)[[n.t]]<-"out5C"
n.t=n.t+1
out5D<-try(samplesize.NI.survival(1,0.2,0.2,1.5, sig.level = 0.05))
correct[[n.t]]<-ifelse((inherits(out5D,"numeric"))&&(all.equal(out5D[2],521)),1,0)  
names(correct)[[n.t]]<-"out5D"
n.t=n.t+1
out5E<-try(samplesize.NI.survival(1,0.2,0.2,1.5, power = 0.64))
correct[[n.t]]<-ifelse((inherits(out5E,"numeric"))&&(all.equal(out5E[2],327)),1,0)  
names(correct)[[n.t]]<-"out5E"
n.t=n.t+1
out5F<-try(samplesize.NI.survival(1,0.2,0.2,1.5, r=2))
correct[[n.t]]<-ifelse((inherits(out5F,"numeric"))&&(all.equal(out5F[2],959)),1,0)  
names(correct)[[n.t]]<-"out5F"
n.t=n.t+1
out5G<-try(samplesize.NI.survival(1,0.2,0.2,1.5, r=0.5))
correct[[n.t]]<-ifelse((inherits(out5G,"numeric"))&&(all.equal(out5G[2],480)),1,0)  
names(correct)[[n.t]]<-"out5G"
n.t=n.t+1
out5H<-try(samplesize.NI.survival(1.2,0.2,0.2,2))
correct[[n.t]]<-ifelse((inherits(out5H,"numeric"))&&(all.equal(out5H[2],403)),1,0)  
names(correct)[[n.t]]<-"out5H"
n.t=n.t+1

# Now check sample size calculations for other configurations for which we have unfortunately no comparator yet. 
out5I<-try(samplesize.NI.survival(1,0.2,0.4,1.5))
correct[[n.t]]<-ifelse((inherits(out5I,"numeric"))&&(all.equal(out5I[2],427)),1,0)  
names(correct)[[n.t]]<-"out5I"
n.t=n.t+1
out5J<-try(samplesize.NI.survival(1,0.2,0.2,1.5, test.type="logrank.Freedman"))
correct[[n.t]]<-ifelse((inherits(out5J,"numeric"))&&(all.equal(out5J[2],657)),1,0)  
names(correct)[[n.t]]<-"out5J"
n.t=n.t+1
out5K<-try(samplesize.NI.survival(1,0.2,0.2,0.5, unfavourable=F))
correct[[n.t]]<-ifelse((inherits(out5K,"numeric"))&&(all.equal(out5K[2],219)),1,0)  
names(correct)[[n.t]]<-"out5K"
n.t=n.t+1

#####################################################
# Sixth set of checks:
# CHeck for calculations on DRMST scale:

out6A<-try(samplesize.NI.survival(1.2,0.2,0.2,-0.15, summary.measure="DRMST"))
correct[[n.t]]<-ifelse((inherits(out6A, "try-error"))&&(grepl("Only a test based on Kaplan Meier is currently supported for DRMST.", out6A[1], fixed=T  )),1,0) 
names(correct)[[n.t]]<-"out6A"
n.t=n.t+1
out6B<-try(samplesize.NI.survival(1.2,0.2,0.2,-0.15, summary.measure="DRMST", test.type = "KM"))
correct[[n.t]]<-ifelse((inherits(out6B, "try-error"))&&(grepl("Function to calculate sample size for DRMST is under development, and not yet available.", out6B[1], fixed=T  )),1,0) 
names(correct)[[n.t]]<-"out6B"
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

ss.NI.s<-(tot.correct==number.of.tests) 


