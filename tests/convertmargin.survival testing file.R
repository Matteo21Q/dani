#################################################
### convertmargin.survival testing file     #####
### 11-12-2023                              #####
#################################################

# Load dani:
# library(dani)

#Initialise vector of outputs 
correct<-list(NULL)
n.t<-1

#####################################################
# First set of checks:
# Check that it stops for non acceptable values of rate, probs and margins

out1A<-try(convertmargin.survival(p.control.expected=-0.2,t.expected=1, NI.margin.original=0.1,summary.measure.original="DS", summary.measure.target="HR", t.DS=1))
correct[[n.t]]<-ifelse((inherits(out1A, "try-error"))&&(grepl("p.control.expected > 0 is not TRUE", out1A[1], fixed=T  )),1,0) 
names(correct)[[n.t]]<-"out1A"
n.t=n.t+1
out1B<-try(convertmargin.survival(p.control.expected=1.2,t.expected=1, NI.margin.original=0.1,summary.measure.original="DS", summary.measure.target="HR", t.DS=1))
correct[[n.t]]<-ifelse((inherits(out1B, "try-error"))&&(grepl("p.control.expected < 1 is not TRUE", out1B[1], fixed=T  )),1,0) 
names(correct)[[n.t]]<-"out1B"
n.t=n.t+1
out1C<-try(convertmargin.survival(p.control.expected="0.2",t.expected=1, NI.margin.original=0.1,summary.measure.original="DS", summary.measure.target="HR", t.DS=1))
correct[[n.t]]<-ifelse((inherits(out1C, "try-error"))&&(grepl("is.numeric(p.control.expected) is not TRUE", out1C[1], fixed=T )),1,0) 
names(correct)[[n.t]]<-"out1C"
n.t=n.t+1
out1D<-try(convertmargin.survival(p.control.expected=0.2,t.expected=1, NI.margin.original="0.1",summary.measure.original="DS", summary.measure.target="HR", t.DS=1))
correct[[n.t]]<-ifelse((inherits(out1D, "try-error"))&&(grepl("is.numeric(NI.margin.original) is not TRUE", out1D[1], fixed=T  )),1,0) 
names(correct)[[n.t]]<-"out1D"
n.t=n.t+1
out1E<-try(convertmargin.survival(rate.control.expected=-0.2, NI.margin.original=0.1,summary.measure.original="DS", summary.measure.target="HR", t.DS=1))
correct[[n.t]]<-ifelse((inherits(out1E, "try-error"))&&(grepl("rate.control.expected > 0 is not TRUE", out1E[1], fixed=T  )),1,0) 
names(correct)[[n.t]]<-"out1E"
n.t=n.t+1
out1F<-try(convertmargin.survival(rate.control.expected="0.2", NI.margin.original=0.1,summary.measure.original="DS", summary.measure.target="HR", t.DS=1))
correct[[n.t]]<-ifelse((inherits(out1F, "try-error"))&&(grepl("is.numeric(rate.control.expected) is not TRUE", out1F[1], fixed=T  )),1,0) 
names(correct)[[n.t]]<-"out1F"
n.t=n.t+1
#####################################################
# Second set of checks:
# Check that it stops for negative ratio margins (HR) and wrong times:

out2A<-try(convertmargin.survival(p.control.expected=0.2,t.expected=1, NI.margin.original=-0.1, summary.measure.original="HR", summary.measure.target="DS", t.DS=1))
correct[[n.t]]<-ifelse((inherits(out2A, "try-error"))&&(grepl("Non-inferiority margin as a hazard ratio should be >0", out2A[1], fixed=T  )),1,0) 
names(correct)[[n.t]]<-"out2A"
n.t=n.t+1
out2B<-try(convertmargin.survival(p.control.expected=0.2, t.expected=-1, NI.margin.original=0.1,summary.measure.original="DS", summary.measure.target="HR", t.DS=1))
correct[[n.t]]<-ifelse((inherits(out2B, "try-error"))&&(grepl("t.expected > 0 is not TRUE", out2B[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out2B"
n.t=n.t+1
out2C<-try(convertmargin.survival(p.control.expected=0.2, t.expected=1, NI.margin.original=0.1,summary.measure.original="DS", summary.measure.target="HR", t.DS=-1))
correct[[n.t]]<-ifelse((inherits(out2C, "try-error"))&&(grepl("Please provide t.DS as a numeric, positive, horizon time for DS", out2C[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out2C"
n.t=n.t+1
out2D<-try(convertmargin.survival(p.control.expected=0.2, t.expected=1, NI.margin.original=0.1,summary.measure.original="DS", summary.measure.target="HR", t.DS="1"))
correct[[n.t]]<-ifelse((inherits(out2D, "try-error"))&&(grepl("Please provide t.DS as a numeric, positive, horizon time for DS", out2D[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out2D"
n.t=n.t+1
out2E<-try(convertmargin.survival(p.control.expected=0.2, t.expected=1, NI.margin.original=0.1,summary.measure.original="DRMST", summary.measure.target="HR", tau.RMST=-1))
correct[[n.t]]<-ifelse((inherits(out2E, "try-error"))&&(grepl("Please provide tau.RMST as a numeric, positive, horizon time for RMST", out2E[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out2E"
n.t=n.t+1
out2F<-try(convertmargin.survival(p.control.expected=0.2, t.expected=1, NI.margin.original=0.1,summary.measure.original="DRMST", summary.measure.target="HR", tau.RMST="1"))
correct[[n.t]]<-ifelse((inherits(out2F, "try-error"))&&(grepl("Please provide tau.RMST as a numeric, positive, horizon time for RMST", out2F[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out2F"
n.t=n.t+1


#####################################################
# Third set of checks:
# Check that it stops for unacceptable summary measures:

# First, original summary measure
out3A<-try(convertmargin.survival(p.control.expected=0.2, t.expected=1, NI.margin.original=0.1,summary.measure.original=T, summary.measure.target="HR", t.DS=1))
correct[[n.t]]<-ifelse((inherits(out3A, "try-error"))&&(grepl("summary.measure.original %in%", out3A[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out3A"
n.t=n.t+1

# Second, target summary measure
out3B<-try(convertmargin.survival(p.control.expected=0.2, t.expected=1, NI.margin.original=0.1,summary.measure.original="DS", summary.measure.target=1, t.DS=1))
correct[[n.t]]<-ifelse((inherits(out3B, "try-error"))&&(grepl("summary.measure.target %in%", out3B[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out3B"
n.t=n.t+1

#####################################################
# Fourth set of checks:
# Check results for few scenarios. 

out4A<-try(convertmargin.survival(p.control.expected=0.1,t.expected=3, NI.margin.original=0.05,summary.measure.original="DS", summary.measure.target="HR", t.DS=3))
correct[[n.t]]<-ifelse(((inherits(out4A,"numeric"))&&(isTRUE(all.equal(out4A,1.5, tolerance=10^(-1))))),1,0) 
names(correct)[[n.t]]<-"out4A"
n.t=n.t+1
out4B<-try(convertmargin.survival(p.control.expected=0.1,t.expected=3, NI.margin.original=0.05,summary.measure.original="DS", summary.measure.target="DRMST", t.DS=3, tau.RMST=3))
correct[[n.t]]<-ifelse(((inherits(out4B,"numeric"))&&(isTRUE(all.equal(out4B,0.078, tolerance=10^(-1))))),1,0) 
names(correct)[[n.t]]<-"out4B"
n.t=n.t+1

# Check results for same scenarios as above but inverting summary measures:
out4C<-try(convertmargin.survival(p.control.expected=0.1,t.expected=3, NI.margin.original=1.5,summary.measure.original="HR", summary.measure.target="DS", t.DS=3))
correct[[n.t]]<-ifelse(((inherits(out4C,"numeric"))&&(isTRUE(all.equal(out4C,0.05, tolerance=10^(-1))))),1,0) 
names(correct)[[n.t]]<-"out4C"
n.t=n.t+1
out4D<-try(convertmargin.survival(p.control.expected=0.1,t.expected=3, NI.margin.original=0.078,summary.measure.original="DRMST", summary.measure.target="DS", t.DS=3, tau.RMST=3))
correct[[n.t]]<-ifelse(((inherits(out4D,"numeric"))&&(isTRUE(all.equal(out4D,0.05, tolerance=10^(-1))))),1,0) 
names(correct)[[n.t]]<-"out4D"
n.t=n.t+1

##################################################
#### Now summarise results

vec.correct<-unlist(correct)  # Create vector from list
number.of.tests<-length(vec.correct)   # How many tests did we do?
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

cm.s<-(tot.correct==number.of.tests) 


