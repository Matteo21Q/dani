#################################################
### convertmargin.binary testing file       #####
### 11-12-2023                              #####
#################################################

# Load dani:
# library(dani)

#Initialise vector of outputs 
correct<-list(NULL)
n.t<-1

#####################################################
# First set of checks:
# Check that it stops for non acceptable values of probs and margins

out1A<-try(convertmargin.binary(p.control.expected=-0.2,NI.margin=0.1,summary.measure.original="RD", summary.measure.target="RR"))
correct[[n.t]]<-ifelse((inherits(out1A, "try-error"))&&(grepl("p.control.expected > 0 is not TRUE", out1A[1], fixed=T  )),1,0) 
names(correct)[[n.t]]<-"out1A"
n.t=n.t+1
out1B<-try(convertmargin.binary(p.control.expected=1.2, NI.margin=0.1,summary.measure.original="RD", summary.measure.target="RR"))
correct[[n.t]]<-ifelse((inherits(out1B, "try-error"))&&(grepl("p.control.expected < 1 is not TRUE", out1B[1], fixed=T  )),1,0) 
names(correct)[[n.t]]<-"out1B"
n.t=n.t+1
out1C<-try(convertmargin.binary(p.control.expected=T, NI.margin=0.1,summary.measure.original="RD", summary.measure.target="RR"))
correct[[n.t]]<-ifelse((inherits(out1C, "try-error"))&&(grepl("is.numeric(p.control.expected) is not TRUE", out1C[1], fixed=T )),1,0) 
names(correct)[[n.t]]<-"out1C"
n.t=n.t+1
out1D<-try(convertmargin.binary(p.control.expected=0.2, NI.margin="0.1", summary.measure.original="RD", summary.measure.target="RR"))
correct[[n.t]]<-ifelse((inherits(out1D, "try-error"))&&(grepl("is.numeric(NI.margin.original) is not TRUE", out1D[1], fixed=T  )),1,0) 
names(correct)[[n.t]]<-"out1D"
n.t=n.t+1

#####################################################
# Second set of checks:
# Check that it stops for negative ratio margins (both OR and RR):

out2A<-try(convertmargin.binary(p.control.expected=0.2, NI.margin=-0.1, summary.measure.original="RR", summary.measure.target="OR"))
correct[[n.t]]<-ifelse((inherits(out2A, "try-error"))&&(grepl("Non-inferiority margin on risk ratio scale should be >0", out2A[1], fixed=T  )),1,0) 
names(correct)[[n.t]]<-"out2A"
n.t=n.t+1
out2B<-try(convertmargin.binary(p.control.expected=0.2, NI.margin=-0.1, summary.measure.original="OR", summary.measure.target="RR"))
correct[[n.t]]<-ifelse((inherits(out2B, "try-error"))&&(grepl("Non-inferiority margin on odds ratio scale should be >0", out2B[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out2B"
n.t=n.t+1

#####################################################
# Third set of checks:
# Check that it stops for unacceptable summary measures:

# First, original summary measure
out3A<-try(convertmargin.binary(p.control.expected=0.2, NI.margin=0.1, summary.measure.original=T, summary.measure.target="RR"))
correct[[n.t]]<-ifelse((inherits(out3A, "try-error"))&&(grepl("summary.measure.original %in%", out3A[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out3A"
n.t=n.t+1

# Second, target summary measure
out3B<-try(convertmargin.binary(p.control.expected=0.2, NI.margin=0.1, summary.measure.original="RD", summary.measure.target=1))
correct[[n.t]]<-ifelse((inherits(out3B, "try-error"))&&(grepl("summary.measure.target %in%", out3B[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out3B"
n.t=n.t+1

#####################################################
# Fourth set of checks:
# Check results for few scenarios. (all should work and return difference as the best frontier, except the last one which should return ratio):

out4A<-try(convertmargin.binary(p.control.expected=0.2, NI.margin=0.1, summary.measure.original="RD", summary.measure.target="RR"))
correct[[n.t]]<-ifelse(((inherits(out4A,"numeric"))&&(isTRUE(all.equal(out4A,1.5, tolerance=10^(-6))))),1,0) 
names(correct)[[n.t]]<-"out4A"
n.t=n.t+1
out4B<-try(convertmargin.binary(p.control.expected=0.3, NI.margin=-0.15, summary.measure.original="RD", summary.measure.target="RR"))
correct[[n.t]]<-ifelse(((inherits(out4B,"numeric"))&&(isTRUE(all.equal(out4B,0.5, tolerance=10^(-6))))),1,0) 
names(correct)[[n.t]]<-"out4B"
n.t=n.t+1
# Check results for same scenarios as above but inverting summary measures:
out4C<-try(convertmargin.binary(p.control.expected=0.2, NI.margin=1.5, summary.measure.original="RR", summary.measure.target="RD"))
correct[[n.t]]<-ifelse(((inherits(out4C,"numeric"))&&(isTRUE(all.equal(out4C,0.1, tolerance=10^(-6))))),1,0) 
names(correct)[[n.t]]<-"out4C"
n.t=n.t+1
out4D<-try(convertmargin.binary(p.control.expected=0.3, NI.margin=0.5, summary.measure.original="RR", summary.measure.target="RD"))
correct[[n.t]]<-ifelse(((inherits(out4D,"numeric"))&&(isTRUE(all.equal(out4D,-0.15, tolerance=10^(-6))))),1,0) 
names(correct)[[n.t]]<-"out4D"
n.t=n.t+1

# COnversion to/from OR:
out4E<-try(convertmargin.binary(p.control.expected=0.2, NI.margin=0.1, summary.measure.original="RD", summary.measure.target="OR"))
correct[[n.t]]<-ifelse(((inherits(out4E,"numeric"))&&(isTRUE(all.equal(out4E,1.7142, tolerance=10^(-3))))),1,0) 
names(correct)[[n.t]]<-"out4E"
n.t=n.t+1
out4F<-try(convertmargin.binary(p.control.expected=0.2, NI.margin=1.7142857, summary.measure.original="OR", summary.measure.target="RD"))
correct[[n.t]]<-ifelse(((inherits(out4F,"numeric"))&&(isTRUE(all.equal(out4F,0.1, tolerance=10^(-3))))),1,0) 
names(correct)[[n.t]]<-"out4F"
n.t=n.t+1

# Conversion to/from AS
out4G<-try(convertmargin.binary(p.control.expected=0.05, NI.margin=0.05, summary.measure.original="RD", summary.measure.target="AS"))
correct[[n.t]]<-ifelse(((inherits(out4G,"numeric"))&&(isTRUE(all.equal(out4G,0.0962, tolerance=10^(-3))))),1,0) 
names(correct)[[n.t]]<-"out4G"
n.t=n.t+1
out4H<-try(convertmargin.binary(p.control.expected=0.05, NI.margin=0.0962371, summary.measure.original="AS", summary.measure.target="RD"))
correct[[n.t]]<-ifelse(((inherits(out4H,"numeric"))&&(isTRUE(all.equal(out4H,0.05, tolerance=10^(-3))))),1,0) 
names(correct)[[n.t]]<-"out4H"
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

cm.b<-(tot.correct==number.of.tests) 


