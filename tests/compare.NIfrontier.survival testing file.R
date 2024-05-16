###################################################
### compare.NIfrontier.survival testing file #####
### 20-07-2023                                #####
###################################################

# Load dani:
# library(dani)

#Initialise vector of outputs 
correct<-list(NULL)
n.t<-1

#####################################################
# First set of checks:
# Check that it stops for non acceptable values for rates:

out1A<-try(compare.NIfrontier.survival(rate.control.expected="0.05", rate.experim.target=0.05, rates.range=c(0.01,0.1), NI.margin=1.2, summary.measure="HR", tau.RMST=3, t.DS=3))
correct[[n.t]]<-ifelse((inherits(out1A, "try-error"))&&(grepl("rates should be specified as numeric", out1A[1], fixed=T  )),1,0) 
names(correct)[[n.t]]<-"out1A"
n.t=n.t+1
out1B<-try(compare.NIfrontier.survival(rate.control.expected=0.05, rate.experim.target="0.05", rates.range=c(0.01,0.1), NI.margin=1.2, summary.measure="HR", tau.RMST=3, t.DS=3))
correct[[n.t]]<-ifelse((inherits(out1B, "try-error"))&&(grepl("rates should be specified as numeric", out1B[1], fixed=T  )),1,0) 
names(correct)[[n.t]]<-"out1B"
n.t=n.t+1
out1C<-try(compare.NIfrontier.survival(rate.control.expected=-0.05, rate.experim.target=0.05, rates.range=c(0.01,0.1), NI.margin=1.2, summary.measure="HR", tau.RMST=3, t.DS=3))
correct[[n.t]]<-ifelse((inherits(out1C, "try-error"))&&(grepl("Event rates should be positive.", out1C[1], fixed=T )),1,0) 
names(correct)[[n.t]]<-"out1C"
n.t=n.t+1
out1D<-try(compare.NIfrontier.survival(rate.control.expected=0.05, rate.experim.target=-0.05, rates.range=c(0.01,0.1), NI.margin=1.2, summary.measure="HR", tau.RMST=3, t.DS=3))
correct[[n.t]]<-ifelse((inherits(out1D, "try-error"))&&(grepl("Event rates should be positive.", out1D[1], fixed=T  )),1,0) 
names(correct)[[n.t]]<-"out1D"
n.t=n.t+1

#####################################################
# Second set of checks:
# Check that it stops for non acceptable values for NI margins:

out2A<-try(compare.NIfrontier.survival(rate.control.expected=0.05, rate.experim.target=0.05, rates.range=c(0.01,0.1), NI.margin="1.2", summary.measure="HR", tau.RMST=3, t.DS=3))
correct[[n.t]]<-ifelse((inherits(out2A, "try-error"))&&(grepl("NI margins should be of numeric class.", out2A[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out2A"
n.t=n.t+1
out2B<-try(compare.NIfrontier.survival(rate.control.expected=0.05, rate.experim.target=0.05, rates.range=c(0.01,0.1), NI.margin="1.2", summary.measure="DRMST", tau.RMST=3, t.DS=3))
correct[[n.t]]<-ifelse((inherits(out2B, "try-error"))&&(grepl("NI margins should be of numeric class.", out2B[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out2B"
n.t=n.t+1
out2C<-try(compare.NIfrontier.survival(rate.control.expected=0.05, rate.experim.target=0.05, rates.range=c(0.01,0.1), NI.margin="1.2", summary.measure="DS", tau.RMST=3, t.DS=3))
correct[[n.t]]<-ifelse((inherits(out2C, "try-error"))&&(grepl("NI margins should be of numeric class.", out2C[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out2C"
n.t=n.t+1
out2D<-try(compare.NIfrontier.survival(rate.control.expected=0.05, rate.experim.target=0.05, rates.range=c(0.01,0.1), NI.margin=-1.2, summary.measure="HR", tau.RMST=3, t.DS=3))
correct[[n.t]]<-ifelse((inherits(out2D, "try-error"))&&(grepl("NI margin should be >0 when defined as a HR.", out2D[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out2D"
n.t=n.t+1

#####################################################
# Third set of checks:
# Test behavior for wrong rates.range format. Should stop and warn: 

out3A<-try(compare.NIfrontier.survival(rate.control.expected=0.05, rate.experim.target=0.05, rates.range=c("0.01","0.1"), NI.margin=1.2, summary.measure="HR", tau.RMST=3, t.DS=3))
correct[[n.t]]<-ifelse((inherits(out3A, "try-error"))&&(grepl("rates.range should be a numeric vector of length 2, with rates>0", out3A[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out3A"
n.t=n.t+1
out3B<-try(compare.NIfrontier.survival(rate.control.expected=0.05, rate.experim.target=0.05, rates.range=c(0.01,0.1,0.2), NI.margin=1.2, summary.measure="HR", tau.RMST=3, t.DS=3))
correct[[n.t]]<-ifelse((inherits(out3B, "try-error"))&&(grepl("rates.range should be a numeric vector of length 2, with rates>0", out3B[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out3B"
n.t=n.t+1
out3C<-try(compare.NIfrontier.survival(rate.control.expected=0.05, rate.experim.target=0.05, rates.range=c(0.05), NI.margin=1.2, summary.measure="HR", tau.RMST=3, t.DS=3))
correct[[n.t]]<-ifelse((inherits(out3C, "try-error"))&&(grepl("rates.range should be a numeric vector of length 2, with rates>0", out3C[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out3C"
n.t=n.t+1
out3D<-try(compare.NIfrontier.survival(rate.control.expected=0.05, rate.experim.target=0.05, rates.range=c(-0.1,0.1), NI.margin=1.2, summary.measure="HR", tau.RMST=3, t.DS=3))
correct[[n.t]]<-ifelse((inherits(out3D, "try-error"))&&(grepl("rates.range should be a numeric vector of length 2, with rates>0", out3D[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out3D"
n.t=n.t+1

# Check that it stops for rate.range not including the expected rate:
out3E<-try(compare.NIfrontier.survival(rate.control.expected=0.05, rate.experim.target=0.05, rates.range=c(0.1,0.15), NI.margin=1.2, summary.measure="HR", tau.RMST=3, t.DS=3))
correct[[n.t]]<-ifelse((inherits(out3E, "try-error"))&&(grepl("Range of risks should include the expected value.", out3E[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out3E"
n.t=n.t+1

# Check behavior for range = single point. It should stop and say that it is useless to print frontiers for single point: 
out3F<-try(compare.NIfrontier.survival(rate.control.expected=0.05, rate.experim.target=0.05, rates.range=c(0.05,0.05), NI.margin=1.2, summary.measure="HR", tau.RMST=3, t.DS=3))
correct[[n.t]]<-ifelse((inherits(out3F, "try-error"))&&(grepl("If the range is a single point, then any summary measure will have same power if the margins are matched", out3F[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out3F"
n.t=n.t+1

#####################################################
# Fourth set of checks:
# CHeck that it stops for wrong values of t expected and risks

out4A<-try(compare.NIfrontier.survival(t.expected="3", p.control.expected=0.3, NI.margin=1.2, summary.measure="HR", tau.RMST=3, t.DS=3))
correct[[n.t]]<-ifelse((inherits(out4A, "try-error"))&&(grepl("t.expected should be a positive number.", out4A[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4A"
n.t=n.t+1
out4B<-try(compare.NIfrontier.survival(t.expected=3, p.control.expected="0.3", NI.margin=1.2, summary.measure="HR", tau.RMST=3, t.DS=3))
correct[[n.t]]<-ifelse((inherits(out4B, "try-error"))&&(grepl("p.control.expected should be a numeric >0 and <1.", out4B[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4B"
n.t=n.t+1
out4C<-try(compare.NIfrontier.survival(t.expected=3, p.control.expected=0.3, p.experim.target="0.3", NI.margin=1.2, summary.measure="HR", tau.RMST=3, t.DS=3))
correct[[n.t]]<-ifelse((inherits(out4C, "try-error"))&&(grepl("p.experim.target should be a numeric >0 and <1.", out4C[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4C"
n.t=n.t+1
out4D<-try(compare.NIfrontier.survival(t.expected=0, p.control.expected=0.3, NI.margin=1.2, summary.measure="HR", tau.RMST=3, t.DS=3))
correct[[n.t]]<-ifelse((inherits(out4D, "try-error"))&&(grepl("t.expected should be a positive number", out4D[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4D"
n.t=n.t+1
out4E<-try(compare.NIfrontier.survival(t.expected=3, p.control.expected=-0.1, NI.margin=1.2, summary.measure="HR", tau.RMST=3, t.DS=3))
correct[[n.t]]<-ifelse((inherits(out4E, "try-error"))&&(grepl("p.control.expected should be a numeric >0", out4E[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4E"
n.t=n.t+1
out4F<-try(compare.NIfrontier.survival(t.expected=3, p.control.expected=0.3, p.experim.target=-0.3, NI.margin=1.2, summary.measure="HR", tau.RMST=3, t.DS=3))
correct[[n.t]]<-ifelse((inherits(out4F, "try-error"))&&(grepl("p.experim.target should be a numeric >0 and <1.", out4F[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4F"
n.t=n.t+1
out4G<-try(compare.NIfrontier.survival(t.expected=3, p.control.expected=1.1, NI.margin=1.2, summary.measure="HR", tau.RMST=3, t.DS=3))
correct[[n.t]]<-ifelse((inherits(out4G, "try-error"))&&(grepl("p.control.expected should be a numeric >0 and <1.", out4G[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4G"
n.t=n.t+1
out4H<-try(compare.NIfrontier.survival(t.expected=3, p.control.expected=0.3, p.experim.target=1.3, NI.margin=1.2, summary.measure="HR", tau.RMST=3, t.DS=3))
correct[[n.t]]<-ifelse((inherits(out4H, "try-error"))&&(grepl("p.experim.target should be a numeric >0 and <1.", out4H[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4H"
n.t=n.t+1

#####################################################
# FIFth set of checks:
# Check behavior for alternative that is inferior or NI margin that mean superiority

out5A<-try(compare.NIfrontier.survival(t.expected=3, p.control.expected=0.3, p.experim.target=0.4, NI.margin=0.09, summary.measure="DS", tau.RMST=3, t.DS=3))
correct[[n.t]]<-ifelse((inherits(out5A, "try-error"))&&(grepl("The alternative hypothesis does not imply non-inferiority of the experimental treatment", out5A[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out5A"
n.t=n.t+1
out5B<-try(compare.NIfrontier.survival(rate.control.expected = 0.3, rate.experim.target=0.6, NI.margin=1.5, summary.measure="HR", tau.RMST=3, t.DS=3))
correct[[n.t]]<-ifelse((inherits(out5B, "try-error"))&&(grepl("The alternative hypothesis does not imply non-inferiority of the experimental treatment", out5B[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out5B"
n.t=n.t+1

# Check behavior for NI margin = 0 (DRMST or DS) or 1 (HR) (superiority): should stop and say any frontier is equally powerful
out5C<-try(compare.NIfrontier.survival(rate.control.expected = 0.3, rate.experim.target=0.3, NI.margin=1, summary.measure="HR", tau.RMST=3, t.DS=3))
correct[[n.t]]<-ifelse((inherits(out5C, "try-error"))&&(grepl("A Non-inferiority margin of 1 for the hazard ratio means this is a superiority trial.", out5C[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out5C"
n.t=n.t+1
out5D<-try(compare.NIfrontier.survival(rate.control.expected = 0.3, rate.experim.target=0.3, NI.margin=0, summary.measure="DS", tau.RMST=3, t.DS=3))
correct[[n.t]]<-ifelse((inherits(out5D, "try-error"))&&(grepl("A Non-inferiority margin of 0 for a difference (DS or DRMST) means this is a superiority trial.", out5D[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out5D"
n.t=n.t+1
out5E<-try(compare.NIfrontier.survival(rate.control.expected = 0.3, rate.experim.target=0.3, NI.margin=0, summary.measure="DRMST", tau.RMST=3, t.DS=3))
correct[[n.t]]<-ifelse((inherits(out5E, "try-error"))&&(grepl("A Non-inferiority margin of 0 for a difference (DS or DRMST) means this is a superiority trial.", out5E[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out5E"
n.t=n.t+1

#####################################################
# Sixth set of checks:
# Check behavior for other wrong inputs

# Check if it stops as expected for summary measure different from "HR", "DRMST" or "DS
out6A<-try(compare.NIfrontier.survival(rate.control.expected = 0.3, rate.experim.target=0.3, NI.margin=1.2, summary.measure="pippo", tau.RMST=3, t.DS=3))
correct[[n.t]]<-ifelse((inherits(out6A, "try-error"))&&(grepl("summary.measure should be one of either DS (difference in survival at specific time point), HR (hazard ratio) or DRMST (difference in restricted mean survival time).", out6A[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out6A"
n.t=n.t+1

# Check what happens for wrong values of tau and t
out6B<-try(compare.NIfrontier.survival(rate.control.expected = 0.3, rate.experim.target=0.3, NI.margin=1.2, summary.measure="HR", tau.RMST="3", t.DS=3))
correct[[n.t]]<-ifelse((inherits(out6B, "try-error"))&&(grepl("Horizon time should be defined (as a numeric) for both RMST and DS.", out6B[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out6B"
n.t=n.t+1
out6C<-try(compare.NIfrontier.survival(rate.control.expected = 0.3, rate.experim.target=0.3, NI.margin=1.2, summary.measure="HR", tau.RMST=3, t.DS="3"))
correct[[n.t]]<-ifelse((inherits(out6C, "try-error"))&&(grepl("Horizon time should be defined (as a numeric) for both RMST and DS.", out6C[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out6C"
n.t=n.t+1
out6D<-try(compare.NIfrontier.survival(rate.control.expected = 0.3, rate.experim.target=0.3, NI.margin=1.2, summary.measure="HR", tau.RMST=3, t.DS=0))
correct[[n.t]]<-ifelse((inherits(out6D, "try-error"))&&(grepl("Horizon time should be positive.", out6D[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out6D"
n.t=n.t+1
out6E<-try(compare.NIfrontier.survival(rate.control.expected = 0.3, rate.experim.target=0.3, NI.margin=1.2, summary.measure="HR", t.DS=3, tau.RMST=0))
correct[[n.t]]<-ifelse((inherits(out6E, "try-error"))&&(grepl("Horizon time should be positive.", out6E[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out6E"
n.t=n.t+1

# Check what happens if we pass both risks and rates
out6F<-try(compare.NIfrontier.survival(rate.control.expected = 0.3, rate.experim.target=0.3, p.control.expected=0.3, t.expected=3, NI.margin=1.2, summary.measure="HR", tau.RMST=3, t.DS=3))
correct[[n.t]]<-ifelse((inherits(out6F, "try-error"))&&(grepl("Please only specify either the control event rate or the expected control risk at time t.expected.", out6F[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out6F"
n.t=n.t+1

#####################################################
# Seventh set of checks:
# Check results for few combinations of parameters. (all should work and return a ranking of the summary measures):

out7A<-try(compare.NIfrontier.survival(rate.control.expected = 0.3, rate.experim.target=0.3, NI.margin=1.2, summary.measure="HR", t.DS=3, tau.RMST=3))
correct[[n.t]]<-ifelse((inherits(out7A,"data.frame"))&&(isTRUE(all.equal(as.numeric(out7A[20,]),c(0.6000000, 0.7200000, 0.7731598, 0.7048443, 0.5161395, 0.5602501, 0.5038838), tolerance=10^(-6)))),1,0)
names(correct)[[n.t]]<-"out7A"
n.t=n.t+1
out7B<-try(compare.NIfrontier.survival(rate.control.expected = 0.3, rate.experim.target=0.35, NI.margin=1.2, summary.measure="HR", t.DS=3, tau.RMST=3))
correct[[n.t]]<-ifelse((inherits(out7B,"data.frame"))&&(isTRUE(all.equal(as.numeric(out7B[20,]),c(0.6000000, 0.7200000, 0.7731598, 0.7048443, 0.5161395, 0.5602501, 0.5038838), tolerance=10^(-6)))),1,0)
names(correct)[[n.t]]<-"out7B"
n.t=n.t+1
out7C<-try(compare.NIfrontier.survival(rate.control.expected = 0.3, rate.experim.target=0.25, NI.margin=1.2, summary.measure="HR", t.DS=3, tau.RMST=3))
correct[[n.t]]<-ifelse((inherits(out7C,"data.frame"))&&(isTRUE(all.equal(as.numeric(out7C[20,]),c(0.6000000, 0.7200000, 0.7731598, 0.7048443, 0.5161395, 0.5602501, 0.5038838), tolerance=10^(-6)))),1,0)
names(correct)[[n.t]]<-"out7C"
n.t=n.t+1
out7D<-try(compare.NIfrontier.survival(rate.control.expected = 0.03, rate.experim.target=0.03, NI.margin=2, summary.measure="HR", t.DS=3, tau.RMST=3))
correct[[n.t]]<-ifelse((inherits(out7D,"data.frame"))&&(isTRUE(all.equal(as.numeric(out7D[20,]),c(0.3300000, 0.6600000, 0.4092901, 0.3841800, 0.6977822, 0.4835918, 0.4641589), tolerance=10^(-6)))),1,0)
names(correct)[[n.t]]<-"out7D"
n.t=n.t+1
out7E<-try(compare.NIfrontier.survival(p.control.expected = 0.8, t.expected=3, NI.margin=-0.16, summary.measure="DS", t.DS=3, tau.RMST=3))
correct[[n.t]]<-ifelse((inherits(out7E,"data.frame"))&&(isTRUE(all.equal(as.numeric(out7E[20,]),c(0.8364793, 1.6729585 ,       NA, 1.9877181, 1.1754084 ,       NA ,1.4819225), tolerance=10^(-6)))),1,0)
names(correct)[[n.t]]<-"out7E"
n.t=n.t+1
out7F<-try(compare.NIfrontier.survival(p.control.expected = 0.8, t.expected=3, NI.margin=-0.16, summary.measure="DS", t.DS=0.3, tau.RMST=0.3))
correct[[n.t]]<-ifelse((inherits(out7F,"data.frame"))&&(isTRUE(all.equal(as.numeric(out7F[20,]),c( 0.8364793, 1.9184607, 1.6038662, 1.5752901, 1.4141686, 1.1087447, 1.0812622), tolerance=10^(-6)))),1,0)
names(correct)[[n.t]]<-"out7F"
n.t=n.t+1
out7G<-try(compare.NIfrontier.survival(p.control.expected = 0.4, t.expected=3, NI.margin=-0.132, summary.measure="DRMST", t.DS=3, tau.RMST=3))
correct[[n.t]]<-ifelse((inherits(out7G,"data.frame"))&&(isTRUE(all.equal(as.numeric(out7G[20,]),c( 0.4702752, 0.5879678, 0.5868741, 0.5458915, 0.5142636 ,0.5133757, 0.4807157), tolerance=10^(-6)))),1,0)
names(correct)[[n.t]]<-"out7G"
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

c.NIf.s<-(tot.correct==number.of.tests) 


