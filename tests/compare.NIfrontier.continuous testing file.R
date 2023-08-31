####################################################
###compare.NIfrontier.continuous testing file  #####
### 20-07-2023                                 #####
####################################################

# Load dani:
# library(dani)


#Initialise vector of outputs 
correct<-list(NULL)
n.t<-1

#####################################################
# First set of checks:
# Check that it stops for non-acceptable mean values:

# Check that it stops for mean of zero in control arm:
out1A<-try(compare.NIfrontier.continuous(mean.control.expected=0,mean.experim.target=1,mean.range=c(-2,4),NI.margin=2, summary.measure="difference"))
correct[[n.t]]<-ifelse((inherits(out1A, "try-error"))&&(grepl("The ratio of means is not an appropriate summary measure when the expected control mean is 0.", out1A[1], fixed=T  )),1,0) 
names(correct)[[n.t]]<-"out1A"
n.t=n.t+1

# test behavior for non-numeric arguments. Should stop and warn:
out1B<-try(compare.NIfrontier.continuous(mean.control.expected="a",mean.experim.target=11,mean.range=c(1,19),NI.margin=2, summary.measure="difference"))
correct[[n.t]]<-ifelse((inherits(out1B, "try-error"))&&(grepl("mean.control.expected, mean.experim.target and NI.margin should all be numeric.", out1B[1], fixed=T  )),1,0) 
names(correct)[[n.t]]<-"out1B"
n.t=n.t+1
out1C<-try(compare.NIfrontier.continuous(mean.control.expected=10,mean.experim.target="a",mean.range=c(1,19),NI.margin=2, summary.measure="difference"))
correct[[n.t]]<-ifelse((inherits(out1C, "try-error"))&&(grepl("mean.control.expected, mean.experim.target and NI.margin should all be numeric.", out1C[1], fixed=T )),1,0) 
names(correct)[[n.t]]<-"out1C"
n.t=n.t+1
out1D<-try(compare.NIfrontier.continuous(mean.control.expected=10,mean.experim.target=11,mean.range=c(1,19),NI.margin="a", summary.measure="difference"))
correct[[n.t]]<-ifelse((inherits(out1D, "try-error"))&&(grepl("mean.control.expected, mean.experim.target and NI.margin should all be numeric.", out1D[1], fixed=T )),1,0) 
names(correct)[[n.t]]<-"out1D"
n.t=n.t+1

#####################################################
# First set of checks:
# Check wrong mean ranges.

# Check that it stops for mean.range not including the expected mean:
out2A<-try(compare.NIfrontier.continuous(mean.control.expected=10,mean.experim.target=11,mean.range=c(-2,4),NI.margin=2, summary.measure="difference"))
correct[[n.t]]<-ifelse((inherits(out2A, "try-error"))&&(grepl("Range of means should include the expected value.", out2A[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out2A"
n.t=n.t+1

# Check behavior for range = single point. It should stop and say that it is useless to print frontiers for single point: 
out2B<-try(compare.NIfrontier.continuous(mean.control.expected=10,mean.experim.target=10,mean.range=c(10,10),NI.margin=2, summary.measure="difference"))
correct[[n.t]]<-ifelse((inherits(out2B, "try-error"))&&(grepl("If the range is a single point, then any summary measure will have same power if the margins are matched.", out2B[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out2B"
n.t=n.t+1

# Test behavior for wrong mean.range format. Should stop and warn: 
out2C<-try(compare.NIfrontier.continuous(mean.control.expected=10,mean.experim.target=11,mean.range=c("1","19"),NI.margin=2, summary.measure="difference"))
correct[[n.t]]<-ifelse((inherits(out2C, "try-error"))&&(grepl("mean.range should be a numeric vector of length 2.", out2C[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out2C"
n.t=n.t+1
out2D<-try(compare.NIfrontier.continuous(mean.control.expected=10,mean.experim.target=11,mean.range=c(1,2,3),NI.margin=2, summary.measure="difference"))
correct[[n.t]]<-ifelse((inherits(out2D, "try-error"))&&(grepl("mean.range should be a numeric vector of length 2.", out2D[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out2D"
n.t=n.t+1
out2E<-try(compare.NIfrontier.continuous(mean.control.expected=10,mean.experim.target=11,mean.range=c(1),NI.margin=2, summary.measure="difference"))
correct[[n.t]]<-ifelse((inherits(out2E, "try-error"))&&(grepl("mean.range should be a numeric vector of length 2.", out2E[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out2E"
n.t=n.t+1

#####################################################
# Third set of checks:
# Check behavior for alternative that is inferior or NI margin that mean superiority

# Check behavior for alternative that is inferior. (It should stop)
out3A<-try(compare.NIfrontier.continuous(mean.control.expected=10,mean.experim.target=14,mean.range=c(1,19),NI.margin=2, summary.measure="difference"))
correct[[n.t]]<-ifelse((inherits(out3A, "try-error"))&&(grepl("The alternative hypothesis does not imply non-inferiority of the experimental treatment", out3A[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out3A"
n.t=n.t+1
out3B<-try(compare.NIfrontier.continuous(mean.control.expected=-20,mean.experim.target=-25,mean.range=c(-1,-39),NI.margin=-2, summary.measure="difference"))
correct[[n.t]]<-ifelse((inherits(out3B, "try-error"))&&(grepl("The alternative hypothesis does not imply non-inferiority of the experimental treatment", out3B[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out3B"
n.t=n.t+1
out3C<-try(compare.NIfrontier.continuous(mean.control.expected=10,mean.experim.target=5,mean.range=c(1,10),NI.margin=-2, summary.measure="difference"))
correct[[n.t]]<-ifelse((inherits(out3C, "try-error"))&&(grepl("The alternative hypothesis does not imply non-inferiority of the experimental treatment", out3C[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out3C"
n.t=n.t+1

# Check behavior for NI margin = 0 (superiority): should stop and say any frontier is equally powerful
out3D<-try(compare.NIfrontier.continuous(mean.control.expected=10,mean.experim.target=10,mean.range=c(1,19),NI.margin=0, summary.measure="difference"))
correct[[n.t]]<-ifelse((inherits(out3D, "try-error"))&&(grepl("A Non-inferiority margin of 0 for the mean difference means this is a superiority trial.", out3D[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out3D"
n.t=n.t+1

#####################################################
# Fourth set of checks:
# Check behavior for other wrong inputs

# Check if it stops as expected for summary measure different from "difference" or "ratio"
out4A<-try(compare.NIfrontier.continuous(mean.control.expected=10,mean.experim.target=11,mean.range=c(1,19),NI.margin=2, summary.measure="pippo"))
correct[[n.t]]<-ifelse((inherits(out4A, "try-error"))&&(grepl("summary.measure should be one of either difference or ratio.", out4A[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4A"
n.t=n.t+1

# Check behavior for NI margin = 1 (superiority): should stop and say any frontier is equally powerful
out4B<-try(compare.NIfrontier.continuous(mean.control.expected=10,mean.experim.target=10,mean.range=c(1,19),NI.margin=1, summary.measure="ratio"))
correct[[n.t]]<-ifelse((inherits(out4B, "try-error"))&&(grepl("A Non-inferiority margin of 1 for the mean ratio means this is a superiority trial.", out4B[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4B"
n.t=n.t+1

#####################################################
# Fifth set of checks:
# Check results for few combinations of parameters. (all should work and return a ranking of the summary measures):

# Check results for few means different from 0. (all should work and return difference as the best frontier, except the last one which should return ratio):
out5A<-try(compare.NIfrontier.continuous(mean.control.expected=10,mean.experim.target=11,mean.range=c(1,19),NI.margin=2, summary.measure="difference"))
correct[[n.t]]<-ifelse((class(out5A)=="data.frame")&&(isTRUE(all.equal(as.numeric(out5A[100,]),c( 19.00000, 21.00000, 22.80000, 14.21267, 15.64736), tolerance=10^(-6)))),1,0)
names(correct)[[n.t]]<-"out5A"
n.t=n.t+1
out5B<-try(compare.NIfrontier.continuous(mean.control.expected=100,mean.experim.target=100,mean.range=c(1,199),NI.margin=2, summary.measure="difference"))
correct[[n.t]]<-ifelse((class(out5B)=="data.frame")&&(isTRUE(all.equal(as.numeric(out5B[100,]),c(199.0000, 201.0000, 202.9800, 141.4284, 142.8492), tolerance=10^(-6)))),1,0)
names(correct)[[n.t]]<-"out5B"
n.t=n.t+1
out5C<-try(compare.NIfrontier.continuous(mean.control.expected=100,mean.experim.target=100,mean.range=c(1,199),NI.margin=20, summary.measure="difference"))
correct[[n.t]]<-ifelse((class(out5C)=="data.frame")&&(isTRUE(all.equal(as.numeric(out5C[100,]),c(199.0000,219.0000, 238.8000, 154.7966, 170.4888), tolerance=10^(-6)))),1,0)
names(correct)[[n.t]]<-"out5C"
n.t=n.t+1
out5D<-try(compare.NIfrontier.continuous(mean.control.expected=-20,mean.experim.target=-21,mean.range=c(-1,-39),NI.margin=2, summary.measure="difference"))
correct[[n.t]]<-ifelse((class(out5D)=="data.frame")&&(isTRUE(all.equal(as.numeric(out5D[100,]),c(-1.00000,  1.00000, -0.90000, 28.31960, 26.94086), tolerance=10^(-6)))),1,0)
names(correct)[[n.t]]<-"out5D"
n.t=n.t+1
out5E<-try(compare.NIfrontier.continuous(mean.control.expected=-20,mean.experim.target=-21,mean.range=c(-1,-39),NI.margin=-2, summary.measure="difference"))
correct[[n.t]]<-ifelse((class(out5E)=="data.frame")&&(isTRUE(all.equal(as.numeric(out5E[100,]),c(-1.00000, -3.00000, -1.10000, 25.49510, 26.79944), tolerance=10^(-6)))),1,0)
names(correct)[[n.t]]<-"out5E"
n.t=n.t+1
out5F<-try(compare.NIfrontier.continuous(mean.control.expected=10,mean.experim.target=9,mean.range=c(1,20),NI.margin=-2, summary.measure="difference"))
correct[[n.t]]<-ifelse((class(out5F)=="data.frame")&&(isTRUE(all.equal(as.numeric(out5F[100,]),c( 20.00000, 18.00000, 16.00000, 12.80625, 11.66190), tolerance=10^(-6)))),1,0)
names(correct)[[n.t]]<-"out5F"
n.t=n.t+1

# Check results for same scenarios as above but defining margins as ratio:
out5G<-try(compare.NIfrontier.continuous(mean.control.expected=10,mean.experim.target=11,mean.range=c(1,19),NI.margin=1.2, summary.measure="ratio"))
correct[[n.t]]<-ifelse((class(out5G)=="data.frame")&&(isTRUE(all.equal(as.numeric(out5G[100,]),c(19.00000, 21.00000, 22.80000, 14.21267, 15.64736), tolerance=10^(-6)))),1,0) 
names(correct)[[n.t]]<-"out5G"
n.t=n.t+1
out5H<-try(compare.NIfrontier.continuous(mean.control.expected=100,mean.experim.target=100,mean.range=c(1,199),NI.margin=1.02, summary.measure="ratio"))
correct[[n.t]]<-ifelse((class(out5H)=="data.frame")&&(isTRUE(all.equal(as.numeric(out5H[100,]),c( 199.0000, 201.0000, 202.9800, 141.4284, 142.8492), tolerance=10^(-6)))),1,0) 
names(correct)[[n.t]]<-"out5H"
n.t=n.t+1
out5I<-try(compare.NIfrontier.continuous(mean.control.expected=100,mean.experim.target=100,mean.range=c(1,199),NI.margin=1.2, summary.measure="ratio"))
correct[[n.t]]<-ifelse((class(out5I)=="data.frame")&&(isTRUE(all.equal(as.numeric(out5I[100,]),c(199.0000, 219.0000, 238.8000, 154.7966, 170.4888), tolerance=10^(-6)))),1,0) 
names(correct)[[n.t]]<-"out5I"
n.t=n.t+1
out5J<-try(compare.NIfrontier.continuous(mean.control.expected=-20,mean.experim.target=-21,mean.range=c(-1,-39),NI.margin=0.9, summary.measure="ratio"))
correct[[n.t]]<-ifelse((class(out5J)=="data.frame")&&(isTRUE(all.equal(as.numeric(out5J[100,]),c(-1.00000,  1.00000, -0.90000 ,28.31960,26.94086), tolerance=10^(-6)))),1,0) 
names(correct)[[n.t]]<-"out5J"
n.t=n.t+1
out5K<-try(compare.NIfrontier.continuous(mean.control.expected=-20,mean.experim.target=-21,mean.range=c(-1,-39),NI.margin=1.1, summary.measure="ratio"))
correct[[n.t]]<-ifelse((class(out5K)=="data.frame")&&(isTRUE(all.equal(as.numeric(out5K[100,]),c(-1.00000, -3.00000, -1.10000, 25.49510, 26.79944), tolerance=10^(-6)))),1,0) 
names(correct)[[n.t]]<-"out5K"
n.t=n.t+1
out5L<-try(compare.NIfrontier.continuous(mean.control.expected=10,mean.experim.target=9,mean.range=c(1,20),NI.margin=0.8, summary.measure="ratio"))
correct[[n.t]]<-ifelse((class(out5L)=="data.frame")&&(isTRUE(all.equal(as.numeric(out5L[100,]),c(20.00000, 18.00000, 16.00000, 12.80625, 11.66190), tolerance=10^(-6)))),1,0) 
names(correct)[[n.t]]<-"out5L"
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

c.NIf.c<-(tot.correct==number.of.tests) 
