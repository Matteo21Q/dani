#################################################
### compare.NIfrontier.binary testing file  #####
### 20-07-2023                              #####
#################################################

# Load dani:
# library(dani)

#Initialise vector of outputs 
correct<-list(NULL)
n.t<-1

#####################################################
# First set of checks:
# Check that it stops for non acceptable values of probs and margins

out1A<-try(compare.NIfrontier.binary(p.control.expected=-0.2,p.experim.target=0.2,p.range=c(0.05,0.25),NI.margin=0.05, summary.measure="RD"))
correct[[n.t]]<-ifelse((inherits(out1A, "try-error"))&&(grepl("p.control.expected and p.experim.target should range between 0 and 1 (excluded)", out1A[1], fixed=T  )),1,0) 
names(correct)[[n.t]]<-"out1A"
n.t=n.t+1
out1B<-try(compare.NIfrontier.binary(p.control.expected=1.2,p.experim.target=0.2,p.range=c(0.05,0.25),NI.margin=0.05, summary.measure="RD"))
correct[[n.t]]<-ifelse((inherits(out1B, "try-error"))&&(grepl("p.control.expected and p.experim.target should range between 0 and 1 (excluded)", out1B[1], fixed=T  )),1,0) 
names(correct)[[n.t]]<-"out1B"
n.t=n.t+1
out1C<-try(compare.NIfrontier.binary(p.control.expected=T,p.experim.target=0.2,p.range=c(0.05,0.25),NI.margin=0.05, summary.measure="RD"))
correct[[n.t]]<-ifelse((inherits(out1C, "try-error"))&&(grepl("p.control.expected, p.experim.target and NI.margin should all be numeric", out1C[1], fixed=T )),1,0) 
names(correct)[[n.t]]<-"out1C"
n.t=n.t+1
out1D<-try(compare.NIfrontier.binary(p.control.expected=0.2,p.experim.target=-0.2,p.range=c(0.05,0.25),NI.margin=0.05, summary.measure="RD"))
correct[[n.t]]<-ifelse((inherits(out1D, "try-error"))&&(grepl("p.control.expected and p.experim.target should range between 0 and 1 (excluded)", out1D[1], fixed=T  )),1,0) 
names(correct)[[n.t]]<-"out1D"
n.t=n.t+1
out1E<-try(compare.NIfrontier.binary(p.control.expected=0.2,p.experim.target=1.2,p.range=c(0.05,0.25),NI.margin=0.05, summary.measure="RD"))
correct[[n.t]]<-ifelse((inherits(out1E, "try-error"))&&(grepl("p.control.expected and p.experim.target should range between 0 and 1 (excluded)", out1E[1], fixed=T  )),1,0) 
names(correct)[[n.t]]<-"out1E"
n.t=n.t+1
out1F<-try(compare.NIfrontier.binary(p.control.expected=0.2,p.experim.target=F,p.range=c(0.05,0.25),NI.margin=0.05, summary.measure="RD"))
correct[[n.t]]<-ifelse((inherits(out1F, "try-error"))&&(grepl("p.control.expected, p.experim.target and NI.margin should all be numeric", out1F[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out1F"
n.t=n.t+1
out1G<-try(compare.NIfrontier.binary(p.control.expected=0.2,p.experim.target=0.2,p.range=c(0.05,0.25),NI.margin="0", summary.measure="RD"))
correct[[n.t]]<-ifelse((inherits(out1G, "try-error"))&&(grepl("p.control.expected, p.experim.target and NI.margin should all be numeric", out1G[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out1G"
n.t=n.t+1

#####################################################
# Second set of checks:
# Check that it stops for negative ratio margins (both OR and RR):

out2A<-try(compare.NIfrontier.binary(p.control.expected=0.2,p.experim.target=0.2,p.range=c(0.05,0.25),NI.margin=-2, summary.measure="RR"))
correct[[n.t]]<-ifelse((inherits(out2A, "try-error"))&&(grepl("Ratio (RR or OR) NI margins cannot be negative.", out2A[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out2A"
n.t=n.t+1
out2B<-try(compare.NIfrontier.binary(p.control.expected=0.2,p.experim.target=0.2,p.range=c(0.05,0.25),NI.margin=-0.05, summary.measure="OR"))
correct[[n.t]]<-ifelse((inherits(out2B, "try-error"))&&(grepl("Ratio (RR or OR) NI margins cannot be negative.", out2B[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out2B"
n.t=n.t+1

#####################################################
# Third set of checks:
# Check that it stops for unacceptable p.range values:

# First, range does not include p.expected
out3A<-try(compare.NIfrontier.binary(p.control.expected=0.1,p.experim.target=0.1,p.range=c(0.2,0.4),NI.margin=0.1, summary.measure="RD"))
correct[[n.t]]<-ifelse((inherits(out3A, "try-error"))&&(grepl("Range of risks should include the expected value.", out3A[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out3A"
n.t=n.t+1

# Check behavior for range = single point. It should stop and say that it is useless to print frontiers for single point: 
out3B<-try(compare.NIfrontier.binary(p.control.expected=0.1,p.experim.target=0.1,p.range=c(0.1,0.1),NI.margin=0.1, summary.measure="RD"))
correct[[n.t]]<-ifelse((inherits(out3B, "try-error"))&&(grepl("If the range is a single point, then any summary measure will have same power if the margins are matched.", out3B[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out3B"
n.t=n.t+1
# Test behavior for wrong p.range format. Should stop and warn: 
out3C<-try(compare.NIfrontier.binary(p.control.expected=0.1,p.experim.target=0.1,p.range=c("1","19"),NI.margin=0.1, summary.measure="RD"))
correct[[n.t]]<-ifelse((inherits(out3C, "try-error"))&&(grepl("p.range should be a numeric vector of length 2, with values in the (0,1) range.", out3C[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out3C"
n.t=n.t+1
out3D<-try(compare.NIfrontier.binary(p.control.expected=0.1,p.experim.target=0.1,p.range=c(0.1,0.2,0.3),NI.margin=0.1, summary.measure="RD"))
correct[[n.t]]<-ifelse((inherits(out3D, "try-error"))&&(grepl("p.range should be a numeric vector of length 2, with values in the (0,1) range.", out3D[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out3D"
n.t=n.t+1
out3E<-try(compare.NIfrontier.binary(p.control.expected=0.1,p.experim.target=0.1,p.range=c(0.1),NI.margin=0.1, summary.measure="RD"))
correct[[n.t]]<-ifelse((inherits(out3E, "try-error"))&&(grepl("p.range should be a numeric vector of length 2, with values in the (0,1) range.", out3E[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out3E"
n.t=n.t+1
out3F<-try(compare.NIfrontier.binary(p.control.expected=0.1,p.experim.target=0.1,p.range=c(-0.1,0.6),NI.margin=0.1, summary.measure="RD"))
correct[[n.t]]<-ifelse((inherits(out3F, "try-error"))&&(grepl("p.range should be a numeric vector of length 2, with values in the (0,1) range.", out3F[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out3F"
n.t=n.t+1
out3G<-try(compare.NIfrontier.binary(p.control.expected=0.1,p.experim.target=0.1,p.range=c(1.2,0.6),NI.margin=0.1, summary.measure="RD"))
correct[[n.t]]<-ifelse((inherits(out3G, "try-error"))&&(grepl("p.range should be a numeric vector of length 2, with values in the (0,1) range.", out3G[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out3G"
n.t=n.t+1
out3H<-try(compare.NIfrontier.binary(p.control.expected=0.1,p.experim.target=0.1,p.range=c(0.6,1.6),NI.margin=0.1, summary.measure="RD"))
correct[[n.t]]<-ifelse((inherits(out3C, "try-error"))&&(grepl("p.range should be a numeric vector of length 2, with values in the (0,1) range.", out3H[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out3H"
n.t=n.t+1

#####################################################
# Fourth set of checks:
# Check behavior for alternative that is inferior or NI margin that mean superiority

# First alternative inferior
out4A<-try(compare.NIfrontier.binary(p.control.expected=0.1,p.experim.target=0.4,p.range=c(0.1,0.9),NI.margin=0.1, summary.measure="RD"))
correct[[n.t]]<-ifelse((inherits(out4A, "try-error"))&&(grepl("The alternative hypothesis does not imply non-inferiority of the experimental treatment", out4A[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4A"
n.t=n.t+1
out4B<-try(compare.NIfrontier.binary(p.control.expected=0.4,p.experim.target=0.1,p.range=c(0.1,0.9),NI.margin=-0.1, summary.measure="RD"))
correct[[n.t]]<-ifelse((inherits(out4B, "try-error"))&&(grepl("The alternative hypothesis does not imply non-inferiority of the experimental treatment", out4B[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4B"
n.t=n.t+1
out4C<-try(compare.NIfrontier.binary(p.control.expected=0.1,p.experim.target=0.3,p.range=c(0.1,0.9),NI.margin=2, summary.measure="RR"))
correct[[n.t]]<-ifelse((inherits(out4C, "try-error"))&&(grepl("The alternative hypothesis does not imply non-inferiority of the experimental treatment", out4C[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4C"
n.t=n.t+1

# Check behavior for NI margin = 0 (superiority): should stop and say any frontier is equally powerful
out4D<-try(compare.NIfrontier.binary(p.control.expected=0.1,p.experim.target=0.1,p.range=c(0.01,0.2),NI.margin=0, summary.measure="RD"))
correct[[n.t]]<-ifelse((inherits(out4D, "try-error"))&&(grepl("A Non-inferiority margin of 0 for the risk difference means this is a superiority trial.", out4D[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4D"
n.t=n.t+1
out4E<-try(compare.NIfrontier.binary(p.control.expected=0.1,p.experim.target=0.1,p.range=c(0.01,0.2),NI.margin=1, summary.measure="RR"))
correct[[n.t]]<-ifelse((inherits(out4E, "try-error"))&&(grepl("A Non-inferiority margin of 1 for a ratio means this is a superiority trial.", out4E[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4E"
n.t=n.t+1
out4F<-try(compare.NIfrontier.binary(p.control.expected=0.1,p.experim.target=0.1,p.range=c(0.01,0.2),NI.margin=1, summary.measure="OR"))
correct[[n.t]]<-ifelse((inherits(out4F, "try-error"))&&(grepl("A Non-inferiority margin of 1 for a ratio means this is a superiority trial.", out4F[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4F"
n.t=n.t+1

#####################################################
# Fifth set of checks:
# Check if it stops as expected for summary measure different from "RD", "OR" or "RR"

out5A<-try(compare.NIfrontier.binary(p.control.expected=0.1,p.experim.target=0.1,p.range=c(0.1,0.9),NI.margin=2, summary.measure="pippo"))
correct[[n.t]]<-ifelse((inherits(out5A, "try-error"))&&(grepl("summary.measure should be one of either RD (risk difference), RR (risk ratio) or OR (odds ratio).", out5A[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out5A"
n.t=n.t+1

#####################################################
# Sixth set of checks:
# Check results for few p different from 0. (all should work and return difference as the best frontier, except the last one which should return ratio):

out6A<-try(compare.NIfrontier.binary(p.control.expected=0.1,p.experim.target=0.1,p.range=c(0.01,0.3),NI.margin=0.05, summary.measure="RD"))
correct[[n.t]]<-ifelse((inherits(out6A,"data.frame"))&&(isTRUE(all.equal(as.numeric(out6A[100,]),c(0.3000000, 0.3500000, 0.4500000, 0.4050000, 0.3201562, 0.4031129, 0.3647259), tolerance=10^(-6)))),1,0) 
names(correct)[[n.t]]<-"out6A"
n.t=n.t+1
out6B<-try(compare.NIfrontier.binary(p.control.expected=0.8,p.experim.target=0.8,p.range=c(0.01,0.99),NI.margin=0.05, summary.measure="RD"))
correct[[n.t]]<-ifelse((inherits(out6B,"data.frame"))&&(isTRUE(all.equal(as.numeric(out6B[100,]),c(0.99, 1.04, 1.051875,0.9929204,0.3061046, 0.3155012, 0.2707735), tolerance=10^(-6)))),1,0)
names(correct)[[n.t]]<-"out6B"
n.t=n.t+1
out6C<-try(compare.NIfrontier.binary(p.control.expected=0.1,p.experim.target=0.1,p.range=c(0.01,0.99),NI.margin=-0.05, summary.measure="RD"))
correct[[n.t]]<-ifelse((inherits(out6C,"data.frame"))&&(isTRUE(all.equal(as.numeric(out6C[100,]),c(0.99, 0.94, 0.495, 0.9791209,1.223806 ,0.9737171, 1.250981), tolerance=10^(-6)))),1,0)
names(correct)[[n.t]]<-"out6C"
n.t=n.t+1
out6D<-try(compare.NIfrontier.binary(p.control.expected=0.2,p.experim.target=0.1,p.range=c(0.01,0.99),NI.margin=0.05, summary.measure="RD"))
correct[[n.t]]<-ifelse((inherits(out6D,"data.frame"))&&(isTRUE(all.equal(as.numeric(out6D[100,]),c(0.99,  1.04,1.2375,0.9924812, 1.153126, 1.304035, 1.118985), tolerance=10^(-6)))),1,0)
names(correct)[[n.t]]<-"out6D"
n.t=n.t+1

# Check results for same scenarios as above but defining margins as RR:
out6E<-try(compare.NIfrontier.binary(p.control.expected=0.1,p.experim.target=0.1,p.range=c(0.01,0.3),NI.margin=1.5, summary.measure="RR"))
correct[[n.t]]<-ifelse((inherits(out6E,"data.frame"))&&all.equal(out6A, out6E, tolerance=10^(-6)),1,0) 
names(correct)[[n.t]]<-"out6E"
n.t=n.t+1
out6F<-try(compare.NIfrontier.binary(p.control.expected=0.8,p.experim.target=0.8,p.range=c(0.01,0.99),NI.margin=0.85/0.8, summary.measure="RR"))
correct[[n.t]]<-ifelse((inherits(out6F,"data.frame"))&&all.equal(out6B, out6F, tolerance=10^(-6)),1,0) 
names(correct)[[n.t]]<-"out6F"
n.t=n.t+1
out6G<-try(compare.NIfrontier.binary(p.control.expected=0.1,p.experim.target=0.1,p.range=c(0.01,0.99),NI.margin=0.5, summary.measure="RR"))
correct[[n.t]]<-ifelse((inherits(out6G,"data.frame"))&&all.equal(out6C, out6G, tolerance=10^(-6)),1,0) 
names(correct)[[n.t]]<-"out6G"
n.t=n.t+1
out6H<-try(compare.NIfrontier.binary(p.control.expected=0.2,p.experim.target=0.1,p.range=c(0.01,0.99),NI.margin=1.25, summary.measure="RR"))
correct[[n.t]]<-ifelse((inherits(out6H,"data.frame"))&&all.equal(out6D, out6H, tolerance=10^(-6)),1,0) 
names(correct)[[n.t]]<-"out6H"
n.t=n.t+1

# Check results for same scenarios as above but defining margins as OR:
out6I<-try(compare.NIfrontier.binary(p.control.expected=0.1,p.experim.target=0.1,p.range=c(0.01,0.3),NI.margin=(0.9 * 1.5) / (1 - 1.5 * 0.1), summary.measure="OR"))
correct[[n.t]]<-ifelse((inherits(out6I,"data.frame"))&&all.equal(out6A, out6I, tolerance=10^(-6)),1,0) 
names(correct)[[n.t]]<-"out6I"
n.t=n.t+1
out6J<-try(compare.NIfrontier.binary(p.control.expected=0.8,p.experim.target=0.8,p.range=c(0.01,0.99),NI.margin=(0.2 * 0.85/0.8) / (1 - 0.85/0.8 * 0.8), summary.measure="OR"))
correct[[n.t]]<-ifelse((inherits(out6J,"data.frame"))&&all.equal(out6B, out6J, tolerance=10^(-6)),1,0) 
names(correct)[[n.t]]<-"out6J"
n.t=n.t+1
out6K<-try(compare.NIfrontier.binary(p.control.expected=0.1,p.experim.target=0.1,p.range=c(0.01,0.99),NI.margin=(0.9 * 0.5) / (1 - 0.5 * 0.1), summary.measure="OR"))
correct[[n.t]]<-ifelse((inherits(out6K,"data.frame"))&&all.equal(out6C, out6K, tolerance=10^(-6)),1,0) 
names(correct)[[n.t]]<-"out6K"
n.t=n.t+1
out6L<-try(compare.NIfrontier.binary(p.control.expected=0.2,p.experim.target=0.1,p.range=c(0.01,0.99),NI.margin=(0.8 * 1.25) / (1 - 1.25 * 0.2), summary.measure="OR"))
correct[[n.t]]<-ifelse((inherits(out6L,"data.frame"))&&all.equal(out6D, out6L, tolerance=10^(-6)),1,0) 
names(correct)[[n.t]]<-"out6L"
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

c.NIf.b<-(tot.correct==number.of.tests) 


