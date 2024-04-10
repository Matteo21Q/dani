###################################################
### power.NI.continuous testing file          #####
### 08-04-2024                                #####
###################################################

# Load dani:
# library(dani)
library(mratios)

###############################################################################################################################################
### Here we test the power function

### continuous data: ###########################################################

#Initialise vector of outputs 
correct<-list(NULL)
n.t<-1

#####################################################
# First set of checks:
# Check that it stops for non acceptable values of true means and sd:

out1A<-try(power.NI.continuous("20",20,40,-10, n.control=50, n.experim=50))
correct[[n.t]]<-ifelse((inherits(out1A, "try-error"))&&(grepl("is.numeric(mean.control) is not TRUE", out1A[1], fixed=T  )),1,0) 
names(correct)[[n.t]]<-"out1A"
n.t=n.t+1
out1B<-try(power.NI.continuous(20,"20",40,-10, n.control=50, n.experim=50))
correct[[n.t]]<-ifelse((inherits(out1B, "try-error"))&&(grepl("is.numeric(mean.experim) is not TRUE", out1B[1], fixed=T  )),1,0) 
names(correct)[[n.t]]<-"out1B"
n.t=n.t+1
out1C<-try(power.NI.continuous(20,20,"40",-10, n.control=50, n.experim=50))
correct[[n.t]]<-ifelse((inherits(out1C, "try-error"))&&(grepl("is.numeric(sd) is not TRUE", out1C[1], fixed=T )),1,0) 
names(correct)[[n.t]]<-"out1C"
n.t=n.t+1
out1D<-try(power.NI.continuous(20,20,-40,-10, n.control=50, n.experim=50))
correct[[n.t]]<-ifelse((inherits(out1D, "try-error"))&&(grepl("sd > 0 is not TRUE", out1D[1], fixed=T  )),1,0) 
names(correct)[[n.t]]<-"out1D"
n.t=n.t+1
out1E<-try(power.NI.continuous(0,0,40,-10, n.control=50, n.experim=50, summary.measure="mean.ratio"))
correct[[n.t]]<-ifelse((inherits(out1E, "try-error"))&&(grepl("The ratio of means is not an appropriate summary measure when the expected control mean is 0", out1E[1], fixed=T  )),1,0) 
names(correct)[[n.t]]<-"out1E"
n.t=n.t+1

#####################################################
# Second set of checks:
# Check that it stops for non acceptable values of NI margins:

out2A<-try(power.NI.continuous(20,20,40,"-10", n.control=50, n.experim=50))
correct[[n.t]]<-ifelse((inherits(out2A, "try-error"))&&(grepl("is.numeric(NI.margin) is not TRUE", out2A[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out2A"
n.t=n.t+1

out2B<-try(power.NI.continuous(20,20,40,0, n.control=50, n.experim=50))
correct[[n.t]]<-ifelse((inherits(out2B, "try-error"))&&(grepl("A Non-inferiority margin of 0 for the mean difference means this is a superiority trial.", out2B[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out2B"
n.t=n.t+1
out2C<-try(power.NI.continuous(20,20,40,1, n.control=50, n.experim=50, summary.measure="mean.ratio"))
correct[[n.t]]<-ifelse((inherits(out2C, "try-error"))&&(grepl("A Non-inferiority margin of 1 for the mean ratio means this is a superiority trial.", out2C[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out2C"
n.t=n.t+1

# Check that it stops if mean.experimental implies inferiority with current margin:
out2D<-try(power.NI.continuous(20,9,40,-10, n.control=50, n.experim=50))
correct[[n.t]]<-ifelse((inherits(out2D, "try-error"))&&(grepl("In the alternative hypothesis the experimental treatment is not non-inferior.", out2D[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out2D"
n.t=n.t+1
out2E<-try(power.NI.continuous(20,31,40,10, n.control=50, n.experim=50, higher.better=F))
correct[[n.t]]<-ifelse((inherits(out2E, "try-error"))&&(grepl("In the alternative hypothesis the experimental treatment is not non-inferior.", out2E[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out2E"
n.t=n.t+1
out2F<-try(power.NI.continuous(20,9,40,0.5, n.control=50, n.experim=50, summary.measure="mean.ratio"))
correct[[n.t]]<-ifelse((inherits(out2F, "try-error"))&&(grepl("In the alternative hypothesis the experimental treatment is not non-inferior.", out2F[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out2F"
n.t=n.t+1
out2G<-try(power.NI.continuous(20,31,40,1.5, n.control=50, n.experim=50, summary.measure="mean.ratio", higher.better=F))
correct[[n.t]]<-ifelse((inherits(out2G, "try-error"))&&(grepl("In the alternative hypothesis the experimental treatment is not non-inferior.", out2G[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out2G"
n.t=n.t+1

#####################################################
# Third set of checks:
# Check that it stops for unacceptable values of significance level:

out3A<-try(power.NI.continuous(20,20,40,-10, n.control=50, n.experim=50, sig.level="0.025"))
correct[[n.t]]<-ifelse((inherits(out3A, "try-error"))&&(grepl("is.numeric(sig.level) is not TRUE", out3A[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out3A"
n.t=n.t+1
out3B<-try(power.NI.continuous(20,20,40,-10, n.control=50, n.experim=50, sig.level=-0.4))
correct[[n.t]]<-ifelse((inherits(out3B, "try-error"))&&(grepl("sig.level > 0 is not TRUE", out3B[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out3B"
n.t=n.t+1
out3C<-try(power.NI.continuous(20,20,40,-10, n.control=50, n.experim=50, sig.level=1))
correct[[n.t]]<-ifelse((inherits(out3C, "try-error"))&&(grepl("sig.level < 0.5 is not TRUE", out3C[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out3C"
n.t=n.t+1


#####################################################
# Fourth set of checks:
# Check that it stops for unacceptable values of n.control:

out4A<-try(power.NI.continuous(20,20,40,-10, n.control="50", n.experim=50))
correct[[n.t]]<-ifelse((inherits(out4A, "try-error"))&&(grepl("is.numeric(n.control) is not TRUE", out4A[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4A"
n.t=n.t+1
out4B<-try(power.NI.continuous(20,20,40,-10, n.control=0, n.experim=50))
correct[[n.t]]<-ifelse((inherits(out4B, "try-error"))&&(grepl("n.control > 0 is not TRUE", out4B[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4B"
n.t=n.t+1
out4C<-try(power.NI.continuous(20,20,40,-10, n.control=50, n.experim=0))
correct[[n.t]]<-ifelse((inherits(out4C, "try-error"))&&(grepl("n.experim > 0 is not TRUE", out4C[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4C"
n.t=n.t+1

out4D<-try(power.NI.continuous(20,20,40,-10, n.control=50, n.experim="50"))
correct[[n.t]]<-ifelse((inherits(out4D, "try-error"))&&(grepl("is.numeric(n.experim) is not TRUE", out4D[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4D"
n.t=n.t+1
out4E<-try(power.NI.continuous(20,20,40,-10, n.control=50, n.experim=50, n.rep=0))
correct[[n.t]]<-ifelse((inherits(out4E, "try-error"))&&(grepl("n.rep > 0 is not TRUE", out4E[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4E"
n.t=n.t+1

# Check that it work for wrong summary measure value:
out4F<-try(power.NI.continuous(20,20,40,-10, n.control=50, n.experim=50, summary.measure="pippo"))
correct[[n.t]]<-ifelse((inherits(out4F, "try-error"))&&(grepl("summary.measure %in%", out4F[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4F"
n.t=n.t+1

# Check that it works when print.out incorrectly specified:
out4G<-try(power.NI.continuous(20,20,40,-10, n.control=50, n.experim=50, print.out=NA))
correct[[n.t]]<-ifelse((inherits(out4G, "try-error"))&&(grepl("!is.na(print.out) is not TRUE", out4G[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4G"
n.t=n.t+1

# Check that it works when test.type incorrectly specified:
out4H<-try(power.NI.continuous(20,20,40,-10, n.control=50, n.experim=50, test.type=NA))
correct[[n.t]]<-ifelse((inherits(out4H, "try-error"))&&(grepl("is.character(test.type) is not TRUE", out4H[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4H"
n.t=n.t+1
out4I<-try(power.NI.continuous(20,20,40,-10, n.control=50, n.experim=50, test.type="pippo"))
correct[[n.t]]<-ifelse((inherits(out4I, "try-error"))&&(grepl("test.type %in%", out4I[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4I"
n.t=n.t+1

# Check that it works when higher.better incorrectly specified:
out4J<-try(power.NI.continuous(20,20,40,-10, n.control=50, n.experim=50, higher.better="pippo"))
correct[[n.t]]<-ifelse((inherits(out4J, "try-error"))&&(grepl("is.logical(higher.better) is not TRUE", out4J[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4J"
n.t=n.t+1
out4K<-try(power.NI.continuous(20,20,40,-10, n.control=50, n.experim=50, higher.better=NA))
correct[[n.t]]<-ifelse((inherits(out4K, "try-error"))&&(grepl("!is.na(higher.better) is not TRUE", out4K[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4K"
n.t=n.t+1

#####################################################
# Fifth set of checks:
# Now check sample size calculations for certain values on difference scale. These are compared against results from tutorial paper from Julious
set.seed(1)
out5A<-try(power.NI.continuous(20, mean.experim=20, sd=40, NI.margin=-10, sig.level = 0.025, n.control=50, n.experim=50, 
                                    summary.measure = "mean.difference", print.out = TRUE, test.type="t.test",
                                    higher.better=T))
correct[[n.t]]<-ifelse((inherits(out5A,"numeric"))&&(all.equal(out5A,24.2)),1,0) 
names(correct)[[n.t]]<-"out5A"
n.t=n.t+1
set.seed(1)
out5B<-try(power.NI.continuous(0, mean.experim=0, sd=40, NI.margin=-10, sig.level = 0.025, n.control=50, n.experim=50, 
                                    summary.measure = "mean.difference", print.out = TRUE, test.type="t.test",
                                    higher.better=T))
correct[[n.t]]<-ifelse((inherits(out5B,"numeric"))&&(all.equal(out5B,24.2)),1,0) 
names(correct)[[n.t]]<-"out5B"
n.t=n.t+1
set.seed(1)
out5C<-try(power.NI.continuous(0, mean.experim=0.25*0.05, sd=1, NI.margin=-0.05, sig.level = 0.025, n.control=50, n.experim=50, 
                                    summary.measure = "mean.difference", print.out = TRUE, test.type="t.test",
                                    higher.better=T))
correct[[n.t]]<-ifelse((inherits(out5C,"numeric"))&&(all.equal(out5C,4.8)),1,0) 
names(correct)[[n.t]]<-"out5C"
n.t=n.t+1
set.seed(1)
out5D<-try(power.NI.continuous(0, mean.experim=-0.25*0.05, sd=1, NI.margin=-0.05, sig.level = 0.025, n.control=50, n.experim=50, 
                                    summary.measure = "mean.difference", print.out = TRUE, test.type="t.test",
                                    higher.better=T))
correct[[n.t]]<-ifelse((inherits(out5D,"numeric"))&&(all.equal(out5D,3.3)),1,0) 
names(correct)[[n.t]]<-"out5D"
n.t=n.t+1
set.seed(1)
out5E<-try(power.NI.continuous(0, mean.experim=0, sd=1, NI.margin=-1.5, sig.level = 0.025, n.control=50, n.experim=50, 
                                    summary.measure = "mean.difference", print.out = TRUE, test.type="t.test",
                                    higher.better=T))
correct[[n.t]]<-ifelse((inherits(out5E,"numeric"))&&(all.equal(out5E,100)),1,0) 
names(correct)[[n.t]]<-"out5E"
n.t=n.t+1
set.seed(1)
out5F<-try(power.NI.continuous(20, mean.experim=20, sd=40, NI.margin=-10, sig.level = 0.025, n.control=50, n.experim=50, 
                                    summary.measure = "mean.difference", print.out = TRUE, test.type="Z.test",
                                    higher.better=T))
correct[[n.t]]<-ifelse((inherits(out5F,"numeric"))&&(all.equal(out5F,25.1)),1,0) 
names(correct)[[n.t]]<-"out5F"
n.t=n.t+1
set.seed(1)
out5G<-try(power.NI.continuous(20, mean.experim=20, sd=40, NI.margin=10, sig.level = 0.025, n.control=50, n.experim=50, 
                                    summary.measure = "mean.difference", print.out = TRUE, test.type="t.test",
                                    higher.better=F))
correct[[n.t]]<-ifelse((inherits(out5G,"numeric"))&&(all.equal(out5G,23.3)),1,0) 
names(correct)[[n.t]]<-"out5G"
n.t=n.t+1
set.seed(1)
out5H<-try(power.NI.continuous(20, mean.experim=20, sd=40, NI.margin=-10, sig.level = 0.05, n.control=50, n.experim=50, 
                                    summary.measure = "mean.difference", print.out = TRUE, test.type="t.test",
                                    higher.better=T))
correct[[n.t]]<-ifelse((inherits(out5H,"numeric"))&&(all.equal(out5H,36.2)),1,0) 
names(correct)[[n.t]]<-"out5H"
n.t=n.t+1
set.seed(1)
out5I<-try(power.NI.continuous(20, mean.experim=20, sd=40, NI.margin=-10, sig.level = 0.025, n.control=15, n.experim=15, 
                                    summary.measure = "mean.difference", print.out = TRUE, test.type="t.test",
                                    higher.better=T))
correct[[n.t]]<-ifelse((inherits(out5I,"numeric"))&&(all.equal(out5I,9.2)),1,0) 
names(correct)[[n.t]]<-"out5I"
n.t=n.t+1
set.seed(1)
out5J<-try(power.NI.continuous(20, mean.experim=20, sd=40, NI.margin=-10, sig.level = 0.025, n.control=50, n.experim=25, 
                                    summary.measure = "mean.difference", print.out = TRUE, test.type="t.test",
                                    higher.better=T))
correct[[n.t]]<-ifelse((inherits(out5J,"numeric"))&&(all.equal(out5J,15.4)),1,0) 
names(correct)[[n.t]]<-"out5J"
n.t=n.t+1
set.seed(1)
out5K<-try(power.NI.continuous(20, mean.experim=20, sd=40, NI.margin=-10, sig.level = 0.025, n.control=25, n.experim=50, 
                                    summary.measure = "mean.difference", print.out = TRUE, test.type="t.test",
                                    higher.better=T))
correct[[n.t]]<-ifelse((inherits(out5K,"numeric"))&&(all.equal(out5K,19)),1,0) # Checked against prop.test
names(correct)[[n.t]]<-"out5K"
n.t=n.t+1
set.seed(1)
out5L<-try(power.NI.continuous(200, mean.experim=200, sd=400, NI.margin=-10, sig.level = 0.025, n.control=50, n.experim=50, 
                                    summary.measure = "mean.difference", print.out = TRUE, test.type="t.test",
                                    higher.better=T))
correct[[n.t]]<-ifelse((inherits(out5L,"numeric"))&&(all.equal(out5L,3, tolerance=10^(-5))),1,0) 
names(correct)[[n.t]]<-"out5L"
n.t=n.t+1
set.seed(1)
out5M<-try(power.NI.continuous(2, mean.experim=2, sd=4, NI.margin=-10, sig.level = 0.025, n.control=50, n.experim=50, 
                                    summary.measure = "mean.difference", print.out = TRUE, test.type="t.test",
                                    higher.better=T))
correct[[n.t]]<-ifelse((inherits(out5M,"numeric"))&&(all.equal(out5M,100)),1,0) 
names(correct)[[n.t]]<-"out5M"
n.t=n.t+1

#####################################################
# Sixth set of checks:
# Now check sample size calculations for other configurations for which we have unfortunately no comparator yet. 
set.seed(1)
out6A<-try(power.NI.continuous(20, mean.experim=20, sd=1, NI.margin=0.75, sig.level = 0.025, n.control=50, n.experim=50, 
                                    summary.measure = "mean.ratio", print.out = TRUE, test.type="lm",
                                    higher.better=T))
correct[[n.t]] <- ifelse((is.vector(out6A)) && (all.equal(out6A, 100)) , 1, 0) 
names(correct)[[n.t]] <- "out6A"
n.t <- n.t + 1
set.seed(1)
out6B<-try(power.NI.continuous(20, mean.experim=20, sd=1, NI.margin=1.25, sig.level = 0.025, n.control=50, n.experim=50, 
                                    summary.measure = "mean.ratio", print.out = TRUE, test.type="Fiellers",
                                    higher.better=F))
correct[[n.t]] <- ifelse((is.vector(out6B)) && (all.equal(out6B, 100)) , 1, 0) 
names(correct)[[n.t]] <- "out6B"
n.t <- n.t + 1
set.seed(1)
out6C<-try(power.NI.continuous(20, mean.experim=20, sd=1, NI.margin=0.75, sig.level = 0.025, n.control=50, n.experim=50, 
                                    summary.measure = "mean.ratio", print.out = TRUE, test.type="bootstrap",
                                    higher.better=T, M.boot=20, bootCI.type="norm"))
correct[[n.t]] <- ifelse((is.vector(out6C)) && (all.equal(out6C, 100)) , 1, 0) 
names(correct)[[n.t]] <- "out6C"
n.t <- n.t + 1
set.seed(1)
out6D<-try(power.NI.continuous(20, mean.experim=20, sd=10, NI.margin=0.75, sig.level = 0.025, n.control=150, n.experim=150, 
                                    summary.measure = "mean.ratio", print.out = TRUE, test.type="Fiellers",
                                    higher.better=T))
correct[[n.t]] <- ifelse((is.vector(out6D)) && (all.equal(out6D, 99.9)) , 1, 0) 
names(correct)[[n.t]] <- "out6D"
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

pw.NI.c<-(tot.correct==number.of.tests) 


