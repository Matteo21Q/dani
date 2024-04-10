###################################################
### power.NI.binary  testing file             #####
### 05-04-2024                                #####
###################################################

# Load dani:
# library(dani)

#Initialise vector of outputs 
correct<-list(NULL)
n.t<-1

#####################################################
# First set of checks:
# Check that it stops for non acceptable values of expected probabilities:

out1A<-try(power.NI.binary("0.1",0.1,0.05, n.control=100, n.experim=100))
correct[[n.t]]<-ifelse((inherits(out1A, "try-error"))&&(grepl("is.numeric(p.control.expected) is not TRUE", out1A[1], fixed=T  )),1,0) 
names(correct)[[n.t]]<-"out1A"
n.t=n.t+1
out1B<-try(power.NI.binary(-0.1,0.1,0.05, n.control=100, n.experim=100))
correct[[n.t]]<-ifelse((inherits(out1B, "try-error"))&&(grepl("p.control.expected > 0 is not TRUE", out1B[1], fixed=T  )),1,0) 
names(correct)[[n.t]]<-"out1B"
n.t=n.t+1
out1C<-try(power.NI.binary(1,0.1,0.05, n.control=100, n.experim=100))
correct[[n.t]]<-ifelse((inherits(out1C, "try-error"))&&(grepl("p.control.expected < 1 is not TRUE", out1C[1], fixed=T )),1,0) 
names(correct)[[n.t]]<-"out1C"
n.t=n.t+1
out1D<-try(power.NI.binary(0.1,"0.1",0.05, n.control=100, n.experim=100))
correct[[n.t]]<-ifelse((inherits(out1D, "try-error"))&&(grepl("is.numeric(p.experim.target) is not TRUE", out1D[1], fixed=T  )),1,0) 
names(correct)[[n.t]]<-"out1D"
n.t=n.t+1
out1E<-try(power.NI.binary(0.1,0,0.05, n.control=100, n.experim=100))
correct[[n.t]]<-ifelse((inherits(out1E, "try-error"))&&(grepl("p.experim.target > 0 is not TRUE", out1E[1], fixed=T  )),1,0) 
names(correct)[[n.t]]<-"out1E"
n.t=n.t+1
out1F<-try(power.NI.binary(0.1,1.2,0.05, n.control=100, n.experim=100))
correct[[n.t]]<-ifelse((inherits(out1F, "try-error"))&&(grepl("p.experim.target < 1 is not TRUE", out1F[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out1F"
n.t=n.t+1

#####################################################
# Second set of checks:
# Check that it stops for non-acceptable margins:

out2A<-try(power.NI.binary(0.1,0.1,"0.05", n.control=100, n.experim=100))
correct[[n.t]]<-ifelse((inherits(out2A, "try-error"))&&(grepl("is.numeric(NI.margin) is not TRUE", out2A[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out2A"
n.t=n.t+1
out2B<-try(power.NI.binary(0.1,0.1,-0.05, summary.measure="RR", n.control=100, n.experim=100))
correct[[n.t]]<-ifelse((inherits(out2B, "try-error"))&&(grepl("NI margin should be >0 when summary measure is a ratio (OR or RR)", out2B[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out2B"
n.t=n.t+1
out2C<-try(power.NI.binary(0.1,0.1,-0.05, summary.measure="OR", n.control=100, n.experim=100))
correct[[n.t]]<-ifelse((inherits(out2C, "try-error"))&&(grepl("NI margin should be >0 when summary measure is a ratio (OR or RR)", out2C[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out2C"
n.t=n.t+1
out2D<-try(power.NI.binary(0.1,0.1,1, summary.measure="RD", n.control=100, n.experim=100))
correct[[n.t]]<-ifelse((inherits(out2D, "try-error"))&&(grepl("NI margin should be <1 in absolute value when summary measure is RD", out2D[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out2D"
n.t=n.t+1
out2E<-try(power.NI.binary(0.1,0.1,-1.1, summary.measure="RD", n.control=100, n.experim=100))
correct[[n.t]]<-ifelse((inherits(out2E, "try-error"))&&(grepl("NI margin should be <1 in absolute value when summary measure is RD", out2E[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out2E"
n.t=n.t+1
out2F<-try(power.NI.binary(0.1,0.2,0.05, summary.measure="RD", n.control=100, n.experim=100))
correct[[n.t]]<-ifelse((inherits(out2F, "try-error"))&&(grepl("In the alternative hypothesis the experimental treatment is not non-inferior.", out2F[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out2F"
n.t=n.t+1
out2G<-try(power.NI.binary(0.9,0.8,-0.05, n.control=100, n.experim=100, summary.measure="RD", unfavourable = F))
correct[[n.t]]<-ifelse((inherits(out2G, "try-error"))&&(grepl("In the alternative hypothesis the experimental treatment is not non-inferior.", out2G[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out2G"
n.t=n.t+1
out2H<-try(power.NI.binary(0.1,0.2,1.5, n.control=100, n.experim=100, summary.measure="RR"))
correct[[n.t]]<-ifelse((inherits(out2H, "try-error"))&&(grepl("In the alternative hypothesis the experimental treatment is not non-inferior.", out2H[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out2H"
n.t=n.t+1
out2I<-try(power.NI.binary(0.2,0.1,0.75, n.control=100, n.experim=100, summary.measure="RR", unfavourable = F))
correct[[n.t]]<-ifelse((inherits(out2I, "try-error"))&&(grepl("In the alternative hypothesis the experimental treatment is not non-inferior.", out2I[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out2I"
n.t=n.t+1
out2J<-try(power.NI.binary(0.5,0.1,0.5, n.control=100, n.experim=100, summary.measure="OR", unfavourable = F))
correct[[n.t]]<-ifelse((inherits(out2J, "try-error"))&&(grepl("In the alternative hypothesis the experimental treatment is not non-inferior.", out2J[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out2J"
n.t=n.t+1
out2K<-try(power.NI.binary(0.1,0.5,2, n.control=100, n.experim=100, summary.measure="OR"))
correct[[n.t]]<-ifelse((inherits(out2K, "try-error"))&&(grepl("In the alternative hypothesis the experimental treatment is not non-inferior.", out2K[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out2K"
n.t=n.t+1

#####################################################
# Third set of checks:
# Check that it stops for unacceptable values of significance level:

out3A<-try(power.NI.binary(0.1,0.1,0.05, n.control=100, n.experim=100, sig.level = "0.025"))
correct[[n.t]]<-ifelse((inherits(out3A, "try-error"))&&(grepl("is.numeric(sig.level) is not TRUE", out3A[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out3A"
n.t=n.t+1
out3B<-try(power.NI.binary(0.1,0.1,0.05, n.control=100, n.experim=100, sig.level=-0.1))
correct[[n.t]]<-ifelse((inherits(out3B, "try-error"))&&(grepl("sig.level > 0 is not TRUE", out3B[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out3B"
n.t=n.t+1
out3C<-try(power.NI.binary(0.1,0.1,0.05, n.control=100, n.experim=100, sig.level=1))
correct[[n.t]]<-ifelse((inherits(out3C, "try-error"))&&(grepl("sig.level < 0.5 is not TRUE", out3C[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out3C"
n.t=n.t+1

#####################################################
# Fourth set of checks:
# Check with other wrong arguments:

# Check that it stops for unacceptable values of power:
out4A<-try(power.NI.binary(0.1,0.1,0.05, , n.control="100", n.experim=100))
correct[[n.t]]<-ifelse((inherits(out4A, "try-error"))&&(grepl("is.numeric(n.control) is not TRUE", out4A[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4A"
n.t=n.t+1
out4B<-try(power.NI.binary(0.1,0.1,0.05, , n.control=0, n.experim=100))
correct[[n.t]]<-ifelse((inherits(out4B, "try-error"))&&(grepl("n.control > 0 is not TRUE", out4B[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4B"
n.t=n.t+1
out4C<-try(power.NI.binary(0.1,0.1,0.05, n.control=100, n.experim=0))
correct[[n.t]]<-ifelse((inherits(out4C, "try-error"))&&(grepl("n.experim > 0 is not TRUE", out4C[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4C"
n.t=n.t+1
out4D<-try(power.NI.binary(0.1,0.1,0.05, n.control=100, n.experim="100"))
correct[[n.t]]<-ifelse((inherits(out4D, "try-error"))&&(grepl("is.numeric(n.experim) is not TRUE", out4D[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4D"
n.t=n.t+1
out4E<-try(power.NI.binary(0.1,0.1,0.05, n.control=100))
correct[[n.t]]<-ifelse((inherits(out4E, "try-error"))&&(grepl("is missing", out4E[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4E"
n.t=n.t+1

# Check that it works for wrong summary measure value:
out4F<-try(power.NI.binary(0.1,0.1,0.05, n.control=100, n.experim=100, summary.measure = "pippo"))
correct[[n.t]]<-ifelse((inherits(out4F, "try-error"))&&(grepl("summary.measure %in%", out4F[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4F"
n.t=n.t+1

# Check that it works when print.out incorrectly specified:
out4G<-try(power.NI.binary(0.1,0.1,0.05, n.control=100, n.experim=100, print.out = NA))
correct[[n.t]]<-ifelse((inherits(out4G, "try-error"))&&(grepl("!is.na(print.out) is not TRUE", out4G[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4G"
n.t=n.t+1

# Check that it works when test.type incorrectly specified:
out4H<-try(power.NI.binary(0.1,0.1,0.05, n.control=100, n.experim=100, test.type = NA))
correct[[n.t]]<-ifelse((inherits(out4H, "try-error"))&&(grepl("is.character(test.type) is not TRUE", out4H[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4H"
n.t=n.t+1
out4I<-try(power.NI.binary(0.1,0.1,0.05, n.control=100, n.experim=100, test.type = "pippo"))
correct[[n.t]]<-ifelse((inherits(out4I, "try-error"))&&(grepl("test.type %in%", out4I[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4I"
n.t=n.t+1

# Check that it works when unfavourable incorrectly specified:
out4J<-try(power.NI.binary(0.1,0.1,0.05, n.control=100, n.experim=100, unfavourable = NA))
correct[[n.t]]<-ifelse((inherits(out4J, "try-error"))&&(grepl("!is.na(unfavourable) is not TRUE", out4J[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4J"
n.t=n.t+1
out4K<-try(power.NI.binary(0.1,0.1,0.05, n.control=100, n.experim=100, unfavourable = "pippo"))
correct[[n.t]]<-ifelse((inherits(out4K, "try-error"))&&(grepl("is.logical(unfavourable) is not TRUE", out4K[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4K"
n.t=n.t+1


#####################################################
# Fifth set of checks:
# Now check sample size calculations for certain values on RD scale. These are compared against results from artbin
set.seed(1)
out5A<-try(power.NI.binary(0.1, p.experim.target=0.1, NI.margin=0.05, sig.level = 0.025, n.control=100, n.experim=100, 
                                summary.measure = "RD", print.out = TRUE, test.type="Wald",
                                unfavourable=T))
correct[[n.t]]<-ifelse((inherits(out5A,"numeric"))&&(all.equal(out5A,24.1)),1,0) 
names(correct)[[n.t]]<-"out5A"
n.t=n.t+1
set.seed(1)
out5B<-try(power.NI.binary(0.1, p.experim.target=0.1, NI.margin=0.05, sig.level = 0.025, n.control=100, n.experim=100, 
                                summary.measure = "RD", print.out = TRUE, test.type="Newcombe10",
                                unfavourable=T))
correct[[n.t]]<-ifelse((inherits(out5B,"numeric"))&&(all.equal(out5B,22.1)),1,0) 
names(correct)[[n.t]]<-"out5B"
n.t=n.t+1
set.seed(1)
out5C<-try(power.NI.binary(0.1, p.experim.target=0.1, NI.margin=0.05, sig.level = 0.025, n.control=100, n.experim=100, 
                                summary.measure = "RD", print.out = TRUE, test.type="Gart.Nam",
                                unfavourable=T))
correct[[n.t]]<-ifelse((inherits(out5C,"numeric"))&&(all.equal(out5C,22.1)),1,0) 
names(correct)[[n.t]]<-"out5C"
n.t=n.t+1
set.seed(1)
out5D<-try(power.NI.binary(0.1, p.experim.target=0.12, NI.margin=0.05, sig.level = 0.025, n.control=100, n.experim=100, 
                                summary.measure = "RD", print.out = TRUE, test.type="Wald",
                                unfavourable=T))
correct[[n.t]]<-ifelse((inherits(out5D,"numeric"))&&(all.equal(out5D,12.9)),1,0) 
names(correct)[[n.t]]<-"out5D"
n.t=n.t+1
set.seed(1)
out5E<-try(power.NI.binary(0.5, p.experim.target=0.45, NI.margin=0.4, sig.level = 0.025, n.control=100, n.experim=100, 
                                summary.measure = "RD", print.out = TRUE, test.type="Wald",
                                unfavourable=T))
correct[[n.t]]<-ifelse((inherits(out5E,"numeric"))&&(all.equal(out5E,100)),1,0) 
names(correct)[[n.t]]<-"out5E"
n.t=n.t+1
set.seed(1)
out5F<-try(power.NI.binary(0.1, p.experim.target=0.1, NI.margin=0.05, sig.level = 0.05, n.control=100, n.experim=100, 
                                summary.measure = "RD", print.out = TRUE, test.type="Wald",
                                unfavourable=T))
correct[[n.t]]<-ifelse((inherits(out5F,"numeric"))&&(all.equal(out5F,35.9)),1,0) 
names(correct)[[n.t]]<-"out5F"
n.t=n.t+1
set.seed(1)
out5G<-try(power.NI.binary(0.1, p.experim.target=0.1, NI.margin=0.05, sig.level = 0.025, n.control=100, n.experim=100, 
                                summary.measure = "RD", print.out = TRUE, test.type="Wald",
                                unfavourable=T))
correct[[n.t]]<-ifelse((inherits(out5G,"numeric"))&&(all.equal(out5G,24.1)),1,0) 
names(correct)[[n.t]]<-"out5G"
n.t=n.t+1
set.seed(1)
out5H<-try(power.NI.binary(0.1, p.experim.target=0.1, NI.margin=0.05, sig.level = 0.025, n.control=200, n.experim=100, 
                                summary.measure = "RD", print.out = TRUE, test.type="Wald",
                                unfavourable=T))
correct[[n.t]]<-ifelse((inherits(out5H,"numeric"))&&(all.equal(out5H,33.4)),1,0) 
names(correct)[[n.t]]<-"out5H"
n.t=n.t+1
set.seed(1)
out5I<-try(power.NI.binary(0.1, p.experim.target=0.1, NI.margin=0.05, sig.level = 0.025, n.control=100, n.experim=200, 
                                summary.measure = "RD", print.out = TRUE, test.type="Wald",
                                unfavourable=T))
correct[[n.t]]<-ifelse((inherits(out5I,"numeric"))&&(all.equal(out5I,27)),1,0) 
names(correct)[[n.t]]<-"out5I"
n.t=n.t+1
set.seed(1)
out5J<-try(power.NI.binary(0.1, p.experim.target=0.1, NI.margin=0.05, sig.level = 0.025, n.control=100, n.experim=100, 
                                summary.measure = "RD", print.out = TRUE, test.type="Wald",
                                unfavourable=T))
correct[[n.t]]<-ifelse((inherits(out5J,"numeric"))&&(all.equal(out5J,24.1)),1,0) 
names(correct)[[n.t]]<-"out5J"
n.t=n.t+1
#####################################################
# Sixth set of checks:
# Now check sample size calculations for certain values on OR scale. These are compared against results from artcat
set.seed(1)
out6A<-try(power.NI.binary(0.1, p.experim.target=0.1, NI.margin=1.5, sig.level = 0.025, n.control=100, n.experim=100, 
                                summary.measure = "OR", print.out = TRUE, test.type="Wald.Woolf",
                                unfavourable=T))
correct[[n.t]] <- ifelse((is.vector(out6A)) && (all.equal(out6A, 16.7)) , 1, 0)
names(correct)[[n.t]] <- "out6A"
n.t <- n.t + 1
set.seed(1)
out6B<-try(power.NI.binary(0.1, p.experim.target=0.1, NI.margin=1.5, sig.level = 0.025, n.control=100, n.experim=100, 
                                summary.measure = "OR", print.out = TRUE, test.type="logistic",
                                unfavourable=T))
correct[[n.t]] <- ifelse((is.vector(out6B)) && (all.equal(out6B, 16.7)) , 1, 0) 
names(correct)[[n.t]] <- "out6B"
n.t <- n.t + 1
set.seed(1)
out6C<-try(power.NI.binary(0.1, p.experim.target=0.1, NI.margin=1.5, sig.level = 0.025, n.control=100, n.experim=100, 
                                summary.measure = "OR", print.out = TRUE, test.type="adjusted.Wald.Woolf",
                                unfavourable=T))
correct[[n.t]] <- ifelse((is.vector(out6C)) && (all.equal(out6C, 16.7)) , 1, 0) 
names(correct)[[n.t]] <- "out6C"
n.t <- n.t + 1
set.seed(1)
out6D<-try(power.NI.binary(0.1, p.experim.target=0.12, NI.margin=1.5, sig.level = 0.025, n.control=100, n.experim=100, 
                                summary.measure = "OR", print.out = TRUE, test.type="Wald.Woolf",
                                unfavourable=T))
correct[[n.t]] <- ifelse((is.vector(out6D)) && (all.equal(out6D, 8.5)) , 1, 0) 
names(correct)[[n.t]] <- "out6D"
n.t <- n.t + 1
set.seed(1)
out6E<-try(power.NI.binary(0.5, p.experim.target=0.45, NI.margin=3, sig.level = 0.025, n.control=100, n.experim=100, 
                                summary.measure = "OR", print.out = TRUE, test.type="Wald.Woolf",
                                unfavourable=T))
correct[[n.t]] <- ifelse((is.vector(out6E)) && (all.equal(out6E, 99.4)) , 1, 0) 
names(correct)[[n.t]] <- "out6E"
n.t <- n.t + 1
set.seed(1)
out6F<-try(power.NI.binary(0.1, p.experim.target=0.1, NI.margin=1.5, sig.level = 0.05, n.control=100, n.experim=100, 
                                summary.measure = "OR", print.out = TRUE, test.type="Wald.Woolf",
                                unfavourable=T))
correct[[n.t]] <- ifelse((is.vector(out6F)) && (all.equal(out6F, 23.1)) , 1, 0) 
names(correct)[[n.t]] <- "out6F"
n.t <- n.t + 1
set.seed(1)
out6G<-try(power.NI.binary(0.1, p.experim.target=0.1, NI.margin=1.5, sig.level = 0.025, n.control=200, n.experim=100, 
                                summary.measure = "OR", print.out = TRUE, test.type="Wald.Woolf",
                                unfavourable=T))
correct[[n.t]] <- ifelse((is.vector(out6G)) && (all.equal(out6G, 17.3)) , 1, 0) 
names(correct)[[n.t]] <- "out6G"
n.t <- n.t + 1
set.seed(1)
out6H<-try(power.NI.binary(0.1, p.experim.target=0.1, NI.margin=1.5, sig.level = 0.025, n.control=100, n.experim=200, 
                                summary.measure = "OR", print.out = TRUE, test.type="Wald.Woolf",
                                unfavourable=T))
correct[[n.t]] <- ifelse((is.vector(out6H)) && (all.equal(out6H, 21)) , 1, 0) 
names(correct)[[n.t]] <- "out6H"
n.t <- n.t + 1
set.seed(1)
out6I<-try(power.NI.binary(0.1, p.experim.target=0.1, NI.margin=1.5, sig.level = 0.025, n.control=10, n.experim=10, 
                                summary.measure = "OR", print.out = TRUE, test.type="adjusted.Wald.Woolf",
                                unfavourable=T))
correct[[n.t]] <- ifelse((is.vector(out6I)) && (all.equal(out6I, 0.3)) , 1, 0) 
names(correct)[[n.t]] <- "out6I"
n.t <- n.t + 1

#####################################################
# Seventh set of checks:
# Check results against artbin and artcat for one example with favourable outcome
set.seed(1)
out7A<-try(power.NI.binary(0.1, p.experim.target=0.1, NI.margin=-0.05, sig.level = 0.025, n.control=100, n.experim=100, 
                                summary.measure = "RD", print.out = TRUE, test.type="Wald",
                                unfavourable=F))
correct[[n.t]] <- ifelse((is.vector(out7A)) && (all.equal(out7A, 23.8)) , 1, 0) # Checked against riskratio epitools
names(correct)[[n.t]] <- "out7A"
n.t <- n.t + 1
set.seed(1)
out7B<-try(power.NI.binary(0.1, p.experim.target=0.1, NI.margin=0.75, sig.level = 0.025, n.control=100, n.experim=100, 
                                summary.measure = "OR", print.out = TRUE, test.type="Wald.Woolf",
                                unfavourable=F))
correct[[n.t]] <- ifelse((is.vector(out7B)) && (all.equal(out7B, 8.8)) , 1, 0) # Checked against riskratio epitools
names(correct)[[n.t]] <- "out7B"
n.t <- n.t + 1

#####################################################
# Eight set of checks:
# Check results for RR and AS. These are just compared against known results from our paper on NI frontiers:
set.seed(1)
out8A<-try(power.NI.binary(0.05, p.experim.target=0.05, NI.margin=2, sig.level = 0.025, n.control=100, n.experim=100, 
                                summary.measure = "RR", print.out = TRUE, test.type="adjusted.Wald.Katz",
                                unfavourable=T))
correct[[n.t]] <- ifelse((is.vector(out8A)) && (all.equal(out8A, 23.1)) , 1, 0) # Checked against riskratio epitools
names(correct)[[n.t]] <- "out8A"
n.t <- n.t + 1
set.seed(1)
out8B<-try(power.NI.binary(0.05, p.experim.target=0.05, NI.margin=convertmargin.binary(0.05,0.05, "RD", "AS"), sig.level = 0.025, n.control=100, n.experim=100, 
                                summary.measure = "AS", print.out = TRUE, test.type="Wald",
                                unfavourable=T))
correct[[n.t]] <- ifelse((is.vector(out8B)) && (all.equal(out8B, 29.7)) , 1, 0) # Checked against riskratio epitools
names(correct)[[n.t]] <- "out8B"
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

pw.NI.b<-(tot.correct==number.of.tests) 


