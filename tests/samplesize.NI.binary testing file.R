###################################################
### samplesize.NI.binary  testing file        #####
### 11-08-2023                                #####
###################################################

# Load dani:
# library(dani)

#Initialise vector of outputs 
correct<-list(NULL)
n.t<-1

#####################################################
# First set of checks:
# Check that it stops for non acceptable values of expected probabilities:

out1A<-try(samplesize.NI.binary("0.1",0.1,0.05))
correct[[n.t]]<-ifelse((inherits(out1A, "try-error"))&&(grepl("is.numeric(p.control.expected) is not TRUE", out1A[1], fixed=T  )),1,0) 
names(correct)[[n.t]]<-"out1A"
n.t=n.t+1
out1B<-try(samplesize.NI.binary(-0.1,0.1,0.05))
correct[[n.t]]<-ifelse((inherits(out1B, "try-error"))&&(grepl("p.control.expected > 0 is not TRUE", out1B[1], fixed=T  )),1,0) 
names(correct)[[n.t]]<-"out1B"
n.t=n.t+1
out1C<-try(samplesize.NI.binary(1,0.1,0.05))
correct[[n.t]]<-ifelse((inherits(out1C, "try-error"))&&(grepl("p.control.expected < 1 is not TRUE", out1C[1], fixed=T )),1,0) 
names(correct)[[n.t]]<-"out1C"
n.t=n.t+1
out1D<-try(samplesize.NI.binary(0.1,"0.1",0.05))
correct[[n.t]]<-ifelse((inherits(out1D, "try-error"))&&(grepl("is.numeric(p.experim.target) is not TRUE", out1D[1], fixed=T  )),1,0) 
names(correct)[[n.t]]<-"out1D"
n.t=n.t+1
out1E<-try(samplesize.NI.binary(0.1,0,0.05))
correct[[n.t]]<-ifelse((inherits(out1E, "try-error"))&&(grepl("p.experim.target > 0 is not TRUE", out1E[1], fixed=T  )),1,0) 
names(correct)[[n.t]]<-"out1E"
n.t=n.t+1
out1F<-try(samplesize.NI.binary(0.1,1.2,0.05))
correct[[n.t]]<-ifelse((inherits(out1F, "try-error"))&&(grepl("p.experim.target < 1 is not TRUE", out1F[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out1F"
n.t=n.t+1

#####################################################
# Second set of checks:
# Check that it stops for non-acceptable margins:

out2A<-try(samplesize.NI.binary(0.1,0.1,"0.05"))
correct[[n.t]]<-ifelse((inherits(out2A, "try-error"))&&(grepl("is.numeric(NI.margin) is not TRUE", out2A[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out2A"
n.t=n.t+1
out2B<-try(samplesize.NI.binary(0.1,0.1,-0.05, summary.measure="RR"))
correct[[n.t]]<-ifelse((inherits(out2B, "try-error"))&&(grepl("NI margin should be >0 when summary measure is a ratio (OR or RR)", out2B[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out2B"
n.t=n.t+1
out2C<-try(samplesize.NI.binary(0.1,0.1,-0.05, summary.measure="OR"))
correct[[n.t]]<-ifelse((inherits(out2C, "try-error"))&&(grepl("NI margin should be >0 when summary measure is a ratio (OR or RR)", out2C[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out2C"
n.t=n.t+1
out2D<-try(samplesize.NI.binary(0.1,0.1,1, summary.measure="RD"))
correct[[n.t]]<-ifelse((inherits(out2D, "try-error"))&&(grepl("NI margin should be <1 in absolute value when summary measure is RD", out2D[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out2D"
n.t=n.t+1
out2E<-try(samplesize.NI.binary(0.1,0.1,-1.1, summary.measure="RD"))
correct[[n.t]]<-ifelse((inherits(out2E, "try-error"))&&(grepl("NI margin should be <1 in absolute value when summary measure is RD", out2E[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out2E"
n.t=n.t+1
out2F<-try(samplesize.NI.binary(0.1,0.2,0.05, summary.measure="RD"))
correct[[n.t]]<-ifelse((inherits(out2F, "try-error"))&&(grepl("In the alternative hypothesis the experimental treatment is not non-inferior.", out2F[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out2F"
n.t=n.t+1
out2G<-try(samplesize.NI.binary(0.9,0.8,-0.05, summary.measure="RD", unfavourable = F))
correct[[n.t]]<-ifelse((inherits(out2G, "try-error"))&&(grepl("In the alternative hypothesis the experimental treatment is not non-inferior.", out2G[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out2G"
n.t=n.t+1
out2H<-try(samplesize.NI.binary(0.1,0.2,1.5, summary.measure="RR"))
correct[[n.t]]<-ifelse((inherits(out2H, "try-error"))&&(grepl("In the alternative hypothesis the experimental treatment is not non-inferior.", out2H[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out2H"
n.t=n.t+1
out2I<-try(samplesize.NI.binary(0.2,0.1,0.75, summary.measure="RR", unfavourable = F))
correct[[n.t]]<-ifelse((inherits(out2I, "try-error"))&&(grepl("In the alternative hypothesis the experimental treatment is not non-inferior.", out2I[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out2I"
n.t=n.t+1
out2J<-try(samplesize.NI.binary(0.5,0.1,0.5, summary.measure="OR", unfavourable = F))
correct[[n.t]]<-ifelse((inherits(out2J, "try-error"))&&(grepl("In the alternative hypothesis the experimental treatment is not non-inferior.", out2J[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out2J"
n.t=n.t+1
out2K<-try(samplesize.NI.binary(0.1,0.5,2, summary.measure="OR"))
correct[[n.t]]<-ifelse((inherits(out2K, "try-error"))&&(grepl("In the alternative hypothesis the experimental treatment is not non-inferior.", out2K[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out2K"
n.t=n.t+1

#####################################################
# Third set of checks:
# Check that it stops for unacceptable values of significance level:

out3A<-try(samplesize.NI.binary(0.1,0.1,0.05, sig.level = "0.025"))
correct[[n.t]]<-ifelse((inherits(out3A, "try-error"))&&(grepl("is.numeric(sig.level) is not TRUE", out3A[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out3A"
n.t=n.t+1
out3B<-try(samplesize.NI.binary(0.1,0.1,0.05, sig.level=-0.1))
correct[[n.t]]<-ifelse((inherits(out3B, "try-error"))&&(grepl("sig.level > 0 is not TRUE", out3B[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out3B"
n.t=n.t+1
out3C<-try(samplesize.NI.binary(0.1,0.1,0.05, sig.level=1))
correct[[n.t]]<-ifelse((inherits(out3C, "try-error"))&&(grepl("sig.level < 0.5 is not TRUE", out3C[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out3C"
n.t=n.t+1

#####################################################
# Fourth set of checks:
# Check with other wrong arguments:

# Check that it stops for unacceptable values of power:
out4A<-try(samplesize.NI.binary(0.1,0.1,0.05, power = "0.9"))
correct[[n.t]]<-ifelse((inherits(out4A, "try-error"))&&(grepl("is.numeric(power) is not TRUE", out4A[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4A"
n.t=n.t+1
out4B<-try(samplesize.NI.binary(0.1,0.1,0.05, power=0))
correct[[n.t]]<-ifelse((inherits(out4B, "try-error"))&&(grepl("power > 0 is not TRUE", out4B[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4B"
n.t=n.t+1
out4C<-try(samplesize.NI.binary(0.1,0.1,0.05, power=1))
correct[[n.t]]<-ifelse((inherits(out4C, "try-error"))&&(grepl("power < 1 is not TRUE", out4C[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4C"
n.t=n.t+1

# Check that it stops for unacceptable values of allocation ratio:
out4D<-try(samplesize.NI.binary(0.1,0.1,0.05, r = "0.9"))
correct[[n.t]]<-ifelse((inherits(out4D, "try-error"))&&(grepl("is.numeric(r) is not TRUE", out4D[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4D"
n.t=n.t+1
out4E<-try(samplesize.NI.binary(0.1,0.1,0.05, r=0))
correct[[n.t]]<-ifelse((inherits(out4E, "try-error"))&&(grepl("r > 0 is not TRUE", out4E[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4E"
n.t=n.t+1

# Check that it works for wrong summary measure value:
out4F<-try(samplesize.NI.binary(0.1,0.1,0.05, summary.measure = "pippo"))
correct[[n.t]]<-ifelse((inherits(out4F, "try-error"))&&(grepl("summary.measure %in%", out4F[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4F"
n.t=n.t+1

# Check that it works when print.out incorrectly specified:
out4G<-try(samplesize.NI.binary(0.1,0.1,0.05, print.out = NA))
correct[[n.t]]<-ifelse((inherits(out4G, "try-error"))&&(grepl("!is.na(print.out) is not TRUE", out4G[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4G"
n.t=n.t+1

# Check that it works when test.type incorrectly specified:
out4H<-try(samplesize.NI.binary(0.1,0.1,0.05, test.type = NA))
correct[[n.t]]<-ifelse((inherits(out4H, "try-error"))&&(grepl("is.character(test.type) is not TRUE", out4H[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4H"
n.t=n.t+1
out4I<-try(samplesize.NI.binary(0.1,0.1,0.05, test.type = "pippo"))
correct[[n.t]]<-ifelse((inherits(out4I, "try-error"))&&(grepl("test.type %in%", out4I[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4I"
n.t=n.t+1

# Check that it works when unfavourable incorrectly specified:
out4J<-try(samplesize.NI.binary(0.1,0.1,0.05, unfavourable = NA))
correct[[n.t]]<-ifelse((inherits(out4J, "try-error"))&&(grepl("!is.na(unfavourable) is not TRUE", out4J[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4J"
n.t=n.t+1
out4K<-try(samplesize.NI.binary(0.1,0.1,0.05, unfavourable = "pippo"))
correct[[n.t]]<-ifelse((inherits(out4K, "try-error"))&&(grepl("is.logical(unfavourable) is not TRUE", out4K[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4K"
n.t=n.t+1

# Check that it works when cont.corr incorrectly specified:
out4L<-try(samplesize.NI.binary(0.1,0.1,0.05, cont.corr = NA))
correct[[n.t]]<-ifelse((inherits(out4L, "try-error"))&&(grepl("!is.na(cont.corr) is not TRUE", out4L[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4L"
n.t=n.t+1
out4M<-try(samplesize.NI.binary(0.1,0.1,0.05, cont.corr = "pippo"))
correct[[n.t]]<-ifelse((inherits(out4M, "try-error"))&&(grepl("is.logical(cont.corr) is not TRUE", out4M[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4M"
n.t=n.t+1

# Check that it works when round incorrectly specified:
out4N<-try(samplesize.NI.binary(0.1,0.1,0.05, round = NA))
correct[[n.t]]<-ifelse((inherits(out4N, "try-error"))&&(grepl("!is.na(round) is not TRUE", out4N[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4N"
n.t=n.t+1
out4O<-try(samplesize.NI.binary(0.1,0.1,0.05, round = "pippo"))
correct[[n.t]]<-ifelse((inherits(out4O, "try-error"))&&(grepl("is.logical(round) is not TRUE", out4O[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4O"
n.t=n.t+1

# Check that it stops for unacceptable values of loss to follow up:
out4P<-try(samplesize.NI.binary(0.1,0.1,0.05, ltfu = "0.9"))
correct[[n.t]]<-ifelse((inherits(out4P, "try-error"))&&(grepl("is.numeric(ltfu) is not TRUE", out4P[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4P"
n.t=n.t+1
out4Q<-try(samplesize.NI.binary(0.1,0.1,0.05, ltfu=-1))
correct[[n.t]]<-ifelse((inherits(out4Q, "try-error"))&&(grepl("ltfu >= 0 is not TRUE", out4Q[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4Q"
n.t=n.t+1
out4R<-try(samplesize.NI.binary(0.1,0.1,0.05, ltfu=1))
correct[[n.t]]<-ifelse((inherits(out4R, "try-error"))&&(grepl("ltfu < 1 is not TRUE", out4R[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4R"
n.t=n.t+1

#####################################################
# Fifth set of checks:
# Now check sample size calculations for certain values on RD scale. These are compared against results from artbin
out5A<-try(samplesize.NI.binary(0.1, p.experim.target=0.1, NI.margin=0.05, sig.level = 0.025, power = 0.9, r = 1, 
                                 summary.measure = "RD", print.out = TRUE, test.type="Wald",
                                 unfavourable=T, cont.corr=F))
correct[[n.t]]<-ifelse((inherits(out5A,"numeric"))&&(all.equal(out5A[2],757)),1,0) 
names(correct)[[n.t]]<-"out5A"
n.t=n.t+1
out5B<-try(samplesize.NI.binary(0.1, p.experim.target=0.1, NI.margin=0.05, sig.level = 0.025, power = 0.9, r = 1, 
                                 summary.measure = "RD", print.out = TRUE, test.type="score",
                                 unfavourable=T, cont.corr=F))
correct[[n.t]]<-ifelse((inherits(out5B,"numeric"))&&(all.equal(out5B[2],775)),1,0) 
names(correct)[[n.t]]<-"out5B"
n.t=n.t+1
out5C<-try(samplesize.NI.binary(0.1, p.experim.target=0.1, NI.margin=0.05, sig.level = 0.025, power = 0.9, r = 1, 
                                 summary.measure = "RD", print.out = TRUE, test.type="local",
                                 unfavourable=T, cont.corr=F))
correct[[n.t]]<-ifelse((inherits(out5C,"numeric"))&&(all.equal(out5C[2],787)),1,0) 
names(correct)[[n.t]]<-"out5C"
n.t=n.t+1
out5D<-try(samplesize.NI.binary(0.1, p.experim.target=0.12, NI.margin=0.05, sig.level = 0.025, power = 0.9, r = 1, 
                                 summary.measure = "RD", print.out = TRUE, test.type="Wald",
                                 unfavourable=T, cont.corr=F))
correct[[n.t]]<-ifelse((inherits(out5D,"numeric"))&&(all.equal(out5D[2],2284)),1,0) 
names(correct)[[n.t]]<-"out5D"
n.t=n.t+1
out5E<-try(samplesize.NI.binary(0.5, p.experim.target=0.45, NI.margin=0.4, sig.level = 0.025, power = 0.9, r = 1, 
                                 summary.measure = "RD", print.out = TRUE, test.type="Wald",
                                 unfavourable=T, cont.corr=F))
correct[[n.t]]<-ifelse((inherits(out5E,"numeric"))&&(all.equal(out5E[2],26)),1,0) 
names(correct)[[n.t]]<-"out5E"
n.t=n.t+1
out5F<-try(samplesize.NI.binary(0.1, p.experim.target=0.1, NI.margin=0.05, sig.level = 0.05, power = 0.9, r = 1, 
                                 summary.measure = "RD", print.out = TRUE, test.type="Wald",
                                 unfavourable=T, cont.corr=F))
correct[[n.t]]<-ifelse((inherits(out5F,"numeric"))&&(all.equal(out5F[2],617)),1,0) 
names(correct)[[n.t]]<-"out5F"
n.t=n.t+1
out5G<-try(samplesize.NI.binary(0.1, p.experim.target=0.1, NI.margin=0.05, sig.level = 0.025, power = 0.54, r = 1, 
                                 summary.measure = "RD", print.out = TRUE, test.type="Wald",
                                 unfavourable=T, cont.corr=F))
correct[[n.t]]<-ifelse((inherits(out5G,"numeric"))&&(all.equal(out5G[2],306)),1,0) 
names(correct)[[n.t]]<-"out5G"
n.t=n.t+1
out5H<-try(samplesize.NI.binary(0.1, p.experim.target=0.1, NI.margin=0.05, sig.level = 0.025, power = 0.9, r = 0.5, 
                                 summary.measure = "RD", print.out = TRUE, test.type="Wald",
                                 unfavourable=T, cont.corr=F))
correct[[n.t]]<-ifelse((inherits(out5H,"numeric"))&&(all.equal(out5H[2],568)),1,0) 
names(correct)[[n.t]]<-"out5H"
n.t=n.t+1
out5I<-try(samplesize.NI.binary(0.1, p.experim.target=0.1, NI.margin=0.05, sig.level = 0.025, power = 0.9, r = 2, 
                                 summary.measure = "RD", print.out = TRUE, test.type="Wald",
                                 unfavourable=T, cont.corr=F))
correct[[n.t]]<-ifelse((inherits(out5I,"numeric"))&&(all.equal(out5I[2],1135)),1,0) 
names(correct)[[n.t]]<-"out5I"
n.t=n.t+1
out5J<-try(samplesize.NI.binary(0.1, p.experim.target=0.1, NI.margin=0.05, sig.level = 0.025, power = 0.9, r = 1, 
                                 summary.measure = "RD", print.out = TRUE, test.type="Wald",
                                 unfavourable=T, cont.corr=T))
correct[[n.t]]<-ifelse((inherits(out5J,"numeric"))&&(all.equal(out5J[2],797)),1,0) 
names(correct)[[n.t]]<-"out5J"
n.t=n.t+1
out5K<-try(samplesize.NI.binary(0.1, p.experim.target=0.1, NI.margin=0.05, sig.level = 0.025, power = 0.9, r = 1, 
                                summary.measure = "RD", print.out = TRUE, test.type="Wald",
                                unfavourable=T, cont.corr=F, round=F))
correct[[n.t]]<-ifelse((inherits(out5K,"numeric"))&&(all.equal(out5K[2],756.5345, tolerance = 10^(-5))),1,0) 
names(correct)[[n.t]]<-"out5K"
n.t=n.t+1
out5L<-try(samplesize.NI.binary(0.1, p.experim.target=0.1, NI.margin=0.05, sig.level = 0.025, power = 0.9, r = 1, 
                                summary.measure = "RD", print.out = TRUE, test.type="Wald",
                                unfavourable=T, cont.corr=T, ltfu=0.1))
correct[[n.t]]<-ifelse((inherits(out5L,"numeric"))&&(all.equal(out5L[2],881)),1,0) 
names(correct)[[n.t]]<-"out5L"
n.t=n.t+1
#####################################################
# Sixth set of checks:
# Now check sample size calculations for certain values on OR scale. These are compared against results from artcat

out6A<-try(samplesize.NI.binary(0.1, p.experim.target=0.1, NI.margin=1.5, sig.level = 0.025, power = 0.9, r = 1, 
                                 summary.measure = "OR", print.out = TRUE, test.type="Wald",
                                 unfavourable=T, cont.corr=F))
correct[[n.t]] <- ifelse((is.vector(out6A)) && (all.equal(out6A[2], 1421)) , 1, 0)
names(correct)[[n.t]] <- "out6A"
n.t <- n.t + 1
out6B<-try(samplesize.NI.binary(0.1, p.experim.target=0.1, NI.margin=1.5, sig.level = 0.025, power = 0.9, r = 1, 
                                 summary.measure = "OR", print.out = TRUE, test.type="score",
                                 unfavourable=T, cont.corr=F))
correct[[n.t]] <- ifelse((is.vector(out6B)) && (all.equal(out6B[2], 1447)) , 1, 0) 
names(correct)[[n.t]] <- "out6B"
n.t <- n.t + 1
out6C<-try(samplesize.NI.binary(0.1, p.experim.target=0.1, NI.margin=1.5, sig.level = 0.025, power = 0.9, r = 1, 
                                 summary.measure = "OR", print.out = TRUE, test.type="local",
                                 unfavourable=T, cont.corr=F))
correct[[n.t]] <- ifelse((is.vector(out6C)) && (all.equal(out6C[2], 1464)) , 1, 0) 
names(correct)[[n.t]] <- "out6C"
n.t <- n.t + 1
out6D<-try(samplesize.NI.binary(0.1, p.experim.target=0.12, NI.margin=1.5, sig.level = 0.025, power = 0.9, r = 1, 
                                 summary.measure = "OR", print.out = TRUE, test.type="Wald",
                                 unfavourable=T, cont.corr=F))
correct[[n.t]] <- ifelse((is.vector(out6D)) && (all.equal(out6D[2], 5371)) , 1, 0) 
names(correct)[[n.t]] <- "out6D"
n.t <- n.t + 1
out6E<-try(samplesize.NI.binary(0.5, p.experim.target=0.45, NI.margin=3, sig.level = 0.025, power = 0.9, r = 1, 
                                 summary.measure = "OR", print.out = TRUE, test.type="Wald",
                                 unfavourable=T, cont.corr=F))
correct[[n.t]] <- ifelse((is.vector(out6E)) && (all.equal(out6E[2], 51)) , 1, 0) 
names(correct)[[n.t]] <- "out6E"
n.t <- n.t + 1
out6F<-try(samplesize.NI.binary(0.1, p.experim.target=0.1, NI.margin=1.5, sig.level = 0.05, power = 0.9, r = 1, 
                                 summary.measure = "OR", print.out = TRUE, test.type="Wald",
                                 unfavourable=T, cont.corr=F))
correct[[n.t]] <- ifelse((is.vector(out6F)) && (all.equal(out6F[2], 1158)) , 1, 0) 
names(correct)[[n.t]] <- "out6F"
n.t <- n.t + 1
out6G<-try(samplesize.NI.binary(0.1, p.experim.target=0.1, NI.margin=1.5, sig.level = 0.025, power = 0.54, r = 1, 
                                 summary.measure = "OR", print.out = TRUE, test.type="Wald",
                                 unfavourable=T, cont.corr=F))
correct[[n.t]] <- ifelse((is.vector(out6G)) && (all.equal(out6G[2], 574)) , 1, 0) 
names(correct)[[n.t]] <- "out6G"
n.t <- n.t + 1
out6H<-try(samplesize.NI.binary(0.1, p.experim.target=0.1, NI.margin=1.5, sig.level = 0.025, power = 0.9, r = 0.5, 
                                 summary.measure = "OR", print.out = TRUE, test.type="Wald",
                                 unfavourable=T, cont.corr=F))
correct[[n.t]] <- ifelse((is.vector(out6H)) && (all.equal(out6H[2], 1066)) , 1, 0) 
names(correct)[[n.t]] <- "out6H"
n.t <- n.t + 1
out6I<-try(samplesize.NI.binary(0.1, p.experim.target=0.1, NI.margin=1.5, sig.level = 0.025, power = 0.9, r = 2, 
                                 summary.measure = "OR", print.out = TRUE, test.type="Wald",
                                 unfavourable=T, cont.corr=F))
correct[[n.t]] <- ifelse((is.vector(out6I)) && (all.equal(out6I[2], 2131)) , 1, 0) 
names(correct)[[n.t]] <- "out6I"
n.t <- n.t + 1

#####################################################
# Seventh set of checks:
# Check results against artbin and artcat for one example with favourable outcome

out7A<-try(samplesize.NI.binary(0.1, p.experim.target=0.1, NI.margin=-0.05, sig.level = 0.025, power = 0.9, r = 1, 
                                 summary.measure = "RD", print.out = TRUE, test.type="Wald",
                                 unfavourable=F, cont.corr=F))
correct[[n.t]] <- ifelse((is.vector(out7A)) && (all.equal(out7A[2], 757)) , 1, 0) # Checked against riskratio epitools
names(correct)[[n.t]] <- "out7A"
n.t <- n.t + 1
out7B<-try(samplesize.NI.binary(0.1, p.experim.target=0.1, NI.margin=0.75, sig.level = 0.025, power = 0.9, r = 1, 
                                 summary.measure = "OR", print.out = TRUE, test.type="Wald",
                                 unfavourable=F, cont.corr=F))
correct[[n.t]] <- ifelse((is.vector(out7B)) && (all.equal(out7B[2], 2822)) , 1, 0) # Checked against riskratio epitools
names(correct)[[n.t]] <- "out7B"
n.t <- n.t + 1

#####################################################
# Eight set of checks:
# Check results for RR and AS. These are just compared against known results from our paper on NI frontiers:

out8A<-try(samplesize.NI.binary(0.05, p.experim.target=0.05, NI.margin=2, sig.level = 0.025, power = 0.9, r = 1, 
                                 summary.measure = "RR", print.out = TRUE, test.type="Wald",
                                 unfavourable=T, cont.corr=F))
correct[[n.t]] <- ifelse((is.vector(out8A)) && (all.equal(out8A[2], 832)) , 1, 0) # Checked against riskratio epitools
names(correct)[[n.t]] <- "out8A"
n.t <- n.t + 1
out8B<-try(samplesize.NI.binary(0.05, p.experim.target=0.05, NI.margin=convertmargin.binary(0.05,0.05, "RD", "AS"), sig.level = 0.025, power = 0.9, r = 1, 
                                 summary.measure = "AS", print.out = TRUE, test.type="Wald",
                                 unfavourable=T, cont.corr=F))
correct[[n.t]] <- ifelse((is.vector(out8B)) && (all.equal(out8B[2], 568)) , 1, 0) # Checked against riskratio epitools
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

ss.NI.b<-(tot.correct==number.of.tests) 


