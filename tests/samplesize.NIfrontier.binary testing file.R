#####################################################
### samplesize.NIfrontier.binary  testing file  #####
### 18-08-2023                                  #####
#####################################################

# Load dani:
# library(dani)

#Initialise vector of outputs 
correct<-list(NULL)
n.t<-1

#####################################################
# First set of checks:
# Check that it stops for non acceptable values of expected probabilities:
#RD
NI.frontier.RD<-function(p) return(0.1)
out1A<-try(samplesize.NIfrontier.binary("0.1",0.1,NI.frontier.RD, 0.025))
correct[[n.t]]<-ifelse((inherits(out1A, "try-error"))&&(grepl("is.numeric(p.control.expected) is not TRUE", out1A[1], fixed=T  )),1,0) 
names(correct)[[n.t]]<-"out1A"
n.t=n.t+1
out1B<-try(samplesize.NIfrontier.binary(-0.1,0.1,NI.frontier.RD, 0.025))
correct[[n.t]]<-ifelse((inherits(out1B, "try-error"))&&(grepl("p.control.expected > 0 is not TRUE", out1B[1], fixed=T  )),1,0) 
names(correct)[[n.t]]<-"out1B"
n.t=n.t+1
out1C<-try(samplesize.NIfrontier.binary(1,0.1,NI.frontier.RD, 0.025))
correct[[n.t]]<-ifelse((inherits(out1C, "try-error"))&&(grepl("p.control.expected < 1 is not TRUE", out1C[1], fixed=T )),1,0) 
names(correct)[[n.t]]<-"out1C"
n.t=n.t+1
out1D<-try(samplesize.NIfrontier.binary(0.1,"0.1",NI.frontier.RD, 0.025))
correct[[n.t]]<-ifelse((inherits(out1D, "try-error"))&&(grepl("is.numeric(p.experim.target) is not TRUE", out1D[1], fixed=T  )),1,0) 
names(correct)[[n.t]]<-"out1D"
n.t=n.t+1
out1E<-try(samplesize.NIfrontier.binary(0.1,0,NI.frontier.RD, 0.025))
correct[[n.t]]<-ifelse((inherits(out1E, "try-error"))&&(grepl("p.experim.target > 0 is not TRUE", out1E[1], fixed=T  )),1,0) 
names(correct)[[n.t]]<-"out1E"
n.t=n.t+1
out1F<-try(samplesize.NIfrontier.binary(0.1,1.2,NI.frontier.RD, 0.025))
correct[[n.t]]<-ifelse((inherits(out1F, "try-error"))&&(grepl("p.experim.target < 1 is not TRUE", out1F[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out1F"
n.t=n.t+1

#####################################################
# Second set of checks:
# Check that it stops for non-acceptable margins:

NI.frontier.err1<-function(p) return("0.1")
out2A<-try(samplesize.NIfrontier.binary(0.1,0.1,NI.frontier.err1, 0.025))
correct[[n.t]]<-ifelse((inherits(out2A, "try-error"))&&(grepl("is.numeric(NI.margin) is not TRUE", out2A[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out2A"
n.t=n.t+1
NI.frontier.err2<-function(p) return(-0.5)
out2B<-try(samplesize.NIfrontier.binary(0.1,0.1,NI.frontier.err2, 0.025, summary.measure="RR"))
correct[[n.t]]<-ifelse((inherits(out2B, "try-error"))&&(grepl("NI margin should be >0 when summary measure is a ratio (OR or RR)", out2B[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out2B"
n.t=n.t+1
out2C<-try(samplesize.NIfrontier.binary(0.1,0.1,NI.frontier.err2, 0.025, summary.measure="OR"))
correct[[n.t]]<-ifelse((inherits(out2C, "try-error"))&&(grepl("NI margin should be >0 when summary measure is a ratio (OR or RR)", out2C[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out2C"
n.t=n.t+1
NI.frontier.err3<-function(p) return(1)
out2D<-try(samplesize.NIfrontier.binary(0.1,0.1,NI.frontier.err3, 0.025))
correct[[n.t]]<-ifelse((inherits(out2D, "try-error"))&&(grepl("NI margin should be <1 in absolute value when summary measure is RD", out2D[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out2D"
n.t=n.t+1
NI.frontier.err4<-function(p) return(-1)
out2E<-try(samplesize.NIfrontier.binary(0.1,0.1,NI.frontier.err4, 0.025))
correct[[n.t]]<-ifelse((inherits(out2E, "try-error"))&&(grepl("NI margin should be <1 in absolute value when summary measure is RD", out2E[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out2E"
n.t=n.t+1
out2F<-try(samplesize.NIfrontier.binary(0.1,0.2,NI.frontier.RD, 0.025))
correct[[n.t]]<-ifelse((inherits(out2F, "try-error"))&&(grepl("In the alternative hypothesis the experimental treatment is not non-inferior.", out2F[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out2F"
n.t=n.t+1
NI.frontier.RDf<-function(p) return(-0.1)
out2G<-try(samplesize.NIfrontier.binary(0.9,0.8,NI.frontier.RDf,0.025, summary.measure="RD", unfavourable = F))
correct[[n.t]]<-ifelse((inherits(out2G, "try-error"))&&(grepl("In the alternative hypothesis the experimental treatment is not non-inferior.", out2G[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out2G"
n.t=n.t+1
NI.frontier.RR<-function(p) return(1.5)
out2H<-try(samplesize.NIfrontier.binary(0.1,0.2,NI.frontier.RR, 0.025, summary.measure="RR"))
correct[[n.t]]<-ifelse((inherits(out2H, "try-error"))&&(grepl("In the alternative hypothesis the experimental treatment is not non-inferior.", out2H[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out2H"
n.t=n.t+1
out2I<-try(samplesize.NIfrontier.binary(0.1,0.1,0.05, 0.025))
correct[[n.t]]<-ifelse((inherits(out2I, "try-error"))&&(grepl("is.function(NI.frontier) is not TRUE", out2I[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out2I"
n.t=n.t+1

#####################################################
# Third set of checks:
# Check that it stops for unacceptable values of significance level:

out3A<-try(samplesize.NIfrontier.binary(0.1,0.1,NI.frontier.RD, sig.level = "0.025"))
correct[[n.t]]<-ifelse((inherits(out3A, "try-error"))&&(grepl("is.numeric(sig.level) is not TRUE", out3A[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out3A"
n.t=n.t+1
out3B<-try(samplesize.NIfrontier.binary(0.1,0.1,NI.frontier.RD, sig.level=-0.1))
correct[[n.t]]<-ifelse((inherits(out3B, "try-error"))&&(grepl("sig.level > 0 is not TRUE", out3B[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out3B"
n.t=n.t+1
out3C<-try(samplesize.NIfrontier.binary(0.1,0.1,NI.frontier.RD, sig.level=1))
correct[[n.t]]<-ifelse((inherits(out3C, "try-error"))&&(grepl("sig.level < 0.5 is not TRUE", out3C[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out3C"
n.t=n.t+1

#####################################################
# Fourth set of checks:
# Check with other wrong arguments:

# Check that it stops for unacceptable values of power:
out4A<-try(samplesize.NIfrontier.binary(0.1,0.1,NI.frontier.RD, power = "0.9"))
correct[[n.t]]<-ifelse((inherits(out4A, "try-error"))&&(grepl("is.numeric(power) is not TRUE", out4A[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4A"
n.t=n.t+1
out4B<-try(samplesize.NIfrontier.binary(0.1,0.1,NI.frontier.RD, power=0))
correct[[n.t]]<-ifelse((inherits(out4B, "try-error"))&&(grepl("power > 0 is not TRUE", out4B[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4B"
n.t=n.t+1
out4C<-try(samplesize.NIfrontier.binary(0.1,0.1,NI.frontier.RD, power=1))
correct[[n.t]]<-ifelse((inherits(out4C, "try-error"))&&(grepl("power < 1 is not TRUE", out4C[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4C"
n.t=n.t+1

# Check that it stops for unacceptable values of allocation ratio:
out4D<-try(samplesize.NIfrontier.binary(0.1,0.1,NI.frontier.RD, r = "0.9"))
correct[[n.t]]<-ifelse((inherits(out4D, "try-error"))&&(grepl("is.numeric(r) is not TRUE", out4D[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4D"
n.t=n.t+1
out4E<-try(samplesize.NIfrontier.binary(0.1,0.1,NI.frontier.RD, r=0))
correct[[n.t]]<-ifelse((inherits(out4E, "try-error"))&&(grepl("r > 0 is not TRUE", out4E[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4E"
n.t=n.t+1

# Check that it works for wrong summary measure value:
out4F<-try(samplesize.NIfrontier.binary(0.1,0.1,NI.frontier.RD, summary.measure = "pippo"))
correct[[n.t]]<-ifelse((inherits(out4F, "try-error"))&&(grepl("summary.measure %in%", out4F[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4F"
n.t=n.t+1

# Check that it works when print.out incorrectly specified:
out4G<-try(samplesize.NIfrontier.binary(0.1,0.1,NI.frontier.RD, print.out = NA))
correct[[n.t]]<-ifelse((inherits(out4G, "try-error"))&&(grepl("!is.na(print.out) is not TRUE", out4G[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4G"
n.t=n.t+1

# Check that it works when unfavourable incorrectly specified:

out4H<-try(samplesize.NIfrontier.binary(0.1,0.1,NI.frontier.RD, unfavourable = NA))
correct[[n.t]]<-ifelse((inherits(out4H, "try-error"))&&(grepl("!is.na(unfavourable) is not TRUE", out4H[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4H"
n.t=n.t+1
out4I<-try(samplesize.NIfrontier.binary(0.1,0.1,NI.frontier.RD, unfavourable = "pippo"))
correct[[n.t]]<-ifelse((inherits(out4I, "try-error"))&&(grepl("is.logical(unfavourable) is not TRUE", out4I[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4I"
n.t=n.t+1

# Check that it works when round incorrectly specified:
out4L<-try(samplesize.NIfrontier.binary(0.1,0.1,NI.frontier.RD,  round=NA))
correct[[n.t]]<-ifelse((inherits(out4L, "try-error"))&&(grepl("!is.na(round) is not TRUE", out4L[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4L"
n.t=n.t+1
out4M<-try(samplesize.NIfrontier.binary(0.1,0.1,NI.frontier.RD, round="pippo"))
correct[[n.t]]<-ifelse((inherits(out4M, "try-error"))&&(grepl("is.logical(round) is not TRUE", out4M[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4M"
n.t=n.t+1

# Check that it stops for unacceptable values of loss to follow up:
out4N<-try(samplesize.NIfrontier.binary(0.1,0.1,NI.frontier.RD, ltfu = "0.9"))
correct[[n.t]]<-ifelse((inherits(out4N, "try-error"))&&(grepl("is.numeric(ltfu) is not TRUE", out4N[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4N"
n.t=n.t+1
out4O<-try(samplesize.NIfrontier.binary(0.1,0.1,NI.frontier.RD,  ltfu=-1))
correct[[n.t]]<-ifelse((inherits(out4O, "try-error"))&&(grepl("ltfu >= 0 is not TRUE", out4O[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4O"
n.t=n.t+1
out4P<-try(samplesize.NIfrontier.binary(0.1,0.1,NI.frontier.RD, ltfu=1))
correct[[n.t]]<-ifelse((inherits(out4P, "try-error"))&&(grepl("ltfu < 1 is not TRUE", out4P[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4P"
n.t=n.t+1


#####################################################
# Fifth set of checks:
# Now check sample size calculations for certain values on RD scale. 
out5A<-try(samplesize.NIfrontier.binary(0.1, p.experim.target=0.1, NI.frontier.RD, sig.level = 0.025, power = 0.9, r = 1, 
                                summary.measure = "RD", print.out = TRUE, 
                                unfavourable=T))
correct[[n.t]]<-ifelse((inherits(out5A,"numeric"))&&(all.equal(out5A[2],385)),1,0)  
names(correct)[[n.t]]<-"out5A"
n.t=n.t+1
NI.frontier.RR<-function(p) {
  p0.expected<-0.1
  NI.marg=0.1
  RR<-(p0.expected+NI.marg)/p0.expected
  
  return(p*RR-p)
} 
out5B<-try(samplesize.NIfrontier.binary(0.1, p.experim.target=0.1, NI.frontier.RR, sig.level = 0.025, power = 0.9, r = 1, 
                                summary.measure = "RD", print.out = TRUE, 
                                unfavourable=T))
correct[[n.t]]<-ifelse((inherits(out5B,"numeric"))&&(all.equal(out5B[2],866)),1,0)  
names(correct)[[n.t]]<-"out5B"
n.t=n.t+1
NI.frontier.AS<-function(x) {
  NI.marg<-0.1
  p0.expected<-0.12
  AS<-as.numeric(convertmargin.binary(p0.expected, NI.marg,"RD", "AS"))
  
  y<-convertmargin.binary(x, AS,"AS", "RD")
  return(y)
}
out5C<-try(samplesize.NIfrontier.binary(0.1, p.experim.target=0.1, NI.frontier.AS, sig.level = 0.025, power = 0.9, r = 1, 
                                summary.measure = "RD", print.out = TRUE, 
                                unfavourable=T))
correct[[n.t]]<-ifelse((inherits(out5C,"numeric"))&&(all.equal(out5C[2],564)),1,0)  
names(correct)[[n.t]]<-"out5C"
n.t=n.t+1
out5D<-try(samplesize.NIfrontier.binary(0.1, p.experim.target=0.12, NI.frontier.RD, sig.level = 0.025, power = 0.9, r = 1, 
                                summary.measure = "RD", print.out = TRUE, 
                                unfavourable=T))
correct[[n.t]]<-ifelse((inherits(out5D,"numeric"))&&(all.equal(out5D[2],654)),1,0)  
names(correct)[[n.t]]<-"out5D"
n.t=n.t+1
out5E<-try(samplesize.NIfrontier.binary(0.5, p.experim.target=0.45, NI.frontier.RD, sig.level = 0.025, power = 0.9, r = 1, 
                                summary.measure = "RD", print.out = TRUE,
                                unfavourable=T))
correct[[n.t]]<-ifelse((inherits(out5E,"numeric"))&&(all.equal(out5E[2],448)),1,0)  
names(correct)[[n.t]]<-"out5E"
n.t=n.t+1
out5F<-try(samplesize.NIfrontier.binary(0.1, p.experim.target=0.1, NI.frontier.RD, sig.level = 0.05, power = 0.9, r = 1, 
                                summary.measure = "RD", print.out = TRUE, 
                                unfavourable=T))
correct[[n.t]]<-ifelse((inherits(out5F,"numeric"))&&(all.equal(out5F[2],325)),1,0)  
names(correct)[[n.t]]<-"out5F"
n.t=n.t+1
out5G<-try(samplesize.NIfrontier.binary(0.1, p.experim.target=0.1, NI.frontier.RD, sig.level = 0.025, power = 0.54, r = 1, 
                                summary.measure = "RD", print.out = TRUE, 
                                unfavourable=T))
correct[[n.t]]<-ifelse((inherits(out5G,"numeric"))&&(all.equal(out5G[2],166)),1,0)  
names(correct)[[n.t]]<-"out5G"
n.t=n.t+1
out5H<-try(samplesize.NIfrontier.binary(0.1, p.experim.target=0.1, NI.frontier.RD, sig.level = 0.025, power = 0.9, r = 0.5, 
                                summary.measure = "RD", print.out = TRUE, 
                                unfavourable=T))
correct[[n.t]]<-ifelse((inherits(out5H,"numeric"))&&(all.equal(out5H[2],335)),1,0)  
names(correct)[[n.t]]<-"out5H"
n.t=n.t+1
out5I<-try(samplesize.NIfrontier.binary(0.1, p.experim.target=0.1, NI.frontier.RD, sig.level = 0.025, power = 0.9, r = 2, 
                                summary.measure = "RD", print.out = TRUE, 
                                unfavourable=T))
correct[[n.t]]<-ifelse((inherits(out5I,"numeric"))&&(all.equal(out5I[2],483)),1,0)  
names(correct)[[n.t]]<-"out5I"
n.t=n.t+1
out5J<-try(samplesize.NIfrontier.binary(0.2, p.experim.target=0.2, NI.frontier.RDf, sig.level = 0.025, power = 0.9, r = 1, 
                                summary.measure = "RD", print.out = TRUE, 
                                unfavourable=F))
correct[[n.t]]<-ifelse((inherits(out5J,"numeric"))&&(all.equal(out5J[2],656)),1,0)  
names(correct)[[n.t]]<-"out5J"
n.t=n.t+1
out5K<-try(samplesize.NIfrontier.binary(0.1, p.experim.target=0.1, NI.frontier.RD, sig.level = 0.025, power = 0.9, r = 1, 
                                        summary.measure = "RD", print.out = TRUE, 
                                        unfavourable=T, round=F))
correct[[n.t]]<-ifelse((inherits(out5K,"numeric"))&&(all.equal(out5K[2],384.7856, tolerance=10^(-5))),1,0)  
names(correct)[[n.t]]<-"out5K"
n.t=n.t+1
out5L<-try(samplesize.NIfrontier.binary(0.1, p.experim.target=0.1, NI.frontier.RD, sig.level = 0.025, power = 0.9, r = 1, 
                                        summary.measure = "RD", print.out = TRUE, 
                                        unfavourable=T, ltfu=0.1))
correct[[n.t]]<-ifelse((inherits(out5L,"numeric"))&&(all.equal(out5L[2],428, tolerance=10^(-5))),1,0)  
names(correct)[[n.t]]<-"out5L"
n.t=n.t+1
#####################################################
# Sixth set of checks:
# Now check sample size calculations for certain values on OR scale. 

NI.frontier.ratio<-function(p) return(1.5)
out6A<-try(samplesize.NIfrontier.binary(0.1, p.experim.target=0.1, NI.frontier.ratio, sig.level = 0.025, power = 0.9, r = 1, 
                                summary.measure = "OR", print.out = TRUE, 
                                unfavourable=T))
correct[[n.t]] <- ifelse((is.vector(out6A)) && (all.equal(out6A[2], 2715)) , 1, 0)  
names(correct)[[n.t]] <- "out6A"
n.t <- n.t + 1
out6B<-try(samplesize.NIfrontier.binary(0.1, p.experim.target=0.12, NI.frontier.ratio, sig.level = 0.025, power = 0.9, r = 1, 
                                summary.measure = "OR", print.out = TRUE,
                                unfavourable=T))
correct[[n.t]] <- ifelse((is.vector(out6B)) && (all.equal(out6B[2], 8327)) , 1, 0)  
names(correct)[[n.t]] <- "out6B"
n.t <- n.t + 1
out6C<-try(samplesize.NIfrontier.binary(0.5, p.experim.target=0.45, NI.frontier.ratio, sig.level = 0.025, power = 0.9, r = 1, 
                                summary.measure = "OR", print.out = TRUE, 
                                unfavourable=T))
correct[[n.t]] <- ifelse((is.vector(out6C)) && (all.equal(out6C[2], 617)) , 1, 0)  
names(correct)[[n.t]] <- "out6C"
n.t <- n.t + 1
out6D<-try(samplesize.NIfrontier.binary(0.1, p.experim.target=0.1, NI.frontier.ratio, sig.level = 0.05, power = 0.9, r = 1, 
                                summary.measure = "OR", print.out = TRUE, 
                                unfavourable=T))
correct[[n.t]] <- ifelse((is.vector(out6D)) && (all.equal(out6D[2], 2209)) , 1, 0)  
names(correct)[[n.t]] <- "out6D"
n.t <- n.t + 1
out6E<-try(samplesize.NIfrontier.binary(0.1, p.experim.target=0.1, NI.frontier.ratio, sig.level = 0.025, power = 0.54, r = 1, 
                                summary.measure = "OR", print.out = TRUE, 
                                unfavourable=T))
correct[[n.t]] <- ifelse((is.vector(out6E)) && (all.equal(out6E[2], 1102)) , 1, 0)  
names(correct)[[n.t]] <- "out6E"
n.t <- n.t + 1
out6F<-try(samplesize.NIfrontier.binary(0.1, p.experim.target=0.1, NI.frontier.ratio, sig.level = 0.025, power = 0.9, r = 0.5, 
                                summary.measure = "OR", print.out = TRUE, 
                                unfavourable=T))
correct[[n.t]] <- ifelse((is.vector(out6F)) && (all.equal(out6F[2], 1967)) , 1, 0)  
names(correct)[[n.t]] <- "out6F"
n.t <- n.t + 1
out6G<-try(samplesize.NIfrontier.binary(0.1, p.experim.target=0.1, NI.frontier.ratio, sig.level = 0.025, power = 0.9, r = 2, 
                                summary.measure = "OR", print.out = TRUE, 
                                unfavourable=T))
correct[[n.t]] <- ifelse((is.vector(out6G)) && (all.equal(out6G[2], 4215)) , 1, 0)  
names(correct)[[n.t]] <- "out6G"
n.t <- n.t + 1
NI.frontier.ratiof<-function(p) return(0.5)
out6H<-try(samplesize.NIfrontier.binary(0.1, p.experim.target=0.1, NI.frontier.ratiof, sig.level = 0.025, power = 0.9, r = 1, 
                                summary.measure = "OR", print.out = TRUE, 
                                unfavourable=F))
correct[[n.t]] <- ifelse((is.vector(out6H)) && (all.equal(out6H[2], 964)) , 1, 0)  
names(correct)[[n.t]] <- "out6H"
n.t <- n.t + 1

#####################################################
# Seventh set of checks:
# Check results for RR and AS. These are just compared against known results from our paper on NI frontiers:

out7A<-try(samplesize.NIfrontier.binary(0.05, p.experim.target=0.05, NI.frontier.ratio, sig.level = 0.025, power = 0.9, r = 1, 
                                summary.measure = "RR", print.out = TRUE,
                                unfavourable=T))
correct[[n.t]] <- ifelse((is.vector(out7A)) && (all.equal(out7A[2], 4868)) , 1, 0)  
names(correct)[[n.t]] <- "out7A"
n.t <- n.t + 1
out7B<-try(samplesize.NIfrontier.binary(0.05, p.experim.target=0.05, NI.frontier.RD, sig.level = 0.025, power = 0.9, r = 1, 
                                summary.measure = "AS", print.out = TRUE, 
                                unfavourable=T))
correct[[n.t]] <- ifelse((is.vector(out7B)) && (all.equal(out7B[2], 1005)) , 1, 0)  
names(correct)[[n.t]] <- "out7B"
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

ss.NIf.b<-(tot.correct==number.of.tests) 


