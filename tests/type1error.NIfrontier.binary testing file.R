#####################################################
### type1error.NIfrontier.binary  testing file  #####
### 09-04-2024                                  #####
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
out1A<-try(type1error.NIfrontier.binary("0.1",NI.frontier.RD, 0.025, n.control=100, n.experim=100))
correct[[n.t]]<-ifelse((inherits(out1A, "try-error"))&&(grepl("is.numeric(p.control.expected) is not TRUE", out1A[1], fixed=T  )),1,0) 
names(correct)[[n.t]]<-"out1A"
n.t=n.t+1
out1B<-try(type1error.NIfrontier.binary(-0.1,NI.frontier.RD, 0.025, n.control=100, n.experim=100))
correct[[n.t]]<-ifelse((inherits(out1B, "try-error"))&&(grepl("p.control.expected > 0 is not TRUE", out1B[1], fixed=T  )),1,0) 
names(correct)[[n.t]]<-"out1B"
n.t=n.t+1
out1C<-try(type1error.NIfrontier.binary(1,NI.frontier.RD, 0.025, n.control=100, n.experim=100))
correct[[n.t]]<-ifelse((inherits(out1C, "try-error"))&&(grepl("p.control.expected < 1 is not TRUE", out1C[1], fixed=T )),1,0) 
names(correct)[[n.t]]<-"out1C"
n.t=n.t+1

#####################################################
# Second set of checks:
# Check that it stops for non-acceptable margins:

NI.frontier.err1<-function(p) return("0.1")
out2A<-try(type1error.NIfrontier.binary(0.1,NI.frontier.err1, 0.025, n.control=100, n.experim=100))
correct[[n.t]]<-ifelse((inherits(out2A, "try-error"))&&(grepl("is.numeric(NI.frontier(p.control.expected)) is not TRUE", out2A[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out2A"
n.t=n.t+1
out2B<-try(type1error.NIfrontier.binary(0.1,0.05, 0.025, n.control=100, n.experim=100))
correct[[n.t]]<-ifelse((inherits(out2B, "try-error"))&&(grepl("is.function(NI.frontier) is not TRUE", out2B[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out2B"
n.t=n.t+1

#####################################################
# Third set of checks:
# Check that it stops for unacceptable values of significance level:

out3A<-try(type1error.NIfrontier.binary(0.1,NI.frontier.RD, sig.level = "0.025", n.control=100, n.experim=100))
correct[[n.t]]<-ifelse((inherits(out3A, "try-error"))&&(grepl("is.function(sig.level)", out3A[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out3A"
n.t=n.t+1
out3B<-try(type1error.NIfrontier.binary(0.1,NI.frontier.RD, sig.level=-0.1, n.control=100, n.experim=100))
correct[[n.t]]<-ifelse((inherits(out3B, "try-error"))&&(grepl("is.function(sig.level)", out3B[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out3B"
n.t=n.t+1
out3C<-try(type1error.NIfrontier.binary(0.1,NI.frontier.RD, sig.level=1, n.control=100, n.experim=100))
correct[[n.t]]<-ifelse((inherits(out3C, "try-error"))&&(grepl("is.function(sig.level)", out3C[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out3C"
n.t=n.t+1

#####################################################
# Fourth set of checks:
# Check with other wrong arguments:

# Check that it stops for unacceptable values of n.,control/n.experim:
out4A<-try(type1error.NIfrontier.binary(0.1,NI.frontier.RD, n.control="100", n.experim=100))
correct[[n.t]]<-ifelse((inherits(out4A, "try-error"))&&(grepl("is.numeric(n.control) is not TRUE", out4A[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4A"
n.t=n.t+1
out4B<-try(type1error.NIfrontier.binary(0.1,NI.frontier.RD, n.control=0, n.experim=100))
correct[[n.t]]<-ifelse((inherits(out4B, "try-error"))&&(grepl("n.control > 0 is not TRUE", out4B[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4B"
n.t=n.t+1
out4C<-try(type1error.NIfrontier.binary(0.1,NI.frontier.RD, n.control=100, n.experim=0))
correct[[n.t]]<-ifelse((inherits(out4C, "try-error"))&&(grepl("n.experim > 0 is not TRUE", out4C[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4C"
n.t=n.t+1
out4D<-try(type1error.NIfrontier.binary(0.1,NI.frontier.RD, n.control=100, n.experim="100"))
correct[[n.t]]<-ifelse((inherits(out4D, "try-error"))&&(grepl("is.numeric(n.experim) is not TRUE", out4D[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4D"
n.t=n.t+1
out4E<-try(type1error.NIfrontier.binary(0.1,NI.frontier.RD, n.control=100, n.experim=100, n.rep=0))
correct[[n.t]]<-ifelse((inherits(out4E, "try-error"))&&(grepl("n.rep > 0 is not TRUE", out4E[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4E"
n.t=n.t+1

# Check that it works for wrong summary measure value:
out4F<-try(type1error.NIfrontier.binary(0.1,NI.frontier.RD, summary.measure = "pippo", n.control=100, n.experim=100))
correct[[n.t]]<-ifelse((inherits(out4F, "try-error"))&&(grepl("summary.measure %in%", out4F[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4F"
n.t=n.t+1

# Check that it works when print.out incorrectly specified:
out4G<-try(type1error.NIfrontier.binary(0.1,NI.frontier.RD, print.out = NA, n.control=100, n.experim=100))
correct[[n.t]]<-ifelse((inherits(out4G, "try-error"))&&(grepl("!is.na(print.out) is not TRUE", out4G[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4G"
n.t=n.t+1

# Check that it works when unfavourable incorrectly specified:

out4H<-try(type1error.NIfrontier.binary(0.1,NI.frontier.RD, unfavourable = NA, n.control=100, n.experim=100))
correct[[n.t]]<-ifelse((inherits(out4H, "try-error"))&&(grepl("!is.na(unfavourable) is not TRUE", out4H[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4H"
n.t=n.t+1
out4I<-try(type1error.NIfrontier.binary(0.1,NI.frontier.RD, unfavourable = "pippo", n.control=100, n.experim=100))
correct[[n.t]]<-ifelse((inherits(out4I, "try-error"))&&(grepl("is.logical(unfavourable) is not TRUE", out4I[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4I"
n.t=n.t+1


#####################################################
# Fifth set of checks:
# Now check sample size calculations for certain values on RD scale. 
set.seed(1)
out5A<-try(type1error.NIfrontier.binary(0.1, NI.frontier.RD, sig.level = 0.025, n.control=100, n.experim=100, 
                                   summary.measure = "RD", print.out = TRUE, 
                                   unfavourable=T))
correct[[n.t]]<-ifelse((inherits(out5A,"numeric"))&&(all.equal(out5A,2.6)),1,0)  
names(correct)[[n.t]]<-"out5A"
n.t=n.t+1
NI.frontier.RR<-function(p) {
  p0.expected<-0.1
  NI.marg=0.1
  RR<-(p0.expected+NI.marg)/p0.expected
  
  return(p*RR-p)
} 
set.seed(1)
out5B<-try(type1error.NIfrontier.binary(0.1,  NI.frontier.RR, sig.level = 0.025, n.control=100, n.experim=100, 
                                   summary.measure = "RD", print.out = TRUE, 
                                   unfavourable=T))
correct[[n.t]]<-ifelse((inherits(out5B,"numeric"))&&(all.equal(out5B,1.3)),1,0)  
names(correct)[[n.t]]<-"out5B"
n.t=n.t+1
NI.frontier.AS<-function(x) {
  NI.marg<-0.1
  p0.expected<-0.12
  AS<-as.numeric(convertmargin.binary(p0.expected, NI.marg,"RD", "AS"))
  
  y<-convertmargin.binary(x, AS,"AS", "RD")
  return(y)
}
set.seed(1)
out5C<-try(type1error.NIfrontier.binary(0.1,   NI.frontier.AS, sig.level = 0.025, n.control=100, n.experim=100, 
                                   summary.measure = "RD", print.out = TRUE, 
                                   unfavourable=T))
correct[[n.t]]<-ifelse((inherits(out5C,"numeric"))&&(all.equal(out5C,1.4)),1,0)  
names(correct)[[n.t]]<-"out5C"
n.t=n.t+1
set.seed(1)
out5D<-try(type1error.NIfrontier.binary(0.1, NI.frontier.RD, sig.level = 0.025, n.control=100, n.experim=100, 
                                   summary.measure = "RD", print.out = TRUE, 
                                   unfavourable=T))
correct[[n.t]]<-ifelse((inherits(out5D,"numeric"))&&(all.equal(out5D,2.6)),1,0)  
names(correct)[[n.t]]<-"out5D"
n.t=n.t+1
set.seed(1)
out5E<-try(type1error.NIfrontier.binary(0.5, NI.frontier.RD, sig.level = 0.025, n.control=100, n.experim=100, 
                                   summary.measure = "RD", print.out = TRUE,
                                   unfavourable=T))
correct[[n.t]]<-ifelse((inherits(out5E,"numeric"))&&(all.equal(out5E,3.8)),1,0)  
names(correct)[[n.t]]<-"out5E"
n.t=n.t+1
set.seed(1)
out5F<-try(type1error.NIfrontier.binary(0.1,   NI.frontier.RD, sig.level = 0.05, n.control=100, n.experim=100, 
                                   summary.measure = "RD", print.out = TRUE, 
                                   unfavourable=T))
correct[[n.t]]<-ifelse((inherits(out5F,"numeric"))&&(all.equal(out5F,5.5)),1,0)  
names(correct)[[n.t]]<-"out5F"
n.t=n.t+1
set.seed(1)
out5G<-try(type1error.NIfrontier.binary(0.1,   NI.frontier.RD, sig.level = 0.025, n.control=30, n.experim=30, 
                                   summary.measure = "RD", print.out = TRUE, test.type="Newcombe10",
                                   unfavourable=T))
correct[[n.t]]<-ifelse((inherits(out5G,"numeric"))&&(all.equal(out5G,1.5)),1,0)  
names(correct)[[n.t]]<-"out5G"
n.t=n.t+1
set.seed(1)
out5H<-try(type1error.NIfrontier.binary(0.1,   NI.frontier.RD, sig.level = 0.025, n.control=100, n.experim=50, 
                                   summary.measure = "RD", print.out = TRUE, test.type="Newcombe10", 
                                   unfavourable=T))
correct[[n.t]]<-ifelse((inherits(out5H,"numeric"))&&(all.equal(out5H,1.6)),1,0)  
names(correct)[[n.t]]<-"out5H"
n.t=n.t+1
set.seed(1)
out5I<-try(type1error.NIfrontier.binary(0.1,   NI.frontier.RD, sig.level = 0.025, n.control=50, n.experim=100, 
                                   summary.measure = "RD", print.out = TRUE, test.type="Newcombe10", 
                                   unfavourable=T))
correct[[n.t]]<-ifelse((inherits(out5I,"numeric"))&&(all.equal(out5I,2.4)),1,0)  
names(correct)[[n.t]]<-"out5I"
n.t=n.t+1
NI.frontier.RDf<-function(p) return(-0.1)
set.seed(1)
out5J<-try(type1error.NIfrontier.binary(0.2,  NI.frontier.RDf, sig.level = 0.025, n.control=100, n.experim=100, 
                                   summary.measure = "RD", print.out = TRUE, test.type="Newcombe10",
                                   unfavourable=F))
correct[[n.t]]<-ifelse((inherits(out5J,"numeric"))&&(all.equal(out5J,3.4)),1,0)  
names(correct)[[n.t]]<-"out5J"
n.t=n.t+1
set.seed(1)
out5K<-try(type1error.NIfrontier.binary(0.05, NI.frontier.RD, sig.level = 0.025, n.control=100, n.experim=100, 
                                   summary.measure = "RD", print.out = TRUE, test.type="Newcombe10",
                                   unfavourable=T))
correct[[n.t]]<-ifelse((inherits(out5K,"numeric"))&&(all.equal(out5K,1.4, tolerance=10^(-5))),1,0)  
names(correct)[[n.t]]<-"out5K"
n.t=n.t+1
set.seed(1)
out5L<-try(type1error.NIfrontier.binary(0.09,   NI.frontier.RD, sig.level = 0.025, n.control=100, n.experim=100, 
                                   summary.measure = "RD", print.out = TRUE, 
                                   unfavourable=T))
correct[[n.t]]<-ifelse((inherits(out5L,"numeric"))&&(all.equal(out5L,2.2, tolerance=10^(-5))),1,0)  
names(correct)[[n.t]]<-"out5L"
n.t=n.t+1
#####################################################
# Sixth set of checks:
# Now check sample size calculations for certain values on OR scale. 

NI.frontier.ratio<-function(p) return(1.5)
set.seed(1)
out6A<-try(type1error.NIfrontier.binary(0.1,   NI.frontier.ratio, sig.level = 0.025, n.control=100, n.experim=100, 
                                   summary.measure = "OR", print.out = TRUE, 
                                   unfavourable=T))
correct[[n.t]] <- ifelse((is.vector(out6A)) && (all.equal(out6A, 2.5)) , 1, 0)  
names(correct)[[n.t]] <- "out6A"
n.t <- n.t + 1
set.seed(1)
out6B<-try(type1error.NIfrontier.binary(0.12,  NI.frontier.ratio, sig.level = 0.025, n.control=100, n.experim=100, 
                                   summary.measure = "OR", print.out = TRUE,
                                   unfavourable=T))
correct[[n.t]] <- ifelse((is.vector(out6B)) && (all.equal(out6B, 2.4)) , 1, 0)  
names(correct)[[n.t]] <- "out6B"
n.t <- n.t + 1
set.seed(1)
out6C<-try(type1error.NIfrontier.binary(0.5,  NI.frontier.ratio, sig.level = 0.025, n.control=100, n.experim=100, 
                                   summary.measure = "OR", print.out = TRUE, 
                                   unfavourable=T))
correct[[n.t]] <- ifelse((is.vector(out6C)) && (all.equal(out6C, 3.8)) , 1, 0)  
names(correct)[[n.t]] <- "out6C"
n.t <- n.t + 1
set.seed(1)
out6D<-try(type1error.NIfrontier.binary(0.1,   NI.frontier.ratio, sig.level = 0.05, n.control=100, n.experim=100, 
                                   summary.measure = "OR", print.out = TRUE, 
                                   unfavourable=T))
correct[[n.t]] <- ifelse((is.vector(out6D)) && (all.equal(out6D, 4.5)) , 1, 0)  
names(correct)[[n.t]] <- "out6D"
n.t <- n.t + 1
set.seed(1)
out6E<-try(type1error.NIfrontier.binary(0.1,   NI.frontier.ratio, sig.level = 0.025, n.control=30, n.experim=30, 
                                   summary.measure = "OR", print.out = TRUE, test.type="adjusted.Wald.Woolf",
                                   unfavourable=T))
correct[[n.t]] <- ifelse((is.vector(out6E)) && (all.equal(out6E, 0.6)) , 1, 0)  
names(correct)[[n.t]] <- "out6E"
n.t <- n.t + 1
set.seed(1)
out6F<-try(type1error.NIfrontier.binary(0.1,   NI.frontier.ratio, sig.level = 0.025, n.control=100, n.experim=50, 
                                   summary.measure = "OR", print.out = TRUE, test.type="adjusted.Wald.Woolf", 
                                   unfavourable=T))
correct[[n.t]] <- ifelse((is.vector(out6F)) && (all.equal(out6F, 0.9)) , 1, 0)  
names(correct)[[n.t]] <- "out6F"
n.t <- n.t + 1
set.seed(1)
out6G<-try(type1error.NIfrontier.binary(0.1,   NI.frontier.ratio, sig.level = 0.025, n.control=50, n.experim=100, 
                                   summary.measure = "OR", print.out = TRUE, test.type="adjusted.Wald.Woolf", 
                                   unfavourable=T))
correct[[n.t]] <- ifelse((is.vector(out6G)) && (all.equal(out6G, 3)) , 1, 0)  
names(correct)[[n.t]] <- "out6G"
n.t <- n.t + 1
set.seed(1)
NI.frontier.ratiof<-function(p) return(0.5)
out6H<-try(type1error.NIfrontier.binary(0.1,   NI.frontier.ratiof, sig.level = 0.025, n.control=100, n.experim=100, 
                                   summary.measure = "OR", print.out = TRUE, test.type="adjusted.Wald.Woolf",
                                   unfavourable=F))
correct[[n.t]] <- ifelse((is.vector(out6H)) && (all.equal(out6H, 3.4)) , 1, 0)  
names(correct)[[n.t]] <- "out6H"
n.t <- n.t + 1

#####################################################
# Seventh set of checks:
# Check results for RR and AS. These are just compared against known results from our paper on NI frontiers:
set.seed(1)
out7A<-try(type1error.NIfrontier.binary(0.05,  NI.frontier.ratio, sig.level = 0.025, n.control=100, n.experim=100, 
                                   summary.measure = "RR", print.out = TRUE, test.type="adjusted.Wald.Katz",
                                   unfavourable=T))
correct[[n.t]] <- ifelse((is.vector(out7A)) && (all.equal(out7A, 1.4)) , 1, 0)  
names(correct)[[n.t]] <- "out7A"
n.t <- n.t + 1
set.seed(1)
out7B<-try(type1error.NIfrontier.binary(0.05,  NI.frontier.RD, sig.level = 0.025, n.control=100, n.experim=100, 
                                   summary.measure = "AS", print.out = TRUE, test.type="logistic", 
                                   unfavourable=T))
correct[[n.t]] <- ifelse((is.vector(out7B)) && (all.equal(out7B, 2.1)) , 1, 0)  
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

t1e.NIf.b<-(tot.correct==number.of.tests) 


