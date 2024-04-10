###################################################
### test.NI.continuous           testing file #####
### 25-07-2023                                #####
###################################################

# Load dani:
# library(dani)
library(mratios)
library(boot)
library(DescTools)
library(BSDA)

###############################################################################################################################################
### Here we test the test.NI.continuous function

#Initialise vector of outputs 
correct<-list(NULL)
n.t<-1

#####################################################
# First set of checks:
# Check that it stops for non acceptable values of y:

out1A<-try(test.NI.continuous("100", rnorm(10), -1))
correct[[n.t]]<-ifelse((inherits(out1A, "try-error"))&&(grepl("is.numeric(y.control) is not TRUE", out1A[1], fixed=T  )),1,0) 
names(correct)[[n.t]]<-"out1A"
n.t=n.t+1
out1B<-try(test.NI.continuous(rnorm(10), "100", -1))
correct[[n.t]]<-ifelse((inherits(out1B, "try-error"))&&(grepl("is.numeric(y.experim) is not TRUE", out1B[1], fixed=T  )),1,0) 
names(correct)[[n.t]]<-"out1B"
n.t=n.t+1
out1C<-try(test.NI.continuous(100, rnorm(10), -1))
correct[[n.t]]<-ifelse((inherits(out1C, "try-error"))&&(grepl("length(y.control) > 1 is not TRUE", out1C[1], fixed=T )),1,0) 
names(correct)[[n.t]]<-"out1C"
n.t=n.t+1
out1D<-try(test.NI.continuous(rnorm(10), 100, -1))
correct[[n.t]]<-ifelse((inherits(out1D, "try-error"))&&(grepl("length(y.experim) > 1 is not TRUE", out1D[1], fixed=T  )),1,0) 
names(correct)[[n.t]]<-"out1D"
n.t=n.t+1

#####################################################
# Second set of checks:
# Check that it stops for non acceptable values of NI margins:
out2A<-try(test.NI.continuous(rnorm(10), rnorm(10), "1"))
correct[[n.t]]<-ifelse((inherits(out2A, "try-error"))&&(grepl("is.numeric(NI.margin) is not TRUE", out2A[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out2A"
n.t=n.t+1
out2B<-try(test.NI.continuous(rnorm(10), rnorm(10), 1))
correct[[n.t]]<-ifelse((inherits(out2B, "try-error"))&&(grepl("When higher values of the outcome are better, a mean difference NI margin needs to be negative.", out2B[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out2B"
n.t=n.t+1
out2C<-try(test.NI.continuous(rnorm(10), rnorm(10), -1, higher.better=F))
correct[[n.t]]<-ifelse((inherits(out2C, "try-error"))&&(grepl("When lower values of the outcome are better, a mean difference NI margin needs to be positive.", out2C[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out2C"
n.t=n.t+1
out2D<-try(test.NI.continuous(rnorm(10), rnorm(10), 1.2, summary.measure="mean.ratio"))
correct[[n.t]]<-ifelse((inherits(out2D, "try-error"))&&(grepl("When outcome is such that higher values are better, a NI margin on the mean ratio scale needs to be <1.", out2D[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out2D"
n.t=n.t+1
out2E<-try(test.NI.continuous(rnorm(10), rnorm(10), 0.8, summary.measure="mean.ratio", higher.better=F))
correct[[n.t]]<-ifelse((inherits(out2E, "try-error"))&&(grepl("When outcome is such that higher values are worse, a NI margin on the mean ratio scale needs to be >1.", out2E[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out2E"
n.t=n.t+1

#####################################################
# Third set of checks:
# Check that it stops for unacceptable values of significance level:

out3A<-try(test.NI.continuous(rnorm(10), rnorm(10), -1, sig.level="0.025"))
correct[[n.t]]<-ifelse((inherits(out3A, "try-error"))&&(grepl("is.numeric(sig.level) is not TRUE", out3A[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out3A"
n.t=n.t+1
out3B<-try(test.NI.continuous(rnorm(10), rnorm(10), -1, sig.level=-0.1))
correct[[n.t]]<-ifelse((inherits(out3B, "try-error"))&&(grepl("sig.level > 0 is not TRUE", out3B[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out3B"
n.t=n.t+1
out3C<-try(test.NI.continuous(rnorm(10), rnorm(10), -1, sig.level=0.7))
correct[[n.t]]<-ifelse((inherits(out3C, "try-error"))&&(grepl("sig.level < 0.5 is not TRUE", out3C[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out3C"
n.t=n.t+1

#####################################################
# Fourth set of checks:
# Check with other wrong arguments:

# Check that it work for wrong summary measure value:
out4A<-try(test.NI.continuous(rnorm(10), rnorm(10), -1, summary.measure = "pippo"))
correct[[n.t]]<-ifelse((inherits(out4A, "try-error"))&&(grepl("summary.measure ==", out4A[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4A"
n.t=n.t+1

# Check that it works when print.out incorrectly specified:
out4B<-try(test.NI.continuous(rnorm(10), rnorm(10), -1,  print.out = NA))
correct[[n.t]]<-ifelse((inherits(out4B, "try-error"))&&(grepl("!is.na(print.out) is not TRUE", out4B[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4B"
n.t=n.t+1

# Check that it works when test.type incorrectly specified:
out4C<-try(test.NI.continuous(rnorm(10), rnorm(10), -1,  test.type = "pippo"))
correct[[n.t]]<-ifelse((inherits(out4C, "try-error"))&&(grepl("test.type %in%", out4C[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4C"
n.t=n.t+1

# Check that it works when higher.better incorrectly specified:
out4D<-try(test.NI.continuous(rnorm(10), rnorm(10), -1,  higher.better = "pippo"))
correct[[n.t]]<-ifelse((inherits(out4D, "try-error"))&&(grepl("is.logical(higher.better) is not TRUE", out4D[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4D"
n.t=n.t+1

# Check that M.boot has acceptable value:
out4E<-try(test.NI.continuous(rnorm(10), rnorm(10), -1, test.type="bootstrap.basic", M.boot="2000"))
correct[[n.t]]<-ifelse((inherits(out4E, "try-error"))&&(grepl("is.numeric(M.boot) is not TRUE", out4E[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4E"
n.t=n.t+1
out4F<-try(test.NI.continuous(rnorm(10), rnorm(10), -1, test.type="bootstrap.basic", M.boot=1))
correct[[n.t]]<-ifelse((inherits(out4F, "try-error"))&&(grepl("M.boot > 1 is not TRUE", out4F[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4F"
n.t=n.t+1

# Check that sd have acceptable value:
out4G<-try(test.NI.continuous(rnorm(10), rnorm(10), -1, test.type="Z.test", sd.control="0", sd.experim = 2))
correct[[n.t]]<-ifelse((inherits(out4G, "try-error"))&&(grepl("is.numeric(sd.control) is not TRUE", out4G[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4G"
n.t=n.t+1
out4H<-try(test.NI.continuous(rnorm(10), rnorm(10), -1, test.type="Z.test", sd.control=0, sd.experim = 2))
correct[[n.t]]<-ifelse((inherits(out4H, "try-error"))&&(grepl("sd.control > 0 is not TRUE", out4H[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4H"
n.t=n.t+1
out4I<-try(test.NI.continuous(rnorm(10), rnorm(10), -1, test.type="Z.test", sd.control=2, sd.experim = "2"))
correct[[n.t]]<-ifelse((inherits(out4I, "try-error"))&&(grepl("is.numeric(sd.experim) is not TRUE", out4I[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4I"
n.t=n.t+1
out4J<-try(test.NI.continuous(rnorm(10), rnorm(10), -1, test.type="Z.test", sd.control=2, sd.experim = 0))
correct[[n.t]]<-ifelse((inherits(out4J, "try-error"))&&(grepl("sd.experim > 0 is not TRUE", out4J[1] , fixed=T )),1,0) 
names(correct)[[n.t]]<-"out4J"
n.t=n.t+1

#####################################################
# Fifth set of checks:
# Now check tests for certain values on difference scale. These are compared against various comparators
set.seed(1)
out5A<-try(test.NI.continuous(rnorm(10,2), rnorm(10,2), -0.75, test.type="Z.test", sd.control=1, sd.experim = 1))
correct[[n.t]]<-ifelse((inherits(out5A,"list"))&&(all.equal(out5A$CI[2],0.9931647, tolerance = 10^(-5)))&&out5A$non.inferiority==F,1,0) 
names(correct)[[n.t]]<-"out5A"
n.t=n.t+1
set.seed(1)
out5B<-try(test.NI.continuous(rnorm(10,2), rnorm(10,2), -1, test.type="Z.test", sd.control=1, sd.experim = 1))
correct[[n.t]]<-ifelse((inherits(out5B,"list"))&&(all.equal(out5B$CI[2],0.9931647, tolerance=10^(-6))),1,0) 
names(correct)[[n.t]]<-"out5B"
n.t=n.t+1
set.seed(1)
out5C<-try(test.NI.continuous(rnorm(10,2), rnorm(10,2), -1, test.type="t.test"))
correct[[n.t]]<-ifelse((inherits(out5C,"list"))&&(all.equal(out5C$CI[2],1.002217, tolerance=10^(-6)))&&out5C$non.inferiority==T,1,0) 
names(correct)[[n.t]]<-"out5C"
n.t=n.t+1
set.seed(1)
out5D<-try(test.NI.continuous(rnorm(10,2), rnorm(10,2), -1, test.type="t.test", sig.level = 0.05))
correct[[n.t]]<-ifelse((inherits(out5D,"list"))&&(all.equal(out5D$CI[2],0.8463857, tolerance=10^(-6))),1,0) 
names(correct)[[n.t]]<-"out5D"
n.t=n.t+1
set.seed(1)
out5E<-try(test.NI.continuous(rnorm(10,2), rnorm(10,2), 1, higher.better=F, test.type="t.test"))
correct[[n.t]] <- ifelse((inherits(out5E, "list")) && (all.equal(out5E$CI[2], 1.002217, tolerance=10^(-6))), 1, 0) 
names(correct)[[n.t]] <- "out5E"
n.t <- n.t + 1
set.seed(1)
out5F<-try(test.NI.continuous(y.control=rnorm(10,2), y.experim=rnorm(10,2), NI.margin=-1, test.type="bootstrap", bootCI.type = "basic"))
correct[[n.t]] <- ifelse((inherits(out5F, "list")) && (all.equal(out5F$CI[2], 1.017366, tolerance=10^(-6))), 1, 0) 
names(correct)[[n.t]] <- "out5F"
n.t <- n.t + 1
set.seed(1)
out5G<-try(test.NI.continuous(rnorm(10,2), rnorm(10,2), NI.margin=-1, test.type="bootstrap", bootCI.type = "bca"))
correct[[n.t]] <- ifelse((inherits(out5G, "list")) && (all.equal(out5G$CI[2], 0.7991225, tolerance=10^(-6))), 1, 0) 
names(correct)[[n.t]] <- "out5G"
n.t <- n.t + 1
set.seed(1)
out5H<-try(test.NI.continuous(rnorm(10,2), rnorm(10,2), -1, test.type="bootstrap", bootCI.type = "perc"))
correct[[n.t]] <- ifelse((inherits(out5H, "list")) && (all.equal(out5H$CI[2], 0.8371394, tolerance=10^(-6))), 1, 0) 
names(correct)[[n.t]] <- "out5H"
n.t <- n.t + 1
set.seed(1)
dataf<-data.frame(y<-c(rnorm(10,2), rnorm(10,2)), treat<-rep(c(1,0), each=100))
colnames(dataf)<-c("y", "treat")
out5I<-try(test.NI.continuous(data=dataf, formula=as.formula("y~treat(treat)"), NI.margin=-0.75, test.type="Z.test", sd.control=1, sd.experim = 1))
correct[[n.t]]<-ifelse((inherits(out5I,"list"))&&(all.equal(out5I$CI[2],0.2771808, tolerance = 10^(-5)))&&out5I$non.inferiority==T,1,0) 
names(correct)[[n.t]]<-"out5I"
n.t=n.t+1
set.seed(1)
dataf2<-data.frame(y<-c(rnorm(10,2), rnorm(10,2)), treat<-rep(c(1,0), each=100), age=rep(c(20,30,40,50),50))
colnames(dataf2)<-c("y", "treat", "age")
out5J<-try(test.NI.continuous(data=dataf2, formula=as.formula("y~treat(treat)+age"), NI.margin=-0.75, test.type="lm", sd.control=1, sd.experim = 1))
correct[[n.t]]<-ifelse((inherits(out5J,"list"))&&(all.equal(out5J$CI[2],0.2270376, tolerance = 10^(-5)))&&out5J$non.inferiority==T,1,0) 
names(correct)[[n.t]]<-"out5J"
n.t=n.t+1
#####################################################
# Sixth set of checks:
# Now check tests for certain values on ratio scale. 
set.seed(1)
out6A<-try(test.NI.continuous(rnorm(10,2), rnorm(10,2), 0.5, summary.measure="mean.ratio", test.type="Fiellers"))
correct[[n.t]] <- ifelse((is.list(out6A)) && (all.equal(out6A$CI[2], 1.562546, tolerance=10^(-5))) && out6A$non.inferiority, 1, 0) 
names(correct)[[n.t]] <- "out6A"
n.t <- n.t + 1
set.seed(1)
out6B<-try(test.NI.continuous(rnorm(10,2), rnorm(10,2), 0.75, summary.measure="mean.ratio", test.type="Fiellers"))
correct[[n.t]] <- ifelse((is.list(out6B)) && (all.equal(out6B$CI[2], 1.562546, tolerance=10^(-5))) && !out6B$non.inferiority, 1, 0)
names(correct)[[n.t]] <- "out6B"
n.t <- n.t + 1
set.seed(1)
out6C<-try(test.NI.continuous(rnorm(10,2), rnorm(10,2), 0.75, summary.measure="mean.ratio", test.type="Fiellers", sig.level=0.05))
correct[[n.t]] <- ifelse((is.list(out6C)) && (all.equal(out6C$CI[2], 1.457753, tolerance=10^(-5))) && !out6C$non.inferiority, 1, 0)
names(correct)[[n.t]] <- "out6C"
n.t <- n.t + 1
set.seed(1)
out6D<-try(test.NI.continuous(rnorm(10,2), rnorm(10,2), 1.25, summary.measure="mean.ratio", test.type="Fiellers", higher.better=F))
correct[[n.t]] <- ifelse((is.list(out6D)) && (all.equal(out6D$CI[2], 1.562546, tolerance=10^(-6))), 1, 0)
names(correct)[[n.t]] <- "out6D"
n.t <- n.t + 1
set.seed(1)
out6E<-try(test.NI.continuous(y.control=rnorm(10,2), y.experim=rnorm(10,2), NI.margin=0.75, summary.measure="mean.ratio", test.type="lm"))
correct[[n.t]] <- ifelse((is.list(out6E)) && (all.equal(out6E$CI[2], 1.534643, tolerance=10^(-6))), 1, 0)
names(correct)[[n.t]] <- "out6E"
n.t <- n.t + 1
set.seed(1)
out6F<-try(test.NI.continuous(rnorm(10,2), rnorm(10,2), 0.75, summary.measure="mean.ratio", test.type="bootstrap", bootCI.type = "basic"))
correct[[n.t]] <- ifelse((is.list(out6F)) && (all.equal(out6F$CI[2], 1.688022, tolerance=10^(-6))), 1, 0)
names(correct)[[n.t]] <- "out6F"
n.t <- n.t + 1
set.seed(1)
out6G<-try(test.NI.continuous(rnorm(10,2), rnorm(10,2), 0.75, summary.measure="mean.ratio", test.type="bootstrap", bootCI.type = "bca"))
correct[[n.t]] <- ifelse((is.list(out6G)) && (all.equal(out6G$CI[2], 1.41891, tolerance=10^(-6))), 1, 0) 
names(correct)[[n.t]] <- "out6G"
n.t <- n.t + 1
set.seed(1)
out6H<-try(test.NI.continuous(rnorm(10,2), rnorm(10,2), 0.75, summary.measure="mean.ratio", test.type="bootstrap", bootCI.type = "perc"))
correct[[n.t]] <- ifelse((is.list(out6H)) && (all.equal(out6H$CI[2], 1.462579, tolerance=10^(-6))), 1, 0) 
names(correct)[[n.t]] <- "out6H"
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

t.NI.c<-(tot.correct==number.of.tests) 


