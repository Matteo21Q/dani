##################################
#### Main testing file for    ####
#### all functions in the dani####
#### package. 13-12-2023      ####
##################################

# CHeck if necessary packages are all installed and load them:
list.of.packages <- c("mfp", "marginaleffects", "survival", "coxphw",
                      "survRM2", "DescTools", "boot", "flexsurv", "numDeriv",
                      "mratios", "BSDA", "ratesci", "DescrTab2", "exact2x2",
                      "contingencytables", "PropCIs", "tibble")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

# Install and download package or source all files

if (packageVersion('dani')!='1.0.0') {
  library(devtools)
  install_github("Matteo21Q/dani", build_vignettes = TRUE, force=TRUE)
}
library(dani)

# setwd("C:/Users/rmjlmqu/GitHub/dani/R")
# source("convertmargin.binary.R")
# source("convertmargin.continuous.R")
# source("convertmargin.survival.R")
# source("Auxiliary functions.R")
# source("compare.NIfrontier.binary.R")
# source("compare.NIfrontier.continuous.R")
# source("compare.NIfrontier.survival.R")
# source("plot.ROCI.R")
# source("samplesize.ROCI.binary.R")
# source("samplesize.NIfrontier.binary.R")
# source("samplesize.NI.binary.R")
# source("samplesize.NI.continuous.R")
# source("samplesize.NI.survival.R")
# source("power.ROCI.binary.R")
# source("power.NIfrontier.binary.R")
# source("power.NI.binary.R")
# source("power.NI.continuous.R")
# source("type1error.NIfrontier.binary.R")
# source("summary.ROCI.R")
# source("test.ROCI.binary.R")
# source("test.NIfrontier.binary.R")
# source("test.NI.binary.R")
# source("test.NI.continuous.R")
# source("test.NI.survival.R")

# Run all scripts
setwd("C:/Users/rmjlmqu/GitHub/dani/tests")
source("convertmargin.binary testing file.R")
source("convertmargin.continuous testing file.R")
source("convertmargin.survival testing file.R")
source("compare.NIfrontier.binary testing file.R")
source("compare.NIfrontier.continuous testing file.R")
source("compare.NIfrontier.survival testing file.R")
source("samplesize.NI.binary testing file.R")
source("samplesize.NI.continuous testing file.R")
source("samplesize.NI.survival testing file.R")
source("samplesize.NIfrontier.binary testing file.R")
source("samplesize.ROCI.binary testing file.R")
source("power.NI.binary testing file.R")
source("power.NI.continuous testing file.R")
source("power.NI.survival testing file.R")
source("power.NIfrontier.binary testing file.R")
source("power.ROCI.binary testing file.R")
source("test.NI.binary testing file.R")
source("test.NI.continuous testing file.R")
source("test.NI.survival testing file.R")
source("test.NIfrontier.binary testing file.R")
source("test.ROCI.binary testing file.R")
source("type1error.NIfrontier.binary testing file.R")

results<-c(cm.b, cm.c, cm.s, c.NIf.b, c.NIf.c, c.NIf.s, ss.NI.b, ss.NI.c, ss.NI.s, ss.NIf.b,
           ss.ROCI.b, t.NI.b, t.NI.c, t.NI.s, t.NIf.b, t.ROCI.b, pw.NI.b, pw.NI.c, pw.NIf.b, 
           pw.ROCI.b, t1e.NIf.b)
name.results<-c("convertmargin.binary", "convertmargin.continuous", "convertmargin.survival",
                "compare.NIfrontier.binary", "compare.NIfrontier.continuous", 
                "compare.NIfrontier.survival", "samplesize.NI.binary", 
                "samplesize.NI.continuous", "samplesize.NI.survival", 
                "samplesize.NIfrontier.binary", "samplesize.ROCI.binary", 
                "test.NI.binary", "test.NI.continuous", "test.NI.survival", 
                "test.NIfrontier.binary", "test.ROCI.binary",
                "power.NI.binary", "power.NI.continuous", "power.NIfrontier.binary",
                "power.ROCI.binary", "type1error.NIfrontier.binary")

if (all(results)) {
  cat("All testing files returned correct results.\n")
} else {
  cat("The following testing files returned some errors:\n", 
      name.results[which(results==FALSE)], ".\n")
  
}