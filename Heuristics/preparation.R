# =======================================
# file and package preparation
# =======================================

### packages
# install required packages if not installed
list.of.packages <- c("naturalsort", "tictoc","R.utils","matrixStats","foreach","parallel","doParallel")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# load the required packages
library(matrixStats)
library(R.utils)
library(tictoc)
library("naturalsort")
library(foreach)
library(parallel)
library(doParallel)

# additional preparations
numCores <- detectCores()
registerDoParallel(numCores)

### functions
# required function that includes sample of length one
resamp <- function(x,...){if(length(x)==1) x else sample(x,...)} 

### instances
# data available on https://github.com/TerhiS/ILS_SSP-NPM/Heuristics/instances
# note: there are two different data sets: SSP-NPM-I and SSP-NPM-II
# default folder: "example_instance/"
# if required change to "SSP-NPM-I/" or "SSP-NPM-II/"
setwd("Instances/example_instance")
#setwd("Instances/SSP-NPM-I/")
#setwd("Instances/SSP-NPM-II/")
csv_files <- list.files(pattern = 'ins*',all.files = T,full.names = T)
csv_files <- naturalsort(csv_files)
data.list <- lapply(csv_files,read.table,sep=";",header=F)
setwd("~/GitHub/ILS_SSP-NPM/Heuristics")
# =======================================
# END preparation
# =======================================
