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
# data available on http://dx.doi.org/10.17632/ggr36f5gd5.2
# note: the files must be in the working directory! Otherwise, change working directory accordingly!
setwd("example_data/")
csv_files <- list.files(pattern = 'ins*',all.files = T,full.names = T)
csv_files <- naturalsort(csv_files)
data.list <- lapply(csv_files,read.table,sep=";",header=F)
setwd("~/GitHub/MIP_SSP-NPM/ILS")
# =======================================
# END preparation
# =======================================
