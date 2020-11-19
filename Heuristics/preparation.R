# =======================================
# file and package preparation
# =======================================

### packages
# install required packages if not installed
list.of.packages <-
  c(
    "naturalsort",
    "tictoc",
    "R.utils",
    "matrixStats",
    "foreach",
    "parallel",
    "doParallel"
  )
new.packages <-
  list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]
if (length(new.packages))
  install.packages(new.packages)

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
resamp <- function(x, ...) {
  if (length(x) == 1)
    x
  else
    sample(x, ...)
}

# Output preparation
ConvertColonToVector <- function(myList) {
  for (i in length(myList)) {
    if (grepl(":", toString(myList[i]), fixed = T)) {
      # create new entry starting at desired value
      newEntry <- c(myList[[i]][1])
      # add all necessary values to new entry
      for (z in 1:length(myList[[i]]) - 1) {
        newEntry <- c(newEntry, myList[[i]][z] + 1)
      }
      # assign new entry
      myList[[i]] <- newEntry
    }
  }
  return(myList)
}

# Import Results from Construction Heuristic
source("New_Import_HeuristicResults.R",echo = F)

# KTNS
source("NewKTNS.R",echo = F)

# computation of tool load and switches
source("NewLoadingResults.R",echo = F)

# Local Search
source("New_Local_Search.R",echo = F)

# Random Perturbation
source("New_Random_Perturbation.R",echo = F)

# Problem Specific Perturbation
source("New_ProblemSpecific_Perturbation.R",echo = F)

### instances
# data available on https://github.com/TerhiS/ILS_SSP-NPM/Heuristics/instances
# note: there are two different data sets: SSP-NPM-I and SSP-NPM-II
# default folder: "example_instance/"
# if required change to "SSP-NPM-I/" or "SSP-NPM-II/"
ProblemSet <- readline(
  prompt = "Enter Problem Set (SSP-NPM-I, SSP-NPM-II, example_instance): ")
setwd(paste0("Instances/",ProblemSet))

csv_files <-
  list.files(pattern = 'ins*',
             all.files = T,
             full.names = T)
csv_files <- naturalsort(csv_files)
data.list <- lapply(csv_files, read.table, sep = ";", header = F)
setwd("~/GitHub/ILS_SSP-NPM/Heuristics")
# =======================================
# END preparation
# =======================================
