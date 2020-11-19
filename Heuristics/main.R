# =======================================
# Title: Main-file // ILS for the SSP-NPM
# =======================================

# =======================================
# Author: D. Calmels
# =======================================

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Description: 
# This file recomposes several sub-files required for the ILS heuristic
# It consists of
#  - preparation.R
#  - construction_heuristic.R
#  - ils.R
#
#
# It generates several output files based on the %objective% (makespan / total flowtime) and %instance%-identifier
# File "bks_%objective%%instance%.R" contains the best known results (bks) obtained per perturbation move applied
# File "ILS_100_%objevtive%%instance%.R" contains the results after 100 perturbation moves 
#      - the objective values (switch_opt: tool switches, tft_opt: total flowtime, fmax_opt: makespan) 
#      - the elapsed run time (in seconds)
#      - the best known job sequence on each machine (seq_opt)
#      - the tool loading (loads_opt)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# set or create working directory 
if(dir.exists(paths = "~/GitHub/ILS_SSP-NPM/Heuristics") == T){
  setwd("~/GitHub/ILS_SSP-NPM/Heuristics")
} else {
  dir.create(path = "~/GitHub/ILS_SSP-NPM/Heuristics")
  setwd("~/GitHub/ILS_SSP-NPM/Heuristics")
}

# install and load packages and include instances in data.list 
# the default instance folder is "example_instance/"
# if required change the folder in "preparation.R" to "SSP-NPM-I/" or "SSP-NPM-II/"
source("preparation.R",echo = F)

# run  all construction heuristics (IEACT, IGI, MSR)
# Note: The output of the construction heuristic %heuristic% applied to each %instance% 
#       will be stored in a separate file: %heuristic%%instance%.csv. 
#       The file is required for running the ILS.
#       Changes can be made to use the contruction heuristic and the ILS together.
#       Therefore, remove the "Read Input Data"-part in ILS.R and replace it with the output variables of constr_heur.R.
# The construction heuristics can be run individually 
source("construction_heuristics/IEACT.R",echo = F)
source("construction_heuristics/IGI.R",echo = F)
source("construction_heuristics/MSR.R",echo = F)

# run iterated local search
source("iterated_local_search/ILS.R",echo = F)
# Note: The ILS heuristic applied to each %instance% generated two output files!
# Note: the Local Search can be run individually
source("local_search/LS.R")

