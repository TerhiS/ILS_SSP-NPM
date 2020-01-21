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
if(dir.exists(paths = "~/GitHub/ILS_SSP-NPM/ILS") == T){
  setwd("~/GitHub/ILS_SSP-NPM/ILS")
} else {
  dir.create(path = "~/GitHub/ILS_SSP-NPM/ILS")
  setwd("~/GitHub/ILS_SSP-NPM/ILS")
}

# install and load packages and include instances in data.list 
source("preparation.R",echo = F)

# run construction heuristic
source("constr_heur.R",echo = F)
# Note: The output of the construction heuristic applied to each %instance% 
#       will be stored in a separate file: CH%instance%.csv. 
#       The file is required for running the ILS.
#       Changes can be made to use the contruction heuristic and the ILS together.
#       Therefore, remove the "Read Input Data"-part in ILS.R and replace it with the output variables of constr_heur.R.

# run iterated local search
source("ils.R",echo = F)
# Note: The ILS heuristic applied to each %instance% generated two output files!
#       File bks_%heuristic%
