# ILS_SSP-NPM
This repository provides .R-files of several construction heuristics (IEACT, IGI, RAND) 
as well as an iterated local search algorithm (ILS) for the job sequencing and tool switching problem (SSP) 
with non-identical parallel machines (NPM). 

## Abstract
*This work addresses the generalisation of the NP-hard job sequencing and tool switching problem with non-identical parallel machines 
and sequence-dependent setup times where a set of jobs is to be scheduled on a set of unrelated parallel machines with machine-dependent processing and tool switching times. 
Different construction heuristics and iterated local search schemes are presented for three different objectives and are applied to a set of newly generated test instances, publicly available. 
The instances are compared and analysed using the IBM CPLEX solver and the results of different iterated local search heuristics.*

## Authors
[**D. Calmels**](https://www.researchgate.net/profile/Dorothea_Calmels)

## Test instances
The problem instances and an explanation are provided [here](https://github.com/TerhiS/ILS_SSP-NPM/tree/master/Heuristics/instances).	

# Iterated Local Search
An Iterated Local Search (ILS) heuristic and its components are provided [here](https://github.com/TerhiS/ILS_SSP-NPM/tree/master/Heuristics).
The ILS consists of the [construction heuristics](https://github.com/TerhiS/ILS_SSP-NPM/tree/master/Heuristics/construction_heuristics) to build the initial solution, 
the [local search](https://github.com/TerhiS/ILS_SSP-NPM/tree/master/Heuristics/local_search) to obtain the local optimum 
and the perturbation concepts of the [iterated local search](https://github.com/TerhiS/ILS_SSP-NPM/tree/master/Heuristics/local_search) in order to overcome local optima. 
The specific [README](https://github.com/TerhiS/ILS_SSP-NPM/blob/master/Heuristics/ILS/README_ILS.md) in the ILS folder contains detailed information about the ILS heuristic. 
## Built With
[RStudio](https://rstudio.com/products/rstudio/download/) - Integrated Development Environment
## Solved With
[R](https://www.r-project.org/) - Programming Language

# MIP Model
## Content
A MIP model is provided on [Math_Model](https://github.com/TerhiS/ILS_SSP-NPM/tree/master/Math_Model). 
## Built With
[GAMS IDE](https://www.gams.com/download/) - Integrated Development Environment
## Solved With
[IBM ILOG CPLEX](https://www.ibm.com/de-de/products/ilog-cplex-optimization-studio) - Solver
```
Note: A valid CPLEX licence is required!
```
