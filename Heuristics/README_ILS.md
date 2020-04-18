# Iterated Local Search (ILS)

An Iterated Local Search (ILS) heuristic and its components are provided [here](https://github.com/TerhiS/ILS_SSP-NPM/tree/master/Heuristics).
The ILS consists of the [construction heuristics](https://github.com/TerhiS/ILS_SSP-NPM/tree/master/Heuristics/construction_heuristics) to build the initial solution, 
the [local search](https://github.com/TerhiS/ILS_SSP-NPM/tree/master/Heuristics/local_search) to obtain the local optimum 
and the perturbation concepts of the [iterated local search](https://github.com/TerhiS/ILS_SSP-NPM/tree/master/Heuristics/local_search) in order to overcome local optima. 
Three different objectives are considered, namely minimizing the makespan, the total flowtime, the total number of tool switches. 

## Authors
[**D. Calmels**](https://www.researchgate.net/profile/Dorothea_Calmels)
## Test instances
The problem instances and an explanation are provided [here](https://github.com/TerhiS/ILS_SSP-NPM/tree/master/Heuristics/instances).	

## Content
### Construction Heuristic
The [construction heuristics](https://github.com/TerhiS/ILS_SSP-NPM/tree/master/Heuristics/construction_heuristics) iteratively assign jobs to machines. 
Three different construction heuristics are provided: 
- IEACT
- IGI
- MSR

### Iterated Local Search
The [iterated local search](https://github.com/TerhiS/ILS_SSP-NPM/blob/master/Heuristics/iterated_local_search/ILS.R) consists of a local search (the stand-alone local search is provided [here](https://github.com/TerhiS/ILS_SSP-NPM/tree/master/Heuristics/local_search) )
based on job swaps as well as three different perturbation schemes: 
- problem-specific perturbation (prob_spec): re-assigns jobs in unfavourable positions to the best position on another machine, 
- random perturbation (random): re-assigns randomly picked jobs to a random position on another machine, 
- combined perturbation (combi): a combination of the problem-specific perturbation and the random perturbation. 

## Usage Description
The file [main.R](https://github.com/TerhiS/ILS_SSP-NPM/blob/master/Heuristics/main.R) provides the main body of the ILS. 
Use this file only unless you would like to run the files or different versions of the files individually. 
The [main.R](https://github.com/TerhiS/ILS_SSP-NPM/blob/master/Heuristics/main.R) file recomposes several sub-files required for the ILS heuristic
It consists of: 
- [preparation.R](https://github.com/TerhiS/ILS_SSP-NPM/blob/master/Heuristics/preparation.R)
- [construction heuristics](https://github.com/TerhiS/ILS_SSP-NPM/tree/master/Heuristics/construction_heuristics): 
	- [IEACT.R](https://github.com/TerhiS/ILS_SSP-NPM/blob/master/Heuristics/construction_heuristics/IEACT.R)
	- [IGI.R](https://github.com/TerhiS/ILS_SSP-NPM/blob/master/Heuristics/construction_heuristics/IGI.R)
	- [MSR.R](https://github.com/TerhiS/ILS_SSP-NPM/blob/master/Heuristics/construction_heuristics/MSR.R)
- [ILS.R](https://github.com/TerhiS/ILS_SSP-NPM/blob/master/Heuristics/iterated_local_search/ILS.R)

It generates several output files based on the *%perturbation%* (combi / prob_spec / random), the %objective%* (makespan / flowtime / switches) and *%instance%*-identifier. 

## Output
The Algorithms generate several output files. (see the [example results](https://github.com/TerhiS/MIP_SSP-NPM/tree/master/Heuristics/results/example_results) )
Each output file is characterized by the *%method%* (IEACT / IGI / MSR / LS / ILS) and the instance identifier (*%instance%*__.csv__). 
The output file of the ILS is additionally characterized by the *%perturbation%* scheme (combi / prob_spec / random) as well as the *%objective%* considered (makespan / flowtime / switches). 
Separator: ";"

1) __%method%__*%instance%*__.csv__: results of the construction heuristic 
2) __ILS__\_*%iterations%*\_*%objective%*_%instance%_**.csv**: results of the ILS
3) __bks__\_*%objective%*_%instance%_**.csv**: best known results per iteration for a considered objective

The files 1) and 2) contain the following information: 
- *row 1*: total number of tool switches of the sequence generated 
- *row 2*: total flowtime of the sequence generated 
- *row 3*: makespan of the sequence generated
- *row 4*: elapsed computation time in seconds
- *row 5*: generated job sequence per machine denoted as vector c(...): machine1(job1, job2, ...) machine2(job1, job2, ...)
- *row 6*: tool loading of the generated sequence per job denoted as vector c(...): job1(tool1, tool2, ...) job2(tool1, tool2, ...)

The file 3) contains the following information per iteration: 
- *column 1*: iteration count
- *column 2*: total number of tool switches for the current best sequence
- *column 3*: total flowtime for the current best sequence
- *column 4*: makespan for the current best sequence
- *column 5*: elapsed computation time after the current iteration

## Built With
[RStudio](https://rstudio.com/products/rstudio/download/) - Integrated Development Environment

## Solved With
[R](https://www.r-project.org/) - Programming Language