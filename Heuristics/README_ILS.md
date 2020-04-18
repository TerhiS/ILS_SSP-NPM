# Iterated Local Search (ILS)

The ILS consists of the [construction heuristic](https://github.com/TerhiS/MIP_SSP-NPM/blob/master/ILS/constr_heur.R) to build the initial solution the [local search](https://github.com/TerhiS/MIP_SSP-NPM/blob/master/ILS/ils.R) to obtain the local optimum 
and the [perturbation](https://github.com/TerhiS/MIP_SSP-NPM/blob/master/ILS/ils.R) in order to overcome local optima. 

## Acknowledgement 
The models are part of a contribution published in the International Journal of Operational Research 2020, 
published by Inderscience Publishing, Switzerland. 
The definitive authenticated version will be available online via [IJOR/Inderscience Publishing](https://www.inderscience.com/jhome.php?jcode=ijor).

## Authors
[**D. Calmels**](https://www.researchgate.net/profile/Dorothea_Calmels)
## Test instances
The problem instances and an explanation are provided on [Mendeley](http://dx.doi.org/10.17632/ggr36f5gd5.2)

## Content
### Construction Heuristic
The [construction heuristic](https://github.com/TerhiS/MIP_SSP-NPM/blob/master/ILS/constr_heur.R) iteratively assigns jobs to machines. 
In each iteration, the job with the minimum artificial completion time is selected, calculated as
```
processing time of the selected job + (tool switching time * tool set difference of the selected job and the previous job)
```
### Iterated Local Search
The [iterated local search](https://github.com/TerhiS/MIP_SSP-NPM/blob/master/ILS/ils.R) consists of a local search based on job swaps as well as a problem-specific perturbation scheme. 
The perturbation scheme re-assigns jobs in unfavourable positions to the best position on another machine. 

## Usage Description
The file [main.R](https://github.com/TerhiS/MIP_SSP-NPM/blob/master/ILS/main.R) provides the main body of the ILS. 
Use this file only unless you would like to run the files or different versions of the files individually. 
The [main.R](https://github.com/TerhiS/MIP_SSP-NPM/blob/master/ILS/main.R) file recomposes several sub-files required for the ILS heuristic
It consists of: 
- [preparation.R](https://github.com/TerhiS/MIP_SSP-NPM/blob/master/ILS/preparation.R)
- [construction_heuristic.R](https://github.com/TerhiS/MIP_SSP-NPM/blob/master/ILS/constr_heur.R)
- [ils.R](https://github.com/TerhiS/MIP_SSP-NPM/blob/master/ILS/ils.R)

It generates several output files based on the *%objective%* (makespan / total flowtime) and *%instance%*-identifier. 

## Output
The Algorithms generate three output files. (see the [example results](https://github.com/TerhiS/MIP_SSP-NPM/blob/master/ILS/example_results) )
Each output file is characterized by the instance identifier (*%instance%*). 
The output file of the ILS is additionally characterized by the iteration limit (*%iterations%*) as well as the objective considered (*%objective%*). 
Separator: ";"

1) __CH__*%instance%*__.csv__: results of the construction heuristic 
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