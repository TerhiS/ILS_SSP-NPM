# =======================================
# Title: ILS heuristic for the SSP-NPM
# =======================================
# Author: D. Calmels
# =======================================
# =======================================
#### Global Parameters and Variables
# Objectives
Objectives <- c("flowtime", "makespan", "switches")
# Perturbation Strategies
Strategies <-
  c("random", "problem-specific", "combined", "random-combined")
### Stopping Criteria
StoppingCriteria <- c("Time", "Iterations")
#StopCriterion <- "Iterations"
StopCriterion <-
  readline(prompt = "Enter Stop Criterion (Time or Iterations): ")
## Maximum number of perturbation iterations
#MaxIterations <- 100
MaxIterations <-
  as.integer(readline(prompt = "Enter Maximum Number of Iterations: "))
## Maximum run time limit in seconds
#MaxTime <- 60
MaxTime <-
  as.integer(readline(prompt = "Enter Maximum Runtime Limit: "))
# =======================================

# for all instances
for (instance in 1:length(data.list)) {
  # for all objectives
  for (objective in c("flowtime",
                      "makespan",
                      "switches")) {
    # three perturbation strategies
    for (strategy in c(#"random",
      #"problem-specific",
      "combined")) {
      # =================
      ### Initialisation
      # =================
      # All characteristics of the instance
      InstanceMatrix <- as.data.frame(data.list[[instance]])
      # Columns
      colnames(InstanceMatrix) <- c(1:length(InstanceMatrix[1,]))
      # Rows
      row.names(InstanceMatrix) <- c(1:length(InstanceMatrix[, 1]))
      
      # Number of Machines
      MaxMachines <- as.integer(InstanceMatrix[1, 1])
      # Number of Jobs
      MaxJobs <- as.integer(InstanceMatrix[1, 2])
      # Number of Tools
      MaxTools <- as.integer(InstanceMatrix[1, 3])
      
      # Tool Capacity per Machine MachineCapacities[m]
      MachineCapacities <- list()
      for (m in 1:MaxMachines) {
        MachineCapacities[[m]] <- (InstanceMatrix[2, m])
      }
      rm(m)
      
      # Tool Switching Times per Machine[m]
      SwitchingTimes <- list()
      for (m in 1:MaxMachines) {
        SwitchingTimes[m] <- (InstanceMatrix[3, m])
      }
      rm(m)
      
      # Processing Times per Machine and Job [m,j]
      ProcessingTimes <-
        as.data.frame(InstanceMatrix[4:(4 + MaxMachines - 1), ], row.names = c(1:MaxMachines))
      
      # Job-Tool-Matrix
      # Indicates the tools required per job
      JobToolMatrix <-
        InstanceMatrix[((4 + MaxMachines):length(InstanceMatrix[, 1])), (1:MaxJobs)]
      row.names(JobToolMatrix) <- c(1:MaxTools)
      colnames(JobToolMatrix) <- c(1:MaxJobs)
      
      # Required Tools per Job
      RequiredTools <- list()
      for (j in 1:MaxJobs) {
        RequiredTools[[j]] <-
          which(InstanceMatrix[((4 + MaxMachines):(4 + MaxMachines + MaxTools - 1)), j] == 1)
      }
      rm(j)
      
      # Objective-specific parameters and variables
      if (objective == "flowtime") {
        ConstructionHeuristic <- "IEACT"
        Gamma <- 0.05
        Beta <- 0.2
      }
      
      if (objective == "makespan") {
        ConstructionHeuristic <- "IEACT"
        Gamma <- 0.1
        Beta <- 0.2
      }
      
      if (objective == "switches") {
        ConstructionHeuristic <- "IGI"
        Gamma <- 0.1
        if (strategy == "combined" ||
            strategy == "random-combined") {
          Beta <- 0.2
        } else {
          Beta <- 0.8
        }
      }
      
      # Best Solution per iteration
      if (StopCriterion == "Iterations") {
        BestSolution <- matrix(ncol = 2, nrow = MaxIterations)
        colnames(BestSolution) <- c("BestKnownSolution", "Time")
      }
      ########################
      
      # ============================================
      # Import Results from Construction Heuristics
      # ============================================
      ConstructionSolution <- ImportConstructionResults(
        Instance = instance,
        ConstructionHeuristic = ConstructionHeuristic,
        Objective = objective
      )
      BestKnownObjectiveValue <- ConstructionSolution[[1]]
      BestSequence <- ConstructionSolution[[2]]
      
      ########################
      ### Start computation time count
      start.time <- proc.time()[[3]]
      
      # =====================================
      # Local Search (1) on initial solution
      # =====================================
      LS_Sequence <- LocalSearch(
        Objective = objective,
        InitialSequence = BestSequence,
        BestSolutionValue = BestKnownObjectiveValue
      )
      # Result Local Search (1)
      BestSequence <- LS_Sequence
      BestToolLoad <- KTNS(BestSequence)
      BestToolSwitches <-
        ResultsToolSwitches(BestSequence, BestToolLoad)
      BestCompletionTimes <-
        ResultsCompletionTime(BestSequence, BestToolLoad, BestToolSwitches)
      # Best known objective value
      if (objective == "flowtime") {
        BestKnownObjectiveValue <- Reduce("+", BestCompletionTimes)
      }
      if (objective == "makespan") {
        BestKnownObjectiveValue <- Reduce(max, BestCompletionTimes)
      }
      if (objective == "switches") {
        BestKnownObjectiveValue <- Reduce("+", BestToolSwitches)
      }
      
      # =====================================
      ########## Perturbation  ###########
      # =====================================
      
      ### required variables
      RetainedSolutionValue <- BestKnownObjectiveValue
      RetainedSequence <- BestSequence
      RetainedToolSwitches <- BestToolSwitches
      RetainedCompletionTimes <- BestCompletionTimes
      PerturbationOngoing <- T
      PerturbationCount <- 0
      Improved <- T
      
      while (PerturbationOngoing == T) {
        PerturbationOngoing <- F
        PerturbationCount <- PerturbationCount + 1
        
        if (strategy == "combined" ||
            strategy == "random-combined") {
          # if iteration improved
          if (Improved == T) {
            # Continue with Problem-Specific Perturbation
            PerturbationType <- 1
          } else {
            # generate a random number from a uniform distribution in the interval [0;1)
            rand <- runif(1)
            # if the random number is smaller than gamma, a random permutation PerturbationType 0 is performed
            if (rand < Gamma) {
              PerturbationType <- 0
            } else {
              # otherwise a problem-specific perturbation is performed
              PerturbationType <- 1
            }
          }
          
          ### if strategy is problem specific
          if (PerturbationType == 1) {
            PerturbedSequence <-
              ProblemSpecificPerturbation(
                mySequence = RetainedSequence,
                myToolSwitches = RetainedToolSwitches,
                myCompletionTimes = RetainedCompletionTimes,
                myObjective = objective
              )
          }
          ### if strategy is random
          if (PerturbationType == 0) {
            if (strategy == "combined") {
              PerturbedSequence <- RandomPerturbation(RetainedSequence, Beta)
            } else {
              PerturbedSequence <- RandomSequence()
            }
            
          }
        }
        
        if (strategy == "problem-specific") {
          PerturbedSequence <-
            ProblemSpecificPerturbation(
              mySequence = RetainedSequence,
              myToolSwitches = RetainedToolSwitches,
              myCompletionTimes = RetainedCompletionTimes,
              myObjective = objective
            )
        }
        
        if (strategy == "random") {
          PerturbedSequence <- RandomPerturbation(RetainedSequence, Beta)
        }
        
        # KTNS and solution of the perturbed sequence
        Pert_ToolLoad <- KTNS(JobSequence = PerturbedSequence)
        Pert_NumberOfSwitches <-
          ResultsToolSwitches(PerturbedSequence, Pert_ToolLoad)
        Pert_CompletionTimes <-
          ResultsCompletionTime(PerturbedSequence,
                                Pert_ToolLoad,
                                Pert_NumberOfSwitches)
        
        if (objective == "flowtime") {
          NewObjectiveValue <- Reduce("+", Pert_CompletionTimes)
        }
        if (objective == "makespan") {
          NewObjectiveValue <- Reduce(max, Pert_CompletionTimes)
        }
        if (objective == "switches") {
          NewObjectiveValue <- Reduce("+", Pert_NumberOfSwitches)
        }
        
        # =======================================
        # Local Search (2) on perturbed solution
        # =======================================
        
        Pert_LS_Sequence <-
          LocalSearch("flowtime", PerturbedSequence, NewObjectiveValue)
        Pert_LS_ToolLoad <- KTNS(Pert_LS_Sequence)
        Pert_LS_ToolSwitches <-
          ResultsToolSwitches(Pert_LS_Sequence, Pert_LS_ToolLoad)
        Pert_LS_CompletionTimes <-
          ResultsCompletionTime(Pert_LS_Sequence,
                                Pert_LS_ToolLoad,
                                Pert_LS_ToolSwitches)
        if (objective == "flowtime") {
          NewObjectiveValue <- Reduce("+", Pert_LS_CompletionTimes)
        }
        if (objective == "makespan") {
          NewObjectiveValue <- Reduce(max, Pert_LS_CompletionTimes)
        }
        if (objective == "switches") {
          NewObjectiveValue <- Reduce("+", Pert_LS_ToolSwitches)
        }
        # =======================================
        # Acceptance Criterion
        # =======================================
        # if the solution is better than the best known solution
        if (NewObjectiveValue < BestKnownObjectiveValue) {
          Improved <- T
          PerturbationOngoing <- T
          # save new bks
          BestKnownObjectiveValue <- NewObjectiveValue
          ### overwrite (current) best solutions
          BestCompletionTimes <- Pert_LS_CompletionTimes
          BestToolSwitches <- Pert_LS_ToolSwitches
          BestToolLoad <- Pert_LS_ToolLoad
          BestSequence <- Pert_LS_Sequence
          
        } else {
          # Acceptance of the new solution
          Improved <- F
          PerturbationOngoing <- T
        }
        # New starting solution is the best solution from the local search
        RetainedSequence <- Pert_LS_Sequence
        RetainedCompletionTimes <- Pert_LS_CompletionTimes
        RetainedToolSwitches <- Pert_LS_ToolSwitches
        # ==================
        end.time_it <- proc.time()[[3]]
        # Update time.taken_it
        time.taken_it <- end.time_it - start.time
        if (StopCriterion == "Iterations") {
          BestSolution[PerturbationCount, 1] <- BestKnownObjectiveValue
          BestSolution[PerturbationCount, 2] <-
            paste0(round(time.taken_it, digits = 2))
        }
        
        # Termination Criterion
        # ==================
        # If stopping criterion is met, terminate Perturbation
        if (time.taken_it > MaxTime && StopCriterion == "Time") {
          PerturbationOngoing <- F
          break
        }
        
        if (PerturbationCount >= MaxIterations &&
            StopCriterion == "Iterations") {
          PerturbationOngoing <- F
          break
        }
        # ==================
      }
      
      ##############################################
      end.time <- proc.time()[[3]]
      time.taken <- end.time - start.time
      ##############################################
      
      # =======================================
      # Output
      # =======================================
      # transform lists to writable format
      BestSequence <-
        capture.output(cat(paste0(
          ConvertColonToVector(BestSequence)
        )))
      BestToolLoad <-
        capture.output(cat(paste0(
          ConvertColonToVector(BestToolLoad)
        )))
      # result directory
      setwd("../iterated_local_search/")
      # Best solution per iteration file
      if (StopCriterion == "Iterations") {
        write.table(
          BestSolution,
          paste(
            "./",
            instance,
            "_",
            strategy,
            "_",
            objective,
            ".csv",
            sep = ""
          ),
          sep = ";",
          row.names = T,
          col.names = F
        )
      }
      # Write Best Solution To File
      # Generate Solution Matrix
      solution.matrix <-
        as.matrix(rbind(
          BestKnownObjectiveValue,
          paste0(round(time.taken, digits = 2)),
          BestSequence,
          BestToolLoad
        ))
      write.table(
        solution.matrix,
        paste(
          "./ILS",
          objective,
          "_",
          strategy,
          "_",
          instance,
          ".csv",
          sep = ""
        ),
        sep = ";",
        row.names = T,
        col.names = F
      )
      setwd("~/GitHub/ILS_SSP-NPM/Heuristics/")
      # =======================================
      # remove all varaibles and parameeters except instances and instance counters
      rm(list = ls()[!ls() %in% c(
        "data.list",
        "csv_files",
        "instance",
        "objective",
        "strategy",
        "StopCriterion",
        "MaxTime",
        "MaxIterations",
        "ProblemSet",
        # functions:
        "resamp",
        "ConvertColonToVector",
        "LocalSearch",
        "ResultsCompletionTime",
        "ResultsToolSwitches",
        "ImportConstructionResults",
        "KTNS",
        "RandomPerturbation",
        "ProblemSpecificPerturbation",
        "RandomSequence"
      )])
    }
    
  }
}
