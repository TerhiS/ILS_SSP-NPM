# =====================================
########## Perturbation  ###########
# =====================================

### required variables
RetainedSolutionValue <- BestKnownObjectiveValue
RetainedSequence <- LS_Sequence

PerturbationOngoing <- T
PerturbationCount <- 0
Improved <- T

while (PerturbationOngoing == T) {
  PerturbationOngoing <- F
  PerturbationCount <- PerturbationCount + 1
  
  if (strategy == "combined") {
    # if iteration improved
    if (improved == T) {
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
          JobSequence = PerturbedSequence,
          SetupTimes = RetainedToolSwitches,
          CompletionTimes = RetainedCompletionTimes,
          objective = objective,
          Beta = Beta
        )
    }
    ### if strategy is random
    if (PerturbationType == 0) {
      PerturbedSequence <- RandomPerturbation(PerturbedSequence, Beta)
    }
  }
  
  if (strategy == "problem-specific") {
    PerturbedSequence <-
      ProblemSpecificPerturbation(
        JobSequence = RetainedSequence,
        SetupTimes = RetainedToolSwitches,
        CompletionTimes = RetainedCompletionTimes,
        Objective = objective,
        Beta = Beta
      )
  }
  
  if (strategy == "random") {
    PerturbedSequence <- RandomPerturbation(PerturbedSequence, beta)
  }
  
  # KTNS and solution of the perturbed sequence
  Pert_ToolLoad <- KTNS(JobSequence = PerturbedSequence)
  Pert_NumberOfSwitches <-
    ResultsToolSwitches(PerturbedSequence, Pert_ToolLoad)
  Pert_CompletionTimes <-
    ResultsCompletionTime(PerturbedSequence, Pert_ToolLoad, Pert_NumberOfSwitches)
  
  if (objective == "flowtime") {
    NewObjectiveValue <- Reduce("+", Pert_CompletionTimes)
  }
  if (objective == "makespan") {
    NewObjectiveValue <- Reduce(max, Pert_CompletionTimes)
  }
  if (objective == "switches") {
    NewObjectiveValue <- Reduce("+", Pert_NumberOfSwitches)
  }
  
  
  #######################################
  
  # =======================================
  # Local Search
  # =======================================
  
  Pert_LS_Sequence <-
    LocalSearch("flowtime", PerturbedSequence, NewObjectiveValue)
  Pert_LS_ToolLoad <- KTNS(Pert_LS_Sequence)
  Pert_LS_ToolSwitches <- ResultsToolSwitches(Pert_LS_Sequence, Pert_LS_ToolLoad)
  Pert_LS_CompletionTimes <-
    ResultsCompletionTime(Pert_LS_Sequence, Pert_LS_ToolLoad, Pert_LS_ToolSwitches)
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
    BestNumberOfSwitches <- Pert_LS_ToolSwitches
    BestToolLoad <- Pert_LS_ToolLoad
    BestSequence <- Pert_LS_Sequence

  } else {
    # Acceptance of the new solution
    improved <- F
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
  # update iteration best
  bs_pert[PerturbationCount, 2] <- time.taken_it
  bs_pert[PerturbationCount, 1] <- bks
  
  # Termination Criterion
  # ==================
  # If stopping criterion is met, terminate Perturbation
  if (time.taken > MaxTime && StopCriterion == "Time") {
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