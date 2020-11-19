# =======================================
# Local Search (1) around initial solution
# =======================================

LocalSearch <-
  function(Objective,
           InitialSequence,
           BestSolutionValue) {
    # Initialisation
    BestSequence <- InitialSequence
    RetainedSequence <- InitialSequence
    RetainedSolutionValue <- BestSolutionValue
    
    #### Set improvement flag as TRUE
    foreach(m = 1:MaxMachines) %do% {
      improved <- T
      
      while (improved == T) {
        improved <- F
        # new sequence is similar to old sequence
        NewSequence <- RetainedSequence
        # retain improved best known solution
        BestSolutionValue <- RetainedSolutionValue
        
        if (length(RetainedSequence[[m]]) > 1) {
          # for each job on that machine
          for (PositionJob1 in 1:(length(RetainedSequence[[m]]) - 1)) {
            # and any other job on that machine
            for (PositionJob2 in (PositionJob1 + 1):length(RetainedSequence[[m]])) {
              # Swap jobs
              NewSequence[[m]][PositionJob1] <-
                RetainedSequence[[m]][PositionJob2]
              NewSequence[[m]][PositionJob2] <-
                RetainedSequence[[m]][PositionJob1]
             
              NewToolLoad <- KTNS(NewSequence)
              SwitchingTimes <-
                ResultsToolSwitches(NewSequence, NewToolLoad)
              CompletionTimes <-
                ResultsCompletionTime(NewSequence, NewToolLoad, SwitchingTimes)
              
              ####################################
              
              if (Objective == "flowtime") {
                NewObjectiveValue <- Reduce("+", CompletionTimes)
              }
              if (Objective == "makespan") {
                NewObjectiveValue <- Reduce(max, CompletionTimes)
              }
              if (Objective == "switches") {
                NewObjectiveValue <- Reduce("+", SwitchingTimes)
              }
              
              # ========================================
              # Acceptance Criterion
              # ========================================
              # if the solution is better than the best known solution
              if (NewObjectiveValue < BestSolutionValue) {
                # save new bks
                # bks is only updated after all loops break
                RetainedSolutionValue <- NewObjectiveValue
                # use new sequence
                RetainedSequence <- NewSequence
                ### overwrite (current) best solutions
                BestSequence <- NewSequence
                # set improved to TRUE
                improved <- T
                
                break
                
              } else {
                ### otherwise continue with old sequence and old solution
                NewSequence <- RetainedSequence
              }
              if (NewObjectiveValue < BestSolutionValue)
                break
            }
            if (NewObjectiveValue < BestSolutionValue)
              break
          }
        }
      }
    }
    return(BestSequence)
  }
##############################################

