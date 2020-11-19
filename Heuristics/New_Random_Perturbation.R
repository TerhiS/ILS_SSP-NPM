# =======================================
# Random Perturbation of a job sequnce
# =======================================

RandomPerturbation <- function(JobSequence,beta) {
  ## select n jobs at random
  n <- floor(beta * MaxJobs)
  
  SelectedJobs <- resamp(1:MaxJobs, n)
  
  for (j in 1:length(SelectedJobs)) {
    for (m in 1:MaxMachines) {
      JobOnMachine <- SelectedJobs[j] %in% JobSequence[[m]]
      if (isTRUE(JobOnMachine))
        OldMachine <- m
    }
    remove(m)
    
    SuitableMachine <-
      which(MachineCapacities >= sum(JobToolMatrix[, SelectedJobs[j]]))
    SuitableMachine <-
      SuitableMachine[-which(SuitableMachine == OldMachine)]
    if (length(SuitableMachine) > 0) {
      SuitableMachine <- resamp(SuitableMachine, 1)
      # Selected Jobs can only be removed from a sequence with more than one job
      if (length(JobSequence[[OldMachine]]) >= 2) {
        # insert the selected job on a suitable machine 
        JobSequence[[SuitableMachine]] <-
          append(JobSequence[[SuitableMachine]], SelectedJobs[j])
        # remove the selected job from old machine
        JobSequence[[OldMachine]] <-
          JobSequence[[OldMachine]][-which(JobSequence[[OldMachine]] == SelectedJobs[j])]
      }
    }
  }
  remove(j)
  return(JobSequence)
}


