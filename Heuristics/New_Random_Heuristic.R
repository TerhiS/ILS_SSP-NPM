RandomSequence <- function() {
  # Not assigned jobs
  RemainingJobs <- c(1:MaxJobs)
  
  # New Job Sequence
  NewSequence <- vector("list", MaxMachines)
  
  # Fitting Jobs for each machine
  FittingJobsPerMachine <- list()
  for (m in 1:MaxMachines) {
    FittingJobsPerMachine[[m]] <-
      as.vector(which(colSums(JobToolMatrix) <= MachineCapacities[[m]]))
  }
  rm(m)
  
  # Sort the Machines by non decreasing capacity
  MachinesOrderedByCapacity <- order(unlist(MachineCapacities))
  
  # Assign a first job at random to each machine starting with the machine with the lowest Capacity
  for (m in MachinesOrderedByCapacity) {
    SelectedJob <- resamp(FittingJobsPerMachine[[m]], 1)
    NewSequence[[m]] <- SelectedJob
    # Remove the Job from the Fitting Job Lists and the remaining jobs
    for (m2 in 1:MaxMachines) {
      FittingJobsPerMachine[[m2]] <-
        FittingJobsPerMachine[[m2]][!FittingJobsPerMachine[[m2]] %in% SelectedJob]
    }
    # Remove assigend Jobs
    RemainingJobs <- RemainingJobs[!RemainingJobs %in% SelectedJob]
  }
  rm(m)
  rm(m2)
  
  while (sum(RemainingJobs > 0)) {
    # Assign remaining jobs at random to fitting machines
    SelectedJob <- resamp(RemainingJobs, 1)
    # Assign to a random fitting machine
    FittingMachines <- list()
    for (m in 1:MaxMachines) {
      if (SelectedJob %in% FittingJobsPerMachine[[m]] == T) {
        FittingMachines[[m]] <- m
      }
    }
    rm(m)
    SelectedMachine <- resamp(unlist(FittingMachines), 1)
    NewSequence[[SelectedMachine]] <-
      append(NewSequence[[SelectedMachine]], SelectedJob)
    RemainingJobs <- RemainingJobs[!RemainingJobs %in% SelectedJob]
  }
  return(NewSequence)
}


