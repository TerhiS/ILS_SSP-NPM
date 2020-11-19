# Completion Time and Number of Tool Switches for a given Sequence and Tool Load
# JobSequence
# ToolLoad

ResultsToolSwitches <- function(JobSequence, ToolLoad) {
  ToolSwitches <- vector("list", MaxJobs)
  
  for (m in 1:MaxMachines) {
    for (JobPosition in 1:length(JobSequence[[m]])) {
      if (JobPosition == 1) {
        # Tool Switching Time is zero.
        ToolSwitches[[JobSequence[[m]][JobPosition]]] <- 0
      }
      else {
        # Tool Switches is sum new inserted tools
        InsertedTools <-
          setdiff(ToolLoad[[JobSequence[[m]][JobPosition]]],
                  ToolLoad[[JobSequence[[m]][JobPosition -
                                               1]]])
        ToolSwitches[[JobSequence[[m]][JobPosition]]] <-
          length(InsertedTools)
      }
    }
    rm(JobPosition)
  }
  rm(m)
  return(ToolSwitches)
}

ResultsCompletionTime <-
  function(JobSequence, ToolLoad, ToolSwitches) {
    CompletionTimes <- vector("list", MaxJobs)
    for (m in 1:MaxMachines) {
      for (JobPosition in 1:length(JobSequence[[m]])) {
        if (JobPosition == 1) {
          # Completion Time is Processing Time.
          CompletionTimes[[JobSequence[[m]][JobPosition]]] <-
            ProcessingTimes[[JobSequence[[m]][JobPosition]]][m]
        }
        else {
          # Completion Time is Processing Time plus Setup Time
          SetUpTime <-
            SwitchingTimes[[m]] * ToolSwitches[[JobSequence[[m]][JobPosition]]]
          CompletionTimes[[JobSequence[[m]][JobPosition]]] <-
            CompletionTimes[[JobSequence[[m]][JobPosition - 1]]] +
            ProcessingTimes[[JobSequence[[m]][JobPosition]]][m] + SetUpTime
        }
      }
      rm(JobPosition)
    }
    rm(m)
    return(CompletionTimes)
  }

