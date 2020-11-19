# ========================================
# Keep Tool Needed Soonest (KTNS)
# ========================================

# For a given job sequence JobSequence
# determine the Tool Loading ToolLoad for each job
# by keeping the tool in the magazine which is required the soonest

KTNS <- function(JobSequence) {
  # sequence of all tools that are still required without first jobs
  # !Note: in the case of only one job the list is NULL
  AllToolsRequired <- list()
  for (m in 1:MaxMachines) {
    AllToolsRequired[[m]] <-
      unlist(RequiredTools[JobSequence[[m]][-1]])
  }
  remove(m)
  
  ### Tool Loading
  ToolLoad <- RequiredTools
  
  ### initial loading (first jobs)
  for (m in 1:MaxMachines) {
    # Select the first job as active job
    ActiveJob <- JobSequence[[m]][1]
    if (length(JobSequence[[m]]) == 1) {
      # load only the required tools
      ToolLoad[[ActiveJob]] <- RequiredTools[[ActiveJob]]
    } else {
      ### No free tool slots
      # the number of required tools is equal to the capacity
      if (length(RequiredTools[[ActiveJob]]) == MachineCapacities[[m]]) {
        ## do nothing
        # if free slot available and tools to fill the free slot
      } else if (length((RequiredTools[[ActiveJob]])) < MachineCapacities[[m]]) {
        # while still tools to fill
        # Calculate the number of free slots
        MaxFreeSlots <-
          length((length(RequiredTools[[ActiveJob]]) + 1):MachineCapacities[[m]])
        UniqueRequiredTools <-
          union(AllToolsRequired[[m]], AllToolsRequired[[m]])
        NotLoadedRequiredTools <-
          setdiff(UniqueRequiredTools, RequiredTools[[ActiveJob]])
        # if more slots than required tools
        if (MaxFreeSlots > length(NotLoadedRequiredTools)) {
          # fill free slots with required tools
          # but only if the tools is not in use already by first job
          ToolLoad[[ActiveJob]] <-
            append(x = ToolLoad[[ActiveJob]], values = NotLoadedRequiredTools)
        }
        else {
          # fill the free slots with later required tools not already loaded
          InsertedTools <- NotLoadedRequiredTools[1:MaxFreeSlots]
          ToolLoad[[ActiveJob]] <-
            append(ToolLoad[[ActiveJob]], NotLoadedRequiredTools[1:MaxFreeSlots])
          # remove the tools inserted from the required list.
        }
      }
    }
  }
  rm(m)
  rm(ActiveJob)
  
  
  ### tool optimization
  for (m in 1:MaxMachines) {
    # if more than one job on the machine
    if (length(JobSequence[[m]]) > 1) {
      for (ActiveJobPosition in 2:length(JobSequence[[m]])) {
        ActiveJob <- JobSequence[[m]][ActiveJobPosition]
        PreviousJob <- JobSequence[[m]][ActiveJobPosition - 1]
        NotMatchingTools <-
          which(!is.na(match(RequiredTools[[ActiveJob]], ToolLoad[[PreviousJob]])) == F)
        MatchingTools <-
          which(!is.na(match(RequiredTools[[ActiveJob]], ToolLoad[[PreviousJob]])) == T)
        
        # if no tools are to be replaced because required tools are already loaded
        if (length(NotMatchingTools) == 0) {
          ToolLoad[[ActiveJob]] <- ToolLoad[[PreviousJob]]
          
        }
        # if tools need to be replaced
        else if (length(NotMatchingTools) > 0) {
          # if all tools need to be replaced
          if (length(NotMatchingTools) == length(ToolLoad[[PreviousJob]])) {
            ToolLoad[[ActiveJob]] <- RequiredTools[[ActiveJob]]
          }
          # if any tools need to be replaced (less than magazine capacity)
          else if (length(NotMatchingTools) < length(ToolLoad[[PreviousJob]])) {
            ToolLoad[[ActiveJob]] <- RequiredTools[[ActiveJob]]
            
            StillRequiredTools <-
              unlist(RequiredTools[JobSequence[[m]][-(1:ActiveJobPosition)]])
            # which tools are already loaded and later required
            ToolIntersection <-
              intersect(ToolLoad[[PreviousJob]], StillRequiredTools)
            # which tools can theoretically be kept
            ToolIntersection <-
              setdiff(ToolIntersection, ToolLoad[[ActiveJob]])
            
            # if there are more "free slots" than tools required in the future and not in magazine
            if ((length(ToolLoad[[PreviousJob]]) - length(RequiredTools[[ActiveJob]]))
                > length(ToolIntersection)) {
              if (length(unlist(StillRequiredTools)) == 0) {
                # keep old tools until magazine is full
                OldTools <-
                  setdiff(ToolLoad[[PreviousJob]], ToolLoad[[ActiveJob]])
                ToolLoad[[ActiveJob]] <-
                  append(ToolLoad[[ActiveJob]],
                         OldTools[1:(length(ToolLoad[[PreviousJob]]) - length(ToolLoad[[ActiveJob]]))])
                
                remove(OldTools)
                
              } else {
                # replace the free slots with tools needed soonest (and not in the magazin)
                ToolLoad[[ActiveJob]] <-
                  append(ToolLoad[[ActiveJob]], ToolIntersection)
                # keep old tools until magazine is full
                OldTools <-
                  setdiff(ToolLoad[[PreviousJob]],
                          union(ToolLoad[[ActiveJob]], StillRequiredTools))
                ToolLoad[[ActiveJob]] <-
                  append(ToolLoad[[ActiveJob]],
                         OldTools[1:(length(ToolLoad[[PreviousJob]]) - length(ToolLoad[[ActiveJob]]))])
                
                remove(OldTools)
              }
              
            }
            
            # if there are less "free slots" than tools required in future and not in magazine
            else if ((length(ToolLoad[[PreviousJob]]) - length(RequiredTools[[ActiveJob]])) <= length(ToolIntersection) &
                     (length(ToolLoad[[PreviousJob]]) - length(RequiredTools[[ActiveJob]])) > 0) {
              # replace the free slots with tools needed soonest (and not in the magazine)
              ToolLoad[[ActiveJob]] <-
                append(ToolLoad[[ActiveJob]], ToolIntersection[1:(length(ToolLoad[[PreviousJob]]) - length(RequiredTools[[ActiveJob]]))])
            }
          }
        }
      }
    }
  }
  remove(m)
  remove(ActiveJobPosition)
  return(ToolLoad)
  # ========================================
}
