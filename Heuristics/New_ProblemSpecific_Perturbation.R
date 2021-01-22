ProblemSpecificPerturbation <-
  function(mySequence,
           myToolSwitches,
           myCompletionTimes,
           myObjective)   {
    if (myObjective == "flowtime" | myObjective == "makespan") {
      ### bottleneck machine with highest completion time of the last job
      CompletionTimeSum = list()
      for (m in 1:MaxMachines) {
        CompletionTimeSum[[m]] <-
          sum2(unlist(myCompletionTimes), mySequence[[m]])
      }
      remove(m)
      
      # bottleneck machines decreasing order
      OrderedBottleneckMachines <-
        order(unlist(CompletionTimeSum),
              decreasing = T,
              na.last = NA)
      
      # select the bottleneck machine from which jobs can be relocated
      # to another suitable machine
      JobSelected <- F
      while (JobSelected == F) {
        for (m in OrderedBottleneckMachines) {
          # only for machines with more than one remaining job
          if (length(mySequence[[m]]) > 1) {
            ### sum of main processing and setup
            BottleneckJobs <- mySequence[[m]]
            # Setup plus processing time
            ProcessingTimeSum <- list()
            for (j in BottleneckJobs) {
              ProcessingTimeSum[j] <-
                (as.integer(myToolSwitches[j]) * SwitchingTimes[[m]]) + ProcessingTimes[[j]][m]
            }
            remove(j)
            # exclude jobs not processed on that machine
            ProcessingTimeSum[which(ProcessingTimeSum == 'NULL')] <-
              NA
            OrderedSumOfProcessingTimes <-
              order(unlist(ProcessingTimeSum),
                    decreasing = T,
                    na.last = NA)
            # All other machines
            OtherMachines <- c(1:MaxMachines)[c(1:MaxMachines != m)]
            # Select the worst job on the bottleneck machine
            for (n in 1:length(OrderedSumOfProcessingTimes)) {
              # Select the worst job on the bottleneck machine
              SelectedJob <- OrderedSumOfProcessingTimes[[n]]
              # Try for Relocation on another machine
              for (m_new in OtherMachines) {
                # if the job can be relocated stop the loop
                if (length(RequiredTools[[SelectedJob]]) <= MachineCapacities[m_new]) {
                  InsertedJob <- SelectedJob
                  BottleneckMachine <- m
                  JobSelected <- T
                  break
                }
              }
              rm(m_new)
              if (JobSelected == T)
                break
              if (n == length(OrderedSumOfProcessingTimes))
                break
            }
            if (JobSelected == T)
              break
            rm(n)
          }
          if (JobSelected == T)
            break
        }
        if (JobSelected == T)
          break
        rm(m)
      }
      
      #### Generate new sequence
      # Calculate all possible insertion positions
      ApproxProcessingTimes <-
        data.frame(matrix(ncol = MaxJobs, nrow = MaxMachines))
      SuitableMachine <-
        which(MachineCapacities >= length(RequiredTools[[InsertedJob]]))
      SuitableMachine <-
        SuitableMachine[SuitableMachine != BottleneckMachine]
      
      ### Best insertion position
      # calculate all approximated processing plus setup times
      for (m in SuitableMachine) {
        NewLength <- length(mySequence[[m]]) + 1
        for (position in 1:NewLength) {
          # first position
          if (position == 1) {
            SwitchesAfter <-
              length(setdiff(RequiredTools[[mySequence[[m]][position]]], RequiredTools[[InsertedJob]]))
            ApproximatedProcessing <-
              (ProcessingTimes[[InsertedJob]][m] + (SwitchingTimes[[m]] * SwitchesAfter))
            ApproxProcessingTimes[m, position] <-
              ApproximatedProcessing
            
          }
          # last position
          if (position == NewLength) {
            SwitchesBefore  <-
              length(setdiff(RequiredTools[[InsertedJob]], RequiredTools[[mySequence[[m]][length(mySequence[[m]])]]]))
            ApproxProcessingTimes[m, position] <-
              ProcessingTimes[[InsertedJob]][m] + (SwitchingTimes[[m]] * SwitchesBefore)
          }
          # any other position
          else if (position > 1 && position < NewLength) {
            SwitchesBefore <-
              length(setdiff(RequiredTools[[InsertedJob]], RequiredTools[[mySequence[[m]][(position - 1)]]]))
            SwitchesAfter <-
              length(setdiff(RequiredTools[[mySequence[[m]][position]]], RequiredTools[[InsertedJob]]))
            ApproxProcessingTimes[m, position] <-
              ProcessingTimes[[InsertedJob]][m] +
              (SwitchingTimes[[m]] * SwitchesBefore) +
              (SwitchingTimes[[m]] * SwitchesAfter)
          }
        }
      }
      remove(m)
      remove(position)
      
      # position with the lowest processing time and approximated tool switching times
      MinApproxProcessings <-
        which(ApproxProcessingTimes == min(ApproxProcessingTimes, na.rm = T),
              arr.ind = T)
      MinApproxProcessing <- resamp(1:nrow(MinApproxProcessings), 1)
      InsertionMachine <-
        MinApproxProcessings[MinApproxProcessing, 1]
      InsertionPosition <-
        MinApproxProcessings[MinApproxProcessing, 2]
      
      ### New sequence after perturbation
      # Remove job InsertedJob from old sequence on machine BottleneckMachine
      mySequence[[BottleneckMachine]] <-
        mySequence[[BottleneckMachine]][mySequence[[BottleneckMachine]] != InsertedJob]
      # Insert the job InsertedJob on InsertionMachine in position InsertionPosition
      mySequence[[InsertionMachine]] <-
        insert(mySequence[[InsertionMachine]], ats =
                 InsertionPosition, values = InsertedJob)
      
      remove(BottleneckMachine)
    }
    
    if (myObjective == "switches") {
      # --> bottleneck machine with most tool switches
      SwitchingTimeSum <- list()
      for (m in 1:MaxMachines) {
        SwitchingTimeSum[[m]] <-
          sum2(as.numeric(myToolSwitches), mySequence[[m]])
      }
      remove(m)
      
      OrderedBottleneckMachines <-
        order(unlist(SwitchingTimeSum),
              decreasing = T,
              na.last = NA)
      
      JobSelected <- F
      while (JobSelected == F) {
        for (m in OrderedBottleneckMachines) {
          # only for machines with more than one remaining job
          if (length(mySequence[[m]]) > 1) {
            BottleneckJobs <- mySequence[[m]]
            SwitchesSum <- list()
            for (j in BottleneckJobs) {
              SwitchesSum[j] <- (myToolSwitches[j])
            }
            remove(j)
            SwitchesSum[which(SwitchesSum == 'NULL')] <- NA
            SwitchesSum <-
              order(unlist(SwitchesSum),
                    decreasing = T,
                    na.last = NA)
            OtherMachines <- c(1:MaxMachines)[c(1:MaxMachines != m)]
            # Select the worst job on the bottleneck machine
            for (n in 1:length(SwitchesSum)) {
              # Select the worst job on the bottleneck machine
              SelectedJob <- SwitchesSum[[n]]
              # Try for Relocation on another machine
              for (m_new in OtherMachines) {
                # if the job can be relocated stop the loop
                if (length(RequiredTools[[SelectedJob]]) <= MachineCapacities[m_new]) {
                  InsertedJob <- SelectedJob
                  BottleneckMachine <- m
                  JobSelected <- T
                  break
                }
              }
              rm(m_new)
              if (JobSelected == T)
                break
              if (n == length(SwitchesSum))
                break
              
            }
            if (JobSelected == T)
              break
            rm(n)
          }
          if (JobSelected == T)
            break
        }
        if (JobSelected == T)
          break
        rm(m)
      }
      
      ##### Generate new sequence
      # For each insertion position...
      # ... calculate the approximated number of tool switches
      ApproxSwitches <-
        data.frame(matrix(ncol = MaxJobs, nrow = MaxMachines))
      SuitableMachine <-
        which(MachineCapacities >= length(RequiredTools[[InsertedJob]]))
      SuitableMachine <-
        SuitableMachine[SuitableMachine != BottleneckMachine]
      
      for (m in SuitableMachine) {
        NewLength <- length(mySequence[[m]]) + 1
        for (position in 1:NewLength) {
          if (position == 1) {
            SwitchesAfter <-
              length(setdiff(RequiredTools[[mySequence[[m]][position]]], RequiredTools[[InsertedJob]]))
            ApproxSwitches[m, position] <- SwitchesAfter
            
          }
          if (position == NewLength) {
            SwitchesBefore <-
              length(setdiff(RequiredTools[[InsertedJob]], RequiredTools[[mySequence[[m]][length(mySequence[[m]])]]]))
            ApproxSwitches[m, position] <- SwitchesBefore
          }
          else if (position > 1 && position < NewLength) {
            SwitchesBefore <-
              length(setdiff(RequiredTools[[InsertedJob]], RequiredTools[[mySequence[[m]][(position - 1)]]]))
            SwitchesAfter <-
              length(setdiff(RequiredTools[[mySequence[[m]][position]]], RequiredTools[[InsertedJob]]))
            ApproxSwitches[m, position] <-
              SwitchesBefore + SwitchesAfter
          }
        }
      }
      remove(m)
      remove(position)
      
      # position with the lowest processing time and approximated tool switching times
      MinApproxSwitches <-
        which(ApproxSwitches == min(ApproxSwitches, na.rm = T),
              arr.ind = T)
      MinApproxSwitch <- resamp(1:nrow(MinApproxSwitches), 1)
      InsertionMachine <- MinApproxSwitches[MinApproxSwitch, 1]
      InsertionPosition <- MinApproxSwitches[MinApproxSwitch, 2]
      
      ### New sequence after perturbation
      # Remove job InsertedJob from old sequence on machine BottleneckMachine
      mySequence[[BottleneckMachine]] <-
        mySequence[[BottleneckMachine]][mySequence[[BottleneckMachine]] != InsertedJob]
      # Insert the job InsertedJob on InsertionMachine in position InsertionPosition
      mySequence[[InsertionMachine]] <-
        insert(mySequence[[InsertionMachine]], ats = InsertionPosition, values = InsertedJob)
    }
    return(mySequence)
  }
