# =======================================
# Title: Construction heuristic IEACT for the SSP-NPM
# =======================================

# Description: Computes total flowtime / makespan / number of tool switches for the job sequencing and tool switching problm
# param data.list holds the instances
# param m,ma Machines
# param t Tools
# param j,job Jobs
# param c Tool Capacities of the Machines
# param sw Tool Switching Time of the Machines
# param bks Best Known Solution for specific objective
# @bks = {flowtime, makespan, switches}



# ===========================================
############### Start IEACT #################
# ===========================================

# for all problem instances in the data set (default: example_instance)
for (instance in 1:length(data.list)) {
  
  # =================
  ### Initialisation
  # =================
  # All characteristics of the instance
  InstanceMatrix <- as.data.frame(data.list[[instance]])
  # Columns
  colnames(InstanceMatrix) <- c(1:length(InstanceMatrix[1, ]))
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
    as.data.frame(InstanceMatrix[4:(4 + MaxMachines - 1),], row.names = c(1:MaxMachines))
  
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
  

  # auxiliary matrix required to track jobs to be scheduled
  jt_matrix <- JobToolMatrix
  ### Initial completion time of the last job of machine m @f[m]
  f <- array()
  f[1:MaxMachines] <- 0
  ##############################################
  
  # =====================================
  ########## IEACT algorithm ############
  # =====================================
  
  #### additional input parameters and sets ####
  # auxiliary list for processing times
  p_m_IEACT <- ProcessingTimes
  # initialisation job sequence pi_m on machine m
  pi_m_IEACT <- data.frame(matrix(ncol = MaxMachines, nrow = MaxJobs))
  colnames(pi_m_IEACT) <- c(1:MaxMachines)
  
  # ============
  ##### Start of computation time counter
  tic("IEACT")
  
  # ============
  # Job Assignment
  # ============

  # unassigned jobs j_left with the help of setting the processing time of processed jobs to infinity
  j_left <- which(p_m_IEACT[1,] != Inf)
  
  # while still jobs to process
  while (sum(j_left) > 0) {
    # select suitable machines
    # set all machines to unsuitable
    poss_m <- list()
    poss_m[1:MaxMachines] = F
    
    # if job still possible (tool constraint) set machine to suitable
    j_left <- which(p_m_IEACT[1,] != Inf)
    for (job in j_left) {
      for (machine in 1:MaxMachines) {
        if (MachineCapacities[[machine]] >= sum(JobToolMatrix[, job]))
          poss_m[machine] = T
      }
    }
    remove(job)
    remove(machine)
    
    # capacity of selected machine must be large enough to process left jobs and at best with minimum artificial completion time
    free_m <- which(poss_m == T)
    # states which machine(s) is free and suitable
    free_m <- which(f[] == Reduce(min, f[free_m]))
    # if there are multiple free machines, randomly select one
    free_m <- resamp(free_m, 1)
    # processing time of the jobs being processed on the free machine
    df_part <- p_m_IEACT[free_m,]
    
    # if job is the first one on that machine (no tool switching required)
    if (f[free_m] == 0) {
      # select the job with minimum processing time and fitting number of tools
      j_fit <- list()
      for (job in 1:MaxJobs) {
        j_fit[[job]] <- which(sum(JobToolMatrix[, job]) <= MachineCapacities[[free_m]])
      }
      remove(job)
      j_fit <- which(j_fit == 1)
      j_mins <-
        j_fit[which(df_part[j_fit] == min(df_part[j_fit]))]
      # if there are multiple jobs with the same p_jm, randomly select one
      j_min <- resamp(j_mins, 1)
      # assign that job to a free machine
      pi_m_IEACT[(length(pi_m_IEACT[, free_m][!is.na(pi_m_IEACT[, free_m])]) +
                  1), free_m] <- j_min
      # update completion time of that job on machine m (no tool switches for first jobs)
      #!!! cannot take minimum because not all jobs may be fitting
      f[free_m] <- f[free_m] + (df_part[j_min])
      # remove job from data frame for all machines (set processing time to infinity)
      for (m in 1:MaxMachines) {
        p_m_IEACT[m,j_min] = (Inf)
      }
    }
    
    # if the job is not the first job, then tool switches have to be considered
    else {
      # of all jobs left
      previous_j <-
        pi_m_IEACT[(length(pi_m_IEACT[, free_m][!is.na(pi_m_IEACT[, free_m])])), free_m]
      # add the approximated switching time to the processing time
      for (k in which(df_part != Inf)) {
        ts_approx <-
          length(which((jt_matrix[, k] - jt_matrix[, previous_j]) == "-1"))
        if (ts_approx > 0) {
          df_part[k] <- df_part[k] + (SwitchingTimes[[free_m]][1] * ts_approx)
        }
        if (ts_approx == 0) {
          df_part[k] <- df_part[k]
        }
      }
      remove(k)
      # select the job with maximum processing time plus tool switching
      j_fit <- list()
      for (job in j_left) {
        j_fit[[job]] <- which(sum(JobToolMatrix[, job]) <= MachineCapacities[[free_m]])
      }
      remove(job)
      for (job in 1:length(j_fit)) {
        j_fit[[job]] <- sum(j_fit[[job]])
      }
      remove(job)
      j_fit <- which(j_fit == 1)
      j_mins <-
        j_fit[which(df_part[j_fit] == min(df_part[j_fit]))]
      # if there are multiple jobs with the same p_jm, select random job
      j_min <- resamp(j_mins, 1)
      # assign that job to a free machine
      pi_m_IEACT[(length(pi_m_IEACT[, free_m][!is.na(pi_m_IEACT[, free_m])]) +
                  1), free_m] <- j_min
      # update completion time of that job on machine m plus approximated tool switching time
      f[free_m] <- f[free_m] + df_part[j_min]
      # remove job from data frame for all machines
      for (m in 1:MaxMachines) {
        p_m_IEACT[m,j_min] = (Inf)
      }
    }
    remove(free_m)
    j_left <- which(p_m_IEACT[1,] != Inf)
  }
  remove(m)
  
  ##############################################
  
  # ============
  # Job Sequence - Solution
  # ============
  
  # final sequence on each machine without NAs
  l_IEACT <- list()
  for (m in 1:MaxMachines) {
    l_IEACT[[m]] <- (pi_m_IEACT[, m][!is.na(pi_m_IEACT[, m])])
  }
  remove(m)
  
  ####################################
  
  # ========================================
  # Tool Loading and keep tool needed soones (KTNS)
  load_IEACT <- KTNS(l_IEACT)
  # ========================================

  
  # Results
  IEACT_Switches <- ResultsToolSwitches(JobSequence = l_IEACT,ToolLoad = load_IEACT)
  IEACT_CompletionTimes <- ResultsCompletionTime(JobSequence = l_IEACT,ToolLoad = load_IEACT,ToolSwitches = IEACT_Switches)

  toc(log = T, quiet = T)
  c_time <- tic.log(format = T)
  tic.clear()
  tic.clearlog()
  
  # =====================================
  # Output
  # =====================================
  
  ### number of tool switches
  switch <- Reduce("+", IEACT_Switches)
  ### makespan
  fmax <- Reduce(max, IEACT_CompletionTimes)
  ### total flow time
  tft <- Reduce("+", IEACT_CompletionTimes)

  ### job assignment
  seq <- capture.output(cat(paste0(
      ConvertColonToVector(l_IEACT)
    )))
  ### tool loading
  loads <- capture.output(cat(paste0(
    ConvertColonToVector(load_IEACT)
  )))
  
  # write solution of the construction heuristic (IEACT) to file
  setwd(paste0("results/",ProblemSet,"/construction_heuristics"))
  mat <- as.matrix(rbind(switch, tft, fmax, c_time[1], seq, loads))
  f_ieact <- paste0("./IEACT", instance, ".csv", sep = "")
  if (file.exists(f_ieact) == T) {
    print("Attention: file already existed!")
  }
  if (file.exists(f_ieact) == F) {
    write.table(
      mat,
      paste("./IEACT", instance, ".csv", sep = ""),
      sep = ",",
      row.names = T,
      col.names = F
    )
  }
  setwd("~/GitHub/ILS_SSP-NPM/Heuristics")
  # remove all variables and parameters except instances and instance counter
  rm(list = ls()[!ls() %in% c("data.list", "csv_files", "instance", 
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
                              "ProblemSet")])
}

# ===========================================
################ END IEACT ##################
# ===========================================