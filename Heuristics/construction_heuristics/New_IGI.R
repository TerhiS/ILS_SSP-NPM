# =======================================
# Title: IGI construction heuristic for the SSP-NPM
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
############### Start IGI #################
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
  jt_matrix_new <- JobToolMatrix
  
  ### Initial completion time of the last job of machine m @f[m]
  f <- array()
  f[1:MaxMachines] <- 0
  ##############################################
  
  # =====================================
  ########## IGI algorithm ############
  # =====================================
  
  #### additional input parameters and sets ####
  # initialisation job sequence pi_m on machine m
  pi_m_IGI <- data.frame(matrix(ncol = MaxMachines, nrow = MaxJobs))
  colnames(pi_m_IGI) <- c(1:MaxMachines)
  
  # ============
  ##### Start of computation time counter
  tic("IGI")
  
  # ============
  # Job Assignment
  # ============
  
  # unassigned jobs j_left that still have to be scheduled, auxiliary job set aux_j
  aux_j <- array(1:MaxJobs)
  j_left <- aux_j
  
  # number of required tools per job (sum_t[j]), required for initial loading
  sum_t <- array()
  for (i in aux_j) {
    sum_t[i] <- sum(jt_matrix[, i])
  }
  remove(i)

  # while still jobs to process
  while (sum(j_left) > 0) {
    # select suitable machines
    # set all machines to unsuitable
    poss_m <- list()
    poss_m[1:MaxMachines] <- F
    
    # if job still possible (tool constraint) set machine to suitable
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
    
    # if job is the first one on that machine (no tool switching required)
    if (f[free_m] == 0) {
      # prepare tool sums (remove NAs)
      sum_t[is.na(sum_t)] <- 0
      # select the job j_fit with maximum number of required tools and which is not violating capacity restrictions
      # fitting jobs
      j_fit <- list()
      for (job in j_left) {
        j_fit[[job]] <- which(sum(JobToolMatrix[, job]) <= MachineCapacities[[free_m]])
      }
      remove(job)
      # remove unfitting / already assigned jobs (NAs)
      for (job in 1:length(j_fit)) {
        j_fit[[job]] <- sum(j_fit[[job]])
      }
      remove(job)
      j_fit <- which(j_fit == 1)
      
      # select the fitting jobs that require the highest number of tools
      j_tmaxs <- j_fit[which(sum_t[j_fit] == max(sum_t[j_fit]))]
      # if there are multiple jobs with the same tmax, randomly select one
      j_tmax <- resamp(j_tmaxs, 1)
      # assign that job to the free machine and in postion length +1
      pi_m_IGI[(length(pi_m_IGI[, free_m][!is.na(pi_m_IGI[, free_m])]) +
                  1), free_m] <- j_tmax
      # update completion time of that job on machine m
      f[free_m] <- f[free_m] + ProcessingTimes[free_m,j_tmax]
      # remove job from data frame
      aux_j[j_tmax] <- NA
      jt_matrix_new <-
        jt_matrix_new[, -which(colnames(jt_matrix_new) == j_tmax)]
      sum_t[j_tmax] <- 0
      
      # if the job is not the first job, select fitting job that has the highest number of tools in common with the previous job
    } else {
      # number of intersecting tools
      sum_intersections <- array()
      # select the job j_fit with maximum number of required tools and which is not violating capacity restrictions
      # fitting jobs
      j_fit <- list()
      for (job in j_left) {
        j_fit[[job]] <- which(sum(JobToolMatrix[, job]) <= MachineCapacities[[free_m]])
      }
      remove(job)
      # remove unfitting / already assigned jobs (NAs)
      for (job in 1:length(j_fit)) {
        j_fit[[job]] <- sum(j_fit[[job]])
      }
      remove(job)
      j_fit <- which(j_fit == 1)
      
      #if there are more than 1 jobs left
      if (length(j_fit) > 1) {
        #number of jobs already assigned
        n <- length(which(!is.na(pi_m_IGI[, free_m])))
        for (k in which(aux_j != is.na(aux_j))) {
          sum_intersections[k] <-
            length(which((jt_matrix[, pi_m_IGI[n, free_m]] + jt_matrix[, k]) == "2"))
        }
        # jobs with tool intersections
        jobs_intersect <- which(sum_intersections >= 0)
        jobs_intersect <- intersect(j_fit, jobs_intersect)
        # select fitting job with the maximum number of intersections
        j_tmaxs <-
          j_fit[which(sum_intersections[jobs_intersect] == max(sum_intersections[jobs_intersect]))]
        # if there are multiple jobs with the same number of intersections, randomly select one
        j_tmax <- resamp(j_tmaxs, 1)
        ts_approx <-
          length(which((jt_matrix[, j_tmax] - jt_matrix[, n]) == "-1"))
        # assign that job to a free machine in postion length +1
        pi_m_IGI[(length(pi_m_IGI[, free_m][!is.na(pi_m_IGI[, free_m])]) +
                    1), free_m] = j_tmax
        # update completion time of that job on machine m
        if (ts_approx > 0) {
          f[free_m] <-
            f[free_m] + ProcessingTimes[free_m,j_tmax] + (SwitchingTimes[[free_m]][1] * ts_approx)
        }
        if (ts_approx == 0) {
          f[free_m] <- f[free_m] + ProcessingTimes[free_m,j_tmax]
        }
  
        # remove job from data frame
        aux_j[j_tmax] <- NA
        jt_matrix_new <-
          jt_matrix_new[,-which(colnames(jt_matrix_new) == j_tmax)]
        
        #if there is only 1 job left
      } else if (length(j_fit) == 1) {
        # last job to be sequenced on free machine
        j_tmax <- j_fit
        pi_m_IGI[(length(pi_m_IGI[, free_m][!is.na(pi_m_IGI[, free_m])]) +
                    1), free_m] = j_tmax
        aux_j[j_tmax] = NA
      }
    }
    remove(free_m)
    j_left <- which(!is.na(aux_j))
  }
  remove(k)
  
  ##############################################
  
  # ============
  # Job Sequence - Solution
  # ============
  
  # final sequence on each machine without NAs
  l_IGI <- list()
  for (m in 1:MaxMachines) {
    l_IGI[[m]] <- (pi_m_IGI[, m][!is.na(pi_m_IGI[, m])])
  }
  remove(m)
  
  ####################################
  
  # ========================================
  # Tool Loading and keep tool needed soones (KTNS)
  load_IGI <- KTNS(l_IGI)
  # ========================================
  # Results
  IGI_Switches <- ResultsToolSwitches(JobSequence = l_IGI,ToolLoad = load_IGI)
  IGI_CompletionTimes <- ResultsCompletionTime(JobSequence = l_IGI,ToolLoad = load_IGI,ToolSwitches = IGI_Switches)
  
  
  toc(log = T, quiet = T)
  c_time <- tic.log(format = T)
  tic.clear()
  tic.clearlog()
  
  # =====================================
  # Output
  # =====================================
  ### number of tool switches
  switch <- Reduce("+", IGI_Switches)
  ### makespan
  fmax <- Reduce(max, IGI_CompletionTimes)
  ### total flow time
  tft <- Reduce("+", IGI_CompletionTimes)
  
  ### job assignment
  seq <- capture.output(cat(paste0(
    ConvertColonToVector(l_IGI)
  )))
  ### tool loading
  loads <- capture.output(cat(paste0(
    ConvertColonToVector(load_IGI)
  )))
  
  setwd(paste0("results/",ProblemSet,"/construction_heuristics"))
  mat <- as.matrix(rbind(switch, tft, fmax, c_time[1], seq, loads))
  f_igi <- paste0("./IGI", instance, ".csv", sep = "")
  if (file.exists(f_igi) == T) {
    print("Attention: file already existed!")
  }
  if (file.exists(f_igi) == F) {
    write.table(
      mat,
      paste("./IGI", instance, ".csv", sep = ""),
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
################ END IGI ####################
# ===========================================