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
############### Start IEACT #################
# ===========================================

# for all problem instances in the data set (default: example_instance)
for (instance in 1:length(data.list)) {
  # ===============
  # Get input data
  # ===============
  
  #### retrieve input parameters from data frame
  df <- as.data.frame(data.list[[instance]])
  colnames(df) <- c(1:length(df[1,]))     # Columns
  row.names(df) <- c(1:length(df[, 1]))    # Rows
  
  #### Initialization of parameters & sets (jobs, machines, tools, req_tools, capacity)
  max_j <- df[1, 2]    ### Number of jobs @max_j
  max_m <- df[1, 1]    ### Number of machines @max_m
  
  ### Tools
  max_t <- df[1, 3]    # Number of tools @max_t
  
  req_t <- list()    # Set of tools required for job j @req_T
  for (j in 1:max_j) {
    req_t[[j]] <-
      which(df[((4 + max_m):(4 + max_m + max_t - 1)), j] == 1)
  }
  remove(j)
  
  # job tool matrix (required for tool switches) @matrix
  matrix <- df[((4 + max_m):length(df[, 1])), (1:max_j)]
  row.names(matrix) <- c(1:max_t)
  colnames(matrix) <- c(1:max_j)
  
  ### Capacity cap_m[[m]] of machine m
  cap_m <- list()
  for (m in 1:max_m) {
    cap_m[[m]] <- (df[2, m])
  }
  remove(m)
  
  ### Processing Times of the jobs on machine m @p_m[[m]]
  p_m <- list()
  for (m in 1:max_m) {
    p_m[[m]] <- df[(3 + m), (1:max_j)]
  }
  remove(m)
  
  ### Switching Times of machine m @sw_m[m]
  sw_m <- array()
  sw_m[1:max_m] <- unlist(df[3, 1:max_m])
  
  ### Initial completion time of the last job of machine m @f[m]
  f <- array()
  f[1:max_m] <- 0
  
  # job tool matrix (required for tool switches)
  matrix <- df[((4 + max_m):length(df[, 1])), (1:max_j)]
  row.names(matrix) <- c(1:max_t)
  colnames(matrix) <- c(1:max_j)
  # auxiliary matrix required to track jobs to be scheduled
  jt_matrix <- matrix
  jt_matrix_new <- matrix
  ##############################################
  
  # =====================================
  ########## IGI algorithm ############
  # =====================================
  
  #### additional input parameters and sets ####
  # auxiliary list for processing times
  # p_m_IGI <- p_m
  # initialisation job sequence pi_m on machine m
  pi_m_IGI <- data.frame(matrix(ncol = max_m, nrow = max_j))
  colnames(pi_m_IGI) <- c(1:max_m)
  
  # ============
  ##### Start of computation time counter
  tic("IGI")
  
  # ============
  # Job Assignment
  # ============
  
  # unassigned jobs j_left that still have to be scheduled, auxiliary job set aux_j
  aux_j <- array(1:max_j)
  j_left <- aux_j
  
  # number of required tools per job sum_t[j], required for initial loading
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
    poss_m[1:max_m] <- F
    
    # if job still possible (tool constraint) set machine to suitable
    for (job in j_left) {
      for (machine in 1:max_m) {
        if (cap_m[machine] >= sum(matrix[, job]))
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
        j_fit[[job]] <- which(sum(matrix[, job]) <= cap_m[free_m])
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
      f[free_m] <- f[free_m] + p_m[[free_m]][j_tmax]
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
        j_fit[[job]] <- which(sum(matrix[, job]) <= cap_m[free_m])
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
            f[free_m] + p_m[[free_m]][j_tmax] + (sw_m[[free_m]][1] * ts_approx)
        }
        if (ts_approx == 0) {
          f[free_m] <- f[free_m] + p_m[[free_m]][j_tmax]
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
  for (m in 1:max_m) {
    l_IGI[[m]] <- (pi_m_IGI[, m][!is.na(pi_m_IGI[, m])])
  }
  remove(m)
  
  ####################################
  
  # ========================================
  # Tool Loading and keep tool needed soones (KTNS)
  # ========================================
  
  ### initialization of lists requried for KTNS
  
  # list of UNIQUE tools that are still required
  t_req_m <- list()
  for (m in 1:max_m) {
    # if the machine [[m]] is used by at least one job
    if (sum(l_IGI[[m]]) > 0) {
      # if there is only one job on that machine
      if (length(l_IGI[[m]]) == 1) {
        # add the required tools to the tool list
        t_req_m[[m]] <- unlist(req_t[l_IGI[[m]][1]])
        # add other possible tools
        # it is assumed that the tool magazine is always full
        # this assumption was made based on a real-world example
        # this does neither hinder nor influence the correct output of the construction heuristic or the ILS
        t_req_m[[m]] <-
          append(t_req_m[[m]], setdiff(c(1:max_t), t_req_m[[m]]))
      }
      else{
        # append the required tools without the tools for the first job
        # the tool for the first job will be assigned anyways in step ### initial loading 
        t_req_m[[m]] <-
          unlist(union(NULL, as.vector(unlist(req_t[l_IGI[[m]][-1]]))))
        if (length(t_req_m[[m]]) < cap_m[[m]]) {
          t_req_m[[m]] <-
            append(t_req_m[[m]], setdiff(c(1:max_t), t_req_m[[m]]))
        }
      }
    }
    # if there is no job on that machine
    else{
      warning(paste0("No jobs assigned to machine ",m,"!"))
      t_req_m[[m]] <- NA
    }
  }
  remove(m)
  
  # list of ALL tools that are still required
  all_t_req_m <- list()
  for (m in 1:max_m) {
    all_t_req_m[[m]] <- unlist(req_t[l_IGI[[m]][-1]])
  }
  remove(m)
  
  # auxiliary lists for the loading
  load_t <- req_t
  aux_req <- t_req_m
  aux_all_req <- all_t_req_m
  
  ### completion time of job j
  ct <- list()
  # number of tools that are required and not in the magazine
  ts_j <- array(dim <- max_j)
  
  ### initial loading (first jobs)
  # at first, the initial loading for the first job is defined, which makes future tool comparisons easier
  # for all machines..
  for (m in 1:max_m) {
    # ..with jobs assigned
    if (sum(t_req_m[[m]]) > 0) {
      active_j <- l_IGI[[m]][1]
      # no tool switches for the first job
      ts_j[active_j] <- 0
      # if number of required tools equal to tool magazine capacity
      if (length(req_t[[active_j]]) == cap_m[[m]]) {
        # update completion time
        ct[active_j] <- p_m[[m]][active_j]
        
        # if free slot available
      } else if (length((req_t[[active_j]])) < cap_m[[m]]) {
        
        # tool position in the array of the tools in common with future required tools
        tool_pos <- match(req_t[[active_j]], aux_req[[m]])
        # without tools, never used again
        tool_pos <- tool_pos[!is.na(tool_pos)]
        # remove the tools already in the magazine
        if (sum(tool_pos) > 0) {
          aux_req[[m]] <- aux_req[[m]][-tool_pos]
        }
        remove(tool_pos)
        # add tool required the soonest in the free slot
        for (free_slot in (length(req_t[[active_j]]) + 1):cap_m[[m]]) {
          # put next tool in free slot that is required earliest..
          # ..but only if it is not already used by first job
          load_t[[active_j]][free_slot] <- aux_req[[m]][1]
          # remove not inserted // update auxiliary required tool list
          aux_req[[m]] <- aux_req[[m]][-1]
          # update completion time
          ct[active_j] <- p_m[[m]][active_j]
        }
        remove(free_slot)
      }
    }
    remove(active_j)
  }
  remove(m)
  
  ### tool optimization for remaining jobs
  for (m in 1:max_m) {
    # if there is only one job in the sequence on machine m
    if (length(l_IGI[[m]]) == 1) {
      active_j <- l_IGI[[m]][1]
      ct[active_j] <- p_m[[m]][active_j]
    } else {
      for (active_j_pos in 2:length(l_IGI[[m]])) {
        active_j <- l_IGI[[m]][active_j_pos]
        previous_j <- l_IGI[[m]][active_j_pos - 1]
        # new tools required
        not_match <-
          which(!is.na(match(req_t[[active_j]], load_t[[previous_j]])) == F)
        # required tools that are already loaded
        yes_match <-
          which(!is.na(match(req_t[[active_j]], load_t[[previous_j]])) == T)
        # at most required tool switches
        # !!!
        ts_j[active_j] <- length(not_match)
        # if no tools are to be replaced because required tools are already loaded
        if (length(yes_match) == length(req_t[[active_j]])) {
          load_t[[active_j]] <- load_t[[previous_j]]
          aux_all_req[[m]] <-
            aux_all_req[[m]][-(1:length(req_t[[active_j]]))]
          ct[active_j] <- ct[previous_j] + p_m[[m]][active_j]
        }
        # if tools need to be replaced
        else if (length(yes_match) < length(req_t[[active_j]])) {
          # if all tools need to be replaced
          if (ts_j[active_j] == cap_m[[m]]) {
            load_t[[active_j]] <- req_t[[active_j]]
            aux_all_req[[m]] <-
              aux_all_req[[m]][-(1:length(req_t[[active_j]]))]
            ct[active_j] <-
              ct[previous_j] + p_m[[m]][active_j] + ts_j[active_j] * sw_m[[m]][1]
          }
          # if any tools need to be replaced (less than magazine capacity)
          else if (ts_j[active_j] < cap_m[[m]]) {
            load_t[[active_j]] <- req_t[[active_j]]
            aux_all_req[[m]] <-
              aux_all_req[[m]][-(1:length(req_t[[active_j]]))]
            # which tools are already loaded and later required
            inta <-
              intersect(load_t[[previous_j]], aux_all_req[[m]])
            # which tools can theoretically be kept
            inta <- setdiff(inta, load_t[[active_j]])
            # if there are more free slots than tools required in the future and not in magazine
            if ((cap_m[[m]] - length(req_t[[active_j]])) > length(inta)) {
              # replace the free slots with tools needed soonest (and not in the magazin)
              load_t[[active_j]] <- append(load_t[[active_j]], inta)
              # randomly keep old tools until magazin is full
              old_t <-
                setdiff(load_t[[previous_j]], union(load_t[[active_j]], aux_all_req[[m]]))
              load_t[[active_j]] <-
                append(load_t[[active_j]], old_t[1:(cap_m[[m]] - length(load_t[[active_j]]))])
              ct[active_j] <-
                ct[previous_j] + p_m[[m]][active_j] + ts_j[active_j] * sw_m[[m]][1]
              remove(old_t)
            }
            # if job needs exactly cap tool
            else if ((cap_m[[m]] - length(req_t[[active_j]])) == 0) {
              load_t[[active_j]] = req_t[[active_j]]
              ct[active_j] = ct[previous_j] + p_m[[m]][active_j] + ts_j[active_j] * sw_m[[m]][1]
              
            }
            # if there are less free slots than tools required in future and not in magazine
            else if ((cap_m[[m]] - length(req_t[[active_j]])) <= length(inta) &
                     (cap_m[[m]] - length(req_t[[active_j]])) > 0) {
              # replace the free slots with tools needed soonest (and not in the magazin)
              load_t[[active_j]] <-
                append(load_t[[active_j]], inta[1:(cap_m[[m]] - length(req_t[[active_j]]))])
              ct[active_j] <-
                ct[previous_j] + p_m[[m]][active_j] + ts_j[active_j] * sw_m[[m]][1]
            }
          }
        }
      }
    }
  }
  remove(m)
  remove(active_j_pos)
  
  # ========================================
  
  toc(log = T, quiet = T)
  c_time <- tic.log(format = T)
  tic.clear()
  tic.clearlog()
  
  # =====================================
  # Output
  # =====================================
  
  ### number of tool switches
  switch <- sum(ts_j[!is.na(ts_j)])
  ### makespan
  fmax <- Reduce(max, ct)
  ### total flow time
  tft <- Reduce("+", ct)
  ### job assignment
  seq <- array()
  seq[1:max_m] <- l_IGI[1:max_m]
  seq <- capture.output(cat(paste0(strwrap(seq))))
  ### tool loading
  loads <- array()
  loads[1:max_j] <- load_t[1:max_j]
  loads <- capture.output(cat(paste0(strwrap(loads))))
  
  # write solution of the construction heuristic (IGI) to file
  setwd("results/example_results/")
  mat <- as.matrix(rbind(switch, tft, fmax, c_time[1], seq, loads))
  f_igi <- paste0("./IGI", instance, ".csv", sep = "")
  if (file.exists(f_igi) == T) {
    print("file already exists")
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
  rm(list = ls()[!ls() %in% c("data.list", "csv_files", "instance", "resamp")])
}

# ===========================================
################ END IGI ####################
# ===========================================