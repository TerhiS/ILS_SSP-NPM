##------ Thu May 02 15:33:18 2019 ------##

# =======================================
# Title: IEACT construction heuristic for the SSP-NPM
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
# param heur Type of Heuristic
# @heur = {GI // "tool switches objective",SPT // "makespan & flowtime objective}

# =======================================
# file and package preparation
# =======================================

# set working directory
# setwd("C:/Users/Administrator/Dropbox/EURO/Instances")
setwd("C:/Users/Dorothea/Dropbox/Math_Model")
setwd("E:/Dorothea/Math")
setwd("E:/Dorothea/EURO/SSP-NPM-II")
# install required packages if not installed
list.of.packages <- c("naturalsort", "tictoc","R.utils","matrixStats")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# load the required packages
library(matrixStats)
library(R.utils)
library(tictoc)
library("naturalsort")

# required function that includes sample of length one
resamp <- function(x,...){if(length(x)==1) x else sample(x,...)} 

### !!!Compute this before running the process!!! ###
csv_files <- list.files(pattern = 'ins*',all.files = T,full.names = T)
csv_files <- naturalsort(csv_files)
data.list <- lapply(csv_files,read.table,sep=";",header=F)

# ===========================================
################## Start  ######################
# ===========================================

# ===========================================
######## Start of getting input data ########

for (instance in 909:length(data.list)){
#for (instance in 121){
  
  #### retrieve input parameters from data frame
  df <- as.data.frame(data.list[[instance]])
  colnames(df) <- c(1:length(df[1,]))     # Columns
  row.names(df) <- c(1:length(df[,1]))    # Rows
  
  #### Initialization of parameters & sets (jobs, machines, tools, req_tools, capacity)
  max_j <- df[1,2]    ### Number of jobs @max_j
  max_m <- df[1,1]    ### Number of machines @max_m
  
  ### Tools
  max_t <- df[1,3]    # Number of tools @max_t
  
  req_t <- list()    # Set of tools required for job j @req_T
  for (j in 1:max_j){
    req_t[[j]] <- which(df[((4+max_m):(4+max_m+max_t-1)),j]==1)
  }
  remove(j)
  
  # job tool matrix (required for tool switches) @matrix
  matrix <- df[((4+max_m):length(df[,1])),(1:max_j)]
  row.names(matrix) <- c(1:max_t)
  colnames(matrix) <- c(1:max_j)
  
  ### Capacity cap_m[[m]] of machine m
  cap_m <- list()
  for (m in 1:max_m){
    cap_m[[m]] <- (df[2,m])
  }
  remove(m)
  
  ### Processing Times of the jobs on machine m @p_m[[m]] 
  p_m <- list()
  for(m in 1:max_m){
    p_m[[m]] <- df[(3+m),(1:max_j)]
  }
  remove(m)
  
  ### Switching Times of machine m @sw_m[m] 
  sw_m <- array()
  sw_m[1:max_m] <- unlist(df[3,1:max_m])
  
  ### Initial makespan of machine m @f[m]
  f <- array()
  f[1:max_m] <- 0
  
  ##############################################
  
  
  # job tool matrix (required for tool switches)
  matrix <- df[((4+max_m):length(df[,1])),(1:max_j)]
  row.names(matrix) <- c(1:max_t)
  colnames(matrix) <- c(1:max_j)
  # auxiliary matrix required to track jobs to be scheduled
  jt_matrix <- matrix
  
  # =====================================
  ########## IEACT algorithm ############
  # =====================================
  
  #### additional input parameters and sets ####
  # auxiliary list for processing times
  p_m_spt <- p_m
  # initialisation job sequence pi_m on machine m
  pi_m_SPT <- data.frame(matrix(ncol=max_m,nrow=max_j))
  colnames(pi_m_SPT) <- c(1:max_m)
  
  # ============
  # Job Assignment
  # ============
  
  ##### Start of computation time counter ####
  tic("SPT")
  
  # unassigned jobs with the help of setting the processing time of processed jobs to infinity
  j_left <- which(p_m_spt[[1]] != Inf)
  
  # while still jobs to process
  while (sum(j_left)>0){
    # select suitable machines
    # set all machines to unsuitable
    poss_m <- list()
    poss_m[1:max_m] = F
    
    # if job still possible (tool constraint) set machine to suitable
    j_left <- which(p_m_spt[[1]] != Inf)
    for(job in j_left){
    for (machine in 1:max_m){
      if (cap_m[machine] >= sum(matrix[,job]))
      poss_m[machine] = T
      }
    }
    remove(job)
    remove(machine)
  
    # capacity of selected machine must be large enough to process left jobs and at best with minimum artificial completion time
    free_m <- which(poss_m==T)
    # states which machine(s) is free and suitable    
    # !!!!!!!!!!! Achtung bisher Fehler !!!!!
    free_m <- which(f[]==Reduce(min,f[free_m]))
    # if there are multiple free machines, randomly select one
    free_m <- resamp(free_m,1)
    # processing time of the jobs being processed on the free machine 
    df_part <- p_m_spt[free_m]
    # if job is the first one on that machine (no tool switching required)
    if (f[free_m]==0) {
      # select the job with minimum processing time and fitting number of tools
      j_fit <- list()
      for (job in 1:max_j){
      j_fit[[job]] <- which(sum(matrix[,job]) <= cap_m[free_m])}
      remove(job)
      j_fit <- which(j_fit==1)
      j_mins <- j_fit[which(df_part[[1]][j_fit]==min(df_part[[1]][j_fit]))]
      # if there are multiple jobs with the same p_jm, randomly select one
      j_min <- resamp(j_mins,1)
      # assign that job to a free machine
      pi_m_SPT[(length(pi_m_SPT[,free_m][!is.na(pi_m_SPT[,free_m])])+1),free_m] <- j_min
      # update completion time of that job on machine m (no tool switches for first jobs)
      #!!! cannot take minimum because not all jobs may be fitting
      f[free_m] <- f[free_m]+(df_part[[1]][j_min])
      # remove job from data frame for all machines (set processing time to infinity)
      for (m in 1:max_m){
        p_m_spt[[m]][j_min]=(Inf)
      }
    }

    # if the job is not the first job then tool swtiches have to be considered
    else {
      # of all jobs left 
      previous_j <- pi_m_SPT[(length(pi_m_SPT[,free_m][!is.na(pi_m_SPT[,free_m])])),free_m]
      # add the approximated switching time to the processing time
      for (k in which(df_part[[1]] != Inf)){
        ts_approx <- length(which((jt_matrix[,k]-jt_matrix[,previous_j])=="-1"))
        if(ts_approx > 0){
          df_part[[1]][k] <- df_part[[1]][k]+(sw_m[[free_m]][1]*ts_approx)
        }
        if(ts_approx == 0){
          df_part[[1]][k] <- df_part[[1]][k]
        }
      }
      remove(k)
      # select the job with maximum processing time plus tool switching
      j_fit <- list()
      for (job in j_left){
        j_fit[[job]] <- which(sum(matrix[,job]) <= cap_m[free_m])
      }
      remove(job)
      for (job in 1:length(j_fit)){
        j_fit[[job]] <- sum(j_fit[[job]])
      }
      remove(job)
      j_fit <- which(j_fit==1)
      j_mins <- j_fit[which(df_part[[1]][j_fit]==min(df_part[[1]][j_fit]))]
      # if there are multiple jobs with the same p_jm, select random job
      j_min <- resamp(j_mins,1)
      # assign that job to a free machine
      pi_m_SPT[(length(pi_m_SPT[,free_m][!is.na(pi_m_SPT[,free_m])])+1),free_m] <- j_min
      # update completion time of that job on machine m plus approximated tool switching time
      # !!!
      f[free_m] <- f[free_m]+df_part[[1]][j_min]
      # remove job from data frame for all machines
      for (m in 1:max_m){
        p_m_spt[[m]][j_min]=(Inf)
      }
    }
    remove(free_m)
    j_left <- which(p_m_spt[[1]] != Inf)
  } 
  remove(m)

  ##############################################
  
  #### Job Sequence - Solution ####
  
  # print sequence on each machine without NAs  
  l_SPT <- list()
  for (m in 1:max_m){
    l_SPT[[m]] <- (pi_m_SPT[,m][!is.na(pi_m_SPT[,m])])
  }
  remove(m)
  
  
  ####################################
  
  # ========================================
  # Tool Loading and keep tool needed soones (KTNS)
  # ========================================
  
  t_req_m <- list()
  for (m in 1:max_m) {
    if(length(l_SPT[[m]])==1){
      t_req_m[[m]] <- unlist(req_t[l_SPT[[m]][1]])
      t_req_m[[m]] <- append(t_req_m[[m]],setdiff(c(1:max_t),t_req_m[[m]]))
    }
    else{
      t_req_m[[m]] <- unlist(union(NULL,as.vector(unlist(req_t[l_SPT[[m]][-1]]))))
      if(length(t_req_m[[m]]) < cap_m[[m]]){
        t_req_m[[m]] <- append(t_req_m[[m]],setdiff(c(1:max_t),t_req_m[[m]]))
      }
    }
  }
  remove(m)

  # sequence of all tools that are still required
  all_t_req_m <- list()
  for (m in 1:max_m) {
    all_t_req_m[[m]] <- unlist(req_t[l_SPT[[m]][-1]])
  }
  remove(m)
  
  ### auxiliary parameters for the loading
  load_t <- req_t
  aux_req <- t_req_m
  aux_all_req <- all_t_req_m
  
  ### completion time of job j
  ct <- list()
  # number of tools that are required and not in the magazine
  ts_j <- array(dim <- max_j)
  
  ### initial loading (first jobs)
  for (m in 1:max_m) {
    active_j <- l_SPT[[m]][1]
    ts_j[active_j] <- 0
    # if no free slot
    if (length(req_t[[active_j]]) == cap_m[[m]]) {
      ct[active_j] <- p_m[[m]][active_j]
      # if free slot available
    } else if (length((req_t[[active_j]])) < cap_m[[m]]) {
      hu <- match(req_t[[active_j]],aux_req[[m]])
      hu <- hu[!is.na(hu)]
      if (sum(hu) > 0) {
        aux_req[[m]] <- aux_req[[m]][-hu]
      }
      for (free_slot in (length(req_t[[active_j]])+1):cap_m[[m]]) {
        # put next tool in free slot that is required earliest 
        # but only if it is not in use already by first job
        load_t[[active_j]][free_slot] <- aux_req[[m]][1]
        # remove not inserted
        aux_req[[m]] <- aux_req[[m]][-1]
        ct[active_j] <- p_m[[m]][active_j]
      }
    }
  }
  remove(hu)
  remove(m)
  remove(active_j)
  remove(free_slot)
  
  ### tool optimization 
  for (m in 1:max_m) {
    if (length(l_SPT[[m]]) == 1) {
      active_j <- l_SPT[[m]][1]
      ct[active_j] <- p_m[[m]][active_j]
    } else {
      for (active_j_pos in 2:length(l_SPT[[m]])) {
        active_j <- l_SPT[[m]][active_j_pos]
        previous_j <- l_SPT[[m]][active_j_pos-1]
        not_match <- which(!is.na(match(req_t[[active_j]],load_t[[previous_j]]))==F)
        yes_match <- which(!is.na(match(req_t[[active_j]],load_t[[previous_j]]))==T)
        ts_j[active_j] <- length(not_match)
        # if no tools are to be replaced because required tools are already loaded
        if (length(yes_match) == length(req_t[[active_j]])) {
          load_t[[active_j]] <- load_t[[previous_j]]
          aux_all_req[[m]] <- aux_all_req[[m]][-(1:length(req_t[[active_j]]))]
          ct[active_j] <- ct[previous_j] + p_m[[m]][active_j]
        }
        # if tools need to be replaced
        else if (length(yes_match) < length(req_t[[active_j]])) {
          # if all tools need to be replaced 
          if (ts_j[active_j] == cap_m[[m]]) {
            load_t[[active_j]] <- req_t[[active_j]]
            aux_all_req[[m]] <- aux_all_req[[m]][-(1:length(req_t[[active_j]]))]
            ct[active_j] <- ct[previous_j]+p_m[[m]][active_j]+ts_j[active_j]*sw_m[[m]][1]
          }
          # if any tools need to be replaced (less than magazine capacity)
          else if (ts_j[active_j] < cap_m[[m]]) {
            load_t[[active_j]] <- req_t[[active_j]]
            aux_all_req[[m]] <- aux_all_req[[m]][-(1:length(req_t[[active_j]]))]
            # which tools are already loaded and later required
            inta <- intersect(load_t[[previous_j]],aux_all_req[[m]])
            # which tools can theoretically be kept
            inta <- setdiff(inta,load_t[[active_j]])
            # if there are more free slots than tools required in the future and not in magazine
            if ((cap_m[[m]]-length(req_t[[active_j]])) > length(inta)) {
              # replace the free slots with tools needed soonest (and not in the magazin) 
              load_t[[active_j]] <- append(load_t[[active_j]],inta)
              # randomly keep old tools until magazin is full
              old_t <- setdiff(load_t[[previous_j]],union(load_t[[active_j]],aux_all_req[[m]]))
              load_t[[active_j]] <- append(load_t[[active_j]],old_t[1:(cap_m[[m]]-length(load_t[[active_j]]))])
              ct[active_j] <- ct[previous_j]+p_m[[m]][active_j] + ts_j[active_j]*sw_m[[m]][1]
              remove(old_t)
            }
            # if job needs exactly cap tool
            else if ((cap_m[[m]]-length(req_t[[active_j]])) == 0 ) {
              load_t[[active_j]] = req_t[[active_j]]
              ct[active_j] = ct[previous_j] + p_m[[m]][active_j] + ts_j[active_j] * sw_m[[m]][1]
              
            } 
            # if there are less free slots than tools required in future and not in magazine 
            else if ((cap_m[[m]]-length(req_t[[active_j]])) <= length(inta) & (cap_m[[m]]-length(req_t[[active_j]])) > 0) {
              # replace the free slots with tools needed soonest (and not in the magazin)
              load_t[[active_j]] <- append(load_t[[active_j]],inta[1:(cap_m[[m]] - length(req_t[[active_j]]))])
              ct[active_j] <- ct[previous_j] + p_m[[m]][active_j] + ts_j[active_j] * sw_m[[m]][1]
            }
          }
        }
      }
    }
  }
  remove(m)
  remove(active_j_pos)
  
  # ========================================

  toc(log = T,quiet = T)
  c_time <- tic.log(format=T)
  tic.clear()
  tic.clearlog()
  
  # =====================================
  # Output
  # =====================================
  
  ### number of tool switches
  switch <- sum(ts_j[!is.na(ts_j)])
  ### makespan
  fmax <- Reduce(max,ct)
  ### total flow time
  tft <- Reduce("+",ct)
  ### job assignment
  seq <- array()
  seq[1:max_m] <- l_SPT[1:max_m]
  seq <- capture.output(cat(paste0(strwrap(seq))))
  ### tool loading 
  loads <- array()
  loads[1:max_j] <- load_t[1:max_j]
  loads <- capture.output(cat(paste0(strwrap(loads))))
  
  # write solution to file
  #setwd("E:/Dorothea/EURO/Results-II")
  mat <- as.matrix(rbind(switch,tft,fmax,c_time[1],seq,loads))
  f3 <- paste0("./IEACT",instance,".csv",sep="")
  if (file.exists(f3)==T){
    print("file already exists")
  }
  if (file.exists(f3)==F){
    write.table(mat,paste("./IEACT",instance,".csv",sep = ""),sep=",",row.names = T,col.names = F)
  }
  
  # remove all variables and parameters except instances and instance counter
  rm(list=ls()[! ls() %in% c("data.list","csv_files","instance","resamp")])

  }

  