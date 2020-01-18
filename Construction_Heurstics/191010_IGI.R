##------ Thu Oct 10 10:47:15 2019 ------##

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
# param heur Type of Heuristic
# @heur = {GI // "tool switches objective",SPT // "makespan & flowtime objective}

# =======================================
# file and package preparation
# =======================================

# set working directory
# setwd("C:/Users/Administrator/Dropbox/EURO/Instances")
setwd("C:/Users/Administrator/Dropbox/Las_Papers/SSP-NPM-I")
setwd("E:/Dorothea/Las Paper/SSP-NPM-I/")

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
for (instance in 1:length(data.list)){

  #### retrieve input parameters from data frame ####
  df <- as.data.frame(data.list[[instance]])
  # Columns
  colnames(df) <- c(1:length(df[1,]))
  # Rows
  row.names(df) <- c(1:length(df[,1]))
  
  #### Initialization of parameters & sets (jobs, machines, tools, req_tools, capacity)
  ### Number of jobs
  max_j <- df[1,2]
  
  ### Number of machines
  max_m <- df[1,1]
  
  ### Tools
  # Number of tools
  max_t <- df[1,3]
  # Set of tools required for job j
  req_t <- list()
  for (j in 1:max_j){
    req_t[[j]] <- which(df[((4+max_m):(4+max_m+max_t-1)),j]==1)
  }
  remove(j)
  
  # job tool matrix (required for tool switches)
  matrix <- df[((4+max_m):length(df[,1])),(1:max_j)]
  row.names(matrix) <- c(1:max_t)
  colnames(matrix) <- c(1:max_j)
  
  #      ### Capacity cap_m[[m]] of machine m
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
  
  # auxiliary matrix required to track jobs to be scheduled
  jt_matrix <- matrix
  jt_matrix_new <- matrix
  
  ##############################################
  
  # =====================================
  ########## IGI algorithm ############
  # =====================================
  
  ##### Start of computation time counter ####
  tic("GI")
  #### additional input parameters and sets ####
  # auxiliary list for processing times
  p_m_gi <- p_m
  # initialisation job sequence pi_m on machine m
  pi_m_GI <- data.frame(matrix(ncol=max_m,nrow=max_j))
  colnames(pi_m_GI) <- c(1:max_m)
  
  # ============
  # Job Assignment
  # ============   
  
  sum_t <- array()
  # only jobs that still have to be scheduled
  left_j <- array(1:max_j)
  j_left <- left_j
  for (i in left_j) {
    sum_t[i] <- sum(jt_matrix[,i])
  }
  remove(i)

  while (sum(j_left)>0){
    # select suitable machines
    # set all machines to unsuitable
    poss_m <- list()
    poss_m[1:max_m] <- F
    
    # states which machine is free
    for(job in j_left){
      for (machine in 1:max_m){
        if (cap_m[machine] >= sum(matrix[,job]))
          poss_m[machine] <- T
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
    # only for the jobs being processed on the free machine
    df_part <- p_m_gi[free_m]
    # first job is the job with maximum number of tools required
    if (f[free_m]==0) {
      sum_t[is.na(sum_t)] <- 0
      j_fit <- list()
      
      for (job in j_left){
      j_fit[[job]] <- which(sum(matrix[,job]) <= cap_m[free_m])}
      remove(job)
      
      for (job in 1:length(j_fit)){
        j_fit[[job]] <- sum(j_fit[[job]])
      }
      remove(job)
      
      j_fit <- which(j_fit==1)
      # select the fitting jobs that require the highest number of tools
      j_tmaxs <- j_fit[which(sum_t[j_fit]==max(sum_t[j_fit]))]
      # if there are multiple jobs with the same tmax, randomly select one
      j_tmax <- resamp(j_tmaxs,1)
      # assign that job to a free machine in postion length +1
      pi_m_GI[(length(pi_m_GI[,free_m][!is.na(pi_m_GI[,free_m])])+1),free_m] <- j_tmax
      # update completion time of that job on machine m
      f[free_m] <- f[free_m]+p_m[[free_m]][j_tmax]
      # remove job from data frame
      left_j[j_tmax] <- NA
      jt_matrix_new <- jt_matrix_new[,-which(colnames(jt_matrix_new) == j_tmax)]
      sum_t[j_tmax] <- 0
      # otherwise select fitting job that has the highest number of tools in common with the previous job
    } else {
      sum_intersections <- array()
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
      #if there are more than 1 jobs left
        if(length(j_fit)>1){
        #number of jobs already assigned
        n <- length(which(!is.na(pi_m_GI[,free_m])))

        for (k in which(left_j != is.na(left_j))){
          sum_intersections[k] <- length(which((jt_matrix[,pi_m_GI[n,free_m]]+jt_matrix[,k])=="2"))
          # if there are multiple jobs with the same tmax, select first job
        }
        fu <- which(sum_intersections >=0)
        fu <- intersect(j_fit,fu)
        # select fitting job with the maximum number of intersections
        j_tmaxs <- j_fit[which(sum_intersections[fu]==max(sum_intersections[fu]))]
        j_tmax <- resamp(j_tmaxs,1)
        
        ts_approx <- length(which((jt_matrix[,j_tmax]-jt_matrix[,n])=="-1"))
        # assign that job to a free machine in postion length +1
        pi_m_GI[(length(pi_m_GI[,free_m][!is.na(pi_m_GI[,free_m])])+1),free_m]=j_tmax
        # update completion time of that job on machine m
        if(ts_approx > 0){
          f[free_m] <- f[free_m]+p_m[[free_m]][j_tmax] + (sw_m[[free_m]][1]*ts_approx)
        }
        if(ts_approx == 0){
          f[free_m] <- f[free_m]+p_m[[free_m]][j_tmax]
        }
        # remove job from data frame
        left_j[j_tmax] <- NA
        jt_matrix_new <- jt_matrix_new[,-which(colnames(jt_matrix_new) == j_tmax)]
        
        #if there is only 1 job left
      } else if (length(j_fit)==1){
        # last job to be sequenced on free machine 
        j_tmax <- j_fit
        pi_m_GI[(length(pi_m_GI[,free_m][!is.na(pi_m_GI[,free_m])])+1),free_m]=j_tmax
        left_j[j_tmax]=NA
      }
    }
    remove(free_m)
    j_left <- which(!is.na(left_j))
  } 
  remove(k)
  
  ##############################################
  
  #### Job Sequence - Solution ####
  
  # print sequence on each machine without NA  
  l_GI <- list()
  for (m in 1:max_m){
    l_GI[[m]] <- (pi_m_GI[,m][!is.na(pi_m_GI[,m])])
  }
  remove(m)
  
  ####################################
  
  # ========================================
  # Tool Loading and keep tool needed soones (KTNS)
  # ========================================
  
  # sequence of unique tools that are still required
  t_req_m <- list()
  for (m in 1:max_m) {
    if(length(l_GI[[m]])==1){
      t_req_m[[m]] <- unlist(req_t[l_GI[[m]][1]])
      t_req_m[[m]] <- append(t_req_m[[m]],setdiff(c(1:max_t),t_req_m[[m]]))
    }
    else{
      t_req_m[[m]] <- unlist(union(NULL,as.vector(unlist(req_t[l_GI[[m]][-1]]))))
      if(length(t_req_m[[m]]) < cap_m[[m]]){
        t_req_m[[m]] <- append(t_req_m[[m]],setdiff(c(1:max_t),t_req_m[[m]]))
      }
    }
  }
  remove(m)
  
  # sequence of all tools that are still required @all_t_req_m[[m]]
  # because it can happen that a previously loaded tool had to be removed
  all_t_req_m <- list()
  for (m in 1:max_m) {
    all_t_req_m[[m]] <- unlist(req_t[l_GI[[m]][-1]])   # without first jobs
  }
  remove(m)
  
  ### auxiliary parameters for the loading
  load_t <- req_t       # tool loading @load_t
  aux_req <- t_req_m    # unique tools required @aux_req[[m]]
  aux_all_req <- all_t_req_m    # all tools required @aux_all_req[[m]]
  
  ### important variables
  ct <- list()    # completion time of job j @ct[j]
  ts_j <- array(0,dim <- max_j)    # number of required tools not in the magazine @ts_j[j]
  
  ### initial loading (first jobs)
  for (m in 1:max_m) {
    active_j <- l_GI[[m]][1]   # active job active_j
    
    # if no free slot
    if (length(req_t[[active_j]]) == cap_m[[m]]) {
      ct[active_j] <- p_m[[m]][active_j]
      # if free slot available
    } else if (length((req_t[[active_j]])) < cap_m[[m]]) {
      hu <- match(req_t[[active_j]],aux_req[[m]])        # tools still required for other jobs
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
  
  remove(hu,m,active_j,free_slot)
  
  ### tool optimization 
  for (m in 1:max_m) {
    if (length(l_GI[[m]]) == 1) {
      active_j <- l_GI[[m]][1]
      ct[active_j] <- p_m[[m]][active_j]
    } else {
      for (active_j_pos in 2:length(l_GI[[m]])) {
        active_j <- l_GI[[m]][active_j_pos]
        previous_j <- l_GI[[m]][active_j_pos-1]
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
  seq <- l_GI
  seq <- capture.output(cat(paste0(strwrap(seq))))
  ### tool loading 
  loads <- load_t
  loads <- capture.output(cat(paste0(strwrap(loads))))

  # write solution to file

  mat=as.matrix(rbind(switch,tft,fmax,c_time[1],seq,loads))
  f3 = paste0("./IGI",instance,".csv",sep="")
  if (file.exists(f3)==T){
    print("file already exists")
  }
  if (file.exists(f3)==F){
    write.table(mat,paste("./IGI",instance,".csv",sep = ""),sep=",",row.names = T,col.names = F)
  }
  
  # remove all variables and parameters except instances and instance counter
  rm(list=ls()[! ls() %in% c("data.list","csv_files","instance","resamp")])
  }



  