##------ Wed Mar 06 11:08:01 2019 ------##

# =======================================
# Title: MSR heuristic for the SSP-NPM
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
setwd("E:/Dorothea/EURO/SSP-NPM-II")

# install required packages if not installed
list.of.packages <- c("naturalsort", "tictoc","R.utils","matrixStats","foreach","parallel","doParallel")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# load the required packages
library(matrixStats)
library(R.utils)
library(tictoc)
library("naturalsort")
library(foreach)
library(parallel)
library(doParallel)

# required function that includes sample of length one
resamp <- function(x,...){if(length(x)==1) x else sample(x,...)} 

numCores <- detectCores()
registerDoParallel(numCores)

### !!!Compute this before running the process!!! ###
csv_files <- list.files(pattern = 'ins*',all.files = T,full.names = T)
csv_files <- naturalsort(csv_files)
data.list <- lapply(csv_files,read.table,sep=";",header=F)

# ===========================================
################## Start  ######################
# ===========================================

# number of iterations
i = 1000

# for each instance
for (instance in 1:length(data.list)){
#for(instance in 224:length(data.list)){
#foreach (instance=224:length(data.list))  %dopar% {
  
  # #### best known solutions ####
  # bks <- matrix(ncol=4,nrow=10)
  # colnames(bks) <- c("bks_ts","bks_tft","bks_fmax","time")
  # row.names(bks) <- c(100,200,300,400,500,600,700,800,900,1000)
  # best known solutions (bks)
  bks_ts <- Inf
  bks_tft <- Inf
  bks_fmax <-  Inf
  
  #### retrieve input parameters from data frame
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
  
  ### Capacity cap_m[[m]] of machine m
  cap_m <- list()
  for (m in 1:max_m){
    cap_m[[m]] <- (df[2,m])
  }
  remove(m)
  
  ### Processing Times p_m[[m]] of the jobs on machine m
  p_m <- list()
  for(m in 1:max_m){
    p_m[[m]] <- df[(3+m),(1:max_j)]
  }
  remove(m)
  
  # initialisation job sequence pi_m on machine m
  pi_m_rand <- data.frame(matrix(ncol=max_m,nrow=max_j))
  colnames(pi_m_rand) <- c(1:max_m)
  
  ### Switching Times sw_m[m] of machine m
  sw_m <- array()
  sw_m[1:max_m] <- unlist(df[3,1:max_m])
  
  # Initial completion time f[m] on machine m
  f <- array()
  f[1:max_m] <- 0
  
  ##############################################
  
  tic("rand")
  # tic("1000")
  # tic("900")
  # tic("800")
  # tic("700")
  # tic("600")
  # tic("500")
  # tic("400")
  # tic("300")
  # tic("200")
  # tic("100")

for (iteration in 1:i){
# shuffle the possible sequence randomly
  pi_help <- sample (1:max_j)
  m_pos <- 1

  j <- 1
  while(j <= max_j) {
    if(sum(matrix[,pi_help[j]]) <= cap_m[[m_pos]]){
      pi_m_rand[j,m_pos] <- pi_help[j]
      m_pos <- m_pos+1
      # if all machines have been considered, start again with first machine
      if (m_pos > max_m){
        m_pos <- 1
      }
    }
    # if job does not fit on machine
    if (sum(matrix[,pi_help[j]]) > cap_m[[m_pos]]) {
      # assign job to a suitable random machine
      suit_m <- which(cap_m >= sum(matrix[,pi_help[j]]))
      suit_m <- resamp(suit_m,1)
      pi_m_rand[j,suit_m] <- pi_help[j]
    }
    j <- j+1
  }
  remove(j)
  remove(m_pos)
  
  l_rand <- list()
  for (m in 1:max_m){
    l_rand[[m]] <- (pi_m_rand[,m][!is.na(pi_m_rand[,m])])
  }
  remove(m)
  
  ####################################
  
  # ========================================
  # Tool Loading and keep tool needed soones (KTNS)
  # ========================================
  
  t_req_m <- list()
  for (m in 1:max_m) {
    if(length(l_rand[[m]])==1){
      t_req_m[[m]] <- unlist(req_t[l_rand[[m]][1]])
      t_req_m[[m]] <- append(t_req_m[[m]],setdiff(c(1:max_t),t_req_m[[m]]))
    }
    else{
      t_req_m[[m]] <- unlist(union(NULL,as.vector(unlist(req_t[l_rand[[m]][-1]]))))
      if(length(t_req_m[[m]]) < cap_m[[m]]){
        t_req_m[[m]] <- append(t_req_m[[m]],setdiff(c(1:max_t),t_req_m[[m]]))
      }
    }
  }
  remove(m)
  
  # sequence of all tools that are still required
  all_t_req_m <- list()
  for (m in 1:max_m) {
    all_t_req_m[[m]] <- unlist(req_t[l_rand[[m]][-1]])
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
    active_j <- l_rand[[m]][1]
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
    if (length(l_rand[[m]]) == 1) {
      active_j <- l_rand[[m]][1]
      ct[active_j] <- p_m[[m]][active_j]
    } else {
      for (active_j_pos in 2:length(l_rand[[m]])) {
        active_j <- l_rand[[m]][active_j_pos]
        previous_j <- l_rand[[m]][active_j_pos-1]
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

  # compute objectives
  # ============
  ### number of tool switches
  switch <- sum(ts_j[!is.na(ts_j)])
  ### makespan
  fmax <- Reduce(max,ct)
  ### total flow time
  tft <- Reduce("+",ct)
  ### job assignment
  seq <- array()
  seq[1:max_m] <- l_rand[1:max_m]
  seq <- capture.output(cat(paste0(strwrap(seq))))
  ### tool loading 
  loads <- array()
  loads[1:max_j] <- load_t[1:max_j]
  loads <- capture.output(cat(paste0(strwrap(loads))))
  # ============

  # if objective value is better than bks - update bks
  if(switch < bks_ts){
    bks_ts <- switch
    ts_seq <- seq
    ts_loads <- loads
  }
  if(fmax < bks_fmax){
    bks_fmax <- fmax
    fmax_seq <- seq
    fmax_loads <- loads
  }
  if(tft < bks_tft){
    bks_tft <- tft
    tft_seq <- seq
    tft_loads <- loads
  }
# 
#   # Write BKS after iteration 100*1000
# 
#   if(iteration==100){
#     toc(log=T,quiet = T)
#     time100 <- tic.log(format=T)
#     bks[1,1] <- bks_ts
#     bks[1,2] <- bks_tft
#     bks[1,3] <- bks_fmax
#     bks[1,4] <- paste0(time100[[1]])
#     tic.clearlog()
#   }
#   if(iteration==200){
#     toc(log = T,quiet = T)
#     time200 <- tic.log(format=T)
#     bks[2,1] <- bks_ts
#     bks[2,2] <- bks_tft
#     bks[2,3] <- bks_fmax
#     bks[2,4] <- paste0(time200[[1]])
#     tic.clearlog()
#   }
#   if(iteration==300){
#     toc(log = T,quiet = T)
#     time300 <- tic.log(format=T)
#     bks[3,1] <- bks_ts
#     bks[3,2] <- bks_tft
#     bks[3,3] <- bks_fmax
#     bks[3,4] <- paste0(time300[[1]])
#     tic.clearlog()
#   }
#   if(iteration==400){
#     toc(log = T,quiet = T)
#     time400 <- tic.log(format=T)
#     bks[4,1] <- bks_ts
#     bks[4,2] <- bks_tft
#     bks[4,3] <- bks_fmax
#     bks[4,4] <- paste0(time400[1])
#     tic.clearlog()
#   }
#   if(iteration==500){
#     toc(log = T,quiet = T)
#     time500 <- tic.log(format=T)
#     bks[5,1] <- bks_ts
#     bks[5,2] <- bks_tft
#     bks[5,3] <- bks_fmax
#     bks[5,4] <- paste0(time500[[1]])
#     tic.clearlog()
#   }
#   if(iteration==600){
#     toc(log = T,quiet = T)
#     time600 <- tic.log(format=T)
#     bks[6,1] <- bks_ts
#     bks[6,2] <- bks_tft
#     bks[6,3] <- bks_fmax
#     bks[6,4] <- paste0(time600[1])
#     tic.clearlog()
#   }
#   if(iteration==700){
#     toc(log = T,quiet = T)
#     time700=tic.log(format=T)
#     bks[7,1]=bks_ts
#     bks[7,2]=bks_tft
#     bks[7,3]=bks_fmax
#     bks[7,4]=paste0(time700[[1]])
#     tic.clearlog()
#   }
#   if(iteration==800){
#     toc(log = T,quiet = T)
#     time800=tic.log(format=T)
#     bks[8,1]=bks_ts
#     bks[8,2]=bks_tft
#     bks[8,3]=bks_fmax
#     bks[8,4]=paste0(time800[[1]])
#     tic.clearlog()
#   }
#   if(iteration==900){
#     toc(log = T,quiet = T)
#     time900=tic.log(format=T)
#     bks[9,1]=bks_ts
#     bks[9,2]=bks_tft
#     bks[9,3]=bks_fmax
#     bks[9,4]=paste0(time900[[1]])
#     tic.clearlog()
#   }
#   if(iteration==1000){
#     toc(log = T,quiet = T)
#     time1000=tic.log(format=T)
#     bks[10,1]=bks_ts
#     bks[10,2]=bks_tft
#     bks[10,3]=bks_fmax
#     bks[10,4]=paste0(time1000[[1]])
#     tic.clearlog()
#   }
}
  
  
  
  toc(log = T,quiet = T)
  c_time <- tic.log(format=T)
  tic.clear()
  tic.clearlog()

  # =====================================
  # Output
  # =====================================
  # Summary of result to file
  # setwd("E:/Dorothea/EURO/Results-II")
  # f2 <- paste0("./Rand_bks",instance,".csv",sep=";")
  # write.table(bks,paste("./Rand_bks",instance,".csv",sep = ""),sep=";",row.names = T,col.names = F)
  
  mat <- as.matrix(rbind(bks_ts,bks_tft,bks_fmax,c_time[1],ts_seq,ts_loads,fmax_seq,fmax_loads,tft_seq,tft_loads))
  f3 = paste0("./Rand",instance,".csv",sep="")
  if (file.exists(f3)==T) {
    print("file already exists")
  }
  if (file.exists(f3)==F){
    write.table(mat,paste("./Rand",instance,".csv",sep = ""),sep=",",row.names = T,col.names = F)
  }

  rm(list=ls()[! ls() %in% c("data.list","csv_files","instance","i","resamp","numCores")])
}

