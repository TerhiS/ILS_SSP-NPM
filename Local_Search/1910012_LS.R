  # =======================================
  # Title: ILS heuristic for the SSP-NPM
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

### !!!Compute this before running the process!!! ###
# working directory of instances
setwd("C:/Users/Dorothea/Dropbox/Las_Papers/SSP-NPM-II")
setwd("E:/Dorothea/Las Paper/SSP-NPM-II")
setwd("C:/Users/Administrator/Dropbox/Math_Model")
setwd("E:/Dorothea/Math")
# load all instances in R
csv_files <- list.files(pattern = 'ins*',all.files = T,full.names = T)
csv_files <- naturalsort(csv_files)
data.list <- lapply(csv_files,read.table,sep=";",header=F)
# working directory of construction heuristic
setwd("C:/Users/Administrator/Dropbox/Math_Model/greedy")
setwd("E:/Dorothea/EURO/Results-I")
setwd("C:/Users/Dorothea/Dropbox/Las_Papers/Results-I")
setwd("E:/Dorothea/Math/greedy")

# ===========================================
################## Start  ######################
# ===========================================
ins=160
obj="flowtime"
strategy="combi"
# for all instances
#for (ins in c(160)){
for (ins in 729:length(data.list)){
  # for all objectives
  for (obj in c(#"flowtime",
                "makespan"
                 #,
                # "switches"
  )){
    # =====================================
    ########## Read Input Data  ###########
    # =====================================
    
    #### Part 1 - Parameters ####
    
    ### type of construction heuristic
    if(obj == "flowtime"){
      heur <- "IEACT"
    }
    
    if(obj == "makespan"){
      heur <- "IEACT"
    }
    
    if(obj == "switches"){
      heur <- "IGI"
    }
    
    #### retrieve input parameters from data frame
    df <- as.data.frame(data.list[[ins]])
    
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
    
    #### Part 2 - Solution ####
    
    #### retrieve initial solution and objective values from construction heuristic 
    solution <- read.csv(paste(heur,ins,".csv",sep = ""),quote = "",header = F,sep = '"',colClasses = "character")
    solution <- as.matrix(solution[,-1])
    
    ### number of tool switches switch
    switch <- as.integer(gsub(solution[1,2],pattern = ",",replacement = ""))
    ### total flow time tft
    tft <- as.integer(gsub(solution[2,2],pattern = ",",replacement = ""))
    ### makespan fmax
    fmax <- as.integer(gsub(solution[3,2],pattern = ",",replacement = ""))
    ### job assignment seq[[m]]
    seq <- strsplit(gsub(solution[5,2],pattern = ",",replacement = ""),"c")
    seq <- strsplit(seq[[1]][-1],'"' )
    seq <- strsplit(gsub("[[:punct:]]", "", seq)," ")
    for (m in 1:max_m){
      seq[[m]] <- as.integer(seq[[m]])
    }
    remove(m)
    
    
    ####################################

    # ========================================
    # Tool Loading and keep tool needed soones (KTNS)
    # ========================================
    
    # sequence of unique tools that are still required
    t_req_m <- list()
    for (m in 1:max_m) {
      if(length(seq[[m]])==1){
        t_req_m[[m]] <- unlist(req_t[seq[[m]][1]])
        t_req_m[[m]] <- append(t_req_m[[m]],setdiff(c(1:max_t),t_req_m[[m]]))
      }
      else{
        t_req_m[[m]] <- unlist(union(NULL,as.vector(unlist(req_t[seq[[m]][-1]]))))
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
      all_t_req_m[[m]] <- unlist(req_t[seq[[m]][-1]])   # without first jobs
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
      active_j <- seq[[m]][1]   # active job active_j
      ts_j[active_j] <- 0
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
      if (length(seq[[m]]) == 1) {
        active_j <- seq[[m]][1]
        ct[active_j] <- p_m[[m]][active_j]
      } else {
        for (active_j_pos in 2:length(seq[[m]])) {
          active_j <- seq[[m]][active_j_pos]
          previous_j <- seq[[m]][active_j_pos-1]
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
    
    #### Part 3 - best known results ####
    ct_opt <- ct    # best known completion times
    ts_opt <- ts_j    # best known tool switches per job
    loads_opt <- load_t    # best known tool loading
    seq_opt <- seq    # best known tool loading
    switch_opt <- switch    # best known total number of tool switches
    fmax_opt <- fmax    # best known makespan
    tft_opt <- tft    # best known total flowtime
    
    #### Set initial solution as best known solution bks
    #### for different objectives
    if (obj == "flowtime"){
      bks <- tft
    }
    if (obj == "makespan"){
      bks <- fmax
    }
    if (obj == "switches"){
      bks <- switch
    }
    
    ##############################################

    # =======================================
    # Local Search (1) around initial solution
    # =======================================
    ### Start of computation time counter
    tic("LS")
   
    bksR <- bks
    seqR <- seq
    loadsR <- array()
    
    #### Set improvement flag as TRUE
    foreach(ma=1:max_m) %do% {
      
      improved <- T
      while (improved == T){
        
        improved <- F
        # new sequence is similar to old sequence
        seq2 <- seqR
        # retain improved best known solution
        bks <- bksR
        # only for machines with length > 1
        
        if (length(seqR[[ma]])>1){
          # for each job on that machine
          
          for(j1_pos in 1:(length(seqR[[ma]])-1)){

            # and any other job on that machine
            for(j2_pos in (j1_pos+1):length(seqR[[ma]])){

              seq2[[ma]][j1_pos] <- seqR[[ma]][j2_pos]
              seq2[[ma]][j2_pos] <- seqR[[ma]][j1_pos]

              # ========================================
              # Tool Loading and keep tool needed soones (KTNS)
              # ========================================
              
              # sequence of unique tools that are still required
              t_req_m <- list()
              for (m in 1:max_m) {
                if(length(seq2[[m]])==1){
                  t_req_m[[m]] <- unlist(req_t[seq2[[m]][1]])
                  t_req_m[[m]] <- append(t_req_m[[m]],setdiff(c(1:max_t),t_req_m[[m]]))
                }
                else{
                  t_req_m[[m]] <- unlist(union(NULL,as.vector(unlist(req_t[seq2[[m]][-1]]))))
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
                all_t_req_m[[m]] <- unlist(req_t[seq2[[m]][-1]])   # without first jobs
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
                active_j <- seq2[[m]][1]   # active job active_j
                
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
                if (length(seq2[[m]]) == 1) {
                  active_j <- seq2[[m]][1]
                  ct[active_j] <- p_m[[m]][active_j]
                } else {
                  for (active_j_pos in 2:length(seq2[[m]])) {
                    active_j <- seq2[[m]][active_j_pos]
                    previous_j <- seq2[[m]][active_j_pos-1]
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
              
              ####################################
              
              if (obj == "flowtime"){
                obj_new <- Reduce("+",ct)
              }
              if (obj == "makespan"){
                obj_new <- Reduce(max,ct)
              }
              if (obj == "switches"){
                obj_new <- sum(ts_j[!is.na(ts_j)])
              }
              # ========================================
              # Acceptance Criterion
              # ========================================
              # if the solution is better than the best known solution
              if (obj_new < bks){
                # save new bks
                # bks is only updated after all loops break
                bksR <- obj_new
                # use new sequence
                seqR <- seq2
                ### overwrite (current) best solutions
                ct_opt <- ct
                ts_opt <- ts_j
                loads_opt <- load_t
                seq_opt <- seq2
                switch_opt <- sum(ts_j[!is.na(ts_j)])
                fmax_opt <- Reduce(max,ct)
                tft_opt <- Reduce("+",ct)
                
                # set improved to TRUE
                improved <- T
          
                break
              } else {
                ### otherwise use old sequence and old solution
                seq2 <- seqR
              }
              if (obj_new < bks) break
            }
            if (obj_new < bks)
              break
          }
          # if (obj_new < bks)
          #  # remove(j1_pos,j2_pos)
          # break
        }
        # if (obj_new < bks)
        #   remove(j1_pos,j2_pos)
        # break
      }
    }
    rm(active_j)
    
    toc(log = T,quiet = T)
    c_time <- tic.log(format = T)
    tic.clear()
    tic.clearlog() 
    
    ##############################################
    ### Result Local Search
    ##############################################
    ### sequence
    seq_ls <- seq_opt
    ### number of tool switches
    switch_ls <- switch_opt
    ### makespan
    fmax_ls <- fmax_opt
    ### total flow time
    tft_ls <- tft_opt
    ### completion times
    ct_ls <- ct_opt
    ### tool switches per job
    ts_ls <- ts_opt
    ### tool loading
    loads_ls <- loads_opt
    
    # =====================================
    # Output
    # =====================================
    # transform lists to writable format
    seq_opt <- capture.output(cat(paste0(strwrap(seq_ls))))
    loads_opt <- capture.output(cat(paste0(strwrap(loads_ls))))
    

    # generate solution
    mat <- as.matrix(rbind(switch_ls,tft_ls,fmax_ls,c_time[1],seq_opt,loads_opt))
    f3 <- paste0("./ls","_",obj,ins,".csv",sep = "")
    # write output to file
    if (file.exists(f3) == T) {
      print("file already exists")
    }
    if (file.exists(f3) == F) {
      write.table(mat,paste("./ls","_",obj,ins,".csv",sep = ""),sep=",",row.names = T,col.names = F)
    }
    
    # =====================================
    # remove all variables and parameters except instances and instance counter
    rm(list=ls()[! ls() %in% c("data.list","csv_files","ins","obj","resamp")])
  }
}


    