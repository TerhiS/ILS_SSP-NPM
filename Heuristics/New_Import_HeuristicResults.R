ImportConstructionResults <-
  function(Instance,
           ConstructionHeuristic,
           Objective) {
    if (dir.exists(paths = "~/GitHub/ILS_SSP-NPM/Heuristics/results/construction_heuristics/") == T) {
      setwd("~/GitHub/ILS_SSP-NPM/Heuristics/results/construction_heuristics/")
    } else {
      directory <- readline(prompt = "Enter Directory: ")
      setwd(paste0(directory))
    }
    solution <-
      read.csv(
        paste(ConstructionHeuristic, Instance, ".csv", sep = ""),
        quote = "",
        header = F,
        sep = '"',
        colClasses = "character"
      )
    solution <- as.matrix(solution[, -1])
    
    if (Objective == "switches") {
      ### number of tool switches switch
      BestKnownObjectiveValue <-
        as.integer(gsub(solution[1, 2],
                        pattern = ",",
                        replacement = ""))
    }
    if (Objective == "flowtime") {
      ### total flow time tft
      BestKnownObjectiveValue <-
        as.integer(gsub(solution[2, 2],
                        pattern = ",",
                        replacement = ""))
    }
    if (Objective == "makespan") {
      ### makespan fmax
      BestKnownObjectiveValue <-
        as.integer(gsub(solution[3, 2],
                        pattern = ",",
                        replacement = ""))
    }
    
    ### job assignment ConstructionSequence[[m]]
    ConstructionSequence <- strsplit(gsub(solution[5, 2],
                                          pattern = ",",
                                          replacement = ""), "c")
    ConstructionSequence <-
      strsplit(ConstructionSequence[[1]][-1], '"')
    ConstructionSequence <-
      strsplit(gsub("[[:punct:]]", "", ConstructionSequence), " ")
    for (m in 1:MaxMachines) {
      ConstructionSequence[[m]] <- as.integer(ConstructionSequence[[m]])
    }
    remove(m)
    results <- list()
    results[[1]] <- BestKnownObjectiveValue
    results[[2]] <- ConstructionSequence
    return(results)
  }
