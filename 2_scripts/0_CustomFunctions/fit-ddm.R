prepare_ddm_list <- function(data, id_name = "nomem_encr", n_conditions) {
  
  id_var <- rlang::sym(id_name)
  
  id_matches <- data |> 
    distinct(!!id_var) |> 
    mutate("{id_name}_num" := 1:n())
  
  data <- data |> 
    left_join(id_matches)
  
  # Store RTs and condition per trial (incorrect RTs are coded negatively)
  y <- round(ifelse(data$correct == 0, (data$rt*-1), data$rt),3)
  yInit <- rep(NA, length(y))
  
  if(n_conditions == 2) {
    condition <- as.numeric(data$condition)
    nCondition <- max(condition)
  }
  
  #Create numbers for JAGS
  nTrials <- nrow(data)
  nSubjects <- data |> distinct(!!id_var) |> pull(!!id_var) |> length()
  
  if(n_conditions == 2) {
    datalist <- list(
      y = y,
      subject = data |> pull(ends_with("_num")),
      condition = condition,
      nTrials = nTrials,
      nCon    = nCondition,
      nSubjects = nSubjects
    )
  }
  
  if(n_conditions == 1) {
    datalist <- list(
      y = y,
      subject = data |> pull(ends_with("_num")),
      condition = condition,
      nTrials = nTrials,
      nCon    = nCondition,
      nSubjects = nSubjects
    )
  }
  return(datalist)
}

