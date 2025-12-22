
source("2_scripts/dependencies.R")
source("2_scripts/0_CustomFunctions/general.R")

# 1. Load data ------------------------------------------------------------

load("1_data/2_IntermediateData/Tdata_raw.RData")
load("1_data/2_IntermediateData/Qdata_raw.RData")

exclusions <- list()

# 2. Clean trial-level data (preregistered) -------------------------------

## 2.1 Flanker ----

flanker_clean <- flanker_raw |>
  filter(rt > 0, rt < 10) |> # Remove excessively long RTs
  group_by(nomem_encr, condition) |>
  mutate(
    ex_fast_outlier = ifelse(rt < 0.250, TRUE, FALSE),
    ex_slow_outlier = ifelse(as.numeric(scale(log(rt))) > 3, TRUE, FALSE)
  ) |>
  group_by(nomem_encr) |>
  mutate(
    ex_chance_perf  = ifelse(sum(correct==1)/n()*100 < gbinom(64, 0.50), TRUE, FALSE)
  ) |>
  ungroup()

exclusions$flanker <-
  list(
    fast      = paste(round(sum(flanker_clean$ex_fast_outlier)/nrow(flanker_clean)*100, 1), "%"),
    very_slow = paste(round(sum(flanker_raw$rt > 10)/nrow(flanker_raw)*100, 2), "%"),
    slow      = paste(round(sum(flanker_clean$ex_slow_outlier, na.rm = T)/nrow(flanker_clean)*100, 1), "%"),
    chance    = flanker_clean |> group_by(nomem_encr) |> summarise(n = sum(ex_chance_perf)) |> filter(n > 1) |> nrow(),
    raw_n     = unique(flanker_clean$nomem_encr) |> length()
  )

flanker_clean <- flanker_clean |>
  filter(ex_slow_outlier == FALSE, ex_fast_outlier == FALSE, ex_chance_perf == FALSE) |>
  group_by(nomem_encr) |>
  mutate(valid_trials = n()) |>
  ungroup() |>
  select(-starts_with("ex"))

exclusions$flanker$n_val_trials <- flanker_clean |> filter(valid_trials < 20) |> pull(nomem_encr) |> unique() |> length()


flanker_clean <- flanker_clean |>
  filter(valid_trials > 19) |>
  sjlabelled::var_labels(
    nomem_encr   = "LISS unique participant identifier",
    rt           = "Response time in seconds",
    task         = "Task identifier",
    condition    = "Task condition",
    valid_trials = "Number of valid trials after exclusions"
  )

## 2.2 Simon task ----

simon_clean <- simon_raw |>
  filter(rt > 0, rt < 10) |> # Remove excessively long RTs
  group_by(nomem_encr, condition) |>
  mutate(
    ex_fast_outlier = ifelse(rt < 0.250, TRUE, FALSE),
    ex_slow_outlier = ifelse(as.numeric(scale(log(rt))) > 3, TRUE, FALSE)
  ) |>
  group_by(nomem_encr) |>
  mutate(
    ex_chance_perf  = ifelse(sum(correct==1)/n()*100 < gbinom(64, 0.50), TRUE, FALSE)
  ) |>
  ungroup()

exclusions$simon <-
  list(
    fast   = paste(round(sum(simon_clean$ex_fast_outlier)/nrow(simon_clean)*100, 1), "%"),
    very_slow = paste(round(sum(simon_raw$rt > 10)/nrow(simon_raw)*100, 2), "%"),
    slow   = paste(round(sum(simon_clean$ex_slow_outlier, na.rm = T)/nrow(simon_clean)*100, 1), "%"),
    chance = simon_clean |> group_by(nomem_encr) |> summarise(n = sum(ex_chance_perf)) |> filter(n > 1) |> nrow(),
    raw_n   = unique(simon_clean$nomem_encr) |> length()
  )

simon_clean <- simon_clean |>
  filter(ex_slow_outlier == FALSE, ex_fast_outlier == FALSE, ex_chance_perf == FALSE) |>
  group_by(nomem_encr) |>
  mutate(valid_trials = n()) |>
  ungroup() |>
  select(-starts_with("ex"))

exclusions$simon$n_val_trials <- simon_clean |> filter(valid_trials < 20) |> pull(nomem_encr) |> unique() |> length()

simon_clean <- simon_clean |>
  filter(valid_trials > 19) |>
  sjlabelled::var_labels(
    nomem_encr   = "LISS unique participant identifier",
    rt           = "Response time in seconds",
    task         = "Task identifier",
    condition    = "Task condition",
    valid_trials = "Number of valid trials after exclusions"
  )

## 2.3 Color-shape task ----

colorshape_clean <- colorshape_raw |>
  filter(rt < 10, rt > 0) |> # Remove excessively long RTs
  group_by(nomem_encr, condition) |>
  mutate(
    ex_fast_outlier = ifelse(rt < 0.250, TRUE, FALSE),
    ex_slow_outlier = ifelse(as.numeric(scale(log(rt))) > 3, TRUE, FALSE)
  ) |>
  group_by(nomem_encr) |>
  mutate(
    ex_chance_perf  = ifelse(sum(correct==1)/n()*100 < gbinom(64, 0.50), TRUE, FALSE)
  ) |>
  ungroup()

exclusions$colorshape <-
  list(
    fast   = paste(round(sum(colorshape_clean$ex_fast_outlier)/nrow(colorshape_clean)*100, 1), "%"),
    very_slow = paste(round(sum(colorshape_raw$rt > 10)/nrow(colorshape_raw)*100, 2), "%"),
    slow   = paste(round(sum(colorshape_clean$ex_slow_outlier, na.rm = T)/nrow(colorshape_clean)*100, 1), "%"),
    chance = colorshape_clean |> group_by(nomem_encr) |> summarise(n = sum(ex_chance_perf)) |> filter(n > 1) |> nrow(),
    raw_n   = unique(colorshape_clean$nomem_encr) |> length()
  )

colorshape_clean <- colorshape_clean |>
  filter(ex_slow_outlier == FALSE, ex_fast_outlier == FALSE, ex_chance_perf == FALSE) |>
  group_by(nomem_encr) |>
  mutate(valid_trials = n()) |>
  ungroup() |>
  select(-starts_with("ex"))

exclusions$colorshape$n_val_trials <- colorshape_clean |> filter(valid_trials < 20) |> pull(nomem_encr) |> unique() |> length()

colorshape_clean <- colorshape_clean |>
  filter(valid_trials > 19) |>
  sjlabelled::var_labels(
    nomem_encr   = "LISS unique participant identifier",
    rt           = "Response time in seconds",
    task         = "Task identifier",
    condition    = "Task condition",
    valid_trials = "Number of valid trials after exclusions"
  )

## 2.4. Animacy-size task ----

animacysize_clean <- animacysize_raw |>
  filter(rt > 0, rt < 10) |> # Remove excessively long RTs
  group_by(nomem_encr, condition) |>
  mutate(
    ex_fast_outlier = ifelse(rt < 0.250, TRUE, FALSE),
    ex_slow_outlier = ifelse(as.numeric(scale(log(rt))) > 3, TRUE, FALSE)
  ) |>
  group_by(nomem_encr) |>
  mutate(
    ex_chance_perf  = ifelse(sum(correct==1)/n()*100 < gbinom(64, 0.50), TRUE, FALSE)
  ) |>
  ungroup()

exclusions$animacysize <-
  list(
    fast   = paste(round(sum(animacysize_clean$ex_fast_outlier)/nrow(animacysize_clean)*100, 1), "%"),
    very_slow = paste(round(sum(animacysize_raw$rt > 10)/nrow(animacysize_raw)*100, 2), "%"),
    slow   = paste(round(sum(animacysize_clean$ex_slow_outlier, na.rm = T)/nrow(animacysize_clean)*100, 1), "%"),
    chance = animacysize_clean |> group_by(nomem_encr) |> summarise(n = sum(ex_chance_perf)) |> filter(n > 1) |> nrow(),
    raw_n   = unique(animacysize_clean$nomem_encr) |> length()
  )

animacysize_clean <- animacysize_clean |>
  filter(ex_slow_outlier == FALSE, ex_fast_outlier == FALSE, ex_chance_perf == FALSE) |>
  group_by(nomem_encr) |>
  mutate(valid_trials = n()) |>
  ungroup() |>
  select(-starts_with("ex"))

exclusions$animacysize$n_val_trials <- animacysize_clean |> filter(valid_trials < 20) |> pull(nomem_encr) |> unique() |> length()

animacysize_clean <- animacysize_clean |>
  filter(valid_trials > 19) |>
  sjlabelled::var_labels(
    nomem_encr   = "LISS unique participant identifier",
    rt           = "Response time in seconds",
    task         = "Task identifier",
    condition    = "Task condition",
    valid_trials = "Number of valid trials after exclusions"
  )


## 2.5. Global-local task ----

globallocal_clean <- globallocal_raw |>
  filter(rt > 0, rt < 10) |> # Remove excessively long RTs
  group_by(nomem_encr, condition) |>
  mutate(
    ex_fast_outlier = ifelse(rt < 0.250, TRUE, FALSE),
    ex_slow_outlier = ifelse(as.numeric(scale(log(rt))) > 3, TRUE, FALSE)
  ) |>
  group_by(nomem_encr) |>
  mutate(
    ex_chance_perf  = ifelse(sum(correct==1)/n()*100 < gbinom(64, 0.50), TRUE, FALSE)
  ) |>
  ungroup()

exclusions$globallocal <-
  list(
    fast   = paste(round(sum(globallocal_clean$ex_fast_outlier)/nrow(globallocal_clean)*100, 1), "%"),
    very_slow = paste(round(sum(globallocal_raw$rt > 10)/nrow(globallocal_raw)*100, 2), "%"),
    slow   = paste(round(sum(globallocal_clean$ex_slow_outlier, na.rm = T)/nrow(globallocal_clean)*100, 1), "%"),
    chance = globallocal_clean |> group_by(nomem_encr) |> summarise(n = sum(ex_chance_perf)) |> filter(n > 1) |> nrow(),
    raw_n   = unique(globallocal_clean$nomem_encr) |> length()
  )

globallocal_clean <- globallocal_clean |>
  filter(ex_slow_outlier == FALSE, ex_fast_outlier == FALSE, ex_chance_perf == FALSE) |>
  group_by(nomem_encr) |>
  mutate(valid_trials = n()) |>
  ungroup() |>
  select(-starts_with("ex"))

exclusions$globallocal$n_val_trials <- globallocal_clean |> filter(valid_trials < 20) |> pull(nomem_encr) |> unique() |> length()

globallocal_clean <- globallocal_clean |>
  filter(valid_trials > 19) |>
  sjlabelled::var_labels(
    nomem_encr   = "LISS unique participant identifier",
    rt           = "Response time in seconds",
    task         = "Task identifier",
    condition    = "Task condition",
    valid_trials = "Number of valid trials after exclusions"
  )


# 3. Clean participant-level data (preregistered) --------------------------

## 3.1 Flanker ----

flanker_sum <- flanker_clean |> 
  mutate(condition = ifelse(condition == 1, "con", "inc")) |> 
  filter(correct == 1) |> 
  group_by(nomem_encr, wave, condition) |> 
  summarise(rt     = mean(rt, na.rm = TRUE)) |> 
  pivot_wider(names_from = "condition", values_from = "rt", names_prefix = "fl_rt_") |> 
  ungroup() |> 
  mutate(fl_score   = fl_rt_con - fl_rt_inc) |> 
  pivot_longer(c(fl_rt_con, fl_rt_inc), names_to = "condition", values_to = 'rt') |> 
  mutate(
    remove = ifelse(abs(scale(log(rt)) |> as.numeric()) > 3.2, TRUE, FALSE),
    rt        = ifelse(!remove, rt, NA)
    ) |> 
  pivot_wider(names_from = "condition", values_from = c("rt", "remove")) |> 
  mutate(
    remove_score = ifelse(abs(scale(fl_score) |> as.numeric()) > 3.2, TRUE, FALSE),
    fl_score     = ifelse(!remove_score, fl_score, NA)
    ) |> 
  rename(
    fl_rt_con = rt_fl_rt_con,
    fl_rt_inc = rt_fl_rt_inc
  )

exclusions$flanker$rts_excluded <- (sum(flanker_sum$remove_fl_rt_con)/(nrow(flanker_sum)) * 100) + (sum(flanker_sum$remove_fl_rt_inc)/(nrow(flanker_sum)) * 100)
exclusions$flanker$scores_excluded <- sum(flanker_sum$remove_score) / nrow(flanker_sum) * 100

flanker_sum <- flanker_sum |> select(-starts_with("remove"))

## 3.2 Simon ----

simon_sum <- simon_clean |> 
  mutate(condition = ifelse(condition == 1, "con", "inc")) |> 
  filter(correct == 1) |> 
  group_by(nomem_encr, wave, condition) |> 
  summarise(rt     = mean(rt, na.rm = TRUE)) |> 
  pivot_wider(names_from = "condition", values_from = "rt", names_prefix = "si_rt_") |> 
  ungroup() |> 
  mutate(si_score   = si_rt_con - si_rt_inc) |> 
  pivot_longer(c(si_rt_con, si_rt_inc), names_to = "condition", values_to = 'rt') |> 
  mutate(
    remove = ifelse(abs(scale(log(rt)) |> as.numeric()) > 3.2, TRUE, FALSE),
    rt        = ifelse(!remove, rt, NA)
  ) |> 
  pivot_wider(names_from = "condition", values_from = c("rt", "remove")) |> 
  mutate(
    remove_score = ifelse(abs(scale(si_score) |> as.numeric()) > 3.2, TRUE, FALSE),
    si_score     = ifelse(!remove_score, si_score, NA)
  ) |> 
  rename(
    si_rt_con = rt_si_rt_con,
    si_rt_inc = rt_si_rt_inc
  )

exclusions$simon$rts_excluded <- (sum(simon_sum$remove_si_rt_con)/(nrow(simon_sum)) * 100) + (sum(simon_sum$remove_si_rt_inc)/(nrow(simon_sum)) * 100)
exclusions$simon$scores_excluded <- sum(simon_sum$remove_score) / nrow(simon_sum) * 100

simon_sum <- simon_sum |> select(-starts_with("remove"))

## 3.3 Color-shape ----

colorshape_sum <- colorshape_clean |> 
  mutate(
    condition = ifelse(condition == 1, "rep", "sw"),
    remove    = case_when(
      lag(correct, n = 1) == 0 ~ TRUE,
      correct == 0             ~ TRUE,
      .default = FALSE
    )) |> 
  filter(remove == FALSE) |> 
  group_by(nomem_encr, wave, condition) |> 
  summarise(rt = mean(rt, na.rm = TRUE)) |> 
  pivot_wider(names_from = "condition", values_from = "rt", names_prefix = "cs_rt_") |> 
  ungroup() |> 
  mutate(cs_score = cs_rt_rep - cs_rt_sw) |> 
  pivot_longer(c(cs_rt_rep, cs_rt_sw), names_to = "condition", values_to = 'rt') |> 
  mutate(
    remove = ifelse(abs(scale(log(rt)) |> as.numeric()) > 3.2, TRUE, FALSE),
    rt        = ifelse(!remove, rt, NA)
  ) |> 
  pivot_wider(names_from = "condition", values_from = c("rt", "remove")) |> 
  mutate(
    remove_score = ifelse(abs(scale(cs_score) |> as.numeric()) > 3.2, TRUE, FALSE),
    cs_score     = ifelse(!remove_score, cs_score, NA)
  ) |> 
  rename(
    cs_rt_rep = rt_cs_rt_rep,
    cs_rt_sw = rt_cs_rt_sw
  )

exclusions$colorshape$rts_excluded <- (sum(colorshape_sum$remove_cs_rt_rep)/(nrow(colorshape_sum)) * 100) + (sum(colorshape_sum$remove_cs_rt_sw)/(nrow(colorshape_sum)) * 100)
exclusions$colorshape$scores_excluded <- sum(colorshape_sum$remove_score) / nrow(colorshape_sum) * 100

colorshape_sum <- colorshape_sum |> select(-starts_with("remove"))

## 3.4 Global-Local ----

globallocal_sum <- globallocal_clean |> 
  mutate(
    condition = ifelse(condition == 1, "rep", "sw"),
    remove    = case_when(
      lag(correct, n = 1) == 0 ~ TRUE,
      correct == 0             ~ TRUE,
      .default = FALSE
    )) |> 
  filter(remove == FALSE) |> 
  group_by(nomem_encr, wave, condition) |> 
  summarise(rt = mean(rt, na.rm = TRUE)) |> 
  pivot_wider(names_from = "condition", values_from = "rt", names_prefix = "gl_rt_") |> 
  ungroup() |> 
  mutate(gl_score = gl_rt_rep - gl_rt_sw) |> 
  pivot_longer(c(gl_rt_rep, gl_rt_sw), names_to = "condition", values_to = 'rt') |> 
  mutate(
    remove = ifelse(abs(scale(log(rt)) |> as.numeric()) > 3.2, TRUE, FALSE),
    rt        = ifelse(!remove, rt, NA)
  ) |> 
  pivot_wider(names_from = "condition", values_from = c("rt", "remove")) |> 
  mutate(
    remove_score = ifelse(abs(scale(gl_score) |> as.numeric()) > 3.2, TRUE, FALSE),
    gl_score     = ifelse(!remove_score, gl_score, NA)
  ) |> 
  rename(
    gl_rt_rep = rt_gl_rt_rep,
    gl_rt_sw = rt_gl_rt_sw
  )

exclusions$globallocal$rts_excluded <- (sum(globallocal_sum$remove_gl_rt_rep)/(nrow(globallocal_sum)) * 100) + (sum(globallocal_sum$remove_gl_rt_sw)/(nrow(globallocal_sum)) * 100)
exclusions$globallocal$scores_excluded <- sum(globallocal_sum$remove_score) / nrow(globallocal_sum) * 100

globallocal_sum <- globallocal_sum |> select(-starts_with("remove"))


## 3.5 Animacy-size

animacysize_sum <- animacysize_clean |> 
  mutate(
    condition = ifelse(condition == 1, "rep", "sw"),
    remove    = case_when(
      lag(correct, n = 1) == 0 ~ TRUE,
      correct == 0             ~ TRUE,
      .default = FALSE
    )) |> 
  filter(remove == FALSE) |> 
  group_by(nomem_encr, wave, condition) |> 
  summarise(rt = mean(rt, na.rm = TRUE)) |> 
  pivot_wider(names_from = "condition", values_from = "rt", names_prefix = "as_rt_") |> 
  ungroup() |> 
  mutate(as_score = as_rt_rep - as_rt_sw) |> 
  pivot_longer(c(as_rt_rep, as_rt_sw), names_to = "condition", values_to = 'rt') |> 
  mutate(
    remove = ifelse(abs(scale(log(rt)) |> as.numeric()) > 3.2, TRUE, FALSE),
    rt        = ifelse(!remove, rt, NA)
  ) |> 
  pivot_wider(names_from = "condition", values_from = c("rt", "remove")) |> 
  mutate(
    remove_score = ifelse(abs(scale(as_score) |> as.numeric()) > 3.2, TRUE, FALSE),
    as_score     = ifelse(!remove_score, as_score, NA)
  ) |> 
  rename(
    as_rt_rep = rt_as_rt_rep,
    as_rt_sw = rt_as_rt_sw
  )

exclusions$animacysize$rts_excluded <- (sum(animacysize_sum$remove_as_rt_rep)/(nrow(animacysize_sum)) * 100) + (sum(animacysize_sum$remove_as_rt_sw)/(nrow(animacysize_sum)) * 100)
exclusions$animacysize$scores_excluded <- sum(animacysize_sum$remove_score) / nrow(animacysize_sum) * 100

animacysize_sum <- animacysize_sum |> select(-starts_with("remove"))



# 3. Clean self-report measures -------------------------------------------

# 2. Compute self-report variables ----------------------------------------

Qdata_clean <- Qdata_raw |> 
  mutate(
    VP1 = 8 - VP1,
    VP3 = 8 - VP3,
    PP1 = 8 - PP1,
    PP2 = 8 - PP2,
    PP3 = 8 - PP3,
    PP4 = 8 - PP4,
    
    child_thr = across(c(starts_with("VP"))) |> rowMeans(na.rm = T),
    child_dep = across(c(starts_with("PP"))) |> rowMeans(na.rm = T),
    
    edu = case_when(
      as.numeric(edu) == "7" & as.numeric(oplzon) != "7" ~ as.numeric(oplzon),
      as.numeric(edu) == "8" & as.numeric(oplzon) != "8" ~ as.numeric(oplzon),
      as.numeric(edu) == "8" & as.numeric(oplzon) == "8" ~ 1,
      as.numeric(edu) == "9" ~ 1,
      as.numeric(edu) == "7" ~ NA,
      .default = edu
    ),
    
  ) |> 
  select(nomem_encr, nohouse_encr, wave, child_thr, child_dep, age, edu, urb)

exclusions$selfreport$missing_edu <- sum(is.na(Qdata_clean$edu))
exclusions$selfreport$missing_age <- sum(is.na(Qdata_clean$age))
exclusions$selfreport$missing_urb <- sum(is.na(Qdata_clean$urb))
exclusions$selfreport$missing_thr <- sum(is.na(Qdata_clean$child_thr))
exclusions$selfreport$missing_dep <- sum(is.na(Qdata_clean$child_dep))

# 3. Save data ------------------------------------------------------------

save(flanker_clean, simon_clean, colorshape_clean, globallocal_clean, animacysize_clean, file = "1_data/2_IntermediateData/Tdata_long_clean.RData")
save(flanker_sum, simon_sum, colorshape_sum, globallocal_sum, animacysize_sum, file = "1_data/2_IntermediateData/Tdata_sum_clean.RData")
save(Qdata_clean, file = "1_data/2_IntermediateData/Qdata_clean.RData")
save(exclusions, file = '3_output/Results/exclusions.RData')

# 4. Empty global environment ---------------------------------------------

rm(list = ls())

