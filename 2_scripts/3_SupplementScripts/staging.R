
# Load data ---------------------------------------------------------------

load("1_data/2_IntermediateData/DDM_flanker_fit.RData")
load("1_data/2_IntermediateData/DDM_simon_fit.RData")
load("1_data/2_IntermediateData/DDM_colorshape_fit.RData")
load("1_data/2_IntermediateData/DDM_globallocal_fit.RData")
load("1_data/2_IntermediateData/DDM_animacysize_fit.RData")

load("1_data/3_AnalysisData/clean_data.RData")

load("1_data/2_IntermediateData/Tdata_long_clean.RData")



# Section 1 - Psychometrics -----------------------------------------------


## 1.1 RT Histograms ----

flanker_rt_hist <- flanker_clean |>
  group_by(nomem_encr, condition) |>
  summarise(rt = mean(rt)) |>
  mutate(condition = ifelse(condition == "1", "Congruent", "Incongruent")) |>
  mutate(condition = factor(condition)) |>
  ggplot(aes(rt, color = condition, fill = condition)) +
  geom_density(alpha = 0.5) +
  scale_x_continuous(breaks = seq(0, 5, 0.5)) +
  scale_color_uchicago() +
  scale_fill_uchicago() +
  theme_classic() +
  labs(
    title = "Flanker task",
    x = "Response time"
  )

simon_rt_hist <- simon_clean |>
  group_by(nomem_encr, condition) |>
  summarise(rt = mean(rt)) |>
  mutate(condition = ifelse(condition == "1", "Congruent", "Incongruent")) |>
  mutate(condition = factor(condition)) |>
  ggplot(aes(rt, color = condition, fill = condition)) +
  geom_density(alpha = 0.5) +
  scale_x_continuous(breaks = seq(0, 5, 0.5)) +
  scale_color_uchicago() +
  scale_fill_uchicago() +
  theme_classic() +
  labs(
    title = "Simon task",
    x = "Response time"
  )

colorshape_rt_hist <- colorshape_clean |>
  group_by(nomem_encr, condition) |>
  summarise(rt = mean(rt)) |>
  mutate(condition = ifelse(condition == "1", "Repeat", "Switch")) |>
  mutate(condition = factor(condition)) |>
  ggplot(aes(rt, color = condition, fill = condition)) +
  geom_density(alpha = 0.5) +
  scale_x_continuous(breaks = seq(0, 5, 1)) +
  scale_color_uchicago() +
  scale_fill_uchicago() +
  theme_classic() +
  labs(
    title = "Color-shape task",
    x = "Response time"
  )

animacysize_rt_hist <- animacysize_clean |>
  group_by(nomem_encr, condition) |>
  summarise(rt = mean(rt)) |>
  mutate(condition = ifelse(condition == "1", "Repeat", "Switch")) |>
  mutate(condition = factor(condition)) |>
  ggplot(aes(rt, color = condition, fill = condition)) +
  geom_density(alpha = 0.5) +
  scale_x_continuous(breaks = seq(0, 5, 1)) +
  scale_color_uchicago() +
  scale_fill_uchicago() +
  theme_classic() +
  labs(
    title = "Animacy-size task",
    x = "Response time"
  )

globallocal_rt_hist <- globallocal_clean |>
  group_by(nomem_encr, condition) |>
  summarise(rt = mean(rt)) |>
  mutate(condition = ifelse(condition == "1", "Repeat", "Switch")) |>
  mutate(condition = factor(condition)) |>
  ggplot(aes(rt, color = condition, fill = condition)) +
  geom_density(alpha = 0.5) +
  scale_x_continuous(breaks = seq(0, 5, 0.5)) +
  scale_color_uchicago() +
  scale_fill_uchicago() +
  theme_classic() +
  labs(
    title = "Global-local task",
    x = "Response time"
  )

supp_fig_rt_hist <- (flanker_rt_hist + simon_rt_hist) /
  (colorshape_rt_hist + animacysize_rt_hist) /
  (globallocal_rt_hist + plot_spacer())


## 1.2 Accuracy histograms ----

flanker_acc_hist <- flanker_clean |>
  group_by(nomem_encr, condition) |>
  summarise(acc = sum(correct==1)/n()*100) |>
  mutate(condition = ifelse(condition == "1", "Congruent", "Incongruent")) |>
  mutate(condition = factor(condition)) |>
  ggplot(aes(acc, color = condition, fill = condition)) +
  geom_density(alpha = 0.5) +
  scale_color_uchicago() +
  scale_fill_uchicago() +
  theme_classic() +
  labs(
    title = "Flanker task",
    x = "Accuracy"
  )

simon_acc_hist <- simon_clean |>
  group_by(nomem_encr, condition) |>
  summarise(acc = sum(correct==1)/n()*100) |>
  mutate(condition = ifelse(condition == "1", "Congruent", "Incongruent")) |>
  mutate(condition = factor(condition)) |>
  ggplot(aes(acc, color = condition, fill = condition)) +
  geom_density(alpha = 0.5) +
  scale_color_uchicago() +
  scale_fill_uchicago() +
  theme_classic() +
  labs(
    title = "Simon task",
    x = "Accuracy"
  )

colorshape_acc_hist <- colorshape_clean |>
  group_by(nomem_encr, condition) |>
  summarise(acc = sum(correct==1)/n()*100) |>
  mutate(condition = ifelse(condition == "1", "Congruent", "Incongruent")) |>
  mutate(condition = factor(condition)) |>
  ggplot(aes(acc, color = condition, fill = condition)) +
  geom_density(alpha = 0.5) +
  scale_color_uchicago() +
  scale_fill_uchicago() +
  theme_classic() +
  labs(
    title = "Color-shape task",
    x = "Accuracy"
  )

animacysize_acc_hist <- animacysize_clean |>
  group_by(nomem_encr, condition) |>
  summarise(acc = sum(correct==1)/n()*100) |>
  mutate(condition = ifelse(condition == "1", "Congruent", "Incongruent")) |>
  mutate(condition = factor(condition)) |>
  ggplot(aes(acc, color = condition, fill = condition)) +
  geom_density(alpha = 0.5) +
  scale_color_uchicago() +
  scale_fill_uchicago() +
  theme_classic() +
  labs(
    title = "Animacy-size task",
    x = "Accuracy"
  )

globallocal_acc_hist <- globallocal_clean |>
  group_by(nomem_encr, condition) |>
  summarise(acc = sum(correct==1)/n()*100) |>
  mutate(condition = ifelse(condition == "1", "Congruent", "Incongruent")) |>
  mutate(condition = factor(condition)) |>
  ggplot(aes(acc, color = condition, fill = condition)) +
  geom_density(alpha = 0.5) +
  scale_color_uchicago() +
  scale_fill_uchicago() +
  theme_classic() +
  labs(
    title = "Global-Local task",
    x = "Accuracy"
  )

supp_fig_acc_hist <- (flanker_acc_hist + simon_acc_hist) /
  (colorshape_acc_hist + animacysize_acc_hist) /
  (globallocal_acc_hist + plot_spacer())


## 1.3. Condition manipulation checks ----

man_checks <- list(flanker_clean, simon_clean, colorshape_clean,
                   globallocal_clean, animacysize_clean) |>
  map(function(x){
    task <- x |> pull(task) |> unique()
    x |>
      group_by(nomem_encr, condition) |>
      summarise(mean_rt = mean(rt)) |>
      mutate(mean_rt = log(mean_rt)) |>
      ungroup() |>
      pivot_wider(names_from = 'condition', values_from = 'mean_rt') %>%
      summarise(ttest = list(t.test(.$`1`, .$`2`, paired = TRUE) |> broom::tidy())) |>
      mutate(task = task)
  }) |>
  bind_rows() |>
  unnest(ttest) |>
  select(task, estimate, statistic, p.value) |>
  mutate(
    statistic = abs(statistic) %>% formatC(., digits = 2, width = 2, format = 'f'),
    estimate  = abs(estimate) %>% formatC(., digits = 2, width = 2, format = 'f'),
    p.value   = ifelse(p.value < .001, "< .001", p.value)
  ) |>
  mutate(
    
    task = case_when(
      task == "flanker" ~ "Flanker task",
      task == "simon" ~ "Simon task",
      task == "colorshape" ~ "Color-shape task",
      task == "animacysize" ~ "Animacy-size task",
      task == "globallocal2" ~ "Global-local task",
      task == "posner" ~ "Posner task"
    )
  ) |>
  rename(
    Task     = task,
    Estimate = estimate,
    t        = statistic,
    p        = p.value
  )

## 1.4. Split-half reliabilities of cognitive measures ----

cores <- parallel::detectCores()

plan(multisession, workers = cores - 2)

sh_rel_rt <- list(flanker_clean, simon_clean, colorshape_clean, animacysize_clean, globallocal_clean) |>
  furrr::future_map(function(x){
    
    
      result <- x |>
        filter(correct == 1) |>
        select(nomem_encr, rt, task, condition) |>
        splithalf(
          outcome = "RT",
          score = "average",
          halftype = "random",
          conditionlist = c("1", "2"),
          var.RT = "rt",
          var.condition = "condition",
          var.participant = "nomem_encr"
        )
    
    task <- result$data |> pull(task) |> unique()
    result <- result$final_estimates |>
      mutate(task = task)
    
    return(result)
    
  }, .options = furrr_options(seed = TRUE)) |>
  bind_rows()

sh_rel_rt_table <- sh_rel_rt %>%
  mutate(
    across(c(`95_low`, `95_high`, SB_low, SB_high, splithalf, spearmanbrown), ~formatC(x = ., digits = 2, width = 2, format = 'f')),
    sh_ci = paste0(splithalf, " ", "[", `95_low`, ", ", `95_high`, "]"),
    sb_ci = paste0(spearmanbrown, " ", "[", `SB_low`, ", ", `SB_high`, "]"),
    condition = ifelse(condition %in% c("1"), "congruent/repeat", "incongruent/switch")
  ) |>
  select(task, condition, sh_ci, sb_ci) |>
  pivot_wider(names_from = "condition", values_from = c(sh_ci, sb_ci)) |>
  mutate(
    task = case_when(
      task == "flanker" ~ "Flanker task",
      task == "simon" ~ "Simon task",
      task == "colorshape" ~ "Color-shape task",
      task == "animacysize" ~ "Animacy-size task",
      task == "globallocal2" ~ "Global-local task",
      task == "posner" ~ "Posner task"
    )
  )


sh_rel_acc <- list(flanker_clean, simon_clean, colorshape_clean, animacysize_clean, globallocal_clean) |>
  furrr::future_map(function(x){
    
      result <- x |>
        select(nomem_encr, correct, task, condition) |>
        splithalf(
          outcome = "accuracy",
          score = "average",
          halftype = "random",
          conditionlist = c("1", "2"),
          var.ACC = "correct",
          var.condition = "condition",
          var.participant = "nomem_encr"
        )
    
    task <- result$data |> pull(task) |> unique()
    result <- result$final_estimates |>
      mutate(task = task)
    
    return(result)
  }, .options = furrr_options(seed = TRUE)) |>
  bind_rows()

sh_rel_acc_table <- sh_rel_acc %>%
  mutate(
    across(c(`95_low`, `95_high`, SB_low, SB_high, splithalf, spearmanbrown), ~formatC(x = ., digits = 2, width = 2, format = 'f')),
    sh_ci = paste0(splithalf, " ", "[", `95_low`, ", ", `95_high`, "]"),
    sb_ci = paste0(spearmanbrown, " ", "[", `SB_low`, ", ", `SB_high`, "]"),
    condition = ifelse(condition %in% c("1"), "congruent/repeat", "incongruent/switch")
  ) |>
  select(task, condition, sh_ci, sb_ci) |>
  pivot_wider(names_from = "condition", values_from = c(sh_ci, sb_ci)) |>
  mutate(
    task = case_when(
      task == "flanker" ~ "Flanker task",
      task == "simon" ~ "Simon task",
      task == "colorshape" ~ "Color-shape task",
      task == "animacysize" ~ "Animacy-size task",
      task == "globallocal2" ~ "Global-local task",
      task == "posner" ~ "Posner task"
    )
  )


save(supp_fig_acc_hist, supp_fig_rt_hist, man_checks, sh_rel_rt_table, sh_rel_acc_table, file = "3_output/SupplementResults/section1.RData")


# Section 2 - DDM convergence and fit -------------------------------------

## 1.1 Traces ----

flanker_fit_trace <- flanker_traces |>
  mutate(parameter = case_when(
    parameter == "a1" ~ "Bound. sep. - Con",
    parameter == "a2" ~ "Bound. sep. - Inc",
    parameter == "t1" ~ "Non-dec. time - Con",
    parameter == "t2" ~ "Non-dec. time - Inc",
    parameter == "v1" ~ "Drift rate - Con",
    parameter == "v2" ~ "Drift rate - Inc",
  )) |>
  ggplot(aes(n, value, color = factor(chains))) +
  geom_line() +
  facet_wrap(~parameter, scales = 'free') +
  theme_classic() +
  scale_color_uchicago() +
  guides(color = 'none') +
  labs(
    x = "",
    y = "",
    color = "Chain"
  )


simon_fit_trace <- simon_traces |>
  mutate(parameter = case_when(
    parameter == "a1" ~ "Bound. sep. - Con",
    parameter == "a2" ~ "Bound. sep. - Inc",
    parameter == "t1" ~ "Non-dec. time - Con",
    parameter == "t2" ~ "Non-dec. time - Inc",
    parameter == "v1" ~ "Drift rate - Con",
    parameter == "v2" ~ "Drift rate - Inc",
  )) |>
  ggplot(aes(n, value, color = factor(chains))) +
  geom_line() +
  facet_wrap(~parameter, scales = 'free') +
  theme_classic() +
  scale_color_uchicago() +
  guides(color = 'none') +
  labs(
    x = "",
    y = "",
    color = "Chain"
  )

colorshape_fit_trace <- colorshape_traces |>
  mutate(parameter = case_when(
    parameter == "a1" ~ "Bound. sep. - Con",
    parameter == "a2" ~ "Bound. sep. - Inc",
    parameter == "t1" ~ "Non-dec. time - Con",
    parameter == "t2" ~ "Non-dec. time - Inc",
    parameter == "v1" ~ "Drift rate - Con",
    parameter == "v2" ~ "Drift rate - Inc",
  )) |>
  ggplot(aes(n, value, color = factor(chains))) +
  geom_line() +
  facet_wrap(~parameter, scales = 'free') +
  theme_classic() +
  scale_color_uchicago() +
  guides(color = 'none') +
  labs(
    x = "",
    y = "",
    color = "Chain"
  )

animacysize_fit_trace <- animacysize_traces |>
  mutate(parameter = case_when(
    parameter == "a1" ~ "Bound. sep. - Con",
    parameter == "a2" ~ "Bound. sep. - Inc",
    parameter == "t1" ~ "Non-dec. time - Con",
    parameter == "t2" ~ "Non-dec. time - Inc",
    parameter == "v1" ~ "Drift rate - Con",
    parameter == "v2" ~ "Drift rate - Inc",
  )) |>
  ggplot(aes(n, value, color = factor(chains))) +
  geom_line() +
  facet_wrap(~parameter, scales = 'free') +
  theme_classic() +
  scale_color_uchicago() +
  guides(color = 'none') +
  labs(
    x = "",
    y = "",
    color = "Chain",
  )

globallocal_fit_trace <- globallocal_traces |>
  mutate(parameter = case_when(
    parameter == "a1" ~ "Bound. sep. - Con",
    parameter == "a2" ~ "Bound. sep. - Inc",
    parameter == "t1" ~ "Non-dec. time - Con",
    parameter == "t2" ~ "Non-dec. time - Inc",
    parameter == "v1" ~ "Drift rate - Con",
    parameter == "v2" ~ "Drift rate - Inc",
  )) |>
  ggplot(aes(n, value, color = factor(chains))) +
  geom_line() +
  facet_wrap(~parameter, scales = 'free') +
  theme_classic() +
  scale_color_uchicago() +
  guides(color = 'none') +
  labs(
    x = "",
    y = "",
    color = "Chain",
  )


## 1.2 R^ values ----

rhat <-
  tribble(
    ~task,               ~rhat,
    "Flanker task",      max(rhat_flanker$`Point est.`, na.rm = T),
    "Simon task",        max(rhat_simon$`Point est.`, na.rm = T),
    "Color-shape task",  max(rhat_colorshape$`Point est.`, na.rm = T),
    "Animacy-size task", max(rhat_animacysize$`Point est.`, na.rm = T),
    "Global-local task", max(rhat_globallocal$`Point est.`, na.rm = T)
  ) |>
  mutate(rhat = formatC(x = rhat, digits = 3, width = 3, flag = "0", format = 'f'))


## 1.3 Model fit statistics ----

ddm_fit_table <- bind_rows(
  flanker_sim_fit |>
    mutate(condition = ifelse(condition == "1", "Congruent", "Incongruent")) |>
    group_by(condition, percentile) |>
    summarise(
      r_RT = cor(RT_sim, RT),
      r_acc = cor(acc_sim, acc)) |>
    ungroup() |>
    mutate(task = 'Flanker task'),
  simon_sim_fit |>
    mutate(condition = ifelse(condition == "1", "Congruent", "Incongruent")) |>
    group_by(condition, percentile) |>
    summarise(
      r_RT = cor(RT_sim, RT),
      r_acc = cor(acc_sim, acc)) |>
    ungroup() |>
    mutate(task = 'Simon task'),
  colorshape_sim_fit |>
    mutate(condition = ifelse(condition == "1", "Repeat", "Switch")) |>
    group_by(condition, percentile) |>
    summarise(
      r_RT = cor(RT_sim, RT),
      r_acc = cor(acc_sim, acc)) |>
    ungroup() |>
    mutate(task = 'Color-shape task'),
  animacysize_sim_fit |>
    mutate(condition = ifelse(condition == "1", "Repeat", "Switch")) |>
    group_by(condition, percentile) |>
    summarise(
      r_RT = cor(RT_sim, RT),
      r_acc = cor(acc_sim, acc)) |>
    ungroup() |>
    mutate(task = 'Animacy-size task'),
  globallocal_sim_fit |>
    mutate(condition = ifelse(condition == "1", "Repeat", "Switch")) |>
    group_by(condition, percentile) |>
    summarise(
      r_RT = cor(RT_sim, RT),
      r_acc = cor(acc_sim, acc)) |>
    ungroup() |>
    mutate(task = 'Global-local task')
) |>
  pivot_wider(names_from = 'percentile', values_from = c('r_RT', 'r_acc')) |>
  select(task, condition, contains("RT_RT"), r_acc_RT_25) %>%
  mutate(across(-c(task, condition), ~formatC(x = ., digits = 2, width = 3, flag = "0", format = 'f'))) |>
  left_join(rhat) |>
  flextable::flextable() |>
  flextable::autofit() |>
  flextable::set_header_labels(
    task = "Task",
    condition = 'Condition',
    r_RT_RT_25 = "RT - 25th Percentile",
    r_RT_RT_50 = "RT - 50th Percentile",
    r_RT_RT_75 = "RT - 75th Percentile",
    r_acc_RT_25 = "Accuracy",
    rhat        = "R^"
  )







save(flanker_fit_trace, simon_fit_trace, colorshape_fit_trace, animacysize_fit_trace, globallocal_fit_trace, ddm_fit_table, file = "3_output/SupplementResults/section2.RData")

