
# 1. Load data ------------------------------------------------------------

load("1_data/2_IntermediateData/Qdata_clean.RData")
load("1_data/2_IntermediateData/Tdata_sum_clean.RData")

load("3_output/Results/exclusions.RData")

load("1_data/2_IntermediateData/DDM_flanker_fit.RData")
load("1_data/2_IntermediateData/DDM_simon_fit.RData")
load("1_data/2_IntermediateData/DDM_globallocal_fit.RData")
load("1_data/2_IntermediateData/DDM_colorshape_fit.RData")
load("1_data/2_IntermediateData/DDM_animacysize_fit.RData")



# 2. Parse and compute state anxiety and noise level ----------------------

stai_noise <- 
  bind_rows(
    haven::read_sav('1_data/1_InputData/L_CognitiveAdversity_1.0p.sav') |> 
      mutate(wave = 1) |> 
      select(nomem_encr, wave, DatumB, DatumE, matches("SE\\d_\\d"), matches("Q1_\\d")),
    haven::read_sav("1_data/1_InputData/L_CognitiveAdversity_2025_1.0p.sav") |> 
      mutate(wave = 2) |> 
      select(nomem_encr, wave, DatumB, DatumE, matches("SE\\d_\\d"), matches("Q1_\\d"))
  ) |> 
  mutate(
    # STAI variables (absolute)
    stai_simon        = (5-SE1_1) + SE2_1 + SE3_1 + (5-SE4_1) + (5-SE5_1) + SE6_1,
    stai_flanker      = (5-SE1_2) + SE2_2 + SE3_2 + (5-SE4_2) + (5-SE5_2) + SE6_2,
    stai_globallocal  = (5-SE1_3) + SE2_3 + SE3_3 + (5-SE4_3) + (5-SE5_3) + SE6_3,
    stai_colorshape   = (5-SE1_4) + SE2_4 + SE3_4 + (5-SE4_4) + (5-SE5_4) + SE6_4,
    stai_animacysize  = (5-SE1_5) + SE2_5 + SE3_5 + (5-SE4_5) + (5-SE5_5) + SE6_5,
    
    # Average STAI score across tasks
    stai_mean         = across(starts_with('stai')) |> rowMeans(),
    
    # STAI variables (difference from the grand mean)
    stai_simon_diff       = stai_simon - stai_mean,
    stai_flanker_diff     = stai_flanker - stai_mean,
    stai_globallocal_diff = stai_globallocal - stai_mean,
    stai_colorshape_diff  = stai_colorshape - stai_mean,
    stai_animacysize_diff = stai_animacysize - stai_mean,
    one_session           = ifelse(DatumB == DatumE, 0, 1)
  ) |>
  rename(
    noise_simon       = Q1_1,
    noise_flanker     = Q1_2,
    noise_globallocal = Q1_3,
    noise_colorshape  = Q1_4,
    noise_animacysize = Q1_5
  ) |>
  right_join(Qdata_clean) |> 
  select(nomem_encr, starts_with("noise"), matches("stai.*diff")) 



# 2. Get DDM parameter estimates ------------------------------------------

tasks_ddm <- list(
  flanker_param_est, 
  simon_param_est, 
  colorshape_param_est,
  globallocal_param_est,
  animacysize_param_est
) |> 
  reduce(
    full_join
  ) |> 
  select(nomem_encr, everything())


##  2.1 Correct DDM estimates for noise and state anxiety ----

tasks_ddm <- tasks_ddm |>
  left_join(stai_noise) %>%
  mutate(
    fl_v1_c = lm(fl_v1 ~ noise_flanker + stai_flanker_diff, data = ., na.action=na.exclude) |> resid(),
    fl_v2_c = lm(fl_v2 ~ noise_flanker + stai_flanker_diff, data = ., na.action=na.exclude) |> resid(),
    fl_a1_c = lm(fl_a1 ~ noise_flanker + stai_flanker_diff, data = ., na.action=na.exclude) |> resid(),
    fl_a2_c = lm(fl_a2 ~ noise_flanker + stai_flanker_diff, data = ., na.action=na.exclude) |> resid(),
    fl_t1_c = lm(fl_t1 ~ noise_flanker + stai_flanker_diff, data = ., na.action=na.exclude) |> resid(),
    fl_t2_c = lm(fl_t2 ~ noise_flanker + stai_flanker_diff, data = ., na.action=na.exclude) |> resid(),
    
    si_v1_c = lm(si_v1 ~ noise_simon + stai_simon_diff, data = ., na.action=na.exclude) |> resid(),
    si_v2_c = lm(si_v2 ~ noise_simon + stai_simon_diff, data = ., na.action=na.exclude) |> resid(),
    si_a1_c = lm(si_a1 ~ noise_simon + stai_simon_diff, data = ., na.action=na.exclude) |> resid(),
    si_a2_c = lm(si_a2 ~ noise_simon + stai_simon_diff, data = ., na.action=na.exclude) |> resid(),
    si_t1_c = lm(si_t1 ~ noise_simon + stai_simon_diff, data = ., na.action=na.exclude) |> resid(),
    si_t2_c = lm(si_t2 ~ noise_simon + stai_simon_diff, data = ., na.action=na.exclude) |> resid(),
    
    cs_v1_c = lm(cs_v1 ~ noise_colorshape + stai_colorshape_diff, data = ., na.action=na.exclude) |> resid(),
    cs_v2_c = lm(cs_v2 ~ noise_colorshape + stai_colorshape_diff, data = ., na.action=na.exclude) |> resid(),
    cs_a1_c = lm(cs_a1 ~ noise_colorshape + stai_colorshape_diff, data = ., na.action=na.exclude) |> resid(),
    cs_a2_c = lm(cs_a2 ~ noise_colorshape + stai_colorshape_diff, data = ., na.action=na.exclude) |> resid(),
    cs_t1_c = lm(cs_t1 ~ noise_colorshape + stai_colorshape_diff, data = ., na.action=na.exclude) |> resid(),
    cs_t2_c = lm(cs_t2 ~ noise_colorshape + stai_colorshape_diff, data = ., na.action=na.exclude) |> resid(),
    
    gl_v1_c = lm(gl_v1 ~ noise_globallocal + stai_globallocal_diff, data = ., na.action=na.exclude) |> resid(),
    gl_v2_c = lm(gl_v2 ~ noise_globallocal + stai_globallocal_diff, data = ., na.action=na.exclude) |> resid(),
    gl_a1_c = lm(gl_a1 ~ noise_globallocal + stai_globallocal_diff, data = ., na.action=na.exclude) |> resid(),
    gl_a2_c = lm(gl_a2 ~ noise_globallocal + stai_globallocal_diff, data = ., na.action=na.exclude) |> resid(),
    gl_t1_c = lm(gl_t1 ~ noise_globallocal + stai_globallocal_diff, data = ., na.action=na.exclude) |> resid(),
    gl_t2_c = lm(gl_t2 ~ noise_globallocal + stai_globallocal_diff, data = ., na.action=na.exclude) |> resid(),
    
    as_v1_c = lm(as_v1 ~ noise_animacysize + stai_animacysize_diff, data = ., na.action=na.exclude) |> resid(),
    as_v2_c = lm(as_v2 ~ noise_animacysize + stai_animacysize_diff, data = ., na.action=na.exclude) |> resid(),
    as_a1_c = lm(as_a1 ~ noise_animacysize + stai_animacysize_diff, data = ., na.action=na.exclude) |> resid(),
    as_a2_c = lm(as_a2 ~ noise_animacysize + stai_animacysize_diff, data = ., na.action=na.exclude) |> resid(),
    as_t1_c = lm(as_t1 ~ noise_animacysize + stai_animacysize_diff, data = ., na.action=na.exclude) |> resid(),
    as_t2_c = lm(as_t2 ~ noise_animacysize + stai_animacysize_diff, data = ., na.action=na.exclude) |> resid()
  )


# 3. Get raw performance measures -----------------------------------------

tasks_sum <- 
  reduce(
    list(
      flanker_sum, 
      simon_sum, 
      colorshape_sum, 
      globallocal_sum, 
      animacysize_sum
    ),
    full_join
  )

##  3.1 Correct DDM estimates for noise and state anxiety ----

tasks_sum <- tasks_sum |> 
  left_join(stai_noise) %>%
  mutate(
    fl_score_c  = lm(fl_score  ~ noise_flanker + stai_flanker_diff, data = ., na.action=na.exclude) |> resid(),
    fl_rt_con_c = lm(log(fl_rt_con) ~ noise_flanker + stai_flanker_diff, data = ., na.action=na.exclude) |> resid(),
    fl_rt_inc_c = lm(log(fl_rt_inc) ~ noise_flanker + stai_flanker_diff, data = ., na.action=na.exclude) |> resid(),
    
    si_score_c  = lm(si_score  ~ noise_simon + stai_simon_diff, data = ., na.action=na.exclude) |> resid(),
    si_rt_con_c = lm(log(si_rt_con) ~ noise_simon + stai_simon_diff, data = ., na.action=na.exclude) |> resid(),
    si_rt_inc_c = lm(log(si_rt_inc) ~ noise_simon + stai_simon_diff, data = ., na.action=na.exclude) |> resid(),
    
    cs_score_c  = lm(cs_score  ~ noise_colorshape + stai_colorshape_diff, data = ., na.action=na.exclude) |> resid(),
    cs_rt_rep_c = lm(log(cs_rt_rep) ~ noise_colorshape + stai_colorshape_diff, data = ., na.action=na.exclude) |> resid(),
    cs_rt_sw_c  = lm(log(cs_rt_sw)  ~ noise_colorshape + stai_colorshape_diff, data = ., na.action=na.exclude) |> resid(),
    
    gl_score_c  = lm(gl_score  ~ noise_globallocal + stai_globallocal_diff, data = ., na.action=na.exclude) |> resid(),
    gl_rt_rep_c = lm(log(gl_rt_rep) ~ noise_globallocal + stai_globallocal_diff, data = ., na.action=na.exclude) |> resid(),
    gl_rt_sw_c  = lm(log(gl_rt_sw)  ~ noise_globallocal + stai_globallocal_diff, data = ., na.action=na.exclude) |> resid(),
    
    as_score_c  = lm(as_score  ~ noise_animacysize + stai_animacysize_diff, data = ., na.action=na.exclude) |> resid(),
    as_rt_rep_c = lm(log(as_rt_rep) ~ noise_animacysize + stai_animacysize_diff, data = ., na.action=na.exclude) |> resid(),
    as_rt_sw_c  = lm(log(as_rt_sw)  ~ noise_animacysize + stai_animacysize_diff, data = ., na.action=na.exclude) |> resid(),
  )



# 3. Combine data ---------------------------------------------------------


data_clean <- Qdata_clean |> 
  full_join(tasks_sum) |> 
  full_join(tasks_ddm) |> 
  filter(if_any(c(ends_with("score"), matches("_rt_")), ~!is.na(.))) 

exclusions$total$tasks_skip <- nrow(Qdata_clean) - nrow(data_clean)



# 4. Impute missing values in MNLFA moderators ----------------------------

## 4.1 Inspect variability in imputed values ----

nMice = 10

mod <- c("child_thr", "child_dep", "age", "edu", "urb")
dv <- data_clean |> select(matches("_a(1|2)$|_v(1|2)$|_t(1|2)$")) |> names()
dropvars <- data_clean |> select(nomem_encr, starts_with("noise"), starts_with("stai"), matches("_a(1|2)_c$|_v(1|2)_c$|_t(1|2)_c$"), nohouse_encr, wave) |> names()
raw <- data_clean |> select(matches("_rt_"), matches("_score")) |> names()

predM <- data_clean |> 
  select(all_of(c(mod, dv, raw, dropvars))) |> 
  finalfit::missing_predictorMatrix(
    drop_from_imputed = c(dv, raw, dropvars),
    drop_from_imputer = c(raw, dropvars)
  )

where.data_clean <- is.na(data_clean |> select(all_of(c(mod, dv, raw, dropvars)))) |> 
  as_tibble() |> 
  mutate(across(matches(c(dv, raw)), ~FALSE))

imp_n10 <- data_clean |> 
  select(all_of(c(mod, dv, raw, dropvars))) |> 
  sjlabelled::remove_all_labels() |> 
  mice(m = nMice, seed = 5, 
       predictorMatrix = predM,
       where = where.data_clean,
       drop_from_imputer = dropvars)

imp_n10_plot <- plot(imp_n10)

# Increase the number of chains to make sure they mix well
imp_conv <- mice.mids(imp_n10, maxit = 35, print = F)

imp_n10_conv_plot <- plot(imp_conv)


# 4.2 Impute one dataset --------------------------------------------------

nMice = 1

imp_n1 <- data_clean |> 
  select(all_of(c(mod, dv, raw, dropvars))) |> 
  sjlabelled::remove_all_labels() |> 
  mice(m = nMice, seed = 5,
       predictorMatrix = predM,
       where = where.data_clean,
       drop_from_imputer = dropvars) |> 
  complete() |> 
  as_tibble() |> 
  drop_na(child_thr, child_dep, age, edu, urb)

exclusions$non_imputed <- nrow(data_clean) - nrow(imp_n1)

data_clean <- imp_n1 |> 
  select(names(data_clean))

exclusions$sample$final_n <- nrow(data_clean)

# 5. Center continuous variables for main analyses ------------------------

data_clean <- data_clean |> 
  mutate(
    across(c(age, edu, urb), ~scale(., scale = FALSE) |> as.numeric(), .names = "{.col}_c")
  ) |> 
  select(nomem_encr, nohouse_encr, wave, child_thr, child_dep, age, age_c, edu, edu_c, urb, urb_c, everything())


# 5. Add variable labels --------------------------------------------------


data_clean <- data_clean |> 
  sjlabelled::var_labels(
    nomem_encr   = "Unique LISS identifier",
    nohouse_encr = "LISS household identifier",
    wave         = "Wave in which participant participated",
    child_thr    = "Average score on childhood threat items",
    child_dep    = "Average score on childhood deprivation items",
    urb          = "Urban character of place of residence (based on surrounding address density per km^2^",
    urb_c          = "Urban character of place of residence; centered",
    age          = "Age in years", 
    age_c        = "Age in years; centered",
    edu          = "Highest obtained education",
    edu_c        = "Highest obtained education; centered",
    
    fl_score     = "Flanker task. Congruency effect based on mean response times (congruent - incongruent)",
    fl_rt_con    = "Flanker task. Average response time on correct trials of the congruent condition",
    fl_rt_inc    = "Flanker task. Average response time on correct trials of the incongruent condition",
    
    si_score     = "Simon task. Congruency effect based on mean response times of correct trials (congruent - incongruent)",
    si_rt_con    = "Simon task. Average response time on correct trials of the congruent condition",
    si_rt_inc    = "Simon task. Average response time on correct trials of the incongruent condition",
    
    cs_score     = "Color-shape task. Switch cost based on mean response times of correct trials, excluding trials following an error (repeat - switch)",
    cs_rt_rep    = "Color-shape task. Average response time on correct trials of the repeat condition",
    cs_rt_sw     = "Color-shape task. Average response time on correct trials of the switch condition",
    
    gl_score     = "Global-Localtask. Switch cost based on mean response times of correct trials, excluding trials following an error (repeat - switch)",
    gl_rt_rep    = "Global-Local task. Average response time on correct trials of the repeat condition",
    gl_rt_sw     = "Global-Localtask. Average response time on correct trials of the switch condition",
    
    as_score     = "Animacy-Size task. Switch cost based on mean response times of correct trials, excluding trials following an error (repeat - switch)",
    as_rt_rep    = "Animacy-Size task. Average response time on correct trials of the repeat condition",
    as_rt_sw     = "Animacy-Size task. Average response time on correct trials of the switch condition",
    
    fl_score_c     = "Flanker task. Log-transformed Congruency effect based on mean response times (congruent - incongruent). Corrected for noise and state anxiety",
    fl_rt_con_c    = "Flanker task. Log-transformed Average response time on correct trials of the congruent condition. Corrected for noise and state anxiety",
    fl_rt_inc_c    = "Flanker task. Log-transformed Average response time on correct trials of the incongruent condition. Corrected for noise and state anxiety",
    
    si_score_c     = "Simon task. Log-transformed Congruency effect based on mean response times of correct trials (congruent - incongruent). Corrected for noise and state anxiety",
    si_rt_con_c    = "Simon task. Log-transformed Average response time on correct trials of the congruent condition. Corrected for noise and state anxiety",
    si_rt_inc_c    = "Simon task. Log-transformed Average response time on correct trials of the incongruent condition. Corrected for noise and state anxiety",
    
    cs_score_c     = "Color-shape task. Log-transformed Switch cost based on mean response times of correct trials, excluding trials following an error (repeat - switch). Corrected for noise and state anxiety",
    cs_rt_rep_c    = "Color-shape task. Log-transformed Average response time on correct trials of the repeat condition. Corrected for noise and state anxiety",
    cs_rt_sw_c     = "Color-shape task. Log-transformed Average response time on correct trials of the switch condition. Corrected for noise and state anxiety",
    
    gl_score_c     = "Global-Localtask. Log-transformed Switch cost based on mean response times of correct trials, excluding trials following an error (repeat - switch). Corrected for noise and state anxiety",
    gl_rt_rep_c    = "Global-Local task. Log-transformed Average response time on correct trials of the repeat condition. Corrected for noise and state anxiety",
    gl_rt_sw_c     = "Global-Localtask. Log-transformed Average response time on correct trials of the switch condition. Corrected for noise and state anxiety",
    
    as_score_c     = "Animacy-Size task. Log-transformed Switch cost based on mean response times of correct trials, excluding trials following an error (repeat - switch). Corrected for noise and state anxiety",
    as_rt_rep_c    = "Animacy-Size task. Log-transformed Average response time on correct trials of the repeat condition. Corrected for noise and state anxiety",
    as_rt_sw_c     = "Animacy-Size task. Log-transformed Average response time on correct trials of the switch condition. Corrected for noise and state anxiety",
    
    fl_a1        = "Flanker task. Boundary separation of congruent condition (derived from the Drift Diffusion Model)",
    fl_a2        = "Flanker task. Boundary separation of incongruent condition (derived from the Drift Diffusion Model)",
    fl_v1        = "Flanker task. Drift rate of congruent condition (derived from the Drift Diffusion Model)",
    fl_v2        = "Flanker task. Drift rate of incongruent condition (derived from the Drift Diffusion Model)",
    fl_t1        = "Flanker task. Non-decision time of congruent condition (derived from the Drift Diffusion Model)",
    fl_t2        = "Flanker task. Non-decision time of incongruent condition (derived from the Drift Diffusion Model)",
    
    si_a1        = "Simon task. Boundary separation of congruent condition (derived from the Drift Diffusion Model)",
    si_a2        = "Simon task. Boundary separation of incongruent condition (derived from the Drift Diffusion Model)",
    si_v1        = "Simon task. Drift rate of congruent condition (derived from the Drift Diffusion Model)",
    si_v2        = "Simon task. Drift rate of incongruent condition (derived from the Drift Diffusion Model)",
    si_t1        = "Simon task. Non-decision time of congruent condition (derived from the Drift Diffusion Model)",
    si_t2        = "Simon task. Non-decision time of incongruent condition (derived from the Drift Diffusion Model)",
    
    cs_a1        = "Color-shape task. Boundary separation of repeat condition (derived from the Drift Diffusion Model)",
    cs_a2        = "Color-shape task. Boundary separation of switch condition (derived from the Drift Diffusion Model)",
    cs_v1        = "Color-shape task. Drift rate of repeat condition (derived from the Drift Diffusion Model)",
    cs_v2        = "Color-shape task. Drift rate of switch condition (derived from the Drift Diffusion Model)",
    cs_t1        = "Color-shape task. Non-decision time of repeat condition (derived from the Drift Diffusion Model)",
    cs_t2        = "Color-shape task. Non-decision time of switch condition (derived from the Drift Diffusion Model)",
    
    gl_a1        = "Global-Local task. Boundary separation of repeat condition (derived from the Drift Diffusion Model)",
    gl_a2        = "Global-Local task. Boundary separation of switch condition (derived from the Drift Diffusion Model)",
    gl_v1        = "Global-Local task. Drift rate of repeat condition (derived from the Drift Diffusion Model)",
    gl_v2        = "Global-Local task. Drift rate of switch condition (derived from the Drift Diffusion Model)",
    gl_t1        = "Global-Local task. Non-decision time of repeat condition (derived from the Drift Diffusion Model)",
    gl_t2        = "Global-Local task. Non-decision time of switch condition (derived from the Drift Diffusion Model)",
    
    as_a1        = "Animacy-Size task. Boundary separation of repeat condition (derived from the Drift Diffusion Model)",
    as_a2        = "Animacy-Size task. Boundary separation of switch condition (derived from the Drift Diffusion Model)",
    as_v1        = "Animacy-Size task. Drift rate of repeat condition (derived from the Drift Diffusion Model)",
    as_v2        = "Animacy-Size task. Drift rate of switch condition (derived from the Drift Diffusion Model)",
    as_t1        = "Animacy-Size task. Non-decision time of repeat condition (derived from the Drift Diffusion Model)",
    as_t2        = "Animacy-Size task. Non-decision time of switch condition (derived from the Drift Diffusion Model)",
    
    fl_a1_c        = "Flanker task. Boundary separation of congruent condition (corrected for noise and state anxiety)",
    fl_a2_c        = "Flanker task. Boundary separation of incongruent condition (corrected for noise and state anxiety)",
    fl_v1_c        = "Flanker task. Drift rate of congruent condition (corrected for noise and state anxiety)",
    fl_v2_c        = "Flanker task. Drift rate of incongruent condition  (corrected for noise and state anxiety)",
    fl_t1_c        = "Flanker task. Non-decision time of congruent condition (corrected for noise and state anxiety) ",
    fl_t2_c        = "Flanker task. Non-decision time of incongruent condition (corrected for noise and state anxiety) ",
    
    si_a1_c        = "Simon task. Boundary separation of congruent condition (corrected for noise and state anxiety) ",
    si_a2_c        = "Simon task. Boundary separation of incongruent condition  (corrected for noise and state anxiety)",
    si_v1_c        = "Simon task. Drift rate of congruent condition  (corrected for noise and state anxiety)",
    si_v2_c        = "Simon task. Drift rate of incongruent condition (corrected for noise and state anxiety) ",
    si_t1_c        = "Simon task. Non-decision time of congruent condition (corrected for noise and state anxiety) ",
    si_t2_c        = "Simon task. Non-decision time of incongruent condition (corrected for noise and state anxiety) ",
    
    cs_a1_c        = "Color-shape task. Boundary separation of repeat condition (corrected for noise and state anxiety) ",
    cs_a2_c        = "Color-shape task. Boundary separation of switch condition (corrected for noise and state anxiety) ",
    cs_v1_c        = "Color-shape task. Drift rate of repeat condition (corrected for noise and state anxiety) ",
    cs_v2_c        = "Color-shape task. Drift rate of switch condition (corrected for noise and state anxiety) ",
    cs_t1_c        = "Color-shape task. Non-decision time of repeat condition (corrected for noise and state anxiety) ",
    cs_t2_c        = "Color-shape task. Non-decision time of switch condition (corrected for noise and state anxiety) ",
    
    gl_a1_c        = "Global-Local task. Boundary separation of repeat condition (corrected for noise and state anxiety) ",
    gl_a2_c        = "Global-Local task. Boundary separation of switch condition (corrected for noise and state anxiety) ",
    gl_v1_c        = "Global-Local task. Drift rate of repeat condition (corrected for noise and state anxiety) ",
    gl_v2_c        = "Global-Local task. Drift rate of switch condition (corrected for noise and state anxiety)",
    gl_t1_c        = "Global-Local task. Non-decision time of repeat condition (corrected for noise and state anxiety)",
    gl_t2_c        = "Global-Local task. Non-decision time of switch condition (corrected for noise and state anxiety)",
    
    as_a1_c        = "Animacy-Size task. Boundary separation of repeat condition (corrected for noise and state anxiety)",
    as_a2_c        = "Animacy-Size task. Boundary separation of switch condition (corrected for noise and state anxiety)",
    as_v1_c        = "Animacy-Size task. Drift rate of repeat condition (corrected for noise and state anxiety)",
    as_v2_c        = "Animacy-Size task. Drift rate of switch condition (corrected for noise and state anxiety)",
    as_t1_c        = "Animacy-Size task. Non-decision time of repeat condition (corrected for noise and state anxiety)",
    as_t2_c        = "Animacy-Size task. Non-decision time of switch condition (corrected for noise and state anxiety)",
  ) |> 
  sjlabelled::val_labels(
    urb        = c("Extremely urban (2,500 or more)" = 1,
                   "Very urban (1,500 to 2,500)" = 2,
                   "Moderately urban (1,000 to 1,500)" = 3,
                   "Slightly urban (500 to 1,000)" = 4,
                   "Not urban (less than 500)" = 5),
    edu        = c("Primary school" = 1, 
                   "vmbo (intermediate secondary education, US: junior high school)" = 2,
                   "havo/vwo (higher secondary education/preparatory university education, US: senior high school)" = 3,
                   "mbo (intermediate vocational education, US: junior college)" = 4,
                   "hbo (higher vocational education, US: college)" = 5,
                   "wo (university)" = 6,
                   "Other, Not (yet) completed any education, or not yet started any education" = NA)
  )

codebook <- create_codebook(data_clean)

# 6. Save data ------------------------------------------------------------

save(data_clean, file = "1_data/3_AnalysisData/clean_data.RData")
save(exclusions, file = "3_output/Results/exclusions.RData")

openxlsx::write.xlsx(codebook, "1_data/3_AnalysisData/metadata/codebook_AnalysisData.xlsx", colWidths = "auto")

# 7. Remove data from global environment ----------------------------------

rm(list = names(which(!unlist(eapply(.GlobalEnv, 
                                     \(x) inherits(x, what = "function"))))))

