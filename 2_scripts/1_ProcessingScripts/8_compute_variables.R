
# 1. Load data ------------------------------------------------------------

load("1_data/2_IntermediateData/Qdata_clean.RData")
load("1_data/2_IntermediateData/Tdata_sum_clean.RData")

load("3_output/Results/exclusions.RData")

load("1_data/2_IntermediateData/DDM_flanker_fit.RData")
load("1_data/2_IntermediateData/DDM_simon_fit.RData")
load("1_data/2_IntermediateData/DDM_globallocal_fit.RData")
load("1_data/2_IntermediateData/DDM_colorshape_fit.RData")
load("1_data/2_IntermediateData/DDM_animacysize_fit.RData")


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



# 3. Combine data ---------------------------------------------------------

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
dropvars <- data_clean |> select(nomem_encr, nohouse_encr, wave) |> names()
raw <- data_clean |> select(matches("_rt_"), matches("_score$")) |> names()

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
    urb          = "Urban character of place of residence; centered",
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
