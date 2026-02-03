
# 1. Load data ------------------------------------------------------------

load("1_data/3_AnalysisData/clean_data.RData")

data_clean <- data_clean |> 
  mutate(across(ends_with("_score"), ~scale(.) |> as.numeric(), .names = "{.col}_c")) |> 
  mutate(across(matches("v(1|2)$|a(1|2)$|t(1|2)$"), ~scale(.) |> as.numeric(), .names = "{.col}_c"))

# 2. Preregistered RT models ----------------------------------------------

## 2.1 Unity model ----

model_rt_un <- 
  '
  EF =~ NA*fl_score_c + si_score_c + cs_score_c + gl_score_c + as_score_c 
  
  EF ~~ 1*EF
  
  fl_score_c ~ 0
  si_score_c ~ 0
  cs_score_c ~ 0
  gl_score_c ~ 0
  as_score_c ~ 0
'

rt_un_fit <- lavaan::cfa(
  model = model_rt_un, 
  data = data_clean, 
  missing = "ml")

rt_un_fit_sum <- lavaan::summary(rt_un_fit, fit.measures = TRUE)

# Explore areas of misfit
modificationIndices(rt_un_fit) |> as_tibble() |> arrange()


## 2.2 Diversity model

model_rt_div <- 
  '
  inh =~ fl_score_c + si_score_c
  shi =~ cs_score_c + gl_score_c + as_score_c

  inh ~~ 1*inh + shi
  shi ~~ 1*shi
  
  fl_score_c ~ 0
  si_score_c ~ 0
  cs_score_c ~ 0
  gl_score_c ~ 0
  as_score_c ~ 0

'

rt_div_fit <- lavaan::cfa(
  model = model_rt_div, 
  data = data_clean, 
  missing = "ml")

rt_div_fit_sum <- lavaan::summary(rt_div_fit, fit.measures = TRUE, standardized = TRUE)

# Explore areas of misfit
modificationIndices(rt_div_fit) |> as_tibble() |> arrange(desc(mi))


# 3. Non-preregistered RT models ------------------------------------------

## 3.1 Unity model (selected for main analyses) ----

data_clean <- data_clean |> 
    mutate(across(contains("_rt_"), ~log(.) |> scale() |> as.numeric(), .names = "{.col}_l"))

model_rt_un_expl1 <- 
  '
  # Factor loadings
  EF =~ NA*fl_rt_con_l + fl_rt_inc_l + si_rt_con_l + si_rt_inc_l + cs_rt_rep_l + cs_rt_sw_l + gl_rt_rep_l + gl_rt_sw_l + as_rt_rep_l + as_rt_sw_l
  
  # (Co-)variances
  EF ~~ 1*EF

  fl_rt_con_l ~~ fl_rt_inc_l
  si_rt_con_l ~~ si_rt_inc_l
  cs_rt_rep_l ~~ cs_rt_sw_l 
  gl_rt_rep_l ~~ gl_rt_sw_l 
  as_rt_rep_l ~~ as_rt_sw_l 
  
  fl_rt_con_l ~ 0
  si_rt_con_l ~ 0
  cs_rt_rep_l ~ 0
  gl_rt_rep_l ~ 0
  as_rt_rep_l ~ 0
  fl_rt_inc_l ~ 0
  si_rt_inc_l ~ 0
  cs_rt_sw_l ~ 0
  gl_rt_sw_l ~ 0
  as_rt_sw_l ~ 0
'

rt_un_expl1_fit <- lavaan::cfa(
  model = model_rt_un_expl1, 
  data = data_clean, 
  missing = "ml")

rt_un_expl1_fit_sum <- lavaan::summary(rt_un_expl1_fit, fit.measures = TRUE, standardized = TRUE)
rt_un_expl1_fitstats <- rt_un_expl1_fit_sum$fit[c('chisq', 'df', 'pvalue', 'cfi', 'rmsea', 'rmsea.ci.lower', 'rmsea.ci.upper')]




# 4. Drift rate model -----------------------------------------------------

model_v <- 
  '
  # Factor loadings
  V =~ NA*fl_v1_c + fl_v2_c + si_v1_c + si_v2_c + cs_v1_c + cs_v2_c + gl_v1_c + gl_v2_c + as_v1_c + as_v2_c
  
  # (Co-)variances
  V ~~ 1*V 

  fl_v1_c ~~ fl_v2_c
  si_v1_c ~~ si_v2_c
  cs_v1_c ~~ cs_v2_c
  gl_v1_c ~~ gl_v2_c
  as_v1_c ~~ as_v2_c
  
  
  fl_v1_c ~ 0
  fl_v2_c ~ 0
  si_v1_c ~ 0
  si_v2_c ~ 0
  cs_v1_c ~ 0
  cs_v2_c ~ 0
  gl_v1_c ~ 0
  gl_v2_c ~ 0
  as_v1_c ~ 0
  as_v2_c ~ 0
'

v_fit <- lavaan::cfa(
  model = model_v, 
  data = data_clean, 
  missing = "ml"
  )

v_fit_sum <- lavaan::summary(v_fit, fit.measures = TRUE, standardized = TRUE)
v_fitstats <- v_fit_sum$fit[c('chisq', 'df', 'pvalue', 'cfi', 'rmsea', 'rmsea.ci.lower', 'rmsea.ci.upper')]



# 5. Boundary separation model --------------------------------------------

model_a <- 
  '
  # Factor loadings
  A =~ NA*fl_a1 + fl_a2 + si_a1 + si_a2 + cs_a1 + cs_a2 + gl_a1 + gl_a2 + as_a1 + as_a2
  
  # (Co-)variances
  A ~~ 1*A

  fl_a1 ~~ fl_a2
  si_a1 ~~ si_a2
  cs_a1 ~~ cs_a2
  gl_a1 ~~ gl_a2
  as_a1 ~~ as_a2
'

a_fit <- lavaan::cfa(
  model = model_a, 
  data = data_clean, 
  missing = "ml")

a_fit_sum <- lavaan::summary(a_fit, fit.measures = TRUE, standardized = TRUE)
a_fitstats <- a_fit_sum$fit[c('chisq', 'df', 'pvalue', 'cfi', 'rmsea', 'rmsea.ci.lower', 'rmsea.ci.upper')]



# 6. Non-decision time model ----------------------------------------------

model_t <- 
  '
  # Factor loadings
  NDT =~ NA*fl_t1_c + fl_t2_c + si_t1_c + si_t2_c + cs_t1_c + cs_t2_c + gl_t1_c + gl_t2_c + as_t1_c + as_t2_c
  
  # (Co-)variances
  NDT ~~ 1*NDT

  fl_t1_c ~~ fl_t2_c
  si_t1_c ~~ si_t2_c
  cs_t1_c ~~ cs_t2_c
  gl_t1_c ~~ gl_t2_c
  as_t1_c ~~ as_t2_c
  
  fl_t1_c ~ 0
  fl_t2_c ~ 0
  si_t1_c ~ 0
  si_t2_c ~ 0
  cs_t1_c ~ 0
  cs_t2_c ~ 0
  gl_t1_c ~ 0
  gl_t2_c ~ 0
  as_t1_c ~ 0
  as_t2_c ~ 0
'

t_fit <- lavaan::cfa(
  model = model_t, 
  data = data_clean, 
  missing = "ml"
  )

t_fit_sum <- lavaan::summary(t_fit, fit.measures = TRUE, standardized = TRUE)
t_fitstats <- t_fit_sum$fit[c('chisq', 'df', 'pvalue', 'cfi', 'rmsea', 'rmsea.ci.lower', 'rmsea.ci.upper')]



# 7. Save data ------------------------------------------------------------

save(
  rt_un_expl1_fit, rt_un_expl1_fit_sum, rt_un_expl1_fitstats,
  v_fit, v_fit_sum, v_fitstats,
  a_fit, a_fit_sum, a_fitstats,
  t_fit, t_fit_sum, t_fitstats, 
  file = "3_output/Results/1_SEM/SEM_fit.RData"
)

# 8. Remove data from global environment ----------------------------------

rm(list = names(which(!unlist(eapply(.GlobalEnv, 
                                     \(x) inherits(x, what = "function"))))))
