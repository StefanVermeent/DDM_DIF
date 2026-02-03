
# 1. Load data ------------------------------------------------------------

load("1_data/3_AnalysisData/clean_data.RData")

data_clean <- data_clean |> 
  mutate(
    across(matches("v(1|2)"), ~scale(.) |> as.numeric(), .names = "{.col}_l"),
    across(matches("urb_c|edu_c|age_c|child_dep|child_thr"), ~scale(.) |> as.numeric())
  )


mxOption(key="Number of Threads", value=imxGetNumThreads())


# 2. Configural invariance ------------------------------------------------

## 2.1 Create categorical moderators ----

data_clean_conf <- data_clean |> 
  mutate(
    child_dep_cat = case_when(
      child_dep < median(child_dep, na.rm = TRUE) ~ "low",
      child_dep == median(child_dep, na.rm = TRUE) ~ "mid",
      child_dep > median(child_dep, na.rm = TRUE) ~ "high"
    ),
    child_thr_cat = case_when(
      child_thr <  median(child_thr, na.rm = TRUE) ~ "low",
      child_thr == median(child_thr, na.rm = TRUE) ~ "mid",
      child_thr >=  median(child_thr, na.rm = TRUE) ~ "high",
    ),
    urb_cat = urb,
    age_cat = ifelse(age <= 45, "18-45", "45+"),
    edu_cat = case_when(
      edu == 1 | edu == 2 ~ "primary/vmbo",
      edu == 3            ~ "havo/vwo",
      edu == 4            ~ "mbo",
      edu == 5            ~ "hbo",
      edu == 6            ~ "university",
    )
  )

## 2.2 Configural invariance for age ----

model <-   
  '
  # Factor loadings
  V =~ NA*fl_v1_l + fl_v2_l + si_v1_l + si_v2_l + cs_v1_l + cs_v2_l + gl_v1_l + gl_v2_l + as_v1_l + as_v2_l
  
  # (Co-)variances
  V ~~ 1*V 

  fl_v1_l ~~ fl_v2_l
  si_v1_l ~~ si_v2_l
  cs_v1_l ~~ cs_v2_l
  gl_v1_l ~~ gl_v2_l
  as_v1_l ~~ as_v2_l
'


config_age_v <- lavaan::cfa(
  model = model, 
  data = data_clean_conf, 
  group = "age_cat",
  missing = "ml")

config_age_v_sum <- summary(config_age_v, fit.measures = TRUE, standardized = TRUE)
config_age_v_fitstats <- config_age_v_sum$fit[c('chisq', 'df', 'pvalue', 'cfi', 'rmsea', 'rmsea.ci.lower', 'rmsea.ci.upper')]

## 2.3 Configural invariance for education ----

config_edu_v <- lavaan::cfa(
  model = model, 
  data = data_clean_conf, 
  group = "edu_cat",
  missing = "ml")

config_edu_v_sum <- summary(config_edu_v, fit.measures = TRUE, standardized = TRUE)
config_edu_v_fitstats <- config_edu_v_sum$fit[c('chisq', 'df', 'pvalue', 'cfi', 'rmsea', 'rmsea.ci.lower', 'rmsea.ci.upper')]

## 2.4 Configural invariance for urbanicity ----

config_urb_v <- lavaan::cfa(
  model = model, 
  data = data_clean_conf, 
  group = "urb_cat",
  missing = "ml")

config_urb_v_sum <- summary(config_urb_v, fit.measures = TRUE, standardized = TRUE)
config_urb_v_fitstats <- config_urb_v_sum$fit[c('chisq', 'df', 'pvalue', 'cfi', 'rmsea', 'rmsea.ci.lower', 'rmsea.ci.upper')]

## 2.5 Configural invariance for threat

config_thr_v <- lavaan::cfa(
  model = model, 
  data = data_clean_conf, 
  group = "child_thr_cat",
  missing = "ml")

config_thr_v_sum <- summary(config_thr_v, fit.measures = TRUE, standardized = TRUE)
config_thr_v_fitstats <- config_thr_v_sum$fit[c('chisq', 'df', 'pvalue', 'cfi', 'rmsea', 'rmsea.ci.lower', 'rmsea.ci.upper')]

## 2.5 Configural invariance for threat

config_dep_v <- lavaan::cfa(
  model = model, 
  data = data_clean_conf, 
  group = "child_dep_cat",
  missing = "ml")

config_dep_v_sum <- summary(config_dep_v, fit.measures = TRUE, standardized = TRUE)
config_dep_v_fitstats <- config_dep_v_sum$fit[c('chisq', 'df', 'pvalue', 'cfi', 'rmsea', 'rmsea.ci.lower', 'rmsea.ci.upper')]

save(config_age_v_fitstats, config_edu_v_fitstats, config_urb_v_fitstats, config_thr_v_fitstats, config_dep_v_fitstats,
     file = "3_output/Results/1_SEM/MI_config_v.RData")


# 3. MNLFA - Assess full measurement invariance ---------------------------

## 3.1 Create data objects ----
mxdata <- mxData(observed = data_clean, type = "raw")
manVars <- colnames(data_clean |> select(matches("_v(1|2)_l$")))  
nv <- length(manVars)
manVars

## 3.2 Specify and fit the configural model ----

### 3.2.1 Matrices for indicator intercepts of configural model ----

matT0 <- mxMatrix(type="Full", nrow=1, ncol=nv,
                  free=TRUE,
                  values=0,
                  name="matT0")

matB1 <- mxMatrix(type="Full", nrow=1, ncol=nv,
                  free=TRUE,
                  values=0,
                  name="matB1")
matB2 <- mxMatrix(type="Full", nrow=1, ncol=nv,
                  free=TRUE,
                  values=0,
                  name="matB2")
matB3 <- mxMatrix(type="Full", nrow=1, ncol=nv,
                  free=TRUE,
                  values=0,
                  name="matB3")
matB4 <- mxMatrix(type="Full", nrow=1, ncol=nv,
                  free=TRUE,
                  values=0,
                  name="matB4")
matB5 <- mxMatrix(type="Full", nrow=1, ncol=nv,
                  free=TRUE,
                  values=0,
                  name="matB5")

### 3.2.2 Matrices for factor loadings of configural model ----

matL0 <- mxMatrix(type="Full", nrow=nv, ncol=1,
                  free=TRUE,
                  values=1,
                  byrow=TRUE,
                  name="matL0")

matC1 <- mxMatrix(type="Full", nrow=nv, ncol=1,
                  free=TRUE,
                  byrow=TRUE,
                  values=0,
                  name="matC1")
matC2 <- mxMatrix(type="Full", nrow=nv, ncol=1,
                  free=TRUE,
                  byrow=TRUE,
                  values=0,
                  name="matC2")
matC3 <- mxMatrix(type="Full", nrow=nv, ncol=1,
                  free=TRUE,
                  byrow=TRUE,
                  values=0,
                  name="matC3")
matC4 <- mxMatrix(type="Full", nrow=nv, ncol=1,
                  free=TRUE,
                  byrow=TRUE,
                  values=0,
                  name="matC4")
matC5 <- mxMatrix(type="Full", nrow=nv, ncol=1,
                  free=TRUE,
                  byrow=TRUE,
                  values=0,
                  name="matC5")

### 3.2.3 Matrices for residual variances of indicators ----

# Add covariances for parameters of the same task
resid_mat <- outer(sub("_.*", "", manVars), sub("_.*", "", manVars), "==") * 0.5
diag(resid_mat) <- 1

matE0 <- mxMatrix(type="Symm", nrow=nv, ncol=nv,
                  free=outer(sub("_.*", "", manVars), sub("_.*", "", manVars), "=="),
                  values=resid_mat,
                  name="matE0")

matD1 <- mxMatrix(type="Diag", nrow=nv, ncol=nv,
                  free=FALSE,
                  values=0,
                  name="matD1")
matD2 <- mxMatrix(type="Diag", nrow=nv, ncol=nv,
                  free=FALSE,
                  values=0,
                  name="matD2")
matD3 <- mxMatrix(type="Diag", nrow=nv, ncol=nv,
                  free=FALSE,
                  values=0,
                  name="matD3")
matD4 <- mxMatrix(type="Diag", nrow=nv, ncol=nv,
                  free=FALSE,
                  values=0,
                  name="matD4")
matD5 <- mxMatrix(type="Diag", nrow=nv, ncol=nv,
                  free=FALSE,
                  values=0,
                  name="matD5")

### 3.2.4 Matrices for common factor (co-)variance ----

matP0 <- mxMatrix(type="Full", nrow=1, ncol=1,
                  free=FALSE, 
                  values=1,
                  name="matP0")

matH1 <- mxMatrix(type="Full", nrow=1, ncol=1,
                  free=FALSE,  # to identify the model
                  values=0,
                  name="matH1")
matH2 <- mxMatrix(type="Full", nrow=1, ncol=1,
                  free=FALSE,  # to identify the model
                  values=0,
                  name="matH2")
matH3 <- mxMatrix(type="Full", nrow=1, ncol=1,
                  free=FALSE,  # to identify the model
                  values=0,
                  name="matH3")
matH4 <- mxMatrix(type="Full", nrow=1, ncol=1,
                  free=FALSE,  # to identify the model
                  values=0,
                  name="matH4")
matH5 <- mxMatrix(type="Full", nrow=1, ncol=1,
                  free=FALSE,  # to identify the model
                  values=0,
                  name="matH5")


### 3.2.5 Matrices for common factor means ----

matA0 <- mxMatrix(type="Full", nrow=1, ncol=1,
                  free=FALSE,
                  values=0,
                  name="matA0")

matG1 <- mxMatrix(type="Full", nrow=1, ncol=1,
                  free=TRUE, # to identify the model 
                  values=0,
                  name="matG1")
matG2 <- mxMatrix(type="Full", nrow=1, ncol=1,
                  free=TRUE, # to identify the model 
                  values=0,
                  name="matG2")
matG3 <- mxMatrix(type="Full", nrow=1, ncol=1,
                  free=TRUE, # to identify the model 
                  values=0,
                  name="matG3")
matG4 <- mxMatrix(type="Full", nrow=1, ncol=1,
                  free=TRUE, # to identify the model 
                  values=0,
                  name="matG4")
matG5 <- mxMatrix(type="Full", nrow=1, ncol=1,
                  free=TRUE, # to identify the model 
                  values=0,
                  name="matG5")


### 3.2.6 Matrices for definition variable (i.e., the moderators) ----

matV1 <- mxMatrix(type="Full", nrow=1, ncol=1, 
                  free=FALSE, 
                  labels="data.age_c", 
                  name = "Age")
matV2 <- mxMatrix(type="Full", nrow=1, ncol=1, 
                  free=FALSE, 
                  labels="data.edu_c", 
                  name = "Edu")
matV3 <- mxMatrix(type="Full", nrow=1, ncol=1, 
                  free=FALSE, 
                  labels="data.urb_c", 
                  name = "Urb")
matV4 <- mxMatrix(type="Full", nrow=1, ncol=1, 
                  free=FALSE, 
                  labels="data.child_thr", 
                  name = "Thr")
matV5 <- mxMatrix(type="Full", nrow=1, ncol=1, 
                  free=FALSE, 
                  labels="data.child_dep", 
                  name = "Dep")


### 3.2.7 Matrices of intercepts, factor loadings, and residual variances predicted by moderators ----

matT <- mxAlgebra(expression=matT0 + matB1*Age + matB2*Edu + matB3*Urb + matB4*Thr + matB5*Dep, 
                  name="matT")

matL <- mxAlgebra(expression=matL0 + matC1*Age + matC2*Edu + matC3*Urb + matC4*Thr + matC5*Dep, 
                  name="matL")

matE <- mxAlgebra(expression=matE0*exp(matD1*Age + matD2*Edu + matD3*Urb + matD4*Thr + matD5*Dep), 
                  name="matE")


### 3.2.8 Matrix for common factor means predicted by moderators ----

matA <- mxAlgebra(expression=matA0 + matG1*Age + matG2*Edu + matG3*Urb + matG4*Thr + matG5*Dep, 
                  name="matA")


### 3.2.9 Matrix for common factor variances ----

matVar <- mxAlgebra(expression=(matP0 * exp(matH1*Age + matH2*Edu + matH3*Urb + matH4*Thr + matH5*Dep)), 
                    name="matVar")

matIa <- mxMatrix(type="Diag", nrow=1, ncol=1, 
                  free=FALSE,
                  values=1, 
                  name="matIa")

matP <- mxAlgebra(expression = matIa * matVar,
                  name="matP")


### 3.2.10 Specify model-implied matrices ----

matM <- mxAlgebra(expression = matT + t(matL%*%matA),
                  name = "matM")

matC <- mxAlgebra(expression = matL %*% matP %*% t(matL) + matE, 
                  name="matC") 


### 3.2.11 Specify expectation and fit function ----

expF <- mxExpectationNormal(covariance="matC", 
                            means="matM",
                            dimnames=manVars)

fitF <- mxFitFunctionML() 
modConfig_v <- mxModel(model="Configural", 
                       matT, matT0, matB1, matB2, matB3, matB4, matB5,
                       matL, matL0, matC1, matC2, matC3, matC4, matC5,
                       matE, matE0, matD1, matD2, matD3, matD4, matD5,
                       matP, matP0, matH1, matH2, matH3, matH4, matH5,
                       matA, matA0, matG1, matG2, matG3, matG4, matG5,
                       matIa, matV1, matV2, matV3, matV4, matV5,
                       matVar, matM, matC, 
                       expF, fitF, mxdata)

fitConfig_v <- mxRun(modConfig_v)
summary(fitConfig_v) 


## 3.3 Specify and fit the scalar model ----

# Note: Across all models except the Configural model above, the common-factor means, common-factor variances, 
# common-factor covariance, and indicators' residual (co-)variance are allowed to vary as a function of the moderators. 
# Such models are also referred to as 'heteroskedastic MNLFA models 
# (Kolbe et al., 2021; https://doi.org/10.1080/10705511.2020.1766357).

### 3.3.1 Fix direct effects on item intercepts and factor loadings to zero ----

# Constrain indicator intercept moderations to zero
matB1 <- mxMatrix(type = "Full", nrow = 1,
                  ncol = nv,
                  free = FALSE,
                  values = 0,
                  name = "matB1")
matB2 <- mxMatrix(type = "Full", nrow = 1,
                  ncol = nv,
                  free = FALSE,
                  values = 0,
                  name = "matB2")
matB3 <- mxMatrix(type = "Full", nrow = 1,
                  ncol = nv,
                  free = FALSE,
                  values = 0,
                  name = "matB3")
matB4 <- mxMatrix(type = "Full", nrow = 1,
                  ncol = nv,
                  free = FALSE,
                  values = 0,
                  name = "matB4")
matB5 <- mxMatrix(type = "Full", nrow = 1,
                  ncol = nv,
                  free = FALSE,
                  values = 0,
                  name = "matB5")

# Constrain factor loading moderations to zero
matC1 <- mxMatrix(type = "Full", nrow = nv,
                  ncol = 1,
                  free = FALSE,
                  values = 0,
                  name = "matC1")
matC2 <- mxMatrix(type = "Full", nrow = nv,
                  ncol = 1,
                  free = FALSE,
                  values = 0,
                  name = "matC2")
matC3 <- mxMatrix(type = "Full", nrow = nv,
                  ncol = 1,
                  free = FALSE,
                  values = 0,
                  name = "matC3")
matC4 <- mxMatrix(type = "Full", nrow = nv,
                  ncol = 1,
                  free = FALSE,
                  values = 0,
                  name = "matC4")
matC5 <- mxMatrix(type = "Full", nrow = nv,
                  ncol = 1,
                  free = FALSE,
                  values = 0,
                  name = "matC5")

### 3.3.3 Fit scalar model ----

modScalar_v <- mxModel(model="Scalar", 
                       matT, matT0, matB1, matB2, matB3, matB4, matB5,
                       matL, matL0, matC1, matC2, matC3, matC4, matC5,
                       matE, matE0, matD1, matD2, matD3, matD4, matD5,
                       matP, matP0, matH1, matH2, matH3, matH4, matH5,
                       matA, matA0, matG1, matG2, matG3, matG4, matG5,
                       matIa, matV1, matV2, matV3, matV4, matV5,
                       matVar, matM, matC, 
                       expF, fitF, mxdata)

fitScalar_v <- mxRun(modScalar_v)
summary(fitScalar_v)


### 3.3.4 Compare fit of unconstrained model with constrained model ----

omn_miTest_v <- mxCompare(fitConfig_v, fitScalar_v)
omn_miTest_v[2,c(7,8,9)]
omn_miTest_v$p[2] < 0.001

save(fitConfig_v, fitScalar_v, omn_miTest_v, file = "3_output/Results/2_MNLFA/MNLFA_fit_v.RData")


## 3.4 Select anchor indicators ----

### 3.4.1 Run unconstrained model for each indicator ----
fitAbo_v <- list()

for (i in 1:nv) {
  freeparT <- matrix(FALSE, nrow=1, ncol=10)
  freeparT[i] <- TRUE
  freeparL <- matrix(FALSE, nrow=10, ncol=1)
  freeparL[i,1] <- TRUE
  matB1 <- mxMatrix(type="Full", nrow=1, ncol=nv,
                    free=freeparT,
                    values=0,
                    name="matB1")
  matB2 <- mxMatrix(type="Full", nrow=1, ncol=nv,
                    free=freeparT,
                    values=0,
                    name="matB2")
  matB3 <- mxMatrix(type="Full", nrow=1, ncol=nv,
                    free=freeparT,
                    values=0,
                    name="matB3")
  matB4 <- mxMatrix(type="Full", nrow=1, ncol=nv,
                    free=freeparT,
                    values=0,
                    name="matB4")
  matB5 <- mxMatrix(type="Full", nrow=1, ncol=nv,
                    free=freeparT,
                    values=0,
                    name="matB5")
  matC1 <- mxMatrix(type="Full", nrow=nv, ncol=1,
                    free=freeparL,
                    values=0,
                    byrow=TRUE,
                    name="matC1")
  matC2 <- mxMatrix(type="Full", nrow=nv, ncol=1,
                    free=freeparL,
                    values=0,
                    byrow=TRUE,
                    name="matC2")
  matC3 <- mxMatrix(type="Full", nrow=nv, ncol=1,
                    free=freeparL,
                    values=0,
                    byrow=TRUE,
                    name="matC3")
  matC4 <- mxMatrix(type="Full", nrow=nv, ncol=1,
                    free=freeparL,
                    values=0,
                    byrow=TRUE,
                    name="matC4")
  matC5 <- mxMatrix(type="Full", nrow=nv, ncol=1,
                    free=freeparL,
                    values=0,
                    byrow=TRUE,
                    name="matC5")
  modAbo_v <- mxModel(model=paste0("All_but_", i), 
                      matT, matT0, matB1, matB2, matB3, matB4, matB5,
                      matL, matL0, matC1, matC2, matC3, matC4, matC5,
                      matE, matE0, matD1, matD2, matD3, matD4, matD5,
                      matP, matP0, matH1, matH2, matH3, matH4, matH5,
                      matA, matA0, matG1, matG2, matG3, matG4, matG5,
                      matIa, matV1, matV2, matV3, matV4, matV5,
                      matVar, matM, matC, 
                      expF, fitF, mxdata)
  
  fitAbo_v[[i]] <- mxRun(modAbo_v) 
}

### 3.4.2 Compare constrained model with all unconstrained models ----

anchorTest_v <- mxCompare(fitAbo_v, fitScalar_v)
anchorOut_v <- data.frame(Name=paste0("Indicator",1:10), 
                          X2=anchorTest_v$diffLL[seq(2,20,2)],
                          df=anchorTest_v$diffdf[seq(2,20,2)],
                          p=anchorTest_v$p[seq(2,20,2)]) %>% 
  arrange(X2)


### 3.4.3 Save anchors in object ----

anchors_v <- head(anchorOut_v,2) %>% 
  pull(Name) %>% 
  stringr::str_remove("Indicator") %>% 
  as.numeric()

load("3_output/Results/2_MNLFA/MNLFA_fit_v.RData")

save(fitConfig_v, fitScalar_v, omn_miTest_v, 
     fitAbo_v, anchorTest_v, anchorOut_v, anchors_v,
     file = "3_output/Results/2_MNLFA/MNLFA_fit_v.RData")


## 3.5 Test partial invariance ----

### 3.5.1 Specify which DIF effects can be estimated ----

freeparT <- matrix(data=TRUE, nrow=1, ncol=10)
freeparT[1,c(anchors_v)] <- FALSE

freeparL <- matrix(TRUE, nrow=10, ncol=1, byrow=TRUE)
freeparL[anchors_v,1] <- FALSE


### 3.5.2 Specify matrices for unconstrained model ----
matB1 <- mxMatrix(type="Full", nrow=1, ncol=nv,
                  free=freeparT,
                  values=0,
                  name="matB1")
matB2 <- mxMatrix(type="Full", nrow=1, ncol=nv,
                  free=freeparT,
                  values=0,
                  name="matB2")
matB3 <- mxMatrix(type="Full", nrow=1, ncol=nv,
                  free=freeparT,
                  values=0,
                  name="matB3")
matB4 <- mxMatrix(type="Full", nrow=1, ncol=nv,
                  free=freeparT,
                  values=0,
                  name="matB4")
matB5 <- mxMatrix(type="Full", nrow=1, ncol=nv,
                  free=freeparT,
                  values=0,
                  name="matB5")
matC1 <- mxMatrix(type="Full", nrow=nv, ncol=1,
                  free=freeparL,
                  values=0,
                  name="matC1")
matC2 <- mxMatrix(type="Full", nrow=nv, ncol=1,
                  free=freeparL,
                  values=0,
                  name="matC2")
matC3 <- mxMatrix(type="Full", nrow=nv, ncol=1,
                  free=freeparL,
                  values=0,
                  name="matC3")
matC4 <- mxMatrix(type="Full", nrow=nv, ncol=1,
                  free=freeparL,
                  values=0,
                  name="matC4")
matC5 <- mxMatrix(type="Full", nrow=nv, ncol=1,
                  free=freeparL,
                  values=0,
                  name="matC5")


### 3.5.3 Make mxModel object and run the model ----
modAnchors_v <- mxModel(model="AnchorsOnly", 
                        matT, matT0, matB1, matB2, matB3, matB4, matB5,
                        matL, matL0, matC1, matC2, matC3, matC4, matC5,
                        matE, matE0, matD1, matD2, matD3, matD4, matD5,
                        matP, matP0, matH1, matH2, matH3, matH4, matH5,
                        matA, matA0, matG1, matG2, matG3, matG4, matG5,
                        matIa, matV1, matV2, matV3, matV4, matV5,
                        matVar, matM, matC, 
                        expF, fitF, mxdata)

fitAnchors_v <- mxRun(modAnchors_v)

load("3_output/Results/2_MNLFA/MNLFA_fit_v.RData")

save(fitConfig_v, fitScalar_v, omn_miTest_v, 
     fitAbo_v, anchorTest_v, anchorOut_v, anchors_v, fitAnchors_v,
     file = "3_output/Results/2_MNLFA/MNLFA_fit_v.RData")


### 3.5.4 Run constrained model for each moderator and individual DIF path (except the anchors) ----
testIn_v <- c(1:10)[-c(anchors_v)]

matC1_unc <- matC1
matC2_unc <- matC2
matC3_unc <- matC3
matC4_unc <- matC4
matC5_unc <- matC5
matB1_unc <- matB1
matB2_unc <- matB2
matB3_unc <- matB3
matB4_unc <- matB4
matB5_unc <- matB5


apo_grid_v <- expand_grid(
  item = testIn_v, mods = c("age", "edu", "urb", "thr", "dep"), par = c("intercept", "loading")
) 

future::plan(multisession, workers = future::availableCores() - 1)

fitApo_v <- 1:nrow(apo_grid_v) |>  
  furrr::future_map_dfr(function(i) {
    
    
    print(glue("{i} - Initiation successful"))
    if(apo_grid_v$par[[i]] == "loading") { 
      freeparTa <- freeparT
      freeparLa <- freeparL
      freeparLa[apo_grid_v$item[[i]],1] <- FALSE
      
      # Constrain parL only for current moderator
      matC1 <- mxMatrix(type="Full", nrow=nv, ncol=1,
                        free=if(apo_grid_v$mods[[i]] == "age") freeparLa else freeparL,
                        values=0,
                        name="matC1")
      matC2 <- mxMatrix(type="Full", nrow=nv, ncol=1,
                        free=if(apo_grid_v$mods[[i]] == "edu") freeparLa else freeparL,
                        values=0,
                        name="matC2")
      matC3 <- mxMatrix(type="Full", nrow=nv, ncol=1,
                        free=if(apo_grid_v$mods[[i]] == "urb") freeparLa else freeparL,
                        values=0,
                        name="matC3")
      matC4 <- mxMatrix(type="Full", nrow=nv, ncol=1,
                        free=if(apo_grid_v$mods[[i]] == "thr") freeparLa else freeparL,
                        values=0,
                        name="matC4")
      matC5 <- mxMatrix(type="Full", nrow=nv, ncol=1,
                        free=if(apo_grid_v$mods[[i]] == "dep") freeparLa else freeparL,
                        values=0,
                        name="matC5")
      matB1 <- matB1_unc
      matB2 <- matB2_unc
      matB3 <- matB3_unc
      matB4 <- matB4_unc
      matB5 <- matB5_unc
    }
    
    if(apo_grid_v$par[[i]] == "intercept") { 
      freeparTa <- freeparT
      freeparLa <- freeparL
      freeparTa[apo_grid_v$item[[i]]] <- FALSE
      
      # Constrain parT only for current moderator
      matB1 <- mxMatrix(type="Full", nrow=1, ncol=nv,
                        free=if(apo_grid_v$mods[[i]] == "age") freeparTa else freeparT,
                        values=0,
                        name="matB1")
      matB2 <- mxMatrix(type="Full", nrow=1, ncol=nv,
                        free=if(apo_grid_v$mods[[i]] == "edu") freeparTa else freeparT,
                        values=0,
                        name="matB2")
      matB3 <- mxMatrix(type="Full", nrow=1, ncol=nv,
                        free=if(apo_grid_v$mods[[i]] == "urb") freeparTa else freeparT,
                        values=0,
                        name="matB3")
      matB4 <- mxMatrix(type="Full", nrow=1, ncol=nv,
                        free=if(apo_grid_v$mods[[i]] == "thr") freeparTa else freeparT,
                        values=0,
                        name="matB4")
      matB5 <- mxMatrix(type="Full", nrow=1, ncol=nv,
                        free=if(apo_grid_v$mods[[i]] == "dep") freeparTa else freeparT,
                        values=0,
                        name="matB5")
      matC1 <- matC1_unc
      matC2 <- matC2_unc
      matC3 <- matC3_unc
      matC4 <- matC4_unc
      matC5 <- matC5_unc
      
    }
    
    # Construct and run model
    modApo_v <- mxModel(model=glue("Anchors_plus_{apo_grid_v$item[[i]]}_{apo_grid_v$mods[[i]]}_{apo_grid_v$par[[i]]}"), 
                        matT, matT0, matB1, matB2, matB3, matB4, matB5,
                        matL, matL0, matC1, matC2, matC3, matC4, matC5,
                        matE, matE0, matD1, matD2, matD3, matD4, matD5,
                        matP, matP0, matH1, matH2, matH3, matH4, matH5,
                        matA, matA0, matG1, matG2, matG3, matG4, matG5,
                        matIa, matV1, matV2, matV3, matV4, matV5,
                        matVar, matM, matC, 
                        expF, fitF, mxdata)
    fitApo_v <- mxRun(modApo_v)
    
    # Remove constrain specific to current iteration from matrices
    matB1 <- matB1_unc
    matB2 <- matB2_unc
    matB3 <- matB3_unc
    matB4 <- matB4_unc    
    matB5 <- matB5_unc
    
    matC1 <- matC1_unc
    matC2 <- matC2_unc
    matC3 <- matC3_unc
    matC4 <- matC4_unc    
    matC5 <- matC5_unc
    
    
    print(glue("{i} was run"))
    
    # Store results in Tibble
    tibble(
      item = apo_grid_v$item[[i]],
      mod  = apo_grid_v$mods[[i]],
      par  = apo_grid_v$par[[i]],
      fit  = list(fitApo_v)
    )
    
  }, .options=furrr_options(seed=TRUE))

plan("sequential")


### Compare fit of unconstrained model with all constrained models ----
miTest_Apo_v  <- fitApo_v |> 
  rowwise() |> 
  mutate(
    fitcompare = mxCompare(fitAnchors_v, fit) |> as_tibble() |> list(),
    pvalue     = fitcompare |> unnest(cols = c()) |> pull(p) |> last()
  )


load("3_output/Results/2_MNLFA/MNLFA_fit_v.RData")

save(fitConfig_v, fitScalar_v, omn_miTest_v, 
     fitAbo_v, anchorTest_v, anchorOut_v, anchors_v, fitAnchors_v,
     fitApo_v, miTest_Apo_v,
     file = "3_output/Results/2_MNLFA/MNLFA_fit_v.RData")

# 4. Remove data from global environment ----------------------------------

rm(list = names(which(!unlist(eapply(.GlobalEnv, 
                                     \(x) inherits(x, what = "function"))))))
