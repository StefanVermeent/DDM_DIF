
# 1. Load data ------------------------------------------------------------

load("3_output/Results/MNLFA_fit_rt.RData")

load("1_data/3_AnalysisData/clean_data.RData")

data_clean <- data_clean |> 
  mutate(
    across(matches("(con|inc|rep|sw)$"), ~scale(.) |> as.numeric(), .names = "{.col}_l"),
    across(matches("urb_c|edu_c|age_c|child_dep|child_thr"), ~scale(.) |> as.numeric())
  )


mxOption(key="Number of Threads", value=imxGetNumThreads())

# 2. Final RT MNLFA model -----------------------------------------

## 2.1 Create data objects ----
mxdata <- mxData(observed = data_clean, type = "raw")
manVars <- colnames(data_clean |> select(matches("_(con|inc|sw|rep)_l$")))  
nv <- length(manVars)
manVars

CI <- mxCI(reference = c("matG1", "matG2", "matG3", "matG4", "matG5"))

## 2.2 Create a grid specifying parameters to be constrained/estimated for combinations of indicators, moderators, and model parameters ----

rt_grid <- miTest_Apo_rt |> 
  mutate(
    DIF     = ifelse(pvalue < .10, TRUE, FALSE)) |> 
  bind_rows( # Add rows for anchors
    expand_grid(item = c(anchors_rt), mod = unique(fitApo_rt$mod), par = unique(fitApo_rt$par), DIF = FALSE)
  ) |> 
  arrange(item) |> 
  group_by(par, mod) |> 
  summarise(free = DIF |> as.matrix() |> list())

## 2.3 Matrices for indicator intercepts ----

matT0 <- mxMatrix(type="Full", nrow=1, ncol=nv,
                  free=TRUE,
                  values=1,
                  name="matT0")

matB1 <- mxMatrix(type="Full", nrow=1, ncol=nv,
                  free=c(rt_grid |> filter(mod == "age", par == "intercept") |> unnest(free) |> pull(free) |> t()),
                  values=0,
                  name="matB1")

matB2 <- mxMatrix(type="Full", nrow=1, ncol=nv,
                  free=c(rt_grid |> filter(mod == "edu", par == "intercept") |> unnest(free) |> pull(free) |> t()),
                  values=0,
                  name="matB2")

matB3 <- mxMatrix(type="Full", nrow=1, ncol=nv,
                  free=c(rt_grid |> filter(mod == "urb", par == "intercept") |> unnest(free) |> pull(free) |> t()),
                  values=0,
                  name="matB3")

matB4 <- mxMatrix(type="Full", nrow=1, ncol=nv,
                  free=c(rt_grid |> filter(mod == "thr", par == "intercept") |> unnest(free) |> pull(free) |> t()),
                  values=0,
                  name="matB4")

matB5 <- mxMatrix(type="Full", nrow=1, ncol=nv,
                  free=c(rt_grid |> filter(mod == "dep", par == "intercept") |> unnest(free) |> pull(free) |> t()),
                  values=0,
                  name="matB5")


## 2.4 Matrices for factor loadings----

matL0 <- mxMatrix(type="Full", nrow=nv, ncol=1,
                  free=TRUE,
                  values=1,
                  byrow=TRUE,
                  name="matL0")

matC1 <- mxMatrix(type="Full", nrow=nv, ncol=1,
                  free=rt_grid |> filter(mod == "age", par == "loading") |> unnest(free) |> pull(free),
                  values=0,
                  name="matC1")

matC2 <- mxMatrix(type="Full", nrow=nv, ncol=1,
                  free=rt_grid |> filter(mod == "edu", par == "loading") |> unnest(free) |> pull(free),
                  values=0,
                  name="matC2")

matC3 <- mxMatrix(type="Full", nrow=nv, ncol=1,
                  free=rt_grid |> filter(mod == "urb", par == "loading") |> unnest(free) |> pull(free),
                  values=0,
                  name="matC3")

matC4 <- mxMatrix(type="Full", nrow=nv, ncol=1,
                  free=rt_grid |> filter(mod == "thr", par == "loading") |> unnest(free) |> pull(free),
                  values=0,
                  name="matC4")

matC5 <- mxMatrix(type="Full", nrow=nv, ncol=1,
                  free=rt_grid |> filter(mod == "dep", par == "loading") |> unnest(free) |> pull(free),
                  values=0,
                  name="matC5")

## 2.5 Matrices for residual variances of indicators ----

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


## 2.6 Matrices for common factor (co-)variance ----

matP0 <- mxMatrix(type="Full", nrow=1, ncol=1,
                  free=FALSE, 
                  values=1,
                  name="matP0")

matH1 <- mxMatrix(type="Full", nrow=1, ncol=1,
                  free=FALSE,  
                  values=0,
                  name="matH1")
matH2 <- mxMatrix(type="Full", nrow=1, ncol=1,
                  free=FALSE, 
                  values=0,
                  name="matH2")
matH3 <- mxMatrix(type="Full", nrow=1, ncol=1,
                  free=FALSE, 
                  values=0,
                  name="matH3")
matH4 <- mxMatrix(type="Full", nrow=1, ncol=1,
                  free=FALSE,  
                  values=0,
                  name="matH4")
matH5 <- mxMatrix(type="Full", nrow=1, ncol=1,
                  free=FALSE, 
                  values=0,
                  name="matH5")

## 2.7 Matrices for common factor means ----

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

## 2.8 Matrices for definition variable (i.e., the moderators) ----

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


## 2.9 Matrices of intercepts, factor loadings, and residual variances predicted by moderators ----

matT <- mxAlgebra(expression=matT0 + matB1*Age + matB2*Edu + matB3*Urb + matB4*Thr + matB5*Dep, 
                  name="matT")

matL <- mxAlgebra(expression=matL0 + matC1*Age + matC2*Edu + matC3*Urb + matC4*Thr + matC5*Dep, 
                  name="matL")

matE <- mxAlgebra(expression=matE0*exp(matD1*Age + matD2*Edu + matD3*Urb + matD4*Thr + matD5*Dep), 
                  name="matE")


## 2.10 Matrix for common factor means predicted by moderators ----

matA <- mxAlgebra(expression=matA0 + matG1*Age + matG2*Edu + matG3*Urb + matG4*Thr + matG5*Dep, 
                  name="matA")


## 2.11 Matrix for common factor variances ----

matVar <- mxAlgebra(expression=(matP0 * exp(matH1*Age + matH2*Edu + matH3*Urb + matH4*Thr + matH5*Dep)), 
                    name="matVar")

matIa <- mxMatrix(type="Diag", nrow=1, ncol=1, 
                  free=FALSE,
                  values=1, 
                  name="matIa")

matP <- mxAlgebra(expression = matIa * matVar,
                  name="matP")


## 2.12 Specify model-implied matrices ----

matM <- mxAlgebra(expression = matT + t(matL%*%matA),
                  name = "matM")

matC <- mxAlgebra(expression = matL %*% matP %*% t(matL) + matE, 
                  name="matC") 


## 2.13 Specify expectation and fit function ----

expF <- mxExpectationNormal(covariance="matC", 
                            means="matM",
                            dimnames=manVars)

fitF <- mxFitFunctionML() 
modFinal_rt <- mxModel(model="FinalRT", 
                       matT, matT0, matB1, matB2, matB3, matB4, matB5,
                       matL, matL0, matC1, matC2, matC3, matC4, matC5,
                       matE, matE0, matD1, matD2, matD3, matD4, matD5,
                       matP, matP0, matH1, matH2, matH3, matH4, matH5,
                       matA, matA0, matG1, matG2, matG3, matG4, matG5,
                       matIa, matV1, matV2, matV3, matV4, matV5,
                       matVar, matM, matC, 
                       CI, expF, fitF, mxdata)

fitFinal_rt <- mxRun(modFinal_rt, intervals = TRUE)
summary(fitFinal_rt)

## 2.14 Unpack results and compute p-values ----

TestFinal_rt <- summary(fitFinal_rt)$parameters %>% 
  as_tibble() %>% 
  filter(str_detect(matrix, "mat(B|C|G)")) %>% 
  mutate(
    type = case_when(
      str_detect(matrix, "mat(B|C)\\d$") ~ "dif",
      str_detect(matrix, "mat(G)\\d") ~ "impact",
      TRUE ~ 'other'
    )
  ) %>% 
  group_by(type) %>% 
  mutate(
    pvalue = 2 * (1-pnorm(abs(Estimate/Std.Error))),
    pvalue_adj = p.adjust(pvalue, method = "BH")
  ) %>% 
  # Add indicator and latent variable names to OpenMx parameters
  left_join(
    bind_rows(
      expand_grid(matrix = c("matB1", "matB2", "matB3", "matB4", "matB5"), row = 1, col = 1:nv) %>% mutate(indicator = rep(manVars, 5), factor = NA),
      expand_grid(matrix = c("matC1", "matC2", "matC3", "matC4", "matC5"), row = 1:nv) %>% mutate(col = rep(c(rep(1,10),rep(2,10),rep(3,10)), 5), indicator = rep(manVars, 5), factor = rep("rt", 10), 5),
      expand_grid(matrix = c("matG1", "matG2", "matG3", "matG4", "matG5"), col = 1, row = 1) %>% mutate(indicator = NA, factor = rep(c("v","a","t"), 5))
    ) 
  ) %>% 
  # Add moderator names
  mutate(
    mod = case_when(
      mod = str_detect(matrix, "B1|C1|G1") ~ "age",
      mod = str_detect(matrix, "B2|C2|G2") ~ "edu",
      mod = str_detect(matrix, "B3|C3|G3") ~ "urb",
      mod = str_detect(matrix, "B4|C4|G4") ~ "thr",
      mod = str_detect(matrix, "B5|C5|G5") ~ "dep",
    )
  )


save(modFinal_rt, fitFinal_rt, TestFinal_rt, file = "3_output/Results/MNLFA_rt_final.RData")

