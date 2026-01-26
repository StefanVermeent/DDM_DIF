
# 1. Load data ------------------------------------------------------------

load("3_output/Results/MNLFA_fit_v.RData")
load("3_output/Results/MNLFA_fit_a.RData")

load("1_data/3_AnalysisData/clean_data.RData")

data_clean <- data_clean |> 
  mutate(
    across(matches("(v|a|t)(1|2)$"), ~scale(.) |> as.numeric(), .names = "{.col}_l"),
    across(matches("urb_c|edu_c|age_c|child_dep|child_thr"), ~scale(.) |> as.numeric())
  )

# 2. Fit combined DDM MNLFA model -----------------------------------------

## 2.1 Create data objects ----
mxdata <- mxData(observed = data_clean, type = "raw")
manVars <- colnames(data_clean |> select(matches("(v)(1|2)_l"), matches("(a)(1|2)_l"),  matches("(t)(1|2)_l")))  
nv <- length(manVars)
manVars


## 2.2 Create a grid specifying parameters to be constrained/estimated for combinations of indicators, moderators, and model parameters ----

joined_grid <- bind_rows(
  # All (initially ) significant DIF paths for drift rate
  miTest_Apo_v |> 
    mutate(
      DDM_par = "v",
      DIF     = ifelse(pvalue < .10, TRUE, FALSE)) |> 
    bind_rows( # Add rows for anchors
      expand_grid(DDM_par = "v", item = c(anchors_v), mod = unique(fitApo_v$mod), par = unique(fitApo_v$par), DIF = FALSE)
    ) |> 
    arrange(item) |> 
    group_by(DDM_par, par, mod) |> 
    summarise(free = DIF |> as.matrix() |> list()),
  
  # All (initially ) significant DIF paths for drift rate
  miTest_Apo_a |> 
    mutate(
      DDM_par = "a",
      DIF     = ifelse(pvalue < .10, TRUE, FALSE)) |> 
    bind_rows( # Add rows for anchors
      expand_grid(DDM_par = "a", item = c(anchors_a), mod = unique(fitApo_a$mod), par = unique(fitApo_a$par), DIF = FALSE)
    ) |> 
    arrange(item) |> 
    group_by(DDM_par, par, mod) |> 
    summarise(free = DIF |> as.matrix() |> list()),
  
  # All (initially ) significant DIF paths for boundary separation
  miTest_Apo_t |> 
    mutate(
      DDM_par = "t",
      DIF     = ifelse(pvalue < .10, TRUE, FALSE)) |> 
    bind_rows( # Add rows for anchors
      expand_grid(DDM_par = "t", item = c(anchors_t), mod = unique(fitApo_t$mod), par = unique(fitApo_t$par), DIF = FALSE)
    ) |> 
    arrange(item) |> 
    group_by(DDM_par, par, mod) |> 
    summarise(free = DIF |> as.matrix() |> list())
) %>% 
  ungroup()

## 2.3 Matrices for indicator intercepts ----

matT0 <- mxMatrix(type="Full", nrow=1, ncol=nv,
                  free=TRUE,
                  values=1,
                  name="matT0")

matB1 <- mxMatrix(type="Full", nrow=1, ncol=nv,
                  free=c(
                    joined_grid |> filter(DDM_par == "v", mod == "age", par == "intercept") |> unnest(free) |> pull(free) |> t(),
                    joined_grid |> filter(DDM_par == "a", mod == "age", par == "intercept") |> unnest(free) |> pull(free) |> t(),
                    joined_grid |> filter(DDM_par == "t", mod == "age", par == "intercept") |> unnest(free) |> pull(free) |> t()
                  ),
                  values=0,
                  name="matB1")

matB2 <- mxMatrix(type="Full", nrow=1, ncol=nv,
                  free=c(
                    joined_grid |> filter(DDM_par == "v", mod == "edu", par == "intercept") |> unnest(free) |> pull(free) |> t(),
                    joined_grid |> filter(DDM_par == "a", mod == "edu", par == "intercept") |> unnest(free) |> pull(free) |> t(),
                    joined_grid |> filter(DDM_par == "t", mod == "edu", par == "intercept") |> unnest(free) |> pull(free) |> t()
                  ),
                  values=0,
                  name="matB2")

matB3 <- mxMatrix(type="Full", nrow=1, ncol=nv,
                  free=c(
                    joined_grid |> filter(DDM_par == "v", mod == "urb", par == "intercept") |> unnest(free) |> pull(free) |> t(),
                    joined_grid |> filter(DDM_par == "a", mod == "urb", par == "intercept") |> unnest(free) |> pull(free) |> t(),
                    joined_grid |> filter(DDM_par == "t", mod == "urb", par == "intercept") |> unnest(free) |> pull(free) |> t()
                  ),
                  values=0,
                  name="matB3")

matB4 <- mxMatrix(type="Full", nrow=1, ncol=nv,
                  free=c(
                    joined_grid |> filter(DDM_par == "v", mod == "thr", par == "intercept") |> unnest(free) |> pull(free) |> t(),
                    joined_grid |> filter(DDM_par == "a", mod == "thr", par == "intercept") |> unnest(free) |> pull(free) |> t(),
                    joined_grid |> filter(DDM_par == "t", mod == "thr", par == "intercept") |> unnest(free) |> pull(free) |> t()
                  ),
                  values=0,
                  name="matB4")

matB5 <- mxMatrix(type="Full", nrow=1, ncol=nv,
                  free=c(
                    joined_grid |> filter(DDM_par == "v", mod == "dep", par == "intercept") |> unnest(free) |> pull(free) |> t(),
                    joined_grid |> filter(DDM_par == "a", mod == "dep", par == "intercept") |> unnest(free) |> pull(free) |> t(),
                    joined_grid |> filter(DDM_par == "t", mod == "dep", par == "intercept") |> unnest(free) |> pull(free) |> t()
                  ),
                  values=0,
                  name="matB5")

## 2.4 Matrices for factor loadings of configural model ----

matL0 <- mxMatrix(type="Full", nrow=nv, ncol=3,
                  free=c(rep(c(TRUE, FALSE, FALSE), 10),rep(c(FALSE, TRUE, FALSE), 10), rep(c(FALSE, FALSE, TRUE), 10)),
                  values=c(rep(c(1, 0, 0), 10), rep(c(0, 1, 0), 10), rep(c(0, 0, 1), 10)),
                  byrow=TRUE, 
                  name="matL0")

matC1 <- mxMatrix(type="Full", nrow=nv, ncol=3,
                  free=c(
                    c(joined_grid |> filter(DDM_par == "v", mod == "age", par == "loading") |> unnest(free) |> pull(free), rep(FALSE,10), rep(FALSE,10)),
                    c(rep(FALSE,10), joined_grid |> filter(DDM_par == "a", mod == "age", par == "loading") |> unnest(free) |> pull(free), rep(FALSE,10)),
                    c(rep(FALSE,10), rep(FALSE,10), joined_grid |> filter(DDM_par == "t", mod == "age", par == "loading") |> unnest(free) |> pull(free))
                  ),
                  values=0,
                  name="matC1")

matC2 <- mxMatrix(type="Full", nrow=nv, ncol=3,
                  free=c(
                    c(joined_grid |> filter(DDM_par == "v", mod == "edu", par == "loading") |> unnest(free) |> pull(free), rep(FALSE,10), rep(FALSE,10)),
                    c(rep(FALSE,10), joined_grid |> filter(DDM_par == "a", mod == "edu", par == "loading") |> unnest(free) |> pull(free), rep(FALSE,10)),
                    c(rep(FALSE,10), rep(FALSE,10), joined_grid |> filter(DDM_par == "t", mod == "edu", par == "loading") |> unnest(free) |> pull(free))
                  ),
                  values=0,
                  name="matC2")

matC3 <- mxMatrix(type="Full", nrow=nv, ncol=3,
                  free=c(
                    c(joined_grid |> filter(DDM_par == "v", mod == "urb", par == "loading") |> unnest(free) |> pull(free), rep(FALSE,10), rep(FALSE,10)),
                    c(rep(FALSE,10), joined_grid |> filter(DDM_par == "a", mod == "urb", par == "loading") |> unnest(free) |> pull(free), rep(FALSE,10)),
                    c(rep(FALSE,10), rep(FALSE,10), joined_grid |> filter(DDM_par == "t", mod == "urb", par == "loading") |> unnest(free) |> pull(free))
                  ),
                  values=0,
                  name="matC3")

matC4 <- mxMatrix(type="Full", nrow=nv, ncol=3,
                  free=c(
                    c(joined_grid |> filter(DDM_par == "v", mod == "thr", par == "loading") |> unnest(free) |> pull(free), rep(FALSE,10), rep(FALSE,10)),
                    c(rep(FALSE,10), joined_grid |> filter(DDM_par == "a", mod == "thr", par == "loading") |> unnest(free) |> pull(free), rep(FALSE,10)),
                    c(rep(FALSE,10), rep(FALSE,10), joined_grid |> filter(DDM_par == "t", mod == "thr", par == "loading") |> unnest(free) |> pull(free))
                  ),
                  values=0,
                  name="matC4")

matC5 <- mxMatrix(type="Full", nrow=nv, ncol=3,
                  free=c(
                    c(joined_grid |> filter(DDM_par == "v", mod == "dep", par == "loading") |> unnest(free) |> pull(free), rep(FALSE,10), rep(FALSE,10)),
                    c(rep(FALSE,10), joined_grid |> filter(DDM_par == "a", mod == "dep", par == "loading") |> unnest(free) |> pull(free), rep(FALSE,10)),
                    c(rep(FALSE,10), rep(FALSE,10), joined_grid |> filter(DDM_par == "t", mod == "dep", par == "loading") |> unnest(free) |> pull(free))
                  ),
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

matP0 <- mxMatrix(type="Symm", nrow=3, ncol=3,
                  free=c(FALSE,TRUE,TRUE,TRUE,FALSE,TRUE,TRUE,TRUE,FALSE),
                  values=c(1,0,0,0,1,0,0,0,1),
                  name="matP0")

matH1 <- mxMatrix(type="Symm", nrow=3, ncol=3,
                  free=FALSE, # to identify the model
                  values=0,
                  name="matH1")

matH2 <- mxMatrix(type="Symm", nrow=3, ncol=3,
                  free=FALSE, # to identify the model
                  values=0,
                  name="matH2")

matH3 <- mxMatrix(type="Symm", nrow=3, ncol=3,
                  free=FALSE, # to identify the model
                  values=0,
                  name="matH3")

matH4 <- mxMatrix(type="Symm", nrow=3, ncol=3,
                  free=FALSE, # to identify the model
                  values=0,
                  name="matH4")

matH5 <- mxMatrix(type="Symm", nrow=3, ncol=3,
                  free=FALSE, # to identify the model
                  values=0,
                  name="matH5")


## 2.7 Matrices for common factor means ----

matA0 <- mxMatrix(type="Full", nrow=3, ncol=1,
                  free=FALSE,
                  values=0,
                  name="matA0")
matG1 <- mxMatrix(type="Full", nrow=3, ncol=1,
                  free=TRUE, 
                  values=0,
                  name="matG1")
matG2 <- mxMatrix(type="Full", nrow=3, ncol=1,
                  free=TRUE, 
                  values=0,
                  name="matG2")
matG3 <- mxMatrix(type="Full", nrow=3, ncol=1,
                  free=TRUE,
                  values=0,
                  name="matG3")
matG4 <- mxMatrix(type="Full", nrow=3, ncol=1,
                  free=TRUE,
                  values=0,
                  name="matG4")
matG5 <- mxMatrix(type="Full", nrow=3, ncol=1,
                  free=TRUE,
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

matIa <- mxMatrix(type="Diag", nrow=3, ncol=3, 
                  free=FALSE,
                  values=1, 
                  name="matIa")
matIb <- mxMatrix(type="Full", nrow=3, ncol=3, 
                  free=FALSE, 
                  values=c(0,1,1,1,0,1,1,1,0),
                  name="matIb")


## 2.12 Algebra for covariance matrix of factors (transformed to ensure positive definite matrices) ----

matVar <- mxAlgebra(expression =(matIa * matP0) * exp(matH1*Age + matH2*Edu + matH3*Urb + matH4*Thr + matH5*Dep),
                    name="matVar"
)


matR <- mxAlgebra(expression=(exp(2*(matP0 + matH1*Age + matH2*Edu + matH3*Urb + matH4*Thr + matH5*Dep))-1)/
                    (exp(2*(matP0 + matH1*Age + matH2*Edu + matH3*Urb + matH4*Thr + matH5*Dep))+1), 
                  name="matR")

matCov <- mxAlgebra(expression=(matIa * sqrt(matVar)) %*% matR %*% (matIa*sqrt(matVar)), 
                    name="matCov")

matP <- mxAlgebra(expression = matIa * matVar + matIb * matCov, 
                  name="matP")

## 2.13 Specify model-implied matrices ----

matC <- mxAlgebra(expression = matL %*% matP %*%t (matL) + matE, 
                  name="matC") 

matM <- mxAlgebra(expression = matT + t(matL %*% matA), 
                  name="matM") 

## 2.14 Specify expectation and fit function ----

expF <- mxExpectationNormal(covariance="matC", 
                            means="matM",
                            dimnames=manVars)

fitF <- mxFitFunctionML() 

modJoint_DDM <- mxModel(model="FinalDDM", 
                        matT, matT0, matB1, matB2, matB3, matB4, matB5,
                        matL, matL0, matC1, matC2, matC3, matC4, matC5,
                        matE, matE0, matD1, matD2, matD3, matD4, matD5,
                        matP, matP0, matH1, matH2, matH3, matH4, matH5,
                        matA, matA0, matG1, matG2, matG3, matG4, matG5,
                        matIa, matIb, matV1, matV2, matV3, matV4, matV5,
                        matVar, matR, matCov, matM, matC, 
                        expF, fitF, mxdata)

fitFinal_DDM <- mxRun(modJoint_DDM)
summary(fitJoint_DDM) 



# 3. Parse results --------------------------------------------------------

## 3.1 Compute adjusted p-values ----

TestFinal_DDM <- summary(fitFinal_DDM)$parameters %>% 
  as_tibble() %>% 
  filter(matrix %in% c("matB1", "matB2", "matB3", "matB4", "matB5", "matC1", "matC2", "matC3", "matC4", "matC5", "matG1", "matG2", "matG3", "matG4", "matG5")) %>% 
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
      expand_grid(matrix = c("matB1", "matB2", "matB3", "matB4", "matB5"), row = 1, col = 1:30) %>% mutate(indicator = rep(manVars, 5), factor = NA),
      expand_grid(matrix = c("matC1", "matC2", "matC3", "matC4", "matC5"), row = 1:30) %>% mutate(col = rep(c(rep(1,10),rep(2,10),rep(3,10)), 5), indicator = rep(manVars, 5), factor = rep(c(rep("v",10), rep("a",10), rep("t", 10)), 5)),
      expand_grid(matrix = c("matG1", "matG2", "matG3", "matG4", "matG5"), col = 1, row = 1:3) %>% mutate(indicator = NA, factor = rep(c("v","a","t"), 5))
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


## 3.2 Save results ----
save(modFinal_DDM, fitFinal_DDM, TestFinal_DDM, file = "3_output/Results/MNLFA_DDM_final.RData")

