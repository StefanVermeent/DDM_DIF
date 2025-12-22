# 1. Load data and custom functions ---------------------------------------

load("1_data/2_IntermediateData/Tdata_long_clean.RData")

source("2_scripts/0_CustomFunctions/fit-ddm.R")
source("2_scripts/0_CustomFunctions/unpack-ddm.R")

# 2. Model ----------------------------------------------------------------

model_2con <- "model {
 #likelihood function
 for (t in 1:nTrials) {

  y[t] ~ dwiener(alpha[condition[t], subject[t]],
                 tau[condition[t], subject[t]],
                 0.5,
                 delta[condition[t], subject[t]])
 }

 for (s in 1:nSubjects) {
  for (c in 1:nCon) {
    tau[c, s] ~ dnorm(muTau[c], precTau) T(.0001, 1)
    alpha[c, s] ~ dnorm(muAlpha[c], precAlpha) T(.1, 5)
    delta[c, s] ~ dnorm(muDelta[c] , precDelta) T(-10, 10)
  }
 }

 #priors
 for (c in 1:nCon){
  muTau[c] ~ dunif(.0001, 1)
  muAlpha[c] ~ dunif(.1, 5)
  muDelta[c] ~ dunif(-10, 10)
 }

 precAlpha ~ dgamma(.001, .001)
 precTau ~ dgamma(.001, .001)
 precDelta ~ dgamma(.001, .001)
}"

initfunction_2con <- function(chain){
  return(list(
    muAlpha = runif(2, .2, 4.9),
    muTau = runif(2, .01, .05),
    muDelta = runif(2, -9.9, 9.9),
    precAlpha = runif(1, .01, 100),
    precTau = runif(1, .01, 100),
    precDelta = runif(1, .01, 100),
    y = yInit,
    .RNG.name = "lecuyer::RngStream",
    .RNG.seed = sample.int(1e10, 1, replace = F)))
}


# 3. Prepare model --------------------------------------------------------

datalist <- prepare_ddm_list(data = flanker_clean, id_name = "nomem_encr", n_conditions = 2)

parameters <- c("alpha", "tau", "delta", "muAlpha", "muTau", "muDelta")
nChains <- 3
nIterations <- 1000
yInit <- rep(NA, length(datalist$y))

# 4. Run model ------------------------------------------------------------

## 4.1 Fit ----
ddm_flanker_fit <- run.jags(method = "parallel",
                            model = model_2con,
                            monitor = parameters,
                            data = datalist,
                            inits = initfunction_2con,
                            n.chains = nChains,
                            adapt = 1000, 
                            burnin = 2000, 
                            sample = nIterations,
                            summarise = FALSE,
                            thin = 10, 
                            modules = c("wiener", "lecuyer"),
                            plots = F)

## 4.2 Extract results ----

mcmc_flanker <- as.matrix(as.mcmc.list(ddm_flanker_fit), chains = F) |>
  as_tibble()

# Intermediate save in case the session crashes
save(mcmc_flanker, ddm_flanker_fit, file = "1_data/2_IntermediateData/DDM_flanker_fit.RData")


# 5. Parse DDM estimates --------------------------------------------------
load("1_data/2_IntermediateData/DDM_flanker_fit.RData")

flanker_id_matches <- flanker_clean |>
  distinct(nomem_encr) |>
  mutate(nomem_encr_num = 1:n())

## 5.1 Traces and parameter estimates ----
flanker_traces    <- extract_traces_2con(mcmc_flanker, chains = nChains, iterations = nIterations)
flanker_param_est <- extract_ddm_estimates_2con(mcmc = mcmc_flanker, task_prefix = "fl_", id_matches = flanker_id_matches)

## 5.2 Simulation-based model fit assessment ----
flanker_sim_fit <- flanker_param_est %>%
  mutate(
    responses = pmap(., function(nomem_encr, fl_v1, fl_v2, fl_a1, fl_a2, fl_t1, fl_t2) {
      
      bind_rows(
        # Condition 1
        RWiener::rwiener(n=500, alpha=fl_a1, tau=fl_t1, beta=0.5, delta=fl_v1) |>
          as_tibble() |>
          mutate(
            condition = 1
          ),
        # Condition 2
        RWiener::rwiener(n=500, alpha=fl_a2, tau=fl_t2, beta=0.5, delta=fl_v2) |>
          as_tibble() |>
          mutate(
            condition = 2
          )
      )
    })
  ) |>
  unnest(responses) |>
  select(nomem_encr, condition, choice_sim = resp, rt = q) |>
  mutate(choice_sim = ifelse(choice_sim == 'upper', 1, 0)) |>
  group_by(nomem_encr, condition) |>
  summarise(
    RT_25 = quantile(rt, 0.25),
    RT_50 = quantile(rt, 0.50),
    RT_75 = quantile(rt, 0.75),
    acc_sim   = sum(choice_sim) / n()
  ) |>
  pivot_longer(starts_with("RT"), names_to = "percentile", values_to = "RT_sim") |>
  left_join(
    flanker_clean |>
      drop_na(rt) |>
      group_by(nomem_encr, condition) |>
      summarise(
        RT_25 = quantile(rt, 0.25),
        RT_50 = quantile(rt, 0.50),
        RT_75 = quantile(rt, 0.75),
        acc   = sum(correct, na.rm = T) / n()
      ) |>
      pivot_longer(starts_with("RT"), names_to = "percentile", values_to = "RT")
  )

save(mcmc_flanker, ddm_flanker_fit, flanker_traces, flanker_param_est, flanker_sim_fit, file = "1_data/2_IntermediateData/DDM_flanker_fit.RData")


# 6. Refit to obtain R-hat values -----------------------------------------

parameters <- c("muAlpha", "muTau", "muDelta")
nChains <- 3
nIterations <- 1000
yInit <- rep(NA, length(datalist$y))

ddm_flanker_refit <- run.jags(method = "parallel",
                              model = model_2con,
                              monitor = parameters,
                              data = datalist,
                              inits = initfunction_2con,
                              n.chains = nChains,
                              check.conv = TRUE,
                              psrf.target = 1.10,
                              adapt = 1000,
                              burnin = 2000,
                              sample = 1000,
                              summarise = TRUE,
                              thin = 10,
                              modules = c("wiener", "lecuyer"),
                              plots = F)

rhat_flanker <- ddm_flanker_refit$psrf$psrf |>
  unlist() |>
  as_tibble(rownames = "parameter")


# 7. Save results ---------------------------------------------------------

load("1_data/2_IntermediateData/DDM_flanker_fit.RData")

save(mcmc_flanker, ddm_flanker_fit, flanker_traces, flanker_param_est, flanker_sim_fit, rhat_flanker, file = "1_data/2_IntermediateData/DDM_flanker_fit.RData")


# 8. Clean global environment ---------------------------------------------

rm(list = ls())
