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

datalist <- prepare_ddm_list(data = globallocal_clean, id_name = "nomem_encr", n_conditions = 2)

parameters <- c("alpha", "tau", "delta", "muAlpha", "muTau", "muDelta")
nChains <- 3
nIterations <- 1000
yInit <- rep(NA, length(datalist$y))

# 4. Run model ------------------------------------------------------------

## 4.1 Fit ----
ddm_globallocal_fit <- run.jags(method = "parallel",
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

mcmc_globallocal <- as.matrix(as.mcmc.list(ddm_globallocal_fit), chains = F) |>
  as_tibble()

# Intermediate save in case the session crashes
save(mcmc_globallocal, ddm_globallocal_fit, file = "1_data/2_IntermediateData/DDM_globallocal_fit.RData")


# 5. Parse DDM estimates --------------------------------------------------
load("1_data/2_IntermediateData/DDM_globallocal_fit.RData")

globallocal_id_matches <- globallocal_clean |>
  distinct(nomem_encr) |>
  mutate(nomem_encr_num = 1:n())

## 5.1 Traces and parameter estimates ----
globallocal_traces    <- extract_traces_2con(mcmc_globallocal, chains = nChains, iterations = nIterations)
globallocal_param_est <- extract_ddm_estimates_2con(mcmc = mcmc_globallocal, task_prefix = "gl_", id_matches = globallocal_id_matches)

## 5.2 Simulation-based model fit assessment ----
globallocal_sim_fit <- globallocal_param_est %>%
  mutate(
    responses = pmap(., function(nomem_encr, gl_v1, gl_v2, gl_a1, gl_a2, gl_t1, gl_t2) {
      
      bind_rows(
        # Condition 1
        RWiener::rwiener(n=500, alpha=gl_a1, tau=gl_t1, beta=0.5, delta=gl_v1) |>
          as_tibble() |>
          mutate(
            condition = 1
          ),
        # Condition 2
        RWiener::rwiener(n=500, alpha=gl_a2, tau=gl_t2, beta=0.5, delta=gl_v2) |>
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
    globallocal_clean |>
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

save(mcmc_globallocal, ddm_globallocal_fit, globallocal_traces, globallocal_param_est, globallocal_sim_fit, file = "1_data/2_IntermediateData/DDM_globallocal_fit.RData")


# 6. Refit to obtain R-hat values -----------------------------------------

parameters <- c("muAlpha", "muTau", "muDelta")
nChains <- 3
nIterations <- 1000
yInit <- rep(NA, length(datalist$y))

ddm_globallocal_refit <- run.jags(method = "parallel",
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

rhat_globallocal <- ddm_globallocal_refit$psrf$psrf |>
  unlist() |>
  as_tibble(rownames = "parameter")


# 7. Save results ---------------------------------------------------------

load("1_data/2_IntermediateData/DDM_globallocal_fit.RData")

save(mcmc_globallocal, ddm_globallocal_fit, globallocal_traces, globallocal_param_est, globallocal_sim_fit, rhat_globallocal, file = "1_data/2_IntermediateData/DDM_globallocal_fit.RData")


# 8. Remove data from global environment ----------------------------------

rm(list = names(which(!unlist(eapply(.GlobalEnv, 
                                     \(x) inherits(x, what = "function"))))))