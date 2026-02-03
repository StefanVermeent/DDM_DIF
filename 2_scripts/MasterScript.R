# 1. Libraries ---------------------------------------------------------------

Sys.setenv(OMP_NUM_THREADS=parallel::detectCores()) #before library(OpenMx); enables parallel processing of MNLFA models

source("2_scripts/dependencies.R")

mxOption(key='Number of Threads', value=parallel::detectCores()) # Set number of cores used for parallel processing of MNLFA models

list.files("2_scripts/0_CustomFunctions", full.names = TRUE) |> 
  walk(source)


# 2. Preprocessing --------------------------------------------------------

source("2_scripts/1_ProcessingScripts/1_merge_parse.R")
source("2_scripts/1_ProcessingScripts/2_clean.R")
source("2_scripts/1_ProcessingScripts/3_fit_DDM_flanker.R")
source("2_scripts/1_ProcessingScripts/4_fit_DDM_simon.R")
source("2_scripts/1_ProcessingScripts/5_fit_DDM_colorshape.R")
source("2_scripts/1_ProcessingScripts/6_fit_DDM_globallocal.R")
source("2_scripts/1_ProcessingScripts/7_fit_DDM_animacysize.R")
source("2_scripts/1_ProcessingScripts/8_compute_variables.R")


# 3. Analyses -------------------------------------------------------------

source("2_scripts/2_AnalysisScripts/1_determine_SEM_models.R")
source("2_scripts/2_AnalysisScripts/2_MNLFA_rt.R")
source("2_scripts/2_AnalysisScripts/3_MNLFA_v.R")
source("2_scripts/2_AnalysisScripts/4_MNLFA_a.R")
source("2_scripts/2_AnalysisScripts/5_MNLFA_t.R")
source("2_scripts/2_AnalysisScripts/6_MNLFA_DDM_final.R")
source("2_scripts/2_AnalysisScripts/7_MNLFA_rt_final.R")

