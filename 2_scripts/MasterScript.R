


# Libraries ---------------------------------------------------------------

source("2_scripts/dependencies.R")

list.files("2_scripts/0_CustomFunctions", full.names = TRUE) |> 
  walk(source)

# 1. Preprocessing --------------------------------------------------------

source("2_scripts/1_ProcessingScripts/1_merge_parse.R")
source("2_scripts/1_ProcessingScripts/2_clean.R")
source("2_scripts/1_ProcessingScripts/3_fit_DDM_flanker.R")
source("2_scripts/1_ProcessingScripts/4_fit_DDM_simon.R")
source("2_scripts/1_ProcessingScripts/5_fit_DDM_colorshape.R")
source("2_scripts/1_ProcessingScripts/6_fit_DDM_globallocal.R")
source("2_scripts/1_ProcessingScripts/7_fit_DDM_animacysize.R")
source("2_scripts/1_ProcessingScripts/8_compute_variables.R")

# 2. Analyses
source("2_scripts/2_AnalysisScripts/1_determine_SEM_models.R")
source("2_scripts/2_AnalysisScripts/2_MNLFA_rt.R")
source("2_scripts/2_AnalysisScripts/2_MNLFA_v.R")
source("2_scripts/2_AnalysisScripts/2_MNLFA_a.R")
source("2_scripts/2_AnalysisScripts/2_MNLFA_t.R")
source("2_scripts/2_AnalysisScripts/2_MNLFA_final.R")

# 3. 