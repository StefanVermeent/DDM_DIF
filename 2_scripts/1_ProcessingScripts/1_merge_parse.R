# 1. Load raw data files -----------------------------------------------------

## 1.1 Questionnaire data ----
raw_Q_wave1 <- read_sav("1_data/1_InputData/L_CognitiveAdversity_1.0p.sav")
raw_Q_wave2 <- read_sav("1_data/1_InputData/L_CognitiveAdversity_2025_1.0p.sav") |> mutate(prev_participated = ifelse(nomem_encr %in% raw_Q_wave1$nomem_encr, 1, 0)) # re-compute; LISS-computed variable contains 39 errors
raw_Q_2023 <- read_sav("1_data/1_InputData/L_Cognitieve_Vaardigheden_v2_1.0p.sav") # Raw_2023 contains childhood  adversity measures for some participants in wave 1 and 2

## 1.2 Cognitive Task data ----

raw_T_wave1 <- read_delim("1_data/1_InputData/L_CognitiveAdversity_TASKDATA.csv", delim = ";")
raw_T_wave2 <- read_delim("1_data/1_InputData/L_CognitiveAdversity_2025_TaskData.csv", delim = ";")

## 1.3 Custom functions ----

# Required for wave 1 data only
parse_json_data <- function(data, x) {
  data <- data |>
    mutate(
      var = case_when(
        volgorde__1 == x ~ JSON1.4,
        volgorde__2 == x ~ JSON2.4,
        volgorde__3 == x ~ JSON3.4,
        volgorde__4 == x ~ JSON4.4,
        volgorde__5 == x ~ JSON5.4,
        volgorde__6 == x ~ JSON6.4
      )
    )
  return(data$var)
}

# 2. Merge data across waves -----------------------------------

## 2.1 Merge Questionnaire data ----
Qdata_raw <- raw_Q_wave2 |> 
  select(nomem_encr, nohouse_encr, prev_participated, age = leeftijd, urb = sted, edu = oplmet, oplzon) |> 
  filter(prev_participated ==  0) |> 
  mutate(wave = 2) |> 
  bind_rows(raw_Q_wave1 |> select(nomem_encr, nohouse_encr, age = leeftijd, urb = sted, edu = oplmet, oplzon) |> mutate(wave = 1)) |> 
  left_join(
    bind_rows(
      raw_Q_wave1 |> select(nomem_encr, starts_with("VP"), starts_with("PP")) |> filter(if_any(c(starts_with("VP"), starts_with("PP")), ~!is.na(.))),
      raw_Q_wave2 |> select(nomem_encr, starts_with("VP"), starts_with("PP")) |> filter(if_any(c(starts_with("VP"), starts_with("PP")), ~!is.na(.))),
      raw_Q_2023 |> select(nomem_encr, starts_with("VP"), starts_with("PP")) |> filter(if_any(c(starts_with("VP"), starts_with("PP")), ~!is.na(.))),
    )
  ) |> 
  filter(if_any(c(starts_with("VP"), starts_with("PP")), ~!is.na(.)))

## 2.2 Merge cognitive task data ----

Tdata_raw <- raw_T_wave2 |> 
  mutate(
    nomem_encr = str_remove(nomem_encr, "0$") |> as.numeric(),
    wave       = 2) |> 
  left_join(raw_Q_wave2 |> select(nomem_encr, prev_participated)) |> 
  filter(prev_participated == 0) %>%
  rename(
    simon_json       = TASK1.4,
    flanker_json     = TASK2.4,
    globallocal_json = TASK3.4,
    colorshape_json  = TASK4.4,
    animacysize_json = TASK5.4,
    posner_json      = TASK6.4
  ) |> 
  select(nomem_encr, wave, ends_with("json")) |> 
  bind_rows(
    raw_T_wave1 |>
      mutate(wave = 1) |> 
      select(nomem_encr, wave, JSON1.4, JSON2.4, JSON3.4, JSON4.4, JSON5.4, JSON6.4, starts_with("volgorde")) %>%
      mutate(
        simon_json        = parse_json_data(data = ., x = 1),
        flanker_json      = parse_json_data(data = ., x = 2),
        globallocal_json  = parse_json_data(data  = ., x = 3),
        colorshape_json   = parse_json_data(data = ., x = 4),
        animacysize_json  = parse_json_data(data = ., x = 5),
        posner_json       = parse_json_data(data = ., x = 6),
      ) |> 
      select(-starts_with("JSON"), -starts_with("volgorde"))
  )

# 3.  parse and clean task data -------------------------------------------

## 3.1 Flanker ----

flanker_raw <- Tdata_raw |>
  select(nomem_encr, wave, flanker_json) |>
  filter(!is.na(flanker_json)) |>
  filter(flanker_json != "[]", flanker_json != "\"I\"") |>
  mutate(across(c(matches("flanker_json")), ~str_replace_all(., "^\\\"", ""))) |>
  mutate(across(c(matches("flanker_json")), ~str_replace_all(., "\\\\", ""))) |>
  mutate(across(c(matches("flanker_json")), ~map_if(., .p =  ~!is.na(.x), .f = jsonlite::fromJSON))) |>
  unnest(flanker_json) |>
  mutate(
    rt        = rt / 1000,
    correct   = ifelse(correct == TRUE, 1, 0),
    condition = ifelse(str_detect(stimtype, "^congruent"), 1, 2)) |>
  filter(!variable %in% c("interblock", "test_start", "end")) |>
  select(nomem_encr, wave, rt, task, condition, correct)

## 3.2 Simon ----

simon_raw <- Tdata_raw |>
  select(nomem_encr, wave, simon_json) |>
  filter(!is.na(simon_json)) |>
  filter(simon_json != "[]", simon_json != "\"I\"") |>
  mutate(across(c(matches("simon_json")), ~str_replace_all(., "^\\\"", ""))) |>
  mutate(across(c(matches("simon_json")), ~str_replace_all(., "\\\\", ""))) |>
  mutate(across(c(matches("simon_json")), ~map_if(., .p =  ~!is.na(.x), .f = jsonlite::fromJSON))) |>
  unnest(simon_json) |>
  mutate(
    rt        = rt / 1000,
    correct   = ifelse(correct == TRUE, 1, 0),
    condition = ifelse(str_detect(stimtype, "^congruent"), 1, 2)) |>
  filter(!variable %in% c("interblock", "test_start", "end")) |>
  select(nomem_encr, wave, rt, task, condition, correct)

## 3.3 Color-shape ----

colorshape_raw <- Tdata_raw |>
  select(nomem_encr, wave, colorshape_json) |>
  filter(!is.na(colorshape_json)) |>
  filter(colorshape_json != "[]", colorshape_json != "\"I\"") |>
  mutate(across(c(matches("colorshape_json")), ~str_replace_all(., "^\\\"", ""))) |>
  mutate(across(c(matches("colorshape_json")), ~str_replace_all(., "\\\\", ""))) |>
  mutate(across(c(matches("colorshape_json")), ~map_if(., .p =  ~!is.na(.x), .f = jsonlite::fromJSON))) |>
  unnest(colorshape_json) |>
  filter(!variable %in% c("test_start", "colorshape_finish"), condition != "first") |>
  # Due to a coding error, the 'correct' variable logged by JsPsych contains errors in wave 1. Here, I manually fix those errors
  mutate(
    correct = case_when(
      wave == 1 & response == "a" & str_detect(stimulus, "yellow")   & rule == "color" ~ TRUE,
      wave == 1 & response == "a" & str_detect(stimulus, "triangle") & rule == "shape" ~ TRUE,
      wave == 1 & response == "l" & str_detect(stimulus, "blue")     & rule == "color" ~ TRUE,
      wave == 1 & response == "l" & str_detect(stimulus, "circle")   & rule == "shape" ~ TRUE,
      wave == 2 ~ correct,
      TRUE ~ FALSE
    )
  ) |>
  mutate(
    rt        = rt / 1000,
    correct   = ifelse(correct == TRUE, 1, 0),
    condition = ifelse(condition == "repeat", 1, 2)) |>
  select(nomem_encr, wave, rt, task, condition, rule, correct)

## 3.4 Global-Local ----

globallocal_raw <- Tdata_raw |>
  select(nomem_encr, wave, globallocal_json) |>
  filter(!is.na(globallocal_json)) |>
  filter(globallocal_json != "[]", globallocal_json != "\"I\"") |>
  mutate(across(c(matches("globallocal_json")), ~str_replace_all(., "^\\\"", ""))) |>
  mutate(across(c(matches("globallocal_json")), ~str_replace_all(., "\\\\", ""))) |>
  mutate(across(c(matches("globallocal_json")), ~map_if(., .p =  ~!is.na(.x), .f = jsonlite::fromJSON))) |>
  unnest(globallocal_json) |>
  filter(!variable %in% c("test_start", "fixation", "test_interblock", "end"), condition != "first") |>
  mutate(
    rt        = (rt / 1000) - 1, # -1 to account for the stimulus offset delay of 1s
    rt        = ifelse(rt < 0, 0, rt), # Set negative RTs to zero to prevent later issues with log-transformations
    correct   = ifelse(correct == TRUE, 1, 0),
    condition = ifelse(condition == "repeat", 1, 2)) |>
  select(nomem_encr, wave, rt, task, condition, rule, correct)

## 3.5 Animacy-Size ----

animacysize_raw <- Tdata_raw |>
  select(nomem_encr, wave, animacysize_json) |>
  filter(!is.na(animacysize_json)) |>
  filter(animacysize_json != "[]", animacysize_json != "\"I\"") |>
  mutate(across(c(matches("animacysize_json")), ~str_replace_all(., "^\\\"", ""))) |>
  mutate(across(c(matches("animacysize_json")), ~str_replace_all(., "\\\\", ""))) |>
  mutate(across(c(matches("animacysize_json")), ~map_if(., .p =  ~!is.na(.x), .f = jsonlite::fromJSON))) |>
  unnest(animacysize_json) |>
  filter(!variable %in% c("test_start", "interblock", "end"), condition != "first") |>
  mutate(
    rt        = rt / 1000,
    correct   = ifelse(correct == TRUE, 1, 0),
    condition = ifelse(condition == "repeat", 1, 2)) |>
  select(nomem_encr, wave, rt, task, condition, rule, correct)


# 4. Remove participants who did not complete all cognitive tasks ---------

all_unique_ids <- reduce(
  list(
    animacysize_raw |>
      select(nomem_encr, task) |> distinct(),
    globallocal_raw |>
      select(nomem_encr, task) |> distinct(),
    colorshape_raw |>
      select(nomem_encr, task) |> distinct(),
    flanker_raw |>
      select(nomem_encr, task) |> distinct(),
    simon_raw |>
      select(nomem_encr, task) |> distinct()
  ),
  full_join, by = c("nomem_encr")
)

all_incomplete_ids <- all_unique_ids |>
  filter(if_any(c(-nomem_encr), is.na)) |>
  pull(nomem_encr)

all_complete_ids <- all_unique_ids |>
  drop_na() |>
  pull(nomem_encr)

length(all_incomplete_ids) + length(all_complete_ids) == nrow(all_unique_ids)

# Remove participants who did not complete all cognitive tasks

flanker_raw     <- flanker_raw |> filter(nomem_encr %in% all_complete_ids)
simon_raw       <- simon_raw |> filter(nomem_encr %in% all_complete_ids)
colorshape_raw  <- colorshape_raw |> filter(nomem_encr %in% all_complete_ids)
animacysize_raw <- animacysize_raw |> filter(nomem_encr %in% all_complete_ids)
globallocal_raw <- globallocal_raw |> filter(nomem_encr %in% all_complete_ids)


# 5. Select one participant per household ---------------------------------

set.seed(34546)

sampled_participants <- Qdata_raw |> 
  filter(nomem_encr %in% all_complete_ids) |> 
  select(nomem_encr, nohouse_encr) |> 
  group_by(nohouse_encr) |> 
  slice_sample(n = 1) |> 
  pull(nomem_encr)

flanker_raw <- flanker_raw |> filter(nomem_encr %in% sampled_participants) 
simon_raw <- simon_raw |> filter(nomem_encr %in% sampled_participants) 
colorshape_raw <- colorshape_raw |> filter(nomem_encr %in% sampled_participants) 
globallocal_raw <- globallocal_raw |> filter(nomem_encr %in% sampled_participants) 
animacysize_raw <- animacysize_raw |> filter(nomem_encr %in% sampled_participants) 

Qdata_raw <- Qdata_raw |> filter(nomem_encr %in% sampled_participants)

# 5. Save data ------------------------------------------------------------

save(flanker_raw, simon_raw, colorshape_raw, animacysize_raw, globallocal_raw, file = "1_data/2_IntermediateData/Tdata_raw.RData")
save(Qdata_raw, file = "1_data/2_IntermediateData/Qdata_raw.RData")


# 6. Empty global environment ---------------------------------------------

rm(list = ls())