### This script processes all data and results in a way that makes it easier to reference them in the manuscript ###


# 1. Load data ------------------------------------------------------------

load("3_output/Results/MNLFA_DDM_final.RData")
load("3_output/Results/MNLFA_rt_final.RData")
load("3_output/Results/MNLFA_fit_rt.RData")



# 2. Prepare DIF dataframes -----------------------------------------------

## 2.1 Extract all DIF effects ----

DIF <- TestFinal_DDM |> 
  filter(type == "dif") |> 
  mutate(model = "ddm") |> 
  bind_rows(
    TestFinal_rt |> 
      filter(type == "dif") |> 
      mutate(model = "rt")
  ) |> 
  ungroup() |> 
  select(model, matrix, mod, factor, indicator, Estimate, pvalue_adj) |> 
  mutate(
    factor = case_when(
      str_detect(indicator, "_v\\d_") ~ "v",
      str_detect(indicator, "_a\\d_") ~ "a",
      str_detect(indicator, "_t\\d_") ~ "t",
      TRUE ~ "rt"
    ),
    
    indicator = case_when(
      str_detect(indicator, "fl_.1|fl_rt_con") ~ "fl_con",
      str_detect(indicator, "fl_.2|fl_rt_inc") ~ "fl_inc",
      
      str_detect(indicator, "si_.1|si_rt_con") ~ "si_con",
      str_detect(indicator, "si_.2|si_rt_inc") ~ "si_inc",
      
      str_detect(indicator, "cs_.1|cs_rt_rep") ~ "cs_rep",
      str_detect(indicator, "cs_.2|cs_rt_sw")  ~ "cs_sw",
      
      str_detect(indicator, "gl_.1|gl_rt_rep") ~ "gl_rep",
      str_detect(indicator, "gl_.2|gl_rt_sw")  ~ "gl_sw",
      
      str_detect(indicator, "as_.1|as_rt_rep") ~ "as_rep",
      str_detect(indicator, "as_.2|as_rt_sw")  ~ "as_sw",
    )
  ) 

# The dataset does not contain parameter combinations that were contrained to zero.
# As we do want to plot these rows as constrained zero effects, this 'missing_effects' df adds these missing rows.
missing_effects <- expand_grid(
  mod = c("age", "edu", "urb", "thr", "dep"),
  factor = c("v", "a", "t"),
  indicator = c("as_sw", "as_rep",
                "gl_sw",  "gl_rep",
                "cs_sw",  "cs_rep",
                "si_inc", "si_con",
                "fl_inc", "fl_con"),
  model = c("ddm", "rt")
) |> 
  mutate(y = str_c(indicator, "_", model))

## 2.2 Intercept DIF data ----

DIF_intercepts <- missing_effects |> 
  left_join(
    DIF |> 
      filter(str_detect(matrix, "B")) |> 
      filter(model == "ddm") |> 
      rename(ddm_est_num = Estimate, pvalue_ddm_adj = pvalue_adj) |> 
      select(-c(model, matrix)) |> 
      full_join(
        DIF |> 
          filter(str_detect(matrix, "B")) |> 
          filter(model == "rt") |> 
          select(-factor) |> 
          mutate(factor = tibble(factor = c("v", "a", "t")) |> list()) |> 
          unnest(factor) |> 
          rename(rt_est_num = Estimate, pvalue_rt_adj = pvalue_adj) |> 
          select(-c(model, matrix))
      ) |> 
      mutate(
        ddm_est_chr = case_when(
          !is.na(ddm_est_num) & pvalue_ddm_adj < .05 & pvalue_ddm_adj >= .01  ~ str_c(formatC(ddm_est_num, digits = 2, format = "f"), "*"),
          !is.na(ddm_est_num) & pvalue_ddm_adj < .01 & pvalue_ddm_adj >= .001 ~ str_c(formatC(ddm_est_num, digits = 2, format = "f"), "**"),
          !is.na(ddm_est_num) & pvalue_ddm_adj < .001                         ~ str_c(formatC(ddm_est_num, digits = 2, format = "f"), "***"),
          !is.na(ddm_est_num) & pvalue_ddm_adj > .05                          ~ formatC(ddm_est_num, digits = 2, format = "f"),
          is.na(ddm_est_num)                                                  ~ "†"
        ),
        rt_est_chr = case_when(
          !is.na(rt_est_num) & pvalue_rt_adj < .05 & pvalue_rt_adj >= .01  ~ str_c(formatC(rt_est_num, digits = 2, format = "f"), "*"),
          !is.na(rt_est_num) & pvalue_rt_adj < .01 & pvalue_rt_adj >= .001 ~ str_c(formatC(rt_est_num, digits = 2, format = "f"), "**"),
          !is.na(rt_est_num) & pvalue_rt_adj < .001                        ~ str_c(formatC(rt_est_num, digits = 2, format = "f"), "***"),
          !is.na(rt_est_num) & pvalue_rt_adj > .05                         ~ formatC(rt_est_num, digits = 2, format = "f"),
          is.na(rt_est_num)                                                ~ "†"
        )
      ) |> 
      select(-c(pvalue_ddm_adj, pvalue_rt_adj)) |> 
      pivot_longer(
        cols = c(ddm_est_num, rt_est_num, ddm_est_chr, rt_est_chr),
        names_to = c("model", ".value"),
        names_pattern = "(ddm|rt)_est_(num|chr)"
      ) |> 
      rename(estimate_num = num, estimate_chr = chr) |> 
      mutate(y = str_c(indicator, "_", model))
  ) |> 
  mutate(
    y = factor(y, levels = c("as_sw_ddm",  "as_sw_rt",
                             "as_rep_ddm", "as_rep_rt",
                             "gl_sw_ddm",  "gl_sw_rt",
                             "gl_rep_ddm", "gl_rep_rt",
                             "cs_sw_ddm",  "cs_sw_rt",
                             "cs_rep_ddm", "cs_rep_rt",
                             "si_inc_ddm", "si_inc_rt",
                             "si_con_ddm", "si_con_rt",
                             "fl_inc_ddm", "fl_inc_rt",
                             "fl_con_ddm", "fl_con_rt"
    )),
    estimate_chr  = ifelse(is.na(estimate_chr), "†", estimate_chr),
    estimate_chr  = ifelse(model == "rt" & factor %in% c("v", "t"), "", estimate_chr),
    factor    = factor(factor, levels = c("v", "a", "t"), labels = c("Drift rate", "Boundary sep.", "Non-dec. time")),
    mod       = factor(mod, levels = c("age", "edu", "urb", "thr", "dep"), labels = c("Age", "Education", "Urbanicity", "Threat", "Deprivation")),
    txt_color = ifelse(str_detect(estimate_chr, "\\*"), "sig", "nonsig")
  )




## 2.3 Loading DIF data ----

DIF_loadings <- missing_effects |> 
  left_join(
    DIF |> 
      filter(str_detect(matrix, "C")) |> 
      filter(model == "ddm") |> 
      rename(ddm_est_num = Estimate, pvalue_ddm_adj = pvalue_adj) |> 
      select(-c(model, matrix)) |> 
      full_join(
        DIF |> 
          filter(str_detect(matrix, "C")) |> 
          filter(model == "rt") |> 
          select(-factor) |> 
          mutate(factor = tibble(factor = c("v", "a", "t")) |> list()) |> 
          unnest(factor) |> 
          rename(rt_est_num = Estimate, pvalue_rt_adj = pvalue_adj) |> 
          select(-c(model, matrix))
      ) |> 
      mutate(
        ddm_est_chr = case_when(
          !is.na(ddm_est_num) & pvalue_ddm_adj < .05 & pvalue_ddm_adj >= .01  ~ str_c(formatC(ddm_est_num, digits = 2, format = "f"), "*"),
          !is.na(ddm_est_num) & pvalue_ddm_adj < .01 & pvalue_ddm_adj >= .001 ~ str_c(formatC(ddm_est_num, digits = 2, format = "f"), "**"),
          !is.na(ddm_est_num) & pvalue_ddm_adj < .001                         ~ str_c(formatC(ddm_est_num, digits = 2, format = "f"), "***"),
          !is.na(ddm_est_num) & pvalue_ddm_adj > .05                          ~ formatC(ddm_est_num, digits = 2, format = "f"),
          is.na(ddm_est_num)                                                  ~ "†"
        ),
        rt_est_chr = case_when(
          !is.na(rt_est_num) & pvalue_rt_adj < .05 & pvalue_rt_adj >= .01  ~ str_c(formatC(rt_est_num, digits = 2, format = "f"), "*"),
          !is.na(rt_est_num) & pvalue_rt_adj < .01 & pvalue_rt_adj >= .001 ~ str_c(formatC(rt_est_num, digits = 2, format = "f"), "**"),
          !is.na(rt_est_num) & pvalue_rt_adj < .001                        ~ str_c(formatC(rt_est_num, digits = 2, format = "f"), "***"),
          !is.na(rt_est_num) & pvalue_rt_adj > .05                         ~ formatC(rt_est_num, digits = 2, format = "f"),
          is.na(rt_est_num)                                                ~ "†"
        )
      ) |> 
      select(-c(pvalue_ddm_adj, pvalue_rt_adj)) |> 
      pivot_longer(
        cols = c(ddm_est_num, rt_est_num, ddm_est_chr, rt_est_chr),
        names_to = c("model", ".value"),
        names_pattern = "(ddm|rt)_est_(num|chr)"
      ) |> 
      rename(estimate_num = num, estimate_chr = chr) |> 
      mutate(y = str_c(indicator, "_", model))
  ) |> 
  mutate(
    y = factor(y, levels = c("as_sw_ddm",  "as_sw_rt",
                             "as_rep_ddm", "as_rep_rt",
                             "gl_sw_ddm",  "gl_sw_rt",
                             "gl_rep_ddm", "gl_rep_rt",
                             "cs_sw_ddm",  "cs_sw_rt",
                             "cs_rep_ddm", "cs_rep_rt",
                             "si_inc_ddm", "si_inc_rt",
                             "si_con_ddm", "si_con_rt",
                             "fl_inc_ddm", "fl_inc_rt",
                             "fl_con_ddm", "fl_con_rt"
    )),
    estimate_chr  = ifelse(is.na(estimate_chr), "†", estimate_chr),
    estimate_chr  = ifelse(model == "rt" & factor %in% c("v", "t"), "", estimate_chr),
    factor    = factor(factor, levels = c("v", "a", "t"), labels = c("Drift rate", "Boundary sep.", "Non-dec. time")),
    mod       = factor(mod, levels = c("age", "edu", "urb", "thr", "dep"), labels = c("Age", "Education", "Urbanicity", "Threat", "Deprivation")),
    txt_color = ifelse(str_detect(estimate_chr, "\\*"), "sig", "nonsig")
  )


# 3. Generate DIF Figures -------------------------------------------------

# Dataframes for adding boxes/lines to the plots
vlines <- tibble(
  x = rep(c(1.5, 2.5), 10),  
  ymin = rep(seq(0.5, 19.5, by = 2), each = 2),                        
  ymax = rep(seq(1.5, 20.5, by = 2), each = 2)                           
)

hlines <- tibble(
  x = 0.5,
  xend = 3.5,
  y = seq(1.5, 20.5, 1)
)

boxlines <- tibble(xmin = 0.5, xmax = 3.5, ymin = seq(0.5, 20.5, 2), ymax = seq(2.5, 22.5, 2))

## 3.1 Intercept DIF plots ----

plot_intercept_DIF <- DIF_intercepts |> 
  ggplot() + 
  geom_tile(aes(x = factor, y = as.numeric(y), fill = estimate_num)) +
  geom_text(aes(x = factor, y = as.numeric(y), label = estimate_chr, color = txt_color), show.legend = FALSE) +
  geom_rect(data = boxlines, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = 2.5), fill = NA, color =  "black", linewidth = 2) +
  geom_segment(data = vlines, aes(x=x, xend=x, y=ymin, yend=ymax)) +
  geom_segment(data = hlines, aes(x=x, xend = xend, y = y))  +
  facet_wrap(~mod, ncol = 5) +
  scale_y_continuous(
    breaks = seq(1.5, 19.5, by = 2), 
    labels = c("Animacy-size\nSwitch", "Animacy-size\nRepeat",
               "Global-local\nSwitch", "Global-local\nRepeat",
               "Color-shape\nSwitch", "Color-shape\nRepeat",
               "Simon\nIncongruent", "Simon\nCongruent",
               "Flanker\nIncongruent", "Flanker\nCongruent"
    )) +
  scale_fill_gradient2(
    na.value = "white",
    low  = "#a6611a",
    mid  = "white",
    high = "#018571",
    limits = c(-0.15, 0.15),
    breaks = seq(-0.15, 0.15, 0.15),
    oob = scales::squish
  ) +
  scale_color_manual(values = c(sig = "black", nonsig = "darkgrey")) +
  coord_cartesian(ylim  = c(1.35, 19.65)) +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 12),
    strip.text = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 11),
    legend.title = element_blank(),
    legend.position = "bottom"
  )


## 3.2 Loading DIF plots ----

plot_loading_DIF <- DIF_loadings |> 
  ggplot() + 
  geom_tile(aes(x = factor, y = as.numeric(y), fill = estimate_num)) +
  geom_text(aes(x = factor, y = as.numeric(y), label = estimate_chr, color = txt_color), show.legend = FALSE) +
  geom_rect(data = boxlines, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = 2.5), fill = NA, color =  "black", linewidth = 2) +
  geom_segment(data = vlines, aes(x=x, xend=x, y=ymin, yend=ymax)) +
  geom_segment(data = hlines, aes(x=x, xend = xend, y = y))  +
  facet_wrap(~mod, ncol = 5) +
  scale_y_continuous(
    breaks = seq(1.5, 19.5, by = 2), 
    labels = c("Animacy-size\nSwitch", "Animacy-size\nRepeat",
               "Global-local\nSwitch", "Global-local\nRepeat",
               "Color-shape\nSwitch", "Color-shape\nRepeat",
               "Simon\nIncongruent", "Simon\nCongruent",
               "Flanker\nIncongruent", "Flanker\nCongruent"
    )) +
  scale_fill_gradient2(
    na.value = "white",
    low  = "#a6611a",
    mid  = "white",
    high = "#018571",
    limits = c(-0.15, 0.15),
    breaks = seq(-0.15, 0.15, 0.15),
    oob = scales::squish
  ) +
  scale_color_manual(values = c(sig = "black", nonsig = "darkgrey")) +
  coord_cartesian(ylim  = c(1.35, 19.65)) +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 12),
    strip.text = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 11),
    legend.title = element_blank(),
    legend.position = "bottom"
  )


# 4. Gather in-text DIF statistics ----------------------------------------

## 4.1 Intercept DIF statistics for RT model ----

DIF_int_rt_stats <- DIF |> 
  filter(str_detect(matrix, "B")) |> 
  filter(model == "rt") |> 
  mutate(
    identifier = str_c(mod, "_", indicator),
    Estimate   = formatC(Estimate, digits = 2, format = "f"),
  ) |> 
  rowwise() |> 
  mutate(pvalue_adj = format_p(pvalue_adj)) |> 
  ungroup() |> 
  select(-c(model, matrix, factor, mod, indicator)) |> 
  nest(data = -identifier) |> 
  deframe()


## 4.2 Intercept DIF statistics for DDM model ----

DIF_int_ddm_stats <- DIF |> 
  filter(str_detect(matrix, "B")) |> 
  filter(model == "ddm") |> 
  mutate(
    identifier = str_c(mod, "_", factor, "_", indicator),
    Estimate   = formatC(Estimate, digits = 2, format = "f"),
  ) |> 
  rowwise() |> 
  mutate(pvalue_adj = format_p(pvalue_adj)) |> 
  ungroup() |> 
  select(-c(model, matrix, factor, mod, indicator)) |> 
  nest(data = -identifier) |> 
  deframe()

## 4.3 Loading DIF statistics for rt model ----

DIF_load_rt_stats <- DIF |> 
  filter(str_detect(matrix, "C")) |> 
  filter(model == "rt") |> 
  mutate(
    identifier = str_c(mod, "_", factor, "_", indicator),
    Estimate   = formatC(Estimate, digits = 2, format = "f"),
  ) |> 
  rowwise() |> 
  mutate(pvalue_adj = format_p(pvalue_adj)) |> 
  ungroup() |> 
  select(-c(model, matrix, factor, mod, indicator)) |> 
  nest(data = -identifier) |> 
  deframe()


## 4.4 Loading DIF statistics for rt model ----

DIF_load_ddm_stats <- DIF |> 
  filter(str_detect(matrix, "C")) |> 
  filter(model == "ddm") |> 
  mutate(
    identifier = str_c(mod, "_", factor, "_", indicator),
    Estimate   = formatC(Estimate, digits = 2, format = "f"),
  ) |> 
  rowwise() |> 
  mutate(pvalue_adj = format_p(pvalue_adj)) |> 
  ungroup() |> 
  select(-c(model, matrix, factor, mod, indicator)) |> 
  nest(data = -identifier) |> 
  deframe()


# 5. Prepare impact dataframe ---------------------------------------------

## 5.1 Prepare data for RT Impact plotting ----

impact_rt <- bind_rows(
  # Mean impact of moderators in scalar (fully constrained) model
  summary(fitScalar_rt)$CI |> 
    as_tibble(rownames = "name") |> 
    mutate(
      mod = case_when(
        str_detect(name, "matG1") ~ "age",
        str_detect(name, "matG2") ~ "edu",
        str_detect(name, "matG3") ~ "urb",
        str_detect(name, "matG4") ~ "thr",
        str_detect(name, "matG5") ~ "dep",
      ),
      dv = "rt",
      model = "scalar"
    ) |> 
    # Add standard errors
    left_join(
      summary(fitScalar_rt)$parameters |> 
        as_tibble() |> 
        select(matrix, name, Std.Error) |> 
        filter(str_detect(name, "matG"))
    ) |> 
    select(-c(matrix, note)),
  # Mean impact of moderators in final (partial DIF) model
  summary(fitFinal_rt)$CI |> 
    as_tibble(rownames = "name") |> 
    mutate(
      mod = case_when(
        str_detect(name, "matG1") ~ "age",
        str_detect(name, "matG2") ~ "edu",
        str_detect(name, "matG3") ~ "urb",
        str_detect(name, "matG4") ~ "thr",
        str_detect(name, "matG5") ~ "dep",
      ),
      dv = "rt",
      model = "final"
    ) |> 
    # Add standard errors
    left_join(
      summary(fitFinal_rt)$parameters |> 
        as_tibble() |> 
        select(matrix, name, Std.Error) |> 
        filter(str_detect(name, "matG"))
    ) |> 
    select(-c(matrix, note))
) 


## 5.2 Prepare data for DDM impact plotting ----

impact_ddm <- bind_rows(
  # Mean impact of moderators  in scalar (fully constrained) model
  summary(fitScalar_DDM)$CI |> 
    as_tibble(rownames = "name") |> 
    mutate(
      mod = case_when(
        str_detect(name, "matG1") ~ "age",
        str_detect(name, "matG2") ~ "edu",
        str_detect(name, "matG3") ~ "urb",
        str_detect(name, "matG4") ~ "thr",
        str_detect(name, "matG5") ~ "dep",
      ),
      dv = case_when(
        str_detect(name, "matG\\d\\[1") ~ "v",
        str_detect(name, "matG\\d\\[2") ~ "a",
        str_detect(name, "matG\\d\\[3") ~ "t",
      ),
      model = "scalar"
    ) |> 
    # Add standard errors
    left_join(
      summary(fitScalar_DDM)$parameters |> 
        as_tibble() |> 
        select(matrix, name, Std.Error) |> 
        filter(str_detect(name, "matG"))
    ) |> 
    select(-c(name, note)),
  # Mean impact of moderators in final (partial DIF) model
  summary(fitFinal_DDM)$CI |> 
    as_tibble(rownames = "name") |> 
    mutate(
      mod = case_when(
        str_detect(name, "matG1") ~ "age",
        str_detect(name, "matG2") ~ "edu",
        str_detect(name, "matG3") ~ "urb",
        str_detect(name, "matG4") ~ "thr",
        str_detect(name, "matG5") ~ "dep",
      ),
      dv = case_when(
        str_detect(name, "matG\\d\\[1") ~ "v",
        str_detect(name, "matG\\d\\[2") ~ "a",
        str_detect(name, "matG\\d\\[3") ~ "t",
      ),
      model = "final"
    ) |>
    # Add standard errors
    left_join(
      summary(fitFinal_DDM)$parameters |> 
        as_tibble() |> 
        select(matrix, name, Std.Error) |> 
        filter(str_detect(name, "matG"))
    ) |> 
    select(-c(name, note))
)


## 5.3 Combine RT and DDM tibbles ----

impact_df <- bind_rows(impact_ddm, impact_rt) |> 
  mutate(
    dv = factor(dv,
                levels = c("rt", "v", "a", "t"),
                labels = c("Response time", "Drift rate", "Boundary separation", "Non-decision time")),
    mod = factor(mod, 
                 levels = c("dep", "thr", "urb", "edu", "age"), 
                 labels = c("Deprivation", "Threat", "Urbanicity", "Education", "Age")),
    model = factor(model,
                   levels = c("final", "scalar"),
                   labels = c("DIF-adjusted", "Unadjusted")),
    measure = ifelse(dv == "Response time", "rt", "ddm")
  ) |> 
  mutate(
    sig = case_when(
      lbound < 0 & ubound < 0 ~ "sig",
      lbound > 0 & ubound > 0 ~ "sig",
      lbound < 0 & ubound > 0 ~ "nonsig",
    )
  ) |> 
  # Compute pvalues for impacts
  mutate(pval_grouping = str_c(mod, "_", dv)) |> 
  nest_by(pval_grouping) |> 
  mutate(
    z = (data$estimate[[1]] - data$estimate[[2]]) /
      sqrt(data$`Std.Error`[[1]]^2 + data$`Std.Error`[[2]]^2),
    pvalue_modcompare = 2 * (1 - pnorm(abs(z)))
  ) |> 
  unnest(data) |> 
  ungroup() |> 
  mutate(pvalue_null = 2 * (1 - pnorm(abs(estimate / Std.Error)))) |> 
  select(-pval_grouping)


# 6. Plot impact ----------------------------------------------------------

plot_impact <- impact_df |> 
  ggplot(aes(mod, estimate, color = model)) +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, fill = measure), color = "black", alpha = 0.08, show.legend = FALSE) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 1) +
  geom_errorbar(aes(ymin = lbound, ymax = ubound), position = position_dodge(0.6), width = 0.3, size = 1) +
  geom_point(aes(fill = measure), size = 3, position = position_dodge(0.6)) +
  scale_color_manual(values = c("#e08214", "#542788")) +
  scale_fill_manual(values = c(rt = "#fee0b6", ddm = "#d8daeb")) +
  facet_wrap(~dv, ncol = 1) +
  theme_minimal() +
  coord_flip(ylim = c(-0.30, 0.70)) +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    axis.title.y = element_blank(),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    axis.title.x = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.title = element_blank()
  ) +
  guides(fill = "none") +
  labs(
    y = "\nMean impact (standardized)"
  )



# 7. Gather in-text impact statistics -------------------------------------

impact_stats <- impact_df |> 
  mutate(
    dv = case_when(
      dv == "Drift rate"          ~ "v",
      dv == "Boundary separation" ~ "a",
      dv == "Non-decision time"   ~ "t",
      dv == "Response time"       ~ "rt"
    ),
    model = ifelse(model == "Unadjusted", "unadjusted", "adjusted"),
    identifier = str_c(str_to_lower(mod), "_", dv, "_", model),
    estimate = formatC(estimate, digits = 2, format = "f"),
  ) |> 
  rowwise() |> 
  mutate(
    pvalue_null = format_p(pvalue_null),
    pvalue_modcompare = format_p(pvalue_modcompare)
    ) |> 
  select(identifier, estimate, lbound, ubound, Std.Error, pvalue_modcompare, pvalue_null) |> 
  nest(data = -identifier) |> 
  deframe()



# 8. Save objects ---------------------------------------------------------

save(plot_intercept_DIF, plot_loading_DIF, plot_impact, file = "3_output/Results/3_plots/MNLFA_figures.RData")
save(DIF_int_rt_stats, DIF_int_ddm_stats, DIF_load_rt_stats, DIF_load_ddm_stats, impact_stats, file = "3_output/Results/4_staging/intext_stats.RData")
