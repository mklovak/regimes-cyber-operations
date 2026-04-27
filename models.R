##################### models.R #################################################
# Political Regimes and State-Level Cyber Aggression
#
# Hypotheses (revised):
#   H0: Winning coalition index does not affect state-backed cyber operations.
#   H1: The larger the winning coalition index, the lower the frequency of
#       conducting unprovoked state-backed cyber operations.
#       Operationalization: read attacker_w4 across M1-M7.
#   H2: The larger the winning coalition index, the higher the frequency of
#       being targeted by unprovoked state-backed cyber operations conducted
#       by other states.
#       Operationalization: read victim_w4 across M1-M7.
#
# Each NB and ZINB model jointly tests H1 and H2.
#
# Structure:
#   Section 0: Data loading & preparation
#   Section 1: Panel A — M1            (broadest panel, GDP only)
#   Section 2: Panel B — M2, M3, M4    (full controls + ZINB + MID-augmented)
#   Section 3: Panel C — M5, M6, M7    (clean DV; M7 is PRIMARY)
#   Section 4: Summary table (M1-M7)
#
# Robustness checks (no-FE versions, logits, MID-any sensitivity, ZINB/NB
# diagnostics, etc.) live in robustness.R.
# Poisson vs NB likelihood-ratio test moved to descriptive_statistics.R.
################################################################################

rm(list = ls())
options(scipen = 999)
cat("\014")

library(tidyverse)
library(readxl)
library(fixest) # NB with year FE and dyad-clustered SEs
library(pscl) # ZINB via zeroinfl()
library(modelsummary) # HTML table export

# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


################################################################################
#                 SECTION 0: DATA LOADING & PREPARATION
################################################################################

cat("############################################################\n")
cat("#  SECTION 0: DATA LOADING                                 #\n")
cat("############################################################\n\n")

# --- 0.1 Load the three panels ---
df_2020 <- read_csv("outputs/df_model_2020.csv", show_col_types = FALSE) %>%
  mutate(directed_dyad_id = paste(attacker, victim, sep = "_"))

df_2016 <- read_csv("outputs/df_model_2016.csv", show_col_types = FALSE) %>%
  mutate(directed_dyad_id = paste(attacker, victim, sep = "_"))

df_2014 <- read_csv("outputs/df_model_2014.csv", show_col_types = FALSE) %>%
  mutate(directed_dyad_id = paste(attacker, victim, sep = "_"))

# --- 0.2 Build MID use-of-force dummy (hostility >= 4) for M4 ---
# Only the >=4 threshold enters primary models; MID-any sensitivity is in
# robustness.R. MID dyadic v4.03 covers through 2014, so 2015-2016 rows get
# NA and drop from M4 automatically.

df_mid <- read_csv("data sources/dyadic_mid_4.03.csv", show_col_types = FALSE)

cow_name_to_code <- c(
  "United States of America" = 2, "Russia" = 365, "China" = 710,
  "Iran" = 630, "North Korea" = 731, "South Korea" = 732,
  "India" = 750, "Pakistan" = 770, "Israel" = 666, "Ukraine" = 369,
  "Georgia" = 372, "Turkey" = 640, "Japan" = 740, "Taiwan" = 713,
  "Vietnam" = 816, "Philippines" = 840, "Estonia" = 366,
  "Lithuania" = 368, "Syria" = 652, "Saudi Arabia" = 670,
  "Lebanon" = 660, "United Kingdom" = 200, "Germany" = 255,
  "France" = 220, "Poland" = 290, "Canada" = 20
)

mid_force_raw <- df_mid %>%
  filter(year >= 2007, year <= 2014, hihost >= 4) %>%
  dplyr::select(statea, stateb, year) %>%
  distinct()

mid_force_names <- mid_force_raw %>%
  mutate(
    att1 = names(cow_name_to_code)[match(statea, cow_name_to_code)],
    vic1 = names(cow_name_to_code)[match(stateb, cow_name_to_code)],
    att2 = names(cow_name_to_code)[match(stateb, cow_name_to_code)],
    vic2 = names(cow_name_to_code)[match(statea, cow_name_to_code)]
  )

mid_force_directed <- bind_rows(
  mid_force_names %>%
    filter(!is.na(att1) & !is.na(vic1)) %>%
    dplyr::select(attacker = att1, victim = vic1, Year = year),
  mid_force_names %>%
    filter(!is.na(att2) & !is.na(vic2)) %>%
    dplyr::select(attacker = att2, victim = vic2, Year = year)
) %>%
  distinct() %>%
  mutate(mid_force = 1L)

df_2016 <- df_2016 %>%
  left_join(mid_force_directed, by = c("attacker", "victim", "Year")) %>%
  mutate(
    mid_force = case_when(
      Year <= 2014 & !is.na(mid_force) ~ 1L,
      Year <= 2014 & is.na(mid_force) ~ 0L,
      Year > 2014 ~ NA_integer_
    )
  )

# --- 0.3 Summary ---
cat("--- Panel A: df_model_2020 ---\n")
cat(sprintf(
  "  Obs: %d | Incidents: %d | Years: 2007-2020 | Controls: W4 + GDP + Year FE\n",
  nrow(df_2020), sum(df_2020$Incident_Count)
))

cat("--- Panel B: df_model_2016 ---\n")
cat(sprintf(
  "  Obs: %d | Incidents: %d | Years: 2007-2016 | Controls: W4 + CINC + GDP + Year FE\n",
  nrow(df_2016), sum(df_2016$Incident_Count)
))
cat(sprintf(
  "  mid_force (hostility >= 4): %d dyad-years (2007-2014)\n",
  sum(df_2016$mid_force, na.rm = TRUE)
))
cat(sprintf(
  "  MID NA (2015-2016): %d obs → drop from M4\n",
  sum(is.na(df_2016$mid_force))
))

cat("--- Panel C: df_model_2014 (PRIMARY) ---\n")
cat(sprintf(
  "  Obs: %d | Incidents: %d | Years: 2007-2014 | Controls: W4 + CINC + GDP + Year FE\n",
  nrow(df_2014), sum(df_2014$Incident_Count_Clean)
))
cat("  DV: Incident_Count_Clean (7 ACD + 40 MID-linked excluded, 122 kinetic dyad-years dropped)\n\n")


################################################################################
#                 SECTION 1: PANEL A — M1
#                 (W4 + GDP + Year FE, 2007-2020)
################################################################################
# Broadest panel, minimal controls. GDP proxies for both capability and
# development. Establishes the raw W4-cyber relationship under temporal FE.

cat("############################################################\n")
cat("#  SECTION 1: PANEL A — M1                                 #\n")
cat("############################################################\n\n")


# --- M1: NB + Year FE ---
cat("--- M1: NB + Year FE (broadest panel, GDP only) ---\n\n")

M1 <- fenegbin(
  Incident_Count ~ attacker_w4 + victim_w4 +
    attacker_ln_gdp_pc + victim_ln_gdp_pc | Year,
  data = df_2020,
  vcov = ~directed_dyad_id
)
print(summary(M1))

cat(sprintf(
  "\nM1: attacker_w4 = %.4f, p = %.2e, IRR = %.4f  [H1]\n",
  coef(M1)["attacker_w4"], pvalue(M1)["attacker_w4"],
  exp(coef(M1)["attacker_w4"])
))
cat(sprintf(
  "    victim_w4   = %.4f, p = %.2e, IRR = %.4f  [H2]\n",
  coef(M1)["victim_w4"], pvalue(M1)["victim_w4"],
  exp(coef(M1)["victim_w4"])
))


################################################################################
#                 SECTION 2: PANEL B — M2, M3, M4
#                 (W4 + CINC + GDP + Year FE, 2007-2016)
################################################################################
# Adds CINC as a capability control. CINC and GDP are near-orthogonal
# (r ≈ 0.12), separating material capability from economic development.
# M3 adds the ZINB extensive/intensive partition; M4 adds kinetic-conflict
# control via MID hostility >= 4.

cat("############################################################\n")
cat("#  SECTION 2: PANEL B — M2, M3, M4                         #\n")
cat("############################################################\n\n")


# --- M2: NB + Year FE ---
cat("--- M2: NB + Year FE (full controls) ---\n\n")

M2 <- fenegbin(
  Incident_Count ~ attacker_w4 + victim_w4 +
    attacker_cinc + victim_cinc +
    attacker_ln_gdp_pc + victim_ln_gdp_pc | Year,
  data = df_2016,
  vcov = ~directed_dyad_id
)
print(summary(M2))

cat(sprintf(
  "\nM2: attacker_w4 = %.4f, p = %.2e  [H1]\n",
  coef(M2)["attacker_w4"], pvalue(M2)["attacker_w4"]
))
cat(sprintf(
  "    victim_w4   = %.4f, p = %.2e  [H2]\n",
  coef(M2)["victim_w4"], pvalue(M2)["victim_w4"]
))


# --- M3: ZINB + Year FE in count stage ---
# pscl::zeroinfl does not have native FE syntax; we use as.factor(Year).
# Inflation stage: CINC only (capability determines structural zeros).
cat("\n\n--- M3: ZINB + Year FE in count stage ---\n\n")

M3 <- zeroinfl(
  Incident_Count ~ attacker_w4 + victim_w4 +
    attacker_cinc + victim_cinc +
    attacker_ln_gdp_pc + victim_ln_gdp_pc + as.factor(Year) |
    attacker_cinc + victim_cinc,
  data = df_2016,
  dist = "negbin"
)
print(summary(M3))

zinb_M3_coefs <- coef(M3, "count")
zinb_M3_pvals <- summary(M3)$coefficients$count[, "Pr(>|z|)"]
cat(sprintf(
  "\nM3 count: attacker_w4 = %.4f, p = %.2e  [H1]\n",
  zinb_M3_coefs["attacker_w4"], zinb_M3_pvals["attacker_w4"]
))
cat(sprintf(
  "          victim_w4   = %.4f, p = %.2e  [H2]\n",
  zinb_M3_coefs["victim_w4"], zinb_M3_pvals["victim_w4"]
))


# --- M4: NB + Year FE + MID(>=4) ---
# Effectively 2007-2014 (MID coverage ends 2014).
# MID(>=4) matches Panel C's exclusion criterion → consistent bridge between
# Panel B and Panel C.
cat("\n\n--- M4: NB + Year FE + MID use-of-force control (hostility >= 4) ---\n\n")

M4 <- fenegbin(
  Incident_Count ~ attacker_w4 + victim_w4 +
    attacker_cinc + victim_cinc +
    attacker_ln_gdp_pc + victim_ln_gdp_pc +
    mid_force | Year,
  data = df_2016,
  vcov = ~directed_dyad_id
)
print(summary(M4))

cat(sprintf(
  "\nM4: attacker_w4 = %.4f, p = %.2e  [H1]\n",
  coef(M4)["attacker_w4"], pvalue(M4)["attacker_w4"]
))
cat(sprintf(
  "    victim_w4   = %.4f, p = %.2e  [H2]\n",
  coef(M4)["victim_w4"], pvalue(M4)["victim_w4"]
))
cat(sprintf(
  "    mid_force   = %.4f, p = %.2e\n",
  coef(M4)["mid_force"], pvalue(M4)["mid_force"]
))
cat(sprintf("    Effective obs: %d (2015-2016 dropped)\n", nobs(M4)))


################################################################################
#                 SECTION 3: PANEL C — M5, M6, M7 (PRIMARY)
#                 Purely Cyber, Purely Unprovoked (2007-2014)
################################################################################
# Theoretically motivated DV (Incident_Count_Clean):
#   - 7 ACD incidents excluded (cyber-to-cyber retaliation per Tallinn Manual)
#   - 40 kinetic-conflict-linked incidents excluded (MID hostility >= 4)
#   - 122 kinetic dyad-years dropped from panel (no false zeros)
#
# M7 is the PRIMARY specification:
#   - Cleanest DV (Panel C)
#   - ZINB to handle severe zero inflation
#   - Year FE in BOTH count and inflation stages
# Caveat: if inflation-stage Year FE creates separation, fall back to M6
# (count-stage FE only) and document the convergence issue.

cat("############################################################\n")
cat("#  SECTION 3: PANEL C — M5, M6, M7 (PRIMARY)               #\n")
cat("############################################################\n\n")


# --- M5: NB + Year FE on clean DV ---
cat("--- M5: NB + Year FE (clean DV) ---\n\n")

M5 <- fenegbin(
  Incident_Count_Clean ~ attacker_w4 + victim_w4 +
    attacker_cinc + victim_cinc +
    attacker_ln_gdp_pc + victim_ln_gdp_pc | Year,
  data = df_2014,
  vcov = ~directed_dyad_id
)
print(summary(M5))

cat(sprintf(
  "\nM5: attacker_w4 = %.4f, p = %.2e, IRR = %.4f  [H1]\n",
  coef(M5)["attacker_w4"], pvalue(M5)["attacker_w4"],
  exp(coef(M5)["attacker_w4"])
))
cat(sprintf(
  "    victim_w4   = %.4f, p = %.2e, IRR = %.4f  [H2]\n",
  coef(M5)["victim_w4"], pvalue(M5)["victim_w4"],
  exp(coef(M5)["victim_w4"])
))


# --- M6: ZINB + Year FE in count stage (clean DV) ---
cat("\n\n--- M6: ZINB + Year FE in count stage (clean DV) ---\n\n")

M6 <- zeroinfl(
  Incident_Count_Clean ~ attacker_w4 + victim_w4 +
    attacker_cinc + victim_cinc +
    attacker_ln_gdp_pc + victim_ln_gdp_pc + as.factor(Year) |
    attacker_cinc + victim_cinc,
  data = df_2014,
  dist = "negbin"
)
print(summary(M6))

zinb_M6_coefs <- coef(M6, "count")
zinb_M6_pvals <- summary(M6)$coefficients$count[, "Pr(>|z|)"]
cat(sprintf(
  "\nM6 count: attacker_w4 = %.4f, p = %.2e  [H1]\n",
  zinb_M6_coefs["attacker_w4"], zinb_M6_pvals["attacker_w4"]
))
cat(sprintf(
  "          victim_w4   = %.4f, p = %.2e  [H2]\n",
  zinb_M6_coefs["victim_w4"], zinb_M6_pvals["victim_w4"]
))


# --- M7: ZINB + Year FE in BOTH stages (PRIMARY) ---
# Year FE enters both count and inflation equations to absorb temporal shocks
# (e.g., Snowden 2013, Sony 2014) consistently across the extensive and
# intensive margins.
cat("\n\n--- M7: ZINB + Year FE in BOTH stages (clean DV) — PRIMARY ---\n\n")

M7 <- zeroinfl(
  Incident_Count_Clean ~ attacker_w4 + victim_w4 +
    attacker_cinc + victim_cinc +
    attacker_ln_gdp_pc + victim_ln_gdp_pc + as.factor(Year) |
    attacker_cinc + victim_cinc + as.factor(Year),
  data = df_2014,
  dist = "negbin"
)
print(summary(M7))

zinb_M7_coefs <- coef(M7, "count")
zinb_M7_pvals <- summary(M7)$coefficients$count[, "Pr(>|z|)"]
cat(sprintf(
  "\nM7 count: attacker_w4 = %.4f, p = %.2e  [H1, PRIMARY]\n",
  zinb_M7_coefs["attacker_w4"], zinb_M7_pvals["attacker_w4"]
))
cat(sprintf(
  "          victim_w4   = %.4f, p = %.2e  [H2, PRIMARY]\n",
  zinb_M7_coefs["victim_w4"], zinb_M7_pvals["victim_w4"]
))


# --- Section 3 Summary ---
cat("\n\n--- Cross-Panel Summary (M1-M7) ---\n")
cat(sprintf(
  "  M1 NB    (A):     attacker_w4 = %+.4f (p=%.2e)  victim_w4 = %+.4f (p=%.2e)\n",
  coef(M1)["attacker_w4"], pvalue(M1)["attacker_w4"],
  coef(M1)["victim_w4"], pvalue(M1)["victim_w4"]
))
cat(sprintf(
  "  M2 NB    (B):     attacker_w4 = %+.4f (p=%.2e)  victim_w4 = %+.4f (p=%.2e)\n",
  coef(M2)["attacker_w4"], pvalue(M2)["attacker_w4"],
  coef(M2)["victim_w4"], pvalue(M2)["victim_w4"]
))
cat(sprintf(
  "  M3 ZINB  (B):     attacker_w4 = %+.4f (p=%.2e)  victim_w4 = %+.4f (p=%.2e)\n",
  zinb_M3_coefs["attacker_w4"], zinb_M3_pvals["attacker_w4"],
  zinb_M3_coefs["victim_w4"], zinb_M3_pvals["victim_w4"]
))
cat(sprintf(
  "  M4 NB+MID(B):     attacker_w4 = %+.4f (p=%.2e)  victim_w4 = %+.4f (p=%.2e)\n",
  coef(M4)["attacker_w4"], pvalue(M4)["attacker_w4"],
  coef(M4)["victim_w4"], pvalue(M4)["victim_w4"]
))
cat(sprintf(
  "  M5 NB    (C):     attacker_w4 = %+.4f (p=%.2e)  victim_w4 = %+.4f (p=%.2e)\n",
  coef(M5)["attacker_w4"], pvalue(M5)["attacker_w4"],
  coef(M5)["victim_w4"], pvalue(M5)["victim_w4"]
))
cat(sprintf(
  "  M6 ZINB  (C):     attacker_w4 = %+.4f (p=%.2e)  victim_w4 = %+.4f (p=%.2e)\n",
  zinb_M6_coefs["attacker_w4"], zinb_M6_pvals["attacker_w4"],
  zinb_M6_coefs["victim_w4"], zinb_M6_pvals["victim_w4"]
))
cat(sprintf(
  "  M7 ZINB+FE(C):    attacker_w4 = %+.4f (p=%.2e)  victim_w4 = %+.4f (p=%.2e)  [PRIMARY]\n",
  zinb_M7_coefs["attacker_w4"], zinb_M7_pvals["attacker_w4"],
  zinb_M7_coefs["victim_w4"], zinb_M7_pvals["victim_w4"]
))


################################################################################
#                 SECTION 4: SUMMARY TABLE
################################################################################

cat("\n\n############################################################\n")
cat("#  SECTION 4: SUMMARY TABLE                                #\n")
cat("############################################################\n\n")

table_dir <- "outputs/tables"
if (!dir.exists(table_dir)) dir.create(table_dir, recursive = TRUE)


# --- Helpers ---
# McFadden pseudo-R² for NB models only (not meaningful for ZINB).
pr2 <- function(model, null_ll) {
  if (inherits(model, "fixest")) {
    sprintf("%.4f", 1 - as.numeric(logLik(model)) / null_ll)
  } else {
    "\u2014"
  }
}

# Null log-likelihoods (intercept-only NB on each estimation sample)
null_2020 <- as.numeric(logLik(fenegbin(Incident_Count ~ 1, data = df_2020)))
null_2016 <- as.numeric(logLik(fenegbin(Incident_Count ~ 1, data = df_2016)))
null_2016_mid <- as.numeric(logLik(fenegbin(
  Incident_Count ~ 1,
  data = df_2016 %>% filter(!is.na(mid_force))
)))
null_2014 <- as.numeric(logLik(fenegbin(Incident_Count_Clean ~ 1, data = df_2014)))

# Sample descriptive stats per panel
nz_2020 <- sum(df_2020$Incident_Count > 0)
nz_2016 <- sum(df_2016$Incident_Count > 0)
nz_2016_mid <- sum(df_2016$Incident_Count[!is.na(df_2016$mid_force)] > 0)
nz_2014 <- sum(df_2014$Incident_Count_Clean > 0)

ti_2020 <- as.character(sum(df_2020$Incident_Count))
ti_2016 <- as.character(sum(df_2016$Incident_Count))
ti_2016_mid <- as.character(sum(df_2016$Incident_Count[!is.na(df_2016$mid_force)]))
ti_2014 <- as.character(sum(df_2014$Incident_Count_Clean))

pct_2020 <- sprintf("%.2f%%", 100 * nz_2020 / nrow(df_2020))
pct_2016 <- sprintf("%.2f%%", 100 * nz_2016 / nrow(df_2016))
pct_2016_mid <- sprintf("%.2f%%", 100 * nz_2016_mid / sum(!is.na(df_2016$mid_force)))
pct_2014 <- sprintf("%.2f%%", 100 * nz_2014 / nrow(df_2014))


# --- Coefficient maps ---
# W4 coefficients first to put H1 and H2 at the top of the table.
# ZINB models use "count_" prefix for count-stage coefs in modelsummary.
cm <- c(
  "attacker_w4"              = "Attacker W4 [H1]",
  "count_attacker_w4"        = "Attacker W4 [H1]",
  "victim_w4"                = "Victim W4 [H2]",
  "count_victim_w4"          = "Victim W4 [H2]",
  "attacker_cinc"            = "Attacker CINC",
  "count_attacker_cinc"      = "Attacker CINC",
  "victim_cinc"              = "Victim CINC",
  "count_victim_cinc"        = "Victim CINC",
  "attacker_ln_gdp_pc"       = "Attacker ln(GDP p.c.)",
  "count_attacker_ln_gdp_pc" = "Attacker ln(GDP p.c.)",
  "victim_ln_gdp_pc"         = "Victim ln(GDP p.c.)",
  "count_victim_ln_gdp_pc"   = "Victim ln(GDP p.c.)",
  "mid_force"                = "MID use of force (\u22654)",
  "(Intercept)"              = "Intercept",
  "count_(Intercept)"        = "Intercept"
)

gm <- list(
  list("raw" = "nobs", "clean" = "Observations", "fmt" = 0),
  list("raw" = "logLik", "clean" = "Log-Likelihood", "fmt" = 1),
  list("raw" = "ll", "clean" = "Log-Likelihood", "fmt" = 1),
  list("raw" = "AIC", "clean" = "AIC", "fmt" = 1),
  list("raw" = "aic", "clean" = "AIC", "fmt" = 1)
)


# --- Main table: M1-M7 ---
rows_main <- tibble(
  term = c(
    "Year FE", "Non-zero obs", "% non-zero", "Total incidents",
    "McFadden pseudo-R\u00B2"
  ),
  `M1 NB (A)` = c("Yes", as.character(nz_2020), pct_2020, ti_2020, pr2(M1, null_2020)),
  `M2 NB (B)` = c("Yes", as.character(nz_2016), pct_2016, ti_2016, pr2(M2, null_2016)),
  `M3 ZINB (B)` = c("Yes (count)", as.character(nz_2016), pct_2016, ti_2016, "\u2014"),
  `M4 NB+MID (B)` = c("Yes", as.character(nz_2016_mid), pct_2016_mid, ti_2016_mid, pr2(M4, null_2016_mid)),
  `M5 NB (C)` = c("Yes", as.character(nz_2014), pct_2014, ti_2014, pr2(M5, null_2014)),
  `M6 ZINB (C)` = c("Yes (count)", as.character(nz_2014), pct_2014, ti_2014, "\u2014"),
  `M7 ZINB+FE (C)` = c("Yes (both)", as.character(nz_2014), pct_2014, ti_2014, "\u2014")
)
attr(rows_main, "position") <- c(NA, NA, NA, NA, NA)

modelsummary(
  list(
    "M1 NB (A)"      = M1,
    "M2 NB (B)"      = M2,
    "M3 ZINB (B)"    = M3,
    "M4 NB+MID (B)"  = M4,
    "M5 NB (C)"      = M5,
    "M6 ZINB (C)"    = M6,
    "M7 ZINB+FE (C)" = M7
  ),
  output = file.path(table_dir, "table_main.html"),
  coef_map = cm,
  gof_map = gm,
  add_rows = rows_main,
  stars = c("*" = 0.05, "**" = 0.01, "***" = 0.001),
  title = "Main Models: Winning Coalition Index and Cyber Operations (M1-M7)",
  notes = paste(
    "All models include year fixed effects: in the linear predictor for NB",
    "models (M1, M2, M4, M5); in the count stage for M3 and M6; in both count",
    "and inflation stages for M7. Dyad-clustered SEs for NB models;",
    "model-based SEs for ZINB models (pscl does not support clustering).",
    "ZINB tables show count-stage coefficients only; inflation stage uses CINC",
    "(plus Year FE in M7). Each model jointly tests H1 (read attacker_w4) and",
    "H2 (read victim_w4). M7 is the primary specification."
  )
)
cat("Saved: outputs/tables/table_main.html\n")

cat("\nDone. Primary results: read attacker_w4 (H1) and victim_w4 (H2) from M7.\n")

# ---
# --- M7_inf: M7 with W4 in BOTH inflation and count stages ---
# Mirrors the count-stage W4 specification in the inflation equation.
# Yields four W4 coefficients organized as a 2x2:
#   - count_attacker_w4 = H1 intensive margin (frequency, given attacking)
#   - count_victim_w4   = H2 intensive margin (frequency, given attackable)
#   - zero_attacker_w4  = H1 extensive margin (whether ever attacks)
#   - zero_victim_w4    = H2 extensive margin (whether ever attackable)
# IMPORTANT: inflation stage models P(structural zero), so signs are FLIPPED
# relative to count stage. Negative inflation coefficient = MORE likely to
# be in the active dyad pool.

cat("\n\n--- M7_inf: M7 with W4 added to inflation stage ---\n\n")

M7_inf <- zeroinfl(
  Incident_Count_Clean ~ attacker_w4 + victim_w4 +
    attacker_cinc + victim_cinc +
    attacker_ln_gdp_pc + victim_ln_gdp_pc + as.factor(Year) |
    attacker_w4 + victim_w4 +
      attacker_cinc + victim_cinc + as.factor(Year),
  data = df_2014,
  dist = "negbin"
)
print(summary(M7_inf))

# Extract the 2x2 of W4 coefficients
c_coef <- summary(M7_inf)$coefficients$count
z_coef <- summary(M7_inf)$coefficients$zero

cat("\n--- W4 coefficients across both stages ---\n")
cat(sprintf(
  "  Count stage (intensive margin, neg = lower frequency):\n"
))
cat(sprintf(
  "    attacker_w4 = %+.4f, p = %.2e  [H1 intensive]\n",
  c_coef["attacker_w4", "Estimate"], c_coef["attacker_w4", "Pr(>|z|)"]
))
cat(sprintf(
  "    victim_w4   = %+.4f, p = %.2e  [H2 intensive]\n",
  c_coef["victim_w4", "Estimate"], c_coef["victim_w4", "Pr(>|z|)"]
))
cat(sprintf(
  "  Zero/inflation stage (extensive margin, NEG = MORE active):\n"
))
cat(sprintf(
  "    attacker_w4 = %+.4f, p = %.2e  [H1 extensive: POSITIVE supports H1]\n",
  z_coef["attacker_w4", "Estimate"], z_coef["attacker_w4", "Pr(>|z|)"]
))
cat(sprintf(
  "    victim_w4   = %+.4f, p = %.2e  [H2 extensive: NEGATIVE supports H2]\n",
  z_coef["victim_w4", "Estimate"], z_coef["victim_w4", "Pr(>|z|)"]
))

# LR test: is M7_inf significantly better than M7?
ll_diff <- 2 * (as.numeric(logLik(M7_inf)) - as.numeric(logLik(M7)))
lr_p <- pchisq(ll_diff, df = 2, lower.tail = FALSE)
cat(sprintf(
  "\nModel comparison (M7 nested in M7_inf):\n"
))
cat(sprintf(
  "  AIC: M7 = %.1f, M7_inf = %.1f, diff = %+.1f (negative favors M7_inf)\n",
  AIC(M7), AIC(M7_inf), AIC(M7_inf) - AIC(M7)
))
cat(sprintf(
  "  LR test: chi^2(2) = %.2f, p = %.4f\n", ll_diff, lr_p
))
