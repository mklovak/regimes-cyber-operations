##################### models.R #################################################
# Political Regimes and State-Level Cyber Aggression
#
# Structure:
#   Section 0: Data loading & preparation
#   Section 1: Panel A — df_model_2020 (W4 + GDP, 2007-2020)
#              M1 Poisson, M2 NB, M3 Logit
#   Section 2: Panel B — df_model_2016 (W4 + GDP + CINC, 2007-2016)
#              M5 NB, M6 Logit, M7 ZINB, M8 NB+MID, M9 Logit+MID
#   Section 3: Panel C — df_model_2014 (clean DV, 2007-2014) ← PRIMARY
#              M10 NB, M11 Logit, M12 ZINB, M13 ZINB+FE
#   Section 4: Summary tables and hypothesis verdicts
#
# Robustness checks (R1-R9) are in robustness.R
################################################################################

rm(list = ls())
options(scipen = 999)
cat("\014")

library(tidyverse)
library(readxl)
library(fixest) # NB, Logit, Poisson with dyad-clustered SEs
library(pscl) # Zero-Inflated Negative Binomial (ZINB) via zeroinfl().
# pscl = "Political Science Computational Laboratory" (Stanford).
# fixest does not support zero-inflated models, so M7/M12/M13
# use pscl (model-based SEs only). All other models use fixest
# (dyad-clustered SEs).
library(modelsummary) # HTML table export (supports both fixest and pscl objects)
library(MASS) # glm.nb for Poisson vs NB likelihood ratio test (R1)

# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


################################################################################
#                 SECTION 0: DATA LOADING & PREPARATION
################################################################################

cat("############################################################\n")
cat("#  SECTION 0: DATA LOADING                                 #\n")
cat("############################################################\n\n")

# --- 0.1 Load the three panels ---
df_2020 <- read_csv("outputs/df_model_2020.csv", show_col_types = FALSE) %>%
  mutate(
    directed_dyad_id = paste(attacker, victim, sep = "_"),
    has_attack       = as.integer(Incident_Count > 0)
  )

df_2016 <- read_csv("outputs/df_model_2016.csv", show_col_types = FALSE) %>%
  mutate(
    directed_dyad_id = paste(attacker, victim, sep = "_"),
    has_attack       = as.integer(Incident_Count > 0)
  )

df_2014 <- read_csv("outputs/df_model_2014.csv", show_col_types = FALSE) %>%
  mutate(
    directed_dyad_id = paste(attacker, victim, sep = "_"),
    has_attack_clean = as.integer(Incident_Count_Clean > 0)
  )

# --- 0.2 Build MID active-dispute dummy for M8/M9 ---
# MID dyadic v4.03 covers through 2014.
# mid_active = 1 if the dyad-year has any MID (any hostility level), 0 otherwise.
# For 2015-2016: set NA (MID coverage ends), which drops those years from M8/M9.

df_mid <- read_csv("data sources/dyadic_mid_4.03.csv", show_col_types = FALSE)

# All MID dyad-years (any hostility level)
mid_all <- df_mid %>%
  filter(year >= 2007, year <= 2014) %>%
  dplyr::select(statea, stateb, year) %>%
  distinct()

# COW code mapping (same as data_preparation)
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

# Expand MID to directed dyad-year with country names
mid_names <- mid_all %>%
  mutate(
    att1 = names(cow_name_to_code)[match(statea, cow_name_to_code)],
    vic1 = names(cow_name_to_code)[match(stateb, cow_name_to_code)],
    att2 = names(cow_name_to_code)[match(stateb, cow_name_to_code)],
    vic2 = names(cow_name_to_code)[match(statea, cow_name_to_code)]
  )

mid_directed <- bind_rows(
  mid_names %>%
    filter(!is.na(att1) & !is.na(vic1)) %>%
    dplyr::select(attacker = att1, victim = vic1, Year = year),
  mid_names %>%
    filter(!is.na(att2) & !is.na(vic2)) %>%
    dplyr::select(attacker = att2, victim = vic2, Year = year)
) %>%
  distinct() %>%
  mutate(mid_active = 1L)

# Merge into df_2016
df_2016 <- df_2016 %>%
  left_join(mid_directed, by = c("attacker", "victim", "Year")) %>%
  mutate(
    # 2007-2014: 0 if no MID, 1 if MID
    # 2015-2016: NA (MID data unavailable — these rows drop from M8/M9)
    mid_active = case_when(
      Year <= 2014 & !is.na(mid_active) ~ 1L,
      Year <= 2014 & is.na(mid_active) ~ 0L,
      Year > 2014 ~ NA_integer_
    )
  )

# Also build mid_force: hostility >= 4 (use of force or war)
# This threshold matches Panel C's exclusion criterion, creating a
# consistent bridge: M8b/M9b control for the same kinetic conflict
# that Panel C excludes entirely.
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
  "  Obs: %d | Incidents: %d | Years: 2007-2020 | Controls: W4 + GDP\n",
  nrow(df_2020), sum(df_2020$Incident_Count)
))

cat("--- Panel B: df_model_2016 ---\n")
cat(sprintf(
  "  Obs: %d | Incidents: %d | Years: 2007-2016 | Controls: W4 + GDP + CINC\n",
  nrow(df_2016), sum(df_2016$Incident_Count)
))
cat(sprintf(
  "  mid_active (any MID): %d dyad-years (2007-2014)\n",
  sum(df_2016$mid_active, na.rm = TRUE)
))
cat(sprintf(
  "  mid_force (hostility >= 4): %d dyad-years (2007-2014)\n",
  sum(df_2016$mid_force, na.rm = TRUE)
))
cat(sprintf(
  "  MID NA (2015-2016): %d obs → dropped from M8-M9b\n",
  sum(is.na(df_2016$mid_active))
))

cat("--- Panel C: df_model_2014 (PRIMARY) ---\n")
cat(sprintf(
  "  Obs: %d | Incidents: %d | Years: 2007-2014 | Controls: W4 + GDP + CINC\n",
  nrow(df_2014), sum(df_2014$Incident_Count_Clean)
))
cat("  DV: Incident_Count_Clean (7 ACD + 40 MID-linked excluded, 122 kinetic dyad-years dropped)\n\n")


################################################################################
#                 SECTION 1: PANEL A — df_model_2020
#                 (W4 + GDP, 2007-2020)
################################################################################
# Broadest panel, minimal controls. GDP proxies for both capability and
# development. Establishes that the raw W4-cyber relationship exists.

cat("############################################################\n")
cat("#  SECTION 1: PANEL A — df_model_2020 (W4 + GDP)           #\n")
cat("############################################################\n\n")


# --- M1: Poisson ---
# Baseline count model. Expected to be rejected due to overdispersion.
# The LR test (R1) justifies NB for all subsequent models.
cat("--- M1: Poisson (full sample, GDP only) ---\n\n")

M1 <- fepois(
  Incident_Count ~ attacker_w4 + victim_w4 +
    attacker_ln_gdp_pc + victim_ln_gdp_pc,
  data = df_2020,
  vcov = ~directed_dyad_id
)
print(summary(M1))

cat(sprintf(
  "\nM1: attacker_w4 = %.4f, p = %.2e\n",
  coef(M1)["attacker_w4"], pvalue(M1)["attacker_w4"]
))


# --- M2: Negative Binomial ---
# H1 test on broadest panel: does W4 predict cyber frequency?
cat("\n\n--- M2: Negative Binomial (full sample, GDP only) ---\n\n")

M2 <- fenegbin(
  Incident_Count ~ attacker_w4 + victim_w4 +
    attacker_ln_gdp_pc + victim_ln_gdp_pc,
  data = df_2020,
  vcov = ~directed_dyad_id
)
print(summary(M2))

cat(sprintf(
  "\nM2: attacker_w4 = %.4f, p = %.2e, IRR = %.4f\n",
  coef(M2)["attacker_w4"], pvalue(M2)["attacker_w4"],
  exp(coef(M2)["attacker_w4"])
))


# --- R1: Poisson vs NB (overdispersion LR test) ---
# Performed here inline because it justifies the estimator choice for all
# subsequent models. Also listed as R1 in robustness.R for completeness.
cat("\n\n--- R1: Poisson vs NB (Likelihood Ratio Test) ---\n")

# Refit both with glm.nb for LR test (fixest doesn't support lrtest directly)
m1_glm <- glm(
  Incident_Count ~ attacker_w4 + victim_w4 +
    attacker_ln_gdp_pc + victim_ln_gdp_pc,
  data = df_2020, family = poisson
)
m2_glm <- MASS::glm.nb(
  Incident_Count ~ attacker_w4 + victim_w4 +
    attacker_ln_gdp_pc + victim_ln_gdp_pc,
  data = df_2020, control = glm.control(maxit = 500)
)

lr_stat <- 2 * (logLik(m2_glm) - logLik(m1_glm))
lr_pval <- pchisq(as.numeric(lr_stat), df = 1, lower.tail = FALSE)
cat(sprintf("LR statistic: %.1f, p = %.2e\n", lr_stat, lr_pval))
cat("Conclusion: Poisson rejected. NB is the correct estimator.\n")


# --- M3: Logit ---
# H2 test on broadest panel: does W4 predict the likelihood of any attack?
cat("\n\n--- M3: Logit (full sample, GDP only) ---\n\n")

M3 <- feglm(
  has_attack ~ attacker_w4 + victim_w4 +
    attacker_ln_gdp_pc + victim_ln_gdp_pc,
  data = df_2020,
  family = binomial,
  vcov = ~directed_dyad_id
)
print(summary(M3))

cat(sprintf(
  "\nM3: attacker_w4 = %.4f, p = %.2e, OR = %.4f\n",
  coef(M3)["attacker_w4"], pvalue(M3)["attacker_w4"],
  exp(coef(M3)["attacker_w4"])
))


# --- Section 1 Summary ---
cat("\n\n--- Section 1 Summary (Panel A) ---\n")
cat(sprintf(
  "  M1 Poisson:  attacker_w4 = %.4f, p = %.2e\n",
  coef(M1)["attacker_w4"], pvalue(M1)["attacker_w4"]
))
cat(sprintf(
  "  M2 NB:       attacker_w4 = %.4f, p = %.2e\n",
  coef(M2)["attacker_w4"], pvalue(M2)["attacker_w4"]
))
cat(sprintf(
  "  M3 Logit:    attacker_w4 = %.4f, p = %.2e\n",
  coef(M3)["attacker_w4"], pvalue(M3)["attacker_w4"]
))
cat(sprintf("  R1 LR test:  %.1f, p = %.2e → Poisson rejected\n", lr_stat, lr_pval))
cat("H1 supported on broadest panel (M2). H2 status depends on controls.\n\n")


################################################################################
#                 SECTION 2: PANEL B — df_model_2016
#                 (W4 + GDP + CINC, 2007-2016)
################################################################################
# Adds CINC as a capability control. CINC and GDP are near-orthogonal
# (r ≈ 0.12), separating material capability from economic development.
# Key question: does W4 survive when CINC enters?

cat("############################################################\n")
cat("#  SECTION 2: PANEL B — df_model_2016 (W4 + GDP + CINC)    #\n")
cat("############################################################\n\n")


# --- M5: Negative Binomial ---
cat("--- M5: Negative Binomial (CINC + GDP) ---\n\n")

M5 <- fenegbin(
  Incident_Count ~ attacker_w4 + victim_w4 +
    attacker_cinc + victim_cinc +
    attacker_ln_gdp_pc + victim_ln_gdp_pc,
  data = df_2016,
  vcov = ~directed_dyad_id
)
print(summary(M5))

cat(sprintf(
  "\nM5: attacker_w4 = %.4f, p = %.2e, IRR = %.4f\n",
  coef(M5)["attacker_w4"], pvalue(M5)["attacker_w4"],
  exp(coef(M5)["attacker_w4"])
))


# --- M6: Logit ---
cat("\n\n--- M6: Logit (CINC + GDP) ---\n\n")

M6 <- feglm(
  has_attack ~ attacker_w4 + victim_w4 +
    attacker_cinc + victim_cinc +
    attacker_ln_gdp_pc + victim_ln_gdp_pc,
  data = df_2016,
  family = binomial,
  vcov = ~directed_dyad_id
)
print(summary(M6))

cat(sprintf(
  "\nM6: attacker_w4 = %.4f, p = %.2e, OR = %.4f\n",
  coef(M6)["attacker_w4"], pvalue(M6)["attacker_w4"],
  exp(coef(M6)["attacker_w4"])
))


# --- M7: ZINB ---
# Separates structural zeros (states that CAN'T attack) from strategic zeros
# (states that COULD but DON'T).
# Inflation stage: CINC only (capability determines structural zeros)
# Count stage: full specification
cat("\n\n--- M7: ZINB (CINC + GDP, inflation: CINC) ---\n\n")

M7 <- zeroinfl(
  Incident_Count ~ attacker_w4 + victim_w4 +
    attacker_cinc + victim_cinc +
    attacker_ln_gdp_pc + victim_ln_gdp_pc |
    attacker_cinc + victim_cinc,
  data = df_2016,
  dist = "negbin"
)
print(summary(M7))

zinb_M7_coefs <- coef(M7, "count")
zinb_M7_pvals <- summary(M7)$coefficients$count[, "Pr(>|z|)"]
cat(sprintf(
  "\nM7 count: attacker_w4 = %.4f, p = %.2e\n",
  zinb_M7_coefs["attacker_w4"], zinb_M7_pvals["attacker_w4"]
))


# --- M8: NB + MID active control ---
# Does W4 still predict cyber aggression after controlling for active
# kinetic conflict? MID coverage ends 2014 → 2015-2016 rows drop (NA).
cat("\n\n--- M8: NB + MID active control (effectively 2007-2014) ---\n\n")

M8 <- fenegbin(
  Incident_Count ~ attacker_w4 + victim_w4 +
    attacker_cinc + victim_cinc +
    attacker_ln_gdp_pc + victim_ln_gdp_pc +
    mid_active,
  data = df_2016,
  vcov = ~directed_dyad_id
)
print(summary(M8))

cat(sprintf(
  "\nM8: attacker_w4 = %.4f, p = %.2e\n",
  coef(M8)["attacker_w4"], pvalue(M8)["attacker_w4"]
))
cat(sprintf(
  "    mid_active  = %.4f, p = %.2e\n",
  coef(M8)["mid_active"], pvalue(M8)["mid_active"]
))
cat(sprintf("    Effective obs: %d (2015-2016 dropped)\n", nobs(M8)))


# --- M9: Logit + MID active control ---
cat("\n\n--- M9: Logit + MID active control (effectively 2007-2014) ---\n\n")

M9 <- feglm(
  has_attack ~ attacker_w4 + victim_w4 +
    attacker_cinc + victim_cinc +
    attacker_ln_gdp_pc + victim_ln_gdp_pc +
    mid_active,
  data = df_2016,
  family = binomial,
  vcov = ~directed_dyad_id
)
print(summary(M9))

cat(sprintf(
  "\nM9: attacker_w4 = %.4f, p = %.2e\n",
  coef(M9)["attacker_w4"], pvalue(M9)["attacker_w4"]
))
cat(sprintf(
  "    mid_active  = %.4f, p = %.2e\n",
  coef(M9)["mid_active"], pvalue(M9)["mid_active"]
))


# --- M8b: NB + MID use-of-force control (hostility >= 4) ---
# Same as M8 but controls only for kinetic USE OF FORCE, not all disputes.
# This threshold matches Panel C's exclusion criterion.
cat("\n\n--- M8b: NB + MID use-of-force control (hostility >= 4) ---\n\n")

M8b <- fenegbin(
  Incident_Count ~ attacker_w4 + victim_w4 +
    attacker_cinc + victim_cinc +
    attacker_ln_gdp_pc + victim_ln_gdp_pc +
    mid_force,
  data = df_2016,
  vcov = ~directed_dyad_id
)
print(summary(M8b))

cat(sprintf(
  "\nM8b: attacker_w4 = %.4f, p = %.2e\n",
  coef(M8b)["attacker_w4"], pvalue(M8b)["attacker_w4"]
))
cat(sprintf(
  "     mid_force   = %.4f, p = %.2e\n",
  coef(M8b)["mid_force"], pvalue(M8b)["mid_force"]
))


# --- M9b: Logit + MID use-of-force control (hostility >= 4) ---
cat("\n\n--- M9b: Logit + MID use-of-force control (hostility >= 4) ---\n\n")

M9b <- feglm(
  has_attack ~ attacker_w4 + victim_w4 +
    attacker_cinc + victim_cinc +
    attacker_ln_gdp_pc + victim_ln_gdp_pc +
    mid_force,
  data = df_2016,
  family = binomial,
  vcov = ~directed_dyad_id
)
print(summary(M9b))

cat(sprintf(
  "\nM9b: attacker_w4 = %.4f, p = %.2e\n",
  coef(M9b)["attacker_w4"], pvalue(M9b)["attacker_w4"]
))
cat(sprintf(
  "     mid_force   = %.4f, p = %.2e\n",
  coef(M9b)["mid_force"], pvalue(M9b)["mid_force"]
))


# --- Section 2 Summary ---
cat("\n\n--- Section 2 Summary (Panel B) ---\n")
cat(sprintf(
  "  M5   NB:              attacker_w4 = %.4f, p = %.2e\n",
  coef(M5)["attacker_w4"], pvalue(M5)["attacker_w4"]
))
cat(sprintf(
  "  M6   Logit:           attacker_w4 = %.4f, p = %.2e\n",
  coef(M6)["attacker_w4"], pvalue(M6)["attacker_w4"]
))
cat(sprintf(
  "  M7   ZINB count:      attacker_w4 = %.4f, p = %.2e\n",
  zinb_M7_coefs["attacker_w4"], zinb_M7_pvals["attacker_w4"]
))
cat(sprintf(
  "  M8   NB+MID(any):     attacker_w4 = %.4f, p = %.2e\n",
  coef(M8)["attacker_w4"], pvalue(M8)["attacker_w4"]
))
cat(sprintf(
  "  M9   Logit+MID(any):  attacker_w4 = %.4f, p = %.2e\n",
  coef(M9)["attacker_w4"], pvalue(M9)["attacker_w4"]
))
cat(sprintf(
  "  M8b  NB+MID(>=4):     attacker_w4 = %.4f, p = %.2e\n",
  coef(M8b)["attacker_w4"], pvalue(M8b)["attacker_w4"]
))
cat(sprintf(
  "  M9b  Logit+MID(>=4):  attacker_w4 = %.4f, p = %.2e\n",
  coef(M9b)["attacker_w4"], pvalue(M9b)["attacker_w4"]
))
cat("H1 survives CINC addition (M5) and both MID controls (M8, M8b).\n")
cat("MID is a massive predictor — kinetic conflict drives cyber activity.\n")
cat("M8b/M9b use the same hostility threshold as Panel C exclusion.\n\n")


################################################################################
#                 SECTION 3: PANEL C — df_model_2014 (PRIMARY)
#                 Purely Cyber, Purely Unprovoked (2007-2014)
################################################################################
# The theoretically motivated DV:
#   - 7 ACD incidents excluded (cyber-to-cyber retaliation per Tallinn Manual)
#   - 40 kinetic-conflict-linked incidents excluded (MID hostility >= 4)
#   - 122 kinetic dyad-years dropped from panel (no false zeros)
#
# This is the cleanest test of the selectorate mechanism in cyberspace.

cat("############################################################\n")
cat("#  SECTION 3: PANEL C — PRIMARY (CLEAN PANEL)              #\n")
cat("############################################################\n\n")


# --- M10: Negative Binomial (PRIMARY H1) ---
cat("--- M10: Negative Binomial (clean panel) — PRIMARY H1 ---\n\n")

M10 <- fenegbin(
  Incident_Count_Clean ~ attacker_w4 + victim_w4 +
    attacker_cinc + victim_cinc +
    attacker_ln_gdp_pc + victim_ln_gdp_pc,
  data = df_2014,
  vcov = ~directed_dyad_id
)
print(summary(M10))

cat(sprintf(
  "\nM10: attacker_w4 = %.4f, p = %.2e, IRR = %.4f\n",
  coef(M10)["attacker_w4"], pvalue(M10)["attacker_w4"],
  exp(coef(M10)["attacker_w4"])
))


# --- M11: Logit (PRIMARY H2/H3) ---
cat("\n\n--- M11: Logit (clean panel) — PRIMARY H2/H3 ---\n\n")

M11 <- feglm(
  has_attack_clean ~ attacker_w4 + victim_w4 +
    attacker_cinc + victim_cinc +
    attacker_ln_gdp_pc + victim_ln_gdp_pc,
  data = df_2014,
  family = binomial,
  vcov = ~directed_dyad_id
)
print(summary(M11))

cat(sprintf(
  "\nM11: attacker_w4 = %.4f, p = %.2e, OR = %.4f\n",
  coef(M11)["attacker_w4"], pvalue(M11)["attacker_w4"],
  exp(coef(M11)["attacker_w4"])
))


# --- M12: ZINB (structural zeros on clean panel) ---
cat("\n\n--- M12: ZINB (clean panel, inflation: CINC) ---\n\n")

M12 <- zeroinfl(
  Incident_Count_Clean ~ attacker_w4 + victim_w4 +
    attacker_cinc + victim_cinc +
    attacker_ln_gdp_pc + victim_ln_gdp_pc |
    attacker_cinc + victim_cinc,
  data = df_2014,
  dist = "negbin"
)
print(summary(M12))

zinb_M12_coefs <- coef(M12, "count")
zinb_M12_pvals <- summary(M12)$coefficients$count[, "Pr(>|z|)"]
cat(sprintf(
  "\nM12 count: attacker_w4 = %.4f, p = %.2e\n",
  zinb_M12_coefs["attacker_w4"], zinb_M12_pvals["attacker_w4"]
))


# --- M13: ZINB + Year FE (temporal confounders on clean panel) ---
cat("\n\n--- M13: ZINB + Year FE (clean panel) ---\n\n")

M13 <- zeroinfl(
  Incident_Count_Clean ~ attacker_w4 + victim_w4 +
    attacker_cinc + victim_cinc +
    attacker_ln_gdp_pc + victim_ln_gdp_pc + as.factor(Year) |
    attacker_cinc + victim_cinc,
  data = df_2014,
  dist = "negbin"
)
print(summary(M13))

zinb_M13_coefs <- coef(M13, "count")
zinb_M13_pvals <- summary(M13)$coefficients$count[, "Pr(>|z|)"]
cat(sprintf(
  "\nM13 count: attacker_w4 = %.4f, p = %.2e\n",
  zinb_M13_coefs["attacker_w4"], zinb_M13_pvals["attacker_w4"]
))


# --- Section 3 Summary ---
cat("\n\n--- Section 3 Summary (Panel C — Primary) ---\n")
cat(sprintf(
  "  M10 NB:         attacker_w4 = %.4f, p = %.2e  %s\n",
  coef(M10)["attacker_w4"], pvalue(M10)["attacker_w4"],
  ifelse(pvalue(M10)["attacker_w4"] < 0.05, "-> H1 SUPPORTED", "-> H1 not supported")
))
cat(sprintf(
  "  M11 Logit:      attacker_w4 = %.4f, p = %.2e  %s\n",
  coef(M11)["attacker_w4"], pvalue(M11)["attacker_w4"],
  ifelse(pvalue(M11)["attacker_w4"] < 0.05, "-> H2 SUPPORTED", "-> H2 not supported")
))
cat(sprintf(
  "  M12 ZINB count: attacker_w4 = %.4f, p = %.2e  %s\n",
  zinb_M12_coefs["attacker_w4"], zinb_M12_pvals["attacker_w4"],
  ifelse(zinb_M12_pvals["attacker_w4"] < 0.05, "-> H1 survives zeros", "-> weakens")
))
cat(sprintf(
  "  M13 ZINB+FE:    attacker_w4 = %.4f, p = %.2e  %s\n",
  zinb_M13_coefs["attacker_w4"], zinb_M13_pvals["attacker_w4"],
  ifelse(zinb_M13_pvals["attacker_w4"] < 0.05, "-> H1 survives FE", "-> weakens")
))

cat("\nH3 (victim targeting):\n")
cat(sprintf(
  "  M10: victim_w4 = %.4f, p = %.2e\n",
  coef(M10)["victim_w4"], pvalue(M10)["victim_w4"]
))
cat(sprintf(
  "  M11: victim_w4 = %.4f, p = %.2e\n",
  coef(M11)["victim_w4"], pvalue(M11)["victim_w4"]
))
cat("  -> H3 not supported (absorbed by CINC)\n\n")


################################################################################
#                 SECTION 4: SUMMARY
################################################################################

cat("############################################################\n")
cat("#  SECTION 4: SUMMARY                                      #\n")
cat("############################################################\n\n")

# --- 4.1 Coefficient progression ---
cat("--- attacker_w4 coefficient progression ---\n\n")
cat("Panel A (W4 + GDP, 2007-2020):\n")
cat(sprintf(
  "  M1  Poisson:     %.4f  p = %.2e\n",
  coef(M1)["attacker_w4"], pvalue(M1)["attacker_w4"]
))
cat(sprintf(
  "  M2  NB:          %.4f  p = %.2e\n",
  coef(M2)["attacker_w4"], pvalue(M2)["attacker_w4"]
))
cat(sprintf(
  "  M3  Logit:       %.4f  p = %.2e\n",
  coef(M3)["attacker_w4"], pvalue(M3)["attacker_w4"]
))

cat("\nPanel B (W4 + GDP + CINC, 2007-2016):\n")
cat(sprintf(
  "  M5   NB:              %.4f  p = %.2e\n",
  coef(M5)["attacker_w4"], pvalue(M5)["attacker_w4"]
))
cat(sprintf(
  "  M6   Logit:           %.4f  p = %.2e\n",
  coef(M6)["attacker_w4"], pvalue(M6)["attacker_w4"]
))
cat(sprintf(
  "  M7   ZINB count:      %.4f  p = %.2e\n",
  zinb_M7_coefs["attacker_w4"], zinb_M7_pvals["attacker_w4"]
))
cat(sprintf(
  "  M8   NB+MID(any):     %.4f  p = %.2e\n",
  coef(M8)["attacker_w4"], pvalue(M8)["attacker_w4"]
))
cat(sprintf(
  "  M9   Logit+MID(any):  %.4f  p = %.2e\n",
  coef(M9)["attacker_w4"], pvalue(M9)["attacker_w4"]
))
cat(sprintf(
  "  M8b  NB+MID(>=4):     %.4f  p = %.2e\n",
  coef(M8b)["attacker_w4"], pvalue(M8b)["attacker_w4"]
))
cat(sprintf(
  "  M9b  Logit+MID(>=4):  %.4f  p = %.2e\n",
  coef(M9b)["attacker_w4"], pvalue(M9b)["attacker_w4"]
))

cat("\nPanel C (clean DV, 2007-2014) <- PRIMARY:\n")
cat(sprintf(
  "  M10 NB:          %.4f  p = %.2e  <- PRIMARY H1\n",
  coef(M10)["attacker_w4"], pvalue(M10)["attacker_w4"]
))
cat(sprintf(
  "  M11 Logit:       %.4f  p = %.2e  <- PRIMARY H2\n",
  coef(M11)["attacker_w4"], pvalue(M11)["attacker_w4"]
))
cat(sprintf(
  "  M12 ZINB count:  %.4f  p = %.2e\n",
  zinb_M12_coefs["attacker_w4"], zinb_M12_pvals["attacker_w4"]
))
cat(sprintf(
  "  M13 ZINB+FE:     %.4f  p = %.2e\n",
  zinb_M13_coefs["attacker_w4"], zinb_M13_pvals["attacker_w4"]
))


# --- 4.2 Side-by-side table (fixest models only — ZINB not compatible) ---
cat("\n\n--- Regression Table (fixest models) ---\n\n")

etable(M1, M2, M3, M5, M6, M8, M9, M8b, M9b, M10, M11,
  headers = c(
    "M1 Pois", "M2 NB", "M3 Logit",
    "M5 NB", "M6 Logit",
    "M8 NB+MID", "M9 Log+MID",
    "M8b NB+MID4", "M9b Log+MID4",
    "M10 NB*", "M11 Log*"
  ),
  se.below = TRUE,
  fitstat = ~ n + ll + aic + bic,
  dict = c(
    attacker_w4        = "Attacker W4",
    victim_w4          = "Victim W4",
    attacker_cinc      = "Attacker CINC",
    victim_cinc        = "Victim CINC",
    attacker_ln_gdp_pc = "Attacker ln(GDP p.c.)",
    victim_ln_gdp_pc   = "Victim ln(GDP p.c.)",
    mid_active         = "MID active (any)",
    mid_force          = "MID use of force (>=4)"
  )
)
cat("* = primary specification (clean panel)\n")


# --- 4.3 ZINB summary (pscl models — separate table) ---
cat("\n--- ZINB Models (count stage coefficients) ---\n\n")
cat(sprintf("%-20s %12s %12s %12s\n", "", "M7 (Panel B)", "M12 (Panel C)", "M13 (C+FE)"))
cat(sprintf(
  "%-20s %12.4f %12.4f %12.4f\n", "Attacker W4",
  zinb_M7_coefs["attacker_w4"], zinb_M12_coefs["attacker_w4"], zinb_M13_coefs["attacker_w4"]
))
cat(sprintf(
  "%-20s %12.2e %12.2e %12.2e\n", "  p-value",
  zinb_M7_pvals["attacker_w4"], zinb_M12_pvals["attacker_w4"], zinb_M13_pvals["attacker_w4"]
))
cat(sprintf(
  "%-20s %12.4f %12.4f %12.4f\n", "Attacker CINC",
  zinb_M7_coefs["attacker_cinc"], zinb_M12_coefs["attacker_cinc"], zinb_M13_coefs["attacker_cinc"]
))
cat(sprintf(
  "%-20s %12.4f %12.4f %12.4f\n", "Victim CINC",
  zinb_M7_coefs["victim_cinc"], zinb_M12_coefs["victim_cinc"], zinb_M13_coefs["victim_cinc"]
))
cat(sprintf(
  "%-20s %12.1f %12.1f %12.1f\n", "Log-Likelihood",
  logLik(M7), logLik(M12), logLik(M13)
))
cat(sprintf(
  "%-20s %12.1f %12.1f %12.1f\n", "AIC",
  AIC(M7), AIC(M12), AIC(M13)
))


# --- 4.4 Hypothesis verdicts ---
cat("\n\n--- Hypothesis Verdicts ---\n\n")
cat("H0: W4 has no effect on state-backed cyber operations\n")
cat("    REJECTED — attacker_w4 significant in all NB/ZINB specifications\n\n")

cat("H1: Larger attacker W4 -> lower FREQUENCY of unprovoked cyber operations\n")
cat("    SUPPORTED — negative and significant in M2, M5, M7, M8, M8b, M10, M12, M13\n")
cat("    Effect holds across all panels, survives CINC, both MID controls, ZINB, year FE\n\n")

cat("H2: Larger attacker W4 -> lower LIKELIHOOD of conducting cyber operations\n")
cat("    NOT SUPPORTED — not significant in M3, M6, M9, M11 when CINC present\n")
cat("    Capability (CINC) dominates the extensive margin\n\n")

cat("H3: Larger victim W4 -> higher likelihood of being targeted\n")
cat("    NOT SUPPORTED — victim_w4 not significant in any specification\n")
cat("    Absorbed by victim CINC\n\n")

cat("--- Interpretation ---\n")
cat("Selectorate theory's structural mechanism (preference formation via\n")
cat("coalition size) operates on the INTENSIVE margin: among states that\n")
cat("engage in cyber operations, those with larger coalitions attack less\n")
cat("frequently. This constraint does not depend on public observation —\n")
cat("it holds in a covert domain.\n\n")
cat("The ACCOUNTABILITY mechanism (from democratic peace theory) fails on\n")
cat("the EXTENSIVE margin: the binary decision to enter cyber conflict is\n")
cat("driven by material capability (CINC), not coalition size. Capable\n")
cat("democracies engage in cyber operations at similar rates to capable\n")
cat("autocracies.\n\n")
cat("Cyberspace reveals what kinetic conflict conceals: the selectorate\n")
cat("mechanism and the accountability mechanism are separable, and only\n")
cat("the former transfers to a covert domain.\n")


################################################################################
#                 SECTION 5: HTML TABLES (modelsummary)
################################################################################
# modelsummary supports fixest and pscl objects natively.
# Uses dyad-clustered SEs from fixest; model-based SEs from pscl (ZINB).

cat("\n\n############################################################\n")
cat("#  SECTION 5: HTML TABLES (modelsummary)                   #\n")
cat("############################################################\n\n")

table_dir <- "outputs/tables"
if (!dir.exists(table_dir)) dir.create(table_dir, recursive = TRUE)

# Common coefficient map for readable labels (fixest models)
cm <- c(
  "attacker_w4"        = "Attacker W4",
  "victim_w4"          = "Victim W4",
  "attacker_cinc"      = "Attacker CINC",
  "victim_cinc"        = "Victim CINC",
  "attacker_ln_gdp_pc" = "Attacker ln(GDP p.c.)",
  "victim_ln_gdp_pc"   = "Victim ln(GDP p.c.)",
  "mid_active"         = "MID active (any)",
  "mid_force"          = "MID use of force (>=4)",
  "(Intercept)"        = "Intercept"
)

# Extended map for tables mixing fixest + ZINB models
# pscl::zeroinfl prefixes coefficients with "count_" and "zero_"
cm_mixed <- c(
  "attacker_w4"              = "Attacker W4",
  "count_attacker_w4"        = "Attacker W4",
  "victim_w4"                = "Victim W4",
  "count_victim_w4"          = "Victim W4",
  "attacker_cinc"            = "Attacker CINC",
  "count_attacker_cinc"      = "Attacker CINC",
  "victim_cinc"              = "Victim CINC",
  "count_victim_cinc"        = "Victim CINC",
  "attacker_ln_gdp_pc"       = "Attacker ln(GDP p.c.)",
  "count_attacker_ln_gdp_pc" = "Attacker ln(GDP p.c.)",
  "victim_ln_gdp_pc"         = "Victim ln(GDP p.c.)",
  "count_victim_ln_gdp_pc"   = "Victim ln(GDP p.c.)",
  "mid_active"               = "MID active (any)",
  "mid_force"                = "MID use of force (>=4)",
  "(Intercept)"              = "Intercept",
  "count_(Intercept)"        = "Intercept"
)

# Goodness-of-fit rows to include
gm <- list(
  list("raw" = "nobs", "clean" = "Observations", "fmt" = 0),
  list("raw" = "logLik", "clean" = "Log-Likelihood", "fmt" = 1),
  list("raw" = "AIC", "clean" = "AIC", "fmt" = 1),
  list("raw" = "BIC", "clean" = "BIC", "fmt" = 1)
)


# --- Pre-compute non-zero stats for table footnotes ---
nz_2020 <- sum(df_2020$Incident_Count > 0)
nz_2016 <- sum(df_2016$Incident_Count > 0)
nz_2016_mid <- sum(df_2016$Incident_Count[!is.na(df_2016$mid_active)] > 0)
nz_2014 <- sum(df_2014$Incident_Count_Clean > 0)

pct_2020 <- sprintf("%.4f%%", nz_2020 / nrow(df_2020) * 100)
pct_2016 <- sprintf("%.4f%%", nz_2016 / nrow(df_2016) * 100)
pct_2016_mid <- sprintf("%.4f%%", nz_2016_mid / sum(!is.na(df_2016$mid_active)) * 100)
pct_2014 <- sprintf("%.4f%%", nz_2014 / nrow(df_2014) * 100)

ti_2020 <- as.character(sum(df_2020$Incident_Count))
ti_2016 <- as.character(sum(df_2016$Incident_Count))
ti_2016_mid <- as.character(sum(df_2016$Incident_Count[!is.na(df_2016$mid_active)]))
ti_2014 <- as.character(sum(df_2014$Incident_Count_Clean))


# --- Table 1: Panel A (M1 Poisson, M2 NB, M3 Logit) ---
rows_t1 <- tibble(
  term = c("Non-zero obs", "% non-zero", "Total incidents"),
  `M1 Poisson` = c(as.character(nz_2020), pct_2020, ti_2020),
  `M2 Neg. Binom.` = c(as.character(nz_2020), pct_2020, ti_2020),
  `M3 Logit` = c(as.character(nz_2020), pct_2020, "—")
)
attr(rows_t1, "position") <- c(NA, NA, NA)

modelsummary(
  list("M1 Poisson" = M1, "M2 Neg. Binom." = M2, "M3 Logit" = M3),
  output = file.path(table_dir, "table1_panel_a.html"),
  coef_map = cm,
  gof_map = gm,
  add_rows = rows_t1,
  stars = c("*" = 0.05, "**" = 0.01, "***" = 0.001),
  title = "Panel A: Baseline Models (W4 + GDP, 2007-2020)",
  notes = "Dyad-clustered SEs in parentheses."
)
cat("Saved: outputs/tables/table1_panel_a.html\n")


# --- Table 2: Panel B main (M5 NB, M6 Logit, M7 ZINB) ---
rows_t2 <- tibble(
  term = c("Non-zero obs", "% non-zero", "Total incidents"),
  `M5 Neg. Binom.` = c(as.character(nz_2016), pct_2016, ti_2016),
  `M6 Logit` = c(as.character(nz_2016), pct_2016, "—"),
  `M7 ZINB (count)` = c(as.character(nz_2016), pct_2016, ti_2016)
)
attr(rows_t2, "position") <- c(NA, NA, NA)

modelsummary(
  list("M5 Neg. Binom." = M5, "M6 Logit" = M6, "M7 ZINB (count)" = M7),
  output = file.path(table_dir, "table2_panel_b.html"),
  coef_map = cm_mixed,
  gof_map = gm,
  add_rows = rows_t2,
  stars = c("*" = 0.05, "**" = 0.01, "***" = 0.001),
  title = "Panel B: Full Controls (W4 + CINC + GDP, 2007-2016)",
  notes = "Dyad-clustered SEs for M5-M6; model-based SEs for M7 (ZINB). M7 shows count-stage coefficients."
)
cat("Saved: outputs/tables/table2_panel_b.html\n")


# --- Table 3: Panel B + MID controls (M8, M9, M8b, M9b) ---
rows_t3 <- tibble(
  term = c("Non-zero obs", "% non-zero", "Total incidents"),
  `M8 NB+MID(any)` = c(as.character(nz_2016_mid), pct_2016_mid, ti_2016_mid),
  `M9 Logit+MID(any)` = c(as.character(nz_2016_mid), pct_2016_mid, "—"),
  `M8b NB+MID(>=4)` = c(as.character(nz_2016_mid), pct_2016_mid, ti_2016_mid),
  `M9b Logit+MID(>=4)` = c(as.character(nz_2016_mid), pct_2016_mid, "—")
)
attr(rows_t3, "position") <- c(NA, NA, NA)

modelsummary(
  list(
    "M8 NB+MID(any)" = M8, "M9 Logit+MID(any)" = M9,
    "M8b NB+MID(>=4)" = M8b, "M9b Logit+MID(>=4)" = M9b
  ),
  output = file.path(table_dir, "table3_panel_b_mid.html"),
  coef_map = cm,
  gof_map = gm,
  add_rows = rows_t3,
  stars = c("*" = 0.05, "**" = 0.01, "***" = 0.001),
  title = "Panel B: Militarized Dispute Controls (effectively 2007-2014)",
  notes = "Dyad-clustered SEs. MID coverage ends 2014; 2015-2016 dropped. M8b/M9b use hostility >= 4 threshold matching Panel C exclusion."
)
cat("Saved: outputs/tables/table3_panel_b_mid.html\n")


# --- Table 4: Panel C primary (M10 NB, M11 Logit, M12 ZINB, M13 ZINB+FE) ---
rows_t4 <- tibble(
  term = c("Non-zero obs", "% non-zero", "Total incidents"),
  `M10 Neg. Binom.` = c(as.character(nz_2014), pct_2014, ti_2014),
  `M11 Logit` = c(as.character(nz_2014), pct_2014, "—"),
  `M12 ZINB (count)` = c(as.character(nz_2014), pct_2014, ti_2014),
  `M13 ZINB+FE (count)` = c(as.character(nz_2014), pct_2014, ti_2014)
)
attr(rows_t4, "position") <- c(NA, NA, NA)

modelsummary(
  list(
    "M10 Neg. Binom." = M10, "M11 Logit" = M11,
    "M12 ZINB (count)" = M12, "M13 ZINB+FE (count)" = M13
  ),
  output = file.path(table_dir, "table4_panel_c_primary.html"),
  coef_map = cm_mixed,
  gof_map = gm,
  add_rows = rows_t4,
  stars = c("*" = 0.05, "**" = 0.01, "***" = 0.001),
  title = "Panel C: Primary — Purely Cyber, Purely Unprovoked (2007-2014)",
  notes = "Dyad-clustered SEs for M10-M11; model-based SEs for M12-M13 (ZINB). Clean DV: 7 ACD + 40 MID-linked incidents excluded; 122 kinetic dyad-years dropped."
)
cat("Saved: outputs/tables/table4_panel_c_primary.html\n")


# --- Table 5: H1 progression (NB across all panels) ---
rows_t5 <- tibble(
  term = c("Non-zero obs", "% non-zero", "Total incidents"),
  `M2 (Panel A)` = c(as.character(nz_2020), pct_2020, ti_2020),
  `M5 (Panel B)` = c(as.character(nz_2016), pct_2016, ti_2016),
  `M8 (B+MID any)` = c(as.character(nz_2016_mid), pct_2016_mid, ti_2016_mid),
  `M8b (B+MID>=4)` = c(as.character(nz_2016_mid), pct_2016_mid, ti_2016_mid),
  `M10 (Panel C)` = c(as.character(nz_2014), pct_2014, ti_2014)
)
attr(rows_t5, "position") <- c(NA, NA, NA)

modelsummary(
  list(
    "M2 (Panel A)" = M2, "M5 (Panel B)" = M5,
    "M8 (B+MID any)" = M8, "M8b (B+MID>=4)" = M8b, "M10 (Panel C)" = M10
  ),
  output = file.path(table_dir, "table5_h1_progression.html"),
  coef_map = cm,
  gof_map = gm,
  add_rows = rows_t5,
  stars = c("*" = 0.05, "**" = 0.01, "***" = 0.001),
  title = "H1 Progression: Winning Coalition Size and Cyber Attack Frequency",
  notes = "All models: negative binomial with dyad-clustered SEs. M8b bridges to Panel C (same hostility threshold). M10 is the primary specification."
)
cat("Saved: outputs/tables/table5_h1_progression.html\n")

cat("\nAll tables saved to:", table_dir, "\n")
