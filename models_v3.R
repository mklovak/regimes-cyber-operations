##################### Environment Setup ########################################
rm(list = ls())
try(dev.off(), silent = TRUE)
options(scipen = 999)
cat("\014")

library(tidyverse)
library(dplyr)
library(fixest) # fenegbin, feglm — handles NB, logit, clustered SEs, FE natively

# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

##################### 1. Load and merge data ###################################
# Base dataset: df_model_2016 (directed dyad-year, 2007-2016, W4 + CINC)
# GDP lookup: unique country-year ln(GDP per capita) extracted from df_model_2020
# Merge produces the "kitchen sink" dataset: W4 + CINC + GDP per capita

df <- read_csv("outputs/df_model_2016.csv")

# cat("============================================================\n")
# cat("  Data Loading\n")
# cat("============================================================\n")
# cat("df rows:", nrow(df), "\n")

# # Drop rows missing any variable (zero incidents lost — verified)
# complete <- complete.cases(
#   df$attacker_w4, df$victim_w4,
#   df$attacker_cinc, df$victim_cinc,
#   df$attacker_ln_gdp_pc, df$victim_ln_gdp_pc
# )
# cat("Rows dropped (missing GDP):", sum(!complete), "\n") # 0
# cat("Incidents in dropped rows:", sum(df$Incident_Count[!complete]), "\n") # 0

# df <- df[complete, ]

##################### 2. Derived variables #####################################

# directed_dyad_id: clustering unit for sandwich SEs (attacker-victim pair across years)
df <- df %>%
  mutate(
    directed_dyad_id = paste(attacker, victim, sep = "_"),
    has_attack = as.integer(Incident_Count > 0)
  )

cat("\n============================================================\n")
cat("  Final Dataset Summary\n")
cat("============================================================\n")
cat("Observations:", nrow(df), "\n")
cat("Unique directed dyads:", n_distinct(df$directed_dyad_id), "\n")
cat("Unique attackers:", n_distinct(df$attacker), "\n")
cat("Years:", min(df$Year), "-", max(df$Year), "\n")
cat("Dyad-years with incidents:", sum(df$has_attack), "\n")
cat("Total incidents:", sum(df$Incident_Count), "\n")
cat("% non-zero:", round(mean(df$has_attack) * 100, 4), "%\n")
cat(
  "Overdispersion check — Var/Mean:",
  round(var(df$Incident_Count) / mean(df$Incident_Count), 2), "\n\n"
)


################################################################################
#                 MODEL 1c: NEGATIVE BINOMIAL — H1 + H3 (kitchen sink)
################################################################################
# H1: Attacker W4 is negatively associated with the FREQUENCY of cyber operations.
#     Selectorate theory predicts that leaders with small winning coalitions (low W4)
#     face fewer constraints and can pursue cyber aggression more freely.
#     Expect: attacker_w4 < 0 (higher W4 → fewer attacks).
#
# H3: States with larger W4 are more likely to be VICTIMS.
#     Expect: victim_w4 > 0 (democracies are more attractive targets).
#     Note: from descriptive stats, H3 disappeared under GDP control because
#     victim wealth (not regime type) drives targeting. Expect victim_w4 to be
#     non-significant when GDP is included.
#
# Controls:
#   - attacker_cinc, victim_cinc: national material capability (military/industrial)
#   - attacker_ln_gdp_pc, victim_ln_gdp_pc: economic development (public goods channel)
#   CINC and GDP are nearly orthogonal (r=0.12) — independent dimensions of state capacity.
#
# Estimator: Negative Binomial (fixest::fenegbin)
#   - Overdispersion: Var/Mean >> 1, 99.95% zeros → NB over Poisson
#   - Dyad-clustered SEs: observations within a dyad are not independent across years
#
# Reference: fixest clusters SEs natively via vcov argument — no sandwich crash.

cat("============================================================\n")
cat("  Model 1c: Negative Binomial (CINC + GDP controls)\n")
cat("============================================================\n\n")

m1c <- fenegbin(
  Incident_Count ~ attacker_w4 + victim_w4 +
    attacker_cinc + victim_cinc +
    attacker_ln_gdp_pc + victim_ln_gdp_pc,
  data = df,
  vcov = ~directed_dyad_id
)

summary(m1c)

# ---- Interpretation ----
cat("\n--- Model 1c Interpretation ---\n")
coefs <- coef(m1c)
pvals <- pvalue(m1c)

cat(sprintf(
  "attacker_w4:        coef = %.4f, p = %.2e  %s\n",
  coefs["attacker_w4"], pvals["attacker_w4"],
  ifelse(pvals["attacker_w4"] < 0.05,
    "→ H1 SUPPORTED: higher W4 reduces attack frequency",
    "→ H1 not supported at p<0.05"
  )
))

cat(sprintf(
  "victim_w4:          coef = %.4f, p = %.2e  %s\n",
  coefs["victim_w4"], pvals["victim_w4"],
  ifelse(pvals["victim_w4"] < 0.05,
    "→ H3 SUPPORTED: higher victim W4 increases targeting",
    "→ H3 not supported (likely absorbed by GDP — see descriptive stats)"
  )
))

cat(sprintf(
  "attacker_cinc:      coef = %.4f, p = %.2e\n",
  coefs["attacker_cinc"], pvals["attacker_cinc"]
))
cat(sprintf(
  "victim_cinc:        coef = %.4f, p = %.2e\n",
  coefs["victim_cinc"], pvals["victim_cinc"]
))
cat(sprintf(
  "attacker_ln_gdp_pc: coef = %.4f, p = %.2e\n",
  coefs["attacker_ln_gdp_pc"], pvals["attacker_ln_gdp_pc"]
))
cat(sprintf(
  "victim_ln_gdp_pc:   coef = %.4f, p = %.2e\n",
  coefs["victim_ln_gdp_pc"], pvals["victim_ln_gdp_pc"]
))

# Incidence rate ratios (exponentiated coefficients) for substantive interpretation
cat("\n--- Incidence Rate Ratios (IRR = exp(coef)) ---\n")
cat("A one-unit increase in attacker_w4 (0→1, full autocracy→full democracy)\n")
cat(sprintf(
  "multiplies expected incident count by %.4f (%.1f%% change)\n",
  exp(coefs["attacker_w4"]),
  (exp(coefs["attacker_w4"]) - 1) * 100
))
cat("A one-unit increase in victim_ln_gdp_pc (~2.7x richer)\n")
cat(sprintf(
  "multiplies expected incident count by %.4f (%.1f%% change)\n",
  exp(coefs["victim_ln_gdp_pc"]),
  (exp(coefs["victim_ln_gdp_pc"]) - 1) * 100
))


################################################################################
#                 MODEL 1c-FE: NEGATIVE BINOMIAL + YEAR FIXED EFFECTS
################################################################################
# Robustness check: year FE absorb temporal shocks (e.g., Stuxnet discovery 2010,
# post-Snowden escalation 2013+, election interference wave 2016).
# If H1 survives year FE, the W4 effect is not driven by time trends.
#
# Note: convergence warnings on theta ("alternation limit reached") are expected
# with 99.95% zeros — this is the NB dispersion parameter struggling with extreme
# zero-inflation, not a model failure.

cat("\n\n============================================================\n")
cat("  Model 1c-FE: Negative Binomial + Year Fixed Effects\n")
cat("============================================================\n\n")

m1c_fe <- fenegbin(
  Incident_Count ~ attacker_w4 + victim_w4 +
    attacker_cinc + victim_cinc +
    attacker_ln_gdp_pc + victim_ln_gdp_pc | Year,
  data = df,
  vcov = ~directed_dyad_id
)

summary(m1c_fe)

# ---- Comparison: with vs without year FE ----
cat("\n--- Year FE Robustness Check ---\n")
coefs_fe <- coef(m1c_fe)
pvals_fe <- pvalue(m1c_fe)

cat(sprintf(
  "attacker_w4:  no FE = %.4f (p=%.2e)  |  year FE = %.4f (p=%.2e)  %s\n",
  coefs["attacker_w4"], pvals["attacker_w4"],
  coefs_fe["attacker_w4"], pvals_fe["attacker_w4"],
  ifelse(pvals_fe["attacker_w4"] < 0.05, "✓ survives", "✗ absorbed by time trends")
))

cat(sprintf(
  "victim_w4:    no FE = %.4f (p=%.2e)  |  year FE = %.4f (p=%.2e)\n",
  coefs["victim_w4"], pvals["victim_w4"],
  coefs_fe["victim_w4"], pvals_fe["victim_w4"]
))


################################################################################
#                 MODEL 2: LOGIT — H2 (likelihood of attack)
################################################################################
# H2: Attacker W4 is negatively associated with the LIKELIHOOD of conducting
#     cyber operations.
#     DV: has_attack (binary: 1 if Incident_Count > 0, else 0)
#     This tests a different quantity than H1: not "how many attacks" but
#     "whether any attack occurs at all". With 99.95% zeros, the extensive
#     margin (attack/no attack) may be more informative than the intensive
#     margin (count conditional on attacking).
#
# Same controls as Model 1c. Dyad-clustered SEs.

cat("\n\n============================================================\n")
cat("  Model 2: Logit — H2 (CINC + GDP controls)\n")
cat("============================================================\n\n")

m2 <- feglm(
  has_attack ~ attacker_w4 + victim_w4 +
    attacker_cinc + victim_cinc +
    attacker_ln_gdp_pc + victim_ln_gdp_pc,
  data = df,
  family = binomial(link = "logit"),
  vcov = ~directed_dyad_id
)

summary(m2)

# ---- Interpretation ----
cat("\n--- Model 2 Interpretation ---\n")
coefs_l <- coef(m2)
pvals_l <- pvalue(m2)

cat(sprintf(
  "attacker_w4:        coef = %.4f, p = %.2e  %s\n",
  coefs_l["attacker_w4"], pvals_l["attacker_w4"],
  ifelse(pvals_l["attacker_w4"] < 0.05,
    "→ H2 SUPPORTED: higher W4 reduces attack likelihood",
    "→ H2 not supported at p<0.05"
  )
))

cat(sprintf(
  "victim_w4:          coef = %.4f, p = %.2e\n",
  coefs_l["victim_w4"], pvals_l["victim_w4"]
))

# Odds ratios for substantive interpretation
cat("\n--- Odds Ratios (OR = exp(coef)) ---\n")
cat(sprintf(
  "attacker_w4 OR: %.4f — full autocracy→democracy multiplies odds of attack by %.4f\n",
  exp(coefs_l["attacker_w4"]), exp(coefs_l["attacker_w4"])
))
cat(sprintf(
  "victim_ln_gdp_pc OR: %.4f — 1 log-unit richer → %.1f%% change in odds of being targeted\n",
  exp(coefs_l["victim_ln_gdp_pc"]),
  (exp(coefs_l["victim_ln_gdp_pc"]) - 1) * 100
))


################################################################################
#                 MODEL 2-FE: LOGIT + YEAR FIXED EFFECTS
################################################################################
# Same robustness logic as Model 1c-FE.

cat("\n\n============================================================\n")
cat("  Model 2-FE: Logit + Year Fixed Effects\n")
cat("============================================================\n\n")

m2_fe <- feglm(
  has_attack ~ attacker_w4 + victim_w4 +
    attacker_cinc + victim_cinc +
    attacker_ln_gdp_pc + victim_ln_gdp_pc | Year,
  data = df,
  family = binomial(link = "logit"),
  vcov = ~directed_dyad_id
)

summary(m2_fe)

# ---- Comparison ----
cat("\n--- Year FE Robustness Check (Logit) ---\n")
coefs_l_fe <- coef(m2_fe)
pvals_l_fe <- pvalue(m2_fe)

cat(sprintf(
  "attacker_w4:  no FE = %.4f (p=%.2e)  |  year FE = %.4f (p=%.2e)  %s\n",
  coefs_l["attacker_w4"], pvals_l["attacker_w4"],
  coefs_l_fe["attacker_w4"], pvals_l_fe["attacker_w4"],
  ifelse(pvals_l_fe["attacker_w4"] < 0.05, "✓ survives", "✗ absorbed")
))


################################################################################
#                 SUMMARY TABLE
################################################################################

cat("\n\n============================================================\n")
cat("  Summary of All Models\n")
cat("============================================================\n\n")

etable(m1c, m1c_fe, m2, m2_fe,
  headers = c("NB", "NB + Year FE", "Logit", "Logit + Year FE"),
  se.below = TRUE,
  fitstat = ~ n + ll + aic + bic,
  dict = c(
    attacker_w4        = "Attacker W4",
    victim_w4          = "Victim W4",
    attacker_cinc      = "Attacker CINC",
    victim_cinc        = "Victim CINC",
    attacker_ln_gdp_pc = "Attacker ln(GDP p.c.)",
    victim_ln_gdp_pc   = "Victim ln(GDP p.c.)"
  )
)

cat("\nSEs clustered at dyad level (attacker_victim) throughout.\n")
cat("Year FE absorbed in FE columns (coefficients not shown).\n")
