##################### models_acd.R #############################################
# Active Cyber Defence: does W4 predict different TYPES of cyber operations?
#
# Theoretical basis (Adamson 2020):
#   Larger coalitions don't necessarily fight less — they fight for different
#   purposes. Republics fought more for public goods (defense); Empires fought
#   more for private goods (plunder). If this logic extends to cyber:
#     - Unprovoked aggression = private good (espionage, coercion, regime interests)
#     - Hack-back / retaliation = public good (defending the state after attack)
#   Prediction: W4 should strongly predict unprovoked aggression (autocracies
#   initiate more) but NOT hack-backs (democracies retaliate too).
#
# Depends on: data_preparation_acd.R output (df_model_acd.csv)
################################################################################

rm(list = ls())
options(scipen = 999)
cat("\014")

library(tidyverse)
library(fixest)

# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

##################### Load data ################################################

df <- read_csv("outputs/df_model_acd.csv")

df <- df %>%
    mutate(
        directed_dyad_id = paste(attacker, victim, sep = "_"),
        has_attack = as.integer(Incident_Count > 0),
        has_unprovoked = as.integer(Incident_Count_Unprovoked > 0),
        has_hackback = as.integer(Incident_Count_HackBack > 0)
    )

cat("============================================================\n")
cat("  Dataset Summary\n")
cat("============================================================\n")
cat("Observations:", nrow(df), "\n")
cat("Dyad-years with any incident:", sum(df$has_attack), "\n")
cat("  Unprovoked only:", sum(df$has_unprovoked & !df$has_hackback), "\n")
cat("  Hack-back only:", sum(!df$has_unprovoked & df$has_hackback), "\n")
cat("  Both in same dyad-year:", sum(df$has_unprovoked & df$has_hackback), "\n")
cat("Total incidents:", sum(df$Incident_Count), "\n")
cat("  Unprovoked:", sum(df$Incident_Count_Unprovoked), "\n")
cat("  Hack-back:", sum(df$Incident_Count_HackBack), "\n\n")


################################################################################
#   MODEL 3a: NEGATIVE BINOMIAL — UNPROVOKED AGGRESSION ONLY
################################################################################
# If the Adamson substitution logic holds in cyber, W4 should be a strong
# negative predictor of unprovoked aggression: autocracies initiate more
# offensive cyber operations that are NOT responses to prior attacks.
# This is the "private goods" channel — espionage, IP theft, coercion.
#
# Expect: attacker_w4 strongly negative, possibly stronger than in the
# combined model (Model 1c) because hack-backs dilute the signal.

cat("============================================================\n")
cat("  Model 3a: NB — Unprovoked Aggression Only\n")
cat("============================================================\n\n")

m3a <- fenegbin(
    Incident_Count_Unprovoked ~ attacker_w4 + victim_w4 +
        attacker_cinc + victim_cinc +
        attacker_ln_gdp_pc + victim_ln_gdp_pc,
    data = df,
    vcov = ~directed_dyad_id
)
summary(m3a)

coefs_3a <- coef(m3a)
pvals_3a <- pvalue(m3a)

cat("\n--- Model 3a Interpretation ---\n")
cat(sprintf(
    "attacker_w4: coef = %.4f, p = %.2e  %s\n",
    coefs_3a["attacker_w4"], pvals_3a["attacker_w4"],
    ifelse(pvals_3a["attacker_w4"] < 0.05,
        "→ W4 predicts unprovoked aggression",
        "→ NOT significant"
    )
))
cat(sprintf(
    "IRR: %.4f (%.1f%% change, full autocracy→democracy)\n",
    exp(coefs_3a["attacker_w4"]),
    (exp(coefs_3a["attacker_w4"]) - 1) * 100
))


################################################################################
#   MODEL 3b: NEGATIVE BINOMIAL — HACK-BACK ONLY
################################################################################
# The Adamson prediction: W4 should NOT predict hack-backs. Democracies
# retaliate after being attacked — this is a public good (national defense).
# The decision to respond is driven by the prior attack, not by regime type.
#
# Expect: attacker_w4 weak or non-significant.

cat("\n\n============================================================\n")
cat("  Model 3b: NB — Hack-Back Only\n")
cat("============================================================\n\n")

m3b <- fenegbin(
    Incident_Count_HackBack ~ attacker_w4 + victim_w4 +
        attacker_cinc + victim_cinc +
        attacker_ln_gdp_pc + victim_ln_gdp_pc,
    data = df,
    vcov = ~directed_dyad_id
)
summary(m3b)

coefs_3b <- coef(m3b)
pvals_3b <- pvalue(m3b)

cat("\n--- Model 3b Interpretation ---\n")
cat(sprintf(
    "attacker_w4: coef = %.4f, p = %.2e  %s\n",
    coefs_3b["attacker_w4"], pvals_3b["attacker_w4"],
    ifelse(pvals_3b["attacker_w4"] < 0.05,
        "→ W4 predicts hack-backs (unexpected)",
        "→ NOT significant — consistent with Adamson: retaliation is regime-neutral"
    )
))


################################################################################
#   MODEL 3a-FE / 3b-FE: YEAR FIXED EFFECTS ROBUSTNESS
################################################################################

cat("\n\n============================================================\n")
cat("  Robustness: Year Fixed Effects\n")
cat("============================================================\n\n")

m3a_fe <- fenegbin(
    Incident_Count_Unprovoked ~ attacker_w4 + victim_w4 +
        attacker_cinc + victim_cinc +
        attacker_ln_gdp_pc + victim_ln_gdp_pc | Year,
    data = df,
    vcov = ~directed_dyad_id
)

m3b_fe <- fenegbin(
    Incident_Count_HackBack ~ attacker_w4 + victim_w4 +
        attacker_cinc + victim_cinc +
        attacker_ln_gdp_pc + victim_ln_gdp_pc | Year,
    data = df,
    vcov = ~directed_dyad_id
)

coefs_3a_fe <- coef(m3a_fe)
pvals_3a_fe <- pvalue(m3a_fe)
coefs_3b_fe <- coef(m3b_fe)
pvals_3b_fe <- pvalue(m3b_fe)

cat("--- Unprovoked (3a) ---\n")
cat(sprintf(
    "attacker_w4:  no FE = %.4f (p=%.2e)  |  year FE = %.4f (p=%.2e)  %s\n",
    coefs_3a["attacker_w4"], pvals_3a["attacker_w4"],
    coefs_3a_fe["attacker_w4"], pvals_3a_fe["attacker_w4"],
    ifelse(pvals_3a_fe["attacker_w4"] < 0.05, "✓ survives", "✗ absorbed")
))

cat("--- Hack-back (3b) ---\n")
cat(sprintf(
    "attacker_w4:  no FE = %.4f (p=%.2e)  |  year FE = %.4f (p=%.2e)\n",
    coefs_3b["attacker_w4"], pvals_3b["attacker_w4"],
    coefs_3b_fe["attacker_w4"], pvals_3b_fe["attacker_w4"]
))


################################################################################
#   MODEL 4a/4b: LOGIT — BINARY SPLIT
################################################################################
# Extensive margin: does W4 predict WHETHER a state initiates each type?

cat("\n\n============================================================\n")
cat("  Model 4a: Logit — Unprovoked (binary)\n")
cat("============================================================\n\n")

m4a <- feglm(
    has_unprovoked ~ attacker_w4 + victim_w4 +
        attacker_cinc + victim_cinc +
        attacker_ln_gdp_pc + victim_ln_gdp_pc,
    data = df,
    family = binomial(link = "logit"),
    vcov = ~directed_dyad_id
)
summary(m4a)

cat("\n============================================================\n")
cat("  Model 4b: Logit — Hack-Back (binary)\n")
cat("============================================================\n\n")

m4b <- feglm(
    has_hackback ~ attacker_w4 + victim_w4 +
        attacker_cinc + victim_cinc +
        attacker_ln_gdp_pc + victim_ln_gdp_pc,
    data = df,
    family = binomial(link = "logit"),
    vcov = ~directed_dyad_id
)
summary(m4b)

coefs_4a <- coef(m4a)
pvals_4a <- pvalue(m4a)
coefs_4b <- coef(m4b)
pvals_4b <- pvalue(m4b)

cat("\n--- Logit Comparison ---\n")
cat(sprintf(
    "Unprovoked:  attacker_w4 = %.4f (p=%.2e)  OR = %.4f\n",
    coefs_4a["attacker_w4"], pvals_4a["attacker_w4"],
    exp(coefs_4a["attacker_w4"])
))
cat(sprintf(
    "Hack-back:   attacker_w4 = %.4f (p=%.2e)  OR = %.4f\n",
    coefs_4b["attacker_w4"], pvals_4b["attacker_w4"],
    exp(coefs_4b["attacker_w4"])
))


################################################################################
#   SUMMARY TABLE
################################################################################

cat("\n\n============================================================\n")
cat("  Summary: All ACD Models\n")
cat("============================================================\n\n")

etable(m3a, m3a_fe, m3b, m3b_fe, m4a, m4b,
    headers = c(
        "NB Unprov.", "NB Unprov.+FE",
        "NB HackBack", "NB HackBack+FE",
        "Logit Unprov.", "Logit HackBack"
    ),
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

cat("\nSEs clustered at directed dyad level throughout.\n")
cat("Year FE absorbed in FE columns.\n")
cat("2-year hack-back window used for primary specification.\n")


################################################################################
#   KEY PREDICTION CHECK
################################################################################

cat("\n\n============================================================\n")
cat("  Adamson (2020) Substitution Test\n")
cat("============================================================\n\n")
cat("Prediction: W4 should predict unprovoked aggression (private goods)\n")
cat("but NOT hack-backs (public goods / national defense).\n\n")

cat(sprintf(
    "Unprovoked NB:   attacker_w4 = %.4f, p = %.2e  %s\n",
    coefs_3a["attacker_w4"], pvals_3a["attacker_w4"],
    ifelse(pvals_3a["attacker_w4"] < 0.05, "✓ SIGNIFICANT", "✗ not significant")
))
cat(sprintf(
    "Hack-back NB:    attacker_w4 = %.4f, p = %.2e  %s\n",
    coefs_3b["attacker_w4"], pvals_3b["attacker_w4"],
    ifelse(pvals_3b["attacker_w4"] >= 0.05, "✓ NOT significant (as predicted)",
        "✗ significant (contradicts prediction)"
    )
))

cat("\nIf both checks pass: regime type determines the TYPE of cyber operations,\n")
cat("not just the quantity. Autocracies initiate more unprovoked aggression;\n")
cat("democracies retaliate at similar rates — the 'hack-back' is regime-neutral.\n")
cat("This mirrors Adamson's finding that Rome's Republic fought more for defense\n")
cat("while the Empire fought more for private gain.\n")
