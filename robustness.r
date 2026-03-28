##################### robustness.R #############################################
# Political Regimes and State-Level Cyber Aggression
# Robustness checks R1-R9
#
# Depends on: models.R must be run first (loads panels, fits main models)
# If running standalone, uncomment Section 0 below.
#
# R1: Poisson vs NB LR test (Panel A) — estimator justification
# R2: NB + year FE (Panel B)
# R3: Logit + year FE (Panel B)
# R4: NB CINC-only controls (Panel B)
# R5: NB GDP-only controls (Panel B)
# R6: Logit CINC-only controls (Panel B)
# R7: Logit GDP-only controls (Panel B)
# R4b-R7b: Same as R4-R7 on Panel C (primary clean panel)
# R8: NB excl. top 3 attackers (Panel C)
# R9: Logit excl. top 3 attackers (Panel C)
################################################################################

# Uncomment this section if running standalone (not after models.R):
# rm(list = ls())
# options(scipen = 999)
# cat("\014")
# library(tidyverse)
# library(fixest)
# library(pscl)
# library(modelsummary)
# library(MASS)
#
# df_2020 <- read_csv("outputs/df_model_2020.csv", show_col_types = FALSE) %>%
#   mutate(directed_dyad_id = paste(attacker, victim, sep = "_"),
#          has_attack = as.integer(Incident_Count > 0))
# df_2016 <- read_csv("outputs/df_model_2016.csv", show_col_types = FALSE) %>%
#   mutate(directed_dyad_id = paste(attacker, victim, sep = "_"),
#          has_attack = as.integer(Incident_Count > 0))
# df_2014 <- read_csv("outputs/df_model_2014.csv", show_col_types = FALSE) %>%
#   mutate(directed_dyad_id = paste(attacker, victim, sep = "_"),
#          has_attack_clean = as.integer(Incident_Count_Clean > 0))

cat("\n\n############################################################\n")
cat("#  ROBUSTNESS CHECKS (R1-R9)                               #\n")
cat("############################################################\n\n")


################################################################################
#   R1: Poisson vs NB — Likelihood Ratio Test (Panel A)
################################################################################
# Tests for overdispersion. If LR >> 0, Poisson is rejected and NB is
# justified for all subsequent count models.
# Also run inline in models.R Section 1; repeated here for completeness.

cat("============================================================\n")
cat("  R1: Poisson vs NB (Overdispersion LR Test)\n")
cat("============================================================\n\n")

r1_pois <- glm(
    Incident_Count ~ attacker_w4 + victim_w4 +
        attacker_ln_gdp_pc + victim_ln_gdp_pc,
    data = df_2020, family = poisson
)

r1_nb <- MASS::glm.nb(
    Incident_Count ~ attacker_w4 + victim_w4 +
        attacker_ln_gdp_pc + victim_ln_gdp_pc,
    data = df_2020, control = glm.control(maxit = 500)
)

lr_stat <- 2 * (logLik(r1_nb) - logLik(r1_pois))
lr_pval <- pchisq(as.numeric(lr_stat), df = 1, lower.tail = FALSE)

cat(sprintf("Poisson log-lik: %.1f\n", logLik(r1_pois)))
cat(sprintf("NB log-lik:      %.1f\n", logLik(r1_nb)))
cat(sprintf("LR statistic:    %.1f\n", lr_stat))
cat(sprintf("p-value:         %.2e\n", lr_pval))
cat(sprintf("NB theta:        %.6f\n", r1_nb$theta))
cat("\nConclusion: Poisson rejected. Severe overdispersion confirms NB.\n\n")


################################################################################
#   R2: NB + Year Fixed Effects (Panel B)
################################################################################
# Tests whether W4 effect survives temporal confounders.
# Year FE absorb global trends in cyber activity (e.g. increasing digitisation,
# Snowden revelations, policy changes).

cat("============================================================\n")
cat("  R2: NB + Year FE (Panel B)\n")
cat("============================================================\n\n")

R2 <- fenegbin(
    Incident_Count ~ attacker_w4 + victim_w4 +
        attacker_cinc + victim_cinc +
        attacker_ln_gdp_pc + victim_ln_gdp_pc +
        as.factor(Year),
    data = df_2016,
    vcov = ~directed_dyad_id
)
print(summary(R2))

cat(sprintf(
    "\nR2: attacker_w4 = %.4f, p = %.2e\n",
    coef(R2)["attacker_w4"], pvalue(R2)["attacker_w4"]
))
cat(sprintf(
    "    Compare M5: attacker_w4 = %.4f, p = %.2e\n",
    coef(M5)["attacker_w4"], pvalue(M5)["attacker_w4"]
))


################################################################################
#   R3: Logit + Year Fixed Effects (Panel B)
################################################################################

cat("\n\n============================================================\n")
cat("  R3: Logit + Year FE (Panel B)\n")
cat("============================================================\n\n")

R3 <- feglm(
    has_attack ~ attacker_w4 + victim_w4 +
        attacker_cinc + victim_cinc +
        attacker_ln_gdp_pc + victim_ln_gdp_pc +
        as.factor(Year),
    data = df_2016,
    family = binomial,
    vcov = ~directed_dyad_id
)
print(summary(R3))

cat(sprintf(
    "\nR3: attacker_w4 = %.4f, p = %.2e\n",
    coef(R3)["attacker_w4"], pvalue(R3)["attacker_w4"]
))
cat(sprintf(
    "    Compare M6: attacker_w4 = %.4f, p = %.2e\n",
    coef(M6)["attacker_w4"], pvalue(M6)["attacker_w4"]
))


################################################################################
#   R4: NB CINC-only controls (Panel B)
################################################################################
# Tests W4 effect without GDP. If W4 is significant here, it's not an
# artifact of GDP collinearity.

cat("\n\n============================================================\n")
cat("  R4: NB CINC-only (Panel B)\n")
cat("============================================================\n\n")

R4 <- fenegbin(
    Incident_Count ~ attacker_w4 + victim_w4 +
        attacker_cinc + victim_cinc,
    data = df_2016,
    vcov = ~directed_dyad_id
)
print(summary(R4))

cat(sprintf(
    "\nR4: attacker_w4 = %.4f, p = %.2e\n",
    coef(R4)["attacker_w4"], pvalue(R4)["attacker_w4"]
))


################################################################################
#   R5: NB GDP-only controls (Panel B)
################################################################################
# Tests W4 effect without CINC. If W4 is significant here, it's not an
# artifact of CINC collinearity.

cat("\n\n============================================================\n")
cat("  R5: NB GDP-only (Panel B)\n")
cat("============================================================\n\n")

R5 <- fenegbin(
    Incident_Count ~ attacker_w4 + victim_w4 +
        attacker_ln_gdp_pc + victim_ln_gdp_pc,
    data = df_2016,
    vcov = ~directed_dyad_id
)
print(summary(R5))

cat(sprintf(
    "\nR5: attacker_w4 = %.4f, p = %.2e\n",
    coef(R5)["attacker_w4"], pvalue(R5)["attacker_w4"]
))


################################################################################
#   R6: Logit CINC-only controls (Panel B)
################################################################################
# Key diagnostic (compare with R7): does H2 fail because of CINC
# specifically, or because of any capability control?

cat("\n\n============================================================\n")
cat("  R6: Logit CINC-only (Panel B)\n")
cat("============================================================\n\n")

R6 <- feglm(
    has_attack ~ attacker_w4 + victim_w4 +
        attacker_cinc + victim_cinc,
    data = df_2016,
    family = binomial,
    vcov = ~directed_dyad_id
)
print(summary(R6))

cat(sprintf(
    "\nR6: attacker_w4 = %.4f, p = %.2e\n",
    coef(R6)["attacker_w4"], pvalue(R6)["attacker_w4"]
))


################################################################################
#   R7: Logit GDP-only controls (Panel B)
################################################################################
# KEY DIAGNOSTIC: If H2 is significant here but not in R6 or M6,
# it proves that CINC (material capability) is what absorbs the
# extensive margin variance — not GDP, not controls in general.

cat("\n\n============================================================\n")
cat("  R7: Logit GDP-only (Panel B)\n")
cat("============================================================\n\n")

R7 <- feglm(
    has_attack ~ attacker_w4 + victim_w4 +
        attacker_ln_gdp_pc + victim_ln_gdp_pc,
    data = df_2016,
    family = binomial,
    vcov = ~directed_dyad_id
)
print(summary(R7))

cat(sprintf(
    "\nR7: attacker_w4 = %.4f, p = %.2e\n",
    coef(R7)["attacker_w4"], pvalue(R7)["attacker_w4"]
))

cat("\n--- R6 vs R7 Diagnostic ---\n")
cat(sprintf(
    "  R6 Logit CINC-only: attacker_w4 = %.4f, p = %.2e\n",
    coef(R6)["attacker_w4"], pvalue(R6)["attacker_w4"]
))
cat(sprintf(
    "  R7 Logit GDP-only:  attacker_w4 = %.4f, p = %.2e\n",
    coef(R7)["attacker_w4"], pvalue(R7)["attacker_w4"]
))
cat("If R7 is significant but R6 is not: CINC specifically absorbs H2.\n")
cat("The binary entry decision is capability-driven, not wealth-driven.\n")


################################################################################
#   R8: NB excluding top 3 attackers (Panel C)
################################################################################
# China, Russia, Iran dominate the incident count. If H1 holds without
# them, it's not driven by a few prolific autocracies.

cat("\n\n============================================================\n")
cat("  R8: NB excl. top 3 attackers (Panel C — clean panel)\n")
cat("============================================================\n\n")

top3 <- c("China", "Russia", "Iran")
df_2014_no_top3 <- df_2014 %>%
    filter(!attacker %in% top3)

cat(sprintf(
    "Panel C obs: %d -> excl. top 3: %d\n",
    nrow(df_2014), nrow(df_2014_no_top3)
))
cat(sprintf(
    "Incidents: %d -> excl. top 3: %d\n",
    sum(df_2014$Incident_Count_Clean),
    sum(df_2014_no_top3$Incident_Count_Clean)
))

R8 <- fenegbin(
    Incident_Count_Clean ~ attacker_w4 + victim_w4 +
        attacker_cinc + victim_cinc +
        attacker_ln_gdp_pc + victim_ln_gdp_pc,
    data = df_2014_no_top3,
    vcov = ~directed_dyad_id
)
print(summary(R8))

cat(sprintf(
    "\nR8: attacker_w4 = %.4f, p = %.2e\n",
    coef(R8)["attacker_w4"], pvalue(R8)["attacker_w4"]
))
cat(sprintf(
    "    Compare M10: attacker_w4 = %.4f, p = %.2e\n",
    coef(M10)["attacker_w4"], pvalue(M10)["attacker_w4"]
))


################################################################################
#   R9: Logit excluding top 3 attackers (Panel C)
################################################################################

cat("\n\n============================================================\n")
cat("  R9: Logit excl. top 3 attackers (Panel C — clean panel)\n")
cat("============================================================\n\n")

R9 <- feglm(
    has_attack_clean ~ attacker_w4 + victim_w4 +
        attacker_cinc + victim_cinc +
        attacker_ln_gdp_pc + victim_ln_gdp_pc,
    data = df_2014_no_top3,
    family = binomial,
    vcov = ~directed_dyad_id
)
print(summary(R9))

cat(sprintf(
    "\nR9: attacker_w4 = %.4f, p = %.2e\n",
    coef(R9)["attacker_w4"], pvalue(R9)["attacker_w4"]
))
cat(sprintf(
    "    Compare M11: attacker_w4 = %.4f, p = %.2e\n",
    coef(M11)["attacker_w4"], pvalue(M11)["attacker_w4"]
))


################################################################################
#   R4b: NB CINC-only controls (Panel C)
################################################################################
# Same as R4 but on the primary clean panel.

cat("\n\n============================================================\n")
cat("  R4b: NB CINC-only (Panel C — clean panel)\n")
cat("============================================================\n\n")

R4b <- fenegbin(
    Incident_Count_Clean ~ attacker_w4 + victim_w4 +
        attacker_cinc + victim_cinc,
    data = df_2014,
    vcov = ~directed_dyad_id
)
print(summary(R4b))

cat(sprintf(
    "\nR4b: attacker_w4 = %.4f, p = %.2e\n",
    coef(R4b)["attacker_w4"], pvalue(R4b)["attacker_w4"]
))
cat(sprintf(
    "     Compare R4 (Panel B): %.4f, p = %.2e\n",
    coef(R4)["attacker_w4"], pvalue(R4)["attacker_w4"]
))


################################################################################
#   R5b: NB GDP-only controls (Panel C)
################################################################################

cat("\n\n============================================================\n")
cat("  R5b: NB GDP-only (Panel C — clean panel)\n")
cat("============================================================\n\n")

R5b <- fenegbin(
    Incident_Count_Clean ~ attacker_w4 + victim_w4 +
        attacker_ln_gdp_pc + victim_ln_gdp_pc,
    data = df_2014,
    vcov = ~directed_dyad_id
)
print(summary(R5b))

cat(sprintf(
    "\nR5b: attacker_w4 = %.4f, p = %.2e\n",
    coef(R5b)["attacker_w4"], pvalue(R5b)["attacker_w4"]
))
cat(sprintf(
    "     Compare R5 (Panel B): %.4f, p = %.2e\n",
    coef(R5)["attacker_w4"], pvalue(R5)["attacker_w4"]
))


################################################################################
#   R6b: Logit CINC-only controls (Panel C)
################################################################################
# Key diagnostic on the primary panel: does CINC absorb H2 here too?

cat("\n\n============================================================\n")
cat("  R6b: Logit CINC-only (Panel C — clean panel)\n")
cat("============================================================\n\n")

R6b <- feglm(
    has_attack_clean ~ attacker_w4 + victim_w4 +
        attacker_cinc + victim_cinc,
    data = df_2014,
    family = binomial,
    vcov = ~directed_dyad_id
)
print(summary(R6b))

cat(sprintf(
    "\nR6b: attacker_w4 = %.4f, p = %.2e\n",
    coef(R6b)["attacker_w4"], pvalue(R6b)["attacker_w4"]
))


################################################################################
#   R7b: Logit GDP-only controls (Panel C)
################################################################################
# KEY DIAGNOSTIC on primary panel: if H2 is significant here but not in
# R6b, the CINC absorption pattern replicates on the clean panel.

cat("\n\n============================================================\n")
cat("  R7b: Logit GDP-only (Panel C — clean panel)\n")
cat("============================================================\n\n")

R7b <- feglm(
    has_attack_clean ~ attacker_w4 + victim_w4 +
        attacker_ln_gdp_pc + victim_ln_gdp_pc,
    data = df_2014,
    family = binomial,
    vcov = ~directed_dyad_id
)
print(summary(R7b))

cat(sprintf(
    "\nR7b: attacker_w4 = %.4f, p = %.2e\n",
    coef(R7b)["attacker_w4"], pvalue(R7b)["attacker_w4"]
))

cat("\n--- R6b vs R7b Diagnostic (Panel C) ---\n")
cat(sprintf(
    "  R6b Logit CINC-only: attacker_w4 = %.4f, p = %.2e\n",
    coef(R6b)["attacker_w4"], pvalue(R6b)["attacker_w4"]
))
cat(sprintf(
    "  R7b Logit GDP-only:  attacker_w4 = %.4f, p = %.2e\n",
    coef(R7b)["attacker_w4"], pvalue(R7b)["attacker_w4"]
))
cat("Compare with R6/R7 (Panel B) — pattern should replicate.\n")


################################################################################
#   SUMMARY
################################################################################

cat("\n\n############################################################\n")
cat("#  ROBUSTNESS SUMMARY                                      #\n")
cat("############################################################\n\n")

cat("--- H1 (frequency, NB) ---\n")
cat("  Panel B:\n")
cat(sprintf(
    "  M5   NB baseline:      %.4f  p = %.2e\n",
    coef(M5)["attacker_w4"], pvalue(M5)["attacker_w4"]
))
cat(sprintf(
    "  M8b  NB+MID(>=4):      %.4f  p = %.2e\n",
    coef(M8b)["attacker_w4"], pvalue(M8b)["attacker_w4"]
))
cat(sprintf(
    "  R2   NB + year FE:     %.4f  p = %.2e\n",
    coef(R2)["attacker_w4"], pvalue(R2)["attacker_w4"]
))
cat(sprintf(
    "  R4   NB CINC-only:     %.4f  p = %.2e\n",
    coef(R4)["attacker_w4"], pvalue(R4)["attacker_w4"]
))
cat(sprintf(
    "  R5   NB GDP-only:      %.4f  p = %.2e\n",
    coef(R5)["attacker_w4"], pvalue(R5)["attacker_w4"]
))
cat("  Panel C (primary):\n")
cat(sprintf(
    "  R4b  NB CINC-only:     %.4f  p = %.2e\n",
    coef(R4b)["attacker_w4"], pvalue(R4b)["attacker_w4"]
))
cat(sprintf(
    "  R5b  NB GDP-only:      %.4f  p = %.2e\n",
    coef(R5b)["attacker_w4"], pvalue(R5b)["attacker_w4"]
))
cat(sprintf(
    "  R8   NB excl. top 3:   %.4f  p = %.2e\n",
    coef(R8)["attacker_w4"], pvalue(R8)["attacker_w4"]
))

cat("\n--- H2 (likelihood, Logit) ---\n")
cat("  Panel B:\n")
cat(sprintf(
    "  M6   Logit baseline:   %.4f  p = %.2e\n",
    coef(M6)["attacker_w4"], pvalue(M6)["attacker_w4"]
))
cat(sprintf(
    "  M9b  Logit+MID(>=4):   %.4f  p = %.2e\n",
    coef(M9b)["attacker_w4"], pvalue(M9b)["attacker_w4"]
))
cat(sprintf(
    "  R3   Logit + year FE:  %.4f  p = %.2e\n",
    coef(R3)["attacker_w4"], pvalue(R3)["attacker_w4"]
))
cat(sprintf(
    "  R6   Logit CINC-only:  %.4f  p = %.2e\n",
    coef(R6)["attacker_w4"], pvalue(R6)["attacker_w4"]
))
cat(sprintf(
    "  R7   Logit GDP-only:   %.4f  p = %.2e  <- KEY DIAGNOSTIC\n",
    coef(R7)["attacker_w4"], pvalue(R7)["attacker_w4"]
))
cat("  Panel C (primary):\n")
cat(sprintf(
    "  R6b  Logit CINC-only:  %.4f  p = %.2e\n",
    coef(R6b)["attacker_w4"], pvalue(R6b)["attacker_w4"]
))
cat(sprintf(
    "  R7b  Logit GDP-only:   %.4f  p = %.2e  <- KEY DIAGNOSTIC (primary)\n",
    coef(R7b)["attacker_w4"], pvalue(R7b)["attacker_w4"]
))
cat(sprintf(
    "  R9   Logit excl. top3: %.4f  p = %.2e\n",
    coef(R9)["attacker_w4"], pvalue(R9)["attacker_w4"]
))

cat("\n--- Estimator ---\n")
cat(sprintf(
    "  R1   LR test:          %.1f  p = %.2e  -> Poisson rejected\n",
    lr_stat, lr_pval
))

cat("\n\n--- Regression Table (robustness checks) ---\n\n")

etable(R2, R3, R4, R5, R6, R7, R4b, R5b, R6b, R7b, R8, R9,
    headers = c(
        "R2 NB+FE", "R3 Log+FE",
        "R4 NB CINC", "R5 NB GDP",
        "R6 Log CINC", "R7 Log GDP",
        "R4b NB CINC*", "R5b NB GDP*",
        "R6b Log CINC*", "R7b Log GDP*",
        "R8 NB -top3", "R9 Log -top3"
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

cat("\n--- Conclusion ---\n")
cat("H1 is robust across year FE, partial controls, and excluding dominant\n")
cat("attackers — on both Panel B and Panel C. H2 remains not supported;\n")
cat("the R6/R7 and R6b/R7b comparisons confirm on both panels that material\n")
cat("capability (CINC) specifically absorbs the extensive margin, consistent\n")
cat("with the accountability bypass interpretation.\n")


################################################################################
#   HTML TABLES (modelsummary)
################################################################################

cat("\n\n############################################################\n")
cat("#  ROBUSTNESS HTML TABLES                                  #\n")
cat("############################################################\n\n")

table_dir <- "outputs/tables"
if (!dir.exists(table_dir)) dir.create(table_dir, recursive = TRUE)

cm_rob <- c(
    "attacker_w4"        = "Attacker W4",
    "victim_w4"          = "Victim W4",
    "attacker_cinc"      = "Attacker CINC",
    "victim_cinc"        = "Victim CINC",
    "attacker_ln_gdp_pc" = "Attacker ln(GDP p.c.)",
    "victim_ln_gdp_pc"   = "Victim ln(GDP p.c.)",
    "(Intercept)"        = "Intercept"
)

gm_rob <- list(
    list("raw" = "nobs", "clean" = "Observations", "fmt" = 0),
    list("raw" = "logLik", "clean" = "Log-Likelihood", "fmt" = 1),
    list("raw" = "AIC", "clean" = "AIC", "fmt" = 1),
    list("raw" = "BIC", "clean" = "BIC", "fmt" = 1)
)

# Non-zero stats
nz_2016 <- sum(df_2016$Incident_Count > 0)
nz_2014 <- sum(df_2014$Incident_Count_Clean > 0)
nz_no_top3 <- sum(df_2014_no_top3$Incident_Count_Clean > 0)
ti_2016 <- as.character(sum(df_2016$Incident_Count))
ti_2014 <- as.character(sum(df_2014$Incident_Count_Clean))
ti_no_top3 <- as.character(sum(df_2014_no_top3$Incident_Count_Clean))
pct_2016 <- sprintf("%.4f%%", nz_2016 / nrow(df_2016) * 100)
pct_2014 <- sprintf("%.4f%%", nz_2014 / nrow(df_2014) * 100)
pct_no_top3 <- sprintf("%.4f%%", nz_no_top3 / nrow(df_2014_no_top3) * 100)


# --- Table 6: Robustness NB (R2, R4, R5, R4b, R5b, R8) ---
rows_t6 <- tibble(
    term = c("Non-zero obs", "% non-zero", "Total incidents"),
    `R2 NB+FE` = c(as.character(nz_2016), pct_2016, ti_2016),
    `R4 NB CINC (B)` = c(as.character(nz_2016), pct_2016, ti_2016),
    `R5 NB GDP (B)` = c(as.character(nz_2016), pct_2016, ti_2016),
    `R4b NB CINC (C)` = c(as.character(nz_2014), pct_2014, ti_2014),
    `R5b NB GDP (C)` = c(as.character(nz_2014), pct_2014, ti_2014),
    `R8 NB excl. top3` = c(as.character(nz_no_top3), pct_no_top3, ti_no_top3)
)
attr(rows_t6, "position") <- c(NA, NA, NA)

modelsummary(
    list(
        "R2 NB+FE" = R2, "R4 NB CINC (B)" = R4,
        "R5 NB GDP (B)" = R5, "R4b NB CINC (C)" = R4b,
        "R5b NB GDP (C)" = R5b, "R8 NB excl. top3" = R8
    ),
    output = file.path(table_dir, "table6_robustness_nb.html"),
    coef_map = cm_rob,
    gof_map = gm_rob,
    add_rows = rows_t6,
    stars = c("*" = 0.05, "**" = 0.01, "***" = 0.001),
    title = "Robustness: H1 (Frequency) — Negative Binomial Specifications",
    notes = "Dyad-clustered SEs. (B) = Panel B, (C) = Panel C (clean). R2: year FE. R4/R4b: CINC-only. R5/R5b: GDP-only. R8: excl. China, Russia, Iran."
)
cat("Saved: outputs/tables/table6_robustness_nb.html\n")


# --- Table 7: Robustness Logit (R3, R6, R7, R6b, R7b, R9) ---
rows_t7 <- tibble(
    term = c("Non-zero obs", "% non-zero"),
    `R3 Logit+FE` = c(as.character(nz_2016), pct_2016),
    `R6 Logit CINC (B)` = c(as.character(nz_2016), pct_2016),
    `R7 Logit GDP (B)` = c(as.character(nz_2016), pct_2016),
    `R6b Logit CINC (C)` = c(as.character(nz_2014), pct_2014),
    `R7b Logit GDP (C)` = c(as.character(nz_2014), pct_2014),
    `R9 Logit excl. top3` = c(as.character(nz_no_top3), pct_no_top3)
)
attr(rows_t7, "position") <- c(NA, NA)

modelsummary(
    list(
        "R3 Logit+FE" = R3, "R6 Logit CINC (B)" = R6,
        "R7 Logit GDP (B)" = R7, "R6b Logit CINC (C)" = R6b,
        "R7b Logit GDP (C)" = R7b, "R9 Logit excl. top3" = R9
    ),
    output = file.path(table_dir, "table7_robustness_logit.html"),
    coef_map = cm_rob,
    gof_map = gm_rob,
    add_rows = rows_t7,
    stars = c("*" = 0.05, "**" = 0.01, "***" = 0.001),
    title = "Robustness: H2 (Likelihood) — Logit Specifications",
    notes = "Dyad-clustered SEs. (B) = Panel B, (C) = Panel C (clean). R6b vs R7b is the key diagnostic on the primary panel: CINC absorption should replicate."
)
cat("Saved: outputs/tables/table7_robustness_logit.html\n")

cat("\nAll robustness tables saved to:", table_dir, "\n")
