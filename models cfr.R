##################### models_cfr.R #############################################
# H1 robustness on CFR Cyber Operations Tracker (monadic panel).
#
# Hypothesis tested:
#   H1: The larger the winning coalition index, the lower the frequency of
#       conducting unprovoked state-backed cyber operations.
#       Read attacker_w4 across all four CFR models.
#
# H2 is NOT tested here — CFR does not record victim states, so the panel is
# country-year (monadic), not directed-dyad-year. CFR serves only to corroborate
# the attacker-side finding from DCID under a different sourcing methodology.
#
# Specification mapping to the DCID models (models.R):
#   M1 (CFR) ↔ M1 (DCID)  : NB + Year FE on broadest panel (W4 + GDP)
#   M2 (CFR) ↔ M2 (DCID)  : NB + Year FE with full controls (W4 + CINC + GDP)
#   M3 (CFR) ↔ M3 (DCID)  : ZINB + Year FE in count stage
#   M4 (CFR) ↔ M7 (DCID)  : ZINB + Year FE in BOTH stages — PRIMARY CFR SPEC
#
# CFR has no analog to the DCID Panel C ("clean DV") because the kinetic-
# conflict exclusion is dyadic. CFR also has no MID-augmented model because
# MID is naturally dyadic.
#
# Standard errors:
#   - NB models: clustered at the attacker (country) level via fixest.
#   - ZINB models: model-based SEs (pscl does not support clustering).
################################################################################

rm(list = ls())
options(scipen = 999)
cat("\014")

library(tidyverse)
library(fixest)
library(pscl)
library(modelsummary)

# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


################################################################################
#                 SECTION 0: DATA LOADING
################################################################################

cat("############################################################\n")
cat("#  SECTION 0: DATA LOADING (CFR monadic panels)            #\n")
cat("############################################################\n\n")

df_2020 <- read_csv("outputs/df_attacker_2020.csv", show_col_types = FALSE)
df_2016 <- read_csv("outputs/df_attacker_2016.csv", show_col_types = FALSE)

cat(sprintf(
    "--- Panel A (2020): %d country-years, %d incidents, %d non-zero (%.2f%%)\n",
    nrow(df_2020), sum(df_2020$Incident_Count),
    sum(df_2020$Incident_Count > 0),
    100 * mean(df_2020$Incident_Count > 0)
))

cat(sprintf(
    "--- Panel B (2016): %d country-years, %d incidents, %d non-zero (%.2f%%)\n\n",
    nrow(df_2016), sum(df_2016$Incident_Count),
    sum(df_2016$Incident_Count > 0),
    100 * mean(df_2016$Incident_Count > 0)
))


################################################################################
#                 SECTION 1: PANEL A — M1
#                 (W4 + GDP + Year FE, 2007-2020)
################################################################################

cat("############################################################\n")
cat("#  SECTION 1: PANEL A — M1 (CFR)                           #\n")
cat("############################################################\n\n")


# --- M1: NB + Year FE ---
cat("--- M1 (CFR): NB + Year FE (broadest panel, GDP only) ---\n\n")

M1 <- fenegbin(
    Incident_Count ~ attacker_w4 + attacker_ln_gdp_pc | Year,
    data = df_2020,
    vcov = ~attacker
)
print(summary(M1))

cat(sprintf(
    "\nM1 (CFR): attacker_w4 = %.4f, p = %.2e, IRR = %.4f  [H1]\n",
    coef(M1)["attacker_w4"], pvalue(M1)["attacker_w4"],
    exp(coef(M1)["attacker_w4"])
))


################################################################################
#                 SECTION 2: PANEL B — M2, M3, M4 (PRIMARY)
#                 (W4 + CINC + GDP + Year FE, 2007-2016)
################################################################################

cat("############################################################\n")
cat("#  SECTION 2: PANEL B — M2, M3, M4 (CFR)                   #\n")
cat("############################################################\n\n")


# --- M2: NB + Year FE (full controls) ---
cat("--- M2 (CFR): NB + Year FE (full controls) ---\n\n")

M2 <- fenegbin(
    Incident_Count ~ attacker_w4 + attacker_cinc + attacker_ln_gdp_pc | Year,
    data = df_2016,
    vcov = ~attacker
)
print(summary(M2))

cat(sprintf(
    "\nM2 (CFR): attacker_w4 = %.4f, p = %.2e, IRR = %.4f  [H1]\n",
    coef(M2)["attacker_w4"], pvalue(M2)["attacker_w4"],
    exp(coef(M2)["attacker_w4"])
))


# --- M3: ZINB + Year FE in count stage ---
# Inflation stage: attacker_cinc only (capability determines structural zeros).
cat("\n\n--- M3 (CFR): ZINB + Year FE in count stage ---\n\n")

M3 <- zeroinfl(
    Incident_Count ~ attacker_w4 + attacker_cinc + attacker_ln_gdp_pc + as.factor(Year) |
        attacker_cinc,
    data = df_2016,
    dist = "negbin"
)
print(summary(M3))

zinb_M3_coefs <- coef(M3, "count")
zinb_M3_pvals <- summary(M3)$coefficients$count[, "Pr(>|z|)"]
cat(sprintf(
    "\nM3 (CFR) count: attacker_w4 = %.4f, p = %.2e  [H1]\n",
    zinb_M3_coefs["attacker_w4"], zinb_M3_pvals["attacker_w4"]
))


# --- M4: ZINB + Year FE in BOTH stages (PRIMARY CFR SPEC) ---
# Mirrors M7 in models.R: temporal FE applied symmetrically to extensive and
# intensive margins.
cat("\n\n--- M4 (CFR): ZINB + Year FE in BOTH stages — PRIMARY ---\n\n")

M4 <- zeroinfl(
    Incident_Count ~ attacker_w4 + attacker_cinc + attacker_ln_gdp_pc + as.factor(Year) |
        attacker_cinc + as.factor(Year),
    data = df_2016,
    dist = "negbin"
)
print(summary(M4))

zinb_M4_coefs <- coef(M4, "count")
zinb_M4_pvals <- summary(M4)$coefficients$count[, "Pr(>|z|)"]
cat(sprintf(
    "\nM4 (CFR) count: attacker_w4 = %.4f, p = %.2e  [H1, PRIMARY CFR]\n",
    zinb_M4_coefs["attacker_w4"], zinb_M4_pvals["attacker_w4"]
))


################################################################################
#                 SECTION 3: CROSS-SPEC SUMMARY
################################################################################

cat("\n\n--- Cross-Spec Summary (CFR M1-M4) ---\n")
cat(sprintf(
    "  M1 NB        (A):     attacker_w4 = %+.4f (p=%.2e)\n",
    coef(M1)["attacker_w4"], pvalue(M1)["attacker_w4"]
))
cat(sprintf(
    "  M2 NB        (B):     attacker_w4 = %+.4f (p=%.2e)\n",
    coef(M2)["attacker_w4"], pvalue(M2)["attacker_w4"]
))
cat(sprintf(
    "  M3 ZINB      (B):     attacker_w4 = %+.4f (p=%.2e)\n",
    zinb_M3_coefs["attacker_w4"], zinb_M3_pvals["attacker_w4"]
))
cat(sprintf(
    "  M4 ZINB+FE   (B):     attacker_w4 = %+.4f (p=%.2e)  [PRIMARY CFR]\n",
    zinb_M4_coefs["attacker_w4"], zinb_M4_pvals["attacker_w4"]
))


################################################################################
#                 SECTION 4: SUMMARY TABLE
################################################################################

cat("\n\n############################################################\n")
cat("#  SECTION 4: SUMMARY TABLE (CFR)                          #\n")
cat("############################################################\n\n")

table_dir <- "outputs/tables"
if (!dir.exists(table_dir)) dir.create(table_dir, recursive = TRUE)


# --- Helpers ---
pr2 <- function(model, null_ll) {
    if (inherits(model, "fixest")) {
        sprintf("%.4f", 1 - as.numeric(logLik(model)) / null_ll)
    } else {
        "\u2014"
    }
}

null_2020 <- as.numeric(logLik(fenegbin(Incident_Count ~ 1, data = df_2020)))
null_2016 <- as.numeric(logLik(fenegbin(Incident_Count ~ 1, data = df_2016)))

nz_2020 <- sum(df_2020$Incident_Count > 0)
nz_2016 <- sum(df_2016$Incident_Count > 0)

ti_2020 <- as.character(sum(df_2020$Incident_Count))
ti_2016 <- as.character(sum(df_2016$Incident_Count))

pct_2020 <- sprintf("%.2f%%", 100 * nz_2020 / nrow(df_2020))
pct_2016 <- sprintf("%.2f%%", 100 * nz_2016 / nrow(df_2016))


# --- Coefficient map ---
cm <- c(
    "attacker_w4"              = "Attacker W4 [H1]",
    "count_attacker_w4"        = "Attacker W4 [H1]",
    "attacker_cinc"            = "Attacker CINC",
    "count_attacker_cinc"      = "Attacker CINC",
    "attacker_ln_gdp_pc"       = "Attacker ln(GDP p.c.)",
    "count_attacker_ln_gdp_pc" = "Attacker ln(GDP p.c.)",
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


# --- Main CFR table ---
rows_cfr <- tibble(
    term = c(
        "Year FE", "Non-zero obs", "% non-zero", "Total incidents",
        "McFadden pseudo-R\u00B2"
    ),
    `M1 NB (A)` = c("Yes", as.character(nz_2020), pct_2020, ti_2020, pr2(M1, null_2020)),
    `M2 NB (B)` = c("Yes", as.character(nz_2016), pct_2016, ti_2016, pr2(M2, null_2016)),
    `M3 ZINB (B)` = c("Yes (count)", as.character(nz_2016), pct_2016, ti_2016, "\u2014"),
    `M4 ZINB+FE (B)` = c("Yes (both)", as.character(nz_2016), pct_2016, ti_2016, "\u2014")
)
attr(rows_cfr, "position") <- c(NA, NA, NA, NA, NA)

modelsummary(
    list(
        "M1 NB (A)"      = M1,
        "M2 NB (B)"      = M2,
        "M3 ZINB (B)"    = M3,
        "M4 ZINB+FE (B)" = M4
    ),
    output = file.path(table_dir, "table_cfr.html"),
    coef_map = cm,
    gof_map = gm,
    add_rows = rows_cfr,
    stars = c("*" = 0.05, "**" = 0.01, "***" = 0.001),
    title = "CFR Robustness: Winning Coalition Index and Cyber Operations (H1 only)",
    notes = paste(
        "Monadic country-year panel built from CFR Cyber Operations Tracker.",
        "All models include year fixed effects: in the linear predictor for NB",
        "models (M1, M2); in the count stage for M3; in both count and inflation",
        "stages for M4. Country-clustered SEs for NB models; model-based SEs for",
        "ZINB models. Multi-sponsor incidents expanded to one row per sponsor.",
        "H2 is not tested on CFR (no victim information). M4 is the primary CFR",
        "specification, mirroring M7 in the DCID main analysis."
    )
)
cat("Saved: outputs/tables/table_cfr.html\n")

cat("\nDone. CFR robustness for H1: read attacker_w4 from M4 (primary).\n")
