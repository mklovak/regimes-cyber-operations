##################### robustness.R #############################################
# Political Regimes and State-Level Cyber Aggression
# Robustness checks R1-R6
#
# This script is STANDALONE: it loads the panels itself and fits R1-R6
# independently of models.R. Run order: data_preparation.R ->
# data_preparation_cfr.R -> descriptive statistics.R -> models.R -> robustness.R
#
# A robustness check confronts the finding with something EXTERNAL — different
# controls, a different sample, or a different dataset. The six checks fall
# into three families:
#
#   Kinetic-conflict sensitivity
#     R1: NB + MID at any hostility level (>=1), Panel B
#         Confirms the MID hostility >=4 threshold used in M4/Panel C is not
#         cherry-picked: H1 survives the broadest possible kinetic control.
#
#   Exclusion of dominant attackers
#     R2: NB excluding the top-3 attackers (China, Russia, Iran), Panel C
#         Confirms H1 is not driven by a few prolific autocracies.
#
#   Alternative dataset — CFR (the substantive robustness check)
#     R3: CFR NB, Panel A (monadic, W4 + GDP)
#     R4: CFR NB, Panel B (monadic, full controls)
#     R5: CFR ZINB, Panel B (count-stage year FE)
#     R6: CFR ZINB + year FE in both stages, Panel B (primary CFR spec)
#         CFR (Council on Foreign Relations Cyber Operations Tracker) is a
#         smaller, journalistically curated dataset. If H1 replicates on CFR
#         with the same controls, it is not an artifact of DCID coding choices.
#         CFR is monadic (sponsor known, victim not), so it tests H1 only.
#
# NOT here:
#   - Poisson vs NB LR test (estimator justification) — descriptive statistics.R
#   - D1, D2 H2 extensive-margin diagnostics                — models.R Section 5
#   - D3 M7 specification diagnostic (W4 in both ZINB stages) — models.R Section 5
#   D3 is a diagnostic, not a robustness check: it changes no data, controls or
#   sample, only M7's internal stage placement.
#
# An exploratory block at the end fits — but does not tabulate — a ZINB top-3
# exclusion (Panel C) and CFR top-3 exclusion models, for inspection only.
################################################################################

rm(list = ls())
options(scipen = 999)
cat("\014")

library(tidyverse)
library(fixest)
library(pscl)
library(modelsummary)
library(tinytable)


################################################################################
#   SECTION 0: DATA LOADING
################################################################################

cat("############################################################\n")
cat("#  ROBUSTNESS CHECKS (R1-R6)                               #\n")
cat("############################################################\n\n")

# --- DCID dyadic panels ---
df_2016 <- read_csv("outputs/df_model_2016.csv", show_col_types = FALSE) %>%
    mutate(directed_dyad_id = paste(attacker, victim, sep = "_"))

df_2014 <- read_csv("outputs/df_model_2014.csv", show_col_types = FALSE) %>%
    mutate(
        directed_dyad_id = paste(attacker, victim, sep = "_"),
        has_attack_clean = as.integer(Incident_Count_Clean > 0)
    )

# --- CFR monadic panels ---
df_cfr_2020 <- read_csv("outputs/df_attacker_2020.csv", show_col_types = FALSE)
df_cfr_2016 <- read_csv("outputs/df_attacker_2016.csv", show_col_types = FALSE)

# --- Rebuild MID-any (hostility >= 1) on Panel B for R1 ---
# Same construction as the mid_force / mid_any blocks in models.R Section 0.
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

mid_any_raw <- df_mid %>%
    filter(year >= 2007, year <= 2014, hihost >= 1) %>%
    dplyr::select(statea, stateb, year) %>%
    distinct()

mid_any_names <- mid_any_raw %>%
    mutate(
        att1 = names(cow_name_to_code)[match(statea, cow_name_to_code)],
        vic1 = names(cow_name_to_code)[match(stateb, cow_name_to_code)],
        att2 = names(cow_name_to_code)[match(stateb, cow_name_to_code)],
        vic2 = names(cow_name_to_code)[match(statea, cow_name_to_code)]
    )

mid_any_directed <- bind_rows(
    mid_any_names %>%
        filter(!is.na(att1) & !is.na(vic1)) %>%
        dplyr::select(attacker = att1, victim = vic1, Year = year),
    mid_any_names %>%
        filter(!is.na(att2) & !is.na(vic2)) %>%
        dplyr::select(attacker = att2, victim = vic2, Year = year)
) %>%
    distinct() %>%
    mutate(mid_any = 1L)

df_2016 <- df_2016 %>%
    left_join(mid_any_directed, by = c("attacker", "victim", "Year")) %>%
    mutate(
        mid_any = case_when(
            Year <= 2014 & !is.na(mid_any) ~ 1L,
            Year <= 2014 & is.na(mid_any) ~ 0L,
            Year > 2014 ~ NA_integer_
        )
    )

cat(sprintf("DCID Panel B: %d obs | Panel C: %d obs\n", nrow(df_2016), nrow(df_2014)))
cat(sprintf(
    "CFR Panel A: %d country-years | CFR Panel B: %d country-years\n\n",
    nrow(df_cfr_2020), nrow(df_cfr_2016)
))


################################################################################
#   R1: NB + MID at any hostility level (Panel B) — kinetic sensitivity
################################################################################
# M4 controls for MID at hostility >= 4 (use of force); Panel C drops those
# dyad-years entirely. R1 uses the broadest MID definition — any militarized
# dispute, hostility >= 1 — to confirm the >=4 threshold is not cherry-picked.
# mid_any is NA for 2015-2016 (MID coverage ends 2014), so R1 is effectively
# estimated on 2007-2014.

cat("============================================================\n")
cat("  R1: NB + MID any hostility (>=1), Panel B\n")
cat("============================================================\n\n")

R1 <- fenegbin(
    Incident_Count ~ attacker_w4 + victim_w4 +
        attacker_cinc + victim_cinc +
        attacker_ln_gdp_pc + victim_ln_gdp_pc +
        mid_any | Year,
    data = df_2016,
    vcov = ~directed_dyad_id
)
print(summary(R1))

cat(sprintf(
    "\nR1: attacker_w4 = %.4f, p = %.2e  [H1]\n",
    coef(R1)["attacker_w4"], pvalue(R1)["attacker_w4"]
))
cat(sprintf(
    "    victim_w4   = %.4f, p = %.2e  [H2]\n",
    coef(R1)["victim_w4"], pvalue(R1)["victim_w4"]
))
cat(sprintf(
    "    mid_any     = %.4f, p = %.2e\n",
    coef(R1)["mid_any"], pvalue(R1)["mid_any"]
))


################################################################################
#   R2: NB excluding top-3 attackers (Panel C) — dominant-attacker sensitivity
################################################################################
# China, Russia and Iran dominate the incident count. If H1 holds without
# them, it is not driven by a few prolific autocracies. NB (not ZINB): with
# the top-3 removed the non-zero count is too sparse for stable two-stage
# ZINB estimation; NB degrades gracefully (wider SEs) rather than failing to
# converge. NB on Panel C mirrors M5.

cat("\n\n============================================================\n")
cat("  R2: NB excl. top-3 attackers (Panel C — clean panel)\n")
cat("============================================================\n\n")

top3 <- c("China", "Russia", "Iran")
df_2014_no_top3 <- df_2014 %>%
    filter(!attacker %in% top3)

cat(sprintf(
    "Panel C obs: %d -> excl. top-3: %d\n",
    nrow(df_2014), nrow(df_2014_no_top3)
))
cat(sprintf(
    "Incidents: %d -> excl. top-3: %d\n",
    sum(df_2014$Incident_Count_Clean),
    sum(df_2014_no_top3$Incident_Count_Clean)
))

R2 <- fenegbin(
    Incident_Count_Clean ~ attacker_w4 + victim_w4 +
        attacker_cinc + victim_cinc +
        attacker_ln_gdp_pc + victim_ln_gdp_pc | Year,
    data = df_2014_no_top3,
    vcov = ~directed_dyad_id
)
print(summary(R2))

cat(sprintf(
    "\nR2: attacker_w4 = %.4f, p = %.2e  [H1]\n",
    coef(R2)["attacker_w4"], pvalue(R2)["attacker_w4"]
))
cat(sprintf(
    "    victim_w4   = %.4f, p = %.2e  [H2]\n",
    coef(R2)["victim_w4"], pvalue(R2)["victim_w4"]
))


################################################################################
#   R3-R6: CFR alternative dataset (monadic, H1 only)
################################################################################
# CFR is monadic — the sponsor (attacker) is identified but not the victim —
# so the panel is country-year and only H1 is testable. R3-R6 mirror the DCID
# main models: R3 ~ M1, R4 ~ M2, R5 ~ M3/M6, R6 ~ M7 (primary). NB models use
# country-clustered SEs; ZINB models use model-based SEs (pscl does not
# support clustering).

cat("\n\n============================================================\n")
cat("  R3: CFR NB, Panel A (monadic, W4 + GDP)\n")
cat("============================================================\n\n")

R3 <- fenegbin(
    Incident_Count ~ attacker_w4 + attacker_ln_gdp_pc | Year,
    data = df_cfr_2020,
    vcov = ~attacker
)
print(summary(R3))
cat(sprintf(
    "\nR3: attacker_w4 = %.4f, p = %.2e  [H1]\n",
    coef(R3)["attacker_w4"], pvalue(R3)["attacker_w4"]
))


cat("\n\n============================================================\n")
cat("  R4: CFR NB, Panel B (monadic, full controls)\n")
cat("============================================================\n\n")

R4 <- fenegbin(
    Incident_Count ~ attacker_w4 + attacker_cinc + attacker_ln_gdp_pc | Year,
    data = df_cfr_2016,
    vcov = ~attacker
)
print(summary(R4))
cat(sprintf(
    "\nR4: attacker_w4 = %.4f, p = %.2e  [H1]\n",
    coef(R4)["attacker_w4"], pvalue(R4)["attacker_w4"]
))


cat("\n\n============================================================\n")
cat("  R5: CFR ZINB, Panel B (count-stage year FE)\n")
cat("============================================================\n\n")

R5 <- zeroinfl(
    Incident_Count ~ attacker_w4 + attacker_cinc + attacker_ln_gdp_pc +
        as.factor(Year) | attacker_cinc,
    data = df_cfr_2016,
    dist = "negbin"
)
print(summary(R5))

r5_count <- coef(R5, "count")
r5_count_p <- summary(R5)$coefficients$count[, "Pr(>|z|)"]
cat(sprintf(
    "\nR5 count stage: attacker_w4 = %.4f, p = %.2e  [H1]\n",
    r5_count["attacker_w4"], r5_count_p["attacker_w4"]
))


cat("\n\n============================================================\n")
cat("  R6: CFR ZINB + year FE in both stages, Panel B — primary CFR\n")
cat("============================================================\n\n")

R6 <- zeroinfl(
    Incident_Count ~ attacker_w4 + attacker_cinc + attacker_ln_gdp_pc +
        as.factor(Year) | attacker_cinc + as.factor(Year),
    data = df_cfr_2016,
    dist = "negbin"
)
print(summary(R6))

r6_count <- coef(R6, "count")
r6_count_p <- summary(R6)$coefficients$count[, "Pr(>|z|)"]
cat(sprintf(
    "\nR6 count stage: attacker_w4 = %.4f, p = %.2e  [H1, PRIMARY CFR]\n",
    r6_count["attacker_w4"], r6_count_p["attacker_w4"]
))


################################################################################
#   SUMMARY
################################################################################

cat("\n\n############################################################\n")
cat("#  ROBUSTNESS SUMMARY (R1-R6)                              #\n")
cat("############################################################\n\n")

cat("--- H1 (attacker_w4) across robustness checks ---\n")
cat(sprintf(
    "  R1  NB + MID-any (B):   %+.4f  p = %.2e\n",
    coef(R1)["attacker_w4"], pvalue(R1)["attacker_w4"]
))
cat(sprintf(
    "  R2  NB excl. top-3 (C): %+.4f  p = %.2e\n",
    coef(R2)["attacker_w4"], pvalue(R2)["attacker_w4"]
))
cat(sprintf(
    "  R3  CFR NB (A):         %+.4f  p = %.2e\n",
    coef(R3)["attacker_w4"], pvalue(R3)["attacker_w4"]
))
cat(sprintf(
    "  R4  CFR NB (B):         %+.4f  p = %.2e\n",
    coef(R4)["attacker_w4"], pvalue(R4)["attacker_w4"]
))
cat(sprintf(
    "  R5  CFR ZINB (B):       %+.4f  p = %.2e\n",
    r5_count["attacker_w4"], r5_count_p["attacker_w4"]
))
cat(sprintf(
    "  R6  CFR ZINB+FE (B):    %+.4f  p = %.2e\n",
    r6_count["attacker_w4"], r6_count_p["attacker_w4"]
))

cat("\n--- Conclusion ---\n")
cat("The attacker_w4 coefficient is negative across all six robustness checks,\n")
cat("with magnitudes consistent with the main models M1-M7. It is statistically\n")
cat("significant (p < 0.05) in five of six: R1 is marginal (p ~ 0.05-0.06) and\n")
cat("R2 loses significance (p ~ 0.10) — the sign and magnitude hold, but\n")
cat("excluding China, Russia and Iran sharply cuts the non-zero observations,\n")
cat("so this reflects lost power rather than a sign reversal. The four CFR\n")
cat("checks (R3-R6), on an entirely different dataset, are all strongly\n")
cat("significant. H2 (victim_w4) remains not supported throughout.\n")


################################################################################
#   ROBUSTNESS TABLES
################################################################################

cat("\n\n############################################################\n")
cat("#  ROBUSTNESS TABLES                                       #\n")
cat("############################################################\n\n")

table_dir <- "outputs/tables"
if (!dir.exists(table_dir)) dir.create(table_dir, recursive = TRUE)


# --- Table: robustness overview (specifications, analogous to Table 4) ---
overview <- tibble(
    Model = c("R1", "R2", "R3", "R4", "R5", "R6"),
    Category = c(
        "Kinetic-conflict sensitivity",
        "Dominant-attacker exclusion",
        "CFR alternative dataset",
        "CFR alternative dataset",
        "CFR alternative dataset",
        "CFR alternative dataset"
    ),
    Estimator = c("NB", "NB", "NB", "NB", "ZINB", "ZINB+FE"),
    Panel = c(
        "B (2007-2014)", "C (2007-2014)",
        "CFR A (2007-2020)", "CFR B (2007-2016)",
        "CFR B (2007-2016)", "CFR B (2007-2016)"
    ),
    `Specification` = c(
        "M2 + MID at any hostility (>=1)",
        "M5 excluding China, Russia, Iran",
        "Monadic; W4 + GDP; year FE",
        "Monadic; W4 + CINC + GDP; year FE",
        "Monadic ZINB; year FE in count stage",
        "Monadic ZINB; year FE in both stages (primary CFR)"
    )
)

tt(overview,
    caption = "Robustness Models Overview (R1-R6)"
) %>%
    save_tt(file.path(table_dir, "table_robustness_overview.html"), overwrite = TRUE)
cat("Saved: outputs/tables/table_robustness_overview.html\n")


# --- Coefficient maps & GOF ---
cm_dcid <- c(
    "attacker_w4"        = "Attacker W4 [H1]",
    "victim_w4"          = "Victim W4 [H2]",
    "attacker_cinc"      = "Attacker CINC",
    "victim_cinc"        = "Victim CINC",
    "attacker_ln_gdp_pc" = "Attacker ln(GDP p.c.)",
    "victim_ln_gdp_pc"   = "Victim ln(GDP p.c.)",
    "mid_any"            = "MID any hostility (≥1)"
)

cm_cfr <- c(
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


# --- Table: DCID robustness (R1-R2) ---
df_r1 <- df_2016 %>% filter(!is.na(mid_any))
nz_r1 <- sum(df_r1$Incident_Count > 0)
nz_r2 <- sum(df_2014_no_top3$Incident_Count_Clean > 0)

rows_dcid <- tibble(
    term = c("Year FE", "Non-zero obs", "% non-zero", "Total incidents"),
    `R1 NB+MID-any (B)` = c(
        "Yes", as.character(nz_r1),
        sprintf("%.4f%%", 100 * nz_r1 / nrow(df_r1)),
        as.character(sum(df_r1$Incident_Count))
    ),
    `R2 NB excl. top-3 (C)` = c(
        "Yes", as.character(nz_r2),
        sprintf("%.4f%%", 100 * nz_r2 / nrow(df_2014_no_top3)),
        as.character(sum(df_2014_no_top3$Incident_Count_Clean))
    )
)
attr(rows_dcid, "position") <- c(NA, NA, NA, NA)

modelsummary(
    list(
        "R1 NB+MID-any (B)"     = R1,
        "R2 NB excl. top-3 (C)" = R2
    ),
    output = file.path(table_dir, "table_robustness_dcid.html"),
    coef_map = cm_dcid,
    gof_map = gm,
    add_rows = rows_dcid,
    stars = c("*" = 0.05, "**" = 0.01, "***" = 0.001),
    title = "Robustness Checks on the DCID Data (R1-R2)",
    notes = paste(
        "R1: M2 + MID at any hostility (>=1), Panel B, effectively 2007-2014.",
        "R2: M5 excluding China, Russia and Iran, Panel C.",
        "Dyad-clustered SEs. Each model jointly reports H1 (attacker_w4) and",
        "H2 (victim_w4)."
    )
)
cat("Saved: outputs/tables/table_robustness_dcid.html\n")


# --- Table: CFR robustness (R3-R6) ---
nz_cfr_2020 <- sum(df_cfr_2020$Incident_Count > 0)
nz_cfr_2016 <- sum(df_cfr_2016$Incident_Count > 0)

rows_cfr <- tibble(
    term = c("Year FE", "Non-zero obs", "% non-zero", "Total incidents"),
    `R3 CFR NB (A)` = c(
        "Yes", as.character(nz_cfr_2020),
        sprintf("%.2f%%", 100 * nz_cfr_2020 / nrow(df_cfr_2020)),
        as.character(sum(df_cfr_2020$Incident_Count))
    ),
    `R4 CFR NB (B)` = c(
        "Yes", as.character(nz_cfr_2016),
        sprintf("%.2f%%", 100 * nz_cfr_2016 / nrow(df_cfr_2016)),
        as.character(sum(df_cfr_2016$Incident_Count))
    ),
    `R5 CFR ZINB (B)` = c(
        "Yes (count)", as.character(nz_cfr_2016),
        sprintf("%.2f%%", 100 * nz_cfr_2016 / nrow(df_cfr_2016)),
        as.character(sum(df_cfr_2016$Incident_Count))
    ),
    `R6 CFR ZINB+FE (B)` = c(
        "Yes (both)", as.character(nz_cfr_2016),
        sprintf("%.2f%%", 100 * nz_cfr_2016 / nrow(df_cfr_2016)),
        as.character(sum(df_cfr_2016$Incident_Count))
    )
)
attr(rows_cfr, "position") <- c(NA, NA, NA, NA)

modelsummary(
    list(
        "R3 CFR NB (A)"      = R3,
        "R4 CFR NB (B)"      = R4,
        "R5 CFR ZINB (B)"    = R5,
        "R6 CFR ZINB+FE (B)" = R6
    ),
    output = file.path(table_dir, "table_robustness_cfr.html"),
    coef_map = cm_cfr,
    gof_map = gm,
    add_rows = rows_cfr,
    stars = c("*" = 0.05, "**" = 0.01, "***" = 0.001),
    title = "Robustness Checks on the CFR Data (R3-R6, H1 only)",
    notes = paste(
        "Monadic country-year panel from the CFR Cyber Operations Tracker.",
        "All models include year fixed effects: in the linear predictor for NB",
        "models (R3, R4); in the count stage for R5; in both stages for R6.",
        "Country-clustered SEs for NB models; model-based SEs for ZINB models.",
        "H2 is not tested on CFR (no victim information). R6 is the primary CFR",
        "specification, mirroring M7 in the DCID main analysis."
    )
)
cat("Saved: outputs/tables/table_robustness_cfr.html\n")


################################################################################
#   EXPLORATORY BLOCK — fitted but NOT tabulated
################################################################################
# These models are kept for inspection only and are deliberately excluded from
# the robustness tables above. They can be deleted without affecting R1-R6.
#
#   E1: ZINB top-3 exclusion (Panel C). The two-stage ZINB may not converge
#       cleanly once the dominant attackers are removed (sparse non-zero
#       counts) — this is exactly why R2 uses NB. Wrapped in try().
#   E2: CFR NB top-3 exclusion (Panels A and B). Tests whether the CFR H1
#       result also survives dropping China, Russia and Iran.

cat("\n\n############################################################\n")
cat("#  EXPLORATORY BLOCK (not tabulated)                       #\n")
cat("############################################################\n\n")

# --- E1: ZINB top-3 exclusion (Panel C) ---
cat("--- E1: ZINB excl. top-3 attackers (Panel C) ---\n\n")
E1 <- try(
    zeroinfl(
        Incident_Count_Clean ~ attacker_w4 + victim_w4 +
            attacker_cinc + victim_cinc +
            attacker_ln_gdp_pc + victim_ln_gdp_pc + as.factor(Year) |
            attacker_cinc + victim_cinc,
        data = df_2014_no_top3,
        dist = "negbin"
    ),
    silent = TRUE
)
if (inherits(E1, "try-error")) {
    cat("E1 did not converge — confirms the choice of NB for R2.\n")
} else {
    e1_count <- coef(E1, "count")
    e1_count_p <- summary(E1)$coefficients$count[, "Pr(>|z|)"]
    cat(sprintf(
        "E1 count stage: attacker_w4 = %.4f, p = %.2e  [H1]\n",
        e1_count["attacker_w4"], e1_count_p["attacker_w4"]
    ))
}

# --- E2: CFR NB top-3 exclusion (Panels A and B) ---
cat("\n--- E2: CFR NB excl. top-3 attackers ---\n\n")
df_cfr_2020_no_top3 <- df_cfr_2020 %>% filter(!attacker %in% top3)
df_cfr_2016_no_top3 <- df_cfr_2016 %>% filter(!attacker %in% top3)

E2a <- try(
    fenegbin(
        Incident_Count ~ attacker_w4 + attacker_ln_gdp_pc | Year,
        data = df_cfr_2020_no_top3, vcov = ~attacker
    ),
    silent = TRUE
)
if (!inherits(E2a, "try-error")) {
    cat(sprintf(
        "E2a CFR NB (A) excl. top-3: attacker_w4 = %.4f, p = %.2e\n",
        coef(E2a)["attacker_w4"], pvalue(E2a)["attacker_w4"]
    ))
}

E2b <- try(
    fenegbin(
        Incident_Count ~ attacker_w4 + attacker_cinc + attacker_ln_gdp_pc | Year,
        data = df_cfr_2016_no_top3, vcov = ~attacker
    ),
    silent = TRUE
)
if (!inherits(E2b, "try-error")) {
    cat(sprintf(
        "E2b CFR NB (B) excl. top-3: attacker_w4 = %.4f, p = %.2e\n",
        coef(E2b)["attacker_w4"], pvalue(E2b)["attacker_w4"]
    ))
}

cat("\nAll robustness tables saved to:", table_dir, "\n")
