# Developed with the assistance of Claude AI (Anthropic)
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
#   Section 5: Diagnostics (D1-D3)
#
# Robustness checks (R1-R6: MID-any sensitivity, top-3 exclusion, CFR
# alternative dataset) live in robustness.R.
# Poisson vs NB likelihood-ratio test lives in descriptive_statistics.R.
################################################################################

rm(list = ls())
options(scipen = 999)
cat("\014")

library(tidyverse)
library(readxl)
library(fixest) # NB with year FE and dyad-clustered SEs
library(pscl) # ZINB via zeroinfl()
library(modelsummary) # HTML table export
library(tinytable) # small custom tables (D3)

# --- Thesis-ready figures: Cambria 13pt, line height 1.5 -----------------------
# Cambria ships inside the Microsoft Word app bundle on macOS; register the
# files with showtext so Graph_4.png embeds Cambria. Falls back to sans if
# Cambria is unavailable.
cambria_dir <- "/Applications/Microsoft Word.app/Contents/Resources/DFonts"
if (file.exists(file.path(cambria_dir, "Cambria.ttc"))) {
  library(showtext)
  sysfonts::font_add(
    "Cambria",
    regular    = file.path(cambria_dir, "Cambria.ttc"),
    bold       = file.path(cambria_dir, "Cambriab.ttf"),
    italic     = file.path(cambria_dir, "Cambriai.ttf"),
    bolditalic = file.path(cambria_dir, "Cambriaz.ttf")
  )
  showtext::showtext_auto()
  showtext::showtext_opts(dpi = 300)
  thesis_family <- "Cambria"
} else {
  thesis_family <- "sans"
}

# Theme used for the forest plot Graph_4.
thesis_theme <- ggplot2::theme_minimal(base_family = thesis_family, base_size = 13) +
  ggplot2::theme(
    text          = ggplot2::element_text(family = thesis_family, size = 13, lineheight = 1.5),
    axis.text     = ggplot2::element_text(size = 13),
    axis.title    = ggplot2::element_text(size = 13),
    plot.subtitle = ggplot2::element_text(size = 13),
    legend.text   = ggplot2::element_text(size = 13),
    legend.title  = ggplot2::element_text(size = 13)
  )

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

# --- 0.2b Build MID-any dummy (hostility >= 1) for R1 in robustness.R ---
# Same construction as mid_force but with the broadest MID threshold (any
# militarized dispute, not just use of force). Used by the kinetic-conflict
# sensitivity check R1 — confirms the >=4 threshold in M4/Panel C is not
# cherry-picked. Coverage ends 2014, so 2015-2016 rows get NA.

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
  output = file.path(table_dir, "Table_5.html"),
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
cat("Saved: outputs/tables/Table_5.html\n")


# --- Coefficient forest plot: W4 across M1-M7 (H1 and H2) ---
# Visualises the winning-coalition coefficients with 95% confidence intervals
# across all seven main models. H1 (attacker_w4) and H2 (victim_w4) are shown
# together so the contrast is immediate: H1 points sit below zero, H2 points
# straddle it. NB models use dyad-clustered SEs; ZINB models show count-stage
# coefficients with model-based SEs.

plot_dir <- "outputs/plots"
if (!dir.exists(plot_dir)) dir.create(plot_dir, recursive = TRUE)

# Estimate + 95% CI for a term, from an NB (fixest) or ZINB (zeroinfl) model.
get_coef_ci <- function(model, term) {
  if (inherits(model, "fixest")) {
    est <- unname(coef(model)[term])
    se_ <- unname(se(model)[term])
  } else {
    cs <- summary(model)$coefficients$count
    est <- unname(cs[term, "Estimate"])
    se_ <- unname(cs[term, "Std. Error"])
  }
  c(estimate = est, conf.low = est - 1.96 * se_, conf.high = est + 1.96 * se_)
}

main_models <- list(
  "M1 NB (A)" = M1, "M2 NB (B)" = M2, "M3 ZINB (B)" = M3,
  "M4 NB+MID (B)" = M4, "M5 NB (C)" = M5, "M6 ZINB (C)" = M6,
  "M7 ZINB+FE (C)" = M7
)

coef_df <- bind_rows(lapply(names(main_models), function(nm) {
  m <- main_models[[nm]]
  a <- get_coef_ci(m, "attacker_w4")
  v <- get_coef_ci(m, "victim_w4")
  tibble(
    model = nm,
    hypothesis = c("H1: Attacker W4", "H2: Victim W4"),
    estimate = c(a[["estimate"]], v[["estimate"]]),
    conf.low = c(a[["conf.low"]], v[["conf.low"]]),
    conf.high = c(a[["conf.high"]], v[["conf.high"]])
  )
})) %>%
  mutate(model = factor(model, levels = rev(names(main_models))))

p_forest <- ggplot(coef_df, aes(x = estimate, y = model, color = hypothesis)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high),
    position = position_dodge(width = 0.55), linewidth = 0.7, size = 0.5
  ) +
  scale_color_manual(values = c(
    "H1: Attacker W4" = "#1D3557", "H2: Victim W4" = "#E63946"
  )) +
  labs(
    subtitle = paste0(
      "Points are coefficient estimates; bars are 95% confidence intervals.\n",
      "ZINB models (M3, M6, M7) show count-stage coefficients."
    ),
    x = "Coefficient (log incidence-rate ratio)", y = NULL, color = NULL
  ) +
  thesis_theme +
  theme(legend.position = "bottom")

ggsave(file.path(plot_dir, "Graph_4.png"),
  p_forest,
  width = 9, height = 5.5
)
cat("Saved: outputs/plots/Graph_4.png\n")

cat("\nDone. Primary results: read attacker_w4 (H1) and victim_w4 (H2) from M7.\n")

################################################################################
#                 SECTION 5: DIAGNOSTICS (D1-D3)
################################################################################
# NOT robustness checks — these models probe specification choices and support
# interpretations in the text; they do not confront the finding with external
# data, controls or samples (that is what R1-R6 in robustness.R do).
#
# D1, D2 — H2 extensive-margin diagnostics. Two logit models on has_attack_clean
#   (any unprovoked cyber operation received in a dyad-year, yes/no), Panel C,
#   year FE, dyad-clustered SEs. They show whether victim regime type predicts
#   target selection once capability (CINC) or wealth (GDP p.c.) is controlled.
#     D1: CINC-only controls.   D2: GDP-only controls.
#
# D3 — M7 specification diagnostic. Re-estimates M7 with W4 added to the
#   inflation stage as well as the count stage, to confirm the H1/H2
#   conclusions do not depend on M7's stage-placement choice.
#
# The robustness checks proper (R1-R6) live in robustness.R.

cat("\n\n############################################################\n")
cat("#  SECTION 5: DIAGNOSTICS (D1-D3)                          #\n")
cat("############################################################\n\n")

df_2014 <- df_2014 %>%
  mutate(has_attack_clean = as.integer(Incident_Count_Clean > 0))


# --- D1: Logit on has_attack_clean, CINC-only controls (Panel C) ---
cat("--- D1: Logit, CINC-only controls (Panel C) ---\n\n")

D1 <- feglm(
  has_attack_clean ~ attacker_w4 + victim_w4 +
    attacker_cinc + victim_cinc | Year,
  data = df_2014,
  family = binomial,
  vcov = ~directed_dyad_id
)
print(summary(D1))

cat(sprintf(
  "\nD1: victim_w4 = %.4f, p = %.2e  [H2 extensive margin]\n",
  coef(D1)["victim_w4"], pvalue(D1)["victim_w4"]
))


# --- D2: Logit on has_attack_clean, GDP-only controls (Panel C) ---
cat("\n\n--- D2: Logit, GDP-only controls (Panel C) ---\n\n")

D2 <- feglm(
  has_attack_clean ~ attacker_w4 + victim_w4 +
    attacker_ln_gdp_pc + victim_ln_gdp_pc | Year,
  data = df_2014,
  family = binomial,
  vcov = ~directed_dyad_id
)
print(summary(D2))

cat(sprintf(
  "\nD2: victim_w4 = %.4f, p = %.2e  [H2 extensive margin]\n",
  coef(D2)["victim_w4"], pvalue(D2)["victim_w4"]
))


# --- D1/D2 diagnostic table ---
cm_d <- c(
  "attacker_w4"        = "Attacker W4 [H1]",
  "victim_w4"          = "Victim W4 [H2]",
  "attacker_cinc"      = "Attacker CINC",
  "victim_cinc"        = "Victim CINC",
  "attacker_ln_gdp_pc" = "Attacker ln(GDP p.c.)",
  "victim_ln_gdp_pc"   = "Victim ln(GDP p.c.)"
)

modelsummary(
  list(
    "D1: Logit, CINC only" = D1,
    "D2: Logit, GDP only"  = D2
  ),
  output = file.path(table_dir, "table_h2_diag.html"),
  coef_map = cm_d,
  gof_map = gm,
  stars = c("*" = 0.05, "**" = 0.01, "***" = 0.001),
  title = "H2 Extensive-Margin Diagnostics: Logit on Any Cyber Operation Received (Panel C)",
  notes = paste(
    "Outcome: has_attack_clean (any unprovoked cyber operation received in a",
    "dyad-year, yes/no). Panel C (clean DV, 2007-2014). Year fixed effects;",
    "dyad-clustered SEs. These are interpretive diagnostics for the H2",
    "non-finding, not robustness checks: they show victim W4 does not predict",
    "target selection once capability (CINC) or wealth (GDP p.c.) is controlled."
  )
)
cat("Saved: outputs/tables/table_h2_diag.html\n")


# --- D3: M7 specification diagnostic — W4 in both ZINB stages ---
# M7 places W4 in the count stage only. D3 re-estimates M7 with W4 added to the
# inflation stage as well, and checks whether the H1/H2 conclusions change.
# This diagnoses the M7 stage-placement choice; it is not a robustness check
# (it changes no data, controls or sample — only M7's internal specification).
# Inflation-stage signs are FLIPPED relative to the count stage: the inflation
# stage models P(structural zero), so a negative coefficient = more likely to
# be in the active dyad pool.

cat("\n\n--- D3: M7 diagnostic — W4 in both ZINB stages ---\n\n")

D3 <- zeroinfl(
  Incident_Count_Clean ~ attacker_w4 + victim_w4 +
    attacker_cinc + victim_cinc +
    attacker_ln_gdp_pc + victim_ln_gdp_pc + as.factor(Year) |
    attacker_w4 + victim_w4 +
      attacker_cinc + victim_cinc + as.factor(Year),
  data = df_2014,
  dist = "negbin"
)
print(summary(D3))

d3_c <- coef(D3, "count")
d3_z <- coef(D3, "zero")
d3_cp <- summary(D3)$coefficients$count[, "Pr(>|z|)"]
d3_zp <- summary(D3)$coefficients$zero[, "Pr(>|z|)"]

cat(sprintf(
  "\nD3 count stage:     attacker_w4 = %+.4f (p = %.2e)  victim_w4 = %+.4f (p = %.2e)\n",
  d3_c["attacker_w4"], d3_cp["attacker_w4"],
  d3_c["victim_w4"], d3_cp["victim_w4"]
))
cat(sprintf(
  "D3 inflation stage: attacker_w4 = %+.4f (p = %.2e)  victim_w4 = %+.4f (p = %.2e)\n",
  d3_z["attacker_w4"], d3_zp["attacker_w4"],
  d3_z["victim_w4"], d3_zp["victim_w4"]
))
cat(sprintf(
  "AIC: M7 = %.1f, D3 = %.1f (lower = better fit)\n",
  AIC(M7), AIC(D3)
))

# --- D3 table: 2x2 of W4 coefficients across both ZINB stages ---
d3_star <- function(p) {
  if (p < 0.001) {
    "***"
  } else if (p < 0.01) {
    "**"
  } else if (p < 0.05) {
    "*"
  } else {
    ""
  }
}
d3_fmt <- function(b, p) sprintf("%.3f%s (p = %.3f)", b, d3_star(p), p)

d3_tbl <- tibble(
  Coefficient = c("Attacker W4 [H1]", "Victim W4 [H2]"),
  `Count stage (frequency)` = c(
    d3_fmt(d3_c["attacker_w4"], d3_cp["attacker_w4"]),
    d3_fmt(d3_c["victim_w4"], d3_cp["victim_w4"])
  ),
  `Inflation stage (structural zero)` = c(
    d3_fmt(d3_z["attacker_w4"], d3_zp["attacker_w4"]),
    d3_fmt(d3_z["victim_w4"], d3_zp["victim_w4"])
  )
)

tt(d3_tbl,
  caption = paste(
    "D3: Winning Coalition Index in Both ZINB Stages",
    "(M7 specification diagnostic, Panel C)"
  )
) %>%
  save_tt(file.path(table_dir, "table_d3.html"), overwrite = TRUE)
cat("Saved: outputs/tables/table_d3.html\n")
