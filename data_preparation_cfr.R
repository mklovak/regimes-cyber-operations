##################### data_preparation_cfr.R ##################################
# Robustness panel construction for H1, based on the CFR Cyber Operations
# Tracker (Council on Foreign Relations) instead of DCID 2.0.
#
# Why this exists:
#   DCID has known author-bias (rivalry oversampling) and includes some non-
#   state and ambiguous incidents. CFR is a smaller, journalistically curated
#   dataset of state-sponsored operations. If H1 (attacker_w4 < 0) holds on
#   CFR with the same controls, the result is not an artifact of DCID coding
#   choices.
#
# Structural difference from the DCID pipeline:
#   CFR is monadic — it identifies the SPONSOR (attacker) but not the victim.
#   So this script builds a country-year panel rather than a directed-dyad-
#   year panel. H2 (victim-side) cannot be tested on CFR.
#
# Output:
#   outputs/df_attacker_2020.csv  — 2007-2020, W4 + GDP, monadic
#   outputs/df_attacker_2016.csv  — 2007-2016, W4 + CINC + GDP, monadic
#
# Multi-sponsor handling:
#   Rows flagged MULTI_SPONSOR_PRIMARY have multiple states in the Sponsor
#   column (e.g., "United States, Israel" for Stuxnet). sponsor_cow records
#   only the primary code. We split Sponsor on commas and create one panel
#   row per sponsor — so Stuxnet contributes +1 to both Israel-2010 and
#   USA-2010. This is the correct count interpretation: each state did
#   sponsor the operation.
################################################################################

rm(list = ls())
options(scipen = 999)
cat("\014")

library(readxl)
library(tidyverse)
library(stringr)

# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


##################### 1. Load CFR incident data ################################

df_cfr_raw <- read_excel("data sources/cfr_attacker_coded.xlsx")

cat(sprintf("CFR raw incidents: %d\n", nrow(df_cfr_raw)))
cat(sprintf(
    "Date range: %s to %s\n",
    format(min(df_cfr_raw$Date, na.rm = TRUE), "%Y-%m-%d"),
    format(max(df_cfr_raw$Date, na.rm = TRUE), "%Y-%m-%d")
))

# Add Year, drop rows with missing date
df_cfr <- df_cfr_raw %>%
    mutate(Year = as.integer(format(Date, "%Y"))) %>%
    filter(!is.na(Year), !is.na(Sponsor))


##################### 2. Expand multi-sponsor rows #############################
# Split Sponsor on comma → one row per (incident, sponsor).
# Single-sponsor rows are unaffected (no comma to split on).

df_cfr_long <- df_cfr %>%
    mutate(sponsor_list = str_split(Sponsor, ",\\s*")) %>%
    unnest(sponsor_list) %>%
    mutate(sponsor_clean = str_trim(sponsor_list)) %>%
    dplyr::select(Title, Year, sponsor_clean, sponsor_flag)

n_multi <- df_cfr %>%
    filter(sponsor_flag == "MULTI_SPONSOR_PRIMARY") %>%
    nrow()
cat(sprintf(
    "Multi-sponsor incidents expanded: %d (added rows = %d)\n",
    n_multi, nrow(df_cfr_long) - nrow(df_cfr)
))


##################### 3. Map CFR sponsor names → W4 country names ##############
# CFR uses UN-style long names; W4 uses short forms. This is the inverse of
# wdi_to_w4 in data_preparation.R, plus a few additions for Korean/Iranian/
# Russian variants and the smaller multi-sponsor states.
#
# Non-country values ("Espionage", "Government, Military") receive NA and
# drop in the next step.

cfr_to_w4 <- c(
    "United States"                            = "United States of America",
    "United States of America"                 = "United States of America",
    "Russian Federation"                       = "Russia",
    "Iran (Islamic Republic of)"               = "Iran",
    "Korea (Democratic People's Republic of)"  = "North Korea",
    "Korea (Republic of)"                      = "South Korea",
    "Syrian Arab Republic"                     = "Syria",
    "Viet Nam"                                 = "Vietnam"
    # All other names (China, Israel, India, Pakistan, UK, France, Germany,
    # Canada, Australia, Belarus, Spain, Netherlands, Lithuania, Ukraine,
    # Bulgaria, New Zealand, Taiwan, Saudi Arabia, UAE) match W4 directly.
)

df_cfr_long <- df_cfr_long %>%
    mutate(
        attacker = ifelse(sponsor_clean %in% names(cfr_to_w4),
            cfr_to_w4[sponsor_clean],
            sponsor_clean
        )
    )

# Diagnostic: which sponsor strings fail to map to a real country?
non_country_sponsors <- df_cfr_long %>%
    filter(!attacker %in% c(
        "United States of America", "Russia", "China", "Iran", "North Korea",
        "South Korea", "India", "Pakistan", "Israel", "Ukraine", "Belarus",
        "United Kingdom", "France", "Germany", "Canada", "Australia",
        "Lithuania", "Netherlands", "Spain", "Bulgaria", "New Zealand",
        "Taiwan", "Saudi Arabia", "United Arab Emirates", "Syria", "Vietnam",
        "Poland", "Estonia", "Georgia", "Lebanon", "Turkey", "Japan",
        "Philippines"
    )) %>%
    count(sponsor_clean, sort = TRUE)

if (nrow(non_country_sponsors) > 0) {
    cat("\nNon-country sponsor strings (will be dropped):\n")
    print(non_country_sponsors)
}

# Drop non-country rows by requiring the mapped name to appear in W4 data.
# We do this implicitly via the W4 left-join below — any unmatched name gets
# NA on attacker_w4 and is dropped at the final filter step.


##################### 4. Aggregate to (country, year) counts ###################

df_cfr_counts <- df_cfr_long %>%
    count(attacker, Year, name = "Incident_Count")

cat(sprintf(
    "\nUnique (country, year) cells with at least one incident: %d\n",
    nrow(df_cfr_counts)
))
cat(sprintf(
    "Distinct sponsoring countries: %d\n",
    n_distinct(df_cfr_counts$attacker)
))


##################### 5. Pull attacker-side covariates from existing panels ####
# Reuse the cleaned attacker-side covariates from df_model_2020.csv and
# df_model_2016.csv — these already include the full WDI pipeline plus BOK
# (North Korea) and ITU (Taiwan) imputation, plus W4 and CINC merges. Taking
# attacker-side rows and deduplicating gives a country-year covariate panel.

df_2020_full <- read_csv("outputs/df_model_2020.csv", show_col_types = FALSE)
df_2016_full <- read_csv("outputs/df_model_2016.csv", show_col_types = FALSE)

attacker_covars_2020 <- df_2020_full %>%
    dplyr::select(
        attacker, Year,
        attacker_w4, attacker_gdp_pc, attacker_ln_gdp_pc
    ) %>%
    distinct()

attacker_covars_2016 <- df_2016_full %>%
    dplyr::select(
        attacker, Year,
        attacker_w4, attacker_cinc,
        attacker_gdp_pc, attacker_ln_gdp_pc
    ) %>%
    distinct()

cat(sprintf(
    "\nAttacker-year covariate rows: 2020 panel = %d, 2016 panel = %d\n",
    nrow(attacker_covars_2020), nrow(attacker_covars_2016)
))


##################### 6. Build df_attacker_2020 (2007-2020, W4 + GDP) ##########

df_attacker_2020 <- attacker_covars_2020 %>%
    filter(Year >= 2007, Year <= 2020) %>%
    left_join(df_cfr_counts, by = c("attacker", "Year")) %>%
    mutate(
        Incident_Count = replace_na(Incident_Count, 0L),
        has_attack     = as.integer(Incident_Count > 0)
    ) %>%
    filter(!is.na(attacker_w4), !is.na(attacker_ln_gdp_pc))

# Diagnostic: how many CFR incidents did we successfully merge?
total_cfr_in_range <- sum(df_cfr_counts$Incident_Count[df_cfr_counts$Year >= 2007 &
    df_cfr_counts$Year <= 2020])
total_in_panel <- sum(df_attacker_2020$Incident_Count)
cat(sprintf(
    "\ndf_attacker_2020 — Obs: %d | Incidents in panel: %d | CFR incidents 2007-2020: %d | Lost to W4/GDP missingness: %d\n",
    nrow(df_attacker_2020), total_in_panel, total_cfr_in_range,
    total_cfr_in_range - total_in_panel
))

if (!dir.exists("outputs")) dir.create("outputs")
write.csv(df_attacker_2020, "outputs/df_attacker_2020.csv", row.names = FALSE)
cat("Saved: outputs/df_attacker_2020.csv\n")


##################### 7. Build df_attacker_2016 (2007-2016, W4 + CINC + GDP) ###

df_attacker_2016 <- attacker_covars_2016 %>%
    filter(Year >= 2007, Year <= 2016) %>%
    left_join(df_cfr_counts, by = c("attacker", "Year")) %>%
    mutate(
        Incident_Count = replace_na(Incident_Count, 0L),
        has_attack     = as.integer(Incident_Count > 0)
    ) %>%
    filter(
        !is.na(attacker_w4),
        !is.na(attacker_cinc),
        !is.na(attacker_ln_gdp_pc)
    )

total_cfr_in_range_2016 <- sum(df_cfr_counts$Incident_Count[df_cfr_counts$Year >= 2007 &
    df_cfr_counts$Year <= 2016])
total_in_panel_2016 <- sum(df_attacker_2016$Incident_Count)
cat(sprintf(
    "\ndf_attacker_2016 — Obs: %d | Incidents in panel: %d | CFR incidents 2007-2016: %d | Lost to W4/CINC/GDP missingness: %d\n",
    nrow(df_attacker_2016), total_in_panel_2016, total_cfr_in_range_2016,
    total_cfr_in_range_2016 - total_in_panel_2016
))

write.csv(df_attacker_2016, "outputs/df_attacker_2016.csv", row.names = FALSE)
cat("Saved: outputs/df_attacker_2016.csv\n")


##################### 8. Final summary #########################################

cat("\n############################################################\n")
cat("#  CFR-BASED MONADIC PANELS BUILT                          #\n")
cat("############################################################\n")
cat(sprintf(
    "  outputs/df_attacker_2020.csv  %d obs, %d incidents (W4 + GDP, 2007-2020)\n",
    nrow(df_attacker_2020), sum(df_attacker_2020$Incident_Count)
))
cat(sprintf(
    "  outputs/df_attacker_2016.csv  %d obs, %d incidents (W4 + CINC + GDP, 2007-2016)\n",
    nrow(df_attacker_2016), sum(df_attacker_2016$Incident_Count)
))

cat("\nTop 10 attackers by total CFR incidents in 2020 panel:\n")
df_attacker_2020 %>%
    group_by(attacker) %>%
    summarise(
        incidents = sum(Incident_Count),
        avg_w4 = round(mean(attacker_w4, na.rm = TRUE), 3),
        .groups = "drop"
    ) %>%
    arrange(desc(incidents)) %>%
    slice_head(n = 10) %>%
    print()

cat("\nNote: H2 cannot be tested on CFR (no victim information).\n")
cat("This panel is for H1 robustness only.\n")
