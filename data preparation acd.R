##################### data_preparation_acd.R ###################################
# Active Cyber Defence (ACD) incident categorization
#
# Depends on: data_preparation_v4.R outputs (df_model_2016.csv must exist)
# Input:      raw DCID 2.0 xlsx, COW country codes, gdp_lookup.csv
# Output:     outputs/df_model_acd.csv
#
# Classifies each DCID incident as "hack-back" (retaliatory response to a prior
# attack by the victim within a time window) or "unprovoked" (no prior attack).
#
# Tallinn Manual definition:
#   "Active Cyber Defence: The taking of proactive defensive measures outside
#    the defended cyber infrastructure."
#   "Hack back: A type of active cyber defence, the main purpose of which is
#    to take action against an identified source of a malicious cyber operation."
#
# Logic: if country A attacks country B, and country B had previously attacked
# country A within the past W years, then A's attack is classified as a hack-back.
# Otherwise it is classified as unprovoked aggression.
################################################################################

rm(list = ls())
options(scipen = 999)
cat("\014")

library(tidyverse)
library(readxl)

# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

################################################################################
#   1. Load and clean DCID (reuses exact logic from data_preparation_v4.R)
################################################################################

df_raw <- read_excel("data sources/DCID_2.0_Release_update_February_2023.xlsx")
country_codes <- read_csv("data sources/COW-country-codes.csv") %>%
    distinct(CCode, .keep_all = TRUE)

df_inc <- df_raw %>%
    mutate(StateA = trimws(StateA), StateB = trimws(StateB)) %>%
    mutate(
        duration_days = as.numeric(difftime(interactionenddate, interactionstartdate, units = "days")),
        start_date    = as.Date(interactionstartdate),
        Year          = as.integer(format(interactionstartdate, "%Y"))
    ) %>%
    filter(duration_days >= 0) %>%
    # Map initiator COW code -> attacker name
    left_join(country_codes, by = c("initiator" = "CCode")) %>%
    rename(attacker = StateNme) %>%
    # Determine victim
    mutate(
        victim = case_when(
            (attacker == StateA) |
                (attacker == "United States of America" & StateA == "US") |
                (attacker == "North Korea" & StateA == "N Korea") |
                (attacker == "South Korea" & StateA == "S Korea") ~ StateB,
            (attacker == StateB) |
                (attacker == "United States of America" & StateB == "US") |
                (attacker == "North Korea" & StateB == "N Korea") |
                (attacker == "South Korea" & StateB == "S Korea") ~ StateA,
            TRUE ~ NA_character_
        )
    ) %>%
    filter(!is.na(victim)) %>%
    mutate(
        victim = case_when(
            victim == "US" ~ "United States of America",
            victim == "N Korea" ~ "North Korea",
            victim == "S Korea" ~ "South Korea",
            victim == "UK" ~ "United Kingdom",
            TRUE ~ victim
        )
    ) %>%
    filter(Year >= 2007 & Year <= 2016) %>%
    dplyr::select(Name, attacker, victim, Year, start_date, cyber_objective) %>%
    arrange(start_date)

cat("============================================================\n")
cat("  DCID Incidents (2007-2016)\n")
cat("============================================================\n")
cat("Total incidents after cleaning:", nrow(df_inc), "\n")
cat("Unique attackers:", n_distinct(df_inc$attacker), "\n")
cat("Unique victims:", n_distinct(df_inc$victim), "\n\n")


################################################################################
#   2. Classify incidents: hack-back vs unprovoked
################################################################################
# For each incident where A attacks B on date D, check whether B attacked A
# at any point in [D - window, D). If yes -> hack_back. If no -> unprovoked.

classify_hackback <- function(df, window_years) {
    window_days <- window_years * 365
    is_hb <- logical(nrow(df))

    for (i in seq_len(nrow(df))) {
        a <- df$attacker[i]
        b <- df$victim[i]
        d <- df$start_date[i]

        prior <- df %>%
            filter(
                attacker == b, victim == a,
                start_date < d, start_date >= d - window_days
            )

        is_hb[i] <- nrow(prior) > 0
    }
    is_hb
}

cat("Classifying incidents (2-year window)...\n")
df_inc$hackback_2yr <- classify_hackback(df_inc, 2)

cat("Classifying incidents (3-year window)...\n")
df_inc$hackback_3yr <- classify_hackback(df_inc, 3)


################################################################################
#   3. Descriptive comparison: 2yr vs 3yr window
################################################################################

cat("\n============================================================\n")
cat("  Hack-Back Classification: 2yr vs 3yr Window\n")
cat("============================================================\n\n")

n <- nrow(df_inc)
hb2 <- sum(df_inc$hackback_2yr)
hb3 <- sum(df_inc$hackback_3yr)

cat(sprintf(
    "2-year window:  %3d hack-back (%4.1f%%)  |  %3d unprovoked (%4.1f%%)\n",
    hb2, hb2 / n * 100, n - hb2, (n - hb2) / n * 100
))
cat(sprintf(
    "3-year window:  %3d hack-back (%4.1f%%)  |  %3d unprovoked (%4.1f%%)\n",
    hb3, hb3 / n * 100, n - hb3, (n - hb3) / n * 100
))
cat(sprintf(
    "Reclassified (unprovoked@2yr -> hack-back@3yr): %d\n\n",
    sum(df_inc$hackback_3yr & !df_inc$hackback_2yr)
))

# W4-by-type breakdown (directional: who initiates what?)
# Need W4 for this — quick merge from panel
df_w4 <- read_csv("outputs/df_model_2016.csv") %>%
    distinct(attacker, Year, attacker_w4)

df_inc_w4 <- df_inc %>%
    left_join(df_w4, by = c("attacker", "Year")) %>%
    filter(!is.na(attacker_w4))

cat("--- Attacker W4 by incident type (2-year window) ---\n")
cat(sprintf(
    "Unprovoked attackers:  mean W4 = %.3f  (n = %d)\n",
    mean(df_inc_w4$attacker_w4[!df_inc_w4$hackback_2yr]),
    sum(!df_inc_w4$hackback_2yr)
))
cat(sprintf(
    "Hack-back attackers:   mean W4 = %.3f  (n = %d)\n",
    mean(df_inc_w4$attacker_w4[df_inc_w4$hackback_2yr]),
    sum(df_inc_w4$hackback_2yr)
))

cat("\n--- Attacker W4 by incident type (3-year window) ---\n")
cat(sprintf(
    "Unprovoked attackers:  mean W4 = %.3f  (n = %d)\n",
    mean(df_inc_w4$attacker_w4[!df_inc_w4$hackback_3yr]),
    sum(!df_inc_w4$hackback_3yr)
))
cat(sprintf(
    "Hack-back attackers:   mean W4 = %.3f  (n = %d)\n",
    mean(df_inc_w4$attacker_w4[df_inc_w4$hackback_3yr]),
    sum(df_inc_w4$hackback_3yr)
))


################################################################################
#   4. Aggregate to dyad-year (2-year window = primary specification)
################################################################################

df_agg <- df_inc %>%
    mutate(is_unprovoked = !hackback_2yr, is_hackback = hackback_2yr) %>%
    group_by(attacker, victim, Year) %>%
    summarise(
        Incident_Count = n(),
        Incident_Count_Unprovoked = sum(is_unprovoked),
        Incident_Count_HackBack = sum(is_hackback),
        .groups = "drop"
    )

cat("\n============================================================\n")
cat("  Aggregated Dyad-Year Counts\n")
cat("============================================================\n")
cat("Dyad-years with incidents:", nrow(df_agg), "\n")
cat("  Total incidents:", sum(df_agg$Incident_Count), "\n")
cat("  Unprovoked:", sum(df_agg$Incident_Count_Unprovoked), "\n")
cat("  Hack-back:", sum(df_agg$Incident_Count_HackBack), "\n")


################################################################################
#   5. Merge onto existing panel
################################################################################
# Read the panel produced by data_preparation_v4.R (has W4 + CINC, and
# potentially GDP if you've already modified the script).

df_panel <- read_csv("outputs/df_model_2016.csv")

# Drop old Incident_Count — we replace with split version
df_panel <- df_panel %>% dplyr::select(-Incident_Count)

# If GDP columns not already in panel, merge from gdp_lookup
if (!"attacker_ln_gdp_pc" %in% names(df_panel)) {
    cat("\nGDP not in panel — merging from gdp_lookup.csv\n")
    gdp <- read_csv("outputs/gdp_lookup.csv")
    df_panel <- df_panel %>%
        left_join(gdp, by = c("attacker" = "country", "Year" = "Year")) %>%
        rename(attacker_ln_gdp_pc = ln_gdp_pc) %>%
        left_join(gdp, by = c("victim" = "country", "Year" = "Year")) %>%
        rename(victim_ln_gdp_pc = ln_gdp_pc)
}

# Merge split incident counts onto panel scaffold
df_panel <- df_panel %>%
    left_join(df_agg, by = c("attacker", "victim", "Year")) %>%
    mutate(
        Incident_Count            = replace_na(Incident_Count, 0L),
        Incident_Count_Unprovoked = replace_na(Incident_Count_Unprovoked, 0L),
        Incident_Count_HackBack   = replace_na(Incident_Count_HackBack, 0L)
    )

# Complete cases filter
complete <- complete.cases(
    df_panel$attacker_w4, df_panel$victim_w4,
    df_panel$attacker_cinc, df_panel$victim_cinc,
    df_panel$attacker_ln_gdp_pc, df_panel$victim_ln_gdp_pc
)

cat("\nRows dropped (missing controls):", sum(!complete), "\n")
cat("Incidents in dropped rows:", sum(df_panel$Incident_Count[!complete]), "\n")

df_out <- df_panel[complete, ]

cat(sprintf(
    "\nFinal panel: %d obs | %d unprovoked | %d hack-back | %d total incidents\n",
    nrow(df_out),
    sum(df_out$Incident_Count_Unprovoked),
    sum(df_out$Incident_Count_HackBack),
    sum(df_out$Incident_Count)
))

write_csv(df_out, "outputs/df_model_acd.csv")
cat("Saved: outputs/df_model_acd.csv\n")
