##################### Environment Setup ########################################
rm(list=ls())
try(dev.off(), silent = TRUE)
options(scipen=999)
cat("\014")

library(tidyverse)
library(tidyr)
library(dplyr)
library(readr)

##################### Load cleaned incident data ###############################
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# This file is produced by descriptive_statistics.R
df_cyber_inc_clean <- read_csv("df_cyber_inc_clean.csv")

cat("Loaded df_cyber_inc_clean:", nrow(df_cyber_inc_clean), "observations.\n")
cat("Unique attackers:", n_distinct(df_cyber_inc_clean$attacker), "\n")
cat("Unique victims:  ", n_distinct(df_cyber_inc_clean$victim), "\n\n")

##################### Step 1: Aggregate incident counts ########################
# Each incident is counted once, in its start year (the year the operation was
# initiated). Multi-year incidents are attributed to their initiation year,
# consistent with selectorate theory's focus on the decision to launch.

dyad_year_counts <- df_cyber_inc_clean %>%
  filter(Year >= 2000 & Year <= 2020) %>%
  group_by(attacker, victim, Year) %>%
  summarise(Incident_Count = n(), .groups = "drop")

cat("Aggregated incident counts:", nrow(dyad_year_counts), "attacker-victim-year combinations.\n")
cat("Total incidents counted:   ", sum(dyad_year_counts$Incident_Count), "\n\n")

##################### Step 2: Load W4 (winning coalition) data #################
df_w4 <- read_csv("Winning Coalition original dataset/NewWmeasure.csv")

df_w4_clean <- df_w4 %>%
  dplyr::select(
    w4_country_name = country_name,
    w4_year = year,
    w4_score = W4
  ) %>%
  filter(w4_year >= 2000 & w4_year <= 2020)

cat("W4 data loaded:", n_distinct(df_w4_clean$w4_country_name),
    "countries,", nrow(df_w4_clean), "country-year observations.\n\n")

##################### Step 3: Verify DCID ↔ W4 name match #####################
all_cyber_countries <- unique(c(df_cyber_inc_clean$attacker, df_cyber_inc_clean$victim))
all_w4_countries <- unique(df_w4_clean$w4_country_name)
missing_from_w4 <- setdiff(all_cyber_countries, all_w4_countries)

if (length(missing_from_w4) > 0) {
  cat("--- ACTION REQUIRED ---\n")
  cat("DCID countries not found in W4 data:\n")
  print(missing_from_w4)
  stop("Fix country name mismatches before proceeding.")
} else {
  cat("--- SUCCESS: All DCID country names match W4 dataset. ---\n\n")
}

##################### Step 4: Create dyad-year base ############################
# Using ALL countries with W4 data (not just DCID participants) to avoid sample
# selection bias. Countries not involved in any DCID incident contribute
# informative zero observations.

all_years <- 2000:2020

df_base <- expand_grid(
  attacker = all_w4_countries,
  victim   = all_w4_countries,
  Year     = all_years
) %>%
  filter(attacker != victim)

cat("Dyad-year base:", n_distinct(all_w4_countries), "× 174 victims ×",
    length(all_years), "years =", nrow(df_base), "directed-dyad-year obs.\n")

##################### Step 5: Merge counts and W4 scores #######################
df_model <- df_base %>%
  # Join incident counts (dyads with no incidents get NA → replaced with 0)
  left_join(dyad_year_counts, by = c("attacker", "victim", "Year")) %>%
  mutate(Incident_Count = replace_na(Incident_Count, 0)) %>%

  # Join W4 for attacker
  left_join(df_w4_clean,
            by = c("attacker" = "w4_country_name", "Year" = "w4_year")) %>%
  rename(attacker_w4 = w4_score) %>%

  # Join W4 for victim
  left_join(df_w4_clean,
            by = c("victim" = "w4_country_name", "Year" = "w4_year")) %>%
  rename(victim_w4 = w4_score)

##################### Step 6: Add NATO membership control ######################
nato_founding <- c("United States of America", "United Kingdom", "France",
                   "Canada", "Italy", "Netherlands", "Belgium", "Norway",
                   "Denmark", "Portugal", "Iceland", "Luxembourg")
nato_1952 <- c("Greece", "Turkey")
nato_1999 <- c("Poland", "Czech Republic", "Hungary")
nato_2004 <- c("Bulgaria", "Romania", "Slovakia", "Slovenia",
               "Estonia", "Latvia", "Lithuania")
nato_2009 <- c("Croatia", "Albania")

df_model <- df_model %>%
  mutate(
    attacker_is_nato = case_when(
      attacker %in% nato_founding & Year >= 1949 ~ 1,
      attacker %in% nato_1952    & Year >= 1952 ~ 1,
      attacker == "Germany"      & Year >= 1955 ~ 1,
      attacker == "Spain"        & Year >= 1982 ~ 1,
      attacker %in% nato_1999    & Year >= 1999 ~ 1,
      attacker %in% nato_2004    & Year >= 2004 ~ 1,
      attacker %in% nato_2009    & Year >= 2009 ~ 1,
      attacker == "Montenegro"   & Year >= 2017 ~ 1,
      attacker == "North Macedonia" & Year >= 2020 ~ 1,
      TRUE ~ 0
    ),
    victim_is_nato = case_when(
      victim %in% nato_founding & Year >= 1949 ~ 1,
      victim %in% nato_1952    & Year >= 1952 ~ 1,
      victim == "Germany"      & Year >= 1955 ~ 1,
      victim == "Spain"        & Year >= 1982 ~ 1,
      victim %in% nato_1999    & Year >= 1999 ~ 1,
      victim %in% nato_2004    & Year >= 2004 ~ 1,
      victim %in% nato_2009    & Year >= 2009 ~ 1,
      victim == "Montenegro"   & Year >= 2017 ~ 1,
      victim == "North Macedonia" & Year >= 2020 ~ 1,
      TRUE ~ 0
    )
  )
# Note: Finland and Sweden joined NATO in 2023-2024, outside our data range.

##################### Step 7: Save and report ##################################
write.csv(df_model, "df_model.csv", row.names = FALSE)

# Summary statistics
cat("\n============================================================\n")
cat("MODEL DATASET SUMMARY\n")
cat("============================================================\n")
cat(sprintf("  Unique countries:              %d\n",   n_distinct(all_w4_countries)))
cat(sprintf("  Total dyad-year observations:  %d\n",   nrow(df_model)))
cat(sprintf("  Non-zero Incident_Count:       %d\n",   sum(df_model$Incident_Count > 0)))
cat(sprintf("  Total incidents:               %d\n",   sum(df_model$Incident_Count)))
cat(sprintf("  Observations with NA W4:       %d\n",   sum(is.na(df_model$attacker_w4) | is.na(df_model$victim_w4))))
cat(sprintf("  Year range:                    %d - %d\n", min(df_model$Year), max(df_model$Year)))

# After filtering NAs (what models.R will use)
df_model_final <- df_model %>%
  filter(!is.na(attacker_w4) & !is.na(victim_w4))

cat(sprintf("  Regression-ready observations:  %d\n",  nrow(df_model_final)))
cat(sprintf("  Non-zero (after NA filter):    %d\n",   sum(df_model_final$Incident_Count > 0)))
cat(sprintf("  Zero-inflation rate:           %.2f%%\n", mean(df_model_final$Incident_Count == 0) * 100))
cat("============================================================\n")
cat("Saved df_model.csv\n")
