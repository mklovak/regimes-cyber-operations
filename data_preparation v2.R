##################### Environment Setup ########################################
rm(list = ls())
try(dev.off(), silent = TRUE)
options(scipen = 999)
cat("\014")

library(tidyverse)
library(tidyr)
library(dplyr)
library(readr)
library(readxl)
library(stringr)

##################### Load cleaned incident data ###############################
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

df_cyber_inc_clean <- read_csv("df_cyber_inc_clean.csv")
cat("Loaded df_cyber_inc_clean:", nrow(df_cyber_inc_clean), "observations.\n")
cat("Unique attackers:", n_distinct(df_cyber_inc_clean$attacker), "\n")
cat("Unique victims:  ", n_distinct(df_cyber_inc_clean$victim), "\n\n")

##################### Step 1: Aggregate incident counts ########################
dyad_year_counts <- df_cyber_inc_clean %>%
  filter(Year >= 2007 & Year <= 2020) %>%
  group_by(attacker, victim, Year) %>%
  summarise(Incident_Count = n(), .groups = "drop")

cat("Aggregated incident counts:", nrow(dyad_year_counts), "attacker-victim-year combinations.\n")
cat("Total incidents counted:   ", sum(dyad_year_counts$Incident_Count), "\n\n")

##################### Step 2: Load W4 (winning coalition) data #################
df_w4 <- read_csv("Winning Coalition original dataset/NewWmeasure.csv")

df_w4_clean <- df_w4 %>%
  dplyr::select(w4_country_name = country_name, w4_year = year, w4_score = W4) %>%
  filter(w4_year >= 2007 & w4_year <= 2020)

cat("W4 data loaded:", n_distinct(df_w4_clean$w4_country_name),
    "countries,", nrow(df_w4_clean), "country-year observations.\n\n")

##################### Step 3: Verify DCID <-> W4 name match ####################
all_cyber_countries <- unique(c(df_cyber_inc_clean$attacker, df_cyber_inc_clean$victim))
all_w4_countries    <- unique(df_w4_clean$w4_country_name)
missing_from_w4     <- setdiff(all_cyber_countries, all_w4_countries)

if (length(missing_from_w4) > 0) {
  cat("--- ACTION REQUIRED ---\n")
  cat("DCID countries not found in W4 data:\n")
  print(missing_from_w4)
  stop("Fix country name mismatches before proceeding.")
} else {
  cat("--- SUCCESS: All DCID country names match W4 dataset. ---\n\n")
}

##################### Step 4: Create dyad-year base (2007-2020) ################
all_years <- 2007:2020

df_base <- expand_grid(
  attacker = all_w4_countries,
  victim   = all_w4_countries,
  Year     = all_years
) %>%
  filter(attacker != victim)

cat("Dyad-year base:", n_distinct(all_w4_countries), "countries x",
    length(all_years), "years =", nrow(df_base), "directed-dyad-year obs.\n")

##################### Step 5: Merge counts and W4 scores #######################
df_model <- df_base %>%
  left_join(dyad_year_counts, by = c("attacker", "victim", "Year")) %>%
  mutate(Incident_Count = replace_na(Incident_Count, 0)) %>%
  left_join(df_w4_clean, by = c("attacker" = "w4_country_name", "Year" = "w4_year")) %>%
  rename(attacker_w4 = w4_score) %>%
  left_join(df_w4_clean, by = c("victim" = "w4_country_name", "Year" = "w4_year")) %>%
  rename(victim_w4 = w4_score)

##################### Step 6: Add NATO membership ##############################
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

################################################################################
#                  Step 7: GDP PER CAPITA (constant 2015 USD)
#
# Sources:
#   - WDI (NY.GDP.PCAP.KD) for 172 countries
#   - Bank of Korea (BOK) GNI estimates + chain-linking for North Korea
#   - Taiwan DGBAS national accounts + chain-linking for Taiwan
################################################################################

cat("\n============================================================\n")
cat("Step 7: GDP per capita integration\n")
cat("============================================================\n")

# --- 7a: Load WDI GDP data (172 countries) ---
df_gdp_raw <- read_csv("WDI data/a14efc5a-7fe8-4ea4-b8ed-57b7727e3394_Data.csv")

year_cols <- grep("^\\d{4}", names(df_gdp_raw), value = TRUE)

df_gdp_long <- df_gdp_raw %>%
  dplyr::select(`Country Name`, all_of(year_cols)) %>%
  pivot_longer(cols = all_of(year_cols), names_to = "Year", values_to = "gdp_pc") %>%
  mutate(
    Year   = as.integer(str_extract(Year, "^\\d{4}")),
    gdp_pc = as.numeric(na_if(gdp_pc, ".."))
  ) %>%
  filter(!is.na(gdp_pc)) %>%
  rename(wdi_name = `Country Name`)

# Map WDI names to W4 names
wdi_to_w4 <- c(
  "United States"                   = "United States of America",
  "Russian Federation"              = "Russia",
  "Iran, Islamic Rep."              = "Iran",
  "Egypt, Arab Rep."                = "Egypt",
  "Venezuela, RB"                   = "Venezuela",
  "Korea, Rep."                     = "South Korea",
  "Korea, Dem. People's Rep."       = "North Korea",
  "Congo, Dem. Rep."                = "Democratic Republic of the Congo",
  "Congo, Rep."                     = "Republic of the Congo",
  "Czechia"                         = "Czech Republic",
  "Slovak Republic"                 = "Slovakia",
  "Turkiye"                         = "Turkey",
  "Viet Nam"                        = "Vietnam",
  "Lao PDR"                         = "Laos",
  "Kyrgyz Republic"                 = "Kyrgyzstan",
  "Gambia, The"                     = "The Gambia",
  "Cote d'Ivoire"                   = "Ivory Coast",
  "Syrian Arab Republic"            = "Syria",
  "Yemen, Rep."                     = "Yemen",
  "Myanmar"                         = "Burma/Myanmar",
  "Cabo Verde"                      = "Cape Verde",
  "Somalia, Fed. Rep."              = "Somalia"
)

df_gdp_long <- df_gdp_long %>%
  mutate(country = ifelse(wdi_name %in% names(wdi_to_w4), wdi_to_w4[wdi_name], wdi_name)) %>%
  dplyr::select(country, Year, gdp_pc)

# --- 7b: North Korea GDP (chain-linked from BOK real growth rates) ---
# Source: Bank of Korea annual GDP estimates for North Korea.
# Anchor: 2015 per capita GNI = 1,393,000 KRW / 1,131 KRW/USD = $1,232 (constant 2015 USD).
# Method: chain-link using BOK real GDP growth rates and population.

nk_data <- tibble(
  Year       = 2007:2020,
  growth_pct = c(-1.2, 3.1, -0.9, -0.5, 0.8, 1.3, 1.1, 1.0, -1.1, 3.9, -3.5, -4.1, 0.4, -4.5),
  pop_k      = c(23200, 23298, 23380, 24187, 24308, 24427, 24545, 24662, 24779, 24897, 25014, 25132, 25250, 25368)
)

nk_anchor_year <- 2015
nk_anchor_pc   <- 1232  # constant 2015 USD

# Compute total real GDP index (2015 = 1)
nk_data$real_gdp_index <- NA
nk_data$real_gdp_index[nk_data$Year == nk_anchor_year] <- 1

# Forward from 2015
for (i in which(nk_data$Year == nk_anchor_year):nrow(nk_data)) {
  if (i < nrow(nk_data)) {
    nk_data$real_gdp_index[i + 1] <- nk_data$real_gdp_index[i] * (1 + nk_data$growth_pct[i + 1] / 100)
  }
}
# Backward from 2015
for (i in which(nk_data$Year == nk_anchor_year):1) {
  if (i > 1) {
    nk_data$real_gdp_index[i - 1] <- nk_data$real_gdp_index[i] / (1 + nk_data$growth_pct[i] / 100)
  }
}

# Convert to per capita (adjusting for population change relative to anchor year)
nk_anchor_pop <- nk_data$pop_k[nk_data$Year == nk_anchor_year]
nk_data$gdp_pc <- nk_anchor_pc * nk_data$real_gdp_index * (nk_anchor_pop / nk_data$pop_k)

df_gdp_nk <- nk_data %>%
  dplyr::select(Year, gdp_pc) %>%
  mutate(country = "North Korea")

cat("  North Korea GDP per capita (constant 2015 USD):\n")
cat(sprintf("    Range: $%.0f - $%.0f\n", min(df_gdp_nk$gdp_pc), max(df_gdp_nk$gdp_pc)))

# --- 7c: Taiwan GDP (chain-linked from DGBAS real growth rates) ---
# Source: Taiwan DGBAS Principal Figures (2008 SNA).
# Anchor: 2015 per capita GDP at current prices = $22,780 USD.
# Method: chain-link using real GDP growth rates and population.

tw_data <- tibble(
  Year       = 2007:2020,
  growth_pct = c(6.85, 0.80, -1.61, 10.25, 3.67, 2.22, 2.48, 4.72, 1.47, 2.17, 3.66, 2.91, 3.06, 3.42),
  pop        = c(22917444, 22997696, 23078402, 23140948, 23193518, 23270367,
                 23344670, 23403635, 23462914, 23515945, 23555522, 23580080,
                 23596027, 23582179)
)

tw_anchor_year <- 2015
tw_anchor_pc   <- 22780  # USD at 2015 prices (current = constant in base year)

tw_data$real_gdp_index <- NA
tw_data$real_gdp_index[tw_data$Year == tw_anchor_year] <- 1

for (i in which(tw_data$Year == tw_anchor_year):nrow(tw_data)) {
  if (i < nrow(tw_data)) {
    tw_data$real_gdp_index[i + 1] <- tw_data$real_gdp_index[i] * (1 + tw_data$growth_pct[i + 1] / 100)
  }
}
for (i in which(tw_data$Year == tw_anchor_year):1) {
  if (i > 1) {
    tw_data$real_gdp_index[i - 1] <- tw_data$real_gdp_index[i] / (1 + tw_data$growth_pct[i] / 100)
  }
}

tw_anchor_pop <- tw_data$pop[tw_data$Year == tw_anchor_year]
tw_data$gdp_pc <- tw_anchor_pc * tw_data$real_gdp_index * (tw_anchor_pop / tw_data$pop)

df_gdp_tw <- tw_data %>%
  dplyr::select(Year, gdp_pc) %>%
  mutate(country = "Taiwan")

cat("  Taiwan GDP per capita (constant 2015 USD):\n")
cat(sprintf("    Range: $%.0f - $%.0f\n", min(df_gdp_tw$gdp_pc), max(df_gdp_tw$gdp_pc)))

# --- 7d: Combine all GDP data ---
df_gdp_all <- bind_rows(df_gdp_long, df_gdp_nk, df_gdp_tw) %>%
  mutate(ln_gdp_pc = log(gdp_pc))

# Merge into df_model
df_model <- df_model %>%
  left_join(df_gdp_all, by = c("attacker" = "country", "Year" = "Year")) %>%
  rename(attacker_gdp_pc = gdp_pc, attacker_ln_gdp_pc = ln_gdp_pc) %>%
  left_join(df_gdp_all, by = c("victim" = "country", "Year" = "Year")) %>%
  rename(victim_gdp_pc = gdp_pc, victim_ln_gdp_pc = ln_gdp_pc)

cat("  GDP coverage (attacker): ", sprintf("%.1f%%", 100 * mean(!is.na(df_model$attacker_ln_gdp_pc))), "\n")
cat("  GDP coverage (victim):   ", sprintf("%.1f%%", 100 * mean(!is.na(df_model$victim_ln_gdp_pc))), "\n")

# Report which W4 countries still lack GDP
missing_gdp_countries <- setdiff(
  all_w4_countries,
  unique(df_gdp_all$country)
)
cat("  Countries without GDP data:", length(missing_gdp_countries), "\n")
if (length(missing_gdp_countries) > 0) cat("    ", paste(missing_gdp_countries, collapse = ", "), "\n")


################################################################################
#                  Step 8: INTERNET PENETRATION
#
# Source: WDI (IT.NET.USER.ZS) — % of individuals using the Internet
# North Korea: coded as 0% (no public internet access).
# Taiwan: hard-coded from ITU / Taiwan NCC estimates.
# Zanzibar: uses Tanzania's values (Zanzibar is part of Tanzania).
################################################################################

cat("\n============================================================\n")
cat("Step 8: Internet penetration integration\n")
cat("============================================================\n")

# --- 8a: Load WDI internet data (173 countries) ---
df_inet_raw <- read_csv("WDI data/internet penetration/3d38d8b0-c9d5-4ab9-a240-dee4c0547b26_Data.csv")

year_cols_inet <- grep("^\\d{4}", names(df_inet_raw), value = TRUE)

df_inet_long <- df_inet_raw %>%
  dplyr::select(`Country Name`, all_of(year_cols_inet)) %>%
  pivot_longer(cols = all_of(year_cols_inet), names_to = "Year", values_to = "internet_pct") %>%
  mutate(
    Year         = as.integer(str_extract(Year, "^\\d{4}")),
    internet_pct = as.numeric(na_if(internet_pct, ".."))
  ) %>%
  rename(wdi_name = `Country Name`) %>%
  mutate(country = ifelse(wdi_name %in% names(wdi_to_w4), wdi_to_w4[wdi_name], wdi_name)) %>%
  dplyr::select(country, Year, internet_pct)

# --- 8b: North Korea — 0% for all years ---
# WDI reports 0% through 2012, then stops. Reality unchanged: no public internet.
df_inet_nk <- tibble(
  country      = "North Korea",
  Year         = 2007:2020,
  internet_pct = 0
)

# --- 8c: Taiwan — from ITU / Taiwan National Communications Commission ---
# Sources: ITU World Telecommunication/ICT Indicators, Taiwan NCC annual reports.
df_inet_tw <- tibble(
  country      = "Taiwan",
  Year         = 2007:2020,
  internet_pct = c(64.4, 65.5, 65.9, 71.5, 72.0, 75.4, 76.0, 78.0,
                   78.0, 82.3, 82.3, 89.4, 90.7, 90.0)
)

# --- 8d: Zanzibar — use Tanzania values (Zanzibar is part of Tanzania in WDI) ---
df_inet_tz <- df_inet_long %>%
  filter(country == "Tanzania") %>%
  mutate(country = "Zanzibar")

# --- 8e: Combine all internet data ---
# Remove NK placeholder from WDI (has 0s and NAs), replace with our clean version
df_inet_long <- df_inet_long %>% filter(country != "North Korea")

df_inet_all <- bind_rows(df_inet_long, df_inet_nk, df_inet_tw, df_inet_tz) %>%
  filter(!is.na(internet_pct))

# Merge into df_model
df_model <- df_model %>%
  left_join(df_inet_all, by = c("attacker" = "country", "Year" = "Year")) %>%
  rename(attacker_internet = internet_pct) %>%
  left_join(df_inet_all, by = c("victim" = "country", "Year" = "Year")) %>%
  rename(victim_internet = internet_pct)

cat("  Internet coverage (attacker): ", sprintf("%.1f%%", 100 * mean(!is.na(df_model$attacker_internet))), "\n")
cat("  Internet coverage (victim):   ", sprintf("%.1f%%", 100 * mean(!is.na(df_model$victim_internet))), "\n")


################################################################################
#            Step 9: SAVE df_model2020.csv (GDP + Internet, 2007-2020)
################################################################################

cat("\n============================================================\n")
cat("Step 9: Save df_model2020.csv\n")
cat("============================================================\n")

write.csv(df_model, "df_model2020.csv", row.names = FALSE)

df_2020_final <- df_model %>%
  filter(!is.na(attacker_w4) & !is.na(victim_w4))

cat(sprintf("  Total observations:           %d\n", nrow(df_model)))
cat(sprintf("  Regression-ready (non-NA W4): %d\n", nrow(df_2020_final)))
cat(sprintf("  Non-zero incidents:           %d\n", sum(df_2020_final$Incident_Count > 0)))
cat(sprintf("  GDP available (both sides):   %d (%.1f%%)\n",
            sum(!is.na(df_2020_final$attacker_ln_gdp_pc) & !is.na(df_2020_final$victim_ln_gdp_pc)),
            100 * mean(!is.na(df_2020_final$attacker_ln_gdp_pc) & !is.na(df_2020_final$victim_ln_gdp_pc))))
cat(sprintf("  Internet available (both):    %d (%.1f%%)\n",
            sum(!is.na(df_2020_final$attacker_internet) & !is.na(df_2020_final$victim_internet)),
            100 * mean(!is.na(df_2020_final$attacker_internet) & !is.na(df_2020_final$victim_internet))))
cat("  Saved: df_model2020.csv\n")


################################################################################
#                  Step 10: CINC (Composite Index of National Capability)
#
# Source: COW National Material Capabilities v6.0 (1816-2016)
# Download: https://correlatesofwar.org/data-sets/national-material-capabilities/
# File: NMC-60-abridged.csv
################################################################################

cat("\n============================================================\n")
cat("Step 10: CINC integration + Save df_model2016.csv\n")
cat("============================================================\n")

df_nmc <- read_csv("COW NMC/NMC-60-abridged/NMC-60-abridged.csv")

# NMC uses COW country codes (ccode). We need to map to W4 country names.
# Load COW country codes for the mapping.
cow_codes <- read_csv("DCID original dataset and COW codes/COW-country-codes.csv") %>%
  distinct(CCode, .keep_all = TRUE)

# Map NMC ccodes to COW country names, then to W4 names
df_cinc <- df_nmc %>%
  filter(year >= 2007 & year <= 2016) %>%
  dplyr::select(ccode, year, cinc) %>%
  left_join(cow_codes, by = c("ccode" = "CCode")) %>%
  rename(cow_name = StateNme)

# Some COW names differ from W4 names. Apply the same corrections used in
# descriptive_statistics.R (COW -> W4 name mapping).
cow_to_w4 <- c(
  "Congo"      = "Republic of the Congo",
  "Gambia"     = "The Gambia",
  "Myanmar"    = "Burma/Myanmar",
  "East Timor" = "Timor-Leste",
  "Macedonia"  = "North Macedonia",
  "Swaziland"  = "Eswatini",
  "Yugoslavia" = "Serbia"             # COW ccode 345; NMC labels it YUG post-2006
)

df_cinc <- df_cinc %>%
  mutate(country = ifelse(cow_name %in% names(cow_to_w4), cow_to_w4[cow_name], cow_name)) %>%
  dplyr::select(country, Year = year, cinc) %>%
  filter(!is.na(cinc))

# Verify coverage
cinc_countries  <- unique(df_cinc$country)
missing_cinc    <- setdiff(all_w4_countries, cinc_countries)
cat("  CINC countries matched: ", length(intersect(all_w4_countries, cinc_countries)), "\n")
cat("  Missing from CINC:     ", length(missing_cinc), "\n")
if (length(missing_cinc) > 0) cat("    ", paste(missing_cinc, collapse = ", "), "\n")

# Build 2007-2016 model dataset
df_model_2016 <- df_model %>%
  filter(Year >= 2007 & Year <= 2016) %>%
  # Attacker CINC
  left_join(df_cinc, by = c("attacker" = "country", "Year" = "Year")) %>%
  rename(attacker_cinc = cinc) %>%
  # Victim CINC
  left_join(df_cinc, by = c("victim" = "country", "Year" = "Year")) %>%
  rename(victim_cinc = cinc)

write.csv(df_model_2016, "df_model2016.csv", row.names = FALSE)

df_2016_final <- df_model_2016 %>%
  filter(!is.na(attacker_w4) & !is.na(victim_w4))

cat(sprintf("  Total observations (2007-2016): %d\n", nrow(df_model_2016)))
cat(sprintf("  Regression-ready (non-NA W4):   %d\n", nrow(df_2016_final)))
cat(sprintf("  Non-zero incidents:             %d\n", sum(df_2016_final$Incident_Count > 0)))
cat(sprintf("  CINC available (both sides):    %d (%.1f%%)\n",
            sum(!is.na(df_2016_final$attacker_cinc) & !is.na(df_2016_final$victim_cinc)),
            100 * mean(!is.na(df_2016_final$attacker_cinc) & !is.na(df_2016_final$victim_cinc))))
cat(sprintf("  Internet available (both):      %d (%.1f%%)\n",
            sum(!is.na(df_2016_final$attacker_internet) & !is.na(df_2016_final$victim_internet)),
            100 * mean(!is.na(df_2016_final$attacker_internet) & !is.na(df_2016_final$victim_internet))))
cat("  Saved: df_model2016.csv\n")


################################################################################
#                          SUMMARY
################################################################################

cat("\n\n============================================================\n")
cat("                  DATA PREPARATION SUMMARY\n")
cat("============================================================\n\n")

cat("df_model2020.csv (2007-2020, GDP specification):\n")
cat("  Variables: Incident_Count, attacker_w4, victim_w4,\n")
cat("             attacker_ln_gdp_pc, victim_ln_gdp_pc,\n")
cat("             attacker_internet, victim_internet,\n")
cat("             attacker_is_nato, victim_is_nato\n")
cat("  Use for: Spec B — W4 + GDP + Internet + NATO + Year FE\n\n")

cat("df_model2016.csv (2007-2016, CINC specification):\n")
cat("  Variables: all of the above PLUS\n")
cat("             attacker_cinc, victim_cinc\n")
cat("  Use for: Spec A — W4 + CINC + Internet + NATO + Year FE\n\n")

cat("Notes:\n")
cat("  - North Korea GDP: BOK GNI estimates, chain-linked (constant 2015 USD proxy).\n")
cat("  - Taiwan GDP: DGBAS national accounts, chain-linked (constant 2015 USD).\n")
cat("  - North Korea internet: coded 0% (no public internet access).\n")
cat("  - Taiwan internet: ITU/NCC estimates.\n")
cat("  - Zanzibar internet: Tanzania values (Zanzibar is part of Tanzania in WDI).\n")
cat("  - Zanzibar GDP: not available (Zanzibar has no DCID incidents).\n")
cat("  - CINC: COW NMC v6.0, covers 1816-2016.\n")
cat("============================================================\n")

