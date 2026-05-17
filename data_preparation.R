##################### Environment Setup ########################################
rm(list = ls())
try(dev.off(), silent = TRUE)
options(scipen = 999)
cat("\014")

library(readxl)
library(tidyverse)
library(tidyr)
library(dplyr)
library(readr)
library(stringr)

# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# setwd(dirname(rstudioapi::getSourceEditorContext()$path))

##################### 1. Load and clean DCID incident data #####################
# Raw sources: DCID 2.0 + COW country codes
df_cyber_inc <- read_excel("data sources/DCID_2.0_Release_update_February_2023.xlsx")
country_codes <- read_csv("data sources/COW-country-codes.csv")

# Remove any leading or trailing whitespaces
df_cyber_inc <- df_cyber_inc %>%
    mutate(
        StateA = trimws(StateA),
        StateB = trimws(StateB)
    )

# Keep only the first entry for each 'CCode' because some of them repeat
country_codes_unique <- country_codes %>%
    distinct(CCode, .keep_all = TRUE)

# Add 'duration_days' and 'Year' (start year of the incident) columns
df_cyber_inc_timings <-
    df_cyber_inc %>%
    mutate(
        duration_days = as.numeric(difftime(interactionenddate, interactionstartdate, units = "days")),
        Year = as.numeric(format(interactionstartdate, "%Y"))
    ) %>%
    filter(duration_days >= 0) # there is one bad observation with negative duration so we need to remove it

# Add 'before' and 'after' 2016 indicators
# cut_date <- as.Date("2016-07-09") # https://www.nato.int/cps/en/natohq/official_texts_133177.htm
# df_cyber_inc_timings <- df_cyber_inc_timings %>%
#     mutate(
#         period_2016 = ifelse(interactionstartdate < cut_date, "Before July 2016", "After July 2016")
#     )

# Change the country codes to country names for attackers and victims
df_cyber_inc_names <-
    df_cyber_inc_timings %>%
    # Join with country codes to get the attacker's name (`initiator`)
    left_join(country_codes_unique, by = c("initiator" = "CCode")) %>%
    # Rename the original column name to 'attacker'
    rename(attacker = StateNme) %>%
    mutate(
        victim = case_when(
            # Conditions where the attacker matches StateA -- then the victim must be StateB.
            (attacker == StateA) |
                (attacker == "United States of America" & StateA == "US") |
                (attacker == "North Korea" & StateA == "N Korea") |
                (attacker == "South Korea" & StateA == "S Korea") ~ StateB,
            # Conditions where the attacker matches StateB -- then the victim must be StateA.
            (attacker == StateB) |
                (attacker == "United States of America" & StateB == "US") |
                (attacker == "North Korea" & StateB == "N Korea") |
                (attacker == "South Korea" & StateB == "S Korea") ~ StateA,
            # Otherwise mark victim NA
            TRUE ~ NA_character_
        )
    )

# Show which rows will be dropped (attacker didn't match either StateA or StateB)
# dropped_rows <- df_cyber_inc_names %>% filter(is.na(victim))
# cat("Rows removed:", nrow(dropped_rows), "\n")
# if (nrow(dropped_rows) > 0) {
#   dropped_rows %>%
#     dplyr::select(Name, StateA, StateB, attacker, initiator) %>%
#     print()
# }

df_cyber_inc_names <- df_cyber_inc_names %>%
    filter(!is.na(victim)) %>%
    # Standardize victim country names to match attacker names
    mutate(
        victim = case_when(
            victim == "US" ~ "United States of America",
            victim == "N Korea" ~ "North Korea",
            victim == "S Korea" ~ "South Korea",
            victim == "UK" ~ "United Kingdom",
            TRUE ~ victim
        )
    )

# Create a column 'rivals' for dyad pair but avoid duplicates
# (e.g. US-China and China-US are the same rivals pair so we need to keep only one pair)
# df_cyber_inc_names <-
#     df_cyber_inc_names %>%
#     mutate(
#         rivals = mapply(function(a, b) {
#             paste(sort(c(a, b)), collapse = "-")
#         }, StateA, StateB)
#     )

# # NATO membership indicators based on 'Year'
# # Uncomment this block if NATO analysis is needed
# df_cyber_inc_names <-
#   df_cyber_inc_names %>%
#   mutate(
#     # Check if attacker was in NATO in that Year
#     attacker_is_nato = case_when(
#       attacker %in% c("United States of America", "United Kingdom", "France", "Canada", "Italy", "Netherlands", "Belgium", "Norway", "Denmark", "Portugal", "Iceland", "Luxembourg") & Year >= 1949 ~ 1,
#       attacker %in% c("Greece", "Turkey") & Year >= 1952 ~ 1,
#       attacker == "Germany" & Year >= 1955 ~ 1,
#       attacker == "Spain" & Year >= 1982 ~ 1,
#       attacker %in% c("Poland", "Czech Republic", "Hungary") & Year >= 1999 ~ 1,
#       attacker %in% c("Bulgaria", "Romania", "Slovakia", "Slovenia", "Estonia", "Latvia", "Lithuania") & Year >= 2004 ~ 1,
#       attacker %in% c("Croatia", "Albania") & Year >= 2009 ~ 1,
#       attacker == "Montenegro" & Year >= 2017 ~ 1,
#       attacker == "North Macedonia" & Year >= 2020 ~ 1,
#       TRUE ~ 0
#     ),
#
#     # Check if victim was in NATO in that Year
#     victim_is_nato = case_when(
#       victim %in% c("United States of America", "United Kingdom", "France", "Canada", "Italy", "Netherlands", "Belgium", "Norway", "Denmark", "Portugal", "Iceland", "Luxembourg") & Year >= 1949 ~ 1,
#       victim %in% c("Greece", "Turkey") & Year >= 1952 ~ 1,
#       victim == "Germany" & Year >= 1955 ~ 1,
#       victim == "Spain" & Year >= 1982 ~ 1,
#       victim %in% c("Poland", "Czech Republic", "Hungary") & Year >= 1999 ~ 1,
#       victim %in% c("Bulgaria", "Romania", "Slovakia", "Slovenia", "Estonia", "Latvia", "Lithuania") & Year >= 2004 ~ 1,
#       victim %in% c("Croatia", "Albania") & Year >= 2009 ~ 1,
#       victim == "Montenegro" & Year >= 2017 ~ 1,
#       victim == "North Macedonia" & Year >= 2020 ~ 1,
#       TRUE ~ 0
#     )
#   )
# # "Finland" and "Sweden" joined at 2023-2024, and we have no data for that period

# Select relevant columns and decode using Codebook
df_cyber_inc_clean <-
    df_cyber_inc_names %>%
    dplyr::select(
        Name, attacker, victim, Year, cyber_objective
        # Uncomment below if additional columns are needed:
        # `3rdpartyinitiator`, interactionstartdate, interactionenddate,
        # duration_days, period_2016, method, targettype,
        # information_operation, objective_achievement, Concession, severity,
        # `damage type`, `Crit Inf`, `Supply Ch`, Ransomware,
        # `Political Objective`, Justice, Cert, FBI
        # Sources1, Sources2, Sources3, Sources4, Sources5,
        # attacker_is_nato, victim_is_nato,
    ) %>%
    mutate(
        # Decode 'cyber_objective' column (using Codebook pp. 6-7)
        cyber_objective = case_when(
            cyber_objective == 1 ~ "Disruption",
            cyber_objective == 2 ~ "Short-term espionage",
            cyber_objective == 3 ~ "Long-term espionage",
            cyber_objective == 4 ~ "Degrade",
            cyber_objective == 5 ~ "Influence"
        )
        # Uncomment and add decoding for additional columns as needed:
        # method = case_when(
        #   method == 1 ~ "Vandalism (website defacements, propaganda)",
        #   method == 2 ~ "DDoS, Botnets",
        #   method == 3 | method == 3.1 ~ "Network Intrusion (Trojans, Backdoors)",
        #   method == 4.1 ~ "Network Infiltration (Logic bombs)",
        #   method == 4.2 ~ "Network Infiltration (Viruses)",
        #   method == 4.3 ~ "Network Infiltration (Worms)",
        #   method == 4.4 ~ "Network Infiltration (Keystroke logging)"),
        # targettype = case_when(
        #   targettype == 1 ~ "Private/non-state",
        #   targettype == 2 ~ "Government non-military",
        #   targettype == 3 ~ "Government military"),
        # information_operation = ifelse(information_operation == 1, "Yes", "No"),
        # objective_achievement = ifelse(objective_achievement == 1, "Yes", "No"),
        # Concession = ifelse(Concession == 1, "Yes", "No"),
        # `3rdpartyinitiator` = ifelse(`3rdpartyinitiator` == 1, "Yes", "No"),
        # `Supply Ch` = ifelse(`Supply Ch` == 1, "Yes", "No"),
        # Ransomware = ifelse(Ransomware == 1, "Yes", "No"),
        # `damage type` = case_when(
        #   `damage type` == 1 ~ "Direct and immediate",
        #   `damage type` == 2 ~ "Direct and delayed",
        #   `damage type` == 3 ~ "Indirect and immediate",
        #   `damage type` == 4 ~ "Indirect and delayed"),
        # `Crit Inf` = case_when(
        #   `Crit Inf` == 1 ~ "Chemical Sector",
        #   `Crit Inf` == 2 ~ "Commercial Facilities Sector",
        #   `Crit Inf` == 3 ~ "Communications Sector",
        #   `Crit Inf` == 4 ~ "Critical Manufacturing Sector",
        #   `Crit Inf` == 5 ~ "Dams Sector",
        #   `Crit Inf` == 6 ~ "Defense Industrial Base Sector",
        #   `Crit Inf` == 7 ~ "Emergency Services Sector",
        #   `Crit Inf` == 8 ~ "Energy Sector",
        #   `Crit Inf` == 9 ~ "Financial Services Sector",
        #   `Crit Inf` == 10 ~ "Food and Agriculture Sector",
        #   `Crit Inf` == 11 ~ "Government Facilities Sector",
        #   `Crit Inf` == 12 ~ "Healthcare and Public Health Sector",
        #   `Crit Inf` == 13 ~ "Information Technology Sector",
        #   `Crit Inf` == 14 ~ "Nuclear Reactors, Materials, and Waste Sector",
        #   `Crit Inf` == 15 ~ "Transportation Systems Sector",
        #   `Crit Inf` == 16 ~ "Water and Wastewater Systems Sector",
        #   `Crit Inf` == 17 ~ "Other (Election Infrastructure, Academia)")
    )

##################### 2. Aggregate incident counts #############################
# Each incident is counted once, in its start year:
# Collapses multiple incidents into one row. For example, if Russia attacked the US 3 times in 2015,
# that's 3 rows in the original DCID data but becomes 1 row in dyad_year_counts with Incident_Count = 3
dyad_year_counts <- df_cyber_inc_clean %>%
    filter(Year >= 2007 & Year <= 2020) %>%
    group_by(attacker, victim, Year) %>%
    summarise(Incident_Count = n(), .groups = "drop")
nrow(dyad_year_counts) # Aggregated incident counts (attacker-victim-year combinations): 205

# sum(dyad_year_counts$Incident_Count) # Total incidents: 398
# nrow(df_cyber_inc) # 429
# df_cyber_inc_clean %>% filter(Year < 2007 | Year > 2020) %>% nrow() # 26
# Incidents before 2007: 26
# Out of total 424 incidents it's 6.1%

##################### 3. Load W4 (winning coalition index)#######################
df_w4 <- read_csv("data sources/NewWmeasure.csv")

df_w4_clean <- df_w4 %>%
    dplyr::select(w4_country_name = country_name, w4_year = year, w4_score = W4) %>%
    filter(w4_year >= 2007 & w4_year <= 2020)

n_distinct(df_w4_clean$w4_country_name) # 175 countries
# nrow(df_w4_clean) # 2446 country-year observations.

# Verify: DCID country names vs W4 country names
all_cyber_countries <- unique(c(df_cyber_inc_clean$attacker, df_cyber_inc_clean$victim))
all_w4_countries <- unique(df_w4_clean$w4_country_name)
missing_from_w4 <- setdiff(all_cyber_countries, all_w4_countries)
length(missing_from_w4) # 0 DCID countries missing from W4

##################### 4. Build dyad-year base (2007-2020) ######################
# Using ALL countries with W4 data (not just DCID countries) to avoid sample
# selection bias. Countries not involved in any DCID incident contribute
# informative zero observations.
all_years <- 2007:2020

df_base <- expand_grid(
    attacker = all_w4_countries,
    victim   = all_w4_countries,
    Year     = all_years
) %>%
    filter(attacker != victim)

# Dyad-year base: 175 attackers x 175 victims x 14 years = 426300 directed-dyad-year obs.
n_distinct(all_w4_countries) # 175
length(all_years) # 14
nrow(df_base) # 426300

########### 5. Merge incident counts and W4 scores for all countries ###########
# Merge incident counts: dyads with no incidents get NA -> replaced with 0 (zero inflation).
# And merge W4 scores for both attacker and victim
df_model <- df_base %>%
    left_join(dyad_year_counts, by = c("attacker", "victim", "Year")) %>%
    mutate(Incident_Count = replace_na(Incident_Count, 0)) %>%
    # W4 for attacker
    left_join(df_w4_clean,
        by = c("attacker" = "w4_country_name", "Year" = "w4_year")
    ) %>%
    rename(attacker_w4 = w4_score) %>%
    # W4 for victim
    left_join(df_w4_clean,
        by = c("victim" = "w4_country_name", "Year" = "w4_year")
    ) %>%
    rename(victim_w4 = w4_score)
nrow(df_model) # 426300

##################### 6. Load and merge GDP per capita #########################
# Sources:
#   - WDI (NY.GDP.PCAP.KD) for most countries (constant 2015 USD)
#   - Bank of Korea (BOK) for North Korea
#   - National Statistics, R.O.C. for Taiwan

df_gdp_raw <- read_csv("data sources/a14efc5a-7fe8-4ea4-b8ed-57b7727e3394_Data.csv")
year_cols <- grep("^\\d{4}", names(df_gdp_raw), value = TRUE)

# WDI -> W4 country name mapping
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

# Reshape from wide to long, clean year labels and missing values ("..")
df_gdp_long <- df_gdp_raw %>%
    dplyr::select(`Country Name`, all_of(year_cols)) %>%
    pivot_longer(cols = all_of(year_cols), names_to = "Year", values_to = "gdp_pc") %>%
    mutate(
        Year   = as.integer(str_extract(Year, "^\\d{4}")),
        gdp_pc = as.numeric(na_if(gdp_pc, ".."))
    )

nrow(df_gdp_long %>% filter(is.na(gdp_pc))) # 195 country-years with missing GDP values
df_gdp_long <- df_gdp_long %>% filter(!is.na(gdp_pc))

# Map WDI country names to W4 country names (e.g. "Russian Federation" -> "Russia")
df_gdp_long <- df_gdp_long %>%
    rename(wdi_name = `Country Name`) %>%
    mutate(country = ifelse(wdi_name %in% names(wdi_to_w4), wdi_to_w4[wdi_name], wdi_name)) %>%
    dplyr::select(country, Year, gdp_pc)

# North Korea GNI: chain-link using BOK real GDP growth rates and population
nk_raw <- read_excel("data sources/Indicators_NK.xlsx", skip = 2)
nk_data <- nk_raw %>%
    filter(trimws(Indicator) %in% c("Real GDP growth", "Population")) %>%
    dplyr::select(-Unit) %>%
    pivot_longer(cols = -Indicator, names_to = "Year", values_to = "value") %>%
    mutate(Indicator = trimws(Indicator), Year = as.integer(Year)) %>%
    pivot_wider(names_from = Indicator, values_from = value) %>%
    rename(growth_pct = `Real GDP growth`, pop_k = Population) %>%
    filter(Year >= 2007 & Year <= 2020)

# Anchor: 2015 per capita GNI = 1,393,000 KRW / 1,130.95 KRW/USD = $1,232
# Exchange rate source: World Bank (PA.NUS.FCRF) for South Korea, 2015
nk_anchor_year <- 2015
nk_anchor_pc <- 1232

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

nk_anchor_pop <- nk_data$pop_k[nk_data$Year == nk_anchor_year]
nk_data$gdp_pc <- nk_anchor_pc * nk_data$real_gdp_index * (nk_anchor_pop / nk_data$pop_k)

df_gdp_nk <- nk_data %>%
    dplyr::select(Year, gdp_pc) %>%
    mutate(country = "North Korea")

# Taiwan GDP: chain-link using real GDP growth rates and population
tw_raw <- read_csv("data sources/E018101010_002034281.csv", skip = 2, n_max = 14, show_col_types = FALSE) # reads exactly the 14 data rows (2007–2020) and stops before the empty rows and footnotes at the bottom
tw_data <- tw_raw %>%
    dplyr::select(
        Year       = Period,
        pop        = `Population (Mid-Year,Persons)`,
        growth_pct = `GDP Growth Rate (%)`,
        gdp_pc_usd = `Per Capita GDP ( U.S.$,at Current Prices )`
    ) %>%
    mutate(across(everything(), as.numeric)) %>%
    filter(Year >= 2007 & Year <= 2020)

# Anchor: 2015 per capita GDP at current prices from the source file
tw_anchor_year <- 2015
tw_anchor_pc <- tw_data$gdp_pc_usd[tw_data$Year == tw_anchor_year]

tw_data$real_gdp_index <- NA
tw_data$real_gdp_index[tw_data$Year == tw_anchor_year] <- 1

# Forward from 2015
for (i in which(tw_data$Year == tw_anchor_year):nrow(tw_data)) {
    if (i < nrow(tw_data)) {
        tw_data$real_gdp_index[i + 1] <- tw_data$real_gdp_index[i] * (1 + tw_data$growth_pct[i + 1] / 100)
    }
}
# Backward from 2015
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

# Combine all GDP data
df_gdp_all <- bind_rows(df_gdp_long, df_gdp_nk, df_gdp_tw) %>%
    mutate(ln_gdp_pc = log(gdp_pc))

# Verify that all W4 countries have GDP data
missing_gdp <- setdiff(all_w4_countries, unique(df_gdp_all$country))
cat("W4 countries missing from GDP data:", length(missing_gdp), "\n")
if (length(missing_gdp) > 0) cat("  ", paste(missing_gdp, collapse = ", "), "\n")
# Zanzibar is missing; it is included in data for Tanzania

# Merge GDP into df_model
df_model <- df_model %>%
    left_join(df_gdp_all, by = c("attacker" = "country", "Year" = "Year")) %>%
    rename(attacker_gdp_pc = gdp_pc, attacker_ln_gdp_pc = ln_gdp_pc) %>%
    left_join(df_gdp_all, by = c("victim" = "country", "Year" = "Year")) %>%
    rename(victim_gdp_pc = gdp_pc, victim_ln_gdp_pc = ln_gdp_pc)

# ##################### 7. Load and merge Internet Penetration ###################
# # Commented out: not used in current models (GDP and Internet are collinear, r = 0.85)
# # Uncomment if Internet Penetration is needed as a control variable
#
# df_inet_raw <- read_csv("data sources/3d38d8b0-c9d5-4ab9-a240-dee4c0547b26_Data.csv")
# year_cols_inet <- grep("^\\d{4}", names(df_inet_raw), value = TRUE)
#
# df_inet_long <- df_inet_raw %>%
#     dplyr::select(`Country Name`, all_of(year_cols_inet)) %>%
#     pivot_longer(cols = all_of(year_cols_inet), names_to = "Year", values_to = "internet_pct") %>%
#     mutate(
#         Year         = as.integer(str_extract(Year, "^\\d{4}")),
#         internet_pct = as.numeric(na_if(internet_pct, ".."))
#     ) %>%
#     rename(wdi_name = `Country Name`) %>%
#     mutate(country = ifelse(wdi_name %in% names(wdi_to_w4), wdi_to_w4[wdi_name], wdi_name)) %>%
#     dplyr::select(country, Year, internet_pct)
#
# df_inet_nk <- tibble(
#     country      = "North Korea",
#     Year         = 2007:2020,
#     internet_pct = 0
# )
#
# df_inet_tw_raw <- read_csv("data sources/Individuals using the Internet.csv")
# df_inet_tw <- df_inet_tw_raw %>%
#     pivot_longer(cols = -Economy, names_to = "Year", values_to = "internet_pct") %>%
#     mutate(
#         Year         = as.integer(Year),
#         internet_pct = as.numeric(str_remove(internet_pct, "%")),
#         country      = "Taiwan"
#     ) %>%
#     dplyr::select(country, Year, internet_pct)
# df_inet_tw <- df_inet_tw %>% filter(!is.na(internet_pct))
#
# df_inet_tz <- df_inet_long %>%
#     filter(country == "Tanzania") %>%
#     mutate(country = "Zanzibar")
#
# df_inet_long <- df_inet_long %>% filter(country != "North Korea")
# df_inet_all <- bind_rows(df_inet_long, df_inet_nk, df_inet_tw, df_inet_tz)
# df_inet_all <- df_inet_all %>% filter(!is.na(internet_pct))
#
# df_model <- df_model %>%
#     left_join(df_inet_all, by = c("attacker" = "country", "Year" = "Year")) %>%
#     rename(attacker_internet = internet_pct) %>%
#     left_join(df_inet_all, by = c("victim" = "country", "Year" = "Year")) %>%
#     rename(victim_internet = internet_pct)

##################### 7. Save df_model_2020.csv (W4 + GDP, 2007-2020) #########
# Filter to complete cases
complete <- complete.cases(
    df_model$attacker_w4, df_model$victim_w4,
    df_model$attacker_ln_gdp_pc, df_model$victim_ln_gdp_pc
)

df_dropped_2020 <- df_model[!complete, ]
nrow(df_dropped_2020)
cat("Countries missing data (W4/GDP, 2007-2020):", n_distinct(df_dropped_2020$attacker[
    is.na(df_dropped_2020$attacker_w4) |
        is.na(df_dropped_2020$attacker_ln_gdp_pc)
]), "\n")

cat(sprintf(
    "df_model_2020 — Kept: %d obs (%d cyber incidents) | Dropped: %d obs (%d cyber incidents)\n",
    sum(complete), sum(df_model$Incident_Count[complete] > 0),
    sum(!complete), sum(df_model$Incident_Count[!complete] > 0)
))

df_2020 <- df_model[complete, ]

write.csv(df_2020, "outputs/df_model_2020.csv", row.names = FALSE)


##################### 8. Load CINC and save df_model_2016.csv ##################
# NMC uses COW country codes (ccode). Map to W4 country names.
df_nmc <- read_csv("data sources/NMC-60-abridged.csv")
cow_codes <- read_csv("data sources/COW-country-codes.csv") %>%
    distinct(CCode, .keep_all = TRUE)

# COW -> W4 name mapping (different from WDI mapping)
cow_to_w4 <- c(
    "Congo"      = "Republic of the Congo",
    "Gambia"     = "The Gambia",
    "Myanmar"    = "Burma/Myanmar",
    "East Timor" = "Timor-Leste",
    "Macedonia"  = "North Macedonia",
    "Swaziland"  = "Eswatini",
    "Yugoslavia" = "Serbia" # COW ccode 345; NMC labels it YUG post-2006
)

df_cinc <- df_nmc %>%
    filter(year >= 2007 & year <= 2016) %>%
    dplyr::select(ccode, year, cinc) %>%
    left_join(cow_codes, by = c("ccode" = "CCode")) %>%
    mutate(country = ifelse(StateNme %in% names(cow_to_w4), cow_to_w4[StateNme], StateNme)) %>%
    dplyr::select(country, Year = year, cinc) %>%
    filter(!is.na(cinc))

# Verify: W4 countries vs CINC coverage
missing_cinc <- setdiff(all_w4_countries, unique(df_cinc$country))
cat("W4 countries missing from CINC data:", length(missing_cinc), "\n") # 1
if (length(missing_cinc) > 0) cat("  ", paste(missing_cinc, collapse = ", "), "\n") # Zanzibar
# Zanzibar (no CINC/GDP data, zero incidents) is dropped previously at complete-cases filter.

# Build 2007-2016 dataset with CINC and GDP
df_2016_base <- df_model %>%
    filter(Year >= 2007 & Year <= 2016) %>%
    left_join(df_cinc, by = c("attacker" = "country", "Year" = "Year")) %>%
    rename(attacker_cinc = cinc) %>%
    left_join(df_cinc, by = c("victim" = "country", "Year" = "Year")) %>%
    rename(victim_cinc = cinc)

# Filter to complete cases
complete_2016 <- complete.cases(
    df_2016_base$attacker_w4, df_2016_base$victim_w4,
    df_2016_base$attacker_cinc, df_2016_base$victim_cinc,
    df_2016_base$attacker_ln_gdp_pc, df_2016_base$victim_ln_gdp_pc
)

df_dropped_2016 <- df_2016_base[!complete_2016, ]
nrow(df_dropped_2016) # Rows with incomplete data:  5208
cat(
    "Countries missing data (W4/CINC, 2007-2016):", # 3
    n_distinct(df_dropped_2016$attacker[is.na(df_dropped_2016$attacker_w4) |
        is.na(df_dropped_2016$attacker_cinc)]), "\n"
)
cat(sprintf(
    "df_model_2016 — Kept: %d obs (%d incidents) | Dropped: %d obs (%d incidents)\n",
    sum(complete_2016), sum(df_2016_base$Incident_Count[complete_2016] > 0),
    sum(!complete_2016), sum(df_2016_base$Incident_Count[!complete_2016] > 0)
))
# Kept: 299292 obs (136 incidents) | Dropped: 5208 obs (0 incidents)

df_2016 <- df_2016_base[complete_2016, ]

write.csv(df_2016, "outputs/df_model_2016.csv", row.names = FALSE)


##################### 9. Build clean panel and save df_model_2014.csv ##########
# The "purely cyber, purely unprovoked" panel.
# Three exclusion steps:
#   (a) 7 ACD incidents (cyber-to-cyber retaliation per Tallinn Manual)
#   (b) 40 incidents in dyad-years with MID hostility >= 4 (use of force)
#   (c) 122 kinetic dyad-years dropped from panel entirely (no false zeros)
#
# MID dyadic v4.03 covers through 2014 → panel period is 2007-2014.
#
# Sources:
#   - DCID 2.0 (already loaded in Section 1 as df_cyber_inc)
#   - COW Dyadic MID v4.03

cat("\n\n############################################################\n")
cat("#  Section 9: Build df_model_2014 (clean panel)            #\n")
cat("############################################################\n\n")

# --- 9a. Identify ACD incidents ---
# 7 incidents where cyber operations were a direct response to a PRIOR
# cyber operation by the victim. Based on manual review of Political
# Objective field in DCID, using the Tallinn Manual definition.

acd_names <- c(
    "Buckshot Yankee", # US defensive response to Russian intrusion
    "Osinform", # Georgia retaliating against Russian hackers
    "November 2008 defacements_B", # Pakistan retaliating for Indian defacement
    "Japan retaliation", # Japan retaliating for South Korean DDoS
    "September 2010 defacements 2", # India retaliating for Pakistani defacements
    "PCA retaliation", # India retaliating for Pakistani defacements
    "NK retaliation" # NK retaliating for Anti-Kim cyber disruption
)

df_cyber_inc_clean$is_acd <- df_cyber_inc_clean$Name %in% acd_names
cat("ACD incidents identified (Tallinn definition):", sum(df_cyber_inc_clean$is_acd), "\n")

# --- 9b. Load MID dyadic data and identify kinetic dyad-years ---
df_mid <- read_csv("data sources/dyadic_mid_4.03.csv", show_col_types = FALSE)

# Dyad-years with use of force or war (hostility >= 4)
# Threshold rationale: level 3 (display of force) includes naval standoffs
# like US-China South China Sea incidents, which are unrelated to cyber
# espionage campaigns. Level 4 (actual use of force) captures genuine
# kinetic conflict: Russia-Georgia 2008, Russia-Ukraine 2014, etc.
mid_force <- df_mid %>%
    filter(year >= 2007, year <= 2014, hihost >= 4) %>%
    dplyr::select(statea, stateb, year) %>%
    distinct()

cat("MID dyad-years with hostility >= 4 (2007-2014):", nrow(mid_force), "\n")

# Map DCID victim names to COW codes for MID matching
# (attacker COW codes are already in df_cyber_inc_clean$initiator from Section 1)
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

# Prepare incident-level data for MID matching
df_inc_for_mid <- df_cyber_inc_clean %>%
    filter(Year >= 2007 & Year <= 2014) %>%
    mutate(
        attacker_cow = as.integer(
            cow_name_to_code[attacker]
        ),
        victim_cow = as.integer(
            cow_name_to_code[victim]
        )
    )

# Flag incidents where the attacker-victim dyad had active use of force
df_inc_for_mid$mid_force <- mapply(function(a, v, y) {
    if (is.na(a) || is.na(v)) {
        return(FALSE)
    }
    nrow(mid_force %>% filter(
        (statea == a & stateb == v & year == y) |
            (statea == v & stateb == a & year == y)
    )) > 0
}, df_inc_for_mid$attacker_cow, df_inc_for_mid$victim_cow, df_inc_for_mid$Year)

df_inc_for_mid$exclude <- df_inc_for_mid$is_acd | df_inc_for_mid$mid_force

n_acd <- sum(df_inc_for_mid$is_acd)
n_mid <- sum(df_inc_for_mid$mid_force)
n_overlap <- sum(df_inc_for_mid$is_acd & df_inc_for_mid$mid_force)
n_excluded <- sum(df_inc_for_mid$exclude)
n_kept <- sum(!df_inc_for_mid$exclude)

cat(sprintf(
    "ACD: %d | MID-linked: %d | Overlap: %d | Total excluded: %d | Kept: %d\n",
    n_acd, n_mid, n_overlap, n_excluded, n_kept
))

# List overlap incidents (both ACD and in kinetic dyad-year)
if (n_overlap > 0) {
    cat("Overlap incidents (ACD + kinetic conflict):\n")
    df_inc_for_mid %>%
        filter(is_acd & mid_force) %>%
        dplyr::select(Name, attacker, victim, Year) %>%
        print(n = Inf)
}

# List ACD incidents NOT in kinetic dyad-years
cat("ACD-only incidents (no kinetic conflict):\n")
df_inc_for_mid %>%
    filter(is_acd & !mid_force) %>%
    dplyr::select(Name, attacker, victim, Year) %>%
    print(n = Inf)

# --- 9c. Build kinetic dyad-year lookup for panel filtering ---
# MID is undirected; our panel is directed — expand to both directions.
# These entire rows are dropped from the panel so that removing kinetic-
# linked incidents doesn't create false zeros.
mid_force_names <- mid_force %>%
    mutate(
        att_name_1 = names(cow_name_to_code)[match(statea, cow_name_to_code)],
        vic_name_1 = names(cow_name_to_code)[match(stateb, cow_name_to_code)],
        att_name_2 = names(cow_name_to_code)[match(stateb, cow_name_to_code)],
        vic_name_2 = names(cow_name_to_code)[match(statea, cow_name_to_code)]
    )

kinetic_dyad_years <- bind_rows(
    mid_force_names %>%
        filter(!is.na(att_name_1) & !is.na(vic_name_1)) %>%
        dplyr::select(attacker = att_name_1, victim = vic_name_1, Year = year),
    mid_force_names %>%
        filter(!is.na(att_name_2) & !is.na(vic_name_2)) %>%
        dplyr::select(attacker = att_name_2, victim = vic_name_2, Year = year)
) %>% distinct()

cat("Kinetic dyad-years to drop from panel:", nrow(kinetic_dyad_years), "\n")

# --- 9d. Aggregate clean incidents to dyad-year ---
df_agg_clean <- df_inc_for_mid %>%
    filter(!exclude) %>%
    group_by(attacker, victim, Year) %>%
    summarise(Incident_Count_Clean = n(), .groups = "drop")

# --- 9e. Build the clean panel ---
# Start from df_2016 (already has W4 + CINC + GDP), restrict to 2007-2014,
# drop kinetic dyad-years, replace DV with clean counts.
df_2014_base <- df_2016 %>%
    filter(Year >= 2007 & Year <= 2014) %>%
    dplyr::select(-Incident_Count) %>%
    # Drop kinetic dyad-years
    anti_join(kinetic_dyad_years, by = c("attacker", "victim", "Year")) %>%
    # Merge clean incident counts
    left_join(df_agg_clean, by = c("attacker", "victim", "Year")) %>%
    mutate(Incident_Count_Clean = replace_na(Incident_Count_Clean, 0L))

# Complete cases (should be all complete since df_2016 was already filtered)
complete_2014 <- complete.cases(
    df_2014_base$attacker_w4, df_2014_base$victim_w4,
    df_2014_base$attacker_cinc, df_2014_base$victim_cinc,
    df_2014_base$attacker_ln_gdp_pc, df_2014_base$victim_ln_gdp_pc
)
df_2014 <- df_2014_base[complete_2014, ]

cat(sprintf(
    "df_model_2014 — Obs: %d | Incidents: %d | Kinetic dyad-years dropped: %d\n",
    nrow(df_2014), sum(df_2014$Incident_Count_Clean), nrow(kinetic_dyad_years)
))

write.csv(df_2014, "outputs/df_model_2014.csv", row.names = FALSE)


# --- 9f. Save incident lists for review ---
# Original DCID format with exclusion reason appended
df_raw_2014 <- df_cyber_inc %>%
    mutate(Year_tmp = as.integer(format(as.Date(interactionstartdate), "%Y"))) %>%
    filter(Year_tmp >= 2007 & Year_tmp <= 2014) %>%
    left_join(
        df_inc_for_mid %>%
            distinct(Name, Year, is_acd, mid_force, exclude),
        by = c("Name", "Year_tmp" = "Year")
    )

# Kept incidents (145)
df_raw_2014 %>%
    filter(!exclude) %>%
    dplyr::select(-is_acd, -mid_force, -exclude, -Year_tmp) %>%
    write.csv("outputs/incidents_kept.csv", row.names = FALSE)

# Excluded incidents (43) with reason
df_raw_2014 %>%
    filter(exclude) %>%
    mutate(exclusion_reason = case_when(
        is_acd & mid_force ~ "ACD + kinetic conflict",
        is_acd ~ "ACD (cyber-to-cyber retaliation)",
        mid_force ~ "Kinetic conflict (MID hostility >= 4)"
    )) %>%
    dplyr::select(-is_acd, -mid_force, -exclude, -Year_tmp) %>%
    write.csv("outputs/incidents_excluded.csv", row.names = FALSE)

cat(sprintf("Saved: outputs/incidents_kept.csv (%d incidents)\n", n_kept))
cat(sprintf("Saved: outputs/incidents_excluded.csv (%d incidents)\n", n_excluded))


##################### Output Summary ############################################
cat("\n\n############################################################\n")
cat("#  OUTPUT SUMMARY                                          #\n")
cat("############################################################\n\n")
cat("Outputs:\n")
cat(sprintf(
    "  outputs/df_model_2020.csv          %d obs, %d incidents (W4 + GDP, 2007-2020)\n",
    nrow(df_2020), sum(df_2020$Incident_Count)
))
cat(sprintf(
    "  outputs/df_model_2016.csv          %d obs, %d incidents (W4 + GDP + CINC, 2007-2016)\n",
    nrow(df_2016), sum(df_2016$Incident_Count)
))
cat(sprintf(
    "  outputs/df_model_2014.csv          %d obs, %d incidents (clean DV (W4) + GDP + CINC, 2007-2014)\n",
    nrow(df_2014), sum(df_2014$Incident_Count_Clean)
))
cat(sprintf("  outputs/incidents_kept.csv         %d incidents (original DCID format)\n", n_kept))
cat(sprintf("  outputs/incidents_excluded.csv     %d incidents (with exclusion reason)\n", n_excluded))
