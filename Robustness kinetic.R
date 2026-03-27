##################### robustness_kinetic.R #####################################
# R10: Add MID active-conflict control to M1/M2
# R11: Exclude ACD + kinetic-linked incidents AND kinetic dyad-years from panel
#
# Depends on:
#   - outputs/df_model_2016.csv (panel with W4 + CINC + GDP)
#   - data sources/DCID_2.0_Release_update_February_2023.xlsx (original DCID)
#   - data sources/dyadic_mid_4_03.csv (COW MID dyadic data)
#   - data sources/COW-country-codes.csv
#
# MID coverage ends 2014 → this robustness check uses 2007-2014 only.
################################################################################

rm(list = ls())
options(scipen = 999)
cat("\014")

library(tidyverse)
library(readxl)
library(fixest)
library(pscl)

# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

################################################################################
#   1. Load and prepare DCID
################################################################################

df_raw <- read_excel("data sources/DCID_2.0_Release_update_February_2023.xlsx")
country_codes <- read_csv("data sources/COW-country-codes.csv",
    show_col_types = FALSE
) %>%
    distinct(CCode, .keep_all = TRUE)

df_inc <- df_raw %>%
    mutate(StateA = trimws(StateA), StateB = trimws(StateB)) %>%
    mutate(
        duration_days = as.numeric(difftime(interactionenddate, interactionstartdate, units = "days")),
        start_date    = as.Date(interactionstartdate),
        Year          = as.integer(format(interactionstartdate, "%Y"))
    ) %>%
    filter(duration_days >= 0) %>%
    left_join(country_codes, by = c("initiator" = "CCode")) %>%
    rename(attacker = StateNme) %>%
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
    filter(Year >= 2007 & Year <= 2014)

cat("============================================================\n")
cat("  DCID Incidents (2007-2014)\n")
cat("============================================================\n")
cat("Total incidents:", nrow(df_inc), "\n")


################################################################################
#   2. Identify ACD incidents (cyber-to-cyber retaliation, Tallinn Manual)
################################################################################

acd_names <- c(
    "Buckshot Yankee",
    "Osinform",
    "November 2008 defacements_B",
    "Japan retaliation",
    "September 2010 defacements 2",
    "PCA retaliation",
    "NK retaliation"
)

df_inc$is_acd <- df_inc$Name %in% acd_names
cat("ACD incidents (Tallinn definition):", sum(df_inc$is_acd), "\n")


################################################################################
#   3. MID kinetic conflict filter (hostility >= 4: use of force or war)
################################################################################

df_mid <- read_csv("data sources/dyadic_mid_4.03.csv", show_col_types = FALSE)

# Dyad-years with use of force or war
mid_force <- df_mid %>%
    filter(year >= 2007, year <= 2014, hihost >= 4) %>%
    dplyr::select(statea, stateb, year) %>%
    distinct()

# COW code mapping for DCID victims
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

df_inc$attacker_cow <- df_inc$initiator
df_inc$victim_cow <- cow_name_to_code[df_inc$victim]

# Flag incidents in dyad-years with active MID use of force
df_inc$mid_force <- mapply(function(a, v, y) {
    if (is.na(a) || is.na(v)) {
        return(FALSE)
    }
    nrow(mid_force %>% filter(
        (statea == a & stateb == v & year == y) |
            (statea == v & stateb == a & year == y)
    )) > 0
}, df_inc$attacker_cow, df_inc$victim_cow, df_inc$Year)

df_inc$exclude <- df_inc$is_acd | df_inc$mid_force

cat("MID-linked incidents (hostility >= 4):", sum(df_inc$mid_force), "\n")
cat("Overlap (ACD + MID):", sum(df_inc$is_acd & df_inc$mid_force), "\n")
cat("Total excluded:", sum(df_inc$exclude), "\n")
cat("Purely cyber, purely unprovoked:", sum(!df_inc$exclude), "\n\n")


################################################################################
#   4. Build kinetic dyad-year lookup for panel filtering (R11)
################################################################################
# For R11: drop entire dyad-years from the panel where MID hostility >= 4.
# Without this, removing kinetic-linked cyber incidents creates false zeros:
# e.g. Russia->Georgia 2008 would show 0 cyber attacks, but Russia didn't
# "choose not to attack" — we removed the attacks because they accompanied
# kinetic force. These false zeros bias the model toward excess zeros.

# MID is undirected; our panel is directed — expand to both directions
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

cat("Kinetic dyad-years to drop from R11 panel:", nrow(kinetic_dyad_years), "\n\n")


################################################################################
#   5. Aggregate incidents to dyad-year
################################################################################

# All incidents (for baseline and R10)
df_agg_all <- df_inc %>%
    group_by(attacker, victim, Year) %>%
    summarise(
        Incident_Count = n(),
        mid_force_any = as.integer(any(mid_force)),
        .groups = "drop"
    )

# Purely cyber, purely unprovoked (for R11)
df_agg_clean <- df_inc %>%
    filter(!exclude) %>%
    group_by(attacker, victim, Year) %>%
    summarise(Incident_Count_Clean = n(), .groups = "drop")


################################################################################
#   6. Build panels
################################################################################

df_panel_base <- read_csv("outputs/df_model_2016.csv", show_col_types = FALSE) %>%
    filter(Year >= 2007 & Year <= 2014) %>%
    dplyr::select(-Incident_Count)

# --- Panel for baseline + R10 (full panel, 2007-2014) ---
df_r10 <- df_panel_base %>%
    left_join(df_agg_all, by = c("attacker", "victim", "Year")) %>%
    mutate(
        Incident_Count   = replace_na(Incident_Count, 0L),
        mid_force_any    = replace_na(mid_force_any, 0L),
        has_attack       = as.integer(Incident_Count > 0),
        directed_dyad_id = paste(attacker, victim, sep = "_")
    )

complete_r10 <- complete.cases(
    df_r10$attacker_w4, df_r10$victim_w4,
    df_r10$attacker_cinc, df_r10$victim_cinc,
    df_r10$attacker_ln_gdp_pc, df_r10$victim_ln_gdp_pc
)
df_r10 <- df_r10[complete_r10, ]

# --- Panel for R11 (kinetic dyad-years dropped) ---
df_r11 <- df_panel_base %>%
    anti_join(kinetic_dyad_years, by = c("attacker", "victim", "Year")) %>%
    left_join(df_agg_clean, by = c("attacker", "victim", "Year")) %>%
    mutate(
        Incident_Count_Clean = replace_na(Incident_Count_Clean, 0L),
        has_attack_clean     = as.integer(Incident_Count_Clean > 0),
        directed_dyad_id     = paste(attacker, victim, sep = "_")
    )

complete_r11 <- complete.cases(
    df_r11$attacker_w4, df_r11$victim_w4,
    df_r11$attacker_cinc, df_r11$victim_cinc,
    df_r11$attacker_ln_gdp_pc, df_r11$victim_ln_gdp_pc
)
df_r11 <- df_r11[complete_r11, ]

cat("============================================================\n")
cat("  Panel Summary\n")
cat("============================================================\n")
cat(
    "R10 panel (full, 2007-2014):", nrow(df_r10), "obs,",
    sum(df_r10$Incident_Count), "incidents\n"
)
cat(
    "R11 panel (kinetic dyad-years dropped):", nrow(df_r11), "obs,",
    sum(df_r11$Incident_Count_Clean), "incidents\n"
)
cat("Dyad-years removed from R11:", nrow(df_r10) - nrow(df_r11), "\n\n")

cat("\n\n============================================================\n")
cat("  R11: Zero-Inflated Negative Binomial (ZINB)\n")
cat("============================================================\n\n")

# 1. Standard ZINB
# The formula uses the format: count_model | zero_inflation_model
# Correct: CINC in inflation, full spec in count
r11_zinb <- zeroinfl(
    Incident_Count_Clean ~ attacker_w4 + victim_w4 +
        attacker_cinc + victim_cinc +
        attacker_ln_gdp_pc + victim_ln_gdp_pc |
        attacker_cinc + victim_cinc,
    data = df_r11,
    dist = "negbin"
)

# 2. ZINB with Year Fixed Effects
# Adding as.factor(Year) to both the count and zero-inflation sides
r11_zinb_fe <- zeroinfl(
    Incident_Count_Clean ~ attacker_w4 + victim_w4 +
        attacker_cinc + victim_cinc +
        attacker_ln_gdp_pc + victim_ln_gdp_pc + as.factor(Year) |
        attacker_cinc + victim_cinc,
    data = df_r11,
    dist = "negbin"
)
cat("--- R11 ZINB (Standard) ---\n")
print(summary(r11_zinb))

cat("\n--- R11 ZINB (Year Fixed Effects) ---\n")
print(summary(r11_zinb_fe))



################################################################################
#   Baseline M1/M2 on 2007-2014
################################################################################

cat("============================================================\n")
cat("  Baseline M1/M2 (2007-2014, all incidents)\n")
cat("============================================================\n\n")

m1_base <- fenegbin(
    Incident_Count ~ attacker_w4 + victim_w4 +
        attacker_cinc + victim_cinc +
        attacker_ln_gdp_pc + victim_ln_gdp_pc,
    data = df_r10,
    vcov = ~directed_dyad_id
)

m2_base <- feglm(
    has_attack ~ attacker_w4 + victim_w4 +
        attacker_cinc + victim_cinc +
        attacker_ln_gdp_pc + victim_ln_gdp_pc,
    data = df_r10,
    family = binomial,
    vcov = ~directed_dyad_id
)

cat(sprintf(
    "M1 baseline: attacker_w4 = %.4f, p = %.2e\n",
    coef(m1_base)["attacker_w4"], pvalue(m1_base)["attacker_w4"]
))
cat(sprintf(
    "M2 baseline: attacker_w4 = %.4f, p = %.2e\n\n",
    coef(m2_base)["attacker_w4"], pvalue(m2_base)["attacker_w4"]
))


################################################################################
#   R10: MID as control variable
################################################################################

cat("============================================================\n")
cat("  R10: MID Active Conflict as Control Variable\n")
cat("============================================================\n\n")

r10_nb <- fenegbin(
    Incident_Count ~ attacker_w4 + victim_w4 +
        attacker_cinc + victim_cinc +
        attacker_ln_gdp_pc + victim_ln_gdp_pc +
        mid_force_any,
    data = df_r10,
    vcov = ~directed_dyad_id
)

r10_logit <- feglm(
    has_attack ~ attacker_w4 + victim_w4 +
        attacker_cinc + victim_cinc +
        attacker_ln_gdp_pc + victim_ln_gdp_pc +
        mid_force_any,
    data = df_r10,
    family = binomial,
    vcov = ~directed_dyad_id
)

cat("--- R10 NB ---\n")
summary(r10_nb)
cat("\n--- R10 Logit ---\n")
summary(r10_logit)


################################################################################
#   R11: Purely cyber, purely unprovoked
################################################################################

cat("\n\n============================================================\n")
cat("  R11: Purely Cyber, Purely Unprovoked\n")
cat("  ACD + kinetic incidents excluded; kinetic dyad-years dropped from panel\n")
cat("============================================================\n\n")

r11_nb <- fenegbin(
    Incident_Count_Clean ~ attacker_w4 + victim_w4 +
        attacker_cinc + victim_cinc +
        attacker_ln_gdp_pc + victim_ln_gdp_pc,
    data = df_r11,
    vcov = ~directed_dyad_id
)

r11_logit <- feglm(
    has_attack_clean ~ attacker_w4 + victim_w4 +
        attacker_cinc + victim_cinc +
        attacker_ln_gdp_pc + victim_ln_gdp_pc,
    data = df_r11,
    family = binomial,
    vcov = ~directed_dyad_id
)

cat("--- R11 NB ---\n")
summary(r11_nb)
cat("\n--- R11 Logit ---\n")
summary(r11_logit)


################################################################################
#   Summary
################################################################################

cat("\n\n============================================================\n")
cat("  Summary: Kinetic Conflict Robustness\n")
cat("============================================================\n\n")

cat("--- NB (H1: frequency) ---\n")
cat(sprintf(
    "  Baseline (2007-2014):        attacker_w4 = %.4f, p = %.2e  [n=%d, incidents=%d]\n",
    coef(m1_base)["attacker_w4"], pvalue(m1_base)["attacker_w4"],
    nrow(df_r10), sum(df_r10$Incident_Count)
))
cat(sprintf(
    "  R10 (+ MID control):         attacker_w4 = %.4f, p = %.2e  [n=%d]\n",
    coef(r10_nb)["attacker_w4"], pvalue(r10_nb)["attacker_w4"], nrow(df_r10)
))
cat(sprintf(
    "  R11 (purely unprovoked):     attacker_w4 = %.4f, p = %.2e  [n=%d, incidents=%d]\n",
    coef(r11_nb)["attacker_w4"], pvalue(r11_nb)["attacker_w4"],
    nrow(df_r11), sum(df_r11$Incident_Count_Clean)
))

cat("\n--- Logit (H2: likelihood) ---\n")
cat(sprintf(
    "  Baseline (2007-2014):        attacker_w4 = %.4f, p = %.2e\n",
    coef(m2_base)["attacker_w4"], pvalue(m2_base)["attacker_w4"]
))
cat(sprintf(
    "  R10 (+ MID control):         attacker_w4 = %.4f, p = %.2e\n",
    coef(r10_logit)["attacker_w4"], pvalue(r10_logit)["attacker_w4"]
))
cat(sprintf(
    "  R11 (purely unprovoked):     attacker_w4 = %.4f, p = %.2e\n",
    coef(r11_logit)["attacker_w4"], pvalue(r11_logit)["attacker_w4"]
))

cat("\n--- MID control coefficient (R10) ---\n")
cat(sprintf(
    "  NB:    mid_force = %.4f, p = %.2e\n",
    coef(r10_nb)["mid_force_any"], pvalue(r10_nb)["mid_force_any"]
))
cat(sprintf(
    "  Logit: mid_force = %.4f, p = %.2e\n",
    coef(r10_logit)["mid_force_any"], pvalue(r10_logit)["mid_force_any"]
))

cat("\n--- Panel sizes ---\n")
cat(sprintf(
    "  R10: %d obs, %d incidents (full 2007-2014)\n",
    nrow(df_r10), sum(df_r10$Incident_Count)
))
cat(sprintf(
    "  R11: %d obs, %d incidents (kinetic dyad-years dropped)\n",
    nrow(df_r11), sum(df_r11$Incident_Count_Clean)
))

cat("\n")
etable(m1_base, r10_nb, r11_nb, m2_base, r10_logit, r11_logit,
    headers = c(
        "M1 base", "R10 NB", "R11 NB",
        "M2 base", "R10 Logit", "R11 Logit"
    ),
    se.below = TRUE,
    fitstat = ~ n + ll + aic + bic,
    dict = c(
        attacker_w4        = "Attacker W4",
        victim_w4          = "Victim W4",
        attacker_cinc      = "Attacker CINC",
        victim_cinc        = "Victim CINC",
        attacker_ln_gdp_pc = "Attacker ln(GDP p.c.)",
        victim_ln_gdp_pc   = "Victim ln(GDP p.c.)",
        mid_force_any      = "MID use of force"
    )
)

cat("\nIf H1 survives R11: the selectorate mechanism constrains purely cyber\n")
cat("aggression independently of kinetic campaigning. The zeros in the R11\n")
cat("panel are genuine 'chose not to attack' observations, not artifacts of\n")
cat("removing kinetic-linked incidents from conflict dyads.\n")



# Save both panels in original DCID format
df_raw_2014 <- df_raw %>%
    mutate(Year = as.integer(format(as.Date(interactionstartdate), "%Y"))) %>%
    filter(Year >= 2007 & Year <= 2014)

# Match exclusion flags back to raw data by Name
df_raw_2014 <- df_raw_2014 %>%
    left_join(
        df_inc %>% dplyr::select(Name, is_acd, mid_force, exclude),
        by = "Name"
    )

# Purely cyber, purely unprovoked (145 incidents)
df_raw_2014 %>%
    filter(!exclude) %>%
    dplyr::select(-is_acd, -mid_force, -exclude, -Year) %>%
    write_csv("outputs/r11_kept_incidents.csv")

# Excluded incidents (43) with reason
df_raw_2014 %>%
    filter(exclude) %>%
    mutate(
        exclusion_reason = case_when(
            is_acd & mid_force ~ "ACD + kinetic conflict",
            is_acd ~ "ACD (cyber-to-cyber retaliation)",
            mid_force ~ "Kinetic conflict (MID hostility >= 4)"
        )
    ) %>%
    dplyr::select(-is_acd, -mid_force, -exclude, -Year) %>%
    write_csv("outputs/r11_excluded_incidents.csv")

cat("Saved: outputs/r11_kept_incidents.csv (", sum(!df_raw_2014$exclude), ")\n")
cat("Saved: outputs/r11_excluded_incidents.csv (", sum(df_raw_2014$exclude), ")\n")



df_inc %>%
    filter(!exclude) %>%
    group_by(Name, attacker, victim, Year) %>%
    filter(n() > 1) %>%
    print(n = Inf)
