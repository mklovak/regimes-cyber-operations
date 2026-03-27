##################### inspect_acd_classification.R #############################
# Outputs a reviewable CSV of all 2007-2016 incidents with hack-back/unprovoked
# classification and all original DCID columns.
# Run AFTER data_preparation_acd.R (or standalone — self-contained).
################################################################################

rm(list = ls())
options(scipen = 999)
cat("\014")

library(tidyverse)
library(readxl)

# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

##################### 1. Load and clean DCID ###################################

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
    filter(Year >= 2007 & Year <= 2016) %>%
    arrange(start_date)


##################### 2. Classify: 2yr and 3yr #################################

classify_hackback <- function(df, window_years) {
    window_days <- window_years * 365
    n <- nrow(df)
    is_hb <- logical(n)
    prior_name <- character(n)
    prior_date <- as.Date(rep(NA, n))
    days_gap <- integer(n)

    for (i in seq_len(n)) {
        a <- df$attacker[i]
        b <- df$victim[i]
        d <- df$start_date[i]

        prior <- df %>%
            filter(
                attacker == b, victim == a,
                start_date < d, start_date >= d - window_days
            )

        if (nrow(prior) > 0) {
            is_hb[i] <- TRUE
            prior_name[i] <- prior$Name[nrow(prior)] # most recent prior attack
            prior_date[i] <- prior$start_date[nrow(prior)]
            days_gap[i] <- as.integer(d - prior$start_date[nrow(prior)])
        }
    }

    tibble(
        hackback = is_hb,
        trigger_name = ifelse(is_hb, prior_name, NA_character_),
        trigger_date = prior_date,
        days_since_trigger = ifelse(is_hb, days_gap, NA_integer_)
    )
}

cat("Classifying (2-year window)...\n")
class_2yr <- classify_hackback(df_inc, 2)

cat("Classifying (3-year window)...\n")
class_3yr <- classify_hackback(df_inc, 3)

df_out <- df_inc %>%
    bind_cols(
        class_2yr %>% rename(
            hackback_2yr = hackback,
            trigger_name_2yr = trigger_name,
            trigger_date_2yr = trigger_date,
            days_since_trigger_2yr = days_since_trigger
        ),
        class_3yr %>% rename(
            hackback_3yr = hackback,
            trigger_name_3yr = trigger_name,
            trigger_date_3yr = trigger_date,
            days_since_trigger_3yr = days_since_trigger
        )
    ) %>%
    mutate(
        classification_2yr = ifelse(hackback_2yr, "hack-back", "unprovoked"),
        classification_3yr = ifelse(hackback_3yr, "hack-back", "unprovoked"),
        reclassified_at_3yr = (!hackback_2yr & hackback_3yr)
    )


##################### 3. Print summary #########################################

cat("\n============================================================\n")
cat("  Classification Summary\n")
cat("============================================================\n")
cat(sprintf("Total incidents: %d\n", nrow(df_out)))
cat(sprintf(
    "2yr: %d hack-back, %d unprovoked\n",
    sum(df_out$hackback_2yr), sum(!df_out$hackback_2yr)
))
cat(sprintf(
    "3yr: %d hack-back, %d unprovoked\n",
    sum(df_out$hackback_3yr), sum(!df_out$hackback_3yr)
))
cat(sprintf("Reclassified at 3yr: %d\n\n", sum(df_out$reclassified_at_3yr)))


##################### 4. Print all hack-back incidents (2yr) ###################

cat("============================================================\n")
cat("  Hack-Back Incidents (2-year window)\n")
cat("============================================================\n\n")

hb_2yr <- df_out %>%
    filter(hackback_2yr) %>%
    dplyr::select(
        Name, attacker, victim, start_date, Year,
        cyber_objective, method, targettype, severity,
        trigger_name_2yr, trigger_date_2yr, days_since_trigger_2yr
    ) %>%
    arrange(start_date)

print(hb_2yr, n = Inf, width = Inf)


##################### 5. Print reclassified at 3yr #############################

cat("\n\n============================================================\n")
cat("  Reclassified at 3-year Window (unprovoked@2yr → hack-back@3yr)\n")
cat("============================================================\n\n")

reclass <- df_out %>%
    filter(reclassified_at_3yr) %>%
    dplyr::select(
        Name, attacker, victim, start_date, Year,
        cyber_objective, method, targettype, severity,
        trigger_name_3yr, trigger_date_3yr, days_since_trigger_3yr
    ) %>%
    arrange(start_date)

print(reclass, n = Inf, width = Inf)


##################### 6. Save full classified dataset ##########################

write_csv(df_out, "outputs/dcid_classified_review.csv")
cat("\n\nSaved: outputs/dcid_classified_review.csv\n")
cat("Columns: all original DCID + attacker/victim + classification + trigger info\n")
