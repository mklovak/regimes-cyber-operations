##################### Environment Setup ########################################
rm(list=ls()) # Clear all objects from the environment
try(dev.off(), silent = TRUE) # Close all currently open graphical devices/windows
options(scipen=999) # Disable scientific notation in numeric output
cat("\014") # Clear the console screen

# install.packages("readxl")
# install.packages ('tidyverse')
# install.packages("dplyr")

library(readxl)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(patchwork)

##################### Data transformation and preparation ######################
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

df_cyber_inc <- read_excel("DCID original dataset and COW codes/DCID_2.0_Release_update_February_2023.xlsx")  # https://www.ryanmaness.com/cyber-conflict-dataset
country_codes <- read_csv("DCID original dataset and COW codes/COW-country-codes.csv")                        # https://correlatesofwar.org/cow-country-codes/

df_cyber_inc <- df_cyber_inc %>%
  mutate(
    StateA = trimws(StateA),
    StateB = trimws(StateB)
  )

summary(df_cyber_inc)
summary(country_codes)

# Keep only the first entry for each 'CCode' because some of them repeats
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
cut_date <- as.Date("2016-07-09") # https://www.nato.int/cps/en/natohq/official_texts_133177.htm#:~:text=NATO%20%2D%20Official%20text:%20Cyber%20Defence,3.
df_cyber_inc_timings <- df_cyber_inc_timings %>%
  mutate(
    period_2016 = ifelse(interactionstartdate < cut_date, "Before July 2016", "After July 2016")
  )

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
    ) %>%
  
  # Keep only rows where 'victim' is not NA
  # Filter out the observations where attacker is not in the dyadpair -- e.g. 'victim' logic from previous mutation is failed
  filter(!is.na(victim))
cat("Total rows removed:", nrow(df_cyber_inc) - nrow(df_cyber_inc_names), "\n") # 5

# Standardize 'victim' country names to match 'attacker' names
df_cyber_inc_names <- df_cyber_inc_names %>%
  mutate(
    victim = case_when(
      victim == "US" ~ "United States of America",
      victim == "N Korea" ~ "North Korea",
      victim == "S Korea" ~ "South Korea",
      victim == "UK" ~ "United Kingdom",
      TRUE ~ victim # Keep all other names as-is
    )
  )

# Create a column 'rivals' for dyad pair but avoid duplicates 
# (e.g. US-China and China-US are the same rivals pair so we need to keep only one pair)
df_cyber_inc_names <- 
  df_cyber_inc_names %>%
  mutate(
    rivals = mapply(function(a, b) {
      paste(sort(c(a, b)), collapse = "-")
      }, StateA, StateB)
)

# Add NATO countries indicators based on 'Year'
df_cyber_inc_names <- 
  df_cyber_inc_names %>%
  mutate(
    # Check if attacker was in NATO in that Year
    attacker_is_nato = case_when(
      attacker %in% c("United States of America", "United Kingdom", "France", "Canada", "Italy", "Netherlands", "Belgium", "Norway", "Denmark", "Portugal", "Iceland", "Luxembourg") & Year >= 1949 ~ 1,
      attacker %in% c("Greece", "Turkey") & Year >= 1952 ~ 1,
      attacker == "Germany" & Year >= 1955 ~ 1,
      attacker == "Spain" & Year >= 1982 ~ 1,
      attacker %in% c("Poland", "Czech Republic", "Hungary") & Year >= 1999 ~ 1,
      attacker %in% c("Bulgaria", "Romania", "Slovakia", "Slovenia", "Estonia", "Latvia", "Lithuania") & Year >= 2004 ~ 1,
      attacker %in% c("Croatia", "Albania") & Year >= 2009 ~ 1,
      attacker == "Montenegro" & Year >= 2017 ~ 1,
      attacker == "North Macedonia" & Year >= 2020 ~ 1,
      TRUE ~ 0 # Default to 0 (No)
    ),
    
    # Check if victim was in NATO in that Year
    victim_is_nato = case_when(
      victim %in% c("United States of America", "United Kingdom", "France", "Canada", "Italy", "Netherlands", "Belgium", "Norway", "Denmark", "Portugal", "Iceland", "Luxembourg") & Year >= 1949 ~ 1,
      victim %in% c("Greece", "Turkey") & Year >= 1952 ~ 1,
      victim == "Germany" & Year >= 1955 ~ 1,
      victim == "Spain" & Year >= 1982 ~ 1,
      victim %in% c("Poland", "Czech Republic", "Hungary") & Year >= 1999 ~ 1,
      victim %in% c("Bulgaria", "Romania", "Slovakia", "Slovenia", "Estonia", "Latvia", "Lithuania") & Year >= 2004 ~ 1,
      victim %in% c("Croatia", "Albania") & Year >= 2009 ~ 1,
      victim == "Montenegro" & Year >= 2017 ~ 1,
      victim == "North Macedonia" & Year >= 2020 ~ 1,
      TRUE ~ 0 # Default to 0 (No)
    )
  )
#  "Finland" and "Sweden" joined at 2023-2024, and we have no data for that period

# Select only relevant columns and decode them using Codebook
df_cyber_inc_clean <- 
  df_cyber_inc_names %>%
  dplyr::select(Name, rivals, attacker, attacker_is_nato, victim, victim_is_nato,`3rdpartyinitiator`, interactionstartdate,
    interactionenddate, Year, duration_days, period_2016, method, targettype, cyber_objective,
    information_operation, objective_achievement, Concession, severity,
    `damage type`, `Crit Inf`, `Supply Ch`, Ransomware, Sources1, Sources2, Sources3, Sources4, Sources5, `Political Objective`, Justice, Cert, FBI) %>%
  
  mutate(
    # Decode 'method' column (using Codebook pp. 5-6)
    method = case_when(
      method == 1   ~ "Vandalism (website defacements, propaganda)",
      method == 2   ~ "DDoS, Botnets",
      method == 3 | method == 3.1  ~ "Network Intrusion (Trojans, Backdoors)",
      method == 4.1 ~ "Network Infiltration (Logic bombs)",
      method == 4.2 ~ "Network Infiltration (Viruses)",
      method == 4.3 ~ "Network Infiltration (Worms)",
      method == 4.4 ~ "Network Infiltration (Keystroke logging)"),
    
    # Decode 'targettype'  column (using Codebook p. 6)
    targettype = case_when(
      targettype == 1 ~ "Private/non-state",
      targettype == 2 ~ "Government non-military",
      targettype == 3 ~ "Government military"),
    
    # Decode 'cyber_objective' column (objectives for initiators) (using Codebook pp. 6-7)
    cyber_objective = case_when(
      cyber_objective == 1 ~ "Disruption",
      cyber_objective == 2 ~ "Short-term espionage",
      cyber_objective == 3 ~ "Long-term espionage",
      cyber_objective == 4 ~ "Degrade",
      cyber_objective == 5 ~ "Influence"),
    
    # Decode binary columns (using Codebook pp. 7-8, 11) and NATO indicators columns
    information_operation = ifelse(information_operation == 1, "Yes", "No"),
    objective_achievement = ifelse(objective_achievement == 1, "Yes", "No"),
    Concession = ifelse(Concession == 1, "Yes", "No"),
    `3rdpartyinitiator` = ifelse(`3rdpartyinitiator` == 1, "Yes", "No"),
    `Supply Ch` = ifelse(`Supply Ch` == 1, "Yes", "No"),
    `Ransomware` = ifelse(`Ransomware` == 1, "Yes", "No"),
    attacker_is_nato = ifelse(attacker_is_nato == 1, "Yes", "No"),
    victim_is_nato = ifelse(victim_is_nato == 1, "Yes", "No"),
  
    # Decode 'damage type'  column (using Codebook p. 3)
    `damage type` = case_when(
      `damage type` == 1 ~ "Direct and immediate",
      `damage type` == 2 ~ "Direct and delayed",
      `damage type` == 3 ~ "Indirect and immediate",
      `damage type` == 4 ~ "Indirect and delayed"),
    
    # Decode 'Crit Inf' column (using Codebook p. 10)
    `Crit Inf` = case_when(
      `Crit Inf` == 1  ~ "Chemical Sector",
      `Crit Inf` == 2  ~ "Commercial Facilities Sector",
      `Crit Inf` == 3  ~ "Communications Sector",
      `Crit Inf` == 4  ~ "Critical Manufacturing Sector",
      `Crit Inf` == 5  ~ "Dams Sector",
      `Crit Inf` == 6  ~ "Defense Industrial Base Sector",
      `Crit Inf` == 7  ~ "Emergency Services Sector",
      `Crit Inf` == 8  ~ "Energy Sector",
      `Crit Inf` == 9  ~ "Financial Services Sector",
      `Crit Inf` == 10 ~ "Food and Agriculture Sector",
      `Crit Inf` == 11 ~ "Government Facilities Sector",
      `Crit Inf` == 12 ~ "Healthcare and Public Health Sector",
      `Crit Inf` == 13 ~ "Information Technology Sector",
      `Crit Inf` == 14 ~ "Nuclear Reactors, Materials, and Waste Sector",
      `Crit Inf` == 15 ~ "Transportation Systems Sector",
      `Crit Inf` == 16 ~ "Water and Wastewater Systems Sector",
      `Crit Inf` == 17 ~ "Other (Election Infrastructure, Academia)")
  )

##################### Descriptive statistics ###################################
summary(df_cyber_inc_clean)
min(df_cyber_inc$interactionstartdate) # "2000-09-01 UTC"
max(df_cyber_inc$interactionenddate) # "2021-03-02 UTC"

# Most Active Rivalries
df_cyber_inc_clean %>%
  count(rivals, sort = TRUE) %>%
  top_n(10, n) %>%
  ggplot(aes(x = reorder(rivals, n), y = n)) +
  geom_col(fill = "lightblue") + 
  coord_flip() +
  geom_text(aes(label = n), hjust = -0.2, size = 3.5) +
  labs(
    title = "Top 10 rivals in cyberspace",
    x = 'Rivals',
    y = 'Number of accidents',
  ) +
  theme_minimal() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank()
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15)))

# Top Attackers
df_cyber_inc_clean %>%
  count(attacker, sort = TRUE) %>%
  top_n (10, n) %>%
  ggplot(aes(x = n, y = reorder(attacker, n))) +
  geom_point(size = 5, color = "#1D4A7F") +
  geom_text(aes(label = n), hjust = -1, size = 4, color = "#1D4A7F") +
  labs(
    title = "Cyber Incidents by Attacker Country",
    x = "Number of Incidents",
    y = "Attacker Country"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    plot.title = element_text(face = "bold")
  ) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.3)))

# Top non-NATO attackers of NATO countries
df_cyber_inc_clean %>%
  filter(victim_is_nato == "Yes" & attacker_is_nato == "No") %>%
  count(attacker, sort = TRUE)

# NATO vs. non-NATO attackers
df_cyber_inc_clean %>%
  count(attacker_is_nato, victim_is_nato, sort = TRUE)
df_cyber_inc_clean %>%
  filter(attacker_is_nato == "Yes" & victim_is_nato == "Yes") %>% nrow()
# NATO country never attacked another NATO country

# Most Common Attack Methods
df_cyber_inc_clean %>%
  filter(!is.na(method)) %>%
  count(method) %>%
  arrange(desc(n)) %>%
  ggplot(aes(x = n, y = forcats::fct_reorder(method, n))) +
  geom_col(fill = "navy") + 
  geom_text(aes(label = n), hjust = -0.2, size = 3.5) +
  labs(
    title = "Most Common Cyber Attack Methods",
    x = "Number of Incidents (n)",
    y = "Attack Method"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    plot.title = element_text(face = "bold")
  ) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.15)))

# Attack methods targeting NATO vs non-NATO
df_cyber_inc_clean %>%
  filter(!is.na(method)) %>%
  group_by(victim_is_nato, method) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  ggplot(aes(x = count, y = method, color = victim_is_nato)) +
  geom_point(size = 4, alpha = 0.9, position = position_dodge(width = 0.5)) +
  scale_x_continuous(
    breaks = seq(0, 150, by = 25), 
    limits = c(0, 150),
    expand = expansion(mult = c(0, 0.01))
  ) +
  scale_color_manual(
    values = c("No" = "#56B4E9", "Yes" = "#1D4A7F"),
    labels = c("No" = "Not NATO Member", "Yes" = "NATO Member")
  ) +
  labs(
    title = "Attack Methods by Victim NATO Status",
    x = "Number of Incidents",
    y = "Attack Method",
    color = "Victim Status"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    plot.title = element_text(face = "bold", hjust = 0),
    axis.title = element_text(size = 12), 
    axis.text = element_text(size = 11),
    legend.text = element_text(size = 10),
    legend.position = "bottom"
  )

# Most Common Cyber Objectives
df_cyber_inc_clean %>%
  count(cyber_objective) %>%
  arrange(desc(n)) %>%
  ggplot(aes(x = n, y = fct_reorder(cyber_objective, n), fill = n)) +
  geom_col() +
  geom_text(aes(label = n), hjust = -0.2, size = 3.5) +
  labs(
    title = "Most Common Cyber Objectives",
    x = "Number of Incidents",
    y = NULL 
  ) +
  scale_fill_gradient(low = "#B3C6E7", high = "#1D3557") +
  theme_minimal() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    plot.title = element_text(face = "bold"),
    axis.text.y = element_text(size = 12)  
  ) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.15)))


# Most Common Target Types
df_cyber_inc_clean %>%
  count(targettype) %>%
  arrange(desc(n)) %>%
  ggplot(aes(x = n, y = fct_reorder(targettype, n), fill = n)) +
  geom_col() +
  geom_text(aes(label = n), hjust = -0.2, size = 3.5) +
  labs(
    title = "Most Common Target Types",
    x = "Number of Incidents",
    y = NULL  # Remove y-axis label
  ) +
  scale_fill_gradient(low = "#E6E6FA", high = "#1B0082")  + 
  theme_minimal() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    plot.title = element_text(face = "bold"),
    axis.text.y = element_text(size = 12)  
  ) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.15)))

# Top Victims
df_cyber_inc_clean %>%
  count(victim, sort = TRUE) %>%
  top_n(10, n) %>% 
  ggplot(aes(x = n, y = fct_reorder(victim, n), fill = n)) +
  geom_col() +
  geom_text(aes(label = n), hjust = -0.2, size = 3.5) +
  labs(
    title = "Top 10 Victim Countries",
    x = "Number of Incidents",
    y = NULL
  ) +
  scale_fill_gradient(low = "#B3C6E7", high = "#1D3557") +
  theme_minimal() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    plot.title = element_text(face = "bold"),
    axis.text.y = element_text(size = 10)
  ) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.15)))

# NATO vs non-NATO victims over time
df_cyber_inc_clean %>%
  filter(Year <= 2020) %>%
  count(Year, victim_is_nato) %>%
  ggplot(aes(x = Year, y = n, color = victim_is_nato, group = victim_is_nato)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  geom_vline(xintercept = 2016, linetype = "dashed", color = "red") +
  labs(title = "Cyber incidents: NATO vs Non-NATO victims",
       y = "Number of incidents",
       color = "NATO victim") +
  theme_minimal()

# Incidents and Average Severity per Year
incidents_plot <- df_cyber_inc_clean %>%
  filter(Year <= 2020) %>%
  count(Year, name = "total_incidents") %>%
  
  ggplot(aes(x = Year, y = total_incidents)) +
  geom_line(color = "lightblue", linewidth = 1.3) +
  geom_point(color = "lightblue", size = 2) +
  theme_minimal() +
  labs(
    title = "Number of Cyber Incidents and Average Severity",
    y = "Number of Cyber Incidents"
  ) +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank()
  )
# incidents_plot

severity_plot <- df_cyber_inc_clean %>%
  filter(Year <= 2020) %>%
  group_by(Year) %>%
  summarize(avg_severity = mean(severity)) %>%
  
  ggplot(aes(x = Year, y = avg_severity)) +
  geom_line(color = "violet", linewidth = 1.3) +
  geom_point(color = "violet", size = 2) +
  theme_minimal() +
  labs(
    x = "Year",
    y = "Average Severity",
    caption = "Source: DCID 2.0 Dataset"
  )
# severity_plot

incidents_plot / severity_plot # / operator from 'patchwork' stacks p1 on top of p2

# A histogram of incident duration in days
ggplot(df_cyber_inc_clean, aes(x = duration_days)) +
  geom_histogram(binwidth = 40, fill = "steelblue", color = "white") +
  theme_minimal() +
  labs(title = "Cyber Incident Durations",
       x = "Duration in Days",
       y = "Number of Incidents")

# Some incidents are more than 1000 or even 3000 days long.
# Maybe they are long-term espionage or infiltration campaigns?
df_cyber_inc_clean %>%
    filter(duration_days > 1000) %>%
    select(Name, attacker, victim, duration_days, Year, interactionenddate,
           targettype, method, cyber_objective, information_operation) %>%
    arrange(desc(duration_days))

##################### Save cleaned data ########################################
write.csv(df_cyber_inc_clean, "df_cyber_inc_clean.csv", row.names = FALSE)
cat("Saved df_cyber_inc_clean.csv with", nrow(df_cyber_inc_clean), "observations.\n")

