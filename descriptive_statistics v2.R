##################### Environment Setup ########################################
rm(list = ls())
try(dev.off(), silent = TRUE)
options(scipen = 999)
cat("\014")

library(tidyverse)
library(dplyr)
library(ggplot2)
library(patchwork)
library(moments) # for skewness()

# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

##################### Load datasets ############################################
df_2020 <- read_csv("outputs/df_model_2020.csv")
df_2016 <- read_csv("outputs/df_model_2016.csv")


##################### 1. Dataset overview (df_2020) ############################
# Unit of observation: directed dyad-year (attacker -> victim, per year)
# Period: 2007-2020
nrow(df_2020)
n_distinct(df_2020$attacker) # unique countries
length(unique(df_2020$Year)) # years covered

# How many dyad-years have at least one incident?
sum(df_2020$Incident_Count > 0)
sum(df_2020$Incident_Count > 0) / nrow(df_2020) * 100 # % non-zero

# Total incidents per year
df_2020 %>%
  group_by(Year) %>%
  summarise(total = sum(Incident_Count)) %>%
  ggplot(aes(x = Year, y = total)) +
  geom_line(linewidth = 1.2, color = "steelblue") +
  geom_point(size = 2, color = "steelblue") +
  labs(
    title = "Total Cyber Incidents per Year (2007-2020)",
    x = "Year", y = "Number of Incidents"
  ) +
  theme_minimal()

# Top 10 attackers by total incidents
df_2020 %>%
  group_by(attacker) %>%
  summarise(total = sum(Incident_Count)) %>%
  filter(total > 0) %>%
  top_n(10, total) %>%
  ggplot(aes(x = total, y = reorder(attacker, total))) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = total), hjust = -0.2, size = 3.5) +
  labs(
    title = "Top 10 Attacker Countries (2007-2020)",
    x = "Total Incidents", y = NULL
  ) +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank()) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.15)))

# Top 10 victims by total incidents
df_2020 %>%
  group_by(victim) %>%
  summarise(total = sum(Incident_Count)) %>%
  filter(total > 0) %>%
  top_n(10, total) %>%
  ggplot(aes(x = total, y = reorder(victim, total))) +
  geom_col(fill = "#1D3557") +
  geom_text(aes(label = total), hjust = -0.2, size = 3.5) +
  labs(
    title = "Top 10 Victim Countries (2007-2020)",
    x = "Total Incidents", y = NULL
  ) +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank()) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.15)))


##################### 2. Variable descriptions (df_2020) #######################

# --- Dependent variable: Incident_Count ---
# Type: discrete (count)
# Level of measurement: ratio (meaningful zero — no incidents)
summary(df_2020$Incident_Count)
var(df_2020$Incident_Count)
sd(df_2020$Incident_Count)
skewness(df_2020$Incident_Count)
cat("Zero-inflation:", round(mean(df_2020$Incident_Count == 0) * 100, 2), "%\n")

# Distribution of non-zero incident counts
df_2020 %>%
  filter(Incident_Count > 0) %>%
  count(Incident_Count) %>%
  ggplot(aes(x = factor(Incident_Count), y = n)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = n), vjust = -0.3, size = 3.5) +
  labs(
    title = "Distribution of Non-Zero Incident Counts (df_2020)",
    x = "Incident Count", y = "Number of Dyad-Years"
  ) +
  theme_minimal()

# --- Independent variable: attacker_w4 (Winning Coalition Index) ---
# Type: continuous
# Level of measurement: ratio (0 = smallest coalition, 1 = largest)
summary(df_2020$attacker_w4)
var(df_2020$attacker_w4)
sd(df_2020$attacker_w4)
skewness(df_2020$attacker_w4)

ggplot(df_2020, aes(x = attacker_w4)) +
  geom_density(fill = "steelblue", alpha = 0.5) +
  geom_vline(aes(xintercept = mean(attacker_w4)), color = "red", linetype = "dashed") +
  geom_vline(aes(xintercept = median(attacker_w4)), color = "blue", linetype = "dashed") +
  labs(
    title = "Distribution of W4 Index (Attacker)",
    subtitle = "Red = mean, Blue = median",
    x = "W4 Score", y = "Density"
  ) +
  theme_minimal()

# --- Control variable: attacker_ln_gdp_pc (log GDP per capita) ---
# Type: continuous
# Level of measurement: ratio
# Already log-transformed in data_preparation.R
summary(df_2020$attacker_ln_gdp_pc)
var(df_2020$attacker_ln_gdp_pc)
sd(df_2020$attacker_ln_gdp_pc)
skewness(df_2020$attacker_ln_gdp_pc)

ggplot(df_2020, aes(x = attacker_ln_gdp_pc)) +
  geom_density(fill = "#1D3557", alpha = 0.5) +
  geom_vline(aes(xintercept = mean(attacker_ln_gdp_pc)), color = "red", linetype = "dashed") +
  geom_vline(aes(xintercept = median(attacker_ln_gdp_pc)), color = "blue", linetype = "dashed") +
  labs(
    title = "Distribution of ln(GDP per capita) (Attacker)",
    subtitle = "Red = mean, Blue = median",
    x = "ln(GDP per capita)", y = "Density"
  ) +
  theme_minimal()

# --- Control variable: attacker_internet (Internet Penetration %) ---
# Type: continuous
# Level of measurement: ratio (meaningful zero — 0% internet access)
summary(df_2020$attacker_internet)
var(df_2020$attacker_internet)
sd(df_2020$attacker_internet)
skewness(df_2020$attacker_internet)

ggplot(df_2020, aes(x = attacker_internet)) +
  geom_density(fill = "darkgreen", alpha = 0.5) +
  geom_vline(aes(xintercept = mean(attacker_internet)), color = "red", linetype = "dashed") +
  geom_vline(aes(xintercept = median(attacker_internet)), color = "blue", linetype = "dashed") +
  labs(
    title = "Distribution of Internet Penetration (Attacker)",
    subtitle = "Red = mean, Blue = median",
    x = "% of Population Using Internet", y = "Density"
  ) +
  theme_minimal()


##################### 3. Variable descriptions (df_2016) #######################
# Unit of observation: directed dyad-year (attacker -> victim, per year)
# Period: 2007-2016

nrow(df_2016)
sum(df_2016$Incident_Count > 0)
sum(df_2016$Incident_Count > 0) / nrow(df_2016) * 100

# --- Control variable: attacker_cinc (Composite Index of National Capability) ---
# Type: continuous
# Level of measurement: ratio (0 to 1, share of world total)
# Note: CINC is heavily right-skewed because a few major powers (US, China, Russia, India)
# hold most of the world's military capability, while ~160 countries have near-zero scores.
# This is similar to the GDP distribution — the index measures *share* of world capabilities.
summary(df_2016$attacker_cinc)
var(df_2016$attacker_cinc)
sd(df_2016$attacker_cinc)
skewness(df_2016$attacker_cinc)

# Raw CINC distribution (heavily skewed)
ggplot(df_2016, aes(x = attacker_cinc)) +
  geom_density(fill = "darkorange", alpha = 0.5) +
  geom_vline(aes(xintercept = mean(attacker_cinc)), color = "red", linetype = "dashed") +
  geom_vline(aes(xintercept = median(attacker_cinc)), color = "blue", linetype = "dashed") +
  labs(
    title = "Distribution of CINC (Attacker, 2007-2016)",
    subtitle = "Red = mean, Blue = median. Severe right skew.",
    x = "CINC Score", y = "Density"
  ) +
  theme_minimal()

# Log-transformed CINC to reduce skewness
# +0.0001 to avoid log(0) for countries with CINC = 0
df_2016 <- df_2016 %>% mutate(attacker_ln_cinc = log(attacker_cinc + 0.0001))

summary(df_2016$attacker_ln_cinc)
skewness(df_2016$attacker_ln_cinc)

ggplot(df_2016, aes(x = attacker_ln_cinc)) +
  geom_density(fill = "darkorange", alpha = 0.5) +
  geom_vline(aes(xintercept = mean(attacker_ln_cinc)), color = "red", linetype = "dashed") +
  geom_vline(aes(xintercept = median(attacker_ln_cinc)), color = "blue", linetype = "dashed") +
  labs(
    title = "Distribution of ln(CINC) (Attacker, 2007-2016)",
    subtitle = "Red = mean, Blue = median. Log transformation reduces skewness.",
    x = "ln(CINC)", y = "Density"
  ) +
  theme_minimal()
# Consider using ln(CINC) instead of raw CINC in models


##################### 4. Correlation tests (df_2020) ###########################
# Note: with n ~ 392k, p-values will be significant for even trivial correlations.
# Focus on effect size (r), not p-value.

# 4.1 Core relationship: W4 vs Incident Count
cor.test(df_2020$attacker_w4, df_2020$Incident_Count)
cor.test(df_2020$victim_w4, df_2020$Incident_Count)

# 4.2 Controls vs Incident Count
cor.test(df_2020$attacker_ln_gdp_pc, df_2020$Incident_Count)
cor.test(df_2020$victim_ln_gdp_pc, df_2020$Incident_Count)
cor.test(df_2020$attacker_internet, df_2020$Incident_Count)
cor.test(df_2020$victim_internet, df_2020$Incident_Count)

# 4.3 Collinearity between independent variables
# If r > 0.7 between two predictors, multicollinearity may be a concern
cor.test(df_2020$attacker_w4, df_2020$attacker_ln_gdp_pc)
cor.test(df_2020$attacker_w4, df_2020$attacker_internet)
cor.test(df_2020$attacker_ln_gdp_pc, df_2020$attacker_internet)


##################### 5. Correlation tests (df_2016) ###########################

# 5.1 Core relationship: W4 vs Incident Count
cor.test(df_2016$attacker_w4, df_2016$Incident_Count)
cor.test(df_2016$victim_w4, df_2016$Incident_Count)

# 5.2 CINC vs Incident Count (both raw and log)
cor.test(df_2016$attacker_cinc, df_2016$Incident_Count)
cor.test(df_2016$attacker_ln_cinc, df_2016$Incident_Count)
cor.test(df_2016$victim_cinc, df_2016$Incident_Count)

# 5.3 Collinearity: W4 vs CINC
cor.test(df_2016$attacker_w4, df_2016$attacker_cinc)
cor.test(df_2016$attacker_w4, df_2016$attacker_ln_cinc)


##################### 6. Difference of means tests #############################
# Compare variable means for dyad-years with vs without incidents.
# Appropriate test: two-sample t-test (continuous DV ~ binary IV).
# Note: large sample sizes make even tiny differences statistically significant.
# Focus on the magnitude of the difference, not just the p-value.

df_2020 <- df_2020 %>%
  mutate(has_incident = ifelse(Incident_Count > 0, "Yes", "No"))

# Does W4 differ for attackers who initiated incidents vs those who didn't?
t.test(attacker_w4 ~ has_incident, data = df_2020)

# Do targeted countries have different W4 scores?
t.test(victim_w4 ~ has_incident, data = df_2020)

# Do wealthier countries initiate more attacks?
t.test(attacker_ln_gdp_pc ~ has_incident, data = df_2020)

# Are wealthier countries more often targeted?
t.test(victim_ln_gdp_pc ~ has_incident, data = df_2020)

# Does internet penetration differ?
t.test(attacker_internet ~ has_incident, data = df_2020)
t.test(victim_internet ~ has_incident, data = df_2020)
