##################### Environment Setup ########################################
rm(list = ls())
try(dev.off(), silent = TRUE)
options(scipen = 999)
cat("\014")

library(tidyverse)
library(dplyr)
library(ggplot2)
library(moments) # for skewness()

# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

##################### Helper: histogram (%) with mean + median lines ############
plot_histogram <- function(data, var, fill_color, plot_title, x_label) {
  mu <- mean(data[[var]])
  med <- median(data[[var]])

  ggplot(data, aes(x = .data[[var]])) +
    geom_histogram(aes(y = after_stat(count / sum(count) * 100)),
      fill = fill_color, alpha = 0.6, bins = 30, color = "white"
    ) +
    geom_vline(xintercept = mu, color = "red", linetype = "dashed") +
    geom_vline(xintercept = med, color = "blue", linetype = "dashed") +
    labs(
      title = plot_title,
      subtitle = "Red = mean, Blue = median",
      x = x_label, y = "% of Observations"
    ) +
    theme_minimal()
}

##################### Load datasets ############################################
df_2020 <- read_csv("outputs/df_model_2020.csv")
df_2016 <- read_csv("outputs/df_model_2016.csv")


################################################################################
#                 PART 1: DATASET OVERVIEW
################################################################################

# ---- df_2020 overview ----
# Unit of observation: directed dyad-year (attacker -> victim, per year)
# Period: 2007-2020
cat("============================================================\n")
cat("  df_2020 Overview (2007-2020, GDP control)\n")
cat("============================================================\n")
cat("Total observations:", nrow(df_2020), "\n") # 412118
cat("Unique countries:", n_distinct(df_2020$attacker), "\n") # 174
cat("Years covered:", length(unique(df_2020$Year)), "(", min(df_2020$Year), "-", max(df_2020$Year), ")\n") # 2007-2020
cat("Dyad-years with at least one incident:", sum(df_2020$Incident_Count > 0), "\n") # 205
cat("% non-zero:", round(sum(df_2020$Incident_Count > 0) / nrow(df_2020) * 100, 4), "%\n") # 0.0497 %
cat("Total incidents:", sum(df_2020$Incident_Count), "\n\n") # 398

# ---- df_2016 overview ----
# Unit of observation: directed dyad-year (attacker -> victim, per year)
# Period: 2007-2016
cat("============================================================\n")
cat("  df_2016 Overview (2007-2016, CINC control)\n")
cat("============================================================\n")
cat("Total observations:", nrow(df_2016), "\n") # 299292
cat("Unique countries:", n_distinct(df_2016$attacker), "\n") # 174
cat("Years covered:", length(unique(df_2016$Year)), "(", min(df_2016$Year), "-", max(df_2016$Year), ")\n") # 2007-2016
cat("Dyad-years with at least one incident:", sum(df_2016$Incident_Count > 0), "\n") # 136
cat("% non-zero:", round(sum(df_2016$Incident_Count > 0) / nrow(df_2016) * 100, 4), "%\n") # 0.0454 %
cat("Total incidents:", sum(df_2016$Incident_Count), "\n\n") # 258


##################### 1.1 Incident trend over time (df_2020) ###################

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
  scale_x_continuous(breaks = 2007:2020) +
  theme_minimal()

##################### 1.2 Top 5 attackers (df_2020) ###########################

df_2020 %>%
  group_by(attacker) %>%
  summarise(total = sum(Incident_Count)) %>%
  filter(total > 0) %>%
  top_n(5, total) %>%
  ggplot(aes(x = total, y = reorder(attacker, total))) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = total), hjust = -0.2, size = 3.5) +
  labs(
    title = "Top 5 Attacker Countries (2007-2020)",
    x = "Total Incidents", y = NULL
  ) +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank()) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.15)))

##################### 1.3 Top 5 victims (df_2020) ############################

df_2020 %>%
  group_by(victim) %>%
  summarise(total = sum(Incident_Count)) %>%
  filter(total > 0) %>%
  top_n(5, total) %>%
  ggplot(aes(x = total, y = reorder(victim, total))) +
  geom_col(fill = "#1D3557") +
  geom_text(aes(label = total), hjust = -0.2, size = 3.5) +
  labs(
    title = "Top 5 Victim Countries (2007-2020)",
    x = "Total Incidents", y = NULL
  ) +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank()) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.15)))


################################################################################
#                 PART 2: VARIABLE DESCRIPTIONS (df_2020)
################################################################################

cat("\n============================================================\n")
cat("  Variable Descriptions: df_2020\n")
cat("============================================================\n\n")

# --- 2.1 Dependent variable: Incident_Count ---
# Type: discrete (count)
# Level of measurement: ratio (meaningful zero — no incidents)
cat("--- Incident_Count (Dependent Variable) ---\n")
cat("Type: discrete (count)\n")
cat("Level of measurement: ratio (meaningful zero — no incidents)\n")
# summary(df_2020$Incident_Count)
cat("Variance:", var(df_2020$Incident_Count), "\n")
cat("Standard deviation:", sd(df_2020$Incident_Count), "\n")
cat("Skewness:", skewness(df_2020$Incident_Count), "\n")
cat("Zero-inflation:", round(mean(df_2020$Incident_Count == 0) * 100, 2), "%\n")
cat("Variance >> Mean confirms overdispersion — supports Negative Binomial over Poisson.\n\n")

# Distribution of non-zero incident counts
df_2020 %>%
  filter(Incident_Count > 0) %>%
  count(Incident_Count) %>%
  ggplot(aes(x = factor(Incident_Count), y = n)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = n), vjust = -0.3, size = 3.5) +
  labs(
    title = "Distribution of Cyber Incidents (df_2020, non-zero)",
    subtitle = "Most dyad-years with incidents have exactly 1 incident",
    x = "Incident Counts (per dyad-year)", y = "Number of observations (dyad-years)"
  ) +
  theme_minimal()

# --- 2.2 Independent variable: W4 (Winning Coalition Index) ---
# Type: continuous
# Level of measurement: ratio (0 = smallest coalition, 1 = largest)
# Note: attacker_w4 and victim_w4 are the same 174 countries appearing on both
# sides of the directed dyad. Their panel-wide distributions are identical.
# More informative: compare W4 of all countries vs countries that actually
# attacked (non-zero attackers) vs countries that were actually targeted (non-zero victims).

# 2.2a All countries (unique country-year W4 scores)
df_w4_all <- df_2020 %>%
  distinct(attacker, Year, .keep_all = TRUE) %>%
  dplyr::select(country = attacker, Year, w4 = attacker_w4)

cat("--- W4: All Countries (df_2020) ---\n")
cat("Type: continuous\n")
cat("Level of measurement: ratio (0 = smallest coalition, 1 = largest)\n")
cat("Unique country-year observations:", nrow(df_w4_all), "\n")
summary(df_w4_all$w4)
cat("Variance:", var(df_w4_all$w4), "\n")
cat("Standard deviation:", sd(df_w4_all$w4), "\n")
cat("Skewness:", skewness(df_w4_all$w4), "\n\n")

# 2.2b Non-zero attackers (countries that conducted at least one cyber operation)
df_w4_attackers <- df_2020 %>%
  filter(Incident_Count > 0) %>%
  distinct(attacker, Year, .keep_all = TRUE) %>%
  dplyr::select(country = attacker, Year, w4 = attacker_w4)

cat("--- W4: Non-Zero Attackers (df_2020) ---\n")
cat("Unique attacker country-year observations:", nrow(df_w4_attackers), "\n")
cat("Unique attacker countries:", n_distinct(df_w4_attackers$country), "\n")
summary(df_w4_attackers$w4)
cat("Variance:", var(df_w4_attackers$w4), "\n")
cat("Standard deviation:", sd(df_w4_attackers$w4), "\n")
cat("Skewness:", skewness(df_w4_attackers$w4), "\n\n")

# 2.2c Non-zero victims (countries that were targeted at least once)
df_w4_victims <- df_2020 %>%
  filter(Incident_Count > 0) %>%
  distinct(victim, Year, .keep_all = TRUE) %>%
  dplyr::select(country = victim, Year, w4 = victim_w4)

cat("--- W4: Non-Zero Victims (df_2020) ---\n")
cat("Unique victim country-year observations:", nrow(df_w4_victims), "\n")
cat("Unique victim countries:", n_distinct(df_w4_victims$country), "\n")
summary(df_w4_victims$w4)
cat("Variance:", var(df_w4_victims$w4), "\n")
cat("Standard deviation:", sd(df_w4_victims$w4), "\n")
cat("Skewness:", skewness(df_w4_victims$w4), "\n\n")

# Plot 1 (primary): Overlapping density — all three groups
ggplot() +
  geom_density(
    data = df_w4_all,
    aes(x = w4, fill = "All Countries"), alpha = 0.4
  ) +
  geom_density(
    data = df_w4_attackers,
    aes(x = w4, fill = "Non-Zero Attackers"), alpha = 0.35
  ) +
  geom_density(
    data = df_w4_victims,
    aes(x = w4, fill = "Non-Zero Victims"), alpha = 0.35
  ) +
  scale_fill_manual(
    values = c(
      "All Countries" = "#2A9D8F",
      "Non-Zero Attackers" = "steelblue",
      "Non-Zero Victims" = "#E63946"
    ),
    name = NULL
  ) +
  labs(
    title = "W4 Distribution: All Countries vs Attackers vs Victims (df_2020)",
    subtitle = "Teal = all countries baseline. Blue = non-zero attackers. Red = non-zero victims.",
    x = "W4 Score", y = "Density"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# Plot 2 (supplementary): Overlapping histograms — % within each group
df_w4_combined <- bind_rows(
  df_w4_all %>% mutate(group = "All Countries"),
  df_w4_attackers %>% mutate(group = "Non-Zero Attackers"),
  df_w4_victims %>% mutate(group = "Non-Zero Victims")
) %>%
  mutate(group = factor(group, levels = c("All Countries", "Non-Zero Attackers", "Non-Zero Victims")))

w4_colors <- c(
  "All Countries" = "#2A9D8F",
  "Non-Zero Attackers" = "steelblue",
  "Non-Zero Victims" = "#E63946"
)

breaks <- seq(min(df_w4_combined$w4) - 0.01, max(df_w4_combined$w4) + 0.01, length.out = 26)
bin_width <- breaks[2] - breaks[1]

df_w4_binned <- df_w4_combined %>%
  mutate(
    bin_idx = findInterval(w4, breaks, all.inside = TRUE),
    bin_mid = (breaks[bin_idx] + breaks[bin_idx + 1]) / 2
  ) %>%
  count(group, bin_mid) %>%
  group_by(group) %>%
  mutate(pct = n / sum(n) * 100) %>%
  ungroup()

ggplot(df_w4_binned, aes(x = bin_mid, y = pct, fill = group)) +
  geom_col(position = "identity", alpha = 0.4, width = bin_width * 0.95) +
  scale_fill_manual(values = w4_colors, name = NULL) +
  labs(
    title = "W4 Distribution: All Countries vs Attackers vs Victims (df_2020)",
    subtitle = "Each group's bars sum to 100% independently.",
    x = "W4 Score", y = "% of Observations (within group)"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# --- 2.3 Control variable: ln(GDP per capita) ---
# Type: continuous
# Level of measurement: ratio (log-transformed GDP per capita in constant 2015 USD)
# Already log-transformed in data_preparation.R
# Same 174 countries on both sides — analyse once, then split by attacker/victim role.

df_gdp_all <- df_2020 %>%
  distinct(attacker, Year, .keep_all = TRUE) %>%
  dplyr::select(country = attacker, Year, ln_gdp_pc = attacker_ln_gdp_pc)

cat("--- ln(GDP per capita): All Countries (df_2020) ---\n")
cat("Type: continuous\n")
cat("Level of measurement: ratio (log-transformed GDP per capita in constant 2015 USD)\n")
cat("Unique country-year observations:", nrow(df_gdp_all), "\n")
summary(df_gdp_all$ln_gdp_pc)
cat("Variance:", var(df_gdp_all$ln_gdp_pc), "\n")
cat("Standard deviation:", sd(df_gdp_all$ln_gdp_pc), "\n")
cat("Skewness:", skewness(df_gdp_all$ln_gdp_pc), "\n\n")

df_gdp_attackers <- df_2020 %>%
  filter(Incident_Count > 0) %>%
  distinct(attacker, Year, .keep_all = TRUE) %>%
  dplyr::select(country = attacker, Year, ln_gdp_pc = attacker_ln_gdp_pc)

cat("--- ln(GDP per capita): Non-Zero Attackers (df_2020) ---\n")
cat("Unique attacker country-year observations:", nrow(df_gdp_attackers), "\n")
cat("Unique attacker countries:", n_distinct(df_gdp_attackers$country), "\n")
summary(df_gdp_attackers$ln_gdp_pc)
cat("Variance:", var(df_gdp_attackers$ln_gdp_pc), "\n")
cat("Standard deviation:", sd(df_gdp_attackers$ln_gdp_pc), "\n")
cat("Skewness:", skewness(df_gdp_attackers$ln_gdp_pc), "\n\n")

df_gdp_victims <- df_2020 %>%
  filter(Incident_Count > 0) %>%
  distinct(victim, Year, .keep_all = TRUE) %>%
  dplyr::select(country = victim, Year, ln_gdp_pc = victim_ln_gdp_pc)

cat("--- ln(GDP per capita): Non-Zero Victims (df_2020) ---\n")
cat("Unique victim country-year observations:", nrow(df_gdp_victims), "\n")
cat("Unique victim countries:", n_distinct(df_gdp_victims$country), "\n")
summary(df_gdp_victims$ln_gdp_pc)
cat("Variance:", var(df_gdp_victims$ln_gdp_pc), "\n")
cat("Standard deviation:", sd(df_gdp_victims$ln_gdp_pc), "\n")
cat("Skewness:", skewness(df_gdp_victims$ln_gdp_pc), "\n\n")

# Density plot: all countries vs attackers vs victims
ggplot() +
  geom_density(
    data = df_gdp_all,
    aes(x = ln_gdp_pc, fill = "All Countries"), alpha = 0.4
  ) +
  geom_density(
    data = df_gdp_attackers,
    aes(x = ln_gdp_pc, fill = "Non-Zero Attackers"), alpha = 0.35
  ) +
  geom_density(
    data = df_gdp_victims,
    aes(x = ln_gdp_pc, fill = "Non-Zero Victims"), alpha = 0.35
  ) +
  scale_fill_manual(
    values = c(
      "All Countries" = "#2A9D8F",
      "Non-Zero Attackers" = "steelblue",
      "Non-Zero Victims" = "#E63946"
    ),
    name = NULL
  ) +
  labs(
    title = "ln(GDP per capita): All Countries vs Attackers vs Victims (df_2020)",
    subtitle = "Teal = all countries baseline. Blue = non-zero attackers. Red = non-zero victims.",
    x = "ln(GDP per capita)", y = "Density"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# Supplementary histogram
df_gdp_combined <- bind_rows(
  df_gdp_all %>% mutate(group = "All Countries"),
  df_gdp_attackers %>% mutate(group = "Non-Zero Attackers"),
  df_gdp_victims %>% mutate(group = "Non-Zero Victims")
) %>%
  mutate(group = factor(group, levels = c("All Countries", "Non-Zero Attackers", "Non-Zero Victims")))

gdp_breaks <- seq(min(df_gdp_combined$ln_gdp_pc) - 0.01,
  max(df_gdp_combined$ln_gdp_pc) + 0.01,
  length.out = 26
)
gdp_bw <- gdp_breaks[2] - gdp_breaks[1]

df_gdp_binned <- df_gdp_combined %>%
  mutate(
    bin_idx = findInterval(ln_gdp_pc, gdp_breaks, all.inside = TRUE),
    bin_mid = (gdp_breaks[bin_idx] + gdp_breaks[bin_idx + 1]) / 2
  ) %>%
  count(group, bin_mid) %>%
  group_by(group) %>%
  mutate(pct = n / sum(n) * 100) %>%
  ungroup()

ggplot(df_gdp_binned, aes(x = bin_mid, y = pct, fill = group)) +
  geom_col(position = "identity", alpha = 0.4, width = gdp_bw * 0.95) +
  scale_fill_manual(
    values = c(
      "All Countries" = "#2A9D8F",
      "Non-Zero Attackers" = "steelblue",
      "Non-Zero Victims" = "#E63946"
    ),
    name = NULL
  ) +
  labs(
    title = "ln(GDP per capita): All Countries vs Attackers vs Victims (df_2020)",
    subtitle = "Each group's bars sum to 100% independently.",
    x = "ln(GDP per capita)", y = "% of Observations (within group)"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")


################################################################################
#                 PART 3: VARIABLE DESCRIPTIONS (df_2016)
################################################################################

cat("\n============================================================\n")
cat("  Variable Descriptions: df_2016\n")
cat("============================================================\n\n")

# --- 3.1 Dependent variable: Incident_Count ---
cat("--- Incident_Count (Dependent Variable) ---\n")
cat("Type: discrete (count)\n")
cat("Level of measurement: ratio (meaningful zero — no incidents)\n")
summary(df_2016$Incident_Count)
cat("Variance:", var(df_2016$Incident_Count), "\n")
cat("Standard deviation:", sd(df_2016$Incident_Count), "\n")
cat("Skewness:", skewness(df_2016$Incident_Count), "\n")
cat("Zero-inflation:", round(mean(df_2016$Incident_Count == 0) * 100, 2), "%\n\n")

cat("Non-zero incident counts (df_2016):\n")
df_2016 %>%
  filter(Incident_Count > 0) %>%
  count(Incident_Count, name = "Dyad_Years") %>%
  print(n = Inf)

# --- 3.2 W4 (Winning Coalition Index) ---
# Same 174 countries on both sides of the dyad. Shorter time window (2007-2016).
# Showing stats for all countries for completeness; non-zero attacker/victim
# subsets already analysed in Part 2 (df_2020) — distributions should be similar.
df_w4_all_2016 <- df_2016 %>%
  distinct(attacker, Year, .keep_all = TRUE) %>%
  dplyr::select(country = attacker, Year, w4 = attacker_w4)

cat("--- W4: All Countries (df_2016) ---\n")
cat("Type: continuous\n")
cat("Level of measurement: ratio (0 = smallest coalition, 1 = largest)\n")
cat("Unique country-year observations:", nrow(df_w4_all_2016), "\n")
summary(df_w4_all_2016$w4)
cat("Variance:", var(df_w4_all_2016$w4), "\n")
cat("Standard deviation:", sd(df_w4_all_2016$w4), "\n")
cat("Skewness:", skewness(df_w4_all_2016$w4), "\n\n")

# --- 3.3 Control variable: CINC (Composite Index of National Capability) ---
# Type: continuous
# Level of measurement: ratio (share of world total military capability, 0 to ~0.2)
# Same 174 countries on both sides — analyse once, then split by role.
# Note: CINC is heavily right-skewed because a few major powers (US, China, Russia, India)
# hold most of the world's military capability, while ~160 countries have near-zero scores.

df_cinc_all <- df_2016 %>%
  distinct(attacker, Year, .keep_all = TRUE) %>%
  dplyr::select(country = attacker, Year, cinc = attacker_cinc)

cat("--- CINC: All Countries (df_2016) ---\n")
cat("Type: continuous\n")
cat("Level of measurement: ratio (share of world total military capability, 0 to ~0.2)\n")
cat("Unique country-year observations:", nrow(df_cinc_all), "\n")
summary(df_cinc_all$cinc)
cat("Variance:", var(df_cinc_all$cinc), "\n")
cat("Standard deviation:", sd(df_cinc_all$cinc), "\n")
cat("Skewness:", skewness(df_cinc_all$cinc), "\n\n")

df_cinc_attackers <- df_2016 %>%
  filter(Incident_Count > 0) %>%
  distinct(attacker, Year, .keep_all = TRUE) %>%
  dplyr::select(country = attacker, Year, cinc = attacker_cinc)

cat("--- CINC: Non-Zero Attackers (df_2016) ---\n")
cat("Unique attacker country-year observations:", nrow(df_cinc_attackers), "\n")
cat("Unique attacker countries:", n_distinct(df_cinc_attackers$country), "\n")
summary(df_cinc_attackers$cinc)
cat("Variance:", var(df_cinc_attackers$cinc), "\n")
cat("Standard deviation:", sd(df_cinc_attackers$cinc), "\n")
cat("Skewness:", skewness(df_cinc_attackers$cinc), "\n\n")

df_cinc_victims <- df_2016 %>%
  filter(Incident_Count > 0) %>%
  distinct(victim, Year, .keep_all = TRUE) %>%
  dplyr::select(country = victim, Year, cinc = victim_cinc)

cat("--- CINC: Non-Zero Victims (df_2016) ---\n")
cat("Unique victim country-year observations:", nrow(df_cinc_victims), "\n")
cat("Unique victim countries:", n_distinct(df_cinc_victims$country), "\n")
summary(df_cinc_victims$cinc)
cat("Variance:", var(df_cinc_victims$cinc), "\n")
cat("Standard deviation:", sd(df_cinc_victims$cinc), "\n")
cat("Skewness:", skewness(df_cinc_victims$cinc), "\n\n")

# Raw CINC density plot
ggplot() +
  geom_density(
    data = df_cinc_all,
    aes(x = cinc, fill = "All Countries"), alpha = 0.4
  ) +
  geom_density(
    data = df_cinc_attackers,
    aes(x = cinc, fill = "Non-Zero Attackers"), alpha = 0.35
  ) +
  geom_density(
    data = df_cinc_victims,
    aes(x = cinc, fill = "Non-Zero Victims"), alpha = 0.35
  ) +
  scale_fill_manual(
    values = c(
      "All Countries" = "#2A9D8F",
      "Non-Zero Attackers" = "darkorange",
      "Non-Zero Victims" = "#E63946"
    ),
    name = NULL
  ) +
  labs(
    title = "CINC Distribution: All Countries vs Attackers vs Victims (df_2016)",
    subtitle = "Severe right skew — most countries near zero. A few major powers dominate.",
    x = "CINC Score", y = "Density"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# Log-transformed CINC to show reduced skewness
# +0.0001 offset to avoid log(0) for countries with CINC = 0
df_cinc_all <- df_cinc_all %>% mutate(ln_cinc = log(cinc + 0.0001))
df_cinc_attackers <- df_cinc_attackers %>% mutate(ln_cinc = log(cinc + 0.0001))
df_cinc_victims <- df_cinc_victims %>% mutate(ln_cinc = log(cinc + 0.0001))

cat("--- ln(CINC): All Countries (df_2016) ---\n")
summary(df_cinc_all$ln_cinc)
cat("Skewness:", skewness(df_cinc_all$ln_cinc), "\n")
cat("Log transformation reduces skewness substantially.\n\n")

ggplot() +
  geom_density(
    data = df_cinc_all,
    aes(x = ln_cinc, fill = "All Countries"), alpha = 0.4
  ) +
  geom_density(
    data = df_cinc_attackers,
    aes(x = ln_cinc, fill = "Non-Zero Attackers"), alpha = 0.35
  ) +
  geom_density(
    data = df_cinc_victims,
    aes(x = ln_cinc, fill = "Non-Zero Victims"), alpha = 0.35
  ) +
  scale_fill_manual(
    values = c(
      "All Countries" = "#2A9D8F",
      "Non-Zero Attackers" = "darkorange",
      "Non-Zero Victims" = "#E63946"
    ),
    name = NULL
  ) +
  labs(
    title = "ln(CINC) Distribution: All Countries vs Attackers vs Victims (df_2016)",
    subtitle = "Log transformation reduces skewness. Compare group shapes.",
    x = "ln(CINC + 0.0001)", y = "Density"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# Add log-transformed CINC to df_2016 for use in correlation tests below
df_2016 <- df_2016 %>% mutate(attacker_ln_cinc = log(attacker_cinc + 0.0001))


################################################################################
#                 PART 4: CORRELATION TESTS (df_2020)
################################################################################
# Note: with n ~ 392k, p-values will be significant for even trivial correlations.
# Focus on effect size (r), not p-value.

cat("\n============================================================\n")
cat("  Correlation Tests: df_2020\n")
cat("============================================================\n\n")

# 4.1 Core relationship: W4 vs Incident Count
cat("--- 4.1 Core: W4 vs Incident_Count ---\n")
cat("attacker_w4 vs Incident_Count:\n")
cor.test(df_2020$attacker_w4, df_2020$Incident_Count)
cat("victim_w4 vs Incident_Count:\n")
cor.test(df_2020$victim_w4, df_2020$Incident_Count)

# 4.2 Controls vs Incident Count
cat("\n--- 4.2 Controls vs Incident_Count ---\n")
cat("attacker_ln_gdp_pc vs Incident_Count:\n")
cor.test(df_2020$attacker_ln_gdp_pc, df_2020$Incident_Count)
cat("victim_ln_gdp_pc vs Incident_Count:\n")
cor.test(df_2020$victim_ln_gdp_pc, df_2020$Incident_Count)

# 4.3 Collinearity between independent variables
# If r > 0.7 between two predictors, multicollinearity may be a concern
cat("\n--- 4.3 Collinearity check (attacker side) ---\n")
cat("attacker_w4 vs attacker_ln_gdp_pc:\n")
cor.test(df_2020$attacker_w4, df_2020$attacker_ln_gdp_pc)

cat("\n--- 4.4 Collinearity check (victim side) ---\n")
cat("victim_w4 vs victim_ln_gdp_pc:\n")
cor.test(df_2020$victim_w4, df_2020$victim_ln_gdp_pc)


################################################################################
#                 PART 5: CORRELATION TESTS (df_2016)
################################################################################

cat("\n============================================================\n")
cat("  Correlation Tests: df_2016\n")
cat("============================================================\n\n")

# 5.1 Core relationship: W4 vs Incident Count
cat("--- 5.1 Core: W4 vs Incident_Count ---\n")
cat("attacker_w4 vs Incident_Count:\n")
cor.test(df_2016$attacker_w4, df_2016$Incident_Count)
cat("victim_w4 vs Incident_Count:\n")
cor.test(df_2016$victim_w4, df_2016$Incident_Count)

# 5.2 CINC vs Incident Count (both raw and log-transformed)
cat("\n--- 5.2 CINC vs Incident_Count ---\n")
cat("attacker_cinc (raw) vs Incident_Count:\n")
cor.test(df_2016$attacker_cinc, df_2016$Incident_Count)
cat("attacker_ln_cinc (log) vs Incident_Count:\n")
cor.test(df_2016$attacker_ln_cinc, df_2016$Incident_Count)
cat("victim_cinc (raw) vs Incident_Count:\n")
cor.test(df_2016$victim_cinc, df_2016$Incident_Count)

# 5.3 Collinearity: W4 vs CINC
cat("\n--- 5.3 Collinearity check ---\n")
cat("attacker_w4 vs attacker_cinc:\n")
cor.test(df_2016$attacker_w4, df_2016$attacker_cinc)
cat("attacker_w4 vs attacker_ln_cinc:\n")
cor.test(df_2016$attacker_w4, df_2016$attacker_ln_cinc)
cat("victim_w4 vs victim_cinc:\n")
cor.test(df_2016$victim_w4, df_2016$victim_cinc)


################################################################################
#                 PART 6: DIFFERENCE OF MEANS TESTS (df_2020)
################################################################################
# Compare variable means for dyad-years with vs without incidents.
# Appropriate test: two-sample t-test (continuous DV ~ binary IV).
# Note: large sample sizes make even tiny differences statistically significant.
# Focus on the magnitude of the difference, not just the p-value.

cat("\n============================================================\n")
cat("  Difference of Means Tests: df_2020\n")
cat("============================================================\n\n")

df_2020 <- df_2020 %>%
  mutate(has_incident = ifelse(Incident_Count > 0, "Yes", "No"))

# Does W4 differ for attackers who initiated incidents vs those who didn't?
cat("--- attacker_w4 by incident occurrence ---\n")
t.test(attacker_w4 ~ has_incident, data = df_2020)

# Do targeted countries have different W4 scores?
cat("--- victim_w4 by incident occurrence ---\n")
t.test(victim_w4 ~ has_incident, data = df_2020)

# Do wealthier countries initiate more attacks?
cat("--- attacker_ln_gdp_pc by incident occurrence ---\n")
t.test(attacker_ln_gdp_pc ~ has_incident, data = df_2020)

# Are wealthier countries more often targeted?
cat("--- victim_ln_gdp_pc by incident occurrence ---\n")
t.test(victim_ln_gdp_pc ~ has_incident, data = df_2020)


################################################################################
#                 PART 7: DIFFERENCE OF MEANS TESTS (df_2016)
################################################################################

cat("\n============================================================\n")
cat("  Difference of Means Tests: df_2016\n")
cat("============================================================\n\n")

df_2016 <- df_2016 %>%
  mutate(has_incident = ifelse(Incident_Count > 0, "Yes", "No"))

cat("--- attacker_w4 by incident occurrence ---\n")
t.test(attacker_w4 ~ has_incident, data = df_2016)

cat("--- victim_w4 by incident occurrence ---\n")
t.test(victim_w4 ~ has_incident, data = df_2016)

cat("--- attacker_cinc by incident occurrence ---\n")
t.test(attacker_cinc ~ has_incident, data = df_2016)

cat("--- victim_cinc by incident occurrence ---\n")
t.test(victim_cinc ~ has_incident, data = df_2016)
