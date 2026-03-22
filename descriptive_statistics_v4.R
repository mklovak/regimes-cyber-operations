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

##################### Load datasets ############################################
df_2020 <- read_csv("outputs/df_model_2020.csv")
df_2016 <- read_csv("outputs/df_model_2016.csv")

# Create output folder for plots
plot_dir <- "outputs/plots"
if (!dir.exists(plot_dir)) dir.create(plot_dir, recursive = TRUE)

################################################################################
#                 PART 1: DATASET OVERVIEW
################################################################################

# ---- df_2020 overview ----
# Unit of observation: directed dyad-year (attacker -> victim, per year)
# Period: 2007-2020
cat("============================================================\n")
cat("  df_2020 Overview (2007-2020, GDP per capita control)\n")
cat("============================================================\n")
cat("Total observations:", nrow(df_2020), "\n")
cat("Unique countries:", n_distinct(df_2020$attacker), "\n")
cat("Years covered:", length(unique(df_2020$Year)), "(", min(df_2020$Year), "-", max(df_2020$Year), ")\n")
cat("Dyad-years with at least one incident:", sum(df_2020$Incident_Count > 0), "\n")
cat("% non-zero:", round(sum(df_2020$Incident_Count > 0) / nrow(df_2020) * 100, 4), "%\n")
cat("Total incidents:", sum(df_2020$Incident_Count), "\n\n")

# ---- df_2016 overview ----
# Unit of observation: directed dyad-year (attacker -> victim, per year)
# Period: 2007-2016
cat("============================================================\n")
cat("  df_2016 Overview (2007-2016, CINC control)\n")
cat("============================================================\n")
cat("Total observations:", nrow(df_2016), "\n")
cat("Unique countries:", n_distinct(df_2016$attacker), "\n")
cat("Years covered:", length(unique(df_2016$Year)), "(", min(df_2016$Year), "-", max(df_2016$Year), ")\n")
cat("Dyad-years with at least one incident:", sum(df_2016$Incident_Count > 0), "\n")
cat("% non-zero:", round(sum(df_2016$Incident_Count > 0) / nrow(df_2016) * 100, 4), "%\n")
cat("Total incidents:", sum(df_2016$Incident_Count), "\n\n")


##################### 1.1 Incident trend over time (df_2020) ###################

p <- df_2020 %>%
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
print(p)
ggsave(file.path(plot_dir, "1_incidents_per_year.png"), p, width = 10, height = 6)

##################### 1.2 Top 5 attackers (df_2020) ###########################

p <- df_2020 %>%
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
print(p)
ggsave(file.path(plot_dir, "2_top5_attackers.png"), p, width = 8, height = 5)

##################### 1.3 Top 5 victims (df_2020) ############################

p <- df_2020 %>%
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
print(p)
ggsave(file.path(plot_dir, "3_top5_victims.png"), p, width = 8, height = 5)


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
summary(df_2020$Incident_Count)
cat("Variance:", var(df_2020$Incident_Count), "\n")
cat("Standard deviation:", sd(df_2020$Incident_Count), "\n")
cat("Skewness:", skewness(df_2020$Incident_Count), "\n")
cat("Zero-inflation:", round(mean(df_2020$Incident_Count == 0) * 100, 2), "%\n")
cat("Variance >> Mean confirms overdispersion — supports Negative Binomial over Poisson.\n\n")

# Distribution of non-zero incident counts (frequency table)
cat("Non-zero incident counts (df_2020):\n")
df_2020 %>%
  filter(Incident_Count > 0) %>%
  count(Incident_Count, name = "Dyad_Years") %>%
  print(n = Inf)

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

# 2.2b Non-zero attackers
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

# 2.2c Non-zero victims
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

# Plot 1 (primary): Overlapping density
p <- ggplot() +
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
      "All Countries" = "#2A9D8F", "Non-Zero Attackers" = "steelblue",
      "Non-Zero Victims" = "#E63946"
    ), name = NULL
  ) +
  labs(
    title = "W4 Distribution: All Countries vs Attackers vs Victims (df_2020)",
    subtitle = "Teal = all countries baseline. Blue = non-zero attackers. Red = non-zero victims.",
    x = "W4 Score", y = "Density"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")
print(p)
ggsave(file.path(plot_dir, "4_w4_density_2020.png"), p, width = 10, height = 6)

# Plot 2 (supplementary): Overlapping histograms — % within each group
df_w4_combined <- bind_rows(
  df_w4_all %>% mutate(group = "All Countries"),
  df_w4_attackers %>% mutate(group = "Non-Zero Attackers"),
  df_w4_victims %>% mutate(group = "Non-Zero Victims")
) %>%
  mutate(group = factor(group, levels = c("All Countries", "Non-Zero Attackers", "Non-Zero Victims")))

w4_colors <- c(
  "All Countries" = "#2A9D8F", "Non-Zero Attackers" = "steelblue",
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

p <- ggplot(df_w4_binned, aes(x = bin_mid, y = pct, fill = group)) +
  geom_col(position = "identity", alpha = 0.4, width = bin_width * 0.95) +
  scale_fill_manual(values = w4_colors, name = NULL) +
  labs(
    title = "W4 Distribution: All Countries vs Attackers vs Victims (df_2020)",
    subtitle = "Each group's bars sum to 100% independently.",
    x = "W4 Score", y = "% of Observations (within group)"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")
print(p)
ggsave(file.path(plot_dir, "5_w4_histogram_2020.png"), p, width = 10, height = 6)

# --- 2.3 Control variable: ln(GDP per capita) ---
# Type: continuous
# Level of measurement: ratio (log-transformed GDP per capita in constant 2015 USD)
# Already log-transformed in data_preparation.R
# Same 174 countries on both sides — analyse once, then split by attacker/victim role.
#
# Why GDP per capita (not total GDP)?
# GDP per capita proxies for *economic development* — the public goods channel
# in selectorate theory. Large W4 → leaders invest in public goods → higher
# GDP per capita. Total GDP proxies for overall economic *size*, which is a
# different concept: China and Russia have massive total GDP but much lower
# GDP per capita. If we used total GDP, it would behave more like CINC —
# capturing state capacity regardless of whether citizens benefit. GDP per
# capita captures whether wealth reaches the population, which is the
# selectorate theory mechanism.
#
# Why log transformation?
# GDP per capita spans orders of magnitude (from ~$300 Burundi to ~$100,000
# Luxembourg). The log ensures we model proportional differences rather than
# absolute dollar amounts: the difference between $500 and $1,000 matters
# more than the difference between $50,000 and $50,500.
#
# Collinearity note: GDP per capita and W4 are likely correlated (both reflect
# public goods provision). This is also why Internet Penetration was dropped
# (r=0.85 with GDP per capita): internet access is itself a public good. The
# theoretical chain is W4 → GDP/capita → internet penetration. We keep GDP as
# the more fundamental control and drop internet to avoid multicollinearity.

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

# Density plot
p <- ggplot() +
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
      "All Countries" = "#2A9D8F", "Non-Zero Attackers" = "steelblue",
      "Non-Zero Victims" = "#E63946"
    ), name = NULL
  ) +
  labs(
    title = "ln(GDP per capita): All Countries vs Attackers vs Victims (df_2020)",
    subtitle = "Teal = all countries baseline. Blue = non-zero attackers. Red = non-zero victims.",
    x = "ln(GDP per capita)", y = "Density"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")
print(p)
ggsave(file.path(plot_dir, "6_gdp_pc_density_2020.png"), p, width = 10, height = 6)

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

p <- ggplot(df_gdp_binned, aes(x = bin_mid, y = pct, fill = group)) +
  geom_col(position = "identity", alpha = 0.4, width = gdp_bw * 0.95) +
  scale_fill_manual(
    values = c(
      "All Countries" = "#2A9D8F", "Non-Zero Attackers" = "steelblue",
      "Non-Zero Victims" = "#E63946"
    ), name = NULL
  ) +
  labs(
    title = "ln(GDP per capita): All Countries vs Attackers vs Victims (df_2020)",
    subtitle = "Each group's bars sum to 100% independently.",
    x = "ln(GDP per capita)", y = "% of Observations (within group)"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")
print(p)
ggsave(file.path(plot_dir, "7_gdp_pc_histogram_2020.png"), p, width = 10, height = 6)


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
# Level of measurement: ratio (share of world total national material capability, 0 to ~0.2)
# Same 174 countries on both sides — analyse once, then split by role.
#
# CINC is based on six NMC indicators: military expenditure, military personnel,
# energy consumption, iron and steel production, urban population, and total
# population. It therefore captures *overall national material capability*
# (military + industrial + demographic), not purely military strength.
#
# Note: CINC is heavily right-skewed because a few major powers (US, China,
# Russia, India) hold most of the world's material capability, while ~160
# countries have near-zero scores.

df_cinc_all <- df_2016 %>%
  distinct(attacker, Year, .keep_all = TRUE) %>%
  dplyr::select(country = attacker, Year, cinc = attacker_cinc)

cat("--- CINC: All Countries (df_2016) ---\n")
cat("Type: continuous\n")
cat("Level of measurement: ratio (share of world total national material capability, 0 to ~0.2)\n")
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
p <- ggplot() +
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
      "All Countries" = "#2A9D8F", "Non-Zero Attackers" = "darkorange",
      "Non-Zero Victims" = "#E63946"
    ), name = NULL
  ) +
  labs(
    title = "CINC Distribution: All Countries vs Attackers vs Victims (df_2016)",
    subtitle = "Severe right skew — most countries near zero. A few major powers dominate.",
    x = "CINC Score", y = "Density"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")
print(p)
ggsave(file.path(plot_dir, "8_cinc_density_raw_2016.png"), p, width = 10, height = 6)

# Log-transformed CINC to show reduced skewness
# +0.0001 offset to avoid log(0) for countries with CINC = 0
df_cinc_all <- df_cinc_all %>% mutate(ln_cinc = log(cinc + 0.0001))
df_cinc_attackers <- df_cinc_attackers %>% mutate(ln_cinc = log(cinc + 0.0001))
df_cinc_victims <- df_cinc_victims %>% mutate(ln_cinc = log(cinc + 0.0001))

cat("--- ln(CINC): All Countries (df_2016) ---\n")
summary(df_cinc_all$ln_cinc)
cat("Skewness:", skewness(df_cinc_all$ln_cinc), "\n")
cat("Log transformation reduces skewness substantially.\n\n")

p <- ggplot() +
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
      "All Countries" = "#2A9D8F", "Non-Zero Attackers" = "darkorange",
      "Non-Zero Victims" = "#E63946"
    ), name = NULL
  ) +
  labs(
    title = "ln(CINC) Distribution: All Countries vs Attackers vs Victims (df_2016)",
    subtitle = "Log transformation reduces skewness. Compare group shapes.",
    x = "ln(CINC + 0.0001)", y = "Density"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")
print(p)
ggsave(file.path(plot_dir, "9_cinc_density_log_2016.png"), p, width = 10, height = 6)

# Add log-transformed CINC to df_2016 for use in correlation tests below
df_2016 <- df_2016 %>% mutate(attacker_ln_cinc = log(attacker_cinc + 0.0001))


################################################################################
#                 PART 4: CAPABILITY DIAGNOSTIC (df_2016)
################################################################################
# The raw CINC density plot reveals that most countries cluster near zero —
# they likely lack the overall national material capability (military, industrial,
# demographic) to sustain state-backed cyber operations.
#
# This raises a key question: are most zero observations in our data "structural"
# (country is incapable) or "behavioral" (country could attack but chose not to)?
#
# Implications for models:
#   - If most zeros are structural, a standard NB model lumps incapable and
#     restrained states together, potentially conflating capability with choice.
#   - CINC as a control helps: it absorbs the capability channel, isolating
#     W4's effect on *willingness* to attack among capable states.
#   - A Zero-Inflated NB (ZINB) model would address this more formally by
#     separating the "can this dyad produce an incident?" stage (where CINC
#     belongs) from the "how many incidents?" stage (where W4 belongs).
#
# Important: CINC itself may be a *proxy* for low-W4 resource allocation.
# Autocratic leaders (small W4) can channel scarce state resources into
# military and industrial capacity (raising CINC) rather than public goods
# (which would raise GDP per capita). North Korea is the canonical example:
# high CINC (large military, ~1.2M active personnel) but very low GDP per
# capita. NK's CINC ~0.013 for 2007-2016 places it at the 90th percentile of all countries
# (above median of 0.001), while its GDP per capita is ~$1,232 (2015 USD) —
# roughly 45x less than the US (~$56,000) and well below the global median
# (~$4,736). Top 10% in national material capability, bottom tier in economic
# development. So high CINC does not imply defensive posture — it may reflect
# an autocratic investment strategy. CINC captures capacity regardless of
# whether it is used for aggression or defence.

# > exp(median(df_gdp_all$ln_gdp_pc)) # 4736.327

cat("\n============================================================\n")
cat("  Capability Diagnostic (df_2016)\n")
cat("============================================================\n\n")

# Unique country-year level analysis
df_country_year <- df_2016 %>%
  distinct(attacker, Year, .keep_all = TRUE) %>%
  dplyr::select(country = attacker, Year, cinc = attacker_cinc, w4 = attacker_w4)

cat("CINC distribution across all country-years:\n")
cat("  Median:", median(df_country_year$cinc), "\n")
cat("  75th percentile:", quantile(df_country_year$cinc, 0.75), "\n")
cat("  90th percentile:", quantile(df_country_year$cinc, 0.90), "\n")
cat("  95th percentile:", quantile(df_country_year$cinc, 0.95), "\n\n")

# How many countries have zero or near-zero CINC?
near_zero_threshold <- 0.001
n_near_zero <- df_country_year %>%
  filter(cinc < near_zero_threshold) %>%
  distinct(country) %>%
  nrow()
cat(sprintf(
  "Countries with CINC < %.3f (near-zero): %d out of %d (%.1f%%)\n",
  near_zero_threshold, n_near_zero,
  n_distinct(df_country_year$country),
  n_near_zero / n_distinct(df_country_year$country) * 100
))
cat("These states likely lack the national material capability for cyber operations.\n\n")

# Countries that actually attacked — what does their CINC look like?
attackers_cinc <- df_2016 %>%
  filter(Incident_Count > 0) %>%
  distinct(attacker, Year, .keep_all = TRUE) %>%
  dplyr::select(country = attacker, Year, cinc = attacker_cinc, w4 = attacker_w4)

cat("Attacker CINC profile:\n")
cat("  N unique attacker country-years:", nrow(attackers_cinc), "\n")
cat("  Min CINC among attacker country-years:", min(attackers_cinc$cinc), "\n")
cat("  Median CINC among attacker country-years:", median(attackers_cinc$cinc), "\n")
cat("  Mean CINC among attacker country-years:", mean(attackers_cinc$cinc), "\n\n")

# Which attackers have below-median CINC? (punching above their weight)
low_cinc_attackers <- attackers_cinc %>%
  filter(cinc < median(df_country_year$cinc)) %>%
  distinct(country)
cat("Attackers with below-median CINC (punching above their weight):\n")
print(low_cinc_attackers, n = Inf)
cat("\n")

# How many countries are "plausibly capable" of cyber operations?
min_attacker_cinc <- min(attackers_cinc$cinc)
capable_countries <- df_country_year %>%
  filter(cinc >= min_attacker_cinc) %>%
  distinct(country)
cat(sprintf("Minimum CINC among attacker country-years: %.6f\n", min_attacker_cinc))
cat(sprintf(
  "Countries at or above this threshold: %d out of %d (%.1f%%)\n",
  n_distinct(capable_countries$country),
  n_distinct(df_country_year$country),
  n_distinct(capable_countries$country) / n_distinct(df_country_year$country) * 100
))

# Share of incidents by attacker CINC quartile
incidents_by_cinc <- df_2016 %>%
  filter(Incident_Count > 0) %>%
  mutate(cinc_quartile = ntile(attacker_cinc, 4))
cat("\nIncident distribution by attacker CINC quartile:\n")
incidents_by_cinc %>%
  group_by(cinc_quartile) %>%
  summarise(incidents = sum(Incident_Count), dyad_years = n(), .groups = "drop") %>%
  mutate(pct_incidents = round(incidents / sum(incidents) * 100, 1)) %>%
  print()

# W4 distribution among "capable" vs "incapable" states
df_country_year <- df_country_year %>%
  mutate(capable = ifelse(cinc >= min_attacker_cinc, "Capable", "Incapable"))

cat("\nW4 by capability group:\n")
df_country_year %>%
  group_by(capable) %>%
  summarise(
    n_countries = n_distinct(country),
    mean_w4 = round(mean(w4), 4), median_w4 = round(median(w4), 4),
    sd_w4 = round(sd(w4), 4), .groups = "drop"
  ) %>%
  print()
cat("Result: incapable states have slightly HIGHER mean W4 than capable states.\n")
cat("However, the difference is small (0.05) with large overlapping SDs.\n")
cat("The actual W4-CINC correlation is tested in Part 6 below.\n\n")


################################################################################
#                 PART 5: CORRELATION TESTS (df_2020)
################################################################################
# Note: with n ~ 392k, p-values will be significant for even trivial correlations.
# Focus on effect size (r), not p-value.

cat("\n============================================================\n")
cat("  Correlation Tests: df_2020\n")
cat("============================================================\n\n")

# 5.1 Core relationship: W4 vs Incident Count
cat("--- 5.1 Core: W4 vs Incident_Count ---\n")
cat("attacker_w4 vs Incident_Count:\n")
cor.test(df_2020$attacker_w4, df_2020$Incident_Count)
cat("victim_w4 vs Incident_Count:\n")
cor.test(df_2020$victim_w4, df_2020$Incident_Count)

# 5.2 Controls vs Incident Count
cat("\n--- 5.2 Controls vs Incident_Count ---\n")
cat("attacker_ln_gdp_pc vs Incident_Count:\n")
cor.test(df_2020$attacker_ln_gdp_pc, df_2020$Incident_Count)
cat("victim_ln_gdp_pc vs Incident_Count:\n")
cor.test(df_2020$victim_ln_gdp_pc, df_2020$Incident_Count)

# 5.3 Collinearity: W4 vs GDP per capita
# Expected to be correlated: both reflect public goods provision in selectorate
# theory (large W4 → more public goods → higher GDP per capita).
# If r > 0.7, multicollinearity is a concern for including both in the same model.
cat("\n--- 5.3 Collinearity: W4 vs ln(GDP per capita) ---\n")
cat("attacker_w4 vs attacker_ln_gdp_pc:\n")
cor.test(df_2020$attacker_w4, df_2020$attacker_ln_gdp_pc)
cat("victim_w4 vs victim_ln_gdp_pc:\n")
cor.test(df_2020$victim_w4, df_2020$victim_ln_gdp_pc)


################################################################################
#                 PART 6: CORRELATION TESTS (df_2016)
################################################################################

cat("\n============================================================\n")
cat("  Correlation Tests: df_2016\n")
cat("============================================================\n\n")

# 6.1 Core relationship: W4 vs Incident Count
cat("--- 6.1 Core: W4 vs Incident_Count ---\n")
cat("attacker_w4 vs Incident_Count:\n")
cor.test(df_2016$attacker_w4, df_2016$Incident_Count)
cat("victim_w4 vs Incident_Count:\n")
cor.test(df_2016$victim_w4, df_2016$Incident_Count)

# 6.2 CINC vs Incident Count (both raw and log-transformed)
cat("\n--- 6.2 CINC vs Incident_Count ---\n")
cat("attacker_cinc (raw) vs Incident_Count:\n")
cor.test(df_2016$attacker_cinc, df_2016$Incident_Count)
cat("attacker_ln_cinc (log) vs Incident_Count (supplementary — raw CINC used in models):\n")
cor.test(df_2016$attacker_ln_cinc, df_2016$Incident_Count)
cat("victim_cinc (raw) vs Incident_Count:\n")
cor.test(df_2016$victim_cinc, df_2016$Incident_Count)

# 6.3 Collinearity: W4 vs CINC
cat("\n--- 6.3 Collinearity: W4 vs CINC ---\n")
cat("attacker_w4 vs attacker_cinc:\n")
cor.test(df_2016$attacker_w4, df_2016$attacker_cinc)
cat("attacker_w4 vs attacker_ln_cinc (supplementary):\n")
cor.test(df_2016$attacker_w4, df_2016$attacker_ln_cinc)
cat("victim_w4 vs victim_cinc:\n")
cor.test(df_2016$victim_w4, df_2016$victim_cinc)

# 6.4 Cross-control collinearity: CINC vs GDP per capita
# df_2016 has CINC (2007-2016), df_2020 has GDP per capita (2007-2020).
# Merge on overlapping years. CINC captures national material capability
# (military + industrial + demographic). GDP per capita captures economic
# development (the public goods channel). They measure related but distinct
# dimensions of state capacity.
cat("\n--- 6.4 Cross-control: CINC vs ln(GDP per capita) (2007-2016 overlap) ---\n")

df_gdp_for_merge <- df_2020 %>%
  filter(Year >= 2007 & Year <= 2016) %>%
  distinct(attacker, Year, .keep_all = TRUE) %>%
  dplyr::select(country = attacker, Year, ln_gdp_pc = attacker_ln_gdp_pc)

df_cinc_for_merge <- df_2016 %>%
  distinct(attacker, Year, .keep_all = TRUE) %>%
  dplyr::select(country = attacker, Year, cinc = attacker_cinc)

df_cinc_gdp <- inner_join(df_cinc_for_merge, df_gdp_for_merge,
  by = c("country", "Year")
)

cat("Matched country-year observations:", nrow(df_cinc_gdp), "\n\n")

cat("CINC vs ln(GDP per capita):\n")
cor.test(df_cinc_gdp$cinc, df_cinc_gdp$ln_gdp_pc)

cat("ln(CINC) vs ln(GDP per capita) (supplementary):\n")
df_cinc_gdp <- df_cinc_gdp %>% mutate(ln_cinc = log(cinc + 0.0001))
cor.test(df_cinc_gdp$ln_cinc, df_cinc_gdp$ln_gdp_pc)


################################################################################
#                 PART 7: DIFFERENCE OF MEANS TESTS (df_2020)
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

cat("--- attacker_w4 by incident occurrence ---\n")
t.test(attacker_w4 ~ has_incident, data = df_2020)

cat("--- victim_w4 by incident occurrence ---\n")
t.test(victim_w4 ~ has_incident, data = df_2020)

# Do countries with higher GDP per capita initiate more attacks?
cat("--- attacker_ln_gdp_pc by incident occurrence ---\n")
t.test(attacker_ln_gdp_pc ~ has_incident, data = df_2020)

# Are countries with higher GDP per capita more often targeted?
cat("--- victim_ln_gdp_pc by incident occurrence ---\n")
t.test(victim_ln_gdp_pc ~ has_incident, data = df_2020)


################################################################################
#                 PART 8: DIFFERENCE OF MEANS TESTS (df_2016)
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


################################################################################
#                 PART 9: SUMMARY OF DESCRIPTIVE FINDINGS
################################################################################

cat("\n\n============================================================\n")
cat("  Summary of Descriptive Findings\n")
cat("============================================================\n\n")

cat("--- Collinearity ---\n")
cat("W4 vs ln(GDP per capita):   r = 0.32 (moderate, expected from selectorate theory)\n")
cat("W4 vs CINC:                 r = -0.075 (negligible — nearly independent)\n")
cat("CINC vs ln(GDP per capita): r = 0.12 (weak — different dimensions of state capacity)\n")
cat("Conclusion: no multicollinearity issues. CINC and GDP per capita provide\n")
cat("genuinely independent robustness checks across the two model specifications.\n\n")

cat("--- Correlations with Incident_Count ---\n")
cat("attacker_w4:        r = -0.023 (df_2020), -0.019 (df_2016) — small but consistent\n")
cat("victim_w4:          r = +0.009 (df_2020), +0.008 (df_2016) — tiny positive\n")
cat("attacker_cinc:      r = +0.064 — higher CINC states attack more\n")
cat("attacker_ln_gdp_pc: r = +0.002 (p=0.17) — NOT significant\n")
cat("victim_ln_gdp_pc:   r = +0.015 — richer countries get targeted more\n")
cat("Note: with 99.95% zeros, Pearson r understates the true relationship.\n")
cat("T-tests below are more informative.\n\n")

cat("--- Difference of Means (t-tests) ---\n")
cat("attacker_w4:        0.478 (attackers) vs 0.702 (non-attackers) — 0.22 gap on 0-1 scale\n")
cat("victim_w4:          0.755 (victims) vs 0.701 (non-victims) — modest 0.05 gap\n")
cat("attacker_cinc:      0.079 (attackers) vs 0.006 (non-attackers) — 13x higher\n")
cat("victim_cinc:        0.052 (victims) vs 0.006 (non-victims) — powerful states targeted\n")
cat("attacker_ln_gdp_pc: 8.75 vs 8.52 — barely significant (p=0.002)\n")
cat("victim_ln_gdp_pc:   9.53 vs 8.52 — ~1 log point, victims ~2.7x richer\n\n")

cat("--- Key interpretation ---\n")
cat("GDP per capita does NOT predict who attacks (r=0.002, p=0.17) but strongly\n")
cat("predicts who gets targeted (victims ~2.7x richer). Economic development\n")
cat("creates targets worth attacking (financial systems, IP, critical infrastructure)\n")
cat("but does not create motivation to attack. The motivation channel is W4.\n\n")
cat("This also explains why H3 (victim W4 -> more targeted) disappears when GDP\n")
cat("per capita is controlled: victims with high W4 are also rich, and it is the\n")
cat("wealth that makes them targets, not the regime type per se.\n\n")

cat("--- Capability diagnostic ---\n")
cat("51% of countries have near-zero CINC (<0.001) — structural zeros.\n")
cat("Incident distribution across CINC quartiles is nearly flat (21-28%),\n")
cat("meaning CINC alone does not strongly predict who attacks.\n")
cat("W4-CINC correlation is negligible (r = -0.075), so the two variables\n")
cat("operate independently: CINC controls for capacity, W4 captures regime type.\n")
cat("The fact that H1 survives after controlling for CINC confirms that the\n")
cat("regime-type mechanism operates independently of national material capability.\n")

cat("\nPlots saved to:", plot_dir, "\n")
