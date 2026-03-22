################################################################################
# Quick test: Do core findings survive with GDP/CINC/Internet controls?
# Run this BEFORE building the full model suite.
################################################################################

rm(list = ls())
options(scipen = 999)
cat("\014")

library(tidyverse)
library(MASS)
library(sandwich)
library(lmtest)
library(pscl)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

################################################################################
#              SPECIFICATION A: CINC (2007-2016)
################################################################################

cat("============================================================\n")
cat("  SPEC A: 2007-2016 (CINC + Internet)\n")
cat("============================================================\n\n")

df_a <- read_csv("df_model2016.csv") %>%
  filter(!is.na(attacker_w4) & !is.na(victim_w4) &
           !is.na(attacker_cinc) & !is.na(victim_cinc) &
           !is.na(attacker_internet) & !is.na(victim_internet))

df_a$dyad_id    <- paste(df_a$attacker, df_a$victim, sep = "_")
df_a$has_attack <- as.integer(df_a$Incident_Count > 0)

cat(sprintf("Observations: %d | Non-zero: %d | Zero-rate: %.4f%%\n\n",
            nrow(df_a), sum(df_a$Incident_Count > 0),
            100 * mean(df_a$Incident_Count == 0)))

# --- A1: NB baseline (W4 + NATO only, replicates original M2) ---
A1 <- glm.nb(Incident_Count ~ attacker_w4 + victim_w4 +
               attacker_is_nato + victim_is_nato,
             data = df_a)
A1_cl <- coeftest(A1, vcov = vcovCL(A1, cluster = df_a$dyad_id))

# --- A2: NB + CINC + Internet (the critical test) ---
A2 <- glm.nb(Incident_Count ~ attacker_w4 + victim_w4 +
               attacker_cinc + victim_cinc +
               attacker_internet + victim_internet +
               attacker_is_nato + victim_is_nato,
             data = df_a)
A2_cl <- coeftest(A2, vcov = vcovCL(A2, cluster = df_a$dyad_id))

# --- A3: NB + CINC + Internet + Year FE ---
A3 <- glm.nb(Incident_Count ~ attacker_w4 + victim_w4 +
               attacker_cinc + victim_cinc +
               attacker_internet + victim_internet +
               attacker_is_nato + victim_is_nato +
               factor(Year),
             data = df_a)
A3_cl <- coeftest(A3, vcov = vcovCL(A3, cluster = df_a$dyad_id))

# --- A4: Logistic (H2: likelihood of attack) ---
A4 <- glm(has_attack ~ attacker_w4 + victim_w4 +
            attacker_cinc + victim_cinc +
            attacker_internet + victim_internet,
          family = binomial(link = "logit"),
          data = df_a)

# --- Print results ---
cat("--- A1: NB baseline (W4 + NATO only) ---\n")
print(A1_cl)

cat("\n--- A2: NB + CINC + Internet (CRITICAL TEST) ---\n")
print(A2_cl)

cat("\n--- A3: NB + CINC + Internet + Year FE ---\n")
print(A3_cl)

cat("\n--- A4: Logistic (likelihood of attack) ---\n")
print(summary(A4)$coefficients)

# --- H1 comparison ---
cat("\n\n=== H1 COMPARISON (Spec A) ===\n")
cat(sprintf("  A1 (baseline):     attacker_w4 = %.4f, p = %.2e\n",
            A1_cl["attacker_w4", "Estimate"], A1_cl["attacker_w4", "Pr(>|z|)"]))
cat(sprintf("  A2 (+ controls):   attacker_w4 = %.4f, p = %.2e\n",
            A2_cl["attacker_w4", "Estimate"], A2_cl["attacker_w4", "Pr(>|z|)"]))
cat(sprintf("  A3 (+ Year FE):    attacker_w4 = %.4f, p = %.2e\n",
            A3_cl["attacker_w4", "Estimate"], A3_cl["attacker_w4", "Pr(>|z|)"]))
cat(sprintf("  A4 (logistic):     attacker_w4 = %.4f, p = %.2e\n",
            coef(A4)["attacker_w4"],
            summary(A4)$coefficients["attacker_w4", "Pr(>|z|)"]))

b_a2 <- A2_cl["attacker_w4", "Estimate"]
p_a2 <- A2_cl["attacker_w4", "Pr(>|z|)"]
cat(sprintf("\n  H1 with controls: %s (beta=%.4f, p=%.2e)\n",
            ifelse(b_a2 < 0 & p_a2 < 0.05, "SUPPORTED", "NOT SUPPORTED"), b_a2, p_a2))
cat(sprintf("  IRR = %.4f => one-unit W4 increase => %.1f%% change in incidents\n",
            exp(b_a2), (exp(b_a2) - 1) * 100))


################################################################################
#              SPECIFICATION B: GDP (2007-2020)
################################################################################

cat("\n\n============================================================\n")
cat("  SPEC B: 2007-2020 (GDP + Internet)\n")
cat("============================================================\n\n")

df_b <- read_csv("df_model2020.csv") %>%
  filter(!is.na(attacker_w4) & !is.na(victim_w4) &
           !is.na(attacker_ln_gdp_pc) & !is.na(victim_ln_gdp_pc) &
           !is.na(attacker_internet) & !is.na(victim_internet))

df_b$dyad_id    <- paste(df_b$attacker, df_b$victim, sep = "_")
df_b$has_attack <- as.integer(df_b$Incident_Count > 0)

cat(sprintf("Observations: %d | Non-zero: %d | Zero-rate: %.4f%%\n\n",
            nrow(df_b), sum(df_b$Incident_Count > 0),
            100 * mean(df_b$Incident_Count == 0)))

# --- B1: NB baseline (W4 + NATO only) ---
B1 <- glm.nb(Incident_Count ~ attacker_w4 + victim_w4 +
               attacker_is_nato + victim_is_nato,
             data = df_b)
B1_cl <- coeftest(B1, vcov = vcovCL(B1, cluster = df_b$dyad_id))

# --- B2: NB + GDP + Internet (the critical test) ---
B2 <- glm.nb(Incident_Count ~ attacker_w4 + victim_w4 +
               attacker_ln_gdp_pc + victim_ln_gdp_pc +
               attacker_internet + victim_internet +
               attacker_is_nato + victim_is_nato,
             data = df_b)
B2_cl <- coeftest(B2, vcov = vcovCL(B2, cluster = df_b$dyad_id))

# --- B3: NB + GDP + Internet + Year FE ---
B3 <- glm.nb(Incident_Count ~ attacker_w4 + victim_w4 +
               attacker_ln_gdp_pc + victim_ln_gdp_pc +
               attacker_internet + victim_internet +
               attacker_is_nato + victim_is_nato +
               factor(Year),
             data = df_b)
B3_cl <- coeftest(B3, vcov = vcovCL(B3, cluster = df_b$dyad_id))

# --- B4: Logistic (H2: likelihood of attack) ---
B4 <- glm(has_attack ~ attacker_w4 + victim_w4 +
            attacker_ln_gdp_pc + victim_ln_gdp_pc +
            attacker_internet + victim_internet,
          family = binomial(link = "logit"),
          data = df_b)

# --- Print results ---
cat("--- B1: NB baseline (W4 + NATO only) ---\n")
print(B1_cl)

cat("\n--- B2: NB + GDP + Internet (CRITICAL TEST) ---\n")
print(B2_cl)

cat("\n--- B3: NB + GDP + Internet + Year FE ---\n")
print(B3_cl)

cat("\n--- B4: Logistic (likelihood of attack) ---\n")
print(summary(B4)$coefficients)

# --- H1 comparison ---
cat("\n\n=== H1 COMPARISON (Spec B) ===\n")
cat(sprintf("  B1 (baseline):     attacker_w4 = %.4f, p = %.2e\n",
            B1_cl["attacker_w4", "Estimate"], B1_cl["attacker_w4", "Pr(>|z|)"]))
cat(sprintf("  B2 (+ controls):   attacker_w4 = %.4f, p = %.2e\n",
            B2_cl["attacker_w4", "Estimate"], B2_cl["attacker_w4", "Pr(>|z|)"]))
cat(sprintf("  B3 (+ Year FE):    attacker_w4 = %.4f, p = %.2e\n",
            B3_cl["attacker_w4", "Estimate"], B3_cl["attacker_w4", "Pr(>|z|)"]))
cat(sprintf("  B4 (logistic):     attacker_w4 = %.4f, p = %.2e\n",
            coef(B4)["attacker_w4"],
            summary(B4)$coefficients["attacker_w4", "Pr(>|z|)"]))

b_b2 <- B2_cl["attacker_w4", "Estimate"]
p_b2 <- B2_cl["attacker_w4", "Pr(>|z|)"]
cat(sprintf("\n  H1 with controls: %s (beta=%.4f, p=%.2e)\n",
            ifelse(b_b2 < 0 & p_b2 < 0.05, "SUPPORTED", "NOT SUPPORTED"), b_b2, p_b2))
cat(sprintf("  IRR = %.4f => one-unit W4 increase => %.1f%% change in incidents\n",
            exp(b_b2), (exp(b_b2) - 1) * 100))


################################################################################
#                        SUMMARY
################################################################################

cat("\n\n============================================================\n")
cat("              QUICK RESULTS SUMMARY\n")
cat("============================================================\n\n")

cat("H1 (attacker W4 -> fewer attacks):\n")
cat(sprintf("  Spec A baseline (W4 only):    beta=%.4f  p=%.2e\n",
            A1_cl["attacker_w4","Estimate"], A1_cl["attacker_w4","Pr(>|z|)"]))
cat(sprintf("  Spec A + CINC + Internet:     beta=%.4f  p=%.2e  %s\n",
            A2_cl["attacker_w4","Estimate"], A2_cl["attacker_w4","Pr(>|z|)"],
            ifelse(A2_cl["attacker_w4","Pr(>|z|)"] < 0.05, "***", "n.s.")))
cat(sprintf("  Spec B baseline (W4 only):    beta=%.4f  p=%.2e\n",
            B1_cl["attacker_w4","Estimate"], B1_cl["attacker_w4","Pr(>|z|)"]))
cat(sprintf("  Spec B + GDP + Internet:      beta=%.4f  p=%.2e  %s\n",
            B2_cl["attacker_w4","Estimate"], B2_cl["attacker_w4","Pr(>|z|)"],
            ifelse(B2_cl["attacker_w4","Pr(>|z|)"] < 0.05, "***", "n.s.")))

cat("\nH3 (victim W4 -> more targeted):\n")
cat(sprintf("  Spec A + controls:            beta=%.4f  p=%.2e  %s\n",
            A2_cl["victim_w4","Estimate"], A2_cl["victim_w4","Pr(>|z|)"],
            ifelse(A2_cl["victim_w4","Pr(>|z|)"] < 0.05, "***", "n.s.")))
cat(sprintf("  Spec B + controls:            beta=%.4f  p=%.2e  %s\n",
            B2_cl["victim_w4","Estimate"], B2_cl["victim_w4","Pr(>|z|)"],
            ifelse(B2_cl["victim_w4","Pr(>|z|)"] < 0.05, "***", "n.s.")))

cat("\nControl variables (Spec A2 — CINC model):\n")
for (v in c("attacker_cinc", "victim_cinc", "attacker_internet", "victim_internet",
            "attacker_is_nato", "victim_is_nato")) {
  cat(sprintf("  %-20s  beta=%.4f  p=%.2e\n", v, A2_cl[v,"Estimate"], A2_cl[v,"Pr(>|z|)"]))
}

cat("\nControl variables (Spec B2 — GDP model):\n")
for (v in c("attacker_ln_gdp_pc", "victim_ln_gdp_pc", "attacker_internet", "victim_internet",
            "attacker_is_nato", "victim_is_nato")) {
  cat(sprintf("  %-20s  beta=%.4f  p=%.2e\n", v, B2_cl[v,"Estimate"], B2_cl[v,"Pr(>|z|)"]))
}

cat("\n============================================================\n")
cat("If attacker_w4 is significant in A2 and B2, your finding\n")
cat("survives controls for state capacity and internet penetration.\n")
cat("If it drops out, the original result was driven by wealth/capability.\n")
cat("============================================================\n")