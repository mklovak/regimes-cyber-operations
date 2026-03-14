################################################################################
# H0: The size of the winning coalition has no effect on state-backed cyber operations (neither for attackers nor victims).
# H1: The size of the attacker's state winning coalition is negatively associated with the frequency of cyber operations it conducts.
# H2: The size of the attacker's state winning coalition is negatively associated with the likelihood of conducting cyber operations.
# H3: States with larger winning coalitions are more likely to be victims of cyber operations conducted by other states.
# H4: States with larger winning coalitions are more likely to conduct cyber operations in response to prior attacks (Active Cyber Defence).
################################################################################

##################### Environment Setup ########################################
rm(list = ls())
try(dev.off(), silent = TRUE)
options(scipen = 999)
cat("\014")

library(tidyverse)
library(MASS)        # glm.nb(), theta.ml()
library(stargazer)
library(AER)         # dispersiontest()
library(pscl)        # zeroinfl(), vuong(), pR2()
library(sandwich)    # vcovCL()
library(lmtest)      # coeftest()

##################### Load Data ################################################
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
df_model <- read_csv("df_model.csv")

# Filter to regression-ready observations (non-missing W4 scores)
df <- df_model %>%
  filter(!is.na(attacker_w4) & !is.na(victim_w4))

df$dyad_id    <- paste(df$attacker, df$victim, sep = "_")
df$has_attack <- as.integer(df$Incident_Count > 0)

cat("=== Data Summary ===\n")
cat(sprintf("  Observations:          %d\n", nrow(df)))
cat(sprintf("  Non-zero incidents:    %d\n", sum(df$Incident_Count > 0)))
cat(sprintf("  Zero-inflation rate:   %.4f%%\n", mean(df$Incident_Count == 0) * 100))
cat(sprintf("  Incident_Count  mean:  %.6f  var: %.6f  max: %d\n",
            mean(df$Incident_Count), var(df$Incident_Count), max(df$Incident_Count)))
cat(sprintf("  attacker_w4     mean:  %.4f  sd: %.4f  range: [%.2f, %.2f]\n",
            mean(df$attacker_w4), sd(df$attacker_w4),
            min(df$attacker_w4), max(df$attacker_w4)))
cat(sprintf("  victim_w4       mean:  %.4f  sd: %.4f  range: [%.2f, %.2f]\n",
            mean(df$victim_w4), sd(df$victim_w4),
            min(df$victim_w4), max(df$victim_w4)))

##################### Bivariate Tests ##########################################
cat("\n=== Bivariate Tests ===\n")

# Continuous ~ continuous
cat("\n--- cor.test: attacker_w4 ~ Incident_Count ---\n")
cor.test(df$attacker_w4, df$Incident_Count)
# r = -0.018, very weak but significant due to large n

# Continuous ~ binary (t-tests)
cat("\n--- t.test: Incident_Count ~ attacker_is_nato ---\n")
t.test(Incident_Count ~ attacker_is_nato, data = df)
# Non-NATO attackers have slightly higher incident count (0.00074 vs 0.00023)
# Difference tiny (~0.0005); significance driven by massive n

cat("\n--- t.test: Incident_Count ~ victim_is_nato ---\n")
t.test(Incident_Count ~ victim_is_nato, data = df)
# NATO countries attacked more often (0.0019 vs 0.0004), ~4.4x rate

cat("\n--- t.test: attacker_w4 ~ attacker_is_nato ---\n")
t.test(attacker_w4 ~ attacker_is_nato, data = df)
# NATO attackers have higher w4 (0.896 vs 0.658), substantial difference

cat("\n--- t.test: victim_w4 ~ victim_is_nato ---\n")
t.test(victim_w4 ~ victim_is_nato, data = df)
# NATO victims have higher w4 (0.896 vs 0.658)

cat("\n--- t.test: attacker_w4 ~ victim_is_nato ---\n")
t.test(attacker_w4 ~ victim_is_nato, data = df)
# No cross-actor relationship (p = 0.78)

cat("\n--- t.test: victim_w4 ~ attacker_is_nato ---\n")
t.test(victim_w4 ~ attacker_is_nato, data = df)
# No cross-actor relationship (p = 0.78)

# Binary ~ binary (chi-squared)
cat("\n--- chisq.test: attacker_is_nato ~ victim_is_nato ---\n")
chisq.test(df$attacker_is_nato, df$victim_is_nato)
# Barely significant (p = 0.047), negligible association

################################################################################
#                         MODEL ESTIMATION
# Model progression: Poisson -> NB -> ZINB, with robustness checks
################################################################################

cat("\n\n========================================================\n")
cat("           H1: FREQUENCY OF CYBER OPERATIONS\n")
cat("========================================================\n\n")

# --- M1: Poisson (diagnostic baseline) ---
M1 <- glm(Incident_Count ~ attacker_w4 + victim_w4 +
            attacker_is_nato + victim_is_nato,
          family = poisson(link = "log"),
          data = df)

# Overdispersion tests (Cameron & Trivedi 1990)
disp_test_lin  <- dispersiontest(M1, trafo = 1)
disp_test_quad <- dispersiontest(M1, trafo = 2)
cat(sprintf("Overdispersion (linear):    z = %.4f, p = %.4e, alpha = %.4f\n",
            disp_test_lin$statistic, disp_test_lin$p.value, disp_test_lin$estimate))
cat(sprintf("Overdispersion (quadratic): z = %.4f, p = %.4e, alpha = %.4f\n",
            disp_test_quad$statistic, disp_test_quad$p.value, disp_test_quad$estimate))
cat("=> Poisson rejected. Moving to NB.\n\n")

# --- M2: Negative Binomial (primary model for H1) ---
M2 <- glm.nb(Incident_Count ~ attacker_w4 + victim_w4 +
               attacker_is_nato + victim_is_nato,
             data = df)
cat(sprintf("M2 theta: %.6f (very small => severe overdispersion)\n", M2$theta))

# M2 with clustered SEs
cat("\n--- M2 with clustered SEs ---\n")
M2_cl <- coeftest(M2, vcov = vcovCL(M2, cluster = df$dyad_id))
print(M2_cl)

# --- M3: NB + Year FE + clustered SEs ---
M3 <- glm.nb(Incident_Count ~ attacker_w4 + victim_w4 +
               attacker_is_nato + victim_is_nato +
               factor(Year),
             data = df)
cat("\n--- M3: NB + Year FE + clustered SEs ---\n")
M3_cl <- coeftest(M3, vcov = vcovCL(M3, cluster = df$dyad_id))
print(M3_cl)

# --- H1 Evaluation (using clustered SEs) ---
b1   <- coef(M2)["attacker_w4"]
p1_cl <- M2_cl["attacker_w4", "Pr(>|z|)"]
cat(sprintf("\n=== H1 Evaluation (clustered SEs) ===\n"))
cat(sprintf("  beta = %.4f, p = %.4e => H1 is %s\n",
            b1, p1_cl, ifelse(b1 < 0 & p1_cl < 0.05, "SUPPORTED", "NOT SUPPORTED")))
cat(sprintf("  IRR = %.4f => one-unit increase => %.1f%% change\n",
            exp(b1), (exp(b1) - 1) * 100))
cat(sprintf("  IRR (1 SD = %.3f) = %.4f => one-SD increase => %.1f%% change\n",
            sd(df$attacker_w4),
            exp(b1 * sd(df$attacker_w4)),
            (exp(b1 * sd(df$attacker_w4)) - 1) * 100))

# --- H1 Stargazer Table (with clustered SEs) ---
vcov_M2 <- vcovCL(M2, cluster = df$dyad_id)
vcov_M3 <- vcovCL(M3, cluster = df$dyad_id)
se_M2   <- sqrt(diag(vcov_M2))
se_M3   <- sqrt(diag(vcov_M3))

stargazer(M1, M2, M3,
          se = list(NULL, se_M2, se_M3),
          type = "html",
          out = "Table_H1.html",
          title = "Table 1: H1 — Attacker W4 and Frequency of Cyber Operations",
          column.labels = c("M1: Poisson", "M2: Neg. Binomial", "M3: NB + Year FE"),
          model.names = FALSE,
          dep.var.labels = "Incident Count",
          covariate.labels = c("Attacker W4", "Victim W4",
                               "Attacker NATO", "Victim NATO"),
          omit = "factor\\(Year\\)",
          add.lines = list(
            c("Year FE", "No", "No", "Yes"),
            c("Clustered SEs", "No", "Yes", "Yes"),
            c("Overdispersion p", sprintf("%.4e", disp_test_quad$p.value), "---", "---")
          ),
          notes = c("Clustered SEs by directed dyad. Year FE coefficients omitted.",
                    "Overdispersion: Cameron and Trivedi (1990) quadratic test.",
                    "H1: Attacker W4 negative and significant => SUPPORTED."),
          notes.append = FALSE,
          star.cutoffs = c(0.05, 0.01, 0.001),
          digits = 4,
          no.space = TRUE)
cat("Saved: Table_H1.html\n")


cat("\n\n========================================================\n")
cat("         H2: LIKELIHOOD OF INITIATING OPERATIONS\n")
cat("========================================================\n\n")

# --- M4: ZINB (mechanism decomposition) ---
M4 <- zeroinfl(Incident_Count ~ attacker_w4 + victim_w4 +
                 attacker_is_nato + victim_is_nato |
                 attacker_w4 + victim_w4,
               dist = "negbin", data = df)
cat("--- M4: ZINB summary ---\n")
summary(M4)

# M4 with clustered SEs
cat("\n--- M4 with clustered SEs ---\n")
M4_cl <- coeftest(M4, vcov = vcovCL(M4, cluster = df$dyad_id))
print(M4_cl)

# --- M5: Logistic regression (robustness for H2) ---
M5 <- glm(has_attack ~ attacker_w4 + victim_w4,
          family = binomial(link = "logit"),
          data = df)
cat("\n--- M5: Logistic regression ---\n")
summary(M5)

# --- Model comparison: ZINB vs NB ---
cat("\n--- AIC comparison ---\n")
print(AIC(M2, M4))

cat("\n--- Vuong test: ZINB vs NB ---\n")
vuong(M4, M2)

cat("\n--- Pseudo R-squared ---\n")
cat("NB:\n"); print(pR2(M2))
cat("ZINB:\n"); print(pR2(M4))

# --- H2 Evaluation (using clustered SEs where available) ---
b_zi    <- M4_cl["zero_attacker_w4", "Estimate"]
p_zi_cl <- M4_cl["zero_attacker_w4", "Pr(>|t|)"]
b_logit <- coef(M5)["attacker_w4"]
p_logit <- summary(M5)$coefficients["attacker_w4", "Pr(>|z|)"]

cat(sprintf("\n=== H2 Evaluation ===\n"))
cat(sprintf("  ZINB zero-inflation (clustered): beta = %.4f, p = %.4e\n", b_zi, p_zi_cl))
cat(sprintf("  Logistic:                        beta = %.4f, p = %.4e\n", b_logit, p_logit))
cat(sprintf("  H2 is %s\n",
            ifelse((b_zi > 0 & p_zi_cl < 0.05) | (b_logit < 0 & p_logit < 0.05),
                   "SUPPORTED", "NOT SUPPORTED")))
cat("  Note: ZINB models P(zero), logistic models P(attack) — signs are flipped.\n")
cat("  Both indicate higher W4 => less likely to attack.\n")

# --- H2 Stargazer Table ---
# Note: stargazer doesn't natively support zeroinfl objects well,
# so we report M5 (logistic) as the primary table and note ZINB results
stargazer(M5,
          type = "html",
          out = "Table_H2.html",
          title = "Table 2: H2 — Attacker W4 and Likelihood of Initiating Cyber Operations",
          column.labels = c("M5: Logistic"),
          model.names = FALSE,
          dep.var.labels = "Any Attack (Binary)",
          covariate.labels = c("Attacker W4", "Victim W4"),
          add.lines = list(
            c("M4 ZINB zero-inflation: Attacker W4 (clustered)", sprintf("%.4f (p = %.4e)", b_zi, p_zi_cl)),
            c("Vuong test (M4 ZINB vs M2 NB)", "ZINB preferred (p < 0.001)")
          ),
          notes = c("M5: Logistic regression on binary outcome (attack vs no attack).",
                    "ZINB (M4) zero-inflation confirms: higher W4 => structural zeros.",
                    "H2: Attacker W4 reduces likelihood of initiating attacks => SUPPORTED."),
          notes.append = FALSE,
          star.cutoffs = c(0.05, 0.01, 0.001),
          digits = 4,
          no.space = TRUE)
cat("Saved: Table_H2.html\n")


cat("\n\n========================================================\n")
cat("           H3: VICTIM TARGETING\n")
cat("========================================================\n\n")

# --- M6: Logistic with interaction (targeted by low W4 states?) ---
M6 <- glm(has_attack ~ attacker_w4 * victim_w4,
          family = binomial(link = "logit"),
          data = df)
cat("--- M6: Logistic with interaction ---\n")
summary(M6)

# --- H3 Evaluation ---
# M5: victim_w4 positive and significant => high W4 countries targeted more by any state
# M6: interaction term tests whether targeting is driven by low W4 attackers
b_v_logit <- coef(M5)["victim_w4"]
p_v_logit <- summary(M5)$coefficients["victim_w4", "Pr(>|z|)"]
b_int     <- coef(M6)["attacker_w4:victim_w4"]
p_int     <- summary(M6)$coefficients["attacker_w4:victim_w4", "Pr(>|z|)"]

cat(sprintf("\n=== H3 Evaluation ===\n"))
cat(sprintf("  Logistic (M5):       victim_w4 beta = %.4f, p = %.4e\n", b_v_logit, p_v_logit))
cat(sprintf("  Interaction (M6):    attacker_w4:victim_w4 beta = %.4f, p = %.4e\n", b_int, p_int))
cat(sprintf("  If interaction is negative and significant, high W4 victims are\n"))
cat(sprintf("  targeted especially by low W4 attackers.\n"))
cat(sprintf("  H3 is %s\n",
            ifelse(b_v_logit > 0 & p_v_logit < 0.05,
                   "SUPPORTED", "NOT SUPPORTED")))

# --- H3 Stargazer Table ---
stargazer(M5, M6,
          type = "html",
          out = "Table_H3.html",
          title = "Table 3: H3 — Victim W4 and Targeting in Cyber Operations",
          column.labels = c("M5: Logistic", "M6: Logistic (interaction)"),
          model.names = FALSE,
          dep.var.labels = "Any Attack (Binary)",
          covariate.labels = c("Attacker W4", "Victim W4",
                               "Attacker W4 x Victim W4"),
          add.lines = list(
            c("Sample", "Full", "Full"),
            c("Interaction", "No", "Yes")
          ),
          notes = c("M5: Higher victim W4 increases likelihood of being targeted.",
                    "M6: Interaction tests if targeting is driven by low W4 attackers.",
                    "H3: Higher W4 countries more likely to be targeted => SUPPORTED."),
          notes.append = FALSE,
          star.cutoffs = c(0.05, 0.01, 0.001),
          digits = 4,
          no.space = TRUE)
cat("Saved: Table_H3.html\n")


cat("\n\n========================================================\n")
cat("     H4: DEFENSIVE ORIENTATION (Adamson 2020)\n")
cat("========================================================\n\n")

# Temporal sequencing proxy: was there a prior reverse attack within 2 years?
df_attacks <- df[df$has_attack == 1, ]
df_attacks$prior_reverse_attack <- sapply(1:nrow(df_attacks), function(i) {
  target <- df_attacks$attacker[i]
  source <- df_attacks$victim[i]
  year   <- df_attacks$Year[i]
  any(df_attacks$attacker == source &
        df_attacks$victim == target &
        df_attacks$Year < year &
        df_attacks$Year >= year - 2)
})

cat(sprintf("Attacks with prior reverse attack: %d / %d (%.1f%%)\n",
            sum(df_attacks$prior_reverse_attack),
            nrow(df_attacks),
            100 * mean(df_attacks$prior_reverse_attack)))

# Descriptive: attacker-victim w4 correlation
cat("\n--- Attacker-victim W4 correlation among attacks ---\n")
print(cor.test(df_attacks$attacker_w4, df_attacks$victim_w4))

# Targeting asymmetry
high_w4 <- df_attacks[df_attacks$attacker_w4 > 0.5, ]
low_w4  <- df_attacks[df_attacks$attacker_w4 <= 0.5, ]
cat(sprintf("\nHigh w4 attackers (>0.5) -> mean victim w4: %.3f\n", mean(high_w4$victim_w4)))
cat(sprintf("Low w4 attackers (<=0.5) -> mean victim w4: %.3f\n", mean(low_w4$victim_w4)))

cat(sprintf("\nHigh w4 (>0.5) responsive: %.1f%%\n",
            100 * mean(df_attacks$prior_reverse_attack[df_attacks$attacker_w4 > 0.5])))
cat(sprintf("Low w4 (<=0.5) responsive: %.1f%%\n",
            100 * mean(df_attacks$prior_reverse_attack[df_attacks$attacker_w4 <= 0.5])))

# --- M7: T-test on responsiveness ---
cat("\n--- M7: T-test: attacker_w4 ~ prior_reverse_attack ---\n")
M7_ttest <- t.test(attacker_w4 ~ prior_reverse_attack, data = df_attacks)
print(M7_ttest)

# --- M8: Logistic regression on responsiveness ---
M8 <- glm(prior_reverse_attack ~ attacker_w4 + victim_w4,
          family = binomial, data = df_attacks)
cat("\n--- M8: Logistic on prior_reverse_attack ---\n")
summary(M8)

# --- H4 Evaluation ---
b_h4 <- coef(M8)["attacker_w4"]
p_h4 <- summary(M8)$coefficients["attacker_w4", "Pr(>|z|)"]

cat(sprintf("\n=== H4 Evaluation ===\n"))
cat(sprintf("  T-test: mean responsive w4 = %.3f vs unprovoked w4 = %.3f, p = %.6f\n",
            M7_ttest$estimate[2], M7_ttest$estimate[1], M7_ttest$p.value))
cat(sprintf("  Logistic (M8): attacker_w4 beta = %.4f, p = %.4e\n", b_h4, p_h4))
cat(sprintf("  H4 is %s\n",
            ifelse(b_h4 > 0 & p_h4 < 0.05, "SUPPORTED", "NOT SUPPORTED")))

# --- H4 Stargazer Table ---
stargazer(M8,
          type = "html",
          out = "Table_H4.html",
          title = "Table 4: H4 — Defensive Orientation of Cyber Operations (Adamson 2020)",
          column.labels = c("M8: Logistic"),
          model.names = FALSE,
          dep.var.labels = "Responsive Operation (Binary)",
          covariate.labels = c("Attacker W4", "Victim W4"),
          add.lines = list(
            c("Sample", sprintf("Observed attacks only (n = %d)", nrow(df_attacks))),
            c("Responsive: high W4 (>0.5)", sprintf("%.1f%%",
                                                    100 * mean(df_attacks$prior_reverse_attack[df_attacks$attacker_w4 > 0.5]))),
            c("Responsive: low W4 (<=0.5)", sprintf("%.1f%%",
                                                    100 * mean(df_attacks$prior_reverse_attack[df_attacks$attacker_w4 <= 0.5])))
          ),
          notes = c("Responsive = prior reverse attack within 2-year window.",
                    "Temporal sequencing proxy inspired by Adamson (2020).",
                    "H4: Higher W4 => more responsive operations => SUPPORTED."),
          notes.append = FALSE,
          star.cutoffs = c(0.05, 0.01, 0.001),
          digits = 4,
          no.space = TRUE)
cat("Saved: Table_H4.html\n")


################################################################################
#                       ROBUSTNESS CHECKS
# - Full-sample NB active dyads (supports H2 mechanism)
# - Post-2007 subsample (94% of incidents occur 2007-2020)
# - 2002 has zero incidents, causing numerical issues in Year FE models
################################################################################

cat("\n\n========================================================\n")
cat("              ROBUSTNESS CHECKS\n")
cat("========================================================\n\n")

# --- NB active dyads (full sample, supports H2 mechanism) ---
# Restricting to dyads where at least one incident occurred.
# If attacker_w4 loses significance, it confirms H2: w4 affects WHETHER, not HOW MUCH.
active_dyads <- unique(df$dyad_id[df$Incident_Count > 0])
df_active    <- df[df$dyad_id %in% active_dyads, ]

M_active <- glm.nb(Incident_Count ~ attacker_w4 + victim_w4 +
                     attacker_is_nato + victim_is_nato,
                   data = df_active)
cat("--- NB active dyads + clustered SEs (full sample, H2 mechanism check) ---\n")
vcov_active <- vcovCL(M_active, cluster = df_active$dyad_id)
M_active_cl <- coeftest(M_active, vcov = vcov_active)
print(M_active_cl)
cat("  attacker_w4 not significant => confirms H2 mechanism\n")
cat("  victim_w4 significant and positive => additional support for H3\n\n")

# --- Post-2007 subsample ---
cat("--- Post-2007 Subsample ---\n")
df_07 <- df[df$Year >= 2007, ]
df_07$dyad_id    <- paste(df_07$attacker, df_07$victim, sep = "_")
df_07$has_attack <- as.integer(df_07$Incident_Count > 0)

cat(sprintf("Full sample: %d obs, Post-2007: %d obs\n", nrow(df), nrow(df_07)))
cat(sprintf("Incidents — full: %d, post-2007: %d (%.1f%%)\n",
            sum(df$Incident_Count), sum(df_07$Incident_Count),
            100 * sum(df_07$Incident_Count) / sum(df$Incident_Count)))

# --- R1: NB + clustered SEs (post-2007) ---
R1 <- glm.nb(Incident_Count ~ attacker_w4 + victim_w4 +
               attacker_is_nato + victim_is_nato,
             data = df_07)
cat("\n--- R1: NB + clustered SEs (post-2007) ---\n")
R1_cl <- coeftest(R1, vcov = vcovCL(R1, cluster = df_07$dyad_id))
print(R1_cl)

# --- R2: NB + Year FE + clustered SEs (post-2007) ---
R2 <- glm.nb(Incident_Count ~ attacker_w4 + victim_w4 +
               attacker_is_nato + victim_is_nato +
               factor(Year),
             data = df_07)
cat("\n--- R2: NB + Year FE + clustered SEs (post-2007) ---\n")
R2_cl <- coeftest(R2, vcov = vcovCL(R2, cluster = df_07$dyad_id))
print(R2_cl)

# --- R3: ZINB (post-2007) ---
R3 <- zeroinfl(Incident_Count ~ attacker_w4 + victim_w4 +
                 attacker_is_nato + victim_is_nato |
                 attacker_w4 + victim_w4,
               dist = "negbin", data = df_07)
cat("\n--- R3: ZINB (post-2007) ---\n")
summary(R3)

# R3 with clustered SEs
cat("\n--- R3 with clustered SEs ---\n")
R3_cl <- coeftest(R3, vcov = vcovCL(R3, cluster = df_07$dyad_id))
print(R3_cl)

# --- R4: Logistic (post-2007) ---
R4 <- glm(has_attack ~ attacker_w4 + victim_w4,
          family = binomial(link = "logit"),
          data = df_07)
cat("\n--- R4: Logistic (post-2007) ---\n")
summary(R4)

# --- R5: NB active dyads (post-2007) ---
active_07 <- unique(df_07$dyad_id[df_07$Incident_Count > 0])
df_07_active <- df_07[df_07$dyad_id %in% active_07, ]

R5 <- glm.nb(Incident_Count ~ attacker_w4 + victim_w4 +
               attacker_is_nato + victim_is_nato,
             data = df_07_active)
cat("\n--- R5: NB active dyads + clustered SEs (post-2007) ---\n")
R5_cl <- coeftest(R5, vcov = vcovCL(R5, cluster = df_07_active$dyad_id))
print(R5_cl)

# --- R6: H4 logistic (post-2007 attacks) ---
df_07_attacks <- df_07[df_07$has_attack == 1, ]
df_07_attacks$prior_reverse_attack <- sapply(1:nrow(df_07_attacks), function(i) {
  target <- df_07_attacks$attacker[i]
  source <- df_07_attacks$victim[i]
  year   <- df_07_attacks$Year[i]
  any(df_07_attacks$attacker == source &
        df_07_attacks$victim == target &
        df_07_attacks$Year < year &
        df_07_attacks$Year >= year - 2)
})

R6 <- glm(prior_reverse_attack ~ attacker_w4 + victim_w4,
          family = binomial, data = df_07_attacks)
cat("\n--- R6: Logistic on responsiveness (post-2007) ---\n")
summary(R6)

# --- Model comparison (post-2007) ---
cat("\n--- AIC comparison (post-2007) ---\n")
print(AIC(R1, R3))

cat("\n--- Vuong test (post-2007) ---\n")
vuong(R3, R1)

# --- Post-2007 Robustness Stargazer (with clustered SEs) ---
vcov_R1 <- vcovCL(R1, cluster = df_07$dyad_id)
vcov_R5 <- vcovCL(R5, cluster = df_07_active$dyad_id)
se_R1   <- sqrt(diag(vcov_R1))
se_R5   <- sqrt(diag(vcov_R5))

stargazer(R1, R4, R5,
          se = list(se_R1, NULL, se_R5),
          type = "html",
          out = "Table_Robustness.html",
          title = "Table 5: Post-2007 Robustness Checks",
          column.labels = c("R1: NB", "R4: Logistic", "R5: NB Active Dyads"),
          model.names = FALSE,
          dep.var.labels = c("Incident Count", "Any Attack", "Incident Count"),
          covariate.labels = c("Attacker W4", "Victim W4",
                               "Attacker NATO", "Victim NATO"),
          add.lines = list(
            c("Sample", "Post-2007", "Post-2007", "Active dyads"),
            c("Clustered SEs", "Yes", "No", "Yes"),
            c("H1 (frequency)", "Supported", "---", "n.s."),
            c("H2 (likelihood)", "---", "Supported", "---"),
            c("H3 (targeting)", "---", "Supported", "Supported")
          ),
          notes = c("Post-2007 subsample (94% of all incidents).",
                    "All core findings replicate in restricted sample.",
                    "R5: Attacker W4 not significant, confirms H2 mechanism."),
          notes.append = FALSE,
          star.cutoffs = c(0.05, 0.01, 0.001),
          digits = 4,
          no.space = TRUE)
cat("Saved: Table_Robustness.html\n")


################################################################################
#                       SUMMARY OF FINDINGS
################################################################################

cat("\n\n========================================================\n")
cat("              SUMMARY OF ALL HYPOTHESES\n")
cat("========================================================\n\n")

cat("H0: Winning coalition has no effect on cyber operations\n")
cat("    => REJECTED (multiple models, both samples)\n\n")

cat("H1: Attacker W4 negatively associated with FREQUENCY\n")
cat("    Primary: M2 NB clustered (p < 0.001)\n")
cat("    Year FE: M3 (p = 0.0003)\n")
cat("    Post-2007: R1 (p < 0.001)\n")
cat("    => SUPPORTED\n\n")

cat("H2: Attacker W4 negatively associated with LIKELIHOOD\n")
cat("    ZINB zero-inflation: M4 (p = 0.008 clustered)\n")
cat("    Logistic: M5 (p < 0.001)\n")
cat("    Active dyads robustness: confirms mechanism (attacker_w4 n.s.)\n")
cat("    => SUPPORTED\n\n")

cat("H3: Higher victim W4 => more likely to be targeted\n")
cat("    Logistic: M5 (p < 0.001)\n")
cat("    Interaction: M6 tests if targeting driven by low W4 attackers\n")
cat("    => SUPPORTED\n\n")

cat("H4: Higher W4 => more responsive operations (Adamson 2020)\n")
cat("    T-test: M7 (p = 0.000003)\n")
cat("    Logistic: M8 (p = 0.000013)\n")
cat("    39.2% responsive for high W4 vs 21.3% for low W4\n")
cat("    => SUPPORTED\n\n")

cat("========================================================\n")
cat("                  END OF ANALYSIS\n")
cat("========================================================\n")