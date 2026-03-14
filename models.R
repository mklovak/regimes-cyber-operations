##################### Environment Setup ########################################
rm(list = ls())
try(dev.off(), silent = TRUE)
options(scipen = 999)
cat("\014")

library(tidyverse)
library(MASS)        # glm.nb(), theta.ml()
library(stargazer)   
library(AER)         # dispersiontest()
library(pscl)

##################### Load Model Data ##########################################
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

df_model <- read_csv("df_model.csv")

# Filter to regression-ready observations (non-missing W4 scores)
df <- df_model %>%
  filter(!is.na(attacker_w4) & !is.na(victim_w4))

cat("Regression-ready observations:", nrow(df), "\n")
cat("Non-zero Incident_Count:      ", sum(df$Incident_Count > 0), "\n")
cat("Zero-inflation rate:          ", sprintf("%.4f%%", mean(df$Incident_Count == 0) * 100), "\n\n")

##################### Correlations ####################################
cat(sprintf("  Incident_Count  mean: %.6f  var: %.6f  max: %d\n",
            mean(df$Incident_Count), var(df$Incident_Count), max(df$Incident_Count)))
cat(sprintf("  attacker_w4     mean: %.4f  sd: %.4f\n",
            mean(df$attacker_w4), sd(df$attacker_w4)))
cat(sprintf("  victim_w4       mean: %.4f  sd: %.4f\n",
            mean(df$victim_w4), sd(df$victim_w4)))

# 1. Does attacker_w4 correlate with Incident_Count? (continuous variables)
cor.test(df$attacker_w4, df$Incident_Count)
# -0.01786857 correlation is very weak, but significant (p ≈ 0.00000000000000022) due to large sample size (n ≈ 637k).

# 2. Does NATO status relate to Incident_Count? (continuous ~ binary)
t.test(Incident_Count ~ attacker_is_nato, data = df)
# non-NATO attackers have a slightly higher incident count on average (0.00074 vs 0.00023),
# but the difference is tiny (~0.0005). p ≈ 0.000000027 is because of the massive sample size (n ≈ 637k)

t.test(Incident_Count ~ victim_is_nato, data = df)
# NATO countries on average were attacked more often(0.0019 vs 0.0004),
# roughly 4.4x the rate of non-NATO countries.
# Absolute difference is small (~0.0015) due to sparse data (most values are 0).

t.test(attacker_w4 ~ attacker_is_nato, data = df)
# NATO attackers have significantly higher w4 scores (0.896 vs 0.658).
# The NATO vs non-NATO means differ by ~0.24, which is substantial (t = -697.28)

t.test(victim_w4 ~ victim_is_nato, data = df)
# NATO victims have significantly higher w4 scores (0.896 vs 0.658).
# So NATO countries have higher w4 regardless of whether they're the attacker or victim.

t.test(attacker_w4 ~ victim_is_nato, data = df)
# Whether the victim is NATO tells nothing about the attacker's w4. p = 0.76, means are almost identical (0.6932 vs 0.6930)

t.test(victim_w4 ~ attacker_is_nato, data = df)
# Whether the attacker is NATO tells nothing about the victim's w4. p = 0.76, means are almost identical (0.6932 vs 0.6930)

##################### M1: Poisson ##############################################
m_poisson <- glm(Incident_Count ~ attacker_w4 + victim_w4 +
                   attacker_is_nato + victim_is_nato,
                 family = poisson(link = "log"),
                 data = df)

# Overdispersion test (Cameron-Trivedi)
dispersiontest(m_poisson, trafo = 1)  # linear: Var = μ + α·μ
# Alpha = 1.3862, p = 0.0000023 --> strong evidence of overdispersion.
dispersiontest(m_poisson, trafo = 2)  # quadratic: Var = μ + α·μ² (NB structure)
# Alpha = 603.691, p = 0.001543 --> strong evidence of overdispersion.

##################### M2: Negative Binomial ####################################

### Two-step theta estimation approach ###
theta_est <- theta.ml(df$Incident_Count, fitted(m_poisson))
cat(sprintf("  Estimated theta: %.6f\n", theta_est))
m_nb <- glm(Incident_Count ~ attacker_w4 + victim_w4 +
              attacker_is_nato + victim_is_nato,
            family = negative.binomial(theta = theta_est),
            data = df)
summary(m_nb)

### glm.nb() approach, which estimates theta internally ###
m_nb2 <- glm.nb(Incident_Count ~ attacker_w4 + victim_w4 +
                  attacker_is_nato + victim_is_nato,
                data = df)
summary(m_nb2)

### Comparing results ###
cat(sprintf("glm.nb theta:   %.6f\n", m_nb2$theta))
# glm.nb theta:   0.000211, AIC: 4499
cat(sprintf("Two-step theta: %.6f\n", theta_est))
# Two-step theta: 0.000076, AIC: 4717

# glm.nb() is preferred (lower AIC: 4499 vs 4717)

### Interpretations ###
# attacker_w4 is strongly negative (-4.14 on (log scale)): attacker's with higher w4 initiated fewer incidents then attackers with lower w4
range(df$attacker_w4) # 0.07267195 1.00000000
sd(df$attacker_w4) # 0.205455
exp(-4.14 * (1.0 - 0.073))  # ≈ 0.021
# One SD increase (0.21) in attacker_w4 → ~57% fewer incidents
# Across full range (0.07 to 1.00) → ~97.8% fewer incidents
# Countries with higher w4 scores are far less likely to initiate attacks

# victim_is_nato coefficient: 1.08 (log scale)
# exp(1.08) ≈ 2.94 → NATO countries experienced ~3x more attacks than non-NATO countries

# victim_w4 coefficient: -0.07 (log scale), p = 0.91
# The victim's w4 score has no meaningful effect on how often they're attacked

# attacker_is_nato coefficient: 0.67 (log scale), p = 0.059
# exp(0.67) ≈ 1.96 → NATO countries initiate roughly 2x more attacks than non-NATO
# BUT this is borderline non-significant at the 0.05 threshold
# This seems to contradict attacker_w4 (which says higher w4 → fewer attacks,
# and NATO countries have higher w4). 
# The likely explanation is that once I control for w4, NATO membership itself may 
# slightly increase attack initiation (among countries with similar w4 scores, NATO ones might be slightly more aggressive),
# but the evidence is not strong (p = 0.059).

# BUT theta << 1 (≈ 0.0002) confirms severe overdispersion --> still straining to handle excess zeros,
# so move to zero-inflated models

##################### H1 Test ##################################################
b1 <- coef(m_nb2)["attacker_w4"]
p1 <- summary(m_nb2)$coefficients["attacker_w4", "Pr(>|z|)"]
h1_supported <- b1 < 0 & p1 < 0.05

cat(sprintf("H1: attacker_w4 is negative and significant?\n"))
cat(sprintf("  β = %.4f, p = %.4e → H1 is %s\n",
            b1, p1, ifelse(h1_supported, "SUPPORTED", "NOT SUPPORTED")))
cat(sprintf("  IRR = %.4f → one-unit increase → %.1f%% change\n",
            exp(b1), (exp(b1) - 1) * 100))
cat(sprintf("  IRR (1 SD = %.3f) = %.4f → one-SD increase → %.1f%% change\n",
            sd(df$attacker_w4),
            exp(b1 * sd(df$attacker_w4)),
            (exp(b1 * sd(df$attacker_w4)) - 1) * 100))

# Stargazer HTML output
stargazer(m_poisson, m_nb2,
          type = "html",
          out = "regression_table_H1.html",
          title = "Selectorate Theory and Cyber Operations (H1)",
          column.labels = c("Poisson", "Neg. Binomial"),
          model.names = FALSE,
          dep.var.labels = "Incident Count",
          covariate.labels = c("Attacker W4 (B1)", "Victim W4 (B2)",
                               "Attacker NATO (Z1)", "Victim NATO (Z2)"),
          add.lines = list(
            c("Theta", "---", sprintf("%.6f", m_nb2$theta)),
            c("Overdispersion p-value", sprintf("%.4e", disp_test$p.value), "---")
          ),
          notes = c("Overdispersion: Cameron-Trivedi test, p < 0.001.",
                    "NB theta << 1 indicates excess zeros.",
                    "H1: Attacker W4 negative and significant."),
          notes.append = FALSE,
          star.cutoffs = c(0.05, 0.01, 0.001),
          digits = 4,
          no.space = TRUE)
cat("\nSaved: regression_table_H1.html\n")

##################### M3: Zero-inflated Negative Binomial ######################
# Left side of | = count model ("how many incidents?", given they can occur) depends on all four variables including NATO status.
# Right side of | = zero-inflation model (probability  that an observation is a structural zero — "can incidents occur between these two countries in this year at all?").
m_zinb2 <- zeroinfl(Incident_Count ~ attacker_w4 + victim_w4 +
                      attacker_is_nato + victim_is_nato |
                      attacker_w4 + victim_w4,
                    dist = "negbin", data = df)
summary(m_zinb2)
# Zero-inflation model:
# attacker_w4 strongly positive (3.34, p ≈ 0). Higher w4 attackers are much more likely to never attack. 
# The w4 effect is primarily about whether a country attacks at all, not how much. v
# ictim_w4 not significant (p = 0.11).

# Count model:
# attacker_w4 is a borderline (p = 0.097) and the coefficient shrank from -4.14 to -0.98. 
# This means once you account for the structural zeros, w4 doesn't strongly predict how many incidents occur.
# victim_is_nato is significant (1.14, p ≈ 0.000001). Among pairs where incidents can occur, NATO countries are attacked more. exp(1.14) ≈ 3.1x.

AIC(m_nb2, m_zinb2)
# best AIC fo far - 4352 

vuong(m_zinb2, m_nb2)
# ZINB is a meaningfully better fit than NB alone.

##################### Robustness checks ################################################
### M4: Logit ###
# Logistic regression on binary outcome (any attack vs none)
df$has_attack <- as.integer(df$Incident_Count > 0)
m_logit <- glm(has_attack ~ attacker_w4 + victim_w4,
               family = binomial(link = "logit"),
               data = df)
summary(m_logit)
# attacker_w4: strongly negative (-3.77). higher w4 countries less likely to attack
# victim_w4: here is significant (+1.84, p < 0.001), compared to borderline in ZINB (M3). higher w4 countries may be more attractive targets

### Clustered standard errors ###
# For NB model:
df$dyad_id <- paste(df$attacker, df$victim, sep = "_") # dyad identifier
coeftest(m_nb2, vcov = vcovCL(m_nb2, cluster = df$dyad_id))
# only attacker_w4 remains significant (p < 0.001)
# victim_is_nato drops to borderline (p = 0.055)
# attacker_is_nato now non-significant (p = 0.295)
# H1 (attacker w4 → fewer incidents) is robust to clustering

### For ZINB model
coeftest(m_zinb2, vcov = vcovCL(m_zinb2, cluster = df$dyad_id))
# t test of coefficients:
#   
#   Estimate Std. Error t value Pr(>|t|)   
# count_(Intercept)      -1.84096   39.39265 -0.0467 0.962726   
# count_attacker_w4      -0.97583    0.88215 -1.1062 0.268644   
# count_victim_w4         0.27163    0.98342  0.2762 0.782384   
# count_attacker_is_nato  0.68496    0.78486  0.8727 0.382819   
# count_victim_is_nato    1.14187    2.51192  0.4546 0.649411   
# zero_(Intercept)        4.19482   41.38842  0.1014 0.919271   
# zero_attacker_w4        3.33571    1.25739  2.6529 0.007981 **
#   zero_victim_w4         -0.90014    3.12732 -0.2878 0.773476   

# Only attacker_w4 in zero-inflation part survives (p = 0.008)
# Count model: nothing significant — SEs inflated, possible instability
# Core finding robust: higher w4 countries are less likely to be aggressors
# H1 supported across NB and ZINB with and without clustering

# SUMMARY:
# Primary model: NB with clustered SEs — robust, interpretable, H1 clearly supported
# Supporting evidence: ZINB without clustering — shows the mechanism (w4 drives whether attacks happen at all)
# Robustness checks: ZINB with clustered SEs and standalone logistic regression — both confirm the direction.

### Year fixed effectsfor NB model ###
m_nb_yr <- glm.nb(Incident_Count ~ attacker_w4 + victim_w4 +
                    attacker_is_nato + victim_is_nato +
                    factor(Year),
                  data = df)
coeftest(m_nb_yr, vcov = vcovCL(m_nb_yr, cluster = df$dyad_id))
# z test of coefficients:
# 
#                   Estimate Std. Error  z value              Pr(>|z|)    
# (Intercept)       -7.98188    1.13924  -7.0063     0.000000000002447 ***
# attacker_w4       -3.91028    1.08328  -3.6097             0.0003066 ***
# victim_w4         -0.13226    1.16128  -0.1139             0.9093211    
# attacker_is_nato   0.43709    0.65258   0.6698             0.5029954    
# victim_is_nato     0.90477    0.54974   1.6458             0.0998064 .  
# factor(Year)2001   1.64227    0.90007   1.8246             0.0680603 .  
# factor(Year)2002 -26.51062    0.99397 -26.6714 < 0.00000000000000022 ***
# factor(Year)2003   1.34587    1.35238   0.9952             0.3196459    
# factor(Year)2004   1.21076    1.19936   1.0095             0.3127326    
# factor(Year)2005   1.39873    1.13253   1.2350             0.2168147    
# factor(Year)2006   1.72155    0.95886   1.7954             0.0725865 .  
# factor(Year)2007   2.54578    1.04976   2.4251             0.0153039 *  
# factor(Year)2008   3.47551    1.00707   3.4511             0.0005583 ***
# factor(Year)2009   2.94842    1.08036   2.7291             0.0063503 ** 
# factor(Year)2010   3.10557    1.02733   3.0230             0.0025031 ** 
# factor(Year)2011   3.05869    1.02259   2.9911             0.0027795 ** 
# factor(Year)2012   1.76577    1.08505   1.6274             0.1036610    
# factor(Year)2013   2.72259    1.00101   2.7198             0.0065313 ** 
# factor(Year)2014   3.35583    0.99994   3.3560             0.0007907 ***
# factor(Year)2015   3.34656    0.91486   3.6580             0.0002542 ***
# factor(Year)2016   3.22593    1.05168   3.0674             0.0021593 ** 
# factor(Year)2017   2.85033    0.82462   3.4565             0.0005472 ***
# factor(Year)2018   3.15844    1.01941   3.0983             0.0019464 ** 
# factor(Year)2019   2.98510    1.03947   2.8718             0.0040820 ** 
# factor(Year)2020   2.68440    0.92692   2.8960             0.0037790 ** 

###  Year + Country fixed effects ### 
m_nb_fe <- glm.nb(Incident_Count ~ attacker_w4 + victim_w4 +
                    attacker_is_nato + victim_is_nato +
                    factor(Year) +
                    factor(attacker) + factor(victim),
                  data = df)
coeftest(m_nb_fe, vcov = vcovCL(m_nb_fe, cluster = df$dyad_id))
# Computationally brutal model that never finished running with hundreds of unique countries on both sides adding potentially thousands of dummy variables to a 637k observation NB model

### Restricting to country pairs where at least one incident occurred across the entire period ###
active_dyads <- unique(df$dyad_id[df$Incident_Count > 0]) # dyads that ever had an incident
df_active <- df[df$dyad_id %in% active_dyads, ]

# NB on active dyads only
m_nb_active <- glm.nb(Incident_Count ~ attacker_w4 + victim_w4 +
                        attacker_is_nato + victim_is_nato,
                      data = df_active)
coeftest(m_nb_active, vcov = vcovCL(m_nb_active, cluster = df_active$dyad_id))
# attacker_w4 not significant (p = 0.12) — H1 not supported in this subsample
# This is consistent with H2 interpretation (w4 affects WHETHER, not HOW MUCH)
# but does not directly test H2
# victim_w4 is significant (p = 0.02, positive) in m_nb_active, which supports H3

# McFadden's pseudo-R²
# For NB model
1 - (m_nb2$twologlik / m_nb2$null.deviance)

# Or more reliably using the pscl package
pR2(m_nb2)
pR2(m_zinb2)

# Predicted vs actual counts 
pred <- predict(m_nb2, type = "response")
cor(pred, df$Incident_Count)^2  # squared correlation between predicted and actual

##################### Post-2007 Subsample ######################################

df_07 <- df[df$Year >= 2007, ]
cat(sprintf("Full sample: %d obs\nPost-2007: %d obs\n", nrow(df), nrow(df_07)))
cat(sprintf("Incidents - full: %d, post-2007: %d\n",
            sum(df$Incident_Count), sum(df_07$Incident_Count)))

# Dyad ID for clustering
df_07$dyad_id <- paste(df_07$attacker, df_07$victim, sep = "_")

# Binary outcome for logistic
df_07$has_attack <- as.integer(df_07$Incident_Count > 0)

##################### Poisson + overdispersion test ############################
p07_poisson <- glm(Incident_Count ~ attacker_w4 + victim_w4 +
                     attacker_is_nato + victim_is_nato,
                   family = poisson(link = "log"),
                   data = df_07)
disp_test_07 <- dispersiontest(p07_poisson, trafo = 2)
cat(sprintf("Overdispersion test: z = %.4f, p = %.4e\n",
            disp_test_07$statistic, disp_test_07$p.value))

##################### NB #######################################################
p07_nb <- glm.nb(Incident_Count ~ attacker_w4 + victim_w4 +
                   attacker_is_nato + victim_is_nato,
                 data = df_07)
summary(p07_nb)
cat(sprintf("Theta: %.6f\n", p07_nb$theta))

# NB with clustered SEs
coeftest(p07_nb, vcov = vcovCL(p07_nb, cluster = df_07$dyad_id))

##################### NB + Year FE + clustered SEs #############################
p07_nb_yr <- glm.nb(Incident_Count ~ attacker_w4 + victim_w4 +
                      attacker_is_nato + victim_is_nato +
                      factor(Year),
                    data = df_07)
coeftest(p07_nb_yr, vcov = vcovCL(p07_nb_yr, cluster = df_07$dyad_id))

##################### ZINB #####################################################
p07_zinb <- zeroinfl(Incident_Count ~ attacker_w4 + victim_w4 +
                       attacker_is_nato + victim_is_nato |
                       attacker_w4 + victim_w4,
                     dist = "negbin", data = df_07)
summary(p07_zinb)

# ZINB with clustered SEs
coeftest(p07_zinb, vcov = vcovCL(p07_zinb, cluster = df_07$dyad_id))

##################### Logistic regression ######################################
p07_logit <- glm(has_attack ~ attacker_w4 + victim_w4,
                 family = binomial(link = "logit"),
                 data = df_07)
summary(p07_logit)

##################### Active dyads #############################################
active_dyads_07 <- unique(df_07$dyad_id[df_07$Incident_Count > 0])
df_07_active <- df_07[df_07$dyad_id %in% active_dyads_07, ]

p07_nb_active <- glm.nb(Incident_Count ~ attacker_w4 + victim_w4 +
                          attacker_is_nato + victim_is_nato,
                        data = df_07_active)
coeftest(p07_nb_active, vcov = vcovCL(p07_nb_active, cluster = df_07_active$dyad_id))

##################### Model comparison #########################################
AIC(p07_nb, p07_zinb)
vuong(p07_zinb, p07_nb)
pR2(p07_nb)
pR2(p07_zinb)
# Post-2007 robustness check (94% of incidents occur in this period):
# All findings replicate and are slightly stronger
# H1: supported — NB clustered (p < 0.001)
# H2: supported — ZINB zero-inflation clustered (p < 0.001), logistic (p < 0.001)
# H3: supported — logistic (p < 0.001), active dyads (p = 0.044)
# Convergence warning on Year FE model — note in footnote



# Predicted probability of attack at different w4 levels
w4_range <- seq(0.07, 1.0, by = 0.01)

# Using the logistic model (simplest, clearest)
pred_probs <- predict(p07_logit, 
                      newdata = data.frame(attacker_w4 = w4_range,
                                           victim_w4 = mean(df_07$victim_w4)),
                      type = "response")

# Plot it
plot(w4_range, pred_probs, type = "l",
     xlab = "Attacker W4", ylab = "Predicted P(attack)",
     main = "Probability of Attack by Attacker W4")

# Find the w4 where probability drops below key thresholds
data.frame(w4 = w4_range, prob = round(pred_probs, 6))

# W4 distribution of actual attackers vs non-attackers
summary(df_07$attacker_w4[df_07$has_attack == 1])
summary(df_07$attacker_w4[df_07$has_attack == 0])

# Which countries actually attack?
attackers <- unique(df_07$attacker[df_07$has_attack == 1])
attacker_w4s <- sapply(attackers, function(a) {
  mean(df_07$attacker_w4[df_07$attacker == a])
})
sort(attacker_w4s)

# Relative to the lowest w4 country
data.frame(
  w4 = c(0.10, 0.25, 0.50, 0.75, 1.00),
  prob = round(pred_probs[c(4, 19, 44, 69, 94)], 6),
  relative_to_lowest = round(pred_probs[c(4, 19, 44, 69, 94)] / pred_probs[1], 2)
)
# Which countries actually attack and what are their w4 scores?
attackers <- unique(df_07$attacker[df_07$has_attack == 1])
attacker_w4s <- sapply(attackers, function(a) {
  mean(df_07$attacker_w4[df_07$attacker == a])
})
sort(attacker_w4s)

# What percentage of attacks come from countries below certain w4 levels?
attacks <- df_07[df_07$has_attack == 1, ]
cat(sprintf("Attacks from w4 < 0.25: %.1f%%\n", 
            100 * sum(attacks$attacker_w4 < 0.25) / nrow(attacks)))
cat(sprintf("Attacks from w4 < 0.50: %.1f%%\n", 
            100 * sum(attacks$attacker_w4 < 0.50) / nrow(attacks)))
cat(sprintf("Attacks from w4 < 0.75: %.1f%%\n", 
            100 * sum(attacks$attacker_w4 < 0.75) / nrow(attacks)))

# Among high w4 attackers, are their targets mostly low w4?
# (suggesting retaliation against autocracies)
high_w4_attacks <- attacks[attacks$attacker_w4 > 0.75, ]
low_w4_attacks <- attacks[attacks$attacker_w4 < 0.50, ]

cat(sprintf("High w4 attackers → mean victim w4: %.3f\n", mean(high_w4_attacks$victim_w4)))
cat(sprintf("Low w4 attackers → mean victim w4: %.3f\n", mean(low_w4_attacks$victim_w4)))
cor.test(attacks$attacker_w4, attacks$victim_w4)
# Among observed attacks (n = 205), attacker and victim w4 are negatively
# correlated (r = -0.305, p < 0.00001).
# High w4 states target low w4 states (mean victim w4 = 0.507)
# Low w4 states target high w4 states (mean victim w4 = 0.775)
# Consistent with asymmetric offensive-defensive dynamic:
#   autocracies initiate → democracies respond with active cyber defence


# For each incident, check if there was a prior attack in the reverse direction
# within some time window (e.g., 1-2 years)
df_attacks <- df_07[df_07$has_attack == 1, ]
df_attacks$prior_reverse_attack <- sapply(1:nrow(df_attacks), function(i) {
  target <- df_attacks$attacker[i]
  source <- df_attacks$victim[i]
  year <- df_attacks$Year[i]
  any(df_attacks$attacker == source & 
        df_attacks$victim == target & 
        df_attacks$Year < year & 
        df_attacks$Year >= year - 2)
})
df_attacks$w4_diff <- df_attacks$attacker_w4 - df_attacks$victim_w4

df_attacks$dyad_pair <- paste(pmin(df_attacks$attacker, df_attacks$victim),
                              pmax(df_attacks$attacker, df_attacks$victim), 
                              sep = "_")
# Check if attacks go both ways for this pair
reciprocal_pairs <- names(which(table(
  paste(df_attacks$attacker, df_attacks$victim, sep = "->")
) > 0))
# Test it
t.test(attacker_w4 ~ prior_reverse_attack, data = df_attacks)

# Or logistic
glm(prior_reverse_attack ~ attacker_w4 + victim_w4,
    family = binomial, data = df_attacks)



summary(glm(prior_reverse_attack ~ attacker_w4 + victim_w4,
            family = binomial, data = df_attacks))

# Also check the proportions
table(df_attacks$prior_reverse_attack, df_attacks$attacker_w4 > 0.5)

# What percentage of high vs low w4 attacks are responsive?
cat(sprintf("High w4 (>0.5) responsive: %.1f%%\n",
            100 * mean(df_attacks$prior_reverse_attack[df_attacks$attacker_w4 > 0.5])))
cat(sprintf("Low w4 (<=0.5) responsive: %.1f%%\n",
            100 * mean(df_attacks$prior_reverse_attack[df_attacks$attacker_w4 <= 0.5])))

