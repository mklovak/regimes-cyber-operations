_Developed with the assistance of Claude AI (Anthropic)._

# Project Structure

## Data Pipeline (run in the following order)

```
data_preparation.R       →  builds df_model_2020.csv, df_model_2016.csv, df_model_2014.csv
data_preparation_cfr.R   →  builds df_attacker_2020.csv, df_attacker_2016.csv (CFR robustness)
descriptive_statistics.R →  diagnostics, correlations, Poisson vs NB likelihood-ratio test
models.R                 →  primary models M1–M7; diagnostics D1–D3
robustness.R             →  robustness checks R1–R6 (MID-any, top-3 exclusion,
                            CFR alternative dataset); diagnostics D4–D5
```

> **Model families**: `M1–M7` are the primary models (DCID dyadic). `D1–D5` are
> diagnostics (not robustness checks): D1–D2 probe the H2 extensive margin and
> D3 the M7 stage-placement choice (in `models.R`); D4–D5 are supplementary
> top-3-exclusion diagnostics (in `robustness.R`). `R1–R6` are the robustness
> checks — external challenges to the finding (MID-any, top-3 exclusion, CFR
> dataset). The Poisson vs NB likelihood-ratio test lives in
> `descriptive_statistics.R`. The CFR analysis is integrated into `robustness.R`
> as R3–R6.

## Project Structure

```text
.
├── data sources/                                       # Raw data sources
│   ├── COW-country-codes.csv                           # Correlates of War, Country Codes
│   ├── NMC-60-abridged.csv                             # Correlates of War, National Material Capabilities (CINC)
│   ├── DCID_2.0_Release_update_February_2023.xlsx     # Dyadic Cyber Incident and Campaign Data v2.0
│   ├── cfr_attacker_coded.xlsx                         # CFR Cyber Operations Tracker (with sponsor coding)
│   ├── dyadic_mid_4.03.csv                             # COW Dyadic Militarized Interstate Disputes v4.03
│   ├── NewWmeasure.csv                                 # Winning Coalition Index (W4)
│   ├── a14efc5a-..._Data.csv                           # World Bank, GDP per capita (constant 2015 USD)
│   ├── Indicators_NK.xlsx                              # Bank of Korea, North Korea economic indicators
│   ├── E018101010_002034281.csv                        # National Statistics, Taiwan (GDP)
│   ├── Individuals using the Internet.csv              # ITU, Internet Penetration (Taiwan)
│   └── 3d38d8b0-..._Data.csv                           # World Bank, Internet Penetration (unused — collinear with GDP per capita, r = 0.85)
│
├── outputs/                                            # Generated outputs (committed so reviewers can download tables and plots)
│   ├── df_model_2020.csv                               # DCID Panel A: 2007–2020, W4 + ln(GDP per capita)
│   ├── df_model_2016.csv                               # DCID Panel B: 2007–2016, W4 + CINC + ln(GDP per capita)
│   ├── df_model_2014.csv                               # DCID Panel C: 2007–2014, clean DV (kinetic-conflict-free)
│   ├── df_attacker_2020.csv                            # CFR monadic panel: 2007–2020, W4 + ln(GDP per capita)
│   ├── df_attacker_2016.csv                            # CFR monadic panel: 2007–2016, W4 + CINC + ln(GDP per capita)
│   ├── incidents_excluded.csv                          # Audit trail of incidents excluded from Panel C
│   ├── tables/                                         # HTML/docx tables from modelsummary
│   └── plots/                                          # Saved plot outputs from descriptive_statistics.R
│
├── data_preparation.R                                  # DCID pipeline: raw sources → df_model_*.csv
├── data_preparation_cfr.R                              # CFR pipeline: raw sources → df_attacker_*.csv
├── descriptive_statistics.R                            # Descriptive statistics, diagnostics, correlations, Poisson/NB LR test
├── models.R                                            # Primary models M1–M7 + diagnostics D1–D3 (DCID)
└── robustness.R                                        # Robustness checks R1–R6 (MID-any, top-3 exclusion, CFR) + diagnostics D4–D5
```