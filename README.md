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

### Model families

There are three families of models in this project: **M** (primary), **D**
(diagnostics) and **R** (robustness). Diagnostics probe a specification or
support an interpretation in the text and are kept in the code only — they
are not reported in the thesis. Robustness checks confront the finding with
external data, samples, or dataset and are reported in the thesis appendix.

- **M1–M7** — primary models on the DCID dyadic panels (`models.R`). See
  thesis Table 4 for specifications and Table 5 for results.

- **D1–D5** — diagnostics, not reported in the thesis.
  - **D1, D2** — H2 extensive-margin diagnostics: logit on `has_attack_clean`
    with CINC-only (D1) and GDP-only (D2) controls. Defined in `models.R`.
  - **D3** — M7 stage-placement diagnostic: ZINB with W4 added to the
    inflation stage as well as the count stage. Defined in `models.R`.
  - **D4, D5** — supplementary top-3-exclusion diagnostics: a ZINB version
    of R2 on DCID Panel C (D4) and the CFR analogue of R2 on both CFR
    panels (D5). Defined in `robustness.R`.

- **R1–R6** — robustness checks, reported in thesis Tables 6, 7 and 8.
  All defined in `robustness.R`.
  - **R1** — kinetic-conflict sensitivity: NB with MID at any hostility
    level (≥ 1) added to M2 (Panel B).
  - **R2** — exclusion of the three dominant attackers (China, Russia,
    Iran) from Panel C, NB estimator.
  - **R3–R6** — CFR alternative dataset (Council on Foreign Relations
    Cyber Operations Tracker). Monadic panel, so they test H1 only:
    R3 mirrors M1, R4 mirrors M2, R5 mirrors M3/M6, R6 mirrors M7 and is
    the primary CFR specification.

The Poisson-vs-NB likelihood-ratio test (estimator justification) lives in
`descriptive_statistics.R`.

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