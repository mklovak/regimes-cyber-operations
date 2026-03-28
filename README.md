# Winning Coalition Size and State-Level Cyber Aggression

Master's thesis analyzing the relationship between political regimes (measured by the Winning Coalition Index, W4) and state-level cyber aggression, using selectorate theory as the theoretical framework.

## Data Pipeline (run in the following order)
data_preparation.R -> descriptive_statistics.R -> models.R -> robustness.R



## Project Structure

```text
.
├── data sources/                                       # Raw data sources
│   ├── COW-country-codes.csv                           # Correlates of War, Country Codes
│   ├── NMC-60-abridged.csv                             # Correlates of War, National Material Capabilities (CINC)
│   ├── DCID_2.0_Release_update_February_2023.xlsx      # Dyadic Cyber Incident and Campaign Data v.2.0
│   ├── NewWmeasure.csv                                 # Winning Coalition Index (W4)
│   ├── a14efc5a-..._Data.csv                           # World Bank, GDP per capita (constant 2015 USD)
│   ├── Indicators_NK.xlsx                              # Bank of Korea, North Korea economic indicators
│   ├── E018101010_002034281.csv                        # National Statistics, Taiwan (GDP)
│   ├── Individuals using the Internet.csv              # ITU, Internet Penetration (Taiwan)
│   └── 3d38d8b0-..._Data.csv                           # World Bank, Internet Penetration (unused — collinear with GDP per capita, r=0.85)
│
├── outputs/                                            # Generated outputs
│   ├── df_model_2020.csv                               # Model-ready dataset (2007-2020): W4 + ln(GDP per capita)
│   ├── df_model_2016.csv                               # Model-ready dataset (2007-2016): W4 + CINC
│   └── plots/                                          # Saved plot outputs from descriptive_statistics_v3.R
│
├── data_preparation_v4.R                               # Data pipeline: raw sources → model-ready datasets
├── descriptive_statistics_v3.R                         # Descriptive statistics, diagnostics, and correlation analysis
└── models_v2_1.R                                       # Modeling script (to be updated)
```

## Datasets

Both datasets are directed dyad-year panels (attacker → victim, per year) built from all 174 countries with W4 data. Countries not involved in any DCID incident contribute informative zero observations.

| Dataset | Period | Observations | Incidents | Control Variable |
|---------|--------|-------------|-----------|-----------------|
| `df_model_2020.csv` | 2007–2020 | ~392k | 205 | ln(GDP per capita) |
| `df_model_2016.csv` | 2007–2016 | ~299k | 136 | CINC (raw) |

**Why two datasets with different controls?**
- **GDP per capita** proxies for economic development — the public goods channel in selectorate theory (W4 → public goods → higher GDP/capita). Log-transformed because it spans orders of magnitude.
- **CINC** (Composite Index of National Capability) captures overall national material capability (military + industrial + demographic), based on six NMC indicators. Uses raw values (bounded 0–1, no log transformation needed).
- The two controls are nearly orthogonal (r = 0.12), providing genuinely independent robustness checks.
- Internet Penetration was dropped due to collinearity with GDP per capita (r = 0.85). Theoretically, internet access is itself a public good: W4 → GDP/capita → internet penetration.

## Data Sources

| Source | Filename | URL |
|--------|----------|-----|
| DCID 2.0 | `DCID_2.0_Release_update_February_2023.xlsx` | https://www.ryanmaness.com/cyber-conflict-dataset |
| Winning Coalition (W4) | `NewWmeasure.csv` | https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/UYLXRO |
| COW Country Codes | `COW-country-codes.csv` | https://correlatesofwar.org/cow-country-codes |
| NMC / CINC | `NMC-60-abridged.csv` | https://correlatesofwar.org/data-sets/national-material-capabilities/ |
| World Bank GDP per capita | `a14efc5a-..._Data.csv` | https://databank.worldbank.org/source/world-development-indicators |
| World Bank Internet Penetration | `3d38d8b0-..._Data.csv` | https://databank.worldbank.org/source/world-development-indicators |
| Bank of Korea (NK) | `Indicators_NK.xlsx` | https://www.bok.or.kr/eng |
| Taiwan National Statistics (GDP) | `E018101010_002034281.csv` | https://nstatdb.dgbas.gov.tw/dgbasall/webMain.aspx?k=engmain |
| ITU Internet (Taiwan) | `Individuals using the Internet.csv` | https://datahub.itu.int/query/ |