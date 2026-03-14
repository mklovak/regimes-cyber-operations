# Project Structure

```text
.
├── outputs/                                            # CSV and HTML outputs
│   ├── df_cyber_inc_clean.csv                          # Cleaned cyber incident data
│   ├── df_model.csv                                    # Main dataset for modeling
│   ├── df_model2016.csv                                # Dataset for modeling (up to 2016)
│   └── df_model2020.csv                                # Dataset for modeling (up to 2020)
├── data sources/                                       # Raw data sources
│   ├── COW-country-codes.csv                           # https://correlatesofwar.org/cow-country-codes - Correlates of War, Country Codes
│   ├── NMC-60-abridged.csv                             # https://correlatesofwar.org/data-sets/national-material-capabilities/ - Correlates of War, National Material Capabilities
│   ├── DCID_2.0_Release_update_February_2023.xlsx      # https://www.ryanmaness.com/cyber-conflict-dataset - Dyadic Cyber Incident and Campaign Data v.2.0
│   ├── Indicators_NK.xlsx                              # https://www.bok.or.kr/eng - Bank of Korea, yearly reports for North Korea
│   ├── E018101010_002034281.csv                        # https://nstatdb.dgbas.gov.tw/dgbasall/webMain.aspx?k=engmain - National Statistics, Taiwan
│   ├── a14efc5a-7fe8-4ea4-b8ed-57b7727e3394_Data.csv   # https://databank.worldbank.org/source/world-development-indicators - World Bank, GDP
│   ├── NewWmeasure.csv                                 # https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/UYLXRO - Winning Coalition Index
│   └── 3d38d8b0-c9d5-4ab9-a240-dee4c0547b26_Data.csv   # https://databank.worldbank.org/source/world-development-indicators - World Bank, Internet Penetration
├── data_preparation v2.R                               # Data preparation script (version 2)
├── descriptive_statistics.R                            # Descriptive statistics script
└── models v2.R                                         # Modeling script (v2 - Robustness checks)