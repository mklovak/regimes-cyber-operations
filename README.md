# Project Structure

```text
.
├── outputs/                                            # CSV and HTML outputs
│   ├── df_model2016.csv                                # Dataset with CINC controls (2007-2016)
│   └── df_model2020.csv                                # Dataset with GDP and Internet coverage controls(2008-2020)
├── data sources/                                       # Raw data sources
│   ├── COW-country-codes.csv                           # https://correlatesofwar.org/cow-country-codes - Correlates of War, Country Codes
│   ├── NMC-60-abridged.csv                             # https://correlatesofwar.org/data-sets/national-material-capabilities/ - Correlates of War, National Material Capabilities
│   ├── DCID_2.0_Release_update_February_2023.xlsx      # https://www.ryanmaness.com/cyber-conflict-dataset - Dyadic Cyber Incident and Campaign Data v.2.0
│   ├── Indicators_NK.xlsx                              # https://www.bok.or.kr/eng - Bank of Korea, yearly reports for North Korea
│   ├── E018101010_002034281.csv                        # https://nstatdb.dgbas.gov.tw/dgbasall/webMain.aspx?k=engmain - National Statistics, Taiwan
│   ├── Individuals using the Internet.csv              # https://datahub.itu.int/query/ - International Telecommunication Union, Individuals using the Internet at Taiwan
│   ├── a14efc5a-7fe8-4ea4-b8ed-57b7727e3394_Data.csv   # https://databank.worldbank.org/source/world-development-indicators - World Bank, GDP
│   ├── NewWmeasure.csv                                 # https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/UYLXRO - Winning Coalition Index
│   └── 3d38d8b0-c9d5-4ab9-a240-dee4c0547b26_Data.csv   # https://databank.worldbank.org/source/world-development-indicators - World Bank, Internet Penetration
├── data_preparation v3.R                               # Data preparation script (takes raw data and produses df_model2016.csv and df_model2020.csv)
├── descriptive_statistics.R                            # Descriptive statistics script
└── models v2.R                                         # Modeling script (v2 - Robustness checks)