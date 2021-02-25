Replication material for Bicalho, Clara, Melina R. Platas, and Leah R. Rosenzweig. (2021) "“If we move, it moves with us:” Physical Distancing in Africa during COVID-19." _World Development_.

- `/data` contains the raw survey and GIS data
- `/0_helpers.R` includes helper functions used throughout the Rmd scripts
- `/1_clean_data.Rmd` cleans that raw data and uploads to `/data/clean`.
  - This file is run automatically when option is set `clean <- TRUE` in `_Replication.Rmd`
- `_Replication.Rmd` replicates all tables and figures in the main manuscript and outputs to `/tables` and `/figures`
- `_Replication_SI.Rmd` replicates all tables and figures in the supplementary information and outputs to `/tables` and `/figures`
