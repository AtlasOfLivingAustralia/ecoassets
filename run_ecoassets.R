# run script for EcoAssets

# 1. load libraries
source("scripts/load_libraries.R")

# 2. get records from biocache using {galah}
source("scripts/get_records.R")

# 3. generate monitoring datasets 
source("scripts/build_monitoring.R")

# 4. create plots from monitoring datasets
source("scripts/plot_monitoring.R")

# 5. create three relational tables to generate biodiversity datasets
source("scripts/create_relational_tables.R")

# TODO:
# 6. generate aggregated biodiversity summaries from relational tables
source("scripts/facet_biodiversity.R")

# 7. create plots from biodiversity datasets
source("scripts/plot_biodiversity.R")
