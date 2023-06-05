# run script for EcoAssets

# 1. load libraries
source("scripts/load_libraries.R")

# 2. get records from biocache using {galah}
source("scripts/get_records.R")

# 3. monitoring datasets and plots 
source("scripts/assemble_monitoring.R")
source("scripts/plot_monitoring.R")

# 4. biodiversity datasets and plots
source("scripts/create_relational_tables.R")

# TODO
source("scripts/facet_biodiversity.R")

# TODO
source("scripts/plot_biodiversity.R")
