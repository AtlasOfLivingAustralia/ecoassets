# Maximising memory
options(java.parameters = "-Xmx6g")

# Loading required libraries
library(data.table)
library(dplyr)
library(galah)

# Downloading records from the ALA
# Configuring account details
ala_config(email = "s.chowdhury@uqconnect.edu.au",
           caching = TRUE,
           cache_directory = "./cache",
           download_reason_id = 10,
           verbose = TRUE)

# galah can download a maximum of 50 million records in each iteration, so we are downloading occurrence records by states
states <- sort(find_field_values("cl22")$category[1:8])
state_df <- data.frame(
  acronym = c("ACT", "NSW", "NT", "QLD", "SA", "TAS", "VIC", "WA"),
  full_name = states)

# Downloading data
lapply(
  split(state_df, seq_len(nrow(state_df)))[1:8],
  function(a){
    # ala_counts(filters = select_filters(cl22 = a$full_name))
    result <- ala_occurrences(
      filters = select_filters(
        year = c(1980:2021),
        cl22 = a$full_name),
      columns = select_columns(
        "species_guid", # species ID
        "year",
        "kingdom",
        "phylum",
        "class",
        "order",
        "family",
        "genus",
        "decimalLongitude",
        "decimalLatitude",
        "species",
        "basisOfRecord",
        "cl22", # states
        "cl1048", # IBRA
        "cl10944" # CAPAD (national park names)
      )
    )
    saveRDS(result,
            paste0("./cache/", a$acronym, ".rds"))
  }
)




# QLD, NSW and VIC contains over 40 million spatial records. To further accelerate the intersection, we are gouping each state into three separate dataframes
NSW <- fread("NSW.csv")
NSW1 <- NSW[1:5000000,]
NSW2 <- NSW[5000001:10000000,]
NSW3 <- NSW[10000001:14104689,]

fwrite(NSW1, "NSW1.csv")
fwrite(NSW2, "NSW2.csv")
fwrite(NSW3, "NSW3.csv")
rm(NSW, NSW1, NSW2, NSW3)

VIC <- fread("VIC.csv")
VIC1 <- VIC[1:5000000,]
VIC2 <- VIC[5000001:10000000,]
VIC3 <- VIC[10000001:13900171,]

fwrite(VIC1, "VIC1.csv")
fwrite(VIC2, "VIC2.csv")
fwrite(VIC3, "VIC3.csv")
rm(VIC, VIC1, VIC2, VIC3)

QLD <- fread("QLD.csv")
QLD1 <- QLD[1:5000000,]
QLD2 <- QLD[5000001:10000000,]
QLD3 <- QLD[10000001:10326744,]

fwrite(QLD1, "QLD1.csv")
fwrite(QLD2, "QLD2.csv")
fwrite(QLD3, "QLD3.csv")
rm(QLD, QLD1, QLD2, QLD3)