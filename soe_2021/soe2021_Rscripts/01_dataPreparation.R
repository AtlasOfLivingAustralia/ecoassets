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
states <- sort(find_field_values("cl22")$category[1:11])
state_df <- data.frame(
  acronym = c("Ashmore&CartierIslands", "ACT", "CoralSeaIslands", "NSW", 
              "NT", "QLD", "SA", "TAS", "Unknown1", "VIC", "WA"),
  full_name = states)

# Downloading data
lapply(
  split(state_df, seq_len(nrow(state_df)))[1:11],
  function(a){
    # ala_counts(filters = select_filters(cl22 = a$full_name))
    result <- ala_occurrences(
      filters = select_filters(
        #year = c(1980:2021),
        cl22 = a$full_name),
      columns = select_columns(
        "species_guid", # species ID
        "eventDate",
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
        "cl21", # IMCRA
        "cl10947" # NRM zones
      )
    )
    saveRDS(result,
            paste0("./cache/", a$acronym, ".rds"))
  }
)



# Merging data
input_folder <- "cache/RawData/" # folder that contains all the csvs
data <- dir(input_folder, "^.*\\.csv$", full.names = TRUE) # create file names of all the csvs
ala_data <- plyr::ldply(data, data.table::fread)

# Renaming columns
colnames(ala_data)[14] <- c("States&Territories")
colnames(ala_data)[15] <- c("IBRA")
colnames(ala_data)[20] <- c("IMCRA")
colnames(ala_data)[17] <- c("NRM")

# Removing records outside 1901-2020 range
ala_data <- ala_data %>%
  dplyr::filter(year > 1900,
                year < 2021)

# Grouping into smaller dataframes
ala1 <- ala_data[1:5000000,]
fwrite(ala1, "cache/SmallerChunks/df1.csv")
rm(ala1)

ala2 <- ala_data[5000001:10000000,]
fwrite(ala2, "cache/SmallerChunks/df2.csv")
rm(ala2)

ala3 <- ala_data[10000001:15000000,]
fwrite(ala3, "cache/SmallerChunks/df3.csv")
rm(ala3)

ala4 <- ala_data[15000001:20000000,]
fwrite(ala4, "cache/SmallerChunks/df4.csv")
rm(ala4)

ala5 <- ala_data[20000001:25000000,]
fwrite(ala5, "cache/SmallerChunks/df5.csv")
rm(ala5)

ala6 <- ala_data[25000001:30000000,]
fwrite(ala6, "cache/SmallerChunks/df6.csv")
rm(ala6)

ala7 <- ala_data[30000001:35000000,]
fwrite(ala7, "cache/SmallerChunks/df7.csv")
rm(ala7)

ala8 <- ala_data[35000001:40000000,]
fwrite(ala8, "cache/SmallerChunks/df8.csv")
rm(ala8)

ala9 <- ala_data[40000001:45000000,]
fwrite(ala9, "cache/SmallerChunks/df9.csv")
rm(ala9)

ala10 <- ala_data[45000001:50000000,]
fwrite(ala10, "cache/SmallerChunks/df10.csv")
rm(ala10)

ala11 <- ala_data[50000001:55000000,]
fwrite(ala11, "cache/SmallerChunks/df11.csv")
rm(ala11)

ala12 <- ala_data[55000001:60033028,]
fwrite(ala12, "cache/SmallerChunks/df12.csv")
rm(ala12)
