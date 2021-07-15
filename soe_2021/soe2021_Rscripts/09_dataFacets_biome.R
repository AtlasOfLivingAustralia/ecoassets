options(java.parameters = "-Xmx6g")

library(data.table)
library(dplyr)

# Merging data
input_folder <- "cache/Merged/Biome/" # folder that contains all the csvs
data <- dir(input_folder, "^.*\\.csv$", full.names = TRUE) # create file names of all the csvs
ala <- plyr::ldply(data, data.table::fread)

colnames(ala)[18] <- c("CM_Zone")

# Subsetting data
ala <- ala %>%
  dplyr::select(species_guid, year, basisOfRecord,
                B_NAME, CAPAD_Status, wons_status, griis_status,
                conservation_status, CM_Zone, indigenous_Status)

# Renaming some columns
colnames(ala) <- c("species_guid", "year", "basisOfRecord",
                   "biome", "capad_status", "wons_status", "griis_status",
                   "conservation_status", "ConservManageZone", "indigenous_Status")

# Removing blank cells
ala <- ala[!(is.na(ala$species_guid) | ala$species_guid == ""),]
ala <- ala[!(is.na(ala$biome) | ala$biome == ""),]


# Group years into 5-year ranges
ala <- ala %>%
  dplyr::mutate(YearRange = cut(ala$year, breaks = c(1900, seq(1980, 2020, 5))))

levels(ala$YearRange) <- c("1900-1980" ,paste(
  seq(1980, 2015, 5) + 1,
  seq(1985, 2020, 5),
  sep = "-"))


# Removing blank cells
ala <- ala[!(is.na(ala$YearRange) | ala$YearRange == ""),]

################################
# Species count by biome
sppByBiome <- ala %>%
  dplyr::select("species_guid", "biome", "YearRange")

# Removing duplicates
setkey(sppByBiome,NULL)
sppByBiome <- unique(sppByBiome)

sppByBiome <- setDT(sppByBiome)[, .(sppByBiome = .N), keyby = c("biome", "YearRange")]
fwrite(sppByBiome, "SumTable/sppByBiome.csv")
rm(sppByBiome)


# Species inside/outside PA count
pa <- ala %>%
  dplyr::select(biome, species_guid, YearRange, capad_status)

# Removing duplicates
setkey(pa,NULL)
pa <- unique(pa)

pa <- setDT(pa)[, .(sppInPa = .N), keyby = c("biome", "YearRange", "capad_status")]

sppInPa <- pa %>%
  dplyr::filter(capad_status == "inside")

sppInPa <- sppInPa %>%
  dplyr::select(biome, YearRange, sppInPa)

sppOutPa <- pa %>%
  dplyr::filter(capad_status == "outside")

sppOutPa <- sppOutPa %>%
  dplyr::select(biome, YearRange, sppInPa)


fwrite(sppInPa, "SumTable/sppInPa.csv")
fwrite(sppOutPa, "SumTable/sppOutPa.csv")


rm(pa, sppInPa, sppOutPa)

##########################################
# Number of introduced species by biomes
sppByBiome <- ala %>%
  dplyr::select("biome", "YearRange", "species_guid", "griis_status")

sppByBiome <- sppByBiome %>%
  dplyr::mutate(griis_status = ifelse(griis_status == "NULL", "introduced", "other"))

sppByBiome <- sppByBiome %>%
  dplyr::filter(griis_status == "introduced")

# Removing duplicates
setkey(sppByBiome,NULL)
sppByBiome <- unique(sppByBiome)

sppByBiome <- setDT(sppByBiome)[, .(sppByBiome_introduced = .N),
                               keyby = c("biome", "YearRange", "griis_status")]

fwrite(sppByBiome, "SumTable/sppByBiome_introduced.csv")

rm(sppByBiome)

# Number of introduced species inside/outside PA
griis <- ala %>%
  dplyr::select("biome", "YearRange", "species_guid", "griis_status", "capad_status")

griis <- griis %>%
  dplyr::mutate(griis_status = ifelse(griis_status == "NULL", "introduced", "other"))

griis <- griis %>%
  dplyr::filter(griis_status == "introduced")

# Removing duplicates
setkey(griis,NULL)
griis <- unique(griis)

pa <- setDT(griis)[, .(count = .N), keyby = c("biome", "YearRange", "capad_status", "griis_status")]

sppInPa <- pa %>%
  dplyr::filter(capad_status == "inside")

sppInPa <- sppInPa %>%
  dplyr::select(biome, YearRange, count)

colnames(sppInPa)[3] <- c("sppInPa_introduced")

sppOutPa <- pa %>%
  dplyr::filter(capad_status == "outside")

sppOutPa <- sppOutPa %>%
  dplyr::select(biome, YearRange, count)

colnames(sppOutPa)[3] <- c("sppOutPa_introduced")

fwrite(sppInPa, "SumTable/sppInPa_introduced.csv")
fwrite(sppOutPa, "SumTable/sppOutPa_introduced.csv")

rm(griis, pa, sppInPa, sppOutPa)

##############################################
# Number of invasive species by biomes
sppByBiome <- ala %>%
  dplyr::select("biome", "YearRange", "species_guid", "griis_status")

sppByBiome <- sppByBiome %>%
  dplyr::mutate(griis_status = ifelse(griis_status == "INVASIVE", "invasive", "other"))

sppByBiome <- sppByBiome %>%
  dplyr::filter(griis_status == "invasive")


# Removing duplicates
setkey(sppByBiome,NULL)
sppByBiome <- unique(sppByBiome)


sppByBiome <- setDT(sppByBiome)[, .(sppByBiome_invasive = .N),
                               keyby = c("biome", "YearRange", "griis_status")]

fwrite(sppByBiome, "SumTable/sppByBiome_invasive.csv")

rm(sppByBiome)


# Number of invasive species inside/outside PA
griis <- ala %>%
  dplyr::select("biome", "YearRange", "species_guid", "griis_status", "capad_status")

griis <- griis %>%
  dplyr::mutate(griis_status = ifelse(griis_status == "INVASIVE", "invasive", "other"))

griis <- griis %>%
  dplyr::filter(griis_status == "invasive")

# Removing duplicates
setkey(griis,NULL)
griis <- unique(griis)

pa <- setDT(griis)[, .(count = .N), keyby = c("biome", "YearRange", "capad_status", "griis_status")]

sppInPa <- pa %>%
  dplyr::filter(capad_status == "inside")

sppInPa <- sppInPa %>%
  dplyr::select(biome, YearRange, count)

colnames(sppInPa)[3] <- c("sppInPa_invasive")

sppOutPa <- pa %>%
  dplyr::filter(capad_status == "outside")

sppOutPa <- sppOutPa %>%
  dplyr::select(biome, YearRange, count)

colnames(sppOutPa)[3] <- c("sppOutPa_invasive")

fwrite(sppInPa, "SumTable/sppInPa_invasive.csv")
fwrite(sppOutPa, "SumTable/sppOutPa_invasive.csv")

rm(griis, pa, sppInPa, sppOutPa)

############################
# Number of wons species by biomes
sppByBiome <- ala %>%
  dplyr::select("biome", "YearRange", "species_guid", "wons_status")

sppByBiome <- sppByBiome %>%
  dplyr::filter(wons_status == "WoNS")

# Removing duplicates
setkey(sppByBiome,NULL)
sppByBiome <- unique(sppByBiome)


sppByBiome <- setDT(sppByBiome)[, .(sppByBiome_wons = .N),
                               keyby = c("biome", "YearRange", "wons_status")]

fwrite(sppByBiome, "SumTable/sppByBiome_wons.csv")

rm(sppByBiome)

# Number of WoNS species inside/outside PA
wons <- ala %>%
  dplyr::select("biome", "YearRange", "species_guid", "wons_status", "capad_status")

wons <- wons %>%
  dplyr::filter(wons_status == "WoNS")

# Removing duplicates
setkey(wons,NULL)
wons <- unique(wons)

pa <- setDT(wons)[, .(count = .N), keyby = c("biome", "YearRange", 
                                             "capad_status", "wons_status")]

sppInPa <- pa %>%
  dplyr::filter(capad_status == "inside")

sppInPa <- sppInPa %>%
  dplyr::select(biome, YearRange, count)

colnames(sppInPa)[3] <- c("sppInPa_WoNS")

sppOutPa <- pa %>%
  dplyr::filter(capad_status == "outside")

sppOutPa <- sppOutPa %>%
  dplyr::select(biome, YearRange, count)

colnames(sppOutPa)[3] <- c("sppOutPa_WoNS")


fwrite(sppInPa, "SumTable/sppInPa_WoNS.csv")
fwrite(sppOutPa, "SumTable/sppOutPa_WoNS.csv")

rm(wons, pa, sppInPa, sppOutPa)

####################################
ala <- ala %>%
  dplyr::mutate(epbc_status = ifelse(conservation_status == "Non-threatened", 
                                     "Non-threatened", "epbc"))

# Number of epbc species by biomes
sppByBiome <- ala %>%
  dplyr::select("biome", "YearRange", "species_guid", "epbc_status")

sppByBiome <- sppByBiome %>%
  dplyr::filter(epbc_status == "epbc")

# Removing duplicates
setkey(sppByBiome,NULL)
sppByBiome <- unique(sppByBiome)


sppByBiome <- setDT(sppByBiome)[, .(sppByBiome_epbc = .N),
                               keyby = c("biome", "YearRange", "epbc_status")]

fwrite(sppByBiome, "SumTable/sppByBiome_epbc.csv")

rm(sppByIbra, sppByIbra1)

# Number of epbc species inside/outside PA
epbc <- ala %>%
  dplyr::select("biome", "YearRange", "species_guid", 
                "epbc_status", "capad_status")

epbc <- epbc %>%
  dplyr::filter(epbc_status == "epbc")

# Removing duplicates
setkey(epbc,NULL)
epbc <- unique(epbc)

pa <- setDT(epbc)[, .(count = .N), keyby = c("biome", "YearRange", "capad_status", "epbc_status")]

sppInPa <- pa %>%
  dplyr::filter(capad_status == "inside")

sppInPa <- sppInPa %>%
  dplyr::select(biome, YearRange, count)

colnames(sppInPa)[3] <- c("sppInPa_epbc")

sppOutPa <- pa %>%
  dplyr::filter(capad_status == "outside")

sppOutPa <- sppOutPa %>%
  dplyr::select(biome, YearRange, count)

colnames(sppOutPa)[3] <- c("sppOutPa_epbc")

fwrite(sppInPa, "SumTable/sppInPa_epbc.csv")
fwrite(sppOutPa, "SumTable/sppOutPa_epbc.csv")

rm(epbc, pa, sppInPa, sppOutPa)

################################################
# Merging datafiles
list <- list.files(path = "SumTable/Biome", pattern = ".csv", full.names = TRUE)

df1 <- fread(list[[1]])
df2 <- fread(list[[2]])
df2 <- df2 %>%
  dplyr::select(biome, YearRange, sppByBiome_epbc)
df3 <- fread(list[[3]])
df3 <- df3 %>%
  dplyr::select(biome, YearRange, sppByBiome_introduced)
df4 <- fread(list[[4]])
df4 <- df4 %>%
  dplyr::select(biome, YearRange, sppByBiome_invasive)
df5 <- fread(list[[5]])
df5 <- df5 %>%
  dplyr::select(biome, YearRange, sppByBiome_wons)
df6 <- fread(list[[6]])
df7 <- fread(list[[7]])
df8 <- fread(list[[8]])
df9 <- fread(list[[9]])
df10 <- fread(list[[10]])
df11 <- fread(list[[11]])
df12 <- fread(list[[12]])
df13 <- fread(list[[13]])
df14 <- fread(list[[14]])
df15 <- fread(list[[15]])

df <- df1 %>%
  dplyr::left_join(df2, by = c("biome", "YearRange"))
df <- df %>%
  dplyr::left_join(df3, by = c("biome", "YearRange"))
df <- df %>%
  dplyr::left_join(df4, by = c("biome", "YearRange"))
df <- df %>%
  dplyr::left_join(df5, by = c("biome", "YearRange"))
df <- df %>%
  dplyr::left_join(df6, by = c("biome", "YearRange"))
df <- df %>%
  dplyr::left_join(df7, by = c("biome", "YearRange"))
df <- df %>%
  dplyr::left_join(df8, by = c("biome", "YearRange"))
df <- df %>%
  dplyr::left_join(df9, by = c("biome", "YearRange"))
df <- df %>%
  dplyr::left_join(df10, by = c("biome", "YearRange"))
df <- df %>%
  dplyr::left_join(df11, by = c("biome", "YearRange"))
df <- df %>%
  dplyr::left_join(df12, by = c("biome", "YearRange"))
df <- df %>%
  dplyr::left_join(df13, by = c("biome", "YearRange"))
df <- df %>%
  dplyr::left_join(df14, by = c("biome", "YearRange"))
df <- df %>%
  dplyr::left_join(df15, by = c("biome", "YearRange"))

