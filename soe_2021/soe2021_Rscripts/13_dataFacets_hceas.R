options(java.parameters = "-Xmx6g")

library(data.table)
library(dplyr)

# Merging data
input_folder <- "cache/merged/landUseZone/" # folder that contains all the csvs
data <- dir(input_folder, "^.*\\.csv$", full.names = TRUE) # create file names of all the csvs
ala <- plyr::ldply(data, data.table::fread)

colnames(ala)[9] <- c("CM_Zone")

# Subsetting data
ala <- ala %>%
  dplyr::select(speciesID, year, basisOfRecord,
                LandUseZon, CAPAD_Status, wons_status, griis_status,
                conservation_status, CM_Zone, indigenous_Status)

# Renaming some columns
colnames(ala) <- c("species_guid", "year", "basisOfRecord",
                   "landUseZone", "capad_status", "wons_status", "griis_status",
                   "conservation_status", "ConservManageZone", "indigenous_Status")

# Removing blank cells
ala <- ala[!(is.na(ala$species_guid) | ala$species_guid == ""),]

# Total records: 66,674,031

# Group years into 5-year ranges
ala <- ala %>%
  dplyr::mutate(YearRange = cut(ala$year, breaks = c(1900, seq(1980, 2020, 5))))

levels(ala$YearRange) <- c("1900-1980" ,paste(
  seq(1980, 2015, 5) + 1,
  seq(1985, 2020, 5),
  sep = "-"))


# Removing blank cells
ala <- ala[!(is.na(ala$YearRange) | ala$YearRange == ""),]

ala <- ala %>% 
  dplyr::select(species_guid, YearRange, landUseZone, capad_status, wons_status, griis_status, conservation_status)

################################
# Species count by landUseZone
sppBylandUseZone <- ala %>%
  dplyr::select("species_guid", "landUseZone", "YearRange")

# Removing duplicates
setkey(sppBylandUseZone,NULL)
sppBylandUseZone <- unique(sppBylandUseZone)

sppBylandUseZone <- setDT(sppBylandUseZone)[, .(sppBylandUseZone = .N), keyby = c("landUseZone", "YearRange")]
fwrite(sppBylandUseZone, "cache/sumTable/landUseZone/sppBylandUseZone.csv")
rm(sppBylandUseZone)


# Species inside/outside PA count
pa <- ala %>%
  dplyr::select(landUseZone, species_guid, YearRange, capad_status)

# Removing duplicates
setkey(pa,NULL)
pa <- unique(pa)

pa <- setDT(pa)[, .(sppInPa = .N), keyby = c("landUseZone", "YearRange", "capad_status")]

sppInPa <- pa %>%
  dplyr::filter(capad_status == "inside")

sppInPa <- sppInPa %>%
  dplyr::select(landUseZone, YearRange, sppInPa)

sppOutPa <- pa %>%
  dplyr::filter(capad_status == "outside")

sppOutPa <- sppOutPa %>%
  dplyr::select(landUseZone, YearRange, sppInPa)
colnames(sppOutPa)[3] <- "sppOutPa"


fwrite(sppInPa, "cache/sumTable/landUseZone/sppInPa.csv")
fwrite(sppOutPa, "cache/sumTable/landUseZone/sppOutPa.csv")


rm(pa, sppInPa, sppOutPa)

##########################################
# Number of introduced species by landUseZones
sppBylandUseZone <- ala %>%
  dplyr::select("landUseZone", "YearRange", "species_guid", "griis_status")

sppBylandUseZone <- sppBylandUseZone %>%
  dplyr::mutate(griis_status = ifelse(griis_status == "NULL", "introduced", "other"))

sppBylandUseZone <- sppBylandUseZone %>%
  dplyr::filter(griis_status == "introduced")

# Removing duplicates
setkey(sppBylandUseZone,NULL)
sppBylandUseZone <- unique(sppBylandUseZone)

sppBylandUseZone <- setDT(sppBylandUseZone)[, .(sppBylandUseZone_introduced = .N),
                                keyby = c("landUseZone", "YearRange", "griis_status")]

sppBylandUseZone <- sppBylandUseZone %>% 
  dplyr::select(landUseZone, YearRange, sppBylandUseZone_introduced)
fwrite(sppBylandUseZone, "cache/sumTable/landUseZone/sppBylandUseZone_introduced.csv")

rm(sppBylandUseZone)

# Number of introduced species inside/outside PA
griis <- ala %>%
  dplyr::select("landUseZone", "YearRange", "species_guid", "griis_status", "capad_status")

griis <- griis %>%
  dplyr::mutate(griis_status = ifelse(griis_status == "NULL", "introduced", "other"))

griis <- griis %>%
  dplyr::filter(griis_status == "introduced")

# Removing duplicates
setkey(griis,NULL)
griis <- unique(griis)

pa <- setDT(griis)[, .(count = .N), keyby = c("landUseZone", "YearRange", "capad_status", "griis_status")]

sppInPa <- pa %>%
  dplyr::filter(capad_status == "inside")

sppInPa <- sppInPa %>%
  dplyr::select(landUseZone, YearRange, count)

colnames(sppInPa)[3] <- c("sppInPa_introduced")

sppOutPa <- pa %>%
  dplyr::filter(capad_status == "outside")

sppOutPa <- sppOutPa %>%
  dplyr::select(landUseZone, YearRange, count)

colnames(sppOutPa)[3] <- c("sppOutPa_introduced")

fwrite(sppInPa, "cache/sumTable/landUseZone/sppInPa_introduced.csv")
fwrite(sppOutPa, "cache/sumTable/landUseZone/sppOutPa_introduced.csv")

rm(griis, pa, sppInPa, sppOutPa)

##############################################
# Number of invasive species by landUseZones
sppBylandUseZone <- ala %>%
  dplyr::select("landUseZone", "YearRange", "species_guid", "griis_status")

sppBylandUseZone <- sppBylandUseZone %>%
  dplyr::mutate(griis_status = ifelse(griis_status == "INVASIVE", "invasive", "other"))

sppBylandUseZone <- sppBylandUseZone %>%
  dplyr::filter(griis_status == "invasive")


# Removing duplicates
setkey(sppBylandUseZone,NULL)
sppBylandUseZone <- unique(sppBylandUseZone)


sppBylandUseZone <- setDT(sppBylandUseZone)[, .(sppBylandUseZone_invasive = .N),
                                keyby = c("landUseZone", "YearRange", "griis_status")]
sppBylandUseZone <- sppBylandUseZone %>% 
  dplyr::select(landUseZone, YearRange, sppBylandUseZone_invasive)
fwrite(sppBylandUseZone, "cache/sumTable/landUseZone/sppBylandUseZone_invasive.csv")

rm(sppBylandUseZone)


# Number of invasive species inside/outside PA
griis <- ala %>%
  dplyr::select("landUseZone", "YearRange", "species_guid", "griis_status", "capad_status")

griis <- griis %>%
  dplyr::mutate(griis_status = ifelse(griis_status == "INVASIVE", "invasive", "other"))

griis <- griis %>%
  dplyr::filter(griis_status == "invasive")

# Removing duplicates
setkey(griis,NULL)
griis <- unique(griis)

pa <- setDT(griis)[, .(count = .N), keyby = c("landUseZone", "YearRange", "capad_status", "griis_status")]

sppInPa <- pa %>%
  dplyr::filter(capad_status == "inside")

sppInPa <- sppInPa %>%
  dplyr::select(landUseZone, YearRange, count)

colnames(sppInPa)[3] <- c("sppInPa_invasive")

sppOutPa <- pa %>%
  dplyr::filter(capad_status == "outside")

sppOutPa <- sppOutPa %>%
  dplyr::select(landUseZone, YearRange, count)

colnames(sppOutPa)[3] <- c("sppOutPa_invasive")

fwrite(sppInPa, "cache/sumTable/landUseZone/sppInPa_invasive.csv")
fwrite(sppOutPa, "cache/sumTable/landUseZone/sppOutPa_invasive.csv")

rm(griis, pa, sppInPa, sppOutPa)

############################
# Number of wons species by landUseZones
sppBylandUseZone <- ala %>%
  dplyr::select("landUseZone", "YearRange", "species_guid", "wons_status")

sppBylandUseZone <- sppBylandUseZone %>%
  dplyr::filter(wons_status == "WoNS")

# Removing duplicates
setkey(sppBylandUseZone,NULL)
sppBylandUseZone <- unique(sppBylandUseZone)


sppBylandUseZone <- setDT(sppBylandUseZone)[, .(sppBylandUseZone_wons = .N),
                                keyby = c("landUseZone", "YearRange", "wons_status")]
sppBylandUseZone <- sppBylandUseZone %>% 
  dplyr::select(landUseZone, YearRange, sppBylandUseZone_wons)

fwrite(sppBylandUseZone, "cache/sumTable/landUseZone/sppBylandUseZone_wons.csv")

rm(sppBylandUseZone)

# Number of WoNS species inside/outside PA
wons <- ala %>%
  dplyr::select("landUseZone", "YearRange", "species_guid", "wons_status", "capad_status")

wons <- wons %>%
  dplyr::filter(wons_status == "WoNS")

# Removing duplicates
setkey(wons,NULL)
wons <- unique(wons)

pa <- setDT(wons)[, .(count = .N), keyby = c("landUseZone", "YearRange", 
                                             "capad_status", "wons_status")]

sppInPa <- pa %>%
  dplyr::filter(capad_status == "inside")

sppInPa <- sppInPa %>%
  dplyr::select(landUseZone, YearRange, count)

colnames(sppInPa)[3] <- c("sppInPa_WoNS")

sppOutPa <- pa %>%
  dplyr::filter(capad_status == "outside")

sppOutPa <- sppOutPa %>%
  dplyr::select(landUseZone, YearRange, count)

colnames(sppOutPa)[3] <- c("sppOutPa_WoNS")


fwrite(sppInPa, "cache/sumTable/landUseZone/sppInPa_WoNS.csv")
fwrite(sppOutPa, "cache/sumTable/landUseZone/sppOutPa_WoNS.csv")

rm(wons, pa, sppInPa, sppOutPa)

####################################
ala <- ala %>%
  dplyr::mutate(epbc_status = ifelse(conservation_status == "Non-threatened", 
                                     "Non-threatened", "epbc"))

# Number of epbc species by landUseZones
sppBylandUseZone <- ala %>%
  dplyr::select("landUseZone", "YearRange", "species_guid", "epbc_status")

sppBylandUseZone <- sppBylandUseZone %>%
  dplyr::filter(epbc_status == "epbc")

# Removing duplicates
setkey(sppBylandUseZone,NULL)
sppBylandUseZone <- unique(sppBylandUseZone)


sppBylandUseZone <- setDT(sppBylandUseZone)[, .(sppBylandUseZone_epbc = .N),
                                keyby = c("landUseZone", "YearRange", "epbc_status")]
sppBylandUseZone <- sppBylandUseZone %>% 
  dplyr::select(landUseZone, YearRange, sppBylandUseZone_epbc)

fwrite(sppBylandUseZone, "cache/sumTable/landUseZone/sppBylandUseZone_epbc.csv")

rm(sppBylandUseZone)

# Number of epbc species inside/outside PA
epbc <- ala %>%
  dplyr::select("landUseZone", "YearRange", "species_guid", 
                "epbc_status", "capad_status")

epbc <- epbc %>%
  dplyr::filter(epbc_status == "epbc")

# Removing duplicates
setkey(epbc,NULL)
epbc <- unique(epbc)

pa <- setDT(epbc)[, .(count = .N), keyby = c("landUseZone", "YearRange", "capad_status", "epbc_status")]

sppInPa <- pa %>%
  dplyr::filter(capad_status == "inside")

sppInPa <- sppInPa %>%
  dplyr::select(landUseZone, YearRange, count)

colnames(sppInPa)[3] <- c("sppInPa_epbc")

sppOutPa <- pa %>%
  dplyr::filter(capad_status == "outside")

sppOutPa <- sppOutPa %>%
  dplyr::select(landUseZone, YearRange, count)

colnames(sppOutPa)[3] <- c("sppOutPa_epbc")

fwrite(sppInPa, "cache/sumTable/landUseZone/sppInPa_epbc.csv")
fwrite(sppOutPa, "cache/sumTable/landUseZone/sppOutPa_epbc.csv")

rm(epbc, pa, sppInPa, sppOutPa)

################################################
# Merging datafiles
list <- list.files(path = "cache/sumTable/landUseZone/", pattern = ".csv", full.names = TRUE)

df1 <- fread(list[[1]])
df2 <- fread(list[[2]])
df3 <- fread(list[[3]])
df4 <- fread(list[[4]])
df5 <- fread(list[[5]])
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
  dplyr::left_join(df2, by = c("landUseZone", "YearRange"))
df <- df %>%
  dplyr::left_join(df3, by = c("landUseZone", "YearRange"))
df <- df %>%
  dplyr::left_join(df4, by = c("landUseZone", "YearRange"))
df <- df %>%
  dplyr::left_join(df5, by = c("landUseZone", "YearRange"))
df <- df %>%
  dplyr::left_join(df6, by = c("landUseZone", "YearRange"))
df <- df %>%
  dplyr::left_join(df7, by = c("landUseZone", "YearRange"))
df <- df %>%
  dplyr::left_join(df8, by = c("landUseZone", "YearRange"))
df <- df %>%
  dplyr::left_join(df9, by = c("landUseZone", "YearRange"))
df <- df %>%
  dplyr::left_join(df10, by = c("landUseZone", "YearRange"))
df <- df %>%
  dplyr::left_join(df11, by = c("landUseZone", "YearRange"))
df <- df %>%
  dplyr::left_join(df12, by = c("landUseZone", "YearRange"))
df <- df %>%
  dplyr::left_join(df13, by = c("landUseZone", "YearRange"))
df <- df %>%
  dplyr::left_join(df14, by = c("landUseZone", "YearRange"))
df <- df %>%
  dplyr::left_join(df15, by = c("landUseZone", "YearRange"))

fwrite(df, "cache/sumTable/landUseZone_merged.csv")
