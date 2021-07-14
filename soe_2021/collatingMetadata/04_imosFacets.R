library(data.table)
library(tidyverse)

# Reading and inspecting IMOS data
imos <- fread("imos_intersection_imcra.csv")

# Removing part of the string
imos$keyword <- gsub("EarthScience\\|", "", imos$keyword)

# Removing duplicates
setkey(imos,NULL)
imos <- unique(imos)

# Subsetting and renaming columns
imos <- imos %>% 
  dplyr::select(MESO_NAME, WATER_TYPE, identifier, keyword, surveyYear)
colnames(imos) <- c("imcra", "water_type", "identifier", "key", "year")

# Removing records outside 1901-2020 range
imos <- imos %>%
  dplyr::filter(year > 1900,
                year < 2021)

# Grouping years into two different types: i) records in between 1900 and 1980 and  5-year ranges afterwards (from 1981)
imos <- imos %>%
  dplyr::mutate(YearRange = cut(imos$year, breaks = c(1900, seq(1980, 2020, 5))))

levels(imos$YearRange) <- c("1900-1980" ,paste(
  seq(1980, 2015, 5) + 1,
  seq(1985, 2020, 5),
  sep = "-"))


# Removing blank cells
imos <- imos[!(is.na(imos$YearRange) | imos$YearRange == ""),]
head(imos)

# Occurrence records count
surveyByImcra <- imos %>%
  dplyr::select("imcra", "key", "YearRange")
surveyByImcra <- setDT(surveyByImcra)[, .(surveyByImcra = .N), keyby = c("imcra", "key", "YearRange")]

fwrite(surveyByImcra, "cache/data/surveyByImcra_imos.csv")
