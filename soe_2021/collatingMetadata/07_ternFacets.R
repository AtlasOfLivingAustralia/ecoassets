library(data.table)
library(tidyverse)

# Reading and inspecting IMOS data
tern <- fread("cache/data/tern_intersection_ibra.csv")

# Subsetting and renaming columns
tern <- tern %>% 
  dplyr::select(type, REG_NAME_7, key, survey_year)
colnames(tern) <- c("type", "ibra", "keyword", "year")

# Removing records outside 1901-2020 range
tern <- tern %>%
  dplyr::filter(year > 1900,
                year < 2021)

# Grouping years into two different types: i) records in between 1900 and 1980 and  5-year ranges afterwards (from 1981)
tern <- tern %>%
  dplyr::mutate(YearRange = cut(tern$year, breaks = c(1900, seq(1980, 2020, 5))))

levels(tern$YearRange) <- c("1900-1980" ,paste(
  seq(1980, 2015, 5) + 1,
  seq(1985, 2020, 5),
  sep = "-"))


# Removing blank cells
tern <- tern[!(is.na(tern$YearRange) | tern$YearRange == ""),]

# Occurrence records count
surveyByIbra <- tern %>%
  dplyr::select("ibra", "keyword", "YearRange")
surveyByIbra <- setDT(surveyByIbra)[, .(surveyByIbra = .N), keyby = c("ibra", "keyword", "YearRange")]

fwrite(surveyByIbra, "cache/data/surveyByIbra_tern.csv")