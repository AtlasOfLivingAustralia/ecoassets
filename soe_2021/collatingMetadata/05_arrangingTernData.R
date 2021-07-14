library(data.table)
library(tidyverse)

# Reading and inspecting IMOS data
tern <- fread("tern_data_original.csv")

# Subsetting df
tern <- tern %>% 
  dplyr::select(type_name, lat, long, visit_date, features_of_interest)
colnames(tern) <- c("type", "lat", "lon", "date", "keyword")

#############
# Separating keywords
df1 <- data.frame(do.call("rbind", strsplit(as.character(tern$keyword), "|", fixed = TRUE)))
df2 <- cbind(tern, df1)

# Pivot longer
df2 <- df2 %>%
  tidyr::pivot_longer(
    cols = starts_with("x"),
    names_to = "X",
    names_prefix = "X",
    values_to = "key")

df <- df2 %>% 
  dplyr::select(type, lat, lon, date, key)

# Removing duplicates
setkey(df,NULL)
df <- unique(df)

###############
# Separating dates
df1 <- data.frame(do.call("rbind", strsplit(as.character(df$date), "|", fixed = TRUE)))
df2 <- cbind(df, df1)

# Pivot longer
df2 <- df2 %>%
  tidyr::pivot_longer(
    cols = starts_with("x"),
    names_to = "X",
    names_prefix = "X",
    values_to = "survey_date")

df <- df2 %>% 
  dplyr::select(type, lat, lon, date, key, survey_date)

# Removing duplicates
setkey(df,NULL)
tern_sep <- unique(df)

sep <- data.frame(sapply(strsplit(tern_sep$survey_date, split="\\/"), tail, 1L))
colnames(sep) <- c("year")
tern_sep <- cbind(tern_sep, sep)

sep <- data.frame(sapply(strsplit(tern_sep$year, split="\\-"), tail, 1L))
colnames(sep) <- c("survey_year")
tern_sep <- cbind(tern_sep, sep)

fwrite(tern_sep, "cache/data/tern_sep.csv")

