library(data.table)
library(tidyverse)

df <- fread("cache/data/imos_original.csv")

# Arranging multiple keywords
# Separating keywords
colnames(df)
df1 <- data.frame(do.call("rbind", strsplit(as.character(df$keywords), ";", fixed = TRUE)))
df2 <- cbind(df, df1)

# Pivot longer
df3 <- df2 %>%
  tidyr::pivot_longer(
    cols = starts_with("x"),
    names_to = "X",
    names_prefix = "X",
    values_to = "keyword")

# Removing unnecessary columns
df3 <- df3 %>% 
  dplyr::select(!c(X, keywords))

# Splitting  year-month-day
colnames(df3)
df4 <- df3 %>% 
  separate(beginPosition, c("beginYear", "beginMonth", "beginDay"), sep = "-")
df4 <- df4 %>% 
  separate(endPosition, c("endYear", "endMonth", "endDay"), sep = "-")

# Removing years with NA values
df5 <- df4[!(is.na(df4$beginYear) | df4$beginYear == ""),]
df5 <- df5[!(is.na(df5$endYear) | df5$endYear == ""),]

fwrite(df5, "cache/data/imosData.csv")

##############################################################################
# Generating years between year ranges
imos <- fread("cache/data/imosData.csv")

# Removing years with NA values
imos <- imos[!(is.na(imos$beginYear) | imos$beginYear == ""),]
imos <- imos[!(is.na(imos$endYear) | imos$endYear == ""),]

t <- imos %>%
  dplyr::mutate(yearDif = as.integer(endYear) - as.integer(beginYear)) %>%
  tidyr::unite("yearRange", beginYear, endYear, sep = ",", remove = FALSE)


t <- t %>%
  dplyr::mutate(row = rownames(t))

t$beginYear <- as.integer(t$beginYear)
t$endYear <- as.integer(t$endYear)
t$yearDif <- as.integer(t$yearDif)
t$row <- as.integer(t$row)


out <- NULL

for (i in 1:nrow(t)) {
  print(i)
  
  l <- t %>% 
    dplyr::filter(row == i)
  
  if(l$yearDif > 0){
    
    df1 <- seq(l$beginYear, l$endYear)
    df2 <- as.data.frame(paste(df1, collapse = ","))
    colnames(df2) <- "years"
    df2 <- cbind(l, df2)
    
  } 
  
  else{
    df2 <- as.data.frame(l$beginYear)
    colnames(df2) <- "years"
    
    df2 <- cbind(l, df2)
    
  }
  out[[i]] <- df2
}

out_df <- do.call(rbind, out)

# Splitting columns by years
year_sep <- data.frame(do.call("rbind", strsplit(as.character(out_df$years), ",", fixed = TRUE)))

# merging with the original dataframe
df <- cbind(out_df, year_sep)

# Pivot longer
df_longer <- df %>%
  tidyr::pivot_longer(
    cols = starts_with("x"),
    names_to = "X",
    names_prefix = "X",
    values_to = "surveyYear")

# Removing unnecessary columns
df_longer <- df_longer %>% 
  dplyr::select("identifier", "metadataLinkage", "title", "description", "westBoundLongitude", "eastBoundLongitude", "northBoundLatitude", 
                "southBoundLatitude", "keyword", "surveyYear" )

# Removing duplicates
setkey(df_longer,NULL)
imos_revised <- unique(df_longer)

fwrite(imos_revised, "cache/data/imos_surveyYear_sep.csv")