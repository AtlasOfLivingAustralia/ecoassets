# Merging datafiles
list <- list.files(path = "cache/sumTable/imcra/", pattern = ".csv", full.names = TRUE)

# df <- rbindlist(list, use.names = TRUE)

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
df16 <- fread(list[[16]])
df17 <- fread(list[[17]])
df18 <- fread(list[[18]])
df19 <- fread(list[[19]])
df20 <- fread(list[[20]])
df21 <- fread(list[[21]])
df22 <- fread(list[[22]])
df23 <- fread(list[[23]])
df24 <- fread(list[[24]])
df25 <- fread(list[[25]])
df26 <- fread(list[[26]])
df27 <- fread(list[[27]])
df28 <- fread(list[[28]])
df29 <- fread(list[[29]])
df30 <- fread(list[[30]])
df31 <- fread(list[[31]])
df32 <- fread(list[[32]])
df33 <- fread(list[[33]])
df34 <- fread(list[[34]])
df35 <- fread(list[[35]])
df36 <- fread(list[[36]])
df37 <- fread(list[[37]])
df38 <- fread(list[[38]])
df39 <- fread(list[[39]])
df40 <- fread(list[[40]])
df41 <- fread(list[[41]])
df42 <- fread(list[[42]])


df <- df8 %>%
  dplyr::left_join(df1, by = c("IMCRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df2, by = c("IMCRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df3, by = c("IMCRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df4, by = c("IMCRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df5, by = c("IMCRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df6, by = c("IMCRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df7, by = c("IMCRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df9, by = c("IMCRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df10, by = c("IMCRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df11, by = c("IMCRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df12, by = c("IMCRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df13, by = c("IMCRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df14, by = c("IMCRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df15, by = c("IMCRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df16, by = c("IMCRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df17, by = c("IMCRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df18, by = c("IMCRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df19, by = c("IMCRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df20, by = c("IMCRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df21, by = c("IMCRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df22, by = c("IMCRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df23, by = c("IMCRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df24, by = c("IMCRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df25, by = c("IMCRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df26, by = c("IMCRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df27, by = c("IMCRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df28, by = c("IMCRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df29, by = c("IMCRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df30, by = c("IMCRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df31, by = c("IMCRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df32, by = c("IMCRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df33, by = c("IMCRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df34, by = c("IMCRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df35, by = c("IMCRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df36, by = c("IMCRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df37, by = c("IMCRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df38, by = c("IMCRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df39, by = c("IMCRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df40, by = c("IMCRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df41, by = c("IMCRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df42, by = c("IMCRA", "YearRange"))

write.csv(df, "cache/sumTable/fullMerged_imcra.csv")
