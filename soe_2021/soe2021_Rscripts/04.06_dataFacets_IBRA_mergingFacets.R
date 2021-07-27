# Merging datafiles
list <- list.files(path = "cache/sumTable/ibra/", pattern = ".csv", full.names = TRUE)

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
df43 <- fread(list[[43]])
df44 <- fread(list[[44]])
df45 <- fread(list[[45]])
df46 <- fread(list[[46]])
df47 <- fread(list[[47]])
df48 <- fread(list[[48]])
df49 <- fread(list[[49]])
df50 <- fread(list[[50]])
df51 <- fread(list[[51]])
df52 <- fread(list[[52]])
df53 <- fread(list[[53]])
df54 <- fread(list[[54]])
df55 <- fread(list[[55]])
df56 <- fread(list[[56]])
df57 <- fread(list[[57]])
df58 <- fread(list[[58]])
df59 <- fread(list[[59]])
df60 <- fread(list[[60]])
df61 <- fread(list[[61]])
df62 <- fread(list[[62]])
df63 <- fread(list[[63]])
df64 <- fread(list[[64]])
df65 <- fread(list[[65]])
df66 <- fread(list[[66]])
df67 <- fread(list[[67]])
df68 <- fread(list[[68]])
df69 <- fread(list[[69]])
df70 <- fread(list[[70]])
df71 <- fread(list[[71]])
df72 <- fread(list[[72]])
df73 <- fread(list[[73]])

df <- df14 %>%
  dplyr::left_join(df1, by = c("IBRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df2, by = c("IBRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df3, by = c("IBRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df4, by = c("IBRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df5, by = c("IBRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df6, by = c("IBRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df7, by = c("IBRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df8, by = c("IBRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df9, by = c("IBRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df10, by = c("IBRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df11, by = c("IBRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df12, by = c("IBRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df13, by = c("IBRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df15, by = c("IBRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df16, by = c("IBRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df17, by = c("IBRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df18, by = c("IBRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df19, by = c("IBRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df20, by = c("IBRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df21, by = c("IBRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df22, by = c("IBRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df23, by = c("IBRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df24, by = c("IBRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df25, by = c("IBRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df26, by = c("IBRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df27, by = c("IBRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df28, by = c("IBRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df29, by = c("IBRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df30, by = c("IBRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df31, by = c("IBRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df32, by = c("IBRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df33, by = c("IBRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df34, by = c("IBRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df35, by = c("IBRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df36, by = c("IBRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df37, by = c("IBRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df38, by = c("IBRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df39, by = c("IBRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df40, by = c("IBRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df41, by = c("IBRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df42, by = c("IBRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df43, by = c("IBRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df44, by = c("IBRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df45, by = c("IBRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df46, by = c("IBRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df47, by = c("IBRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df48, by = c("IBRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df49, by = c("IBRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df50, by = c("IBRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df51, by = c("IBRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df52, by = c("IBRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df53, by = c("IBRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df54, by = c("IBRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df55, by = c("IBRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df56, by = c("IBRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df57, by = c("IBRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df58, by = c("IBRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df59, by = c("IBRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df60, by = c("IBRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df61, by = c("IBRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df62, by = c("IBRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df63, by = c("IBRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df64, by = c("IBRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df65, by = c("IBRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df66, by = c("IBRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df67, by = c("IBRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df68, by = c("IBRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df68, by = c("IBRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df69, by = c("IBRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df70, by = c("IBRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df71, by = c("IBRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df72, by = c("IBRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df73, by = c("IBRA", "YearRange"))

write.csv(df, "cache/sumTable/fullMerged_ibra.csv")

