### data
sp_target = read.table("0_data/sp_target.csv", sep = ",", h = T)


### data
sp_biogeo = read.table("0_data/sp_biogeo.csv", 
                       sep = ",", 
                       h = T,
                       colClasses = c("code" = "character"))

### break code to list
l = strsplit(sp_biogeo$code, split = "")

### organize into dataframe
df = data.frame(matrix(unlist(l), nrow=length(l), byrow=TRUE))
df$species = sp_biogeo$species

### only species in target dataset
df_filt = df[df$species %in% sp_target$species,]

##
write.table(
  df_filt,
  "biogeo_features.csv", 
  sep = ",",
  row.names = F,
  quote = F
)




       