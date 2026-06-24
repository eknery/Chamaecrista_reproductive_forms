### libraries
if (!require("tidyverse")) install.packages("tidyverse"); library("tidyverse")

### data
sp_target = read.table("0_data/sp_target.csv", sep = ",", h = T)

### data
sp_elev = read.table(
  "0_data/sp_elev.csv",
  sep = ",",
  h = T
  )

### select species 
elev_features = sp_elev[sp_elev$species %in% sp_target$species,]

###
write.table(
  elev_features,
  "1_features/elev_features.csv",
  sep = ",",
  row.names = F,
  quote = F
)
