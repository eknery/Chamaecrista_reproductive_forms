### libraries
if (!require("terra")) install.packages("terra"); library("terra")
if (!require("sf")) install.packages("sf"); library("sf")

### importing direcotry
imp_dir = "0_reported_records/"

### data
sp_target = read.table("0_data/sp_target.csv", sep = ",", h = T)

### all species 
all_spp_names = sp_target$species

### calculte area
area = c()
for (n in 1:length(all_spp_names)){
  sp_name = all_spp_names[n]
  sp_file = readRDS(paste0(imp_dir, gsub("_", " ", sp_name) ))
  sp_points = as.matrix(sp_file[, c("decimalLongitude", "decimalLatitude")])
  sp_multi = st_multipoint(as.matrix(sp_points))
  sp_convex = sf::st_convex_hull(sp_multi)
  area = c(area, st_area(sp_convex))
}

area_features = as.data.frame(cbind(all_spp_names, area))

colnames(area_features) = c("species", "area")

write.table(
  area_features,
  "1_features/area_features.csv",
  sep = ",",
  row.names =  F,
  quote = F
)
