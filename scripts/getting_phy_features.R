### libraries
if (!require("ape")) install.packages("ape"); library("ape")
if (!require("phytools")) install.packages("phytools"); library("phytools")
if (!require("geiger")) install.packages("geiger"); library("geiger")

### sourcing other functions
source("scripts/function_sister_pairs.R")

### phylogenetic tree
phy = read.tree(file = "0_data/mcc.tree")

### data
sp_target = read.table("0_data/sp_target.csv", sep = ",", h = T)

################################# PROCESSING DATA ##############################

### only species with known target
known = sp_target[!is.na(sp_target$target),]

### all species
all_spp = sp_target$species

########################## PHYLOGENETIC FEATURES ###############################

### vector to receive new variables
newvars = c()

### loop over species
for(focal_sp in all_spp){
  ### species to keep
  tips_to_keep = unique(c(focal_sp, known$species))
  ### prunning tree
  phy_prun = keep.tip(
    phy = phy,
    tip = tips_to_keep
  )
  ### phylogenetic distance
  phy_dist = cophenetic(phy_prun)
  ### minimal phylogenetic distance
  focal_sp_dists= round(phy_dist[focal_sp,],5)
  min_phy_dist = round( min(phy_dist[focal_sp,][phy_dist[focal_sp,] != 0]), 5)
  ### sister taxa list
  sister_list = sister_pairs(phy_dist)
  ### sister taxa of focal species
  sister_taxa = sister_list[[which(names(sister_list) == focal_sp)]]
  ### traits from sister taxa
  sister_target = known$target[known$species %in% sister_taxa ]
  ### most frequent trait from sister taxa
  freq_target = names(which.max(table(sister_target)))
  ### adding new entries
  newvars = rbind(newvars,c(focal_sp, freq_target, min_phy_dist))
}

df = as.data.frame(newvars)
colnames(df) = c("species", "sister_life_history", "phy_distance")

write.table(
  df,
  "phy_features.csv", 
  sep = ",",
  row.names = F,
  quote = F
  )
