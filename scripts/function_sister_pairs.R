
sister_pairs = function(phy_dist){
  sister_taxa_list = vector('list', nrow(phy_dist))
  all_spp_names = row.names(phy_dist)
  names(sister_taxa_list) = all_spp_names
  for(focal_sp in all_spp_names){
    focal_sp_dists= round(phy_dist[focal_sp,],5)
    min_phy_dist = round( min(phy_dist[focal_sp,][phy_dist[focal_sp,] != 0]), 5)
    sister_index = which(focal_sp_dists == min_phy_dist)
    sister_taxa_list[[focal_sp]] = names(focal_sp_dists)[sister_index]
  }
  return(sister_taxa_list)
}
