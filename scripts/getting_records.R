### libraries
if (!require("remotes")) install.packages("remotes"); library("remotes")
if (!require("plantR")) remotes::install_github("LimaRAF/plantR"); library("plantR")
if (!require("CoordinateCleaner")) install.packages("CoordinateCleaner"); library("CoordinateCleaner")

### species names
df_names = read.table("0_data/chamaecrista_names.csv", sep=",", h=T)

### exporting directory
exp_dir = "1_reported_records/"

### final variables to keep
final_vars = c(
  "species_name",
  "reported_name",
  "identifiedBy",
  "recordedBy",
  "recordNumber",
  "collectionCode",
  "catalogNumber",
  "country",
  "stateProvince",
  "municipality",
  "decimalLongitude",
  "decimalLatitude",
  "year",
  "month",
  "day"
)

### valid indentifiers
valid_identifiers = c(
  "Barneby",
  "Bortoluzzi",
  "Conceição",
  "Cota",
  "Fernandes",
  "Guedes",
  "Irwin",
  "Lewis",
  "Queiroz",
  "Rando",
  "Souza"
)
### collapse into single string
valid_identifiers = paste0(valid_identifiers, collapse = "|")

for(i in 2:nrow(df_names)){ 
  ### initial record df
  raw_rec = data.frame()
  ### oficial species name
  species_name = df_names$species_name[i]
  species_name = gsub("_", " ", species_name)
  ### reported name
  reported_name = df_names$reported_name[i]
  reported_name = gsub("_x_", "_", reported_name)
  reported_name = gsub("_", " ", reported_name)
  
  ### download records from GBIF
  n_try = 1
  while (nrow(raw_rec) == 0 & n_try < 4){
    tryCatch({
      ### download records
      raw_rec = rgbif2(
        species = reported_name,
        n.records = 10000,
        force = FALSE,
        remove_na = T,
        save = FALSE,
        file.format = "csv",
        compress = FALSE
      )
    }, error=function(e){print("Could not download, trying again!")})
    ### one more try!
    n_try = n_try + 1
  }
  if(nrow(raw_rec) >= 1){
    ### initial treated records
    treated = raw_rec
    ### assume collector as identifier for NAs
    na_id = is.na(treated$identifiedBy)
    treated$identifiedBy[na_id] = treated$recordedBy[na_id]
    ### only valid identifier
    # treated = treated[grepl(pattern = valid_identifiers, treated$identifiedBy), ]
    # treated = treated[!is.na(treated$identifiedBy), ]
    ### force numeric for coordinates
    treated$decimalLatitude = as.numeric(treated$decimalLatitude)
    treated$decimalLongitude = as.numeric(treated$decimalLongitude)
  } else {
    treated = raw_rec
  }
  
  if(nrow(treated) >= 1){
  ### removing invalid coordinates
    treated = cc_val(
      treated, 
      lon = "decimalLongitude", 
      lat = "decimalLatitude", 
      value = "clean"
    )
  }
  
  if(nrow(treated) >= 1){
    ### removing records in the sea
    treated = cc_sea(
      treated, 
      lon = "decimalLongitude", 
      lat = "decimalLatitude",  
      scale = 110, 
      value = "clean", 
      speedup = T
    )
  }
  
  if(nrow(treated) >= 1){
    ### removing records near centroids of countries and provinces
    treated = cc_cen(
      treated, 
      lon = "decimalLongitude", 
      lat = "decimalLatitude", 
      buffer = 1000, 
      geod = TRUE, 
      test = "both", 
      ref = NULL, 
      verify = FALSE, 
      value = "clean"
    )
  }
  
  if(nrow(treated) >= 1){
    ### remvoing records in urban areas
    treated = cc_urb(
      treated,
      lon = "decimalLongitude",
      lat = "decimalLatitude",
      ref = NULL,
      value = "clean",
      verbose = TRUE
    )
  }
  
  if(nrow(treated) >= 10){
    ### removing outliers
    treated = cc_outl(
      treated,
      lon = "decimalLongitude",
      lat = "decimalLatitude",
      method = "distance",
      mltpl = 5,
      tdi = 1000,
      value = "clean",
      sampling_thresh = 0,
      verbose = TRUE,
      min_occs = 10,
      thinning = FALSE,
      thinning_res = 0.5
    )
  }
  
  if(nrow(treated) >= 1){
    ### add names 
    treated$reported_name = reported_name
    treated$species_name = species_name
    ### add missing columns
    for(j in 1:length(final_vars)){
      check = final_vars[j] %in% colnames(treated)
      if(check == F){
        treated[,final_vars[j]] = NA
      }
    }
    ### sort final variables
    sp_data = treated[,final_vars]
    ### export
    saveRDS(
      sp_data,
      file = paste0(exp_dir,reported_name),
    )
    ### check!
    final_time = format(Sys.time(), "%d %m %X")
    print(paste0("Records done: ", reported_name,", ", final_time))
  } else {
    ### check !
    print(paste0("No viable records for: ", reported_name))
  }

}
