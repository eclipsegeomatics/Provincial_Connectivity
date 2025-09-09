#05_identity_threats 

# place holder to decide if this will be seperate breakdown of layers or using the human footprint layer as generated for connectivity
library(sf)
library(terra)
library(fs)
library(dplyr)

data_dir <- fs::path("Required_Spatial_Files")
eco_dir <- fs::path(data_dir, "04_ecological_data")

out_dir <- fs::path("outputs")
draft_out <- fs::path(out_dir, "draft")

out_threat <- fs::path(data_dir, "03_threat_data/")
list.files(out_threat)

# Loading raster and create a blank raster -------------------------------------------

rtemp <- rast(fs::path(data_dir, "05_geographical_data", "General Landscape Connectivity BC","Provincial scale", "cum_currmap_bc100m.tif"))
rtemp[rtemp > -1] <- 0
# 
# # read in the CE huamn disturbance layers (as placeholder?)
# #https://catalogue.data.gov.bc.ca/dataset/7d61ff12-b85f-4aeb-ac8b-7b10e84b046c/resource/7b5789ad-7571-4216-a359-79fd722335f5/download/human-distrubance-description-for-bcdc-archived-and-current.pdf
# 
# gdb_path <- path('/home/user/Documents/00_data/base_vector/bc/BC_CEF_Human_Disturbance_2023/BC_CEF_Human_Disturbance_2023.gdb')
# 
# layers <- st_layers(gdb_path)
# #layer_name <- "BC_CEF_Human_Disturb_BTM_2023"
# 
# #head(hd)
# 
# hd <- st_read(gdb_path) |> 
#   filter(CEF_DISTURB_GROUP_RANK %in% c(1,2,3,4,5,6,7,8,9,10,11)) |> 
#   select(CEF_DISTURB_GROUP_RANK)  
# 
# hd <- st_as_sf(hd)
# 
# # write out 
# st_write(hd, path(out_threat, "CE_HUMAN_DIST_2023.gpkg"))
# 
# # fix geometry and write out clean copy
# hd <- st_read(path(out_threat, "CE_HUMAN_DIST_2023.gpkg"))
# hd <- st_cast(hd, "MULTIPOLYGON")
# st_write(hd, path(out_threat, "CE_HUMAN_DIST_2023_fixgeom.gpkg"))


# read in the new fixed geom
hd <- st_read(path(out_threat, "CE_HUMAN_DIST_2023_fixgeom.gpkg"))

ids <-sort(unique(hd$CEF_DISTURB_GROUP_RANK))

out <- purrr::map(ids, function(i){
  
  #i <- ids[1]
  hdi <- hd |> 
    filter(CEF_DISTURB_GROUP_RANK == i)
  
  hdr <- rasterize(hdi , rtemp, field ="CEF_DISTURB_GROUP_RANK", cover = FALSE, touches = TRUE)
  writeRaster(hdr , path(draft_out, paste0("3_threats_hd_", i,".tif")), overwrite = TRUE)
  
})

# stack raster together and re-score with highest as 12 and lowest as 1

tf <- list.files(path(draft_out), pattern = "^3_threat*")

out <- purrr::map(tf, function(i){

  #i <- tf[1]
  tt <- rast(path(draft_out, i))
  tval <- unique(tt$CEF_DISTURB_GROUP_RANK)
  
  # invert number values 
  reval = 13 - tval 
  
  tt[(tt)>0]<- reval 
  tt[is.na(tt)]<- 0 
  
  ttr <- mask(tt, rtemp) 
  ttr
  
})


# Merge all the layers to combine 

combo <- out[[1]]+ out[[2]] + out[[3]]+ out[[4]]+ out[[5]] +out[[6]] + out[[7]]+
  out[[8]]+ out[[9]]+ out[[10]]+ out[[11]]


writeRaster(combo, path(draft_out, "3_threat_combined.tif"))

# generate focal metrics 


# smoother threats at broad scale - 
#testing various scales

 #try different scales for threats
f7 <- focal(combo, 7, "mean", na.rm=TRUE) 
f11 <- focal(combo, 11, "mean", na.rm = TRUE)
f23 <- focal(combo, 23, "mean", na.rm = TRUE)
f55 <- focal(combo, 51, "mean", na.rm = TRUE)
f75 <- focal(combo, 75, "mean", na.rm = TRUE)
writeRaster(f75, path(draft_out, "2_threat_focal_75.tif"))
f101 <- focal(combo, 101, "mean", na.rm = TRUE)
writeRaster(f101, path(draft_out, "2_threat_focal_101.tif"))    
f151 <- focal(combo, 151, "mean", na.rm = TRUE)
writeRaster(f151, path(draft_out, "2_threat_focal_151.tif"))    
f201 <- focal(combo, 201, "mean", na.rm = TRUE)
writeRaster(f201, path(draft_out, "2_threat_focal_201.tif"))    


##|> mask(r)

writeRaster(f7, path(draft_out, "2_threat_focal_7.tif"))
writeRaster(f11, path(draft_out, "2_threat_focal_11.tif"))
writeRaster(f23, path(draft_out, "2_threat_focal_23.tif"))
writeRaster(f55, path(draft_out, "2_threat_focal_51.tif"))
writeRaster(f75, path(draft_out, "2_threat_focal_75.tif"))
writeRaster(f101, path(draft_out, "2_threat_focal_101.tif"))    




