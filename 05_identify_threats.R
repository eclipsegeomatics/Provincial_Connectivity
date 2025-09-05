#05_identity_threats 

# place holder to decide if this will be seperate breakdown of layers or using the human footprint layer as generated for connectivity
library(sf)
library(terra)

data_dir <- fs::path("Required_Spatial_Files")
eco_dir <- fs::path(data_dir, "04_ecological_data")

out_dir <- fs::path("outputs")
draft_out <- fs::path(out_dir, "draft")


# Loading raster and create a blank raster -------------------------------------------

rtemp <- rast(fs::path(data_dir, "05_geographical_data", "General Landscape Connectivity BC","Provincial scale", "cum_currmap_bc100m.tif"))
rtemp[rtemp > -1] <- 0

# read in the CE huamn disturbance layers (as placeholder?)
#https://catalogue.data.gov.bc.ca/dataset/7d61ff12-b85f-4aeb-ac8b-7b10e84b046c/resource/7b5789ad-7571-4216-a359-79fd722335f5/download/human-distrubance-description-for-bcdc-archived-and-current.pdf

gdb_path <- path('/home/user/Documents/00_data/base_vector/bc/BC_CEF_Human_Disturbance_2023/BC_CEF_Human_Disturbance_2023.gdb')

layers <- st_layers(gdb_path)
layer_name <- BC_CEF_Human_Disturb_BTM_2023

hd <- st_read(gdb_path)

#head(hd)

hd <- hd |> 
  filter(CEF_DISTURB_GROUP_RANK %in% c(1,2,3,4,5,6,7,8,9,10,11))

hd <- hd |> select(CEF_DISTURB_GROUP_RANK)

hd <- st_cast(hd, "MULTIPOLYGON")


hdr <- rasterize(hd , rtemp, field ="CEF_DISTURB_GROUP_RANK", cover = FALSE, touches = TRUE)

writeRaster(hdr , path(draft_out, "2_hotspots.tif"))



