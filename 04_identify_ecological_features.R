## Testing file for the calculation of ecological features 


# required.... base raster (with buffer?)
library(fs)
library(terra)
library(sf)
install.packages("remotes")
remotes::install_github("bcgov/bcmaps")
library(bcmaps)
library(dplyr)

data_dir <- fs::path("Required_Spatial_Files")

eco_dir <- fs::path(data_dir, "04_ecological_data")

# Loading raster and create a blank raster -------------------------------------------

rtemp <- rast(fs::path(data_dir, "05_geographical_data", "General Landscape Connectivity BC","Provincial scale", "cum_currmap_bc100m.tif"))
rtemp[rtemp > -1] <- 0

#rtemp_poly <- as.polygons(rtemp)


# read in geographic dataset and create raster stack
list.files(eco_dir)


# 1) red and blue listed species 
# filter steps - select only species listed as red and blue 
# scoring - Red = 1, blue = 2

rb <- st_read(path(eco_dir, "BC Red and Blue Listed Species", "BIO_NS_SVW_polygon.gpkg" )) |> 
  filter(BC_LIST %in% c("Red", "Blue")) |> 
  mutate(list_sp_score = case_when(
    BC_LIST == "Red" ~ 1, 
    BC_LIST == "Blue" ~ 2
  ))

# convert to raster 
rbr <- rasterize(rb, rtemp, field ="list_sp_score", cover = FALSE, touches = TRUE)
 

# 2) Endemism hotspots 
# filter by number of endemic species 
# score by number of endemic species 

#list.files(path(eco_dir, "Endemism Hotspots"))

eh <- st_read(path(eco_dir,  "Endemism Hotspots" , "CDN_Endemic_Hotspots.gpkg" )) |> 
  mutate(endemic_n = N_CDN)

ehr <- rasterize(eh, rtemp, field ="endemic_n", cover = FALSE, touches = TRUE)



# 3) Key Biodiversity areas 
# score any area as KBA = 1
kb <- st_read(path(eco_dir, "Key Biodiversity Areas", "kba.20250623050652.gpkg" )) |> 
  mutate(kba = 1)

kbr <- rasterize(kb, rtemp, field ="kba", cover = FALSE, touches = TRUE)



# 4) PAcific Estuary ranking
# score any area as KBA = 1
pe <- st_read(path(eco_dir, "Pacific Estuary Ranking", "PECP_estuary_polys_ranked_2019_PUBLIC.gpkg" )) |> 
   mutate(pe_rank = IMP_CL2019) |> 
   mutate(pe_rank = case_when(
     pe_rank == "not ranked"~ 6, 
     pe_rank %in% c("1", "2", "3", "4", "5") ~ as.numeric(pe_rank), 
     .default = NA
   ))
   


unique(pe$pe_rank)




kbr <- rasterize(kb, rtemp, field ="kba", cover = FALSE, touches = TRUE)



# convert all layers of interest into rasters 





# ranking for ecological features 
