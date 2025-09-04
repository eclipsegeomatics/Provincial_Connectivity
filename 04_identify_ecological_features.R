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
# using IMP class 2019 and converting not ranked to the lowest number 6?. Might need to reverse these values
pe <- st_read(path(eco_dir, "Pacific Estuary Ranking", "PECP_estuary_polys_ranked_2019_PUBLIC.gpkg" )) |> 
   mutate(pe_rank = IMP_CL2019) |> 
   mutate(pe_rank = case_when(
     pe_rank == "not ranked"~ 6, 
     pe_rank %in% c("1", "2", "3", "4", "5") ~ as.numeric(pe_rank), 
     .default = 0
   ))
   
per <- rasterize(pe, rtemp, field ="pe_rank", cover = FALSE, touches = TRUE)

unique(pe$pe_rank)



# 5) Priority places for species at risk
# using aoi 
#st_layers(path(eco_dir, "Priority Places for Species at Risk - Terrestrial", "PriorityPlaces.gpkg"))

pp <- st_read(path(eco_dir, "Priority Places for Species at Risk - Terrestrial", "PriorityPlaces.gpkg"), layer= "PriorityPlacesBoundary" ) |> 
  mutate(priority_place = 1) 
  
ppr <- rasterize(pp, rtemp, field ="priority_place", cover = FALSE, touches = TRUE)



# 6 Provincial 
list.files(path(eco_dir,"Provincial Priority Old Growth Forests"))

# ancient forest 
an <- st_read(path(eco_dir,"Provincial Priority Old Growth Forests", "Ancient Forest","OGSR_TAF_polygon.gpkg")) 
an <- an |> 
  select(DESCR) |> 
  mutate(ancient = case_when(
    DESCR == "Ancient at older than 250 years" ~ 1,
    DESCR == "Ancient at older than 400 years" ~ 2,
    .default = 0
  ))
anr <- rasterize(an, rtemp, field ="ancient", cover = FALSE, touches = TRUE)


list.files(path(eco_dir,"Provincial Priority Old Growth Forests", "Ancient Forest","OGSR_TAF_polygon.gpkg"))




## overlay the important ecological zones 




## collate threats and overlay the highest treat zones 






# ranking for ecological features 
