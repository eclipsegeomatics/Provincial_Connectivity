#06_Geographic 

library(fs)
library(terra)
library(sf)
library(dplyr)

data_dir <- fs::path("Required_Spatial_Files")
geo_dir <- fs::path(data_dir, "05_geographical_data")

out_dir <- fs::path("outputs")
draft_out <- fs::path(out_dir, "draft")


# Loading raster and create a blank raster -------------------------------------------

rtemp <- rast(fs::path(data_dir, "05_geographical_data", "General Landscape Connectivity BC","Provincial scale", "cum_currmap_bc100m.tif"))
rtemp[rtemp > -1] <- 0

#rtemp_poly <- as.polygons(rtemp)
aoi <- st_read(path(data_dir, "AOI", "BC_Outline.gpkg")) |> 
  rename("aoi" = juri_en) |> 
  select(aoi)

# read in geographic dataset and create raster stack
list.files(geo_dir)

# read in the static conservation area

#https://bcparks.ca/about/our-mission-responsibilities/types-parks-protected-areas/


#1) Admin conservation Lands

#list.files(path(geo_dir, "Adminstrative Conservation Lands"))

ad <- st_read(path(geo_dir, "Adminstrative Conservation Lands", "WCLCNSRVTN_polygon.gpkg" )) |>
  mutate(type = "admin_cons_lands") |> 
  mutate(type_score = 1) 

ad <- st_intersection(ad, aoi)

adr <- rasterize(ad, rtemp, field ="type_score", cover = FALSE, touches = TRUE)
adr[is.na(adr)]<- 0
adr <- mask(adr, rtemp)


#2) bc parks conservancy 

#list.files(path(geo_dir, "BC Parks Conservancy"))

ad <- st_read(path(geo_dir, "BC Parks Conservancy", "TA_CA_SVW_polygon.gpkg" )) |>
  mutate(type = "conservancy")|> 
  mutate(type_score = 1) 

cor <- rasterize(ad, rtemp, field ="type_score", cover = FALSE, touches = TRUE)
cor[is.na(cor)]<- 0
cor <- mask(cor, rtemp)


# 3) Protected areas 
#list.files(path(geo_dir, "Protected Areas"))

ad <- st_read(path(geo_dir, "Protected Areas", "TA_PEP_SVW_polygon.gpkg" )) |>
  mutate(type = PROT_DESG)|> 
  mutate(type_score = 1) 

prr <- rasterize(ad, rtemp, field ="type_score", cover = FALSE, touches = TRUE)
prr[is.na(prr)]<- 0
prr <- mask(prr, rtemp)




#4) Protected conservation areas (federal)

#st_layers(path(geo_dir, "Protected Conserved Areas", "ProtectedConservedArea_2024.gpkg" ))
ad <- st_read(path(geo_dir, "Protected Conserved Areas", "ProtectedConservedArea_2024.gpkg" )) |>
  select(TYPE_E) |> 
  filter(TYPE_E != "Old Growth Management Areas (Mapped Legal)") |> 
  st_cast("MULTIPOLYGON")

ad <- st_intersection(ad, aoi) 
ad <- ad |> mutate(type_score = 1) 


pcr <- rasterize(ad, rtemp, field ="type_score", cover = FALSE, touches = TRUE)
pcr[is.na(pcr)]<- 0
pcr <- mask(pcr, rtemp)

# Lets join the protected areas to review 

pro <- adr +  cor + prr + pcr

plot(pro)

writeRaster(pro, path( draft_out, "2_geographic_static.tif"))

#################################################################################

# identification of connecting pieces, refugia? 

combo <- out[[1]]+ out[[2]] + out[[3]]+ out[[4]]+ out[[5]] +out[[6]] + out[[7]]+
         out[[8]]+ out[[9]]+ out[[10]]+ out[[11]]





