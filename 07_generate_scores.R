# 
# download the ecoregions layer 

library(bcdata)
library(dplyr)
library(fs)
library(terra)
library(sf)

data_dir <- fs::path("Required_Spatial_Files")
#eco_dir <- fs::path(data_dir, "04_ecological_data")

out_dir <- fs::path("outputs")
draft_out <- fs::path(out_dir, "draft")

#rtemp_poly <- as.polygons(rtemp)
aoi <- st_read(fs::path(data_dir, "AOI", "BC_Outline.gpkg")) |> 
  rename("aoi" = juri_en) |> 
  select(aoi)

# Loading raster and create a blank raster -------------------------------------------

rtemp <- rast(fs::path(data_dir, "05_geographical_data", "General Landscape Connectivity BC","Provincial scale", "cum_currmap_bc100m.tif"))
rtemp[rtemp > -1] <- 0

#rtemp_poly <- as.polygons(rtemp)

# ecoregions 
#https://catalogue.data.gov.bc.ca/dataset/d00389e0-66da-4895-bd56-39a0dd64aa78
# Ecoprovince
# https://catalogue.data.gov.bc.ca/dataset/51832f47-efdf-4956-837a-45fc2c9032dd

#ec <- bcdc_query_geodata("51832f47-efdf-4956-837a-45fc2c9032dd") |>
#  collect()
#st_write(ec , file.path(out_dir, "bc_ecoprov.gpkg"), append = FALSE)

ec <- st_read(file.path(out_dir, "bc_ecoprov.gpkg"))

ec <- sf::st_intersection(ec, aoi) %>% 
  select(ECOPROVINCE_CODE,  ECOPROVINCE_NAME) 

total_area <- ec |> 
  group_by(ECOPROVINCE_NAME) |> 
  summarise()

ecc <- total_area |> 
  mutate(area = st_area(total_area)) 

st_write(ecc , file.path("inputs", "sk_ecoreg_reduced.gpkg"), append = FALSE)


######################################################################
# convert the base layers to quantiles and class 1-4
#####################################################################

### Identify High Ecological Value - by class

# still to decide on focal mean size 
eco <- rast(fs::path(draft_out, "1_ecol_focal_51.tif"))
vals <- values(eco$focal_mean, mat = FALSE)

vals <- vals[is.nan(vals) == 0]
svals <- vals[vals >0] 

hist(svals)

quantile(svals, probs = seq(0, 1, 0.20, na.rm = T ))

# reclass the raster 
m <- c(0, 1, 1,
       1, 2.198, 2,
       2.198, 3.199 , 3,
       3.199, 4.1376, 4,
       4.1376,  14, 5) # highest eco value

rclmat <- matrix(m, ncol=3, byrow=TRUE)
eco_class <- classify(eco, rclmat, include.lowest=TRUE)

eco_class[is.na(eco_class)]<- 0
eco_class <- mask(eco_class, rtemp)

writeRaster(eco_class, file.path(draft_out , "1_eco_focal_class.tif"), overwrite = TRUE)





### Identify the threats and class

# read in the catergorised threat map

th <- rast(fs::path(draft_out, "Resist_rc.tif"))
thp <- project(th, rtemp,  mask=TRUE)

# reproject the raster and mask 
th_class <- mask(thp, rtemp)
plot(th_class)

writeRaster(th_class, fs::path(draft_out, "2_threats_class_raw.tif"))

#try different scales for threats
f11 <- focal(th_class, 11, "max", na.rm = TRUE)
writeRaster(f11, fs::path(draft_out, "2_threat_class_focal_11.tif"))

f3 <- focal(th_class, 3, "max", na.rm = TRUE)
writeRaster(f3, fs::path(draft_out, "2_threat_class_focal_3.tif"))



# th <- rast(path(draft_out, "2_threat_focal_101.tif"))
# vals <- values(th$focal_mean, mat = FALSE)
# 
# vals <- vals[is.nan(vals) == 0]
# svals <- vals[vals >0] 
# 
# hist(svals)
# 
# qq <- quantile(svals, probs = seq(0, 1, 0.25, na.rm = T ))
# 
# # reclass the raster 
# m <- c(0, qq[[2]], 1,
#        qq[[2]], qq[[3]], 2,
#        qq[[3]], qq[[4]] , 3,
#        qq[[4]], qq[[5]], 4) # highest eco value
# 
# rclmat <- matrix(m, ncol=3, byrow=TRUE)
# th_class <- classify(th, rclmat, include.lowest=TRUE)
# 
# th_class[is.na(th_class )]<- 0
# th_class <- mask(th_class , rtemp)
# plot(th_class)
# 
# writeRaster(th_class , file.path(draft_out , "2_threat_focal_class.tif"), overwrite = TRUE)
# 
# 


### Identify the geographic features 

# assume already protected areas have some protection 











### Identify the linkages (Climate and connectivity)







# Overlay these areas 

ecor <- rast(file.path(draft_out , "1_eco_focal_class.tif"))
ecop <- as.polygons(ecor)
ecop <- st_as_sf(ecop) |> 
  rename("eco_class" = focal_mean)

thr <- rast(file.path(draft_out, "2_threat_focal_class.tif"))
thp <- as.polygons(thr)
thp <- st_as_sf(thp)|> 
  rename("thr_class" = focal_mean)

# intersect to determine the overals
ecoth <- st_intersection(thp, ecop)
ecoth <- st_buffer(ecoth, 0)
#ecoth <- st_make_valid(ecoth)
#ecoth$area = st_area(ecoth)

st_write(ecoth, path(draft_out, "test_intersect2.gpkg"))

# summary of all the combinations 

ecoth$area <- as.numeric(st_area(ecoth)/10000)





# In general the national parks / protected areas are low human impact (not always)

# rehabilitation 
# where are the high value areas with high human pressure 

# high priority
heht <- ecoth |> 
  filter(eco_class == 4) |> 
  filter(thr_class == 4)

# moderate priority
meht <- ecoth |> 
  filter(eco_class == 3) |> 
  filter(thr_class == 4)

# low prioirty 
meht <- ecoth |> 
  filter(eco_class == 2) |> 
  filter(thr_class == 4)


# restore 
# where are the high value areas with moderate human pressure

hemt <- ecoth |> 
  filter(eco_class == 4) |> 
  filter(thr_class %in% c(2,3))

memt <- ecoth |> 
  filter(eco_class == 3) |> 
  filter(thr_class %in% c(2,3))


# conserve 
# high value areas with low human pressure 

helt <- ecoth |> 
  filter(eco_class == 4) |> 
  filter(thr_class == 1)

melt <- ecoth |> 
  filter(eco_class == 3) |> 
  filter(thr_class == 1)



# analysis of proposed area..... omniscape 

# level of protection ? 

# add level of 
ecoth <- ecoth |> 
  mutate(type = case_when(
    thr_class == 4 ~ "rehabilitate",
    thr_class == 2 ~ "restore",
    thr_class == 3 ~ "restore",
    thr_class == 1 ~ "conserve"
  ))

ecoth <- ecoth |> 
  mutate(priority = case_when(
    eco_class == 4 ~ "high",
    eco_class %in% c(2,3) ~ "medium",
    eco_class == 1 ~ "low"
  )) |> 
  rowwise() |> 
  mutate(type_prio = paste0(type, "_",priority))


st_write(ecoth, path(draft_out, "test_intersect3.gpkg"))


## Identify high ecological value not protected with high threat (Rehabilitate)








