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
# 
# f3 <- focal(th_class, 3, "max", na.rm = TRUE)
# writeRaster(f3, fs::path(draft_out, "2_threat_class_focal_3.tif"))



# combine ecological features and threat in matrix 
# ecological features and threat matrix
th <- rast(fs::path(draft_out, "2_threat_class_focal_11.tif"))

ef <- rast(fs::path(draft_out , "1_eco_focal_class.tif"))




#### Ecological - threat  matrix  ####################################
# conserve options 
# ef = 5 & 4 , th = 1,2


# maintain options 
# keep only top two codes for each output 

m <- c(0, 2, 0, # lowest diversity 
       3, 3, 3,
       4, 4, 4,
       5, 5, 5) # highest diversity 

rclmat <- matrix(m, ncol=3, byrow=TRUE)

ef_vh_h_m <- classify(ef, rclmat, include.lowest=TRUE)

# multiply by 10 to enable unique combinations 
ef_vh_h_m <- ef_vh_h_m*10 
out <- ef_vh_h_m + th 

# remove the combinations where not important ecological (ie <30)




# unique va;ues 
# conserve 
#          51  - Very High ecological feaure + very low threat - conserve 
# 8        52  - Very High ecological feaure + low threat - conserve 
# 9        41  - High ecological feaure + very low threat - conserve 
# 9        42  - High ecological feaure + low threat - conserve 
out_conserve <- subst(out , c(51, 52, 41, 42), c(51, 52, 41, 42), others=NA)
m <- c(0, 1, 0, 
       1, 60, 1) 
rclmat <- matrix(m, ncol=3, byrow=TRUE)

out_conserve_simple <-  classify(out_conserve, rclmat, include.lowest=TRUE)
out_conserve_simple[is.na(out_conserve_simple[])] <- 0 
plot(out_conserve_simple)
  
# maintain 
# 1         53 - Very High ecological feaure + vmoderate threat - maintain 
# 1         44 - Very High ecological feaure + vmoderate threat - maintain 
# 1         43 - Very High ecological feaure + vmoderate threat - maintain 
# 2         34  - Moderate ecological feaure + very low  threat - maintain 

out_maintain <- subst(out , c(53, 44, 43, 34), c(53, 44, 43, 34), others=NA)
m <- c(0, 1, 0, 
       1, 60, 2) 
rclmat <- matrix(m, ncol=3, byrow=TRUE)
out_maintain_simple <-  classify(out_maintain, rclmat, include.lowest=TRUE)
out_maintain_simple[is.na(out_maintain_simple[])] <- 0 
plot(out_maintain_simple)

# restore 

#          45  - High ecological feaure + very high threat - restore 
#          35  - Moderate ecological feaure + very high threat - restore 
# 8        54  - Very High ecological feaure + high threat - restore 
# 9        55  - Very High ecological feaure + very high threat - restore 

out_restore <- subst(out , c(45, 54, 55, 35), c(45, 54, 55, 35), others=NA)
plot(out_restore)
m <- c(0, 1, 0, 
       1, 60, 3) 
rclmat <- matrix(m, ncol=3, byrow=TRUE)
out_restore_simple <-  classify(out_restore, rclmat, include.lowest=TRUE)
out_restore_simple[is.na(out_restore_simple[])] <- 0 
plot(out_maintain_simple)


out_simple <- out_restore_simple + out_maintain_simple + out_conserve_simple
# 3 + 2 + 1

plot(out_simple)
out_simple <- mask(out_simple , rtemp)


writeRaster(out_simple, fs::path(draft_out, "5_eco_threat_classed.tif"))

# note - might want to over lay the current parks over the top of this?








########################################################################3
## Corridors combination 
#################################################################

##) Climate connectivity layer 

cc <- rast(fs::path(draft_out, "corridors_S2_Parks_extract.tif"))
#vals <- values(cc$corridors_S2_Parks_extract, mat = FALSE)
# reproject the raster and mask 
cc <- project(cc, rtemp,  mask=TRUE)
#vals <- vals[is.nan(vals) == 0]
#svals <- vals[vals >0]
#quantile(svals, probs = seq(0, 1, 0.20, na.rm = T ))

# reclass the raster 
m <- c(0, 4.2, 1,
       4.2, 24.9, 2,
       24.9, 155, 3,
       155, 979, 4,
       979,  7000, 5) # highest eco value

rclmat <- matrix(m, ncol=3, byrow=TRUE)
cc_class <- classify(cc, rclmat, include.lowest=TRUE)

cc_class[is.na(cc_class)]<- 0
cc_class <- mask(cc_class, rtemp)
plot(cc_class)

writeRaster(cc_class, fs::path(draft_out, "4_climate_corr_class.tif"))






# FUTURE CLIMATE CORRIDORS 

# select the highest prioirty areas 
cc_class <- rast(fs::path(draft_out, "4_climate_corr_class.tif"))

# climate keep very important / important areas /moderare (3,4,5 values)
m <- c(0, 2, 0, # lowest diversity 
       3, 3, 4,
       4, 4, 5,
       5, 5, 6) # highest diversity 

rclmat <- matrix(m, ncol=3, byrow=TRUE)

cc_cor <- classify(cc_class, rclmat, include.lowest=TRUE)


# overlay the ecological threat - features with 





# multiply by 10 to enable unique combinations 
cc_vh_h_m <- cc_vh_h_m*10 
out <- cc_vh_h_m + con

writeRaster(out, fs::path(draft_out, "6_corridor_class_test.tif"))


writeRaster(out_simple, fs::path(draft_out, "5_eco_threat_classed.tif"))









## provincial connectivity layer 

con <- rast(fs::path(draft_out, "norm_cum_currW3.tif"))
# reproject the raster and mask 
con <- project(con, rtemp,  mask=TRUE)
con
# reclass the raster 
m <- c(0, 0.2, 1,
       0.2, 0.7, 2,
       0.7, 1.3, 3,
       1.3, 1.7, 4,
       1.7,  100, 5) # highest eco value

rclmat <- matrix(m, ncol=3, byrow=TRUE)
con_class <- classify(con, rclmat, include.lowest=TRUE)

#con_class[is.na(con_class)]<- 0
con_class <- mask(con_class, rtemp)
plot(con_class)

writeRaster(con_class, fs::path(draft_out, "3_cons_corr_class.tif"))


##############################################################

# Matrix area 

cc_class <- rast(fs::path(draft_out, "4_climate_corr_class.tif"))

con <- rast(fs::path(draft_out, "3_cons_corr_class.tif"))

# climate keep very important / important areas /moderare (3,4,5 values)
m <- c(0, 2, 0, # lowest diversity 
       3, 3, 3,
       4, 4, 4,
       5, 5, 5) # highest diversity 

rclmat <- matrix(m, ncol=3, byrow=TRUE)

cc_vh_h_m <- classify(cc_class, rclmat, include.lowest=TRUE)

# multiply by 10 to enable unique combinations 
cc_vh_h_m <- cc_vh_h_m*10 
out <- cc_vh_h_m + con

writeRaster(out, fs::path(draft_out, "6_corridor_class_test.tif"))


# unique va;ues 
# conserve 
#          51  - Very High climate + limited movement pot 
# 8        52  - Very High ecological feaure + impeded flow9
;1

# 9        41  - High ecological feaure + very low threat - conserve 
# 9        42  - High ecological feaure + low threat - conserve 
out_conserve <- subst(out , c(51, 52, 41, 42), c(51, 52, 41, 42), others=NA)
m <- c(0, 1, 0, 
       1, 60, 1) 
rclmat <- matrix(m, ncol=3, byrow=TRUE)

out_conserve_simple <-  classify(out_conserve, rclmat, include.lowest=TRUE)
out_conserve_simple[is.na(out_conserve_simple[])] <- 0 
plot(out_conserve_simple)

# maintain 
# 1         53 - Very High ecological feaure + vmoderate threat - maintain 
# 1         44 - Very High ecological feaure + vmoderate threat - maintain 
# 1         43 - Very High ecological feaure + vmoderate threat - maintain 
# 2         34  - Moderate ecological feaure + very low  threat - maintain 

out_maintain <- subst(out , c(53, 44, 43, 34), c(53, 44, 43, 34), others=NA)
m <- c(0, 1, 0, 
       1, 60, 2) 
rclmat <- matrix(m, ncol=3, byrow=TRUE)
out_maintain_simple <-  classify(out_maintain, rclmat, include.lowest=TRUE)
out_maintain_simple[is.na(out_maintain_simple[])] <- 0 
plot(out_maintain_simple)

# restore 

#          45  - High ecological feaure + very high threat - restore 
#          35  - Moderate ecological feaure + very high threat - restore 
# 8        54  - Very High ecological feaure + high threat - restore 
# 9        55  - Very High ecological feaure + very high threat - restore 

out_restore <- subst(out , c(45, 54, 55, 35), c(45, 54, 55, 35), others=NA)
plot(out_restore)
m <- c(0, 1, 0, 
       1, 60, 3) 
rclmat <- matrix(m, ncol=3, byrow=TRUE)
out_restore_simple <-  classify(out_restore, rclmat, include.lowest=TRUE)
out_restore_simple[is.na(out_restore_simple[])] <- 0 
plot(out_maintain_simple)


out_simple <- out_restore_simple + out_maintain_simple + out_conserve_simple
# 3 + 2 + 1

plot(out_simple)
out_simple <- mask(out_simple , rtemp)


writeRaster(out_simple, fs::path(draft_out, "5_eco_threat_classed.tif"))

# note - might want to over lay the current parks over the top of this?













### Identify the linkages (Climate and connectivity)




# 
# 
# 
# # Overlay these areas 
# 
# ecor <- rast(file.path(draft_out , "1_eco_focal_class.tif"))
# ecop <- as.polygons(ecor)
# ecop <- st_as_sf(ecop) |> 
#   rename("eco_class" = focal_mean)
# 
# thr <- rast(file.path(draft_out, "2_threat_focal_class.tif"))
# thp <- as.polygons(thr)
# thp <- st_as_sf(thp)|> 
#   rename("thr_class" = focal_mean)
# 
# # intersect to determine the overals
# ecoth <- st_intersection(thp, ecop)
# ecoth <- st_buffer(ecoth, 0)
# #ecoth <- st_make_valid(ecoth)
# #ecoth$area = st_area(ecoth)
# 
# st_write(ecoth, path(draft_out, "test_intersect2.gpkg"))
# 
# # summary of all the combinations 
# 
# ecoth$area <- as.numeric(st_area(ecoth)/10000)
# 
# 
# 
# 
# 
# # In general the national parks / protected areas are low human impact (not always)
# 
# # rehabilitation 
# # where are the high value areas with high human pressure 
# 
# # high priority
# heht <- ecoth |> 
#   filter(eco_class == 4) |> 
#   filter(thr_class == 4)
# 
# # moderate priority
# meht <- ecoth |> 
#   filter(eco_class == 3) |> 
#   filter(thr_class == 4)
# 
# # low prioirty 
# meht <- ecoth |> 
#   filter(eco_class == 2) |> 
#   filter(thr_class == 4)
# 
# 
# # restore 
# # where are the high value areas with moderate human pressure
# 
# hemt <- ecoth |> 
#   filter(eco_class == 4) |> 
#   filter(thr_class %in% c(2,3))
# 
# memt <- ecoth |> 
#   filter(eco_class == 3) |> 
#   filter(thr_class %in% c(2,3))
# 
# 
# # conserve 
# # high value areas with low human pressure 
# 
# helt <- ecoth |> 
#   filter(eco_class == 4) |> 
#   filter(thr_class == 1)
# 
# melt <- ecoth |> 
#   filter(eco_class == 3) |> 
#   filter(thr_class == 1)
# 
# 
# 
# # analysis of proposed area..... omniscape 
# 
# # level of protection ? 
# 
# # add level of 
# ecoth <- ecoth |> 
#   mutate(type = case_when(
#     thr_class == 4 ~ "rehabilitate",
#     thr_class == 2 ~ "restore",
#     thr_class == 3 ~ "restore",
#     thr_class == 1 ~ "conserve"
#   ))
# 
# ecoth <- ecoth |> 
#   mutate(priority = case_when(
#     eco_class == 4 ~ "high",
#     eco_class %in% c(2,3) ~ "medium",
#     eco_class == 1 ~ "low"
#   )) |> 
#   rowwise() |> 
#   mutate(type_prio = paste0(type, "_",priority))
# 
# 
# st_write(ecoth, path(draft_out, "test_intersect3.gpkg"))
# 
# 
# ## Identify high ecological value not protected with high threat (Rehabilitate)
# 
# 
# 
# 
# 
# 
# 
# 
