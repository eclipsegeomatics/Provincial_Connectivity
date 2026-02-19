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
eco <- rast(fs::path(draft_out, "1_ecol_focal_51_final.tif"))
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

# reclass the raster 
m <- c(0, 5.967814, 1,
       5.967814, 11.799644 , 2,
       11.799644, 17.184259, 3,
       17.184259, 22.865948, 3,
       22.865948,  60, 5) # highest eco value

rclmat <- matrix(m, ncol=3, byrow=TRUE)
eco_class <- classify(eco, rclmat, include.lowest=TRUE)

eco_class[is.na(eco_class)]<- 0
eco_class <- mask(eco_class, rtemp)

writeRaster(eco_class, file.path(draft_out , "1_eco_focal_class_final.tif"), overwrite = TRUE)





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
# plot
out_simple <- rast(fs::path(draft_out, "5_eco_threat_classed.tif"))
plot(out_simple)
# note - might want to over lay the current parks over the top of this?



# output file = "5_eco_threat_classed.tif"






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

# Matrix area - version 1 -

cc_class <- rast(fs::path(draft_out, "4_climate_corr_class.tif"))

con <- rast(fs::path(draft_out, "3_cons_corr_class.tif"))


# identity high priority climate orridors - keep top 3 groupings 

# climate keep very important / important areas /moderare (3,4,5 values)
m <- c(0, 2, 0, # lowest diversity 
       2, 3, 3,
       3, 4, 4,
       4, 5, 5) # highest diversity 

rclmat <- matrix(m, ncol=3, byrow=TRUE)

cc_cor <- classify(cc_class, rclmat, include.lowest=TRUE)
cc_cor <- cc_cor*10 

# overlay the climate and connectivity corridors 
out <- cc_cor + con  
plot(out)

writeRaster(out, fs::path(draft_out, "6_cli_cor_corridor_class.tif"))

# subset only the corrodor classes
out_cor <- subst(out , c(55, 54, 53, 52, 45, 44, 43, 42, 35, 34, 33, 32 ),  c(55, 54, 53, 52, 45, 44, 43, 42, 35, 34, 33, 32 ), others=0)

writeRaster(out_cor, fs::path(draft_out, "6_corridor_class_toprated.tif"), overwrite = TRUE)


#############################################################################

# Overlay this layer with the classes ecology-threat layer 
out_cor <- rast(fs::path(draft_out, "6_corridor_class_toprated.tif"))

et <- rast(fs::path(draft_out, "5_eco_threat_classed.tif"))
et <- et*100

# overlay the et with corridor top rated..
all <- c(out_cor, et)
# #all1 <- et + out_cor
all <- app(all, fun = "sum", na.rm = TRUE)
plot(all)

#sort(unique(values(all)))
writeRaster(all, fs::path(draft_out, "7_eco_th_corridor_classed.tif"), overwrite = TRUE)

###
# split out into components for mapping (ignorning the connectivity piece)

vhigh <-  c(352, 353, 354, 355, 342, 343, 344, 345, 252, 253, 254, 255)
high <- c(152, 153, 154, 155, 232, 233, 234, 235, 242, 243, 244, 245, 142, 143, 144, 145,332, 333, 334, 335)
mod <- c(52, 53,  54,  55,  42,  43,  44,  45, 132, 133, 134, 135, 300, 200)
low <- c(32, 33,  34,  35, 100)

# climate keep very important / important areas /moderare (3,4,5 values)

# very high 
rvhigh <- subst(all , vhigh, vhigh, others=0)
m <- c(0, 2, 0, # lowest diversity 
       2, 360, 1) # highest diversity 
rclmat <- matrix(m, ncol=3, byrow=TRUE)
rvhigh <- classify(rvhigh, rclmat, include.lowest=TRUE)
rvhigh <- mask(rvhigh , rtemp)

# high 
rhigh <- subst(all , high, high, others=0)
m <- c(0, 2, 0, # lowest diversity 
       2, 360, 2) # highest diversity 
rclmat <- matrix(m, ncol=3, byrow=TRUE)
rhigh <- classify(rhigh, rclmat, include.lowest=TRUE)
rhigh <- mask(rhigh , rtemp)
plot(rhigh)

# moderate
rmod <- subst(all , mod, mod, others=0)
m <- c(0, 2, 0, # lowest diversity 
       2, 360, 3) # highest diversity 
rclmat <- matrix(m, ncol=3, byrow=TRUE)
rmod <- classify(rmod, rclmat, include.lowest=TRUE)
rmod <- mask(rmod , rtemp)
plot(rmod)

# low 
rlow <- subst(all , low, low, others=0)
m <- c(0, 2, 0, # lowest diversity 
       2, 360, 4) # highest diversity 
rclmat <- matrix(m, ncol=3, byrow=TRUE)
rlow <- classify(rlow, rclmat, include.lowest=TRUE)
rlow <- mask(rlow , rtemp)

# merge back together

all_cat <- rlow + rmod + rhigh+rvhigh

plot(all_cat)


writeRaster(all_cat, fs::path(draft_out, "7_eco_th_corridor_rank.tif"), overwrite = TRUE)




# # Matrix area - version 2
# 
# cc_class <- rast(fs::path(draft_out, "4_climate_corr_class.tif"))
# con <- rast(fs::path(draft_out, "3_cons_corr_class.tif"))
# 
# cc_cor <- cc_class*10 
# out <- cc_cor + con  
# plot(out)
# 
# cc_class <- rast(fs::path(draft_out, "4_climate_corr_class.tif"))
# 
# # keep colour ranking based on table 
# # 55, 54, 53, 45, 44, 43, 35, 34 - dark green
# # 25, 24, 33 - light green 
# # 15, 23, 52, 42 - yellow 
# # 51, 32, 14, 13 - orange
# # 41, 31, 21, 22, 12, 11 - red 
# 
# 
# # out dark green 
# out_green <- subst(out , c(55, 54, 53, 45, 44, 43, 35, 34 ), c(55, 54, 53, 45, 44, 43, 35, 34), others=NA)
# m <- c(0, 1, 0, 
#        1, 60, 1) 
# rclmat <- matrix(m, ncol=3, byrow=TRUE)
# out_green_simple <-  classify(out_green, rclmat, include.lowest=TRUE)
# out_green_simple[is.na(out_green_simple[])] <- 0 
# plot(out_green_simple)
# 
# # out light green 
# out_lgreen <- subst(out , c(25, 24, 33 ), c(25, 24, 33 ), others=NA)
# m <- c(0, 1, 0, 
#        1, 60, 2) 
# rclmat <- matrix(m, ncol=3, byrow=TRUE)
# out_lgreen_simple <-  classify(out_lgreen, rclmat, include.lowest=TRUE)
# out_lgreen_simple[is.na(out_lgreen_simple[])] <- 0 
# plot(out_lgreen_simple)
# 
# # out yellow
# out_yellow <- subst(out , c(15, 23, 52, 42 ), c(15, 23, 52, 42 ), others=NA)
# m <- c(0, 1, 0, 
#        1, 60, 3) 
# rclmat <- matrix(m, ncol=3, byrow=TRUE)
# out_yellow_simple <-  classify(out_yellow , rclmat, include.lowest=TRUE)
# out_yellow_simple[is.na(out_yellow_simple[])] <- 0 
# plot(out_yellow_simple)
# 
# 
# 
# # restore 
# 
# #          45  - High ecological feaure + very high threat - restore 
# #          35  - Moderate ecological feaure + very high threat - restore 
# # 8        54  - Very High ecological feaure + high threat - restore 
# # 9        55  - Very High ecological feaure + very high threat - restore 
# 
# out_restore <- subst(out , c(45, 54, 55, 35), c(45, 54, 55, 35), others=NA)
# plot(out_restore)
# m <- c(0, 1, 0, 
#        1, 60, 3) 
# rclmat <- matrix(m, ncol=3, byrow=TRUE)
# out_restore_simple <-  classify(out_restore, rclmat, include.lowest=TRUE)
# out_restore_simple[is.na(out_restore_simple[])] <- 0 
# plot(out_maintain_simple)
# 
# 
# out_simple <- out_restore_simple + out_maintain_simple + out_conserve_simple
# # 3 + 2 + 1
# 
# plot(out_simple)
# out_simple <- mask(out_simple , rtemp)
# 
# 
# writeRaster(out_simple, fs::path(draft_out, "5_eco_threat_classed.tif"))
# 
# # note - might want to over lay the current parks over the top of this?
# 
# 
# 
# 


# final step is to overlay the layers 

### Identify the linkages (Climate and connectivity)

