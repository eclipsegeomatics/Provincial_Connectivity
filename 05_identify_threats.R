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
gdb_path <- path('/home/user/Documents/00_data/base_vector/bc/BC_CEF_Human_Disturbance_2023/BC_CEF_Human_Disturbance_2023.gdb')

layers <- st_layers(gdb_path)
layer_name <- "BC_CEF_Human_Disturb_BTM_2023"

#head(hd)

hd <- st_read(gdb_path) |>
  filter(CEF_DISTURB_GROUP_RANK %in% c(1,2,3,4,5,6,7,8,9,10,11)) |>
  select(CEF_DISTURB_GROUP_RANK, CEF_DISTURB_SUB_GROUP_RANK)
#hd <- st_as_sf(hd)
# # write out 
st_write(hd, path(out_threat, "CE_HUMAN_DIST_2023_sub.gpkg"))
# 
# # fix geometry and write out clean copy
hd <- st_read(path(out_threat, "CE_HUMAN_DIST_2023_sub.gpkg"))
hd <- st_cast(hd, "MULTIPOLYGON")
st_write(hd, path(out_threat, "CE_HUMAN_DIST_2023_sub_fixgeom.gpkg"))



# read in the new fixed geom
hd <- st_read(path(out_threat, "CE_HUMAN_DIST_2023_sub_fixgeom.gpkg"))

ids <-sort(unique(hd$CEF_DISTURB_GROUP_RANK))

out <- purrr::map(ids, function(i){
  
  #i <- ids[1]
  hdi <- hd |> 
    filter(CEF_DISTURB_GROUP_RANK == i)
  
  hdr <- rasterize(hdi , rtemp, field ="CEF_DISTURB_GROUP_RANK", cover = FALSE, touches = TRUE)
  writeRaster(hdr , path(draft_out, paste0("3_threats_hd_", i,".tif")), overwrite = TRUE)
  
})

# 
# # stack raster together and re-score with highest as 12 and lowest as 1
# 
# tf <- list.files(path(draft_out), pattern = "^3_threat*")
# 
# out <- purrr::map(tf, function(i){
# 
#   #i <- tf[1]
#   tt <- rast(path(draft_out, i))
#   tval <- unique(tt$CEF_DISTURB_GROUP_RANK)
#   
#   # invert number values 
#   reval = 13 - tval 
#   
#   tt[(tt)>0]<- reval 
#   tt[is.na(tt)]<- 0 
#   
#   ttr <- mask(tt, rtemp) 
#   ttr
#   
# })

# for each of the types read in or creat the subcatergories


tf <- list.files(path(draft_out), pattern = "^3_threat*")


# 1) housing and urban # group 6
urban <- rast(path(draft_out, "3_threats_hd_6.tif"))
m <- c(0, 5, 0,
       5, 10, 100000)
rclmat <- matrix(m, ncol=3, byrow=TRUE)
rc1 <- classify(urban, rclmat, include.lowest=TRUE)
writeRaster(rc1 , path(draft_out, "3_threat_wt_urban.tif"), overwrite = TRUE)


# ## nighttime lights 
# list.files(fs::path(data_dir, "02_hirsh_pearson","Nighttime Lights" ))
# nl <- rast(fs::path(data_dir, "02_hirsh_pearson","Nighttime Lights","VNL_npp_2024_global_vcmslcfg_v2_c202502261200.average.dat_bc_extended_100m.tif" ))
# nlc <- crop(nl, rtemp)
# nlc <- resample(nlc,rtemp )
# nlc <- mask(nlc, rtemp)
# m <- c(0, 10, 0,
#        10, 1000, 1000)
# rclmat <- matrix(m, ncol=3, byrow=TRUE)
# rc1 <- classify(nlc, rclmat, include.lowest=TRUE)
# varnames(rc1) = "CEF_DISTURB_GROUP_RANK"
# writeRaster(rc1 , path(draft_out, "3_threat_wt_nighhtlights.tif"), overwrite = TRUE)


# 2) recreational 
rec <- rast(path(draft_out, "3_threats_hd_7.tif"))
m <- c(0, 5, 0,
       5, 10, 100)
rclmat <- matrix(m, ncol=3, byrow=TRUE)
rc1 <- classify(rec, rclmat, include.lowest=TRUE)
writeRaster(rc1 , path(draft_out, "3_threat_wt_rec.tif"), overwrite = TRUE)


# 3) agricultural 
ag <- rast(path(draft_out, "3_threats_hd_10.tif"))
m <- c(0, 5, 0,
       5, 999, 10000)
rclmat <- matrix(m, ncol=3, byrow=TRUE)
rc1 <- classify(ag, rclmat, include.lowest=TRUE)
plot(rc1)
writeRaster(rc1 , path(draft_out, "3_threat_wt_ag.tif"), overwrite = TRUE)


# Wood and pulp 

# cutblocks - redownload the raw data and catergorise then rasterize  
#https://catalogue.data.gov.bc.ca/dataset/b1b647a6-f271-42e0-9cd0-89ec24bce9f7

cb <- bcdata::bcdc_query_geodata("b1b647a6-f271-42e0-9cd0-89ec24bce9f7") |> 
  bcdata::select("HARVEST_START_YEAR_CALENDAR") |> 
  bcdata::collect()

cb <- cb |> 
  select(HARVEST_START_YEAR_CALENDAR) |> 
  mutate(yr_block = case_when(
    HARVEST_START_YEAR_CALENDAR >= 2006 ~ "0_20",
    HARVEST_START_YEAR_CALENDAR < 2006 & HARVEST_START_YEAR_CALENDAR > 1986 ~ "20-40",
    HARVEST_START_YEAR_CALENDAR < 1986  ~ "40+")) |> 
  mutate(CEF_DISTURB_GROUP_RANK = case_when(
    HARVEST_START_YEAR_CALENDAR >= 2006 ~ 1000,
    HARVEST_START_YEAR_CALENDAR < 2006 & HARVEST_START_YEAR_CALENDAR > 1986 ~ 100,
    HARVEST_START_YEAR_CALENDAR < 1986  ~ 100))

cb <- rasterize(cb , rtemp, field ="CEF_DISTURB_GROUP_RANK", cover = FALSE, touches = TRUE)
plot(cb)

writeRaster(cb, path(draft_out, "3_threat_wt_cutblocks.tif"))



# livestock and ranching

# rangelands
#https://catalogue.data.gov.bc.ca/dataset/10b1b187-1ef5-421f-8aa2-f716379fdb99

rl <- bcdata::bcdc_query_geodata("10b1b187-1ef5-421f-8aa2-f716379fdb99") |> 
  #bcdata::select("HARVEST_START_YEAR_CALENDAR") |> 
  bcdata::collect()

rl <-rl |> 
  filter(LIFE_CYCLE_STATUS_CODE == "ACTIVE") |> 
  mutate(CEF_DISTURB_GROUP_RANK = 100)

rlr <- rasterize(rl , rtemp, field ="CEF_DISTURB_GROUP_RANK", cover = FALSE, touches = TRUE)
plot(rlr)
writeRaster(rlr, path(draft_out, "3_threat_wt_cutblocks.tif"), overwrite = TRUE)



# oil and gas 
og <- rast(path(draft_out, "3_threats_hd_3.tif"))
m <- c(0, 2, 0,
       2, 999, 10000)
rclmat <- matrix(m, ncol=3, byrow=TRUE)
rc1 <- classify(og, rclmat, include.lowest=TRUE)
plot(rc1)
writeRaster(rc1 , path(draft_out, "3_threat_wt_oil.tif"), overwrite = TRUE)


# mining and quarry
mine<- rast(path(draft_out, "3_threats_hd_1.tif"))
m <- c(0, 0.5, 0,
       0.5, 999, 100000)
rclmat <- matrix(m, ncol=3, byrow=TRUE)
rc1 <- classify(mine, rclmat, include.lowest=TRUE)
plot(rc1)
writeRaster(rc1 , path(draft_out, "3_threat_wt_mining.tif"), overwrite = TRUE)


# roads 
# highway roads = 4, twolandmajor = 3, two land minor = 2, singleland minor = 1 
#list.files(out_threat)
roads <- rast(fs::path(out_threat, "roadsR.tif"))
m <- c(0, 1, 1000,
       1, 2, 10000,
       2,  3, 10000, 
       3, 4, 100000)
rclmat <- matrix(m, ncol=3, byrow=TRUE)
rc1 <- classify(roads, rclmat, include.lowest=TRUE)
rc11<- project(rc1, rtemp)

plot(rc1)
plot(roads)
writeRaster(rc11 , path(draft_out, "3_threat_wt_roads.tif"), overwrite = TRUE)


# rail and infrastructure 
rail <- rast(path(draft_out, "3_threats_hd_2.tif"))
m <- c(0, 0.5, 0,
       0.5, 999, 1000)
rclmat <- matrix(m, ncol=3, byrow=TRUE)
rc1 <- classify(rail, rclmat, include.lowest=TRUE)
plot(rc1)
writeRaster(rc1 , path(draft_out, "3_threat_wt_rail.tif"), overwrite = TRUE)


# utility and tranmission 
# transmission 

# group 4, subgroup 2 - transmision 
hd <- st_read(path(out_threat, "CE_HUMAN_DIST_2023_sub_fixgeom.gpkg"))

trans <- hd |> 
  filter(CEF_DISTURB_GROUP_RANK == 4) |> 
  filter(CEF_DISTURB_SUB_GROUP_RANK ==2 ) |> 
  mutate(CEF_DISTURB_GROUP_RANK = 100)

trans <- rasterize(trans , rtemp, field ="CEF_DISTURB_GROUP_RANK", cover = FALSE, touches = TRUE)
writeRaster(trans, path(draft_out, "3_threat_wt_trans.tif"), overwrite = TRUE)



# ROW 
row<- rast(path(draft_out, "3_threats_hd_5.tif"))
m <- c(0, 0.5, 0,
       0.5, 999, 1)
rclmat <- matrix(m, ncol=3, byrow=TRUE)
rc1 <- classify(row, rclmat, include.lowest=TRUE)
plot(rc1)
writeRaster(rc1 , path(draft_out, "3_threat_wt_row.tif"), overwrite = TRUE)


# thlb 

th <- st_read(path('/home/user/Documents/00_data/base_vector/bc/THLB/thlb_fixedgeom.gpkg'))
tht <- th |> 
  select(is_active) |> 
  mutate(CEF_DISTURB_GROUP_RANK = 100)

thr <- rasterize(tht  , rtemp, field ="CEF_DISTURB_GROUP_RANK", cover = FALSE, touches = TRUE)
m <- c(0, 999, 0,
       999, 2000, 100)
rclmat <- matrix(m, ncol=3, byrow=TRUE)
rc1 <- classify(thr, rclmat, include.lowest=TRUE)
plot(rc1)
writeRaster(rc1 , path(draft_out, "3_threat_wt_thlb.tif"), overwrite = TRUE)





# Fires 
#https://catalogue.data.gov.bc.ca/dataset/22c7cb44-1463-48f7-8e47-88857f207702

fi <- bcdata::bcdc_query_geodata("22c7cb44-1463-48f7-8e47-88857f207702") |> 
  bcdata::select("FIRE_YEAR") |> 
  bcdata::collect()

#head(fi)

fi <- fi |> 
  filter(FIRE_YEAR >= 2006) |> 
  mutate(CEF_DISTURB_GROUP_RANK = 10) |> 
  select(CEF_DISTURB_GROUP_RANK)

fir <- rasterize(fi , rtemp, field ="CEF_DISTURB_GROUP_RANK", cover = FALSE, touches = TRUE)
plot(fir)
writeRaster(fir, path(draft_out, "3_threat_wt_fires.tif"), overwrite = TRUE)


# group 4 subgroup 1 = Dam
hd <- st_read(path(out_threat, "CE_HUMAN_DIST_2023_sub_fixgeom.gpkg"))

dams <- hd |> 
  filter(CEF_DISTURB_GROUP_RANK == 4) |> 
  filter(CEF_DISTURB_SUB_GROUP_RANK ==1 ) |> 
  mutate(CEF_DISTURB_GROUP_RANK = 10000)

dams <- rasterize(dams , rtemp, field ="CEF_DISTURB_GROUP_RANK", cover = FALSE, touches = TRUE)
writeRaster(dams, path(draft_out, "3_threat_wt_dams.tif"))






# # read in the raster together and re-score with highest as 12 and lowest as 1
# 
tf <- list.files(path(draft_out), pattern = "^3_threat_wt_*")
# 
out <- purrr::map(tf, function(i){

  #i <- tf[1]
  tt <- rast(path(draft_out, i))
  #tval <- unique(tt$CEF_DISTURB_GROUP_RANK)

 })


# Merge all the layers to combine 
combo <- c( out[[1]],  out[[2]], out[[3]],out[[4]], out[[5]] , out[[6]], out[[7]], out[[8]], out[[9]], out[[10]], out[[11]],  out[[12]], out[[13]] )
mcombo <- max(combo, na.rm = TRUE)
mcombo <- mask(mcombo, rtemp)

writeRaster(mcombo, path(draft_out, "3_threat_combined_wt_max_raw.tif"))
mcombo <- rast(path(draft_out, "3_threat_combined_wt_max.tif"))

# reclass into catergories (merge 1 and 10)

m <- c(0, 0.9, 0,
       0.9, 99, 1,
       99, 999, 2, 
       999, 9999, 3, 
       9999, 99999, 4,
       99999, Inf,  5)

rclmat <- matrix(m, ncol=3, byrow=TRUE)
rc1 <- classify(mcombo, rclmat, include.lowest=TRUE)
plot(rc1)
rc1[is.na(rc1)] <- 0
writeRaster(rc1 , path(draft_out, "3_threat_combined_wt_max_class.tif"), overwrite = TRUE)

#plot(rc1)
#unique(values(rc1))




##############################################################################
# read in the watershed - Not quite working correctly 
# 
# wshd <- 
#   bcdata::bcdc_query_geodata("WHSE_LAND_USE_PLANNING.RMP_PLAN_NON_LEGAL_POLY_SVW") |> 
#   bcdata::select("id") |> 
#   bcdata::collect()
# st_write(wshd, path(draft_out, "watersheds_full.gpkg"))
# 
# 
# 
# wshd <- wshd |> 
#   select(id)
# st_write(wshd, path(draft_out, "watersheds.gpkg"))
# 
# 
# #assign corridor/climate matrix values to make new raster
# #Read in files and prep
# wshd1 <- wshd |> 
#   mutate(wshd_id=as.numeric(rownames(wshd))) |> 
#   mutate(area_Ha=units::set_units(st_area(wshd),ha)) |> 
#   select("wshd_id","area_Ha") #|> 
#   #units::drop_units(wshd) |> 
#   #dplyr::select(wshd_id,area_Ha)
# st_write(wshd1, path(draft_out, "watersheds.gpkg"))
# 
# 
# wshdv<-vect(wshd1)
# 
# CC_c<- mcombo
# 
# #Get modal value in watershed
# get_mode <- function(x, na.rm = TRUE) {
#   if (na.rm) {
#     x <- stats::na.omit(x)
#   }
#   # Calculate mode for a vector of values
#   ux <- unique(x)
#   ux[which.max(tabulate(match(x, ux)))]
# }
# 
# #First modal of climate connectivity
# CC_wshd <- terra::extract(CC_c, wshdv, fun = get_mode, na.rm = TRUE)
# CC_wshd 
# 
# # up to here
# 
# 
# 
# wshd1$CC_wshd <- CC_wshd[,2] # Column 2 is the result, ID maps to row id
# 
# CCCls_LUT<-data.frame(CC_wshd=c(1,2,3,4,5),CCClass=c('VeryLow','Low','Mod','High','VeryHigh'))
# wshd_cc<-wshd1 %>%
#   left_join(CCCls_LUT)
# write_sf(wshd_cc, file.path(draft_out,'wshd_threat_values.gpkg'), overwrite=TRUE)
# 




## old version 

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




