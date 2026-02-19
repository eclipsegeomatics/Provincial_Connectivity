## Testing file for the calculation of ecological features 


# required.... base raster (with buffer?)
library(fs)
library(terra)
library(sf)
#install.packages("remotes")
#remotes::install_github("bcgov/bcmaps")
#library(bcmaps)
library(dplyr)

data_dir <- fs::path("Required_Spatial_Files")
eco_dir <- fs::path(data_dir, "04_ecological_data")

out_dir <- fs::path("outputs")
draft_out <- fs::path(out_dir, "draft")

#rtemp_poly <- as.polygons(rtemp)
aoi <- st_read(path(data_dir, "AOI", "BC_Outline.gpkg")) |> 
  rename("aoi" = juri_en) |> 
  select(aoi)

# Loading raster and create a blank raster -------------------------------------------

rtemp <- rast(fs::path(data_dir, "05_geographical_data", "General Landscape Connectivity BC","Provincial scale", "cum_currmap_bc100m.tif"))
rtemp[rtemp > -1] <- 0

#rtemp_poly <- as.polygons(rtemp)

# read in geographic dataset and create raster stack
list.files(eco_dir)

# 1) red and blue listed species 
# filter steps - select only species listed as red and blue 
# scoring - Red = 1, blue = 2

rb <- st_read(path(eco_dir,  "BC Red and Blue Listed Species", "BIO_NS_SVW_polygon.gpkg" )) |> 
  filter(BC_LIST %in% c("Red", "Blue")) |> 
  mutate(list_sp_score = case_when(
    BC_LIST == "Red" ~ 2, 
    BC_LIST == "Blue" ~ 1
  ))

rb <- st_intersection(rb, aoi)

# convert to raster 
rbr <- rasterize(rb, rtemp, field ="list_sp_score", cover = FALSE, touches = TRUE)
rbr[is.na(rbr)]<- 0
rbr <- mask(rbr, rtemp)
writeRaster(rbr, path(draft_out, "1_redblue_species.tif"), overwrite = TRUE)


# 2) Endemism hotspots 
# filter by number of endemic species 
# score by number of endemic species 

#list.files(path(eco_dir, "Endemism Hotspots"))

eh <- st_read(path(eco_dir,  "Endemism Hotspots" , "CDN_Endemic_Hotspots.gpkg" )) |> 
  mutate(endemic_n = N_CDN/22) 

eh <- st_intersection(eh , aoi)
ehr <- rasterize(eh, rtemp, field ="endemic_n", cover = FALSE, touches = TRUE)
ehr[is.na(ehr)]<- 0
ehr <- mask(ehr, rtemp)

writeRaster(ehr , path(draft_out, "1_endemic_hotspots.tif"), overwrite = TRUE)


# 3) Key Biodiversity areas 
# score any area as KBA = 1
kb <- st_read(path(eco_dir, "Key Biodiversity Areas", "kba.20250623050652.gpkg" )) |> 
  mutate(kba = 1)
kb  <- st_intersection(kb  , aoi)

kbr <- rasterize(kb, rtemp, field ="kba", cover = FALSE, touches = TRUE)
kbr[is.na(kbr)]<- 0
kbr <- mask(kbr, rtemp)
writeRaster(kbr , path(draft_out, "1_kba.tif"), overwrite = TRUE)

# 
# # 4) Pacific Estuary ranking
# # using IMP class 2019 and converting not ranked to the lowest number 6?. Might need to reverse these values
# pe <- st_read(path(eco_dir, "Pacific Estuary Ranking", "PECP_estuary_polys_ranked_2019_PUBLIC.gpkg" )) |> 
#    mutate(pe_rank = IMP_CL2019) |> 
#    mutate(pe_rank = case_when(
#      pe_rank == "not ranked"~ 6, 
#      pe_rank %in% c("1", "2", "3", "4", "5") ~ as.numeric(pe_rank), 
#      .default = 0
#    ))
#    
# pe   <- st_intersection(pe  , aoi)
# per <- rasterize(pe, rtemp, field ="pe_rank", cover = FALSE, touches = TRUE)
# per[is.na(per)]<- 0
# per <- mask(per, rtemp)
# 
# writeRaster(per, path(draft_out, "1_pacific_estuary_ranking.tif"), overwrite = TRUE)

#unique(pe$pe_rank)



# 5) Priority places for species at risk
# using aoi 
#st_layers(path(eco_dir, "Priority Places for Species at Risk - Terrestrial", "PriorityPlaces.gpkg"))

pp <- st_read(path(eco_dir, "Priority Places for Species at Risk - Terrestrial", "PriorityPlaces.gpkg"), layer= "PriorityPlacesBoundary" ) |> 
  mutate(priority_place = 1) 
  
pp   <- st_intersection(pp  , aoi)
ppr <- rasterize(pp, rtemp, field ="priority_place", cover = FALSE, touches = TRUE)
ppr[is.na(ppr)]<- 0
ppr <- mask(ppr, rtemp)
writeRaster(ppr, path(draft_out, "1_priority_places.tif"), overwrite = TRUE)



# 6 Provincial Priority Old Growth Forests

#list.files(path(eco_dir,"Provincial Priority Old Growth Forests"))

# ancient forest 
an <- st_read(path(eco_dir,"Provincial Priority Old Growth Forests", "Ancient Forest","OGSR_TAF_polygon.gpkg")) 
an <- an |> 
  select(DESCR) |> 
  mutate(ancient = case_when(
    DESCR == "Ancient at older than 250 years" ~ 0.8,
    DESCR == "Ancient at older than 400 years" ~ 1,
    .default = 0
  ))
an  <- st_intersection(an , aoi)
anr <- rasterize(an, rtemp, field ="ancient", cover = FALSE, touches = TRUE)
anr[is.na(anr)]<- 0
anr <- mask(anr, rtemp)
writeRaster(anr, path(draft_out, "1_tap_ancientforest.tif"),overwrite = TRUE)


# 7) Big Trees
#list.files(path(eco_dir,"Provincial Priority Old Growth Forests","Big Trees"))

an <- st_read(path(eco_dir,"Provincial Priority Old Growth Forests", "Big Trees","OSGR_TBTO_polygon.gpkg")) 
#unique(an$DESCR)
an <- an |> 
  select(DESCR) |> 
  mutate(bigtree = case_when(
    DESCR == "Big-treed old growth" ~ 2,
    DESCR == "Big-treed older mature forest" ~ 1,
    .default = 0
  ))
an  <- st_intersection(an, aoi)
anr <- rasterize(an, rtemp, field ="bigtree", cover = FALSE, touches = TRUE)
anr[is.na(anr)]<- 0
anr <- mask(anr, rtemp)

writeRaster(anr, path(draft_out, "1_tap_bigtrees.tif"), overwrite = TRUE)

#

# 8) intact watersheds
#list.files(path(eco_dir,"Provincial Priority Old Growth Forests"))
#list.files(path(eco_dir,"Provincial Priority Old Growth Forests","Intact Watersheds" ))

an <- st_read(path(eco_dir,"Provincial Priority Old Growth Forests", "Intact Watersheds" ,"OGSR_TIW_polygon.gpkg")) 
#unique(an$DESCR)
#[1] "> 90% intact"    "70 - 80% intact" "80 - 90% intact"
an <- an |> 
  select(DESCR) |> 
  mutate(intactws = case_when(
    DESCR == "> 90% intact" ~ 3,
    DESCR == "70 - 80% intact" ~ 1,
    DESCR == "80 - 90% intact" ~ 2,
    .default = 0
  ))
an  <- st_intersection(an, aoi)
anr <- rasterize(an, rtemp, field ="intactws", cover = FALSE, touches = TRUE)
anr[is.na(anr)]<- 0
anr <- mask(anr, rtemp)
writeRaster(anr, path(draft_out, "1_tap_intactwatershed.tif"), overwrite = TRUE)



# 9) "Old Growth Technical Advisory Panel Old Forests" - unsure which layer this is??? 
# 
# list.files(path(eco_dir,"Provincial Priority Old Growth Forests"))
# list.files(path(eco_dir,"Provincial Priority Old Growth Forests","Old Growth Technical Advisory Panel Old Forests"))
# an <- st_read(path(eco_dir,"Provincial Priority Old Growth Forests", "Old Growth Technical Advisory Panel Old Forests" ,"OGSR_TOF_polygon.gpkg")) 


# 10) priority big trees 
#list.files(path(eco_dir,"Provincial Priority Old Growth Forests","Priority Big Trees"))
an <- st_read(path(eco_dir,"Provincial Priority Old Growth Forests", "Priority Big Trees" ,"OGSR_TPBTO_polygon.gpkg")) 
#head(an)
#unique(an$DESCR)
an <- an |> 
  select(DESCR) |> 
  mutate(prior_bigtree = case_when(
    DESCR == "Priority big-treed old growth" ~ 1,
    DESCR == "Priority big-treed older mature forest" ~ 0.8,
    .default = 0
  ))
an  <- st_intersection(an, aoi)
anr <- rasterize(an, rtemp, field ="prior_bigtree", cover = FALSE, touches = TRUE)
anr[is.na(anr)]<- 0
anr <- mask(anr, rtemp)
writeRaster(anr, path(draft_out, "1_tap_prioritybt.tif"), overwrite = TRUE)


# 11) ramsar sites 
# # note there are no sites in Canada 
# list.files(path(eco_dir,"RAMSAR Listed Wetlands and Others","RAMSAR Sites","Boundaries"))
# "RAMSAR Sites" 
# an <- st_read(path(eco_dir,"RAMSAR Listed Wetlands and Others", "RAMSAR Sites","Boundaries","features_publishedPolygon.gpkg")) 

# 12) lake denstiy 
list.files(path(eco_dir,"RAMSAR Listed Wetlands and Others","EAUBC Lakes" ))
an <- st_read(path(eco_dir,"RAMSAR Listed Wetlands and Others", "EAUBC Lakes" ,"EABC_LAKES_polygon.gpkg")) |> 
  select(WSA_TYPE) |> 
  filter(WSA_TYPE == "L") |> # drop the "x' types 
  mutate(lakes = 1)

# generate a density measure of lakes 
anr <- rasterize(an, rtemp, field ="lakes", cover = TRUE)
anr[is.na(anr)]<- 0 
anr <- mask(anr, rtemp)

writeRaster(anr, path(draft_out, "1_lake_density.tif"))





# 13) wetland density 

#list.files(path(eco_dir,"RAMSAR Listed Wetlands and Others","Freshwater Atlas Wetlands" ))
an <- st_read(path(eco_dir,"RAMSAR Listed Wetlands and Others", "Freshwater Atlas Wetlands" ,"FWWTLNDSPL_polygon.gpkg")) |> 
  mutate(wetland = 1)

# generate a density measure of lakes 
anr <- rasterize(an, rtemp, field ="wetland", cover = TRUE)
anr[is.na(anr)]<- 0 
anr <- mask(anr, rtemp)

writeRaster(anr, path(draft_out, "1_wetland_density.tif"))




# 12) Critical habitat Areas
list.files(path(eco_dir,"Species at Risk Critical Habitat","Critical Habitat for federally-listed species at risk", "CRTL_HAB_polygon.gpkg"))

sar <- st_read(path(eco_dir,"Species at Risk Critical Habitat","Critical Habitat for federally-listed species at risk", "CRTL_HAB_polygon.gpkg"))

an <- sar|> 
  select(CH_STAT) |> 
  mutate(sar_habitat = case_when(
    CH_STAT == "Final" ~ 2,
    CH_STAT == "Proposed" ~ 1,
    .default = 0
  ))
an  <- st_intersection(an, aoi)
anr <- rasterize(an, rtemp, field ="sar_habitat", cover = FALSE, touches = TRUE)
anr[is.na(anr)]<- 0 
anr <- mask(anr, rtemp)
writeRaster(anr, path(draft_out, "1_tap_sar_habitat.tif"), overwrite = TRUE)




# 13) Daust - 
#The attached raster has codes ranging from 1 to 3, showing the best 10, 20 and 30% of forest area. Sorry, I have not yet created final cores areas (based on density of best 30% and other ecological criteria).
#Methods
#The model creates ecosystem units by overlaying BGC variant (parkland, SWB scrubland and Alpine removed) with site class (site index classes of 0-5, 5-10, 10-15, 15-20 and 20+) within a modified FMLB.
#It calculates area and then 30% target area for each ecosystem unit.
#It selects areas on the landscape with the highest score until it reaches 30% of each ecosystem unit.
#Score is a function of human footprint (primary versus degraded forest) tree height, stand age and size of primary forest patches (Primary forest is always selected before degraded forest, then taller, older and bigger patches are better).

list.files(path(eco_dir,"Ecotrust_daust"))

rep <- st_read(path(eco_dir,"Ecotrust_daust","Decile_3005_20260217_repo.gpkg"))
rep <- rep |> 
  mutate(DN_code = case_when(
    DN == 1 ~ 3,
    DN == 2 ~ 2,
    DN == 3 ~ 1, 
    DN == 0 ~ 0
  ))

anr <- rasterize(rep, rtemp, field ="DN_code", cover = FALSE, touches = TRUE)
anr[is.na(anr)]<- 0 
anr <- mask(anr, rtemp)
writeRaster(anr, path(draft_out, "1_ecotrust_rep_veg.tif"), overwrite = TRUE)





# stack raster together and re-score with highest as 12 and lowest as 1

# potential to treat water bodies seperately from terrestrial 

tf <- list.files(path(draft_out), pattern = "^1_*")
tf <- tf[!tf %in% c("1_ecol_focal_51.tif", "1_ecol_focal_11.tif", '1_eco_focal_class.tif',"1_ecological_combined.tif","1_ecological_aquatic_combined.tif")]

out <- purrr::map(tf, function(i){
 # i <- tf[1]
  tt <- rast(path(draft_out, i))
  #tt[is.na(tt)]<- 0 
  #ttr <- mask(tt, rtemp) 
  tt
})

# Merge all the layers to combine 

combo <- out[[1]]+ out[[2]] + out[[3]]+ out[[4]]+ out[[5]] +out[[6]] + out[[7]]+
  out[[8]]+ out[[9]]+ out[[10]]+ out[[11]]+ out[[12]]+ out[[13]]

writeRaster(combo, path(draft_out, "1_ecological_combined_final.tif"))

#writeRaster(combo, path(draft_out, "1_ecological_aquatic_combined.tif"))



# generate focal metrics 

# read in the single combo (without aquatics)
#combo <-rast(path(draft_out,  "1_ecological_combined.tif"))


# smoother ecological areas at broad scale - 
#testing various scales

#try different scales for threats
#f7 <- focal(combo, 7, "mean", na.rm=TRUE) 
f11 <- focal(combo, 11, "mean", na.rm = TRUE)
writeRaster(f11, path(draft_out, "1_ecol_focal_11_final.tif"))
f23 <- focal(combo, 23, "mean", na.rm = TRUE)
writeRaster(f23, path(draft_out, "1_ecol_focal_23_final.tif"))
f51 <- focal(combo, 51, "mean", na.rm = TRUE)
writeRaster(f51, path(draft_out, "1_ecol_focal_51_final.tif"))


f75 <- focal(combo, 75, "mean", na.rm = TRUE)
writeRaster(f75, path(draft_out, "2_ecol_focal_75.tif"))
f101 <- focal(combo, 101, "mean", na.rm = TRUE)
writeRaster(f101, path(draft_out, "2_ecol_focal_101.tif"))    
f151 <- focal(combo, 151, "mean", na.rm = TRUE)
writeRaster(f151, path(draft_out, "2_ecol_focal_151.tif"))    
#f201 <- focal(combo, 201, "mean", na.rm = TRUE)
#writeRaster(f201, path(draft_out, "2_threat_focal_201.tif"))    

##|> mask(r)
