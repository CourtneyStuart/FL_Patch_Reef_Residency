# associated GitHub repo: 
# https://github.com/CourtneyStuart/FL_Patch_Reef_Residency

#### LIBRARIES ####
# install packages (first run only)
# install.packages(c("easypackages", "sp", "sf", "raster", "tidyverse", "tidyr", 
#                    "dplyr", "tibble", "conflicted", "rvc", "rfishbase", "readxl",
#                    "purrr", "here", "ggplot2", "PNWColors", "corrplot",
#                    "Cairo", "usdm", "ISLR"))

# load libraries
library(easypackages)
libraries("sp", "sf", "raster", "tidyverse", "tidyr", "dplyr", "tibble",
          "conflicted", "rvc", "rfishbase", "readxl", "purrr", "here", 
          "ggplot2", "PNWColors", "corrplot", "Cairo", "usdm", "ISLR")
# prevent conflicts between packages
conflicted::conflict_prefer("select", "dplyr")
conflicted::conflict_prefer("filter", "dplyr")
conflicted::conflict_prefer("distinct", "dplyr")

# set seed to ensure reproducibility
set.seed(7)   

#### DIRECTORIES ####
# set working directory and relative project path
setwd("E:/Data/Florida/FL_Patch_Reef_Residency/")
# set_here() # set if using here() in this location for the first time
here::i_am(".here")
here::here() 

# save PROJ.4 string for standard projection (ESPG:26958 NAD 83/Florida East) 
my_crs = CRS("+proj=tmerc +lat_0=24.3333333333333 +lon_0=-81 +k=0.999941177 +x_0=200000 +y_0=0 +ellps=GRS80 +towgs84=-2,0,4,0,0,0,0 +units=m +no_defs +type=crs")

# save coordinat system for source EPSG:4326 WGS84 - World Geodetic System 1984
gcs = CRS("+proj=longlat +datum=WGS84 +no_defs +type=crs")

#### ADD PREDATOR DATA ####
# load reef visual census data for Florida Keys in years 2014, 2016, 2018
rvc = getRvcData(years = c(2014, 2016, 2018), regions = "FLA KEYS")

# separate taxonomic, stratum, and sample data
rvc_tax = rvc$taxonomic_data
rvc_strat = rvc$stratum_data
rvc_samp = rvc$sample_data

# save list of predatory species that we're interested in
pred_spp = c("Caranx bartholomaei", "Caranx crysos", "Caranx ruber",
             "Seriola dumerili", "Mycteroperca phenax", "Haemulon parra",
             "Halichoeres bivittatus", "Lutjanus apodus", "Lutjanus buccanella",
             "Lutjanus cyanopterus", "Lutjanus griseus", "Lutjanus jocu", 
             "Lutjanus mahogoni", "Lutjanus synagris", "Ocyurus chrysurus",
             "Sphyraena barracuda", "Cephalopholis cruentata", "Cephalopholis fulva",
             "Epinephelus morio", "Epinephelus striatus", "Mycteroperca bonaci",
             "Mycteroperca microlepis", "Scorpaena plumieri", 
             "Aulostomus maculatus")

# extract taxonomic information for predatory species
pred_tax = rvc_tax %>%
  filter(SCINAME %in% pred_spp) %>%
  tibble::add_column(SOURCE = "South Florida Reef Visual Census")

# extract observations of these predators from the rvc sample data
pred_obs = rvc_samp %>%
  filter(SPECIES_CD %in% pred_tax$SPECIES_CD)

# use filter statement to extract predatory fishes that we assume to be
# of large enough body size to consume sub-adult gray snapper. Where the lower
# bound for sub-adult gray snapper size is 9.51 cm FL and predators can only 
# consume prey 40% of their size or smaller. 
lg_pred = pred_obs %>%
  filter(9.51 <= (LEN * 0.40))

# now calculate the abundance of each predatory species at each site
lg_pred_abun = lg_pred %>%
  group_by(REGION, STRAT, PRIMARY_SAMPLE_UNIT, STATION_NR) %>%
  mutate(LON_DEGREES = mean(LON_DEGREES),
         LAT_DEGREES = mean(LAT_DEGREES)) %>%
  ungroup() %>%
  group_by(REGION, STRAT, PRIMARY_SAMPLE_UNIT, STATION_NR,
           LON_DEGREES, LAT_DEGREES, SPECIES_CD) %>%
  mutate(N = sum(NUM)) %>% 
  select(REGION, STRAT, PRIMARY_SAMPLE_UNIT, STATION_NR,
         LON_DEGREES, LAT_DEGREES, SPECIES_CD, N) %>%
  distinct() # how many of each predatory species were seen at each SSU?

# now, calculate the overall abundance for all predators (N_PRED). 
# finally, calculate the overall predator density at each second-stage sampling 
# unit as N_PRED / 177m2 (station survey area)
lg_pred_dens = lg_pred_abun %>%
  ungroup() %>%
  select(REGION, STRAT, PRIMARY_SAMPLE_UNIT, STATION_NR, LON_DEGREES,
         LAT_DEGREES, N) %>%
  distinct() %>%
  group_by(REGION, STRAT, PRIMARY_SAMPLE_UNIT, STATION_NR, LON_DEGREES,
           LAT_DEGREES) %>%
  mutate(N_PRED = sum(N)) %>%
  mutate(DEN_PRED = (N_PRED/177)) %>%
  select(REGION, STRAT, PRIMARY_SAMPLE_UNIT, STATION_NR, LON_DEGREES,
         LAT_DEGREES, N_PRED, DEN_PRED) %>%
  distinct()

# repeat for bluestriped grunts
# use filter statement to extract predatory fishes that we assume to be
# of large enough body size to consume sub-adult bluestriped grunt. Where the lower
# bound for sub-adult bluestriped grunt size is 11.90 cm FL and predators can only 
# consume prey 40% of their size or smaller. 
hs_pred = pred_obs %>%
  filter(11.90 <= (LEN * 0.40))

# now calculate the abundance of each predatory species at each site
hs_pred_abun = hs_pred %>%
  group_by(REGION, STRAT, PRIMARY_SAMPLE_UNIT, STATION_NR) %>%
  mutate(LON_DEGREES = mean(LON_DEGREES),
         LAT_DEGREES = mean(LAT_DEGREES)) %>%
  ungroup() %>%
  group_by(REGION, STRAT, PRIMARY_SAMPLE_UNIT, STATION_NR,
           LON_DEGREES, LAT_DEGREES, SPECIES_CD) %>%
  mutate(N = sum(NUM)) %>% 
  select(REGION, STRAT, PRIMARY_SAMPLE_UNIT, STATION_NR,
         LON_DEGREES, LAT_DEGREES, SPECIES_CD, N) %>%
  distinct() # how many of each predatory species were seen at each SSU?

# now, calculate the overall abundance for all predators (N_PRED). 
# finally, calculate the overall predator density at each second-stage sampling 
# unit as N_PRED / 177m2 (station survey area)
hs_pred_dens = hs_pred_abun %>%
  ungroup() %>%
  select(REGION, STRAT, PRIMARY_SAMPLE_UNIT, STATION_NR, LON_DEGREES,
         LAT_DEGREES, N) %>%
  distinct() %>%
  group_by(REGION, STRAT, PRIMARY_SAMPLE_UNIT, STATION_NR, LON_DEGREES,
           LAT_DEGREES) %>%
  mutate(N_PRED = sum(N)) %>%
  mutate(DEN_PRED = (N_PRED/177)) %>%
  select(REGION, STRAT, PRIMARY_SAMPLE_UNIT, STATION_NR, LON_DEGREES,
         LAT_DEGREES, N_PRED, DEN_PRED) %>%
  distinct()

# now we need to account for sites where predators meeting these size requirements
# were NOT seen.

# convert density data from tabular data frame to spatial data object
sf_lg_pred_dens = lg_pred_dens %>%
  ungroup() %>%
  mutate(nearest_id = 1:n()) %>%
  st_as_sf(., coords = c(5,6), crs = gcs) %>%
  st_transform(., my_crs) %>%
  add_column(LON_M = unlist(map(.$geometry,1))) %>%
  add_column(LAT_M = unlist(map(.$geometry,2)))

# read in our sub-adult gray snapper patch reef data (from ArcGIS Pro / ArcPython)
lg = read.csv(here("Data", "Model_Data",
                   "Subadult_Gray_Snapper_Patch_Reef_Data.csv"))

# convert to spatial data
sf_lg = lg %>%
  st_as_sf(., coords = c(11, 12), crs = my_crs) %>%
  add_column(LON_M = unlist(map(.$geometry,1))) %>%
  add_column(LAT_M = unlist(map(.$geometry,2)))

# Step 1: Find the nearest feature ID and distance for each point in `sf_lg`
sf_lg = sf_lg %>%
  mutate(
    nearest_id = st_nearest_feature(geometry, sf_lg_pred_dens$geometry),  # index of the nearest feature
    distance = st_distance(geometry, sf_lg_pred_dens$geometry[nearest_id], by_element = TRUE))  # distance to nearest point)

# Step 2: Set nearest points beyond 15 meters to NA
sf_lg = sf_lg %>%
  mutate(nearest_id = ifelse(distance <= units::set_units(15, "m"), nearest_id, NA))  # keep ID only if within 15m

# Step 3: Extract columns from `sf_lg_pred_dens` by nearest ID, fill NAs with 0s
sf_lg_pred_dens = sf_lg_pred_dens %>% select(nearest_id, N_PRED, DEN_PRED)

sf_lg = left_join(st_drop_geometry(sf_lg), 
                 st_drop_geometry(sf_lg_pred_dens), 
                 by = c("nearest_id" = "nearest_id")) %>%
  mutate(N_PRED = ifelse(is.na(N_PRED), 0, N_PRED),   # fill NAs with 0
         DEN_PRED = ifelse(is.na(DEN_PRED), 0, DEN_PRED)) %>%
  select(-nearest_id, -distance) # remove intermediate columns

# save result
write.csv(sf_lg,
          here("Data", "Model_Data", "Subadult_Gray_Snapper_Patch_Reef_Data_All.csv"),
          row.names = FALSE)

# repeat process for bluestriped grunts

# convert density data from tabular data frame to spatial data object
sf_hs_pred_dens = hs_pred_dens %>%
  ungroup() %>%
  mutate(nearest_id = 1:n()) %>%
  st_as_sf(., coords = c(5,6), crs = gcs) %>%
  st_transform(., my_crs) %>%
  add_column(LON_M = unlist(map(.$geometry,1))) %>%
  add_column(LAT_M = unlist(map(.$geometry,2)))

# read in our sub-adult bluestriped grunt patch reef data (from ArcGIS Pro / ArcPython)
hs = read.csv(here("Data", "Model_Data",
                   "Subadult_Bluestriped_Grunt_Patch_Reef_Data.csv"))

# convert to spatial data
sf_hs = hs %>%
  st_as_sf(., coords = c(11, 12), crs = my_crs) %>%
  add_column(LON_M = unlist(map(.$geometry,1))) %>%
  add_column(LAT_M = unlist(map(.$geometry,2)))

# Step 1: Find the nearest feature ID and distance for each point in `sf_hs`
sf_hs = sf_hs %>%
  mutate(
    nearest_id = st_nearest_feature(geometry, sf_hs_pred_dens$geometry),  # index of the nearest feature
    distance = st_distance(geometry, sf_hs_pred_dens$geometry[nearest_id], by_element = TRUE))  # distance to nearest point)

# Step 2: Set nearest points beyond 15 meters to NA
sf_hs = sf_hs %>%
  mutate(nearest_id = ifelse(distance <= units::set_units(15, "m"), nearest_id, NA))  # keep ID only if within 15m

# Step 3: Extract columns from `sf_hs_pred_dens` by nearest ID, fill NAs with 0s
sf_hs_pred_dens = sf_hs_pred_dens %>% select(nearest_id, N_PRED, DEN_PRED)

sf_hs = left_join(st_drop_geometry(sf_hs), 
                  st_drop_geometry(sf_hs_pred_dens), 
                  by = c("nearest_id" = "nearest_id")) %>%
  mutate(N_PRED = ifelse(is.na(N_PRED), 0, N_PRED),   # fill NAs with 0
         DEN_PRED = ifelse(is.na(DEN_PRED), 0, DEN_PRED)) %>%
  select(-nearest_id, -distance) # remove intermediate columns

# save result
write.csv(sf_hs,
          here("Data", "Model_Data", "Subadult_Bluestriped_Grunt_Patch_Reef_Data_All.csv"),
          row.names = FALSE)

#### CHECK CORRELATION ####
# work with the full datasets moving forward
lg = sf_lg
hs = sf_hs

# starting with gray snapper
# str lets you preview data and column types/formats
str(lg)

# first, let's do some data prep and cleaning to make the data frame easier
# to work with
lg = lg %>%
  # starting with some renaming of columns (where new name = old name)
  rename(Species = SPECIES_CD, # species
         Life_Stage = LIFE_STAGE, # life stage
         Presence = PRES, # binary presence (1 = yes, 0 = no)
         Presence_Category = PRES2, # character presence (present or absent)
         Abundance = N, # fish counts (averaged over two buddy divers at site)
         Long_M = LON_M, # longitude of survey (meters)
         Lat_M = LAT_M, # latitude of survey (meters)
         Patch_Area = AREA, # patch reef area (square meters)
         Patch_Perimeter = PERIMETER, # patch reef perimeter (meters)
         Patch_Neigh_Dist = NEAR_DIST, # for each patch reef, distance to nearest neighbor patch reef (meters)
         Area_CRHB = Coral_Reef_and_Hardbottom, # coral reef and hard bottom area in 500 m buffer (square meters)
         Area_SG = Seagrass, # seagrass area in 500 m buffer (square meters)
         Area_US = Unconsolidated_Sediment, # unconsolidated sediment area in 500 m buffer (square meters)
         Pred_Abundance = N_PRED, # predator abundance
         Pred_Density = DEN_PRED) %>%  # predator density
  # let's make sure that our response presence/absence column is a factor
  mutate(Presence = as.factor(Presence),
         Presence_Category = as.factor(Presence_Category)) %>%
  # add new column storing the area sampled by each reef fish survey,
  # where area = pie*radius^2 --> pie*7.5m^2 --> ~177 m^2
  mutate(Survey_Area = rep(as.numeric(177), nrow(.))) %>%
  # add new column storing the density of sub-adult gray snapper,
  # where density = abundance/area of sampling = fish per m^2
  mutate(Density = Abundance/Survey_Area) %>%
  # now keep only the columns we need, the order here will determine column order
  # in the output dataframe 
  select(Patch_ID, Survey_to_Reef_Dist, Species, Life_Stage, Presence, 
         Presence_Category, Abundance, Density, Survey_Area, Long_M, Lat_M,
         Habitat, Mangrove_Dist, Reef_Dist, Depth, StDev_Depth, Slope, 
         Curvature, Plan_Curve, BPI_Fine, BPI_Broad, Rugosity, Mean_Sum_Temp,
         Mean_Sum_DO, Mean_Sum_Sal, Mean_Win_Temp, Mean_Win_DO, Mean_Win_Sal,
         Patch_Area, Patch_Perimeter, PA_Ratio, Patch_Neigh_Dist, Area_CRHB,
         Area_SG, Area_US, Pred_Abundance, Pred_Density)

# sort data by increasing latitude and then add a NEW object_id column based on 
# this sorting (later, we will repeat this for bluestriped grunt and use the 
# object_ids to extract data from the same training and testing sites as gray snapper)
lg = lg[order(lg$Lat_M),]
lg$Object_ID = seq.int(nrow(lg))

# keep only one survey per unique patch reef (so as to not over-represent any
# environmental conditions at patch reefs that had multiple surveys around them)
lg = lg %>% 
  group_by(Patch_ID) %>% 
  sample_n(1) %>%
  ungroup()
lg = as.data.frame(lg)

# set plotting margins
par(mar = c(2.5, 2.5, 2.5, 2.5))

# some variables have only a small range with little variation. rugosity is
# a great example. this likely won't be a very helpful predictor, at least
# not at the spatial resolution of our data (5 x 5 m)...
summary(lg$Rugosity)
hist(lg$Rugosity) 

# curvature and plan curvature are also relatively homogeneous across patch reefs.
# although curvature influences the acceleration/deceleration and direction of
# benthic flow, this may not be as important for mobile sub-adult gray snapper 
# and bluestriped grunt that are feeding on mobile invertebrates and small fishes
# in seagrass, and that are protected from exposure by patch reefs. 
summary(lg$Curvature)
hist(lg$Curvature)
summary(lg$Plan_Curve)
hist(lg$Plan_Curve)

# bathymetric position index (BPI) represents the depth of a cell relative to its
# surroundings, where (-) values indicate valleys and depressions, values 
# near 0 represent flat areas or constant slopes, and (+) values represent
# peaks and summits. for my MSc, BPI was evaluated at two spatial scales across 
# the whole Florida Key's seascape:

# broad-scale BPI was evaluated using concentric rings of 125 m and 1250 m
hist(lg$BPI_Broad)

# fine-scale BPI was evaluated using concentric rings of 5 m and 125 m
hist(lg$BPI_Fine)

# based on Courtney Stuart's first chapter, which modeled habitat suitability 
# for sub-adult gray snapper and bluestriped grunt across the seascape, 
# broad-scale BPI was a much stronger predictor of suitability than fine-scale
# BPI. also, as we can see in these histograms, fine-scale BPI takes on only a 
# few values when examined for patch reefs only. this is because patch reefs
# within concentric rings of 5 m and 125 m likely have very similar depth
# values. For these reasons, we will retain broad-scale BPI. 

# finally, some variables can be explained using a combination of others, for 
# example increasing temperature and/or salinity results in lower dissolved oxygen. 
# salinity and temperature data are also easier to collect/access. Also, previous
# research has revealed significant inter-species variation in response to 
# salinity and temperature, so these are of potential ecological relevance to 
# our patch reef study. 

# let's exclude some variables and create a new data frame of only those
# variables that we'd like to consider for modeling
lg_vars = lg %>%
  # where (-) lets you deselect a column
  select(12:37, -Curvature, -Plan_Curve, -Rugosity, -BPI_Fine,
         -Mean_Sum_DO, -Mean_Win_DO, -Pred_Abundance) # we'll focus on predator density

# let's check to make sure all the variable columns are of the numeric type
str(lg_vars)
lg_vars = as.data.frame(lapply(lg_vars, as.numeric))

# now we can check for correlation among these variables, where strong
# positive or negative correlation (> absolute 0.7) suggests that the two
# variables are collinear --> they will likely explain some of the same
# variance in the dependent variable if included together in the model. 
# collinearity (or multicollinearity, when more than two variables are 
# highly correlated) can distort model interpretations, coefficient 
# estimates, and levels of significance. as a rule of thumb, we'll use
# pearson pairwise correlation (r) threshold of |0.7| (abs). 

# first we'll create a full correlation matrix from all variables, including
# the two new ones (percent coral cover and hard relief) provided by the reviewer
lg_cormat = cor(lg_vars, use = "complete.obs")

# plot the full correlation matrix, this is the color palette we'll use
palette = pnw_palette("Shuksan2", 200, type = "continuous")

# set plotting margins
par(mar=c(0,0,0,0))

# full correlation plot
corrplot(lg_cormat, method = "color", col = palette, type = "upper",
         order = "original", addCoef.col = "black", number.cex = 0.50, 
         number.digits = 2, tl.col = "black", tl.srt = 40, tl.cex = 0.8)

# we could run through the correlation matrix by hand to find problematic
# variables...but instead we'll automate the process using a "usdm" function. 
# the vifcor function runs through all pairs of variables in the lg_vars
# data frame and checks whether any exceed the r threshold. if so, it will
# tell us where the problems lie.
require(usdm)
lg_corr = vifcor(lg_vars, th = 0.7)
lg_corr # check correlation results

# we can also use variance inflation factors (VIF) to assess multicollinearity.
# VIF measures how much the behavior (variance) of a variable is influenced
# by it's interaction with other variables. VIF allows a quick measure of how
# much a variable is contributing to the standard error in the regression. we
# want to keep standard errors as small as possible, so we will use a standard
# VIF threshold of 5.

# the vifstep function runs through all pairs of variables in the lg_vars
# data frame and checks whether any exceed the VIF threshold. if so, it will
# tell us where the problems lie.
lg_vif = vifstep(lg_vars, th = 5)
lg_vif # check VIF results

# these tests suggest that the standard deviation of depth variable needs
# to be removed due to collinearity. This is likely due to correlation with
# slope. let's check:
cor(lg_vars$StDev_Depth, lg_vars$Slope,  use = "complete")
# yes, they're extremely positively correlated. In this case, we will retain
# slope as suggested.

# what about the suggestion to remove patch perimeter? this is likely due to 
# correlation between patch area and patch area
cor(lg_vars$Patch_Area, lg_vars$Patch_Perimeter)
# once again, these variables display strong positive correlation.
# in this case, patch area may be a more ecologically relevant predictor.
# this is because we assume that reef fish are more likely to respond to
# patch area than perimeter. so we will retain patch area and remove patch 
# perimeter in the next step, hopefully this will resolve the issue.

# finally, what about seagrass area within a 500 m buffer? this is possibly 
# correlated with either area of coral & hardbottom or unconsolidated sediment
cor(lg_vars$Area_SG, lg_vars$Area_CRHB, use = "complete") # below our threshold
cor(lg_vars$Area_SG, lg_vars$Area_US, use = "complete") # strong negative correlation
# here, area of surrounding seagrass is what we're more so interested in, 
# because this can serve as a proxy for available prey (where fish
# feed overnight). let's keep seagrass instead of unconsolidated sediments,
# hopefully this resolves the issue. 

# remove collinear variables as described above.
lg_vars2 = lg_vars %>% select(-StDev_Depth, -Patch_Perimeter, -Area_US)

# reduced correlation matrix from retained variables
lg_cormat2 = cor(lg_vars2, use = "complete.obs")

# reduced correlation plot
corrplot(lg_cormat2, method = "color", col = palette, type = "upper",
         order = "original", addCoef.col = "black", number.cex = 0.50, 
         number.digits = 2, tl.col = "black", tl.srt = 40, tl.cex = 0.8)

# check correlation again
lg_corr2 = vifcor(lg_vars2, th = 0.7)
lg_corr2

# check VIF again
lg_vif2 = vifstep(lg_vars2, th = 5)
lg_vif2

# keep PA_Ratio and Patch_Area because we're interested in the relative influences of
# both of these predictors - is patch reef size or complexity of shape more important?

#### CREATE CALIBRATION/VALIDATION SUBSETS ####
lg_full = lg %>%
  select(-Curvature, -Plan_Curve, -Rugosity, -BPI_Fine,
         -Mean_Sum_DO, -Mean_Win_DO, -StDev_Depth,
         -Patch_Perimeter, -Area_US, -Pred_Abundance)

# keep only one observation per patch reed 

# randomize the order of rows
lg_random = sample(nrow(lg_full))
lg_full = lg_full[lg_random, ]

# now randomly split data into two subsets: one for model calibration (70%)
# and one for model validation (30%)
lg_train_id = sample(seq_len(nrow(lg_full)), size = floor(0.70*nrow(lg_full)))  
lg_train = lg_full[lg_train_id,] # creates the training dataset (70%)
lg_test = lg_full[-lg_train_id,]  # creates the test dataset (30%)

# repeat for bluestriped grunt
# first, let's do some data prep and cleaning to make the data frame easier
# to work with
hs = hs %>%
  # starting with some renaming of columns (where new name = old name)
  rename(Species = SPECIES_CD, # species
         Life_Stage = LIFE_STAGE, # life stage
         Presence = PRES, # binary presence (1 = yes, 0 = no)
         Presence_Category = PRES2, # character presence (present or absent)
         Abundance = N, # fish counts (averaged over two buddy divers at site)
         Long_M = LON_M, # longitude of survey (meters)
         Lat_M = LAT_M, # latitude of survey (meters)
         Patch_Area = AREA, # patch reef area (square meters)
         Patch_Perimeter = PERIMETER, # patch reef perimeter (meters)
         Patch_Neigh_Dist = NEAR_DIST, # for each patch reef, distance to nearest neighbor patch reef (meters)
         Area_CRHB = Coral_Reef_and_Hardbottom, # coral reef and hard bottom area in 500 m buffer (square meters)
         Area_SG = Seagrass, # seagrass area in 500 m buffer (square meters)
         Area_US = Unconsolidated_Sediment, # unconsolidated sediment area in 500 m buffer (square meters)
         Pred_Abundance = N_PRED, # predator abundance
         Pred_Density = DEN_PRED) %>%  # predator density
  # let's make sure that our response presence/absence column is a factor
  mutate(Presence = as.factor(Presence),
         Presence_Category = as.factor(Presence_Category)) %>%
  # add new column storing the area sampled by each reef fish survey,
  # where area = pie*radius^2 --> pie*7.5m^2 --> ~177 m^2
  mutate(Survey_Area = rep(as.numeric(177), nrow(.))) %>%
  # add new column storing the density of sub-adult gray snapper,
  # where density = abundance/area of sampling = fish per m^2
  mutate(Density = Abundance/Survey_Area) %>%
  # now keep only the columns we need, the order here will determine column order
  # in the output dataframe 
  select(Patch_ID, Survey_to_Reef_Dist, Species, Life_Stage, Presence, 
         Presence_Category, Abundance, Density, Survey_Area, Long_M, Lat_M,
         Habitat, Mangrove_Dist, Reef_Dist, Depth, StDev_Depth, Slope, 
         Curvature, Plan_Curve, BPI_Fine, BPI_Broad, Rugosity, Mean_Sum_Temp,
         Mean_Sum_DO, Mean_Sum_Sal, Mean_Win_Temp, Mean_Win_DO, Mean_Win_Sal,
         Patch_Area, Patch_Perimeter, PA_Ratio, Patch_Neigh_Dist, Area_CRHB,
         Area_SG, Area_US, Pred_Abundance, Pred_Density)

# sort data by increasing latitude and then add a NEW object_id column based on 
# this sorting (later, we will repeat this for bluestriped grunt and use the 
# object_ids to extract data from the same training and testing sites as gray snapper)
hs = hs[order(hs$Lat_M),]
hs$Object_ID = seq.int(nrow(hs))

# we DO NOT have to repeat the correlation assessment because all gray 
# snapper and bluestriped grunt data come from the SAME EXACT surveys.
# so if we were to repeat the correlation and VIF tests, we would get 
# identical results! instead, jump ahead to saving the modeling datasets:
# full, calibration, and validation. 

# now create a new data frame that stores only the variables retained for modeling
hs_full = hs %>%
  select(-Curvature, -Plan_Curve, -Rugosity, -BPI_Fine,
         -Mean_Sum_DO, -Mean_Win_DO, -StDev_Depth,
         -Patch_Perimeter, -Area_US, -Pred_Abundance)

# now, use the object_id values stored in the gray snapper training and testing
# dataframes to extract bluestriped grunt data from the same exact locations
hs_train = hs_full %>%
  filter(Object_ID %in% lg_train$Object_ID)

hs_test = hs_full %>%
  filter(Object_ID %in% lg_test$Object_ID)

# save result
write.csv(lg_train,
          here("Data", "Model_Data", "Subadult_Gray_Snapper_Patch_Reef_Data_Training.csv"),
          row.names = FALSE)

write.csv(lg_test,
          here("Data", "Model_Data", "Subadult_Gray_Snapper_Patch_Reef_Data_Testing.csv"),
          row.names = FALSE)

write.csv(hs_train,
          here("Data", "Model_Data", "Subadult_Bluestriped_Grunt_Patch_Reef_Data_Training.csv"),
          row.names = FALSE)

write.csv(hs_test,
          here("Data", "Model_Data", "Subadult_Bluestriped_Grunt_Patch_Reef_Data_Testing.csv"),
          row.names = FALSE)

