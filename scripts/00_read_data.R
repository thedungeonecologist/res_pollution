#' ---
#' title: "Read Data"
#' author: "Darren Shoemaker"
#' ---

# Libraries

library(tidyverse)
library(here)

# Read in data ----

dat <- read.csv(here('data/raw', 'res_pollution_v4.csv'), na.strings = c("", "NA"))

# Convert Data to Factors

dat <- dat %>% 
  mutate(Size_Class = as.factor(Size_Class),
         WSA9 = as.factor(WSA9),
         Poll_clus = as.factor(Poll_clus),
         Urban = LURBAN + HURBAN + URBGRASS + COMM,
         Forest = FORESTD + FORESTE + FORESTM, 
         .after = BARREN, .keep = c('unused')) %>% 
  arrange(NIDID)

#[1] Separate Data Types ----

# Nutrient data

nut <- dat %>% 
  select(c(46:77)) %>% 
  filter(!TPYLD == 0) #filter out suspected artificial zeros

# Apply same filter to dat for equal length dataframes

dat2 <- dat %>% 
  filter(!TPYLD == 0)

#' 631 reservoirs in study after filtering out suspected invalid 0s

# Impairment scores ----

impair <- dat %>% 
  select(c(98:117))

# Size ----

size <- dat %>% 
  select(Size_Class)

# Morphology ----

dat3 <- read.csv(file = here('data/processed', 'morphometrical.csv'), na.strings = c("", "NA")) %>%  
  arrange(NIDID) %>%
  mutate(Poll_clus = as.factor(CLUSTER), .keep = 'unused') %>% 
  na.omit()

morph <- dat3 %>% 
  select(-c(NIDID, Poll_clus))
  
# Land Use ----

land <- dat %>% 
  filter(!TPYLD == 0) %>% 
  select(c(36:45)) 

# Climate ----

clim <- dat %>% 
  select(c(78, 81, 89, 92)) %>% 
  mutate(across(c(1:4), scale)) %>% 
  rename('Annual Mean Temperature' = H1, 
         'Temperature Seasonality' = H4,
         'Annual Precipitation' = H12,
         'Precipitation Seasonality' = H15)

# Reservoirs with all data categories (no missing values)

dat_all <- read.csv(file = here('data/raw', 'res_with_all_data.csv')) %>% 
  select(-c(1)) %>% 
  relocate(Poll_clus, .after = NIDID)

# Find missing values in Nutrient Data

clus_table <- read.csv(file = here('data/processed', 'cluster table.csv'))

sum(is.na(dat$Size_Class))
sum(is.na(nut))
sum(is.na(impair))

#[2] Calculate Measures of Variability ----

res_urb <- dat %>% 
  mutate(Ratio_MeanDepth_Max_Depth = Mean_Depth/NID_Height,
         Ratio_Drainage_Surface_Area = Drainage_Area_km2/Surface_Area_km2)

# Coefficient of Variation

sd(dat$H1)/mean(dat$H1)
sd(dat$H4)/mean(dat$H4)
sd(dat$H5)/mean(dat$H5)
sd(dat$H6)/mean(dat$H6)
sd(dat$H7)/mean(dat$H7)
sd(dat$H12)/mean(dat$H12)
sd(dat$H13)/mean(dat$H13)
sd(dat$H14)/mean(dat$H14)
sd(dat$H15)/mean(dat$H15)

sd(dat$NID_Height)/mean(dat$NID_Height)
sd(dat$Maximum_Discharge)/mean(dat$Maximum_Discharge)
sd(dat$NID_Storage)/mean(dat$NID_Storage)
sd(dat$Drainage_Area_km2)/mean(dat$Drainage_Area_km2)
sd(dat$Surface_Area_km2)/mean(dat$Surface_Area_km2)
sd(dat$Mean_Depth)/mean(dat$Mean_Depth)
sd(res_urb$Ratio_MeanDepth_Max_Depth)/mean(res_urb$Ratio_MeanDepth_Max_Depth)
sd(res_urb$Ratio_Drainage_Surface_Area)/mean(res_urb$Ratio_Drainage_Surface_Area)

sd(dat$WATER)/mean(dat$WATER)
sd(dat$WETLANDS)/mean(dat$WETLANDS)
sd(res_urb$URBAN)/mean(res_urb$URBAN)
sd(res_urb$FOREST)/mean(res_urb$FOREST)
sd(dat$SHRUB)/mean(dat$SHRUB)
sd(dat$GRASS)/mean(dat$GRASS)
sd(dat$PASTURE)/mean(dat$PASTURE)
sd(dat$CROPS)/mean(dat$CROPS)
sd(dat$BARREN)/mean(dat$BARREN)