#===========================================
# title: "eulachon_outline"
# author: Brooke Hackett
# date: May 5th 2021
# packages:
library(rio)
library(ggplot2)
library(lubridate)
library(dplyr)
library(magrittr)
library(tidyr)
library(Hmisc)
library(devtools)
library(stringr)
library(ggmap)
library(maps)
library(mapdata)
library(cluster)
library(MASS)
library(corrplot)
#===========================================

#===========================================
# Data import and preparation 
#===========================================

# import data sets

taxa_data <- import("C:\\Users\\hackettb\\Documents\\R\\eulachon_project\\TaxaGroups.csv")

catch_data <- import("C:\\Users\\hackettb\\Documents\\R\\eulachon_project\\catch.csv")


# merge data sets

taxa_catch_data <- merge(catch_data, taxa_data, by = "Species_name")


# change species name, taxonomic group, and area code to factor 

taxa_catch_data$Species_name <- factor(taxa_catch_data$Species_name)

taxa_catch_data$Taxonomic_group <- factor(taxa_catch_data$Taxonomic_group)

taxa_catch_data$MAJOR_STAT_AREA_CODE <- factor(taxa_catch_data$MAJOR_STAT_AREA_CODE)


# manipulate date variables with lubridate

taxa_catch_data$DATE <- ymd(taxa_catch_data$DATE)

taxa_catch_data$MONTH <- month(taxa_catch_data$DATE, label = TRUE, abbr = TRUE)

taxa_catch_data$YEAR <- year(taxa_catch_data$DATE)


# remove irrelevant taxa groups and tows from Hecate Strait and sets with only Eulachon CPUE recorded

filtered_eulachon_data <- taxa_catch_data %>%
  filter(., Taxonomic_group %nin% c("Marine mammal",
                                    "Other",
                                    "Euphausid",
                                    "Worm",
                                    "Isopod"),
         EVENT_ID %nin% c(4363856,
                          4363858,
                          4564096,
                          4564094))


# factor species name, taxonomic group, and area code again 
# with eulachon_data to get rid of extra levels that were filtered out  

filtered_eulachon_data$Species_name <- factor(filtered_eulachon_data$Species_name)

filtered_eulachon_data$Taxonomic_group <- factor(filtered_eulachon_data$Taxonomic_group)

filtered_eulachon_data$MAJOR_STAT_AREA_CODE <- factor(filtered_eulachon_data$MAJOR_STAT_AREA_CODE)


# creating data table in wide format

eulachon_wide_all_species <- pivot_wider(filtered_eulachon_data, 
                             names_from =Species_name,
                             values_from = CPUE,
                             #names_prefix = "CPUE for ",
                             id_cols = !Taxonomic_group)

# eulachon data filtered for species with mean CPUE above 40

eulachon_wide_1 <- eulachon_wide_all_species %>%
  dplyr::select(TRIP_ID:TEMPERATURE)      


eulachon_wide_2 <- eulachon_wide_all_species[14:217] %>%
  dplyr::select(where(~ is.numeric(.x) && mean(.x) > 40))

eulachon_wide <- cbind(eulachon_wide_1, eulachon_wide_2)


# add log CPUE

eulachon_data <- filtered_eulachon_data %>% 
  mutate(., log_CPUE = log(CPUE+1))


# rename factor level North and South 

eulachon_data$MAJOR_STAT_AREA_CODE <- recode_factor(eulachon_data$MAJOR_STAT_AREA_CODE, 
                                                             "1" = "South",
                                                             "8" = "North")

str(eulachon_data)


#=================================================
# filtering by area (north vs south)
#=================================================

# area 1 - south

south_data <- eulachon_data %>%
  filter(., MAJOR_STAT_AREA_CODE %in% "South")


# area 8 - north

north_data <- eulachon_data %>%
  filter(., MAJOR_STAT_AREA_CODE %in% "North")


#==========================================
# top 20 species by area
#==========================================

# South - total and mean CUPE by species summary

CPUE_summary_south <- south_data %>% 
  group_by(Species_name) %>%
  summarise(., MEAN_CPUE_BY_SPECIES_SOUTH = mean(CPUE),
            TOTAL_CPUE_BY_SPECIES_SOUTH = sum(CPUE)) %>% 
  arrange(desc(MEAN_CPUE_BY_SPECIES_SOUTH), TOTAL_CPUE_BY_SPECIES_SOUTH) %>% 
  data.frame()

CPUE_summary_south

top_20_CPUE_south <- south_data %>%
  filter(., Species_name%in%c("HYDROLAGUS.COLLIEI", 
                              "GADUS.CHALCOGRAMMUS", 
                              "SQUALUS.SUCKLEYI",
                              "PAROPHRYS.VETULUS",
                              "MERLUCCIUS.PRODUCTUS",
                              "RAJA.RHINA",
                              "BERINGRAJA.BINOCULATA",
                              "MICROSTOMUS.PACIFICUS",
                              "LYOPSETTA.EXILIS",
                              "GLYPTOCEPHALUS.ZACHIRUS",
                              "PANDALOPSIS.DISPAR",
                              "CLUPEA.PALLASII",
                              "ATHERESTHES.STOMIAS",
                              "HIPPOGLOSSOIDES.ELASSODON",
                              "THALEICHTHYS.PACIFICUS",
                              "GADUS.MACROCEPHALUS",
                              "BATHYRAJA.INTERRUPTA",
                              "PANDALUS.PLATYCEROS",
                              "MYTILIDAE",
                              "ANOPLOPOMA.FIMBRIA"))

# North - total and mean CUPE by species summary

CPUE_summary_north <- north_data %>% 
  group_by(Species_name) %>%
  summarise(., MEAN_CPUE_BY_SPECIES_NORTH = mean(CPUE),
            TOTAL_CPUE_BY_SPECIES_NORTH = sum(CPUE)) %>% 
  arrange(desc(MEAN_CPUE_BY_SPECIES_NORTH), TOTAL_CPUE_BY_SPECIES_NORTH) %>% 
  data.frame()

CPUE_summary_north

top_20_CPUE_north <- north_data %>%
  filter(., Species_name%in%c("HYDROLAGUS.COLLIEI", 
                              "PANDALOPSIS.DISPAR",
                              "GADUS.CHALCOGRAMMUS",
                              "HIPPOGLOSSOIDES.ELASSODON",
                              "ATHERESTHES.STOMIAS",
                              "THALEICHTHYS.PACIFICUS",
                              "LYCODES.PACIFICUS",
                              "LYOPSETTA.EXILIS",
                              "SQUALUS.SUCKLEYI",
                              "PANDALUS.JORDANI",
                              "RAJA.RHINA",
                              "ANOPLOPOMA.FIMBRIA",
                              "GLYPTOCEPHALUS.ZACHIRUS",
                              "PANDALUS.BOREALIS",
                              "PAROPHRYS.VETULUS",
                              "MICROSTOMUS.PACIFICUS",
                              "CLUPEA.PALLASII",
                              "PECTINIDAE",
                              "LYCODES.BREVIPES",
                              "SEBASTES.MALIGER"))

# combine top 20 North and South data sets

top_north_south_data <- rbind(top_20_CPUE_north, top_20_CPUE_south)


#======================================================                
# getting distinct colour palette for plotting 
#======================================================

# primary colours function

primary.colors <- function(n, steps = 3, no.white = TRUE)
{
  i <- round(seq(0, 255, length.out = steps))
  if(is.R()) {
    res <- rgb(expand.grid(i, i, i), maxColorValue = 255)
  } else {
    cmat <- expand.grid(i, i, i)
    res <- rgb(cmat[,1], cmat[,2], cmat[,3], maxColorValue = 255)
  }
  if ( no.white ) res <- res[-length(res)]
  if ( missing(n) )
    res
  else
    res[round(seq(1, length(res), length.out = n))]
}


mycolour <- c('#e6194B', '#3cb44b', '#ffe119', '#4363d8', 
              '#f58231', '#911eb4', '#42d4f4', '#f032e6', 
              '#bfef45', '#fabed4', '#469990', '#dcbeff', 
              '#9A6324', '#fffac8', '#800000', '#aaffc3', 
              '#808000', '#ffd8b1', '#000075', '#a9a9a9',
              "#0000FF", "#8000FF", "#FF00FF", "#0080FF", "#8080FF", "#FF80FF")


mycolour1 <- c( "#000000","#80FFFF","#FF0000" , "#008000", "#808000", "#FF8000",
                "#000080" , "#80FF00", "#FFFF00",'#dcbeff', "#800080", "#FF0080",
               "#008080", "#808080", "#FF8080",'#9A6324',"#80FF80", "#FFFF80",
               "#0000FF", "#8000FF", "#FF00FF", "#0080FF","#8080FF", "#FF80FF",
               "#00FFFF", "#800000")

mycolour2 <- primary.colors(11, steps = 4)

month_colours <- c("Feb"='#e6194B', "Oct"='#3cb44b', "Mar"='#ffe119', "Apr"='#4363d8', 
                   "May"='#f58231', "Jul"='#42d4f4', "Jun"='#f032e6', "Sep"='#fabed4', 
                    "Dec"='#dcbeff', "Nov"='#800000', "Jan"='#aaffc3')


#========================================================
# Table 1. Number of samples by month and year and area
#========================================================

# 204 species total so 204 rows for each sample 
# so number of samples = unique count by MONTH, YEAR, MAJOR_STAT_AREA_CODE / 204

samples_by_month_year_area <- eulachon_data %>% 
  group_by(., MONTH, YEAR, MAJOR_STAT_AREA_CODE) %>%
  summarise(., number_of_samples = n()/204)


#=============================================================
# Table 2. Mean CPUE of top 20 species captured in each area
#=============================================================

# north and south in same table

mean_CPUE_by_area <- top_north_south_data %>%
  group_by(., Species_name, MAJOR_STAT_AREA_CODE)%>%
  summarise(., mean_CPUE = mean(CPUE))


# north and south in different tables

mean_CPUE_north <- top_20_CPUE_north %>% 
  group_by(Species_name) %>%
  summarise(., mean_CPUE_north = mean(CPUE)) %>% 
  arrange(desc(mean_CPUE_north))


mean_CPUE_south <- top_20_CPUE_south %>% 
  group_by(Species_name) %>%
  summarise(., mean_CPUE_south = mean(CPUE)) %>% 
  arrange(desc(mean_CPUE_south))

#=================================================================================
#Table 3. Results of discriminant function analysis testing cluster membership.
#=================================================================================






#===================================================
#Table 4. Diversity measures by month and area
#===================================================






#===================================================================================
# Figure 1. Map of study area showing bottom trawl locations color coded by month
#===================================================================================

trawl_location <- eulachon_data %>%
  group_by(., EVENT_ID)%>%
  summarise(., MONTH=MONTH,
            START_LATITUDE=START_LATITUDE,
            START_LONGITUDE=START_LONGITUDE,
            MAJOR_STAT_AREA_CODE=MAJOR_STAT_AREA_CODE)

distinct_trawl_location <- distinct(trawl_location)


### North and South in the same map

# make the bounding box

bc_bbox <- make_bbox(lat = START_LATITUDE, lon = START_LONGITUDE, data = distinct_trawl_location)
bc_bbox

# grab the maps from Google

bc_big <- get_map(location = bc_bbox, source = "google", maptype = "terrain")


# plot the points and color them by month

ggmap(bc_big) + 
  geom_point(data = distinct_trawl_location, 
             mapping = aes(x = START_LONGITUDE, y = START_LATITUDE, color = MONTH))+
  scale_color_manual(values = month_colours)


### North map

distinct_trawl_location_north <- distinct_trawl_location %>%
  filter(., MAJOR_STAT_AREA_CODE=="North")

# make the bounding box

north_bbox <- make_bbox(lat = START_LATITUDE, lon = START_LONGITUDE, data = distinct_trawl_location_north)
north_bbox

# grab the maps from Google

bc_north <- get_map(location = north_bbox, source = "google", maptype = "terrain")


# plot the points and color them by month

ggmap(bc_north) + 
  geom_point(data = distinct_trawl_location_north, 
             mapping = aes(x = START_LONGITUDE, y = START_LATITUDE, color = MONTH))+
  scale_color_manual(values = month_colours)

### South map

distinct_trawl_location_south <- distinct_trawl_location %>%
  filter(., MAJOR_STAT_AREA_CODE=="South")

# make the bounding box

south_bbox <- make_bbox(lat = START_LATITUDE, lon = START_LONGITUDE, data = distinct_trawl_location_south)
south_bbox

# grab the maps from Google

bc_south <- get_map(location = south_bbox, source = "google", maptype = "terrain")


# plot the points and color them by month

ggmap(bc_south) + 
  geom_point(data = distinct_trawl_location_south, 
             mapping = aes(x = START_LONGITUDE, y = START_LATITUDE, color = MONTH))+
  scale_color_manual(values = month_colours)


  
#=========================================================================================
# Figure 2. Proportion of catch for top species in stacked bar graphs by month and area
#=========================================================================================

# average CPUE for top species by month and area in the same graph

CPUE_by_month_area <- top_north_south_data %>%
  group_by(., Species_name, MONTH, MAJOR_STAT_AREA_CODE) %>%
  summarise(., mean_CPUE = mean(CPUE),
            total_CPUE = sum(CPUE),
            mean_log_CPUE = mean(log_CPUE))


ggplot(CPUE_by_month_area )+
  geom_bar(aes(x=MONTH, y=mean_CPUE, fill=Species_name), stat = "identity")+
  scale_fill_manual(values = mycolour1) +
  facet_wrap(~MAJOR_STAT_AREA_CODE)

# using mean log CPUE for top species by month and area in the same graph

ggplot(CPUE_by_month_area )+
  geom_bar(aes(x=MONTH, y=mean_log_CPUE, fill=Species_name), stat = "identity")+
  scale_fill_manual(values = mycolour1) +
  facet_wrap(~MAJOR_STAT_AREA_CODE)


# mean CPUE for top species by month and area standardized to one  

ggplot(CPUE_by_month_area )+
  geom_bar(aes(x=MONTH, y=mean_CPUE, fill=Species_name), stat = "identity", position = "fill")+
  scale_fill_manual(values = mycolour1) +
  facet_wrap(~MAJOR_STAT_AREA_CODE)

# using mean log CPUE for top species by month and area standardized to one  

ggplot(CPUE_by_month_area )+
  geom_bar(aes(x=MONTH, y=mean_log_CPUE, fill=Species_name), stat = "identity", position = "fill")+
  scale_fill_manual(values = mycolour1) +
  facet_wrap(~MAJOR_STAT_AREA_CODE)

#==============================================================================
# Figure 3. Bottom temperature, depth and salinity boxplots for distributions 
#==============================================================================





#===================================================================================
# Figure 4. Correlation plot among species for co-occurrence in the trawl hauls
#===================================================================================

# all species all areas = way to complex

species_cor <- cor(data.frame(eulachon_wide[,14:38]),use="complete.obs")

species_cor

corrplot(species_cor, tl.col = "black")



#==============================================================================
# Figure 5. Bray-curtis dissimilarity dendrograms between months and areas
#==============================================================================






#==============================================================================
# Figure 6. NMDS plots of species with secondary temperature, depth axes
#==============================================================================




#==============================================================
# Figure 7. Maps of showing cluster locations by month
#==============================================================



















