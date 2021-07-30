#===========================================
# title: "eulachon_outline_final"
# author: Brooke Hackett
# date: July 8th 2021
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
library(gridExtra)
library(vegan)
library(dendextend)
library(ggalt)

library(indicspecies)
library(perm)

library(RAM)
options(scipen = 999)
#===========================================

#===========================================
# Data import and preparation 
#===========================================

# import data sets

taxa.data <- import("C:\\Users\\hackettb\\Documents\\R\\eulachon_project\\eulachon\\TaxaGroups.csv")

catch.data <- import("C:\\Users\\hackettb\\Documents\\R\\eulachon_project\\eulachon\\catch.csv")

temp.data <- import("C:\\Users\\hackettb\\Documents\\R\\eulachon_project\\eulachon\\temperature.csv")

area.data <- import("C:\\Users\\hackettb\\Documents\\R\\eulachon_project\\eulachon\\areasforBrooke.csv")

# merge data sets

area_catch_data <- merge(catch.data, area.data, by = "EVENT_ID", all = TRUE)

area_taxa_catch_data <- merge(area_catch_data, taxa.data, by = "Species_name")

area_taxa_catch_temp_data <- merge( area_taxa_catch_data, temp.data, by = c("TRIP_ID", "EVENT_ID"))


# Changing NAs in AreaGrouping from the north survey to A4

area_taxa_catch_temp_data$AreaGrouping <- replace_na(area_taxa_catch_temp_data$AreaGrouping, "A4")

# reorder data frame

catch_data <- area_taxa_catch_temp_data %>%
  dplyr::select(Species_name, TRIP_ID,EVENT_ID, SET_ID, YEAR, MONTH, DATE, 
                MAJOR_STAT_AREA_CODE, AreaGrouping, START_LATITUDE, START_LONGITUDE,
                END_LATITUDE, END_LONGITUDE, DEPTH, MEAN_TEMPERATURE, 
                CPUE, Taxonomic_group) %>%
  arrange(., Species_name)


# manipulate date variables with lubridate

catch_data$DATE <- ymd(catch_data$DATE)

catch_data$MONTH <- month(catch_data$DATE, label = TRUE, abbr = TRUE)

catch_data$YEAR <- year(catch_data$DATE)

# adding season to data set

catch_data <- catch_data %>% 
  mutate(., SEASON = recode(catch_data$MONTH, 
                            "Jan" = "Winter",
                            "Feb"="Winter",
                            "Mar"="Spring",
                            "Apr"="Spring",
                            "May"="Spring",
                            "Jun"="Summer",
                            "Jul"="Summer",
                            "Aug"="Summer",
                            "Sep"="Fall",
                            "Oct"="Fall",
                            "Nov"="Fall",
                            "Dec"="Winter"), .after=DATE)


# change taxonomic group and species names to factors 

catch_data$Taxonomic_group <- factor(catch_data$Taxonomic_group)

catch_data$Species_name <- factor(catch_data$Species_name)


# adding top 25 species common name to dataset

catch_data <- catch_data %>% 
  mutate(., Species_common_name = recode(catch_data$Species_name,
                                         "ACTINIARIA"="Sea anemone",
                                         "ANOPLOPOMA.FIMBRIA"="Sablefish",
                                         "ATHERESTHES.STOMIAS"= "Arrowtooth flounder",
                                         "BATHYRAJA.INTERRUPTA"="Bering skate",
                                         "BERINGRAJA.BINOCULATA"="Big skate",
                                         "CLUPEA.PALLASII"= "Pacific herring",
                                         "GADUS.CHALCOGRAMMUS"="Walleye pollock",
                                         "GADUS.MACROCEPHALUS"="Pacific cod",
                                         "GLYPTOCEPHALUS.ZACHIRUS"="Rex sole",
                                         "HIPPOGLOSSOIDES.ELASSODON"="Flathead sole",
                                         "HYDROLAGUS.COLLIEI"="Spotted ratfish",
                                         "LYCODES.BREVIPES"="Shortfin eelpout",
                                         "LYCODES.PACIFICUS"="Blackbelly eelpout",
                                         "LYOPSETTA.EXILIS"="Slender soles",
                                         "MERLUCCIUS.PRODUCTUS"="North pacific hake",
                                         "MICROSTOMUS.PACIFICUS"="Pacific dover sole",
                                         "PANDALOPSIS.DISPAR"="Sidestripe shrimp",
                                         "PANDALUS.BOREALIS"="Northern shrimp",
                                         "PANDALUS.JORDANI"="Pink shrimp",
                                         "PANDALUS.PLATYCEROS"="Spot prawn",
                                         "PAROPHRYS.VETULUS"="English sole",
                                         "PECTINIDAE"="Scallop",
                                         "RAJA.RHINA"="Longnose skate",
                                         "SQUALUS.SUCKLEYI"="Pacific spiny dogfish",
                                         "THALEICHTHYS.PACIFICUS"="Eulachon"),
                                          .after=Species_name)

# changing top 25 species name format

catch_data$Species_name <- recode_factor(catch_data$Species_name, 
                                                      "ACTINIARIA"="Actiniaria",
                                                      "ANOPLOPOMA.FIMBRIA"="Anoplopoma fimbria",
                                                      "ATHERESTHES.STOMIAS"= "Atheresthes stomias",
                                                      "BATHYRAJA.INTERRUPTA"="Bathyraja interrupta",
                                                      "BERINGRAJA.BINOCULATA"="Beringraja binoculata",
                                                      "CLUPEA.PALLASII"= "Clupea pallasii",
                                                      "GADUS.CHALCOGRAMMUS"="Gadus chalcogrammus",
                                                      "GADUS.MACROCEPHALUS"="Gadus macrocephalus",
                                                      "GLYPTOCEPHALUS.ZACHIRUS"="Glyptocephalus zachirus",
                                                      "HIPPOGLOSSOIDES.ELASSODON"="Hippoglossoides elassodon",
                                                      "HYDROLAGUS.COLLIEI"="Hydrolagus colliei",
                                                      "LYCODES.BREVIPES"="Lycodes brevipes",
                                                      "LYCODES.PACIFICUS"="Lycodes pacificus",
                                                      "LYOPSETTA.EXILIS"="Lyopsetta exilis",
                                                      "MERLUCCIUS.PRODUCTUS"="Merluccius productus",
                                                      "MICROSTOMUS.PACIFICUS"="Microstomus pacificus",
                                                      "PANDALOPSIS.DISPAR"="Pandalopsis dispar",
                                                      "PANDALUS.BOREALIS"="Pandalus borealis",
                                                      "PANDALUS.JORDANI"="Pandalus jordani",
                                                      "PANDALUS.PLATYCEROS"="Pandalus Platyceros",
                                                      "PAROPHRYS.VETULUS"="Parophrys vetulus",
                                                      "PECTINIDAE"="Pectinidae",
                                                      "RAJA.RHINA"="Raja rhina",
                                                      "SQUALUS.SUCKLEYI"="Squalus suckleyi",
                                                      "THALEICHTHYS.PACIFICUS"="Thaleichthys pacificus")


# remove irrelevant taxa groups and tows from Hecate Strait and sets with only Eulachon CPUE recorded

catch_data <- catch_data %>%
  filter(., Taxonomic_group %nin% c("Marine mammal",
                                    "Other",
                                    "Euphausid",
                                    "Worm",
                                    "Isopod"),
         EVENT_ID %nin% c(4363856,
                          4363858,
                          4564096,
                          4564094,
                          4363883))

# factor species name, taxonomic group again to get rid of extra levels that were filtered out
# factor MAJOR_STAT_AREA_CODE

catch_data$Taxonomic_group <- factor(catch_data$Taxonomic_group)

catch_data$Species_name <- factor(catch_data$Species_name)

catch_data$MAJOR_STAT_AREA_CODE <- factor(catch_data$MAJOR_STAT_AREA_CODE)


# creating data table in wide format

catch_wide <- pivot_wider(catch_data,
                          names_from =Species_common_name,
                          values_from = CPUE,
                          id_cols = !c(Taxonomic_group,Species_name))


# Remove species with mean CPUE < 40

catch_wide_1 <- catch_wide %>%
  dplyr::select(TRIP_ID:MEAN_TEMPERATURE)      

catch_wide_2 <- catch_wide[16:219] %>%
  dplyr::select(where(~ is.numeric(.x) && mean(.x) > 40))

catch_wide_top25 <- cbind(catch_wide_1, catch_wide_2)

# log (CPUE + 1) transform wide data

catch_wide_log <- catch_wide_top25

for (i in 16:40) {
  catch_wide_log[,i]<-log1p(catch_wide_log[,i])
}


#======================================================                
# getting distinct colour palette for plotting 
#======================================================

month_colours <- c("Feb"='#e6194B', "Oct"='#3cb44b', "Mar"='#ffe119', "Apr"='#4363d8', 
                   "May"='#f58231', "Jul"='#42d4f4', "Jun"='#f032e6', "Sep"='#fabed4', 
                   "Dec"='#dcbeff', "Nov"='#800000', "Jan"='#aaffc3')

season_colours <- c('Winter'="blue",
                    'Spring'="orange",
                    'Summer'="red",
                    'Fall'="green")

area_colours <- c('A1'="blue",
                    'A2'="orange",
                    'A3'="red",
                    'A4'="green")

north_south_colours<- c("1"="green", "8"="blue")

mycolour <- c( "#000000","#80FFFF","#FF0000" , "#008000", "#808000", "#FF8000",
               "#000080" , "#80FF00", "#FFFF00",'#dcbeff', "#800080", "#FF0080",
               "#008080", "#808080", "#FF8080",'#9A6324',"#80FF80", "#FFFF80",
               "#0000FF", "#8000FF", "#FF00FF", "#0080FF","#8080FF", "#FF80FF",
               "#00FFFF", "#800000")

#========================================================
# Table 1. Number of samples by month and year and area
#========================================================

# 204 species total so 204 rows for each set 
# so number of sets = unique count by MONTH, YEAR, MAJOR_STAT_AREA_CODE(or AreaGrouping)/204


#### Number of sets North vs South

samples_by_month_year_north_south <- catch_data %>% 
  group_by(., MAJOR_STAT_AREA_CODE,MONTH,YEAR) %>%
  summarise(., number_of_sets = n()/204) 

samples_by_month_year_north_south <-samples_by_month_year_north_south %>%
  arrange(., MAJOR_STAT_AREA_CODE, YEAR)

colnames(samples_by_month_year_north_south ) <- c("Area","Month", "Year","Total sets")

# rename factor level North and South 

samples_by_month_year_north_south$Area <- recode_factor(samples_by_month_year_north_south$Area, 
                                                    "1" = "South",
                                                    "8" = "North")

# format data table in excel 
export(samples_by_month_year_north_south, "samples_by_month_year_north_south.xlsx")


#### Number of sets by AreaGrouping

samples_by_month_year_area <- catch_data %>% 
  group_by(., AreaGrouping, MONTH, YEAR) %>%
  summarise(., number_of_sets = n()/204) 

samples_by_month_year_area <-samples_by_month_year_area %>%
  arrange(., AreaGrouping,YEAR)

colnames(samples_by_month_year_area ) <- c("Area","Month", "Year","Total sets")

export(samples_by_month_year_area, "samples_by_month_year_area.xlsx")

# in wide format

samples_wide <-pivot_wider(samples_by_month_year_area, 
                           names_from = Area, 
                           values_from = "Total sets")

samples_wide <- samples_wide%>% arrange(., Year)

export(samples_wide, "samples_wide.xlsx")


#=============================================================
# Table 2. Mean CPUE of top 25 species by area
#=============================================================

# get top 25 species in long format 

catch_top25 <- catch_data %>% 
  filter(., Species_common_name %in% c("Sea anemone",
                                       "Sablefish",
                                       "Arrowtooth flounder",
                                       "Bering skate",
                                       "Big skate",
                                       "Pacific herring",
                                       "Walleye pollock",
                                       "Pacific cod",
                                       "Rex sole",
                                       "Flathead sole",
                                       "Spotted ratfish",
                                       "Shortfin eelpout",
                                       "Blackbelly eelpout",
                                       "Slender soles",
                                       "North pacific hake",
                                       "Pacific dover sole",
                                       "Sidestripe shrimp",
                                       "Northern shrimp",
                                       "Pink shrimp",
                                       "Spot prawn",
                                       "English sole",
                                       "Scallop",
                                       "Longnose skate",
                                       "Pacific spiny dogfish",
                                       "Eulachon" ))

# add log CPUE

catch_top25 <- catch_top25 %>% 
  mutate(., log_CPUE = log(CPUE+1), .after = CPUE)


# mean CPUE north vs south

mean_CPUE_top25_north_south <- catch_top25 %>%
  group_by(., Species_name, Species_common_name, MAJOR_STAT_AREA_CODE)%>%
  summarise(., mean_CPUE = mean(CPUE))

mean_CPUE_top25_north_south <- pivot_wider(mean_CPUE_top25_north_south, 
                                names_from = MAJOR_STAT_AREA_CODE, 
                                values_from = mean_CPUE)

colnames(mean_CPUE_top25_north_south)<- c("Scientific Name", "Common Name", 
                                          "South CPUE", "North CPUE")

export(mean_CPUE_top25_north_south, "mean_CPUE_top25_north_south.xlsx")


# mean CPUE by area

mean_CPUE_top25_area <- catch_top25 %>%
  group_by(., Species_name, Species_common_name, AreaGrouping)%>%
  summarise(., mean_CPUE = mean(CPUE))

mean_CPUE_top25_area <- pivot_wider(mean_CPUE_top25_area, 
                                           names_from = AreaGrouping, 
                                           values_from = mean_CPUE)

colnames(mean_CPUE_top25_area)<- c("Scientific Name", "Common Name", 
                                          "A1 CPUE", "A2 CPUE",
                                   "A3 CPUE", "A4 CPUE")

export(mean_CPUE_top25_area, "mean_CPUE_top25_area.xlsx")

#===================================================
#Table 4. Diversity measures by month and area
#===================================================

diversity_data <- catch_data %>% 
  mutate(., MONTH_YEAR = format(as.Date(catch_data$DATE,format="%Y-%m-%d")
                                ,"%b-%y"), .after = MONTH)

# combine month-year and area into one column

diversity_data$area_month <- paste(diversity_data$AreaGrouping, diversity_data$MONTH_YEAR, sep=":")


diversity_data <- diversity_data %>% group_by(., area_month, Species_name) %>%
  summarise(mean_CPUE = mean(CPUE))


diversity_data<- pivot_wider(diversity_data, names_from = Species_name, 
                             values_from = mean_CPUE)

diversity_data_rowname<- diversity_data$area_month

diversity_data <- as.data.frame(diversity_data, 
                                row.names =diversity_data$area_month)
diversity_data <- as.data.frame(diversity_data,
                                row.names = diversity_data$area_month)

diversity_data <- diversity_data[,-1]

# calculate diversity index

Simpson <- diversity(diversity_data, "simpson")
Shannon <- diversity(diversity_data, "shannon")

H <- diversity(diversity_data)
Evenness <- H/log(specnumber(diversity_data))

diversity_index <- data.frame(Shannon, Simpson, Evenness, 
                              row.names = diversity_data_rowname)

#### species richness

# compare species richness between areas

area_rich <- catch_wide %>% 
  dplyr::select(., 'Sea anemone':ZAPRORA.SILENUS) 

boxplot(specnumber(area_rich) ~ catch_wide$AreaGrouping, 
        ylab = "# of species", xlab = "Area")


# plot species accumulation curve across samples

plot(specaccum(area_rich), xlab = "# of samples", ylab = "# of species")


# compare species richness by area and season

par(mfrow=c(2,2))

# A1

a1_rich_data <- catch_wide %>% 
  filter(., AreaGrouping=="A1")

a1_rich <- a1_rich_data %>%
  dplyr::select(., 'Sea anemone':ZAPRORA.SILENUS) 

boxplot(specnumber(a1_rich) ~ a1_rich_data$SEASON, 
        ylab = "# of species", xlab = "A1")

# A2

a2_rich_data <- catch_wide %>% 
  filter(., AreaGrouping=="A2")

a2_rich <- a2_rich_data %>%
  dplyr::select(., 'Sea anemone':ZAPRORA.SILENUS) 

boxplot(specnumber(a2_rich) ~ a2_rich_data$SEASON, 
        ylab = "# of species", xlab = "A2")

# A3

a3_rich_data <- catch_wide %>% 
  filter(., AreaGrouping=="A3")

a3_rich <- a3_rich_data %>%
  dplyr::select(., 'Sea anemone':ZAPRORA.SILENUS) 

boxplot(specnumber(a3_rich) ~ a3_rich_data$SEASON, 
        ylab = "# of species", xlab = "A3")

# A4

a4_rich_data <- catch_wide %>% 
  filter(., AreaGrouping=="A4")

a4_rich <- a4_rich_data %>%
  dplyr::select(., 'Sea anemone':ZAPRORA.SILENUS) 

boxplot(specnumber(a4_rich) ~ a4_rich_data$SEASON, 
        ylab = "# of species", xlab = "A4")

par(mfrow=c(1,1))
#===================================================================================
# Figure 1. Map of study area showing bottom trawl locations color coded by month
#===================================================================================

# get a list of distinct trawl locations

trawl_location <- catch_data %>%
  group_by(., EVENT_ID)%>%
  summarise(., MONTH=MONTH,
            START_LATITUDE=START_LATITUDE,
            START_LONGITUDE=START_LONGITUDE,
            MAJOR_STAT_AREA_CODE=MAJOR_STAT_AREA_CODE,
            AreaGrouping=AreaGrouping)

trawl_location <- distinct(trawl_location)


### North and South in the same map

# make the bounding box

bc_bbox <- make_bbox(lat = START_LATITUDE, 
                     lon = START_LONGITUDE, 
                     data = trawl_location)
bc_bbox

# grab the maps from Google

bc_big <- get_map(location = bc_bbox, source = "google", maptype = "terrain")

# plot the points and color them by month

ggmap(bc_big) + 
  geom_point(data = trawl_location, 
             mapping = aes(x = START_LONGITUDE, 
                           y = START_LATITUDE, color = MONTH))+
  scale_color_manual(values = month_colours)

# plot the points coloured by area with 

library(ggalt)

ggmap(bc_big)+
  geom_point(data = trawl_location,
             aes(x=START_LONGITUDE, y=START_LATITUDE,
                 colour = AreaGrouping), cex=0.5, alpha = 0.5,
                 show.legend = FALSE)+
  geom_encircle(data = trawl_location, 
                aes(x=START_LONGITUDE, y=START_LATITUDE,
                    fill = AreaGrouping, color = AreaGrouping),s_shape = 1, 
                    expand = 0,alpha = 0.3 )+
  scale_color_manual(values = area_colours)+
  scale_fill_manual(values = area_colours)+         
  theme(legend.title = element_blank())
  


### North map

trawl_location_north <- trawl_location %>%
  filter(., MAJOR_STAT_AREA_CODE %in% 8)

# make the bounding box

north_bbox <- make_bbox(lat = START_LATITUDE, lon = START_LONGITUDE, 
                        data = trawl_location_north)
north_bbox

# grab the maps from Google

bc_north <- get_map(location = north_bbox, source = "google", maptype = "terrain")


# plot the points and color them by month

ggmap(bc_north) + 
  geom_point(data = trawl_location_north, 
             mapping = aes(x = START_LONGITUDE, y = START_LATITUDE, color = MONTH))+
  scale_color_manual(values = month_colours)


### South map

trawl_location_south <- trawl_location %>%
  filter(., MAJOR_STAT_AREA_CODE%in%1)

# make the bounding box

south_bbox <- make_bbox(lat = START_LATITUDE, lon = START_LONGITUDE, 
                        data = trawl_location_south)
south_bbox

# grab the maps from Google

bc_south <- get_map(location = south_bbox, source = "google", maptype = "terrain")


# plot the points and color them by month

ggmap(bc_south) + 
  geom_point(data = trawl_location_south, 
             mapping = aes(x = START_LONGITUDE, y = START_LATITUDE, color = MONTH))+
  scale_color_manual(values = month_colours)

# coloured by area 

ggmap(bc_south) + 
  geom_point(data = trawl_location_south, 
             mapping = aes(x = START_LONGITUDE, y = START_LATITUDE, color = AreaGrouping))+
  scale_color_manual(values = area_colours)


#=========================================================================================
# Figure 2. Proportion of catch for top species in stacked bar graphs by month and area
#=========================================================================================

### for north and south 

CPUE_by_month_north_south<- catch_top25 %>%
  group_by(., Species_common_name, MONTH, MAJOR_STAT_AREA_CODE) %>%
  summarise(., mean_CPUE = mean(CPUE),
            total_CPUE = sum(CPUE),
            mean_log_CPUE = mean(log_CPUE))

# add row for august to data frame for plotting

aug_row <- data.frame(Species_common_name = "Eulachon",
                      MONTH ="Aug", 
                      MAJOR_STAT_AREA_CODE = "1",
                      mean_CPUE= 0, 
                      total_CPUE = 0, 
                      mean_log_CPUE=0)

CPUE_by_month_north_south <- rbind(CPUE_by_month_north_south, aug_row)


CPUE_by_month_north_south$MONTH<-factor(CPUE_by_month_north_south$MONTH,
                                       levels=c("Jan", "Feb", "Mar", "Apr",
                                                "May", "Jun", "Jul","Aug", 
                                                "Sep", "Oct", "Nov", "Dec"))
# rename factor level North and South 

CPUE_by_month_north_south$MAJOR_STAT_AREA_CODE <- recode_factor(CPUE_by_month_north_south$MAJOR_STAT_AREA_CODE, 
                                                    "1" = "South",
                                                    "8" = "North")



# mean log CPUE for top species by month in north and south in a stacked bar graph

ggplot(CPUE_by_month_north_south )+
  geom_bar(aes(x=MONTH, y=mean_log_CPUE, fill=Species_common_name), stat = "identity")+
  scale_fill_manual(values = mycolour) +
  facet_wrap(~MAJOR_STAT_AREA_CODE)+
  labs(y= "Mean log(CPUE+1)", x= "Month", fill= "Species" )
  

# mean CPUE by month in north and south standardized to one  

ggplot(CPUE_by_month_north_south )+
  geom_bar(aes(x=MONTH, y=mean_CPUE, fill=Species_common_name), 
           stat = "identity", position = "fill")+
  scale_fill_manual(values = mycolour) +
  facet_wrap(~MAJOR_STAT_AREA_CODE)+
  labs(y= "Proportion", x= "Month", fill= "Species" )



### by area 

CPUE_by_month_area <- catch_top25 %>%
  group_by(., Species_common_name, MONTH, AreaGrouping) %>%
  summarise(., mean_CPUE = mean(CPUE),
            total_CPUE = sum(CPUE),
            mean_log_CPUE = mean(log_CPUE))


# add row for august to data frame for plotting

aug_row2 <- data.frame(Species_common_name = "Eulachon",
                      MONTH ="Aug", 
                      AreaGrouping = "A1",
                      mean_CPUE= 0, 
                      total_CPUE = 0, 
                      mean_log_CPUE=0)

CPUE_by_month_area <- rbind(CPUE_by_month_area, aug_row2)


CPUE_by_month_area$MONTH<-factor(CPUE_by_month_area$MONTH,
                                        levels=c("Jan", "Feb", "Mar", "Apr",
                                                 "May", "Jun", "Jul","Aug", 
                                                 "Sep", "Oct", "Nov", "Dec"))


# mean log CPUE for top species by month and area in a stacked bar graph

ggplot(CPUE_by_month_area )+
  geom_bar(aes(x=MONTH, y=mean_log_CPUE, fill=Species_common_name), stat = "identity")+
  scale_fill_manual(values = mycolour) +
  facet_wrap(~AreaGrouping)+
  labs(y= "Mean log(CPUE+1)", x= "Month", fill= "Species" )


# mean CPUE for top species by month and area in a stacked bar graph

ggplot(CPUE_by_month_area )+
  geom_bar(aes(x=MONTH, y=mean_CPUE, fill=Species_common_name), stat = "identity")+
  scale_fill_manual(values = mycolour) +
  facet_wrap(~AreaGrouping)+
  labs(y= "Mean CPUE", x= "Month", fill= "Species" )


# mean CPUE by month and area standardized to one  

ggplot(CPUE_by_month_area )+
  geom_bar(aes(x=MONTH, y=mean_CPUE, fill=Species_common_name), 
           stat = "identity", position = "fill")+
  scale_fill_manual(values = mycolour) +
  facet_wrap(~AreaGrouping)+
  labs(y= "Proportion", x= "Month", fill= "Species" )

#==============================================================================
# Figure 3. Bottom temperature, depth and salinity boxplots for distributions 
#==============================================================================





#===================================================================================
# Figure 4. Correlation plot among species for co-occurrence in the trawl hauls
#===================================================================================

species_cor <- cor(data.frame(catch_wide_log[,16:40]),use="complete.obs")

species_cor

corrplot(species_cor, tl.col = "black", tl.cex = 0.75)

#==============================================================================
# Figure 5. Bray-curtis dissimilarity dendrograms between months and areas
#==============================================================================

####### Cluster by trip #######

# cluster analysis of top 25 species abundance across the 17 trips using the average 
# CPUE across all hauls of a cruise pooled together

# adding year month values to dataset 

cluster_top25 <- catch_top25 %>% 
  mutate(., MONTH_YEAR = format(as.Date(catch_top25$DATE,format="%Y-%m-%d")
                                ,"%b-%y"), .after = MONTH)

mean_top25_log <- cluster_top25 %>% 
  group_by(., TRIP_ID, MAJOR_STAT_AREA_CODE, Species_common_name, SEASON) %>%
  summarise(., mean_log_CPUE = mean(log_CPUE))

mean_top25_log <- mean_top25_log %>% 
  pivot_wider(., names_from = Species_common_name, values_from = mean_log_CPUE) 


# use trip ID as row names 

mean_top25_log <- as.data.frame(mean_top25_log, row.names = mean_top25_log$TRIP_ID)
mean_top25_log <- as.data.frame(mean_top25_log, row.names = mean_top25_log$TRIP_ID)

trip_cluster_data <- mean_top25_log[,-c(1:3)]

# Hierarchical clustering by trip

apply(trip_cluster_data, 1, sum)

##### scale to 1? not done in other papers
# Turn CPUE to relative abundance by dividing each value by sample total abundance

#trip_cluster_data <- decostand(trip_cluster_data, method = "total")

# check total abundance in each sample

#apply(trip_cluster_data, 1, sum)


# calculate Bray-Curtis distance among samples

trip_dist <- vegdist(trip_cluster_data, method = "bray")

# cluster communities using average-linkage algorithm
trip_cluster <- hclust(trip_dist, method = "average")

# plot cluster diagram
plot(trip_cluster, ylab = "Bray-Curtis dissimilarity")

# cluster diagram coloured by area

trip_dend <- as.dendrogram(hclust(trip_dist, method = "average"))

order.dendrogram(trip_dend)

labels_colors(trip_dend)<-north_south_colours[mean_top25_log$MAJOR_STAT_AREA_CODE][order.dendrogram(trip_dend)]

plot(hang.dendrogram(trip_dend, hang = 0.1), ylab = "Bray-Curtis dissimilarity", main = "Cluster Dendrogram")
legend("topright", legend=c("North", "South"), title= "Area", pch=19, 
       col=c("blue", "green"))



######### Cluster by trip using date #######

mean_top25_log_date <- cluster_top25 %>% 
  group_by(., MONTH_YEAR, MAJOR_STAT_AREA_CODE, Species_common_name) %>%
  summarise(., mean_log_CPUE = mean(log_CPUE))

mean_top25_log_date <- mean_top25_log_date %>% 
  pivot_wider(., names_from = Species_common_name, values_from = mean_log_CPUE) 

# use trip ID as row names 

mean_top25_log_date <- as.data.frame(mean_top25_log_date, row.names = mean_top25_log_date$MONTH_YEAR)
mean_top25_log_date <- as.data.frame(mean_top25_log_date, row.names = mean_top25_log_date$MONTH_YEAR)

date_cluster_data <- mean_top25_log_date[,-c(1:2)]

#Hierarchical clustering 

apply(date_cluster_data, 1, sum)

# Turn CPUE to relative abundance by dividing each value by sample total abundance

#date_cluster_data <- decostand(date_cluster_data, method = "total")

# check total abundance in each sample

#apply(date_cluster_data, 1, sum)

# calculate Bray-Curtis distance among samples

date_dist <- vegdist(date_cluster_data, method = "bray")

# cluster communities using average-linkage algorithm
date_cluster <- hclust(date_dist, method = "average")

# plot cluster diagram
plot(date_cluster, ylab = "Bray-Curtis dissimilarity")

# cluster diagram coloured by area

date_dend <- as.dendrogram(hclust(date_dist, method = "average"))

order.dendrogram(date_dend)

labels_colors(date_dend)<-north_south_colours[mean_top25_log_date$MAJOR_STAT_AREA_CODE][order.dendrogram(date_dend)]

plot(hang.dendrogram(date_dend, hang = 0.1), ylab = "Bray-Curtis dissimilarity", main = "Cluster Dendrogram")
legend("topright", legend=c("North", "South"), title= "Area", pch=19, 
       col=c("blue", "green"))


####### Cluster by event and area #######

species_cluster_matrix <- catch_wide_log[,-c(1,3:15)]

cluster_groups <-  catch_wide_log[,c(2,9)]

# use event ID as row names 

species_cluster_matrix <- as.data.frame(species_cluster_matrix_area, 
                                        row.names = species_cluster_matrix$EVENT_ID)
species_cluster_matrix <- as.data.frame(species_cluster_matrix, 
                                        row.names = species_cluster_matrix$EVENT_ID)

species_cluster_matrix <- species_cluster_matrix[,-1]


# calculate Bray-Curtis distance among samples

event_dist <- vegdist(species_cluster_matrix, method = "bray")

# cluster communities using average-linkage algorithm
event_cluster <- hclust(event_dist, method = "average")

# plot cluster diagram
plot(event_cluster, ylab = "Bray-Curtis dissimilarity")

# cluster diagram coloured by area

event_dend <- as.dendrogram(hclust(event_dist, method = "average"))

order.dendrogram(event_dend)

labels_colors(event_dend)<-area_colours[catch_wide_log$AreaGrouping][order.dendrogram(event_dend)]

plot(hang.dendrogram(event_dend), ylab = "Bray-Curtis dissimilarity",
     main = "Cluster Dendrogram")
legend("topright", legend=c("A1", "A2","A3","A4"), title= "Area", pch=19, 
       col=area_colours)


####### Cluster by trip and area #######

# cluster analysis of top 25 species abundance across the 17 trips using the average 
# CPUE across all hauls of a cruise pooled together

mean_top25_log_area <- cluster_top25 %>% 
  group_by(., TRIP_ID, AreaGrouping, Species_common_name) %>%
  summarise(., mean_log_CPUE = mean(log_CPUE))

mean_top25_log_area <- mean_top25_log_area %>% 
  pivot_wider(., names_from = Species_common_name, values_from = mean_log_CPUE) 


# use trip ID as row names 

mean_top25_log_area <- as.data.frame(mean_top25_log_area, 
                                     row.names = mean_top25_log_area$TRIP_ID)
mean_top25_log_area <- as.data.frame(mean_top25_log_area, 
                                     row.names = mean_top25_log_area$TRIP_ID)

area_cluster_data <- mean_top25_log_area[,-c(1:2)]

# Hierarchical clustering by trip

apply(area_cluster_data, 1, sum)

##### scale to 1? not done in other papers
# Turn CPUE to relative abundance by dividing each value by sample total abundance

#trip_cluster_data <- decostand(trip_cluster_data, method = "total")

# check total abundance in each sample

#apply(trip_cluster_data, 1, sum)


# calculate Bray-Curtis distance among samples

area_dist <- vegdist(area_cluster_data, method = "bray")

# cluster communities using average-linkage algorithm
area_cluster <- hclust(area_dist, method = "average")

# plot cluster diagram
plot(area_cluster, ylab = "Bray-Curtis dissimilarity")

# cluster diagram coloured by area

area_dend <- as.dendrogram(hclust(area_dist, method = "average"))

order.dendrogram(area_dend)

labels_colors(area_dend)<-area_colours[mean_top25_log_area$AreaGrouping][order.dendrogram(area_dend)]

plot(hang.dendrogram(area_dend, hang = 0.1), ylab = "Bray-Curtis dissimilarity", main = "Cluster Dendrogram")
legend("topright", legend=c("A1", "A2","A3","A4"), title= "Area", pch=19, 
       col=area_colours)

######### Cluster by trip and area using date #######

mean_top25_log_date_area <- cluster_top25 %>% 
  group_by(., MONTH_YEAR, AreaGrouping, Species_common_name) %>%
  summarise(., mean_log_CPUE = mean(log_CPUE))

mean_top25_log_date_area <- mean_top25_log_date_area %>% 
  pivot_wider(., names_from = Species_common_name, values_from = mean_log_CPUE) 

# use trip ID as row names 

mean_top25_log_date_area <- as.data.frame(mean_top25_log_date_area, 
                                          row.names = mean_top25_log_date_area$MONTH_YEAR)
mean_top25_log_date_area <- as.data.frame(mean_top25_log_date_area, 
                                          row.names = mean_top25_log_date_area$MONTH_YEAR)

date_area_cluster_data <- mean_top25_log_date_area[,-c(1:2)]

#Hierarchical clustering 

apply(date_area_cluster_data, 1, sum)

# Turn CPUE to relative abundance by dividing each value by sample total abundance

#date_cluster_data <- decostand(date_cluster_data, method = "total")

# check total abundance in each sample

#apply(date_cluster_data, 1, sum)

# calculate Bray-Curtis distance among samples

date_area_dist <- vegdist(date_area_cluster_data, method = "bray")

# cluster communities using average-linkage algorithm
date_area_cluster <- hclust(date_area_dist, method = "average")

# plot cluster diagram
plot(date_area_cluster, ylab = "Bray-Curtis dissimilarity")

# cluster diagram coloured by area

date_area_dend <- as.dendrogram(hclust(date_area_dist, method = "average"))

order.dendrogram(date_area_dend)

labels_colors(date_area_dend)<-area_colours[mean_top25_log_date_area$AreaGrouping][order.dendrogram(date_area_dend)]

plot(hang.dendrogram(date_area_dend, hang = 0.1), ylab = "Bray-Curtis dissimilarity", main = "Cluster Dendrogram")
legend("topright", legend=c("A1", "A2","A3","A4"), title= "Area", pch=19, 
       col=area_colours)


######### Cluster by species #######

# cluster analysis of top 25 species CPUE across all events/halls 
# to examine which species had similar distributions among the hauls

# filter event id and species CPUE

species_cluster_data <- catch_wide_log[,-c(1,3:15)]

# use event ID as row names 

species_cluster_data <- as.data.frame(species_cluster_data, row.names = species_cluster_data$EVENT_ID)
species_cluster_data <- as.data.frame(species_cluster_data, row.names = species_cluster_data$EVENT_ID)

species_cluster_data <- species_cluster_data[,-1]

# transpose matrix 

species_cluster_data <- t(species_cluster_data)



####### Hierarchical clustering ########

apply(species_cluster_data, 1, sum)

# convert the CPUE of each species from each event to the proportion of the total catch of
# that species in all events during all trips of the study (relativizing by species totals)

species_cluster_data <- decostand(species_cluster_data, method = "total")

# check total abundance in each sample

apply(species_cluster_data, 1, sum)

# calculate Bray-Curtis distance among species

species_dist <- vegdist(species_cluster_data, method = "bray")

# cluster communities using average-linkage algorithm
species_cluster <- hclust(species_dist, method = "average")

# plot cluster diagram
plot(species_cluster, ylab = "Bray-Curtis dissimilarity")


# adjusted cluster diagram 

species_dend <- as.dendrogram(hclust(species_dist, method = "average"))

plot(species_dend, ylab = "Bray-Curtis dissimilarity",
     main = "Cluster Dendrogram", horiz = TRUE)

cutree(species_dend, k=9)


#====================================
# indicator species analysis 
#====================================

#trip_km = kmeans(trip_cluster_data, centers=5)
#groupskm = trip_km$cluster
#groupskm


###### using area as groupings 

# filter event id and species CPUE

species_cluster_matrix <- catch_wide_log[,-c(1,3:15)]

cluster_groups <-  catch_wide_log[,c(2,9)]

# use event ID as row names 

species_cluster_matrix <- as.data.frame(species_cluster_matrix, 
                                        row.names = species_cluster_matrix$EVENT_ID)
species_cluster_matrix <- as.data.frame(species_cluster_matrix, 
                                        row.names = species_cluster_matrix$EVENT_ID)

species_cluster_matrix <- species_cluster_matrix[,-1]


indval_area <- multipatt(species_cluster_matrix, cluster_groups$AreaGrouping, 
                         control = how(nperm=999))

summary(indval_area, indvalcomp=TRUE)
# Component ‘A’ is the probability that the surveyed
# site belongs to the target site group given the fact that the species has
# been found. This conditional probability is called the specificity or positive
# predictive value of the species as indicator of the site group. (2) Component
#‘B’ is the probability of finding the species in sites belonging to the site group.
# This second conditional probability is called the fidelity or sensitivity of the
# species as indicator of the target site group.
#http://www2.uaem.mx/r-mirror/web/packages/indicspecies/vignettes/indicspeciesTutorial.pdf


summary(indval_area, alpha=1)
# By setting alpha = 1 we say
# we want to display the group to which each species is associated, regardless
# of whether the association significant or not.
# still gives 18 species so those species that occur in sites belonging to all groups
# cannot be statistically tested, because there is no external group for comparison
# 25-18=7 species associated with all sites

indval_area$sign
# species with the highest IndVal corresponded to the set of all sites 
# as indicated by the NAs in the p.value column : 
# Pacific herring 
# Walleye pollock
# Rex sole               
# Flathead sole            
# Spotted ratfish
# Pacific dover sole
# Eulachon 

### Indicator species analysis without site groups combinations

indval_single_area <- multipatt(species_cluster_matrix, cluster_groups$AreaGrouping, 
                         duleg= TRUE, control = how(nperm=999))

summary(indval_single_area)
summary(indval_single_area,indvalcomp=TRUE)

indval_single_area$sign
# only Spotted ratfish did not associate to a group

### Restricting the order of site groups combinations

# max 2 site combinations 

indval_2_area <- multipatt(species_cluster_matrix, cluster_groups$AreaGrouping, 
                           max.order = 2, control = how(nperm=999))

summary(indval_2_area,indvalcomp=TRUE)

# max 3 site combinations 

indval_3_area <- multipatt(species_cluster_matrix, cluster_groups$AreaGrouping, 
                           max.order = 3, control = how(nperm=999))

summary(indval_3_area,indvalcomp=TRUE)


# combining species 

species_comb = combinespecies(species_cluster_matrix, max.order = 2)$XC
 dim(species_comb)

indvalspcomb = multipatt(species_comb, cluster_groups$AreaGrouping, duleg = TRUE,
                          control = how(nperm=999))
summary(indvalspcomb, indvalcomp = TRUE)


#==============================================================================
# Figure 6. NMDS plots of species with secondary temperature, depth axes
#==============================================================================

####### ordination ########

# The metaMDS function automatically transforms data and checks solution
# robustness

event_mds <- metaMDS(species_cluster_matrix, dist = "bray", trymax = 150, k=3,
                     maxit = 300)

# Assess goodness of ordination fit (stress plot)
stressplot(event_mds)


# colour plot north vs south

blank_mds<- ordiplot(event_mds, type = "n")
points(blank_mds, "sites", col = "blue", 
       select=catch_wide_log$MAJOR_STAT_AREA_CODE=="8", pch=3, cex=0.5)
points(blank_mds, "sites", col = "green", 
       select=catch_wide_log$MAJOR_STAT_AREA_CODE=="1", pch=3, cex=0.5)
text(blank_mds, "species", col = "black", cex=0.6)

ordiellipse(blank_mds, catch_wide_log$MAJOR_STAT_AREA_CODE, conf = 0.95, label = FALSE)

legend("topright", legend=c("North", "South"), title= "Area", pch=19, 
       col=c("blue", "green"))

# add temp and depth axes to the plot

plot(envfit(blank_mds, catch_wide_log[, 14:15]), cex = 0.75)


# colour plot by area with ellipse

blank_mds<- ordiplot(event_mds, type = "n")
points(blank_mds, "sites", col = "blue", 
       select=catch_wide_log$AreaGrouping=="A1", pch=3, cex=0.5)
points(blank_mds, "sites", col = "orange", 
       select=catch_wide_log$AreaGrouping=="A2", pch=3, cex=0.5)
points(blank_mds, "sites", col = "red", 
       select=catch_wide_log$AreaGrouping=="A3", pch=3, cex=0.5)
points(blank_mds, "sites", col = "green", 
       select=catch_wide_log$AreaGrouping=="A4", pch=3, cex=0.5)
text(blank_mds, "species", col = "black", cex=0.6)

ordiellipse(blank_mds, catch_wide_log$AreaGrouping, conf = 0.95, 
            label = FALSE, col = area_colours)
legend("topright", legend=c("A1", "A2", "A3", "A4"), title= "Area", pch=19, 
       col=area_colours)
# add temp and depth axes to the plot

plot(envfit(blank_mds, catch_wide_log[, 14:15]), cex = 0.75)




##### filter by area - A1 #####

# set plot window to display all 4 area graphs at the same time
par(mfrow=c(2,2))

a1_data <- catch_wide_log %>% filter(., AreaGrouping=="A1")
  
NMDS_a1 <- a1_data[,-c(1,3:15)]

# use event ID as row names 

NMDS_a1 <- as.data.frame(NMDS_a1, row.names = NMDS_a1$EVENT_ID)
NMDS_a1 <- as.data.frame(NMDS_a1, row.names = NMDS_a1$EVENT_ID)

NMDS_a1 <- NMDS_a1[,-1]

# The metaMDS function automatically transforms data and checks solution
# robustness

mds_a1 <- metaMDS(NMDS_a1, dist = "bray", trymax = 150, k=3,
                           maxit = 300)

# Assess goodness of ordination fit (stress plot)

#stressplot(mds_a1)

# layering the plot 

blank_mds_a1<- ordiplot(mds_a1, type = "n")
points(blank_mds_a1, "sites", col = "blue", 
       select=a1_data$SEASON=="Winter", pch=3, cex=0.5)
points(blank_mds_a1, "sites", col = "orange", 
       select=a1_data$SEASON=="Spring", pch=3, cex=0.5)
points(blank_mds_a1, "sites", col = "red", 
       select=a1_data$SEASON=="Summer", pch=3, cex=0.5)
points(blank_mds_a1, "sites", col = "green", 
       select=a1_data$SEASON=="Fall", pch=3, cex=0.5)
text(blank_mds_a1, "species", col = "black", cex=0.6)


ordiellipse(blank_mds_a1, a1_data$SEASON, 
            conf = 0.95, label = FALSE, col = season_colours)

legend("topright", legend=c("Winter", "Spring", "Summer", "Fall"), title= "Season", pch=19, 
       col=season_colours)
title(main = "A1")

# add temp and depth axis

plot(envfit(blank_mds_a1, a1_data[, 14:15]), cex = 0.75, col = "black")


##### filter by area - A2 #####

a2_data <- catch_wide_log %>% filter(., AreaGrouping=="A2")

NMDS_a2 <- a2_data[,-c(1,3:15)]

# use event ID as row names 

NMDS_a2 <- as.data.frame(NMDS_a2, row.names = NMDS_a2$EVENT_ID)
NMDS_a2 <- as.data.frame(NMDS_a2, row.names = NMDS_a2$EVENT_ID)

NMDS_a2 <- NMDS_a2[,-1]

# The metaMDS function automatically transforms data and checks solution
# robustness

mds_a2 <- metaMDS(NMDS_a2, dist = "bray", trymax = 150, k=3,
                  maxit = 300)

# Assess goodness of ordination fit (stress plot)

#stressplot(mds_a2)

# layering the plot 

blank_mds_a2<- ordiplot(mds_a2, type = "n")
points(blank_mds_a2, "sites", col = "blue", 
       select=a2_data$SEASON=="Winter", pch=3, cex=0.5)
points(blank_mds_a2, "sites", col = "orange", 
       select=a2_data$SEASON=="Spring", pch=3, cex=0.5)
points(blank_mds_a2, "sites", col = "red", 
       select=a2_data$SEASON=="Summer", pch=3, cex=0.5)
points(blank_mds_a2, "sites", col = "green", 
       select=a2_data$SEASON=="Fall", pch=3, cex=0.5)
text(blank_mds_a2, "species", col = "black", cex=0.6)


ordiellipse(blank_mds_a2, a2_data$SEASON, 
            conf = 0.95, label = FALSE, col = season_colours)

legend("topright", legend=c("Winter", "Spring", "Summer", "Fall"), title= "Season", pch=19, 
       col=season_colours)
title(main = "A2")

# add temp and depth axis

plot(envfit(blank_mds_a2, a2_data[, 14:15]), cex = 0.75, col = "black")


##### filter by area - A3 #####

a3_data <- catch_wide_log %>% filter(., AreaGrouping=="A3")

NMDS_a3 <- a3_data[,-c(1,3:15)]

# use event ID as row names 

NMDS_a3 <- as.data.frame(NMDS_a3, row.names = NMDS_a3$EVENT_ID)
NMDS_a3 <- as.data.frame(NMDS_a3, row.names = NMDS_a3$EVENT_ID)

NMDS_a3 <- NMDS_a3[,-1]

# The metaMDS function automatically transforms data and checks solution
# robustness

mds_a3 <- metaMDS(NMDS_a3, dist = "bray", trymax = 150, k=3,
                  maxit = 300)

# Assess goodness of ordination fit (stress plot)

#stressplot(mds_a3)

# layering the plot 

blank_mds_a3<- ordiplot(mds_a3, type = "n")
points(blank_mds_a3, "sites", col = "blue", 
       select=a3_data$SEASON=="Winter", pch=3, cex=0.5)
points(blank_mds_a3, "sites", col = "orange", 
       select=a3_data$SEASON=="Spring", pch=3, cex=0.5)
points(blank_mds_a3, "sites", col = "red", 
       select=a3_data$SEASON=="Summer", pch=3, cex=0.5)
points(blank_mds_a3, "sites", col = "green", 
       select=a3_data$SEASON=="Fall", pch=3, cex=0.5)
text(blank_mds_a3, "species", col = "black", cex=0.6)


ordiellipse(blank_mds_a3, a3_data$SEASON, 
            conf = 0.95, label = FALSE, col = season_colours)

legend("topright", legend=c("Winter", "Spring", "Summer", "Fall"), title= "Season", pch=19, 
       col=season_colours)
title(main = "A3")

# add temp and depth axis

plot(envfit(blank_mds_a3, a3_data[, 14:15]), cex = 0.75, col = "black")

##### filter by area - A4 #####

a4_data <- catch_wide_log %>% filter(., AreaGrouping=="A4")

NMDS_a4 <- a4_data[,-c(1,3:15)]

# use event ID as row names 

NMDS_a4 <- as.data.frame(NMDS_a4, row.names = NMDS_a4$EVENT_ID)
NMDS_a4 <- as.data.frame(NMDS_a4, row.names = NMDS_a4$EVENT_ID)

NMDS_a4 <- NMDS_a4[,-1]

# The metaMDS function automatically transforms data and checks solution
# robustness

mds_a4 <- metaMDS(NMDS_a4, dist = "bray", trymax = 150, k=3,
                  maxit = 300)

# Assess goodness of ordination fit (stress plot)

#stressplot(mds_a4)

# layering the plot 

blank_mds_a4<- ordiplot(mds_a4, type = "n")
points(blank_mds_a4, "sites", col = "blue", 
       select=a4_data$SEASON=="Winter", pch=3, cex=0.5)
points(blank_mds_a4, "sites", col = "orange", 
       select=a4_data$SEASON=="Spring", pch=3, cex=0.5)
points(blank_mds_a4, "sites", col = "red", 
       select=a4_data$SEASON=="Summer", pch=3, cex=0.5)
points(blank_mds_a4, "sites", col = "green", 
       select=a4_data$SEASON=="Fall", pch=3, cex=0.5)
text(blank_mds_a4, "species", col = "black", cex=0.6)


ordiellipse(blank_mds_a4, a4_data$SEASON, 
            conf = 0.95, label = FALSE, col = season_colours)

legend("topright", legend=c("Winter", "Spring", "Summer", "Fall"), 
       title= "Season", pch=19,col=season_colours)
title(main = "A4")

# add temp and depth axis

plot(envfit(blank_mds_a4, a4_data[, 14:15]), cex = 0.75, col = "black")

par(mfrow=c(1,1))

#==============================================================
# Figure 7. Maps of showing cluster locations by month
#==============================================================

# A1 - no results 

B=strassoc(species_cluster_matrix, cluster=cluster_groups$AreaGrouping ,func="B")
 sel=which(B[,"A1"]>0.2)
 sel

 sc= indicators(X=species_cluster_matrix[,sel], 
                cluster=cluster_groups$AreaGrouping, group="A1", verbose=TRUE,
                 At=0.5, Bt=0.2)
 
 print(sc, sqrtIVt = 0.6)
 
 # A2 - no results 
 
 B=strassoc(species_cluster_matrix, cluster=cluster_groups$AreaGrouping ,func="B")
 sel=which(B[,"A2"]>0.2)
 sel
 
 sc= indicators(X=species_cluster_matrix[,sel], 
                cluster=cluster_groups$AreaGrouping, group="A2", verbose=TRUE,
                At=0.5, Bt=0.2)
 
 print(sc, sqrtIVt = 0.6)
 
 
 # A3 - Spotted ratfish, Slender soles, North pacific hake, Sidestripe shrimp

 B=strassoc(species_cluster_matrix, cluster=cluster_groups$AreaGrouping ,func="B")
 sel=which(B[,"A3"]>0.2)
 sel
 
 sc= indicators(X=species_cluster_matrix[,sel], 
                cluster=cluster_groups$AreaGrouping, group="A3", verbose=TRUE,
                At=0.5, Bt=0.2)
 
 print(sc, sqrtIVt = 0.8)

 
 # A4
 
 B=strassoc(species_cluster_matrix, cluster=cluster_groups$AreaGrouping ,func="B")
 sel=which(B[,"A4"]>0.2)
 sel
 
 sc= indicators(X=species_cluster_matrix[,sel], 
                cluster=cluster_groups$AreaGrouping, group="A4", verbose=TRUE,
                At=0.5, Bt=0.2)
 
 print(sc, sqrtIVt = 0.8)
