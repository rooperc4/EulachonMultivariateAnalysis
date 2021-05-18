#===========================================
# title: "eulachon_test_summary"
# author: Brooke Hackett
# date: April 26 2021
# packages:
library(rio)
library(ggplot2)
library(lubridate)
library(dplyr)
library(magrittr)
library(tidyr)
library(Hmisc)
library(RColorBrewer)
#===========================================
# Data import and preparation 
#===========================================

# import data sets

taxa_data <- import("C:\\Users\\hackettb\\Documents\\R\\eulachon_project\\TaxaGroups.csv")

catch_data <- import("C:\\Users\\hackettb\\Documents\\R\\eulachon_project\\catch.csv")

# merge data sets

taxa_catch_data <- merge(catch_data, taxa_data, by = "Species_name")

head(taxa_catch_data)
str(taxa_catch_data)

# change species name, taxonomic group, and area code to factor 

taxa_catch_data$Species_name <- factor(taxa_catch_data$Species_name)

taxa_catch_data$Taxonomic_group <- factor(taxa_catch_data$Taxonomic_group)

taxa_catch_data$MAJOR_STAT_AREA_CODE <- factor(taxa_catch_data$MAJOR_STAT_AREA_CODE)

str(taxa_catch_data)

# manipulate date variables with lubridate

taxa_catch_data$DATE <- ymd(taxa_catch_data$DATE)

taxa_catch_data$MONTH <- month(taxa_catch_data$DATE, label = TRUE, abbr = TRUE)

taxa_catch_data$YEAR <- year(taxa_catch_data$DATE)

str(taxa_catch_data)

# add log CPUE

log_CPUE_data <- taxa_catch_data %>% 
  mutate(., log_CPUE = log(CPUE+1))

head(log_CPUE_data)
str(log_CPUE_data)

# remove irrelivant taxa groups and area 3 data

eulachon_data <- log_CPUE_data %>%
  filter(., Taxonomic_group %nin% c("Marine mammal",
                                    "Other",
                                    "Euphausid",
                                    "Worm",
                                    "Isopod"),
         MAJOR_STAT_AREA_CODE %nin% 3)

str(eulachon_data)

# test 

eulachon_test <- log_CPUE_data %>%
  filter(.,
         MAJOR_STAT_AREA_CODE %nin% 3)

test2 <- log_CPUE_data %>%
  filter(.,
         MAJOR_STAT_AREA_CODE %in% 3)


test_3 <- catch_data %>% filter(., 
           MONTH==11,
           YEAR==2017)
test4 <- test_3 %>% filter(., Species_name=="THALEICHTHYS.PACIFICUS")

test5 <- south_data %>% filter(., YEAR!=2019)

test6 <- south_data %>% filter(., YEAR==2019)

test7 <- north_data %>% filter(., MONTH=="Oct" )
unique(test7$EVENT_ID)


test8 <- test7 %>% filter(., SET_ID == c(20, 21))

test10 <- log_CPUE_data %>%
  filter(., Species_name!="THALEICHTHYS.PACIFICUS") %>%
         group_by(EVENT_ID) %>%
           summarise(., total_cpue = sum(CPUE))
         
View(test10)


test11 <- log_CPUE_data %>%
  group_by(EVENT_ID) %>%
  summarise(., total_cpue = sum(CPUE))

View(test11)

test12<- log_CPUE_data %>%
  filter(., EVENT_ID==4363883)

# factor species name, taxonomic group, and area code again 
# with eulachon_data to get rid of extra levels that were filtered out  

eulachon_data$Species_name <- factor(eulachon_data$Species_name)

eulachon_data$Taxonomic_group <- factor(eulachon_data$Taxonomic_group)

eulachon_data$MAJOR_STAT_AREA_CODE <- factor(eulachon_data$MAJOR_STAT_AREA_CODE)

str(eulachon_data)

# creating data table in wide format

eulachon_wide <- pivot_wider(eulachon_data, 
                            names_from =Species_name,
                            values_from = CPUE,
                            names_prefix = "CPUE for ")
View(eulachon_wide)


#=======================================
# trying different plots 
#=======================================

# average CPUE for each species by month

ggplot(data = eulachon_data, aes(x=Species_name, y=CPUE)) +
  stat_summary(fun = mean,
               geom = "bar") +
  facet_wrap(~ MONTH)

# average CPUE for each species by year

ggplot(data = eulachon_data, aes(x=Species_name, y=CPUE)) +
  stat_summary(fun = mean,
               geom = "bar") +
  facet_wrap(~ YEAR)

# average CPUE for eulachon by month

eulachon_data %>% 
  filter(Species_name %in% "THALEICHTHYS.PACIFICUS") %>%
  ggplot(aes(x = MONTH, y=CPUE)) +
  stat_summary(fun = mean,
               geom = "bar")

# average CPUE for eulachon by year

eulachon_data %>% 
  filter(Species_name %in% "THALEICHTHYS.PACIFICUS") %>%
  ggplot(aes(x = YEAR, y=CPUE)) +
  stat_summary(fun = mean,
               geom = "bar")

# average CPUE for eulachon by month and year

eulachon_data %>% 
  filter(Species_name %in% "THALEICHTHYS.PACIFICUS") %>%
  ggplot(aes(x = MONTH, y=CPUE)) +
  stat_summary(fun = mean,
               geom = "bar") +
  facet_wrap(~ YEAR)

eulachon_data %>% 
  filter(Species_name %in% "THALEICHTHYS.PACIFICUS") %>%
  ggplot(aes(x = YEAR, y=CPUE)) +
  stat_summary(fun = mean,
               geom = "bar") +
  facet_wrap(~ MONTH)

## average log CPUE for eulachon by month and year

eulachon_data %>% 
  filter(Species_name %in% "THALEICHTHYS.PACIFICUS") %>%
  ggplot(aes(x = MONTH, y=log_CPUE)) +
  stat_summary(fun = mean,
               geom = "bar") +
  facet_wrap(~ YEAR)

eulachon_data %>% 
  filter(Species_name %in% "THALEICHTHYS.PACIFICUS") %>%
  ggplot(aes(x = YEAR, y=log_CPUE)) +
  stat_summary(fun = mean,
               geom = "bar") +
  facet_wrap(~ MONTH)

# CPUE vs depth for Eulachon 

eulachon_data %>% 
  filter(Species_name %in% "THALEICHTHYS.PACIFICUS") %>% 
  ggplot(aes(x=DEPTH, y=CPUE))+
  geom_point() +
  geom_smooth()

## log CPUE vs depth for Eulachon 

eulachon_data %>% 
  filter(Species_name %in% "THALEICHTHYS.PACIFICUS") %>% 
  ggplot(aes(x=DEPTH, y=log_CPUE))+
  geom_point() +
  geom_smooth()

#====================================
# top 20 species by CPUE
#====================================
 
# total CPUE all species 

sum(taxa_catch_data$CPUE)
#[1] 10027059

# total CPUE filtered species list

sum(eulachon_data$CPUE)
# [1] 9907000


# total and mean CUPE by species summary

CPUE_summary <- eulachon_data %>% 
  group_by(Species_name) %>%
  summarise(., MEAN_CPUE_BY_SPECIES = mean(CPUE),
            TOTAL_CPUE_BY_SPECIES = sum(CPUE)) %>% 
  arrange(desc(MEAN_CPUE_BY_SPECIES), TOTAL_CPUE_BY_SPECIES) %>% 
  data.frame()

CPUE_summary

# create new data frame with top 20 species by CPUE 
# (same species for both mean and total CPUE)

top_20_CPUE <- eulachon_data %>%
  filter(., Species_name%in%c("HYDROLAGUS.COLLIEI", 
                                "GADUS.CHALCOGRAMMUS", 
                                "SQUALUS.SUCKLEYI",
                                "PANDALOPSIS.DISPAR",
                                "HIPPOGLOSSOIDES.ELASSODON",
                                "ATHERESTHES.STOMIAS",
                                "PAROPHRYS.VETULUS",
                                "THALEICHTHYS.PACIFICUS",
                                "RAJA.RHINA",
                                "LYOPSETTA.EXILIS",
                                "LYCODES.PACIFICUS",
                                "MICROSTOMUS.PACIFICUS",
                                "GLYPTOCEPHALUS.ZACHIRUS",
                                "MERLUCCIUS.PRODUCTUS",
                                "PANDALUS.JORDANI",
                                "BERINGRAJA.BINOCULATA",
                                "ANOPLOPOMA.FIMBRIA",
                                "CLUPEA.PALLASII",
                                "PANDALUS.BOREALIS",
                                "GADUS.MACROCEPHALUS"))


#=====================================
# filtering by area (north vs south)
#====================================

# area 1 - south

south_data <- eulachon_data %>%
  filter(., MAJOR_STAT_AREA_CODE %in% 1)


# area 8 - north

north_data <- eulachon_data %>%
  filter(., MAJOR_STAT_AREA_CODE %in% 8)


#==========================================
# top 20 species by area
#=========================================

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
                              "PANDALUS.JORDANI",
                              "SQUALUS.SUCKLEYI",
                              "RAJA.RHINA",
                              "GLYPTOCEPHALUS.ZACHIRUS",
                              "MICROSTOMUS.PACIFICUS",
                              "ANOPLOPOMA.FIMBRIA",
                              "PAROPHRYS.VETULUS",
                              "PANDALUS.BOREALIS",
                              "CLUPEA.PALLASII",
                              "PECTINIDAE",
                              "LYCODES.BREVIPES",
                              "ACTINIARIA"))

#======================================================                
# getting distinct colour palette 
#======================================================


mycolour <- c('#e6194B', '#3cb44b', '#ffe119', '#4363d8', 
              '#f58231', '#911eb4', '#42d4f4', '#f032e6', 
              '#bfef45', '#fabed4', '#469990', '#dcbeff', 
              '#9A6324', '#fffac8', '#800000', '#aaffc3', 
              '#808000', '#ffd8b1', '#000075', '#a9a9a9')



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

mycolour2 <- primary.colors(26)

# "#000000" "#800000" "#FF0000" "#008000" "#808000" "#FF8000" "#00FF00" "#80FF00" "#FFFF00"
# "000080" "#800080" "#FF0080" "#008080" "#808080" "#FF8080" "#00FF80" "#80FF80" "#FFFF80"
# "#0000FF" "#8000FF" "#FF00FF" "#0080FF" "#8080FF" "#FF80FF" "#00FFFF" "#80FFFF"


c('#e6194B', '#3cb44b', '#ffe119', '#4363d8', 
  '#f58231', '#911eb4', '#42d4f4', '#f032e6', 
  '#bfef45', '#fabed4', '#469990', '#dcbeff', 
  '#9A6324', '#fffac8', '#800000', '#aaffc3', 
  '#808000', '#ffd8b1', '#000075', '#a9a9a9')



mycolour3 <- c("#000000","#80FF80", "#FF0000", "#008000", "#808000", 
               "#FF8000", "#00FF00",'#dcbeff', "#FFFF00", "000080", 
               "#800080", "#FF0080", "#008080", "#808080", "#FF8080", 
               "#00FF80", "#00FFFF", "#FFFF80", "#0000FF", "#8000FF",
               "#FF00FF", "#0080FF", "#8080FF", "#FF80FF","#800000", "#80FFFF")

  
  
  
  
  
  
#======================================================
# test graphs with top 20 species across all surveys 
#======================================================

# average CPUE for top species by month for all areas

ggplot(data = top_20_CPUE, aes(x=Species_name, y=CPUE, fill = Species_name)) +
  stat_summary(fun = mean,
               geom = "bar") +
  facet_wrap(~ MONTH) +
  theme(axis.text.x = element_blank(),
        axis.ticks.x.bottom = element_blank())

# average CPUE for top species by year for all areas

ggplot(data = top_20_CPUE, aes(x=Species_name, y=CPUE, fill = Species_name)) +
  stat_summary(fun = mean,
               geom = "bar") +
  facet_wrap(~ YEAR) +
  theme(axis.text.x = element_blank(),
        axis.ticks.x.bottom = element_blank())
  
# average CPUE for top species by month in the same graph

CPUE_by_month <- top_20_CPUE %>%
  group_by(., Species_name, MONTH) %>%
  summarise(., mean_CPUE = mean(CPUE),
            total_CPUE = sum(CPUE))


ggplot(CPUE_by_month, )+
  geom_bar(aes(x=MONTH, y=mean_CPUE, fill=Species_name), stat = "identity")+
  scale_fill_manual(values = mycolour) 

# using log CPUE

log_CPUE_by_month <- top_20_CPUE %>%
  group_by(., Species_name, MONTH) %>%
  summarise(., mean_log_CPUE = mean(log_CPUE))

ggplot(log_CPUE_by_month)+
  geom_bar(aes(x=MONTH, y=mean_log_CPUE, fill=Species_name), stat = "identity")+
  scale_fill_manual(values = mycolour) 


# average CPUE for top species by year in the same graph

CPUE_by_year <- top_20_CPUE %>%
  group_by(., Species_name, YEAR) %>%
  summarise(., mean_CPUE = mean(CPUE),
            total_CPUE = sum(CPUE))
          
ggplot(CPUE_by_year)+
  geom_bar(aes(x=YEAR, y=mean_CPUE, fill=Species_name), stat = "identity")+
  scale_fill_manual(values = mycolour) 

# using log CPUE

log_CPUE_by_year <- top_20_CPUE %>%
  group_by(., Species_name, YEAR) %>%
  summarise(., mean_log_CPUE = mean(log_CPUE))

ggplot(log_CPUE_by_year)+
  geom_bar(aes(x=YEAR, y=mean_log_CPUE, fill=Species_name), stat = "identity")+
  scale_fill_manual(values = mycolour) 


# average CPUE for top species by month and by year in the same graph

CPUE_by_month_year <- top_20_CPUE %>%
  group_by(., Species_name, MONTH, YEAR) %>%
  summarise(., mean_CPUE = mean(CPUE),
            total_CPUE = sum(CPUE))

ggplot(CPUE_by_month_year )+
  geom_bar(aes(x=MONTH, y=mean_CPUE, fill=Species_name), stat = "identity")+
  scale_fill_manual(values = mycolour) +
  facet_grid(.~YEAR, scales = "free_x", space = "free")

# using log CPUE

log_CPUE_by_month_year <- top_20_CPUE %>%
  group_by(., Species_name, MONTH, YEAR) %>%
  summarise(., mean_log_CPUE = mean(log_CPUE))

ggplot(log_CPUE_by_month_year )+
  geom_bar(aes(x=MONTH, y=mean_log_CPUE, fill=Species_name), stat = "identity")+
  scale_fill_manual(values = mycolour) +
  facet_grid(.~YEAR, scales = "free_x", space = "free")



#======================================================
# test graphs with top 20 species by area
#======================================================

# combine top 20 North and South data sets

top_north_south_data <- rbind(top_20_CPUE_north, top_20_CPUE_south)

# average CPUE for top species by by areas

ggplot(data = top_north_south_data, aes(x=Species_name, y=CPUE, fill= Species_name)) +
  stat_summary(fun = mean,
               geom = "bar") +
  facet_wrap(~ MAJOR_STAT_AREA_CODE) +
  theme(axis.text.x = element_blank(),
        axis.ticks.x.bottom = element_blank())

# average CPUE for top species by year and area

ggplot(data = top_north_south_data, aes(x=Species_name, y=CPUE, fill = Species_name)) +
  stat_summary(fun = mean,
               geom = "bar") +
  facet_wrap(MAJOR_STAT_AREA_CODE ~ YEAR) +
  theme(axis.text.x = element_blank(),
        axis.ticks.x.bottom = element_blank())

# average CPUE for top species by month and area

ggplot(data = top_north_south_data, aes(x=Species_name, y=CPUE, fill = Species_name)) +
  stat_summary(fun = mean,
               geom = "bar") +
  facet_grid(MAJOR_STAT_AREA_CODE ~ MONTH) +
  theme(axis.text.x = element_blank(),
        axis.ticks.x.bottom = element_blank())


# making area labels 

area_label <- as_labeller(c( "1" = "South",
                             "8" = "North"))


# average CPUE for top species by month and area in the same graph

CPUE_by_month_area <- top_north_south_data %>%
  group_by(., Species_name, MONTH, MAJOR_STAT_AREA_CODE) %>%
  summarise(., mean_CPUE = mean(CPUE),
            total_CPUE = sum(CPUE),
            mean_log_CPUE = mean(log_CPUE))


ggplot(CPUE_by_month_area )+
  geom_bar(aes(x=MONTH, y=mean_CPUE, fill=Species_name), stat = "identity")+
  scale_fill_manual(values = mycolour3) +
  facet_wrap(~MAJOR_STAT_AREA_CODE, labeller = area_label)


# using mean log CPUE for top species by month and area in the same graph

ggplot(CPUE_by_month_area )+
  geom_bar(aes(x=MONTH, y=mean_log_CPUE, fill=Species_name), stat = "identity")+
  scale_fill_manual(values = mycolour3) +
  facet_wrap(~MAJOR_STAT_AREA_CODE, labeller = area_label)


# average CPUE for top species by month, year and area in the same graph

CPUE_by_month_year_area <- top_north_south_data %>%
  group_by(., Species_name, MONTH, MAJOR_STAT_AREA_CODE, YEAR) %>%
  summarise(., mean_CPUE = mean(CPUE),
            total_CPUE = sum(CPUE),
            mean_log_CPUE = mean(log_CPUE))

ggplot(CPUE_by_month_year_area )+
  geom_bar(aes(x=MONTH, y=mean_CPUE, fill=Species_name), stat = "identity")+
  scale_fill_manual(values = mycolour3) +
  facet_grid(MAJOR_STAT_AREA_CODE~YEAR, scales = "free_x", space = "free",
             labeller = labeller(MAJOR_STAT_AREA_CODE = area_label))


# using mean log CPUE for top species by month, year and area in the same graph

ggplot(CPUE_by_month_year_area )+
  geom_bar(aes(x=MONTH, y=mean_log_CPUE, fill=Species_name), stat = "identity")+
  scale_fill_manual(values = mycolour3) +
  facet_grid(MAJOR_STAT_AREA_CODE~YEAR, scales = "free_x", space = "free",
             labeller = labeller(MAJOR_STAT_AREA_CODE = area_label))



#============================
# catch comp
#==========================

sum(top_20_CPUE$CPUE)
# [1] 9544895


CPUE_total <- top_20_CPUE %>%
  group_by(., MONTH, YEAR) %>%
  summarise(., total_CPUE = sum(CPUE)) %>%
  data.frame()

CPUE_by_month_year %>%
  group_by(., MONTH, YEAR) %>%
  summarise(., total_CPUE_all_species = sum(total_CPUE))


ggplot(CPUE_by_month_year )+
  geom_bar(aes(x=MONTH, y=mean_CPUE, fill=Species_name), stat = "identity")+
  scale_fill_manual(values = mycolour) +
  facet_grid(.~YEAR, scales = "free_x", space = "free")





catch_comp <- top_20_CPUE %>%
  group_by(., Species_name, YEAR) %>%
  summarise(., mean_CPUE = mean(CPUE))

ggplot(CPUE_by_year)+
  geom_bar(aes(x=YEAR, y=mean_CPUE, fill=Species_name), stat = "identity")+
  scale_fill_manual(values = mycolour) 




## log CPUE vs depth for top 10 species  

top_10_CPUE %>% 
  ggplot(aes(x=DEPTH, y=log_CPUE, fill = Species_name))+
  geom_point(aes(colour = Species_name)) +
  geom_smooth()



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


# rename factor level North and South 

filtered_eulachon_data$MAJOR_STAT_AREA_CODE <- recode_factor(filtered_eulachon_data$MAJOR_STAT_AREA_CODE, 
                                                    "1" = "South",
                                                    "8" = "North")


str(filtered_eulachon_data)

# creating data table in wide format

eulachon_wide <- pivot_wider(filtered_eulachon_data, 
                             names_from =Species_name,
                             values_from = CPUE,
                             names_prefix = "CPUE for ",
                             id_cols = !Taxonomic_group)



# add log CPUE

eulachon_data <- filtered_eulachon_data %>% 
  mutate(., log_CPUE = log(CPUE+1))



# add log CPUE

log_CPUE_data <- taxa_catch_data %>% 
  mutate(., log_CPUE = log(CPUE+1))


# remove irrelevant taxa groups and tows from Hecate Strait and sets with only Eulachon CPUE recorded

eulachon_data <- log_CPUE_data %>%
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

eulachon_data$Species_name <- factor(eulachon_data$Species_name)

eulachon_data$Taxonomic_group <- factor(eulachon_data$Taxonomic_group)

eulachon_data$MAJOR_STAT_AREA_CODE <- factor(eulachon_data$MAJOR_STAT_AREA_CODE)


# rename factor level North and South 

eulachon_data$MAJOR_STAT_AREA_CODE <- recode_factor(eulachon_data$MAJOR_STAT_AREA_CODE, 
                                                    "1" = "South",
                                                    "8" = "North")


str(eulachon_data)

# creating data table in wide format

eulachon_wide <- pivot_wider(eulachon_data, 
                             names_from =Species_name,
                             values_from = CPUE,                          
                             names_prefix = "CPUE for")



#trawl_location2 <- eulachon_data %>%
#  group_by(., EVENT_ID, MONTH, START_LATITUDE, START_LONGITUDE)

#distinct_trawl_locations2 <- trawl_location2 %>% distinct(EVENT_ID)


plot(data=distinct_trawl_location, START_LATITUDE~START_LONGITUDE, 
     col=as.factor(distinct_trawl_location$MONTH), pch=1)
legend("topright", legend=unique(distinct_trawl_location$MONTH), title= "Month", pch=19, 
       col=unique(distinct_trawl_location$MONTH))



corr_simple <- function(data=df,sig=0.9){
  #convert data to numeric in order to run correlations
  #convert to factor first to keep the integrity of the data - each value will become a number rather than turn into NA
  df_cor <- data %>% mutate_if(is.character, as.factor)
  df_cor <- df_cor %>% mutate_if(is.factor, as.numeric)
  #run a correlation and drop the insignificant ones
  corr <- cor(df_cor)
  #prepare to drop duplicates and correlations of 1     
  corr[lower.tri(corr,diag=TRUE)] <- NA 
  #drop perfect correlations
  corr[corr == 1] <- NA 
  #turn into a 3-column table
  corr <- as.data.frame(as.table(corr))
  #remove the NA values from above 
  corr <- na.omit(corr) 
  #select significant values  
  corr <- subset(corr, abs(Freq) > sig) 
  #sort by highest correlation
  corr <- corr[order(-abs(corr$Freq)),] 
  #print table
  print(corr)
  #turn corr back into matrix in order to plot with corrplot
  mtx_corr <- reshape2::acast(corr, Var1~Var2, value.var="Freq")
  
  #plot correlations visually
  corrplot(mtx_corr, is.corr=FALSE, tl.col="black", na.label=" ")
}
corr_simple(data = data.frame(eulachon_wide[,14:217]))



# Correlation plot for top top 20 species from both north and south in one plot

#top_north_south_nolog <- top_north_south_data %>%
# mutate(log_CPUE = NULL)

#eulachon_wide_north_south <- pivot_wider(top_north_south_nolog, 
#                                   names_from =Species_name,
#                                   values_from = CPUE,
#                                   #names_prefix = "CPUE for ",
#                                   id_cols = !Taxonomic_group)

#species_cor_north_south <- cor(data.frame(eulachon_wide_north_south[,14:39]))

#species_cor_north_south

#corrplot(species_cor_north_south, tl.col = "black", type = "upper", method = "circle" )

#doesn't work 


# eulachon data filtered for mean CPUE above 40

  
eulachon_wide_all_species <- pivot_wider(filtered_eulachon_data, 
                                           names_from =Species_name,
                                           values_from = CPUE,
                                           #names_prefix = "CPUE_",
                                           id_cols = !Taxonomic_group) 
  



eulachon_wide_1 <- eulachon_wide_all_species %>%
  dplyr::select(TRIP_ID:TEMPERATURE)      


eulachon_wide_2 <- eulachon_wide_all_species[14:217] %>%
  dplyr::select(where(~ is.numeric(.x) && mean(.x) > 40))

eulachon_wide <- cbind(eulachon_wide_1, eulachon_wide_2)



# south data in wide format

top_20_CPUE_south_nolog <- top_20_CPUE_south %>%
  mutate(log_CPUE = NULL)

eulachon_wide_south <- pivot_wider(top_20_CPUE_south_nolog, 
                                   names_from =Species_name,
                                   values_from = CPUE,
                                   #names_prefix = "CPUE for ",
                                   id_cols = !Taxonomic_group)

# north data in wide format

top_20_CPUE_north_nolog <- top_20_CPUE_north %>%
  mutate(log_CPUE = NULL)

eulachon_wide_north <- pivot_wider(top_20_CPUE_north_nolog, 
                                   names_from =Species_name,
                                   values_from = CPUE,
                                   #names_prefix = "CPUE for ",
                                   id_cols = !Taxonomic_group)


# Correlation plot for top top 20 species from North

top_20_CPUE_north_nolog <- top_20_CPUE_north %>%
  mutate(log_CPUE = NULL)

eulachon_wide_north <- pivot_wider(top_20_CPUE_north_nolog, 
                                   names_from =Species_name,
                                   values_from = CPUE,
                                   #names_prefix = "CPUE for ",
                                   id_cols = !Taxonomic_group)

species_cor_north <- cor(data.frame(eulachon_wide_north[,14:33]),use="complete.obs")

species_cor_north

corrplot(species_cor_north, tl.col = "black", type = "upper", method = "circle" )

# Correlation plot for top top 20 species from South

top_20_CPUE_south_nolog <- top_20_CPUE_south %>%
  mutate(log_CPUE = NULL)

eulachon_wide_south <- pivot_wider(top_20_CPUE_south_nolog, 
                                   names_from =Species_name,
                                   values_from = CPUE,
                                   #names_prefix = "CPUE for ",
                                   id_cols = !Taxonomic_group)

species_cor_south <- cor(data.frame(eulachon_wide_south[,14:33]),use="complete.obs")

species_cor_south

corrplot(species_cor_south, tl.col = "black", type = "upper", method = "circle" )

# top north species 

#eulachon_pca_north <- princomp(eulachon_wide_north[,(14:33)],cor=TRUE)

#summary(eulachon_pca_north)

#plot(eulachon_pca_north)

#loadings(eulachon_pca_north)


# top south species 

#eulachon_pca_south <- princomp(eulachon_wide_south[,(14:33)],cor=TRUE)

#summary(eulachon_pca_south)

#plot(eulachon_pca_south)

#loadings(eulachon_pca_south)