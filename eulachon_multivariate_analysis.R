#===========================================
# title: "eulachon_multivariate_analysis"
# author: Brooke Hackett
# date: May 12th 2021
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
library(tidyselect)
library(picante)
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

# Remove species with mean CPUE < 40

eulachon_wide_1 <- eulachon_wide_all_species %>%
  dplyr::select(TRIP_ID:TEMPERATURE)      

eulachon_wide_2 <- eulachon_wide_all_species[14:217] %>%
  dplyr::select(where(~ is.numeric(.x) && mean(.x) > 40))

eulachon_wide <- cbind(eulachon_wide_1, eulachon_wide_2)



# log transform data

for(i in 14:38){ 
  minfish_eulachon<-min(subset(eulachon_wide[,i],eulachon_wide[,i]>0))/2
  eulachon_wide[,i]<-log(eulachon_wide[,i]+minfish_eulachon)}


#=========================================================================
# following EBS.fish multivariate analysis outline
#=========================================================================

####CORRELATIONS AMONG VARIABLES #########

species_cor <- cor(data.frame(eulachon_wide[,14:38]),use="complete.obs")
species_cor

corrplot(species_cor, tl.col = "black")

####PRINCIPAL COMPONENTS ANALYSIS OF BIOLOGICAL DATA#######

eulachon_pca <- princomp(eulachon_wide[,(14:38)],cor=FALSE)

summary(eulachon_pca)

plot(eulachon_pca)

loadings(eulachon_pca)


####MAKE BIPLOTS ######

eulachon_wide$MAJOR_STAT_AREA_CODE <- as.numeric(as.character(eulachon_wide$MAJOR_STAT_AREA_CODE))


area_name<-eulachon_wide$MAJOR_STAT_AREA_CODE

area_name[area_name==1]<-"S"
area_name[area_name==8]<-"N"


biplot(eulachon_pca,xlabs=as.character(area_name),cex=0.8)#,xlim=c(-.05,.07))

biplot(eulachon_pca,choices=c(1,3),xlabs=as.character(area_name))#,cex=0.8,xlim=c(-.15,.05))
biplot(eulachon_pca,choices=c(1,4),xlabs=as.character(area_name))#,cex=0.8,xlim=c(-.05,.07))
biplot(eulachon_pca,choices=c(2,3),xlabs=as.character(area_name))#,cex=0.8,xlim=c(-.05,.07))
biplot(eulachon_pca,choices=c(2,4),xlabs=as.character(area_name))#,cex=0.8,xlim=c(-.05,.07))
biplot(eulachon_pca,choices=c(3,4),xlabs=as.character(area_name))#,cex=0.8,xlim=c(-.05,.07))



##Heirarchical clustering on the normalized data just for the slope data
#normalize the data

#cluster_data<-cbind(eulachon_wide[,14:38],eulachon_wide["MAJOR_STAT_AREA_CODE"])

#mean_cluster<-colMeans(cluster_data[,1:25])

#sd_cluster<-apply(cluster_data[,1:25],2, sd)

#for(i in 1:25){ 
#  cluster_data[,i]<-(cluster_data[,i]-mean_cluster[i])/sd_cluster[i]}




View(filtered_eulachon_data %>% filter(., EVENT_ID %in% 4363883))

eulachon_cluster <- eulachon_wide %>% filter(., EVENT_ID %nin% 4363883)


event_cluster <- cbind(eulachon_cluster["EVENT_ID"],eulachon_cluster[,14:38])


apply(event_cluster[,2:26], 1, sum)

# Turn CPUE to relative abundance by dividing each value by sample
# total abundance
event_cluster <- decostand(event_cluster[,2:26], method = "total")
# check total abundance in each sample
apply(event_cluster, 1, sum)



# calculate Bray-Curtis distance among samples
event_dist <- vegdist(event_cluster, method = "bray")

# cluster communities using average-linkage algorithm
event_clust <- hclust(event_dist, method = "average")
# plot cluster diagram
plot(event_clust, ylab = "Bray-Curtis dissimilarity")


