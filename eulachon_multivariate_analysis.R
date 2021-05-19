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

taxa_data <- import("C:\\Users\\hackettb\\Documents\\R\\eulachon_project\\eulachon\\TaxaGroups.csv")

catch_data <- import("C:\\Users\\hackettb\\Documents\\R\\eulachon_project\\eulachon\\catch.csv")


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
                          4564094,
                          4363883))


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

eulachon_wide_3 <- cbind(eulachon_wide_1, eulachon_wide_2)

# use event ID as row names 

eulachon_wide <- cbind(eulachon_wide_1, eulachon_wide_2)
row.names(eulachon_wide) <- eulachon_wide$EVENT_ID

#eulachon_wide4 <- data.frame(eulachon_wide3, row.names = 2)


# log transform data

eulachon_wide_log <- eulachon_wide

for(i in 14:38){ 
 minfish_eulachon<-min(subset(eulachon_wide_log[,i],eulachon_wide_log[,i]>0))/2
 eulachon_wide_log[,i]<-log(eulachon_wide_log[,i]+minfish_eulachon)}

# log (CPUE + 1) transformed 

eulachon_wide_log_2 <- eulachon_wide

for (i in 14:38) {
  eulachon_wide_log_2[,i]<-log1p(eulachon_wide_log_2[,i])
}
#=========================================================================
# following EBS.fish multivariate analysis outline
#=========================================================================

####CORRELATIONS AMONG VARIABLES #########

species_cor <- cor(data.frame(eulachon_wide_log[,14:38]),use="complete.obs")
species_cor

corrplot(species_cor, tl.col = "black")

####PRINCIPAL COMPONENTS ANALYSIS OF BIOLOGICAL DATA#######

eulachon_pca <- princomp(eulachon_wide_log[,(14:38)],cor=TRUE)

summary(eulachon_pca)

plot(eulachon_pca)

loadings(eulachon_pca)


####MAKE BIPLOTS ######

eulachon_wide_log$MAJOR_STAT_AREA_CODE <- as.numeric(as.character(eulachon_wide_log$MAJOR_STAT_AREA_CODE))


area_name<-eulachon_wide_log$MAJOR_STAT_AREA_CODE

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


####### Hierarchical clustering ########

apply(eulachon_wide[,14:38], 1, sum)

# Turn CPUE to relative abundance by dividing each value by sample total abundance

cluster_data <- decostand(eulachon_wide[,14:38], method = "total")

# check total abundance in each sample

apply(cluster_data, 1, sum)

# calculate Bray-Curtis distance among samples

event_dist <- vegdist(cluster_data, method = "bray")

# cluster communities using average-linkage algorithm
event_cluster <- hclust(event_dist, method = "average")

# plot cluster diagram
plot(event_cluster, ylab = "Bray-Curtis dissimilarity")



####### ordination ########

# The metaMDS function automatically transforms data and checks solution
# robustness

event_mds <- metaMDS(cluster_data, dist = "bray", trymax = 100, k=3 )


# Assess goodness of ordination fit (stress plot)
stressplot(event_mds)


# automated plotting of results - tries to eliminate overlapping labels
ordipointlabel(event_mds)

# plot site scores as text
ordiplot(event_mds, display = "sites", type = "text")

# layering the plot 

blank_mds<- ordiplot(event_mds, type = "n")
points(blank_mds, "sites", col = "blue", select=eulachon_wide$MAJOR_STAT_AREA_CODE=="8", pch=3, cex=0.5)
points(blank_mds, "sites", col = "green", select=eulachon_wide$MAJOR_STAT_AREA_CODE=="1", pch=3, cex=0.5)
text(blank_mds, "species", col = "black", cex=0.6)


ordiellipse(blank_mds, eulachon_wide$MAJOR_STAT_AREA_CODE, conf = 0.95, label = FALSE)



###### k means clustering ######
library(factoextra)
library(cluster)

scaled_cluster <- scale(eulachon_wide[,14:38])

# check data

apply(scaled_cluster, 2, sd)
colMeans(scaled_cluster)

# number of clusters vs. the total within sum of squares

fviz_nbclust(scaled_cluster, kmeans, method = "wss")

#calculate gap statistic based on number of clusters
gap_stat <- clusGap(scaled_cluster,
                    FUN = kmeans,
                    nstart = 25,
                    K.max = 10,
                    B = 50)

#plot number of clusters vs. gap statistic
fviz_gap_stat(gap_stat)

#perform k-means clustering with k = 4 clusters
km <- kmeans(scaled_cluster, centers = 4, nstart = 25)

#view results
km

#plot results of final k-means model

fviz_cluster(km, data = scaled_cluster)


