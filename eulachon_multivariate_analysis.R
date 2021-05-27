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
library(dendextend)


options(scipen = 999)
#===========================================

#===========================================
# Data import and preparation 
#===========================================

# import data sets

taxa_data <- import("C:\\Users\\hackettb\\Documents\\R\\eulachon_project\\eulachon\\TaxaGroups.csv")

catch_data <- import("C:\\Users\\hackettb\\Documents\\R\\eulachon_project\\eulachon\\catch.csv")

temp_data <- import("C:\\Users\\hackettb\\Documents\\R\\eulachon_project\\eulachon\\temperature.csv")

# merge data sets

taxa_catch_data <- merge(catch_data, taxa_data, by = "Species_name")

taxa_catch_temp_data <- merge( taxa_catch_data, temp_data, by = c("TRIP_ID", "EVENT_ID"))

taxa_catch_temp_data <- taxa_catch_temp_data %>%
  dplyr::select(Species_name, TRIP_ID,EVENT_ID, SET_ID, YEAR, MONTH, DATE, 
                MAJOR_STAT_AREA_CODE,START_LATITUDE, START_LONGITUDE,
                END_LATITUDE, END_LONGITUDE, DEPTH, MEAN_TEMPERATURE, 
                CPUE, Taxonomic_group) %>%
  arrange(., Species_name)


# change species name, taxonomic group, and area code to factor 

taxa_catch_temp_data$Species_name <- factor(taxa_catch_temp_data$Species_name)

taxa_catch_temp_data$Taxonomic_group <- factor(taxa_catch_temp_data$Taxonomic_group)

taxa_catch_temp_data$MAJOR_STAT_AREA_CODE <- factor(taxa_catch_temp_data$MAJOR_STAT_AREA_CODE)


# manipulate date variables with lubridate

taxa_catch_temp_data$DATE <- ymd(taxa_catch_temp_data$DATE)

taxa_catch_temp_data$MONTH <- month(taxa_catch_temp_data$DATE, label = TRUE, abbr = TRUE)

taxa_catch_temp_data$YEAR <- year(taxa_catch_temp_data$DATE)


# remove irrelevant taxa groups and tows from Hecate Strait and sets with only Eulachon CPUE recorded

filtered_eulachon_data <- taxa_catch_temp_data %>%
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

eulachon_wide <- cbind(eulachon_wide_1, eulachon_wide_2)

# use event ID as row names 

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

# adding season to data set

eulachon_wide_season <- eulachon_wide %>% 
  mutate(., SEASON = recode(eulachon_wide$MONTH, 
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


season_colours <- c('Winter'="blue",
                    'Spring'="orange",
                    'Summer'="red",
                    'Fall'="green")

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
plot(event_cluster, ylab = "Bray-Curtis dissimilarity", labels = FALSE)

# cluster diagram coloured by area

area_colours <- c('8'="blue", '1'="green")

bc_dend <- as.dendrogram(hclust(event_dist, method = "average"))

order.dendrogram(bc_dend)

labels_colors(bc_dend)<-area_colours[eulachon_wide$MAJOR_STAT_AREA_CODE][order.dendrogram(bc_dend)]

plot(hang.dendrogram(bc_dend, hang = 0.1), ylab = "Bray-Curtis dissimilarity", main = "Cluster Dendrogram")
legend("topright", legend=c("North", "South"), title= "Area", pch=19, 
       col=c("blue", "green"))

####### ordination ########

# The metaMDS function automatically transforms data and checks solution
# robustness

event_mds <- metaMDS(cluster_data, dist = "bray", trymax = 150, k=3,
                     maxit = 300)


# Assess goodness of ordination fit (stress plot)
stressplot(event_mds)


# automated plotting of results 
ordipointlabel(event_mds)

# colour plot by area

blank_mds<- ordiplot(event_mds, type = "n")
points(blank_mds, "sites", col = "blue", 
       select=eulachon_wide$MAJOR_STAT_AREA_CODE=="8", pch=3, cex=0.5)
points(blank_mds, "sites", col = "green", 
       select=eulachon_wide$MAJOR_STAT_AREA_CODE=="1", pch=3, cex=0.5)
text(blank_mds, "species", col = "black", cex=0.6)

ordiellipse(blank_mds, eulachon_wide$MAJOR_STAT_AREA_CODE, conf = 0.95, label = FALSE)

legend("topright", legend=c("North", "South"), title= "Area", pch=19, 
       col=c("blue", "green"))

# add temp and depth axes to the plot

plot(envfit(blank_mds, eulachon_wide[, 12:13]), cex = 0.75)


# colour plot by season with season ellipse

blank_mds<- ordiplot(event_mds, type = "n")
points(blank_mds, "sites", col = "blue", 
       select=eulachon_wide_season$SEASON=="Winter", pch=3, cex=0.5)
points(blank_mds, "sites", col = "orange", 
       select=eulachon_wide_season$SEASON=="Spring", pch=3, cex=0.5)
points(blank_mds, "sites", col = "red", 
       select=eulachon_wide_season$SEASON=="Summer", pch=3, cex=0.5)
points(blank_mds, "sites", col = "green", 
       select=eulachon_wide_season$SEASON=="Fall", pch=3, cex=0.5)
text(blank_mds, "species", col = "black", cex=0.6)

ordiellipse(blank_mds, eulachon_wide_season$SEASON, conf = 0.95, 
            label = FALSE, col = season_colours)
legend("topright", legend=c("Winter", "Spring", "Summer", "Fall"), title= "Season", pch=19, 
       col=season_colours)
# add temp and depth axes to the plot

plot(envfit(blank_mds, eulachon_wide[, 12:13]), cex = 0.75)


# colour plot by season with area ellipse

blank_mds<- ordiplot(event_mds, type = "n")
points(blank_mds, "sites", col = "blue", 
       select=eulachon_wide_season$SEASON=="Winter", pch=3, cex=0.5)
points(blank_mds, "sites", col = "orange", 
       select=eulachon_wide_season$SEASON=="Spring", pch=3, cex=0.5)
points(blank_mds, "sites", col = "red", 
       select=eulachon_wide_season$SEASON=="Summer", pch=3, cex=0.5)
points(blank_mds, "sites", col = "green", 
       select=eulachon_wide_season$SEASON=="Fall", pch=3, cex=0.5)
text(blank_mds, "species", col = "black", cex=0.6)

ordiellipse(blank_mds, eulachon_wide_season$MAJOR_STAT_AREA_CODE, conf = 0.95, 
            label = FALSE)
legend("topright", legend=c("Winter", "Spring", "Summer", "Fall"), title= "Season", pch=19, 
       col=season_colours)

# add temp and depth axes to the plot

plot(envfit(blank_mds, eulachon_wide[, 12:13]), cex = 0.75)

# plot eulachon abundance

ordisurf(event_mds, cluster_data[, "THALEICHTHYS.PACIFICUS"], bubble = TRUE, main = "Eulachon abundance", 
         cex = 4)


############# Hierarchical clustering by area and season #############


##### filter by area - north #####

eulachon_wide_north <- eulachon_wide_season %>%
  filter(., MAJOR_STAT_AREA_CODE==8)

row.names(eulachon_wide_north) <- eulachon_wide_north$EVENT_ID

# Turn CPUE to relative abundance by dividing each value by sample total abundance

cluster_data_north <- decostand(eulachon_wide_north[,15:39], method = "total")

# check total abundance in each sample

apply(cluster_data_north, 1, sum)

# calculate Bray-Curtis distance among samples

event_dist_north <- vegdist(cluster_data_north, method = "bray")

# cluster communities using average-linkage algorithm
event_cluster_north <- hclust(event_dist_north, method = "average")

# plot cluster diagram
plot(event_cluster_north, ylab = "Bray-Curtis dissimilarity")

# colouring labels by season 

north_dend <- as.dendrogram(hclust(event_dist_north, method = "average"))

order.dendrogram(north_dend)

labels_colors(north_dend)<-season_colours[eulachon_wide_north$SEASON][order.dendrogram(north_dend)]

plot(hang.dendrogram(north_dend, hang = 0.1), ylab = "Bray-Curtis dissimilarity", main = "Cluster Dendrogram - North")
legend("topright", legend=c("Winter", "Spring", "Summer", "Fall"), title= "Season", pch=19, 
       col=season_colours)


##### filter by area - south #####

eulachon_wide_south <- eulachon_wide_season %>%
  filter(., MAJOR_STAT_AREA_CODE==1)

row.names(eulachon_wide_south) <- eulachon_wide_south$EVENT_ID

# Turn CPUE to relative abundance by dividing each value by sample total abundance

cluster_data_south <- decostand(eulachon_wide_south[,15:39], method = "total")

# check total abundance in each sample

apply(cluster_data_south, 1, sum)

# calculate Bray-Curtis distance among samples

event_dist_south <- vegdist(cluster_data_south, method = "bray")

# cluster communities using average-linkage algorithm
event_cluster_south <- hclust(event_dist_south, method = "average")

# plot cluster diagram
plot(event_cluster_south, ylab = "Bray-Curtis dissimilarity")

# colouring labels by season 

south_dend <- as.dendrogram(hclust(event_dist_south, method = "average"))

order.dendrogram(south_dend)

labels_colors(south_dend)<-season_colours[eulachon_wide_south$SEASON][order.dendrogram(south_dend)]

plot(hang.dendrogram(south_dend, hang = 0.1), ylab = "Bray-Curtis dissimilarity", main = "Cluster Dendrogram - South")
legend("topright", legend=c("Winter", "Spring", "Summer", "Fall"), title= "Season", pch=19, 
       col=season_colours)



####### ordination  by area and season ########

#### North ####

# The metaMDS function automatically transforms data and checks solution
# robustness

event_mds_north <- metaMDS(cluster_data_north, dist = "bray", trymax = 150, k=3,
                           maxit = 300)

# Assess goodness of ordination fit (stress plot)

stressplot(event_mds_north)

# layering the plot 

blank_mds_north<- ordiplot(event_mds_north, type = "n")
points(blank_mds_north, "sites", col = "blue", 
       select=eulachon_wide_north$SEASON=="Winter", pch=3, cex=0.5)
points(blank_mds_north, "sites", col = "orange", 
       select=eulachon_wide_north$SEASON=="Spring", pch=3, cex=0.5)
points(blank_mds_north, "sites", col = "red", 
       select=eulachon_wide_north$SEASON=="Summer", pch=3, cex=0.5)
points(blank_mds_north, "sites", col = "green", 
       select=eulachon_wide_north$SEASON=="Fall", pch=3, cex=0.5)
text(blank_mds_north, "species", col = "black", cex=0.6)


ordiellipse(blank_mds_north, eulachon_wide_north$SEASON, 
            conf = 0.95, label = FALSE, col = season_colours)

legend("topright", legend=c("Winter", "Spring", "Summer", "Fall"), title= "Season", pch=19, 
       col=season_colours)

# add temp and depth axis

plot(envfit(blank_mds_north, eulachon_wide_north[, 13:14]), cex = 0.75)

#### South ####

# The metaMDS function automatically transforms data and checks solution
# robustness

##increased max # of iterations from 200 to 300
event_mds_south <- metaMDS(cluster_data_south, dist = "bray", trymax = 150, k=3,
                            maxit = 300, previous.best = TRUE)

# Assess goodness of ordination fit (stress plot)

stressplot(event_mds_south)

# layering the plot 

blank_mds_south<- ordiplot(event_mds_south, type = "n")
points(blank_mds_south, "sites", col = "blue", 
       select=eulachon_wide_south$SEASON=="Winter", pch=3, cex=0.5)
points(blank_mds_south, "sites", col = "orange", 
       select=eulachon_wide_south$SEASON=="Spring", pch=3, cex=0.5)
points(blank_mds_south, "sites", col = "red", 
       select=eulachon_wide_south$SEASON=="Summer", pch=3, cex=0.5)
points(blank_mds_south, "sites", col = "green", 
       select=eulachon_wide_south$SEASON=="Fall", pch=3, cex=0.5)
text(blank_mds_south, "species", col = "black", cex=0.6)


ordiellipse(blank_mds_south, eulachon_wide_south$SEASON, conf = 0.95, 
            label = FALSE, col = season_colours)
legend("topright", legend=c("Winter", "Spring", "Summer", "Fall"), title= "Season", pch=19, 
       col=season_colours)

# add temp and depth axis

plot(envfit(blank_mds_south, eulachon_wide_south[, 13:14]), cex = 0.75)


###### k means clustering ######
library(factoextra)
library(cluster)

scaled_cluster <- scale(eulachon_wide[,14:38])

# check data

apply(scaled_cluster, 2, sd)
colMeans(scaled_cluster)

# number of clusters vs. the total within sum of squares

fviz_nbclust(scaled_cluster, kmeans, method = "wss", k.max = 25)

#calculate gap statistic based on number of clusters
gap_stat <- clusGap(scaled_cluster,
                    FUN = kmeans,
                    K.max = 25)

#plot number of clusters vs. gap statistic
fviz_gap_stat(gap_stat)

#perform k-means clustering with k = 4 clusters
km <- kmeans(scaled_cluster, centers = 18, nstart = 100, iter.max = 100)

#view results
km

#plot results of final k-means model

fviz_cluster(km, data = scaled_cluster)


#######PLOT OF PC's AGAINST LATITUDE, DEPTH and PHYSICAL VARIABLE PCA SCORES#####

fish_scores <- eulachon_pca$scores

par(mfrow=c(2,2))

plot(eulachon_wide_log[,8],fish_scores[,1],xlab="Latitude",ylab="Component 1")
summary(lm(fish_scores[,1]~eulachon_wide_log[,8]))
plot(eulachon_wide_log[,8],fish_scores[,2],xlab="Latitude",ylab="Component 2")
plot(eulachon_wide_log[,8],fish_scores[,3],xlab="Latitude",ylab="Component 3")
plot(eulachon_wide_log[,8],fish_scores[,4],xlab="Latitude",ylab="Component 4")

plot(eulachon_wide_log[,12],fish_scores[,1],xlab="Depth",ylab="Component 1")
summary(lm(fish_scores[,1]~eulachon_wide_log[,12]))
plot(eulachon_wide_log[,12],fish_scores[,2],xlab="Depth",ylab="Component 2")
plot(eulachon_wide_log[,12],fish_scores[,3],xlab="Depth",ylab="Component 3")
plot(eulachon_wide_log[,12],fish_scores[,4],xlab="Depth",ylab="Component 4")

library(mgcv)
pc_gam<-gam(fish_scores[,1]~s(eulachon_wide_log[,8])+s(eulachon_wide_log[,12]))
summary(pc_gam)
plot(pc_gam)

plot(eulachon_wide_log[,13],fish_scores[,1],xlab="Temp",ylab="Component 1")
summary(lm(fish_scores[,1]~eulachon_wide_log[,13]))
plot(eulachon_wide_log[,13],fish_scores[,2],xlab="Temp",ylab="Component 2")
plot(eulachon_wide_log[,13],fish_scores[,3],xlab="Temp",ylab="Component 3")
plot(eulachon_wide_log[,13],fish_scores[,4],xlab="Temp",ylab="Component 4")

###########Testing for multivariate differences among groups##############

# quantify the relationship between dissimilarity measures and different 
# explanatory variables using the permutational MANOVA

# Taxonomic (Bray-Curtis) dissimilarity explained - north vs south

adonis(event_dist ~ MAJOR_STAT_AREA_CODE, data = eulachon_wide)


# Taxonomic (Bray-Curtis) dissimilarity explained - temperature

adonis(event_dist ~ MEAN_TEMPERATURE, data = eulachon_wide)

# Taxonomic (Bray-Curtis) dissimilarity explained - depth

adonis(event_dist ~ DEPTH, data = eulachon_wide)

# Taxonomic (Bray-Curtis) dissimilarity explained - north by season

adonis(event_dist_north ~ SEASON, data = eulachon_wide_north)

# Taxonomic (Bray-Curtis) dissimilarity explained - south by season

adonis(event_dist_south ~ SEASON, data = eulachon_wide_south)


