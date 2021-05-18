
EBS.data <- import("C:\\Users\\hackettb\\Documents\\R\\eulachon_project\\EBS_data.csv")

library(cluster)
library(MASS)

EBS.fish.data<-subset(EBS.data,EBS.data["area"]!=10&EBS.data["area"]!=12&EBS.data["area"]!=6)
EBS.fish.data<-EBS.fish.data[,-(27)]
EBS.fish.data<-EBS.fish.data[,-(28)]

for(i in 20:37){ 
minfish<-min(subset(EBS.fish.data[,i],EBS.fish.data[,i]>0))/2
EBS.fish.data[,i]<-log(EBS.fish.data[,i]+minfish)}

####CORRELATIONS AMONG VARIABLES #########
EBS.cor<-cor(data.frame(EBS.fish.data[,20:37]),use="complete.obs")
EBS.cor

corrplot(EBS.cor)
####PRINCIPAL COMPONENTS ANALYSIS OF BIOLOGICAL DATA#######
EBS.fish.pca<-princomp(EBS.fish.data[,(20:37)],cor=TRUE)
summary(EBS.fish.pca)
plot(EBS.fish.pca)
loadings(EBS.fish.pca)

####MAKE BIPLOTS ######
area.name.x<-EBS.fish.data$area
area.name.x[area.name.x==1|area.name.x==8|area.name.x==11|area.name.x==12]<-"o"
area.name.x[area.name.x==2|area.name.x==4|area.name.x==7|area.name.x==9]<-"x"
area.name.x[area.name.x==3]<-"+"
area.name.x[area.name.x==5]<-"^"
biplot(EBS.fish.pca,xlabs=as.character(area.name.x),cex=0.8)#,xlim=c(-.05,.07))
legend(0,250,c("x Inter-canyon", "o Canyon","+ Pribilof Canyon","^ Zhemchug Canyon"),cex=0.8)
biplot(EBS.fish.pca,choices=c(1,3),xlabs=as.character(area.name.x))#,cex=0.8,xlim=c(-.15,.05))
biplot(EBS.fish.pca,choices=c(1,4),xlabs=as.character(area.name.x))#,cex=0.8,xlim=c(-.05,.07))
biplot(EBS.fish.pca,choices=c(2,3),xlabs=as.character(area.name.x))#,cex=0.8,xlim=c(-.05,.07))
biplot(EBS.fish.pca,choices=c(2,4),xlabs=as.character(area.name.x))#,cex=0.8,xlim=c(-.05,.07))
biplot(EBS.fish.pca,choices=c(3,4),xlabs=as.character(area.name.x))#,cex=0.8,xlim=c(-.05,.07))

##Heirarchical clustering on the normalized data just for the slope data
	#normalize the data
cluster.data<-cbind(EBS.fish.data[,20:37],EBS.fish.data["area"],EBS.fish.data["area.name"])
mean.cluster<-colMeans(cluster.data[,1:18])
sd.cluster<-apply(cluster.data[,1:18],2, sd)
for(i in 1:18){ 
cluster.data[,i]<-(cluster.data[,i]-mean.cluster[i])/sd.cluster[i]}

###########ARE ZHEMCHUG AND PRIBILOF CANYONS UNIQUE FROM THE REST OF THE SLOPE WITH REGARDS TO PHYSICAL VARIABLES?####
# Linear Discriminant Analysis with Jacknifed Prediction 
IC<-cluster.data[,19]
IC[cluster.data[,19]==11]<-"Can"
IC[cluster.data[,19]==9]<-"Int"
IC[cluster.data[,19]==8]<-"Can"
IC[cluster.data[,19]==7]<-"Int"
IC[cluster.data[,19]==5]<-"Can"
IC[cluster.data[,19]==4]<-"Int"
IC[cluster.data[,19]==3]<-"Can"
IC[cluster.data[,19]==2]<-"Int"
IC[cluster.data[,19]==1]<-"Can"
cluster.data<-cbind(cluster.data,IC)
EBS.dfa2 <- qda(IC~AKskate+ALskate+ATF+Kam+Gturb+Phal+FHS+NRS+Sable+Ggren+Pgren+Pcod+Shortraker+POP+RE_BS+Scrab+Pollock+SST,CV=TRUE,data=cluster.data)
#table(IC,predict(EBS.dfa2)$class)
#EBS.dfa2
ct <- table(IC, EBS.dfa2$class)
ct
diag(prop.table(ct, 1))
# total percent correct
sum(diag(prop.table(ct)))

fligner.test(AKskate~IC, data=cluster.data)
fligner.test(ALskate~IC, data=cluster.data)
fligner.test(ATF~IC, data=cluster.data)
fligner.test(Kam~IC, data=cluster.data)
fligner.test(Gturb~IC, data=cluster.data)
fligner.test(Phal~IC, data=cluster.data)
fligner.test(FHS~IC, data=cluster.data)
fligner.test(NRS~IC, data=cluster.data)
fligner.test(Sable~IC, data=cluster.data)
fligner.test(Ggren~IC, data=cluster.data)
fligner.test(Pgren~IC, data=cluster.data)
fligner.test(Pcod~IC, data=cluster.data)
fligner.test(Shortraker~IC, data=cluster.data)
fligner.test(POP~IC, data=cluster.data)
fligner.test(RE_BS~IC, data=cluster.data)
fligner.test(Scrab~IC, data=cluster.data)
fligner.test(Pollock~IC, data=cluster.data)
fligner.test(SST~IC, data=cluster.data)


###COMPARE PRIBILOF CANYON TO SURROUNDING AREA#######
cluster.data3<-subset(cluster.data,cluster.data[,19]>=2&cluster.data[,19]<=4)
PC<-cluster.data3[,19]
PC[cluster.data3[,19]==2]<-"NOT"
PC[cluster.data3[,19]==3]<-"PC"
PC[cluster.data3[,19]==4]<-"NOT"
cluster.data3<-cbind(cluster.data3,PC)
EBS.dfa2 <- qda(PC~AKskate+ALskate+ATF+Kam+Gturb+Phal+FHS+NRS+Sable+Ggren+Pgren+Pcod+Shortraker+POP+RE_BS+Scrab+Pollock+SST,CV=TRUE,data=cluster.data3)
ct <- table(PC, EBS.dfa2$class)
ct
EBS.dfa2

###COMPARE ZHEMCHUG CANYON TO SURROUNDING AREA########
cluster.data4<-subset(cluster.data,cluster.data[,19]==4|cluster.data[,19]==5|cluster.data[,19]==7)
ZC<-cluster.data4[,19]
ZC[cluster.data4[,19]==4]<-"NOT"
ZC[cluster.data4[,19]==5]<-"ZC"
ZC[cluster.data4[,19]==7]<-"NOT"
cluster.data4<-cbind(cluster.data4,ZC)
EBS.dfa2 <- qda(ZC~AKskate+ALskate+ATF+Kam+Gturb+Phal+FHS+NRS+Sable+Ggren+Pgren+Pcod+Shortraker+POP+RE_BS+Scrab+Pollock+SST,CV=TRUE,data=cluster.data4)
ct <- table(ZC, EBS.dfa2$class)
ct
EBS.dfa2

fligner.test(AKskate~ZC, data=cluster.data4)
fligner.test(ALskate~ZC, data=cluster.data4)
fligner.test(ATF~ZC, data=cluster.data4)
fligner.test(Kam~ZC, data=cluster.data4)
fligner.test(Gturb~ZC, data=cluster.data4)
fligner.test(Phal~ZC, data=cluster.data4)
fligner.test(FHS~ZC, data=cluster.data4)
fligner.test(NRS~ZC, data=cluster.data4)
fligner.test(Sable~ZC, data=cluster.data4)
fligner.test(Ggren~ZC, data=cluster.data4)
fligner.test(Pgren~ZC, data=cluster.data4)
fligner.test(Pcod~ZC, data=cluster.data4)
fligner.test(Shortraker~ZC, data=cluster.data4)
fligner.test(POP~ZC, data=cluster.data4)
fligner.test(RE_BS~ZC, data=cluster.data4)
fligner.test(Scrab~ZC, data=cluster.data4)
fligner.test(Pollock~ZC, data=cluster.data4)
fligner.test(SST~ZC, data=cluster.data4)

####### Calculate the mean values of physical habitats for group membership ########
EBS.fish.data2<-subset(EBS.data,EBS.data["area"]!=10&EBS.data["area"]!=12&EBS.data["area"]!=6)
EBS.fish.data2<-EBS.fish.data2[,-(27)]
EBS.fish.data2<-EBS.fish.data2[,-(28)]
cluster.data2<-cbind(EBS.fish.data2[,20:37],G)
aggregate(cluster.data2[,1:18],by=cluster.data2["G"],mean)
aggregate(cluster.data2[,1:18],by=cluster.data2["G"],sd)

###ANOSIM Comparing all areas
anosim.data<-cluster.data[,-(19:20)]
anosim.factor<-cluster.data[,19]
anosim.dist<-vegdist(anosim.data,method="manhattan")
habitat.anosim<-anosim(anosim.dist, grouping=anosim.factor)
summary(habitat.anosim)
plot(habitat.anosim)


###PLOT OF PC's AGAINST LATITUDE, DEPTH and PHYSICAL VARIABLE PCA SCORES#####
fish.scores<-EBS.fish.pca$scores
scores<-EBS.pca$scores
par(mfrow=c(2,2))
plot(EBS.fish.data[,5],fish.scores[,1],xlab="Latitude",ylab="Component 1")
summary(lm(fish.scores[,1]~EBS.fish.data[,5]))
plot(EBS.fish.data[,5],fish.scores[,2],xlab="Latitude",ylab="Component 2")
plot(EBS.fish.data[,5],fish.scores[,3],xlab="Latitude",ylab="Component 3")
plot(EBS.fish.data[,5],fish.scores[,4],xlab="Latitude",ylab="Component 4")
plot(EBS.fish.data[,6],fish.scores[,1],xlab="Depth",ylab="Component 1")
summary(lm(fish.scores[,1]~EBS.fish.data[,6]))
plot(EBS.fish.data[,6],fish.scores[,2],xlab="Depth",ylab="Component 2")
plot(EBS.fish.data[,6],fish.scores[,3],xlab="Depth",ylab="Component 3")
plot(EBS.fish.data[,6],fish.scores[,4],xlab="Depth",ylab="Component 4")

library(mgcv)
pc.gam<-gam(fish.scores[,1]~s(EBS.fish.data[,6])+s(EBS.fish.data[,5]))
summary(pc.gam)
plot(pc.gam)





