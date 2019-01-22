library(R.matlab)
library(Rmixmod)
library(mclust)
library(NbClust)
library(FactoMineR)
library(lle)
library(cluster)
library(aricode)
library(Rtsne)
library(caret)

mf=readMat("mfeatT1.mat")
mfeat=lapply(mf, unlist, use.names=FALSE)
mfeat=as.data.frame(mfeat$X)
mfeat
class=as.factor(t(as.data.frame(mf$y)))
mfeat=data.frame(mf$X,class)
rm(class)
dim(mfeat)
table(mfeat$class)
#2000 241
View(head(mfeat))
pca.mfeat=PCA(mfeat,ncp=2,quali.sup = 241)
plot(pca.mfeat, habillage=241)
#mds.mfeat=cmdscale(dist(mfeat[,-241]),k=2)
#plot(mds.mfeat, col=mfeat$class)
#lle.mfeat=lle(as.matrix(mfeat[,-241]),m=2, k=40,p=0.3)
#plot(lle.mfeat$Y, col=mfeat$class)
#sum(is.na(mfeat[,-241]))

###4
#elbow(mfeat[,-241])
set.seed(42)
#km.mfeat=kmeans(mfeat[,-241],10)
#plot(pca.mfeat$ind$coord,col=km.mfeat$cluster)
#table(km.mfeat$cluster,mfeat$class)
#NMI(km.mfeat$cluster,mfeat$class)
#0.78
#average.mfeat=hclust(as.matrix(mfeat[,-241]),method = "average")
#wss <- sapply(2:30, function(k){kmeans(mfeat[,-241], k, nstart=50,iter.max = 15 )$tot.withinss})
#plot(2:30,wss, type="b",xlab="Nombre de clusters",ylab="inertie",main = "La méthode du coude")


km.mfeat=NbClust(mfeat[,-241], method = "kmeans", distance = "euclidean", index = "silhouette",max.nc = 10)
#best nc = 9
km.mfeat=NbClust(mfeat[,-241], method = "kmeans", distance = "euclidean", index = "silhouette",min.nc = 10,max.nc = 10)
plot(pca.mfeat$ind$coord,col=km.mfeat$Best.partition)
table(km.mfeat$Best.partition,mfeat$class)
NMI(km.mfeat$Best.partition,mfeat$class)
#0.755
ARI(km.mfeat$Best.partition,mfeat$class)
#0.689

ward.mfeat=NbClust(mfeat[,-241],method = "ward.D",index = "silhouette", max.nc = 10)
#best nc 6
ward.mfeat=NbClust(mfeat[,-241],method = "ward.D",index = "silhouette", max.nc = 10, min.nc = 10)
table(ward.mfeat$Best.partition,mfeat$class)
NMI(ward.mfeat$Best.partition,mfeat$class)
#0.796
ARI(ward.mfeat$Best.partition,mfeat$class)
#0.734
plot(pca.mfeat$ind$coord,col=ward.mfeat$Best.partition,main = "CAH Ward sur mfeat")


average.mfeat=NbClust(mfeat[,-241],method = "average", max.nc = 10,index = "silhouette")
#5 cluster
average.mfeat=NbClust(mfeat[,-241],method = "average",min.nc = 10, max.nc = 10,index = "silhouette")
plot(pca.mfeat$ind$coord,col=average.mfeat$Best.partition,main = "CAH average sur mfeat")
table(average.mfeat$Best.partition,mfeat$class)
NMI(average.mfeat$Best.partition,mfeat$class)
#0.5846
ARI(average.mfeat$Best.partition,mfeat$class)
#0.463





single.mfeat=NbClust(mfeat[,-241],method = "single",index = "silhouette", max.nc = 10)
#2
single.mfeat=NbClust(mfeat[,-241],method = "single",index = "silhouette", max.nc = 10,min.nc = 10)

plot(pca.mfeat$ind$coord,col=single.mfeat$Best.partition,main = "CAH single sur mfeat")
table(single.mfeat$Best.partition,mfeat$class)
NMI(single.mfeat$Best.partition,mfeat$class)
#0.005
ARI(single.mfeat$Best.partition,mfeat$class)
#7.197997e-06

complete.mfeat=NbClust(mfeat[,-241],method = "complete",index = "silhouette", max.nc = 10)
#3
complete.mfeat=NbClust(mfeat[,-241],method = "complete",index = "silhouette", max.nc = 10, min.nc = 10)
plot(pca.mfeat$ind$coord,col=complete.mfeat$Best.partition,main = "CAH complete sur mfeat")
#NbClust(mfeat, method = "kmeans", distance = "euclidean")
table(complete.mfeat$Best.partition,mfeat$class)
NMI(complete.mfeat$Best.partition,mfeat$class)
#0.469
ARI(complete.mfeat$Best.partition,mfeat$class)
#0.284






###5
hcpcward.mfeat=HCPC(pca.mfeat,method = "ward")
#3
hcpccomp.mfeat=HCPC(pca.mfeat,method = "complete")
#3
hcpcsingle.mfeat=HCPC(pca.mfeat,method = "single")
#3
hcpcav.mfeat=HCPC(pca.mfeat,method = "average")
#3


###6
#Comparaison avec le nombre de classes conseillée par HCPC
ward.mfeat.r2=NbClust(mfeat[,-241],method = "ward.D",index = "silhouette", max.nc = 3, min.nc = 3)$Best.partition
table(ward.mfeat.r2,hcpcward.mfeat$data.clust$clust)
NMI(ward.mfeat.r2,hcpcward.mfeat$data.clust$clust)
#0.34
ARI(ward.mfeat.r2,hcpcward.mfeat$data.clust$clust)
#0.24
plot(pca.mfeat$ind$coord,col=ward.mfeat.r2)

complete.mfeat.r2=NbClust(mfeat[,-241],method = "complete",index = "silhouette", max.nc = 3, min.nc = 3)$Best.partition
table(complete.mfeat.r2,hcpccomp.mfeat$data.clust$clust)
NMI(complete.mfeat.r2,hcpccomp.mfeat$data.clust$clust)
#0.25
ARI(complete.mfeat.r2,hcpccomp.mfeat$data.clust$clust)
#0.209
plot(pca.mfeat$ind$coord,col=complete.mfeat.r2)

single.mfeat.r2=NbClust(mfeat[,-241],method = "single",index = "silhouette", max.nc = 3, min.nc = 3)$Best.partition
table(single.mfeat.r2,hcpcsingle.mfeat$data.clust$clust)
NMI(single.mfeat.r2,hcpcsingle.mfeat$data.clust$clust)
#0.001
ARI(single.mfeat.r2,hcpcsingle.mfeat$data.clust$clust)
#0.0003

average.mfeat.r2=NbClust(mfeat[,-241],method = "average",index = "silhouette", max.nc = 3, min.nc = 3)$Best.partition
table(average.mfeat.r2,hcpcav.mfeat$data.clust$clust)
NMI(average.mfeat.r2,hcpcav.mfeat$data.clust$clust)
#0.28
ARI(average.mfeat.r2,hcpcav.mfeat$data.clust$clust)
#0.29

##Comparaison avec le vrais nombre de classes 
hcpcward.mfeat2=HCPC(pca.mfeat,method = "ward",nb.clust = 10)
table(ward.mfeat$Best.partition,hcpcward.mfeat2$data.clust$clust)
NMI(ward.mfeat$Best.partition,hcpcward.mfeat2$data.clust$clust)
#0.523
ARI(ward.mfeat$Best.partition,hcpcward.mfeat2$data.clust$clust)
#0.3560
plot(pca.mfeat$ind$coord,col=ward.mfeat$Best.partition)
plot(pca.mfeat$ind$coord,col=hcpcward.mfeat2$data.clust$clust)

hcpccomp.mfeat2=HCPC(pca.mfeat,method = "complete", nb.clust = 10)
table(complete.mfeat$Best.partition,hcpccomp.mfeat2$data.clust$clust)
NMI(complete.mfeat$Best.partition,hcpccomp.mfeat2$data.clust$clust)
#0.416
ARI(complete.mfeat$Best.partition,hcpccomp.mfeat2$data.clust$clust)
#0.269
plot(pca.mfeat$ind$coord,col=complete.mfeat$Best.partition)

hcpcsingle.mfeat2=HCPC(pca.mfeat,method = "single",nb.clust = 10)
table(single.mfeat$Best.partition,hcpcsingle.mfeat2$data.clust$clust)
NMI(single.mfeat$Best.partition,hcpcsingle.mfeat2$data.clust$clust)
#0.005
ARI(single.mfeat$Best.partition,hcpcsingle.mfeat2$data.clust$clust)
#-4.348269e-05
plot(pca.mfeat$ind$coord,col=single.mfeat$Best.partition)


hcpcav.mfeat2=HCPC(pca.mfeat,method = "average",nb.clust = 10)
table(average.mfeat$Best.partition,hcpcav.mfeat2$data.clust$clust)
NMI(average.mfeat$Best.partition,hcpcav.mfeat2$data.clust$clust)
#0.4312
ARI(average.mfeat$Best.partition,hcpcav.mfeat2$data.clust$clust)
#0.301





NMI(hcpcward.mfeat2$data.clust$clust,mfeat$class)
#0.480
ARI(hcpcward.mfeat2$data.clust$clust,mfeat$class)
#0.324

NMI(hcpccomp.mfeat2$data.clust$clust,mfeat$class)
#0.487
ARI(hcpccomp.mfeat2$data.clust$clust,mfeat$class)
#0.327

NMI(hcpcsingle.mfeat2$data.clust$clust,mfeat$class)
#0.48
ARI(hcpcsingle.mfeat2$data.clust$clust,mfeat$class)
#0.324

NMI(hcpcav.mfeat2$data.clust$clust,mfeat$class)
#0.49
ARI(hcpcav.mfeat2$data.clust$clust,mfeat$class)
#0.33


###7
pc.MF=prcomp(mfeat[,-241])
plot(as.data.frame(pc.MF$x[,1:2]), col=mfeat$class)
strategie=mixmodStrategy(algo="EM",initMethod="smallEM",nbTry=10,epsilonInInit=0.00001,seed=42)
#mix.mfeat=mixmodCluster(data=as.data.frame(pc.MF$x[,1:2]),nbCluster=10, strategy=strategie,dataType="quantitative",models=mixmodGaussianModel())
#Gaussian_pk_Lk_Dk_A_Dk
#VEV avec proportion egales
mix.mfeat=mixmodCluster(data=as.data.frame(pc.MF$x[,1:10]),nbCluster=10, strategy=strategie,dataType="quantitative",models=mixmodGaussianModel(listModels=c("Gaussian_pk_Lk_Dk_A_Dk")))

mix.mfeat@bestResult@partition
table(mix.mfeat@bestResult@partition,mfeat$class)
NMI(mix.mfeat@bestResult@partition,mfeat$class)
#0.742
ARI(mix.mfeat@bestResult@partition,mfeat$class)
#0.633
plot(as.data.frame(pc.MF$x[,1:2]), col=mix.mfeat@bestResult@partition)

mc.mfeat=Mclust(data= pc.MF$x[,1:10],G=10, modelNames = c("VVV"))
table( mc.mfeat$classification,mfeat$class)
NMI(mc.mfeat$classification,mfeat$class)
#0.760
ARI(mc.mfeat$classification,mfeat$class)
#0.680

#8
NMI(mix.mfeat@bestResult@partition,mc.mfeat$classification)
#0.740



###9

#MclustDR est une fonction de reduction de dimension pour visualiser des cluster obtenu a partir d'une densité gaussienne
mdr.mfeat=MclustDR(mc.mfeat)
#numdir=10
plot(mdr.mfeat)


#11


tsne.mfeat=Rtsne(mfeat[,-241],dim=2,perplexity=50,check_duplicates = FALSE)

plot(tsne.mfeat$Y,col=mfeat$class,xlab="1ere composante", ylab="2eme composante", main="Vraie partition de mfeat avec t-sne") #true

plot(tsne.mfeat$Y,col=km.mfeat$Best.partition, xlab="1ere composante", ylab="2eme composante", main="Kmean NbClust avec t-sne") #kmean NbClust

plot(tsne.mfeat$Y,col=ward.mfeat$Best.partition, xlab="1ere composante", ylab="2eme composante", main="Ward NbClust avec t-sne") #ward NbClust

plot(tsne.mfeat$Y,col=complete.mfeat$Best.partition, xlab="1ere composante", ylab="2eme composante", main="CAH Complete NbClust avec t-sne") #CAH complete NbClust

plot(tsne.mfeat$Y,col=single.mfeat$Best.partition, xlab="1ere composante", ylab="2eme composante", main="CAH single NbClust avec t-sne") #CAH single NbClust

plot(tsne.mfeat$Y,col=average.mfeat$Best.partition, xlab="1ere composante", ylab="2eme composante", main="CAH average NbClust avec t-sne") #CAH average NbClust


plot(tsne.mfeat$Y,col=hcpcward.mfeat2$data.clust$clust, xlab="1ere composante", ylab="2eme composante", main="CAH HCPC ward NbClust avec t-sne") #HCPC ward

plot(tsne.mfeat$Y,col=hcpccomp.mfeat2$data.clust$clust, xlab="1ere composante", ylab="2eme composante", main="CAH HCPC complete NbClust avec t-sne") #HCPC complete

plot(tsne.mfeat$Y,col=hcpcsingle.mfeat2$data.clust$clust, xlab="1ere composante", ylab="2eme composante", main="CAH HCPC single NbClust avec t-sne") #HCPC single

plot(tsne.mfeat$Y,col=hcpcav.mfeat2$data.clust$clust, xlab="1ere composante", ylab="2eme composante", main="CAH HCPC average NbClust avec t-sne") #HCPC average


plot(tsne.mfeat$Y,col=mix.mfeat@bestResult@partition, xlab="1ere composante", ylab="2eme composante", main="GMM Rmixmod avec t-sne") #GMM Rmixmod

plot(tsne.mfeat$Y,col=mc.mfeat$classification, xlab="1ere composante", ylab="2eme composante", main="GMM mclust avec t-sne") #GMM mclust


##12
library(keras)
install_keras()
mfeat_indice=createDataPartition(mfeat$class,p=0.9, list=F)
mfx_train=mfeat[mfeat_indice,-241]
mfy_train=mfeat[mfeat_indice,241]
mfx_test=mfeat[-mfeat_indice,-241]
mfy_test=mfeat[-mfeat_indice,241]
mfx_train=scale(mfx_train)
mfx_test=scale(mfx_test)

write.csv(mfx_train,"mfx_train.csv",row.names=FALSE)
write.csv(mfx_test,"mfx_test.csv",row.names=FALSE)
write.csv(mfeat[,-241],"mfeat.csv",row.names=FALSE)

auto3=read.csv("auto3.csv", header = F, skip=-1)
plot(auto3, col=mfeat$class,, main="Reduction de mfeat avec un autoencoder")
Automc.mfeat=Mclust(data= auto3,G=10,modelNames = c("VEV"))
NMI(Automc.mfeat$classification,mfeat$class)
#0.48 avec dim=2 et 0.658 avec dim=10
ARI(Automc.mfeat$classification,mfeat$class)
#0.333 avec dim=2 et 0.568 avec dim=10
plot(auto3, col=Automc.mfeat$classification,, main="Mclust de mfeat autoencoder")
