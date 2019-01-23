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

op=readMat("Optdigits.mat")
optdigits=lapply(op, unlist, use.names=FALSE)
optdigits=as.data.frame(op$X)
op
class=as.factor(t(as.data.frame(op$y)))
optdigits=data.frame(op$X,class)
rm(class)
dim(optdigits)
table(optdigits$class)
#  1   2   3   4   5   6   7   8   9  10 
#571 557 572 568 558 558 566 554 562 554 
View(head(optdigits))
pca.optdigits=PCA(optdigits,ncp=2,quali.sup = 65)
plot(pca.optdigits, habillage=65)
#mds.optdigits=cmdscale(dist(optdigits[,-65]),k=2)
#plot(mds.optdigits, col=optdigits$class)
#lle.optdigits=lle(as.matrix(optdigits[,-65]),m=2, k=40,p=0.3)
#plot(lle.optdigits$Y, col=optdigits$class)
#sum(is.na(optdigits[,-65]))

###4
#elbow(optdigits[,-65])
set.seed(42)
#km.optdigits=kmeans(optdigits[,-65],10)
#plot(pca.optdigits$ind$coord,col=km.optdigits$cluster)
#table(km.optdigits$cluster,optdigits$class)
#NMI(km.optdigits$cluster,optdigits$class)
#0.78
#average.optdigits=hclust(as.matrix(optdigits[,-65]),method = "average")
#wss <- sapply(2:30, function(k){kmeans(optdigits[,-65], k, nstart=50,iter.max = 15 )$tot.withinss})
#plot(2:30,wss, type="b",xlab="Nombre de clusters",ylab="inertie",main = "La méthode du coude")


km.optdigits=NbClust(optdigits[,-65], method = "kmeans", distance = "euclidean", index = "silhouette",max.nc = 10)
#best nc = 8
km.optdigits=NbClust(optdigits[,-65], method = "kmeans", distance = "euclidean", index = "silhouette",min.nc = 10,max.nc = 10)
plot(pca.optdigits$ind$coord,col=km.optdigits$Best.partition)
table(km.optdigits$Best.partition,optdigits$class)
NMI(km.optdigits$Best.partition,optdigits$class)
#0.694
ARI(km.optdigits$Best.partition,optdigits$class)
#0.573

ward.optdigits=NbClust(optdigits[,-65],method = "ward.D",index = "silhouette", max.nc = 10)
#best nc 9
ward.optdigits=NbClust(optdigits[,-65],method = "ward.D",index = "silhouette", max.nc = 10, min.nc = 10)
table(ward.optdigits$Best.partition,optdigits$class)
NMI(ward.optdigits$Best.partition,optdigits$class)
#0.833
ARI(ward.optdigits$Best.partition,optdigits$class)
#0.763
plot(pca.optdigits$ind$coord,col=ward.optdigits$Best.partition,main = "CAH Ward sur optdigits")


average.optdigits=NbClust(optdigits[,-65],method = "average", max.nc = 10,index = "silhouette")
#2 cluster
average.optdigits=NbClust(optdigits[,-65],method = "average",min.nc = 10, max.nc = 10,index = "silhouette")
plot(pca.optdigits$ind$coord,col=average.optdigits$Best.partition,main = "CAH average sur optdigits")
table(average.optdigits$Best.partition,optdigits$class)
NMI(average.optdigits$Best.partition,optdigits$class)
#0.695
ARI(average.optdigits$Best.partition,optdigits$class)
#0.57





single.optdigits=NbClust(optdigits[,-65],method = "single",index = "silhouette", max.nc = 10)
#2 cluster
single.optdigits=NbClust(optdigits[,-65],method = "single",index = "silhouette", max.nc = 10,min.nc = 10)

plot(pca.optdigits$ind$coord,col=single.optdigits$Best.partition,main = "CAH single sur optdigits")
table(single.optdigits$Best.partition,optdigits$class)
NMI(single.optdigits$Best.partition,optdigits$class)
#0.001
ARI(single.optdigits$Best.partition,optdigits$class)
#1.856392e-06

complete.optdigits=NbClust(optdigits[,-65],method = "complete",index = "silhouette",max.nc = 10)
#13 cluster
complete.optdigits=NbClust(optdigits[,-65],method = "complete",index = "silhouette", max.nc = 10, min.nc = 10)
plot(pca.optdigits$ind$coord,col=complete.optdigits$Best.partition,main = "CAH complete sur optdigits")
#NbClust(optdigits, method = "kmeans", distance = "euclidean")
table(complete.optdigits$Best.partition,optdigits$class)
NMI(complete.optdigits$Best.partition,optdigits$class)
#0.5483
ARI(complete.optdigits$Best.partition,optdigits$class)
#0.36






###5
hcpcward.optdigits=HCPC(pca.optdigits,method = "ward")
#3
hcpccomp.optdigits=HCPC(pca.optdigits,method = "complete")
#3
hcpcsingle.optdigits=HCPC(pca.optdigits,method = "single")
#3
hcpcav.optdigits=HCPC(pca.optdigits,method = "average")
#3


###6
##Comparaison avec le vrais nombre de classes 
hcpcward.optdigits2=HCPC(pca.optdigits,method = "ward",nb.clust = 10)
table(ward.optdigits$Best.partition,hcpcward.optdigits2$data.clust$clust)
NMI(ward.optdigits$Best.partition,hcpcward.optdigits2$data.clust$clust)
#0.4966
ARI(ward.optdigits$Best.partition,hcpcward.optdigits2$data.clust$clust)
#0.339
plot(pca.optdigits$ind$coord,col=ward.optdigits$Best.partition)
plot(pca.optdigits$ind$coord,col=hcpcward.optdigits2$data.clust$clust)

hcpccomp.optdigits2=HCPC(pca.optdigits,method = "complete", nb.clust = 10)
table(complete.optdigits$Best.partition,hcpccomp.optdigits2$data.clust$clust)
NMI(complete.optdigits$Best.partition,hcpccomp.optdigits2$data.clust$clust)
#0.384
ARI(complete.optdigits$Best.partition,hcpccomp.optdigits2$data.clust$clust)
#0.198
plot(pca.optdigits$ind$coord,col=complete.optdigits$Best.partition)

hcpcsingle.optdigits2=HCPC(pca.optdigits,method = "single",nb.clust = 10)
table(single.optdigits$Best.partition,hcpcsingle.optdigits2$data.clust$clust)
NMI(single.optdigits$Best.partition,hcpcsingle.optdigits2$data.clust$clust)
#0.001
ARI(single.optdigits$Best.partition,hcpcsingle.optdigits2$data.clust$clust)
#-0.0001
plot(pca.optdigits$ind$coord,col=single.optdigits$Best.partition)


hcpcav.optdigits2=HCPC(pca.optdigits,method = "average",nb.clust = 10)
table(average.optdigits$Best.partition,hcpcav.optdigits2$data.clust$clust)
NMI(average.optdigits$Best.partition,hcpcav.optdigits2$data.clust$clust)
#0.089
ARI(average.optdigits$Best.partition,hcpcav.optdigits2$data.clust$clust)
#0.04




NMI(hcpcward.optdigits2$data.clust$clust,optdigits$class)
#0.459
ARI(hcpcward.optdigits2$data.clust$clust,optdigits$class)
#0.324
NMI(hcpccomp.optdigits2$data.clust$clust,optdigits$class)
#0.459
ARI(hcpccomp.optdigits2$data.clust$clust,optdigits$class)
#0.323
NMI(hcpcsingle.optdigits2$data.clust$clust,optdigits$class)
#0.459
ARI(hcpcsingle.optdigits2$data.clust$clust,optdigits$class)
#0.324
NMI(hcpcav.optdigits2$data.clust$clust,optdigits$class)
#0.459
ARI(hcpcav.optdigits2$data.clust$clust,optdigits$class)
#0.324


###7
pc.op=prcomp(optdigits[,-65])
plot(as.data.frame(pc.op$x[,1:2]), col=optdigits$class)
strategie=mixmodStrategy(algo="EM",initMethod="smallEM",nbTry=10,epsilonInInit=0.00001,seed=42)
#mix.optdigits=mixmodCluster(data=as.data.frame(pc.op$x[,1:2]),nbCluster=10, strategy=strategie,dataType="quantitative",models=mixmodGaussianModel())
#Gaussian_pk_L_Dk_A_Dk
#EEV avec proportion egales
mix.optdigits=mixmodCluster(data=as.data.frame(pc.op$x[,1:10]),nbCluster=10, strategy=strategie,dataType="quantitative",models=mixmodGaussianModel(listModels=c("Gaussian_pk_L_Dk_A_Dk")))

mix.optdigits@bestResult@partition
table(mix.optdigits@bestResult@partition,optdigits$class)
NMI(mix.optdigits@bestResult@partition,optdigits$class)
#0.825
ARI(mix.optdigits@bestResult@partition,optdigits$class)
#0.7740
plot(as.data.frame(pc.op$x[,1:2]), col=mix.optdigits@bestResult@partition)

mc.optdigits=Mclust(data= pc.op$x[,1:10],G=10, modelNames = c("EEV"))
table( mc.optdigits$classification,optdigits$class)
NMI(mc.optdigits$classification,optdigits$class)
#0.88
ARI(mc.optdigits$classification,optdigits$class)
#0.881

#8
NMI(mix.optdigits@bestResult@partition,mc.optdigits$classification)
#0.847



###9

#MclustDR est une fonction de reduction de dimension pour visualiser des cluster obtenu a partir d'une densité gaussienne
mdr.optdigits=MclustDR(mc.optdigits)
plot(mdr.optdigits)


#11


tsne.optdigits=Rtsne(optdigits[,-65],dim=2,perplexity=50,check_duplicates = FALSE)

plot(tsne.optdigits$Y,col=optdigits$class,xlab="1ere composante", ylab="2eme composante", main="Vraie partition de optdigits avec t-sne") #true

plot(tsne.optdigits$Y,col=km.optdigits$Best.partition, xlab="1ere composante", ylab="2eme composante", main="Kmean NbClust avec t-sne") #kmean NbClust

plot(tsne.optdigits$Y,col=ward.optdigits$Best.partition, xlab="1ere composante", ylab="2eme composante", main="Ward NbClust avec t-sne") #ward NbClust

plot(tsne.optdigits$Y,col=complete.optdigits$Best.partition, xlab="1ere composante", ylab="2eme composante", main="CAH Complete NbClust avec t-sne") #CAH complete NbClust

plot(tsne.optdigits$Y,col=single.optdigits$Best.partition, xlab="1ere composante", ylab="2eme composante", main="CAH single NbClust avec t-sne") #CAH single NbClust

plot(tsne.optdigits$Y,col=average.optdigits$Best.partition, xlab="1ere composante", ylab="2eme composante", main="CAH average NbClust avec t-sne") #CAH average NbClust


plot(tsne.optdigits$Y,col=hcpcward.optdigits2$data.clust$clust, xlab="1ere composante", ylab="2eme composante", main="CAH HCPC ward NbClust avec t-sne") #HCPC ward

plot(tsne.optdigits$Y,col=hcpccomp.optdigits2$data.clust$clust, xlab="1ere composante", ylab="2eme composante", main="CAH HCPC complete NbClust avec t-sne") #HCPC complete

plot(tsne.optdigits$Y,col=hcpcsingle.optdigits2$data.clust$clust, xlab="1ere composante", ylab="2eme composante", main="CAH HCPC single NbClust avec t-sne") #HCPC single

plot(tsne.optdigits$Y,col=hcpcav.optdigits2$data.clust$clust, xlab="1ere composante", ylab="2eme composante", main="CAH HCPC average NbClust avec t-sne") #HCPC average


plot(tsne.optdigits$Y,col=mix.optdigits@bestResult@partition, xlab="1ere composante", ylab="2eme composante", main="GMM Rmixmod avec t-sne") #GMM Rmixmod

plot(tsne.optdigits$Y,col=mc.optdigits$classification, xlab="1ere composante", ylab="2eme composante", main="GMM mclust avec t-sne") #GMM mclust


##12

optdigits_indice=createDataPartition(optdigits$class,p=0.9, list=F)
ox_train=optdigits[optdigits_indice,-65]
oy_train=optdigits[optdigits_indice,65]
ox_test=optdigits[-optdigits_indice,-65]
oy_test=optdigits[-optdigits_indice,65]
ox_train=((ox_train- min(ox_train)) /(max(ox_train)-min(ox_train)))
ox_test=((ox_test- min(ox_test)) /(max(ox_test)-min(ox_test)))

write.csv(ox_train,"ox_train.csv",row.names=FALSE)
write.csv(ox_test,"ox_test.csv",row.names=FALSE)
write.csv(optdigits[,-65],"optdigits.csv",row.names=FALSE)

auto5=read.csv("auto5.csv", header = F, skip=-1)
plot(auto5, col=optdigits$class,, main="Reduction de optdigits avec un autoencoder")
Automc.optdigits=Mclust(data= tsne.optdigits$Y,G=10, modelNames = c("VVV"))
NMI(Automc.optdigits$classification,optdigits$class)
#0.677  et 0.843 avec dim=10
ARI(Automc.optdigits$classification,optdigits$class)
#0.566 et 0.787 avec dim=10
plot(auto5, col=Automc.optdigits$classification,, main="Mclust de optdigits autoencoder")


#Testons Mclust en prenant le resultat de la tsne pour entrée
tsnemc.optdigits=Mclust(data= tsne.optdigits$Y,G=10, modelNames = c("VVV"))
NMI(tsnemc.optdigits$classification,optdigits$class)
#0.920
ARI(tsnemc.optdigits$classification,optdigits$class)
#0.897
plot(tsne.optdigits$Y, col=tsnemc.optdigits$classification,, main="Mclust de optdigits a partir de la tsne")


