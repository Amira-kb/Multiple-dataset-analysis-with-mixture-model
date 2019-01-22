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

us=readMat("USPS.mat")
usps=lapply(us, unlist, use.names=FALSE)
usps=as.data.frame(usps$X)
usps
class=as.factor(t(as.data.frame(us$y)))
usps=data.frame(us$X,class)
rm(class)
dim(usps)
table(usps$class)
#2000 257
View(head(usps))
pca.usps=PCA(usps,ncp=2,quali.sup = 257)
plot(pca.usps, habillage=257)
#mds.usps=cmdscale(dist(usps[,-257]),k=2)
#plot(mds.usps, col=usps$class)
#lle.usps=lle(as.matrix(usps[,-257]),m=2, k=40,p=0.3)
#plot(lle.usps$Y, col=usps$class)
#sum(is.na(usps[,-257]))

###4
#elbow(usps[,-257])
set.seed(42)
#km.usps=kmeans(usps[,-257],10)
#plot(pca.usps$ind$coord,col=km.usps$cluster)
#table(km.usps$cluster,usps$class)
#NMI(km.usps$cluster,usps$class)
#0.78
#average.usps=hclust(as.matrix(usps[,-257]),method = "average")
#wss <- sapply(2:30, function(k){kmeans(usps[,-257], k, nstart=50,iter.max = 15 )$tot.withinss})
#plot(2:30,wss, type="b",xlab="Nombre de clusters",ylab="inertie",main = "La méthode du coude")


km.usps=NbClust(usps[,-257], method = "kmeans", distance = "euclidean", index = "silhouette",max.nc = 10)
#best nc = 2
km.usps=NbClust(usps[,-257], method = "kmeans", distance = "euclidean", index = "silhouette",min.nc = 10,max.nc = 10)
plot(pca.usps$ind$coord,col=km.usps$Best.partition)
table(km.usps$Best.partition,usps$class)
NMI(km.usps$Best.partition,usps$class)
#0.613
ARI(km.usps$Best.partition,usps$class)
#0.537

ward.usps=NbClust(usps[,-257],method = "ward.D",index = "silhouette", max.nc = 10)
#best nc >=10
ward.usps=NbClust(usps[,-257],method = "ward.D",index = "silhouette", max.nc = 10, min.nc = 10)
table(ward.usps$Best.partition,usps$class)
NMI(ward.usps$Best.partition,usps$class)
#0.772
ARI(ward.usps$Best.partition,usps$class)
#0.676
plot(pca.usps$ind$coord,col=ward.usps$Best.partition,main = "CAH Ward sur usps")


average.usps=NbClust(usps[,-257],method = "average", max.nc = 10,index = "silhouette")
#2 cluster
average.usps=NbClust(usps[,-257],method = "average",min.nc = 10, max.nc = 10,index = "silhouette")
plot(pca.usps$ind$coord,col=average.usps$Best.partition,main = "CAH average sur usps")
table(average.usps$Best.partition,usps$class)
NMI(average.usps$Best.partition,usps$class)
#0.08
ARI(average.usps$Best.partition,usps$class)
#0.007





single.usps=NbClust(usps[,-257],method = "single",index = "silhouette", max.nc = 10)
#2
single.usps=NbClust(usps[,-257],method = "single",index = "silhouette", max.nc = 10,min.nc = 10)

plot(pca.usps$ind$coord,col=single.usps$Best.partition,main = "CAH single sur usps")
table(single.usps$Best.partition,usps$class)
NMI(single.usps$Best.partition,usps$class)
#0.0009
ARI(single.usps$Best.partition,usps$class)
#4.098145e-06

complete.usps=NbClust(usps[,-257],method = "complete",index = "silhouette", max.nc = 10)
#2
complete.usps=NbClust(usps[,-257],method = "complete",index = "silhouette", max.nc = 10, min.nc = 10)
plot(pca.usps$ind$coord,col=complete.usps$Best.partition,main = "CAH complete sur usps")
#NbClust(usps, method = "kmeans", distance = "euclidean")
table(complete.usps$Best.partition,usps$class)
NMI(complete.usps$Best.partition,usps$class)
#0.3532
ARI(complete.usps$Best.partition,usps$class)
#0.188






###5
hcpcward.usps=HCPC(pca.usps,method = "ward")
#3
hcpccomp.usps=HCPC(pca.usps,method = "complete")
#3
hcpcsingle.usps=HCPC(pca.usps,method = "single")
#3
hcpcav.usps=HCPC(pca.usps,method = "average")
#3


###6
##Comparaison avec le vrais nombre de classes 
hcpcward.usps2=HCPC(pca.usps,method = "ward",nb.clust = 10)
table(ward.usps$Best.partition,hcpcward.usps2$data.clust$clust)
NMI(ward.usps$Best.partition,hcpcward.usps2$data.clust$clust)
#0.391
ARI(ward.usps$Best.partition,hcpcward.usps2$data.clust$clust)
#0.281
plot(pca.usps$ind$coord,col=ward.usps$Best.partition)
plot(pca.usps$ind$coord,col=hcpcward.usps2$data.clust$clust)

hcpccomp.usps2=HCPC(pca.usps,method = "complete", nb.clust = 10)
table(complete.usps$Best.partition,hcpccomp.usps2$data.clust$clust)
NMI(complete.usps$Best.partition,hcpccomp.usps2$data.clust$clust)
#0.305
ARI(complete.usps$Best.partition,hcpccomp.usps2$data.clust$clust)
#0.192
plot(pca.usps$ind$coord,col=complete.usps$Best.partition)

hcpcsingle.usps2=HCPC(pca.usps,method = "single",nb.clust = 10)
table(single.usps$Best.partition,hcpcsingle.usps2$data.clust$clust)
NMI(single.usps$Best.partition,hcpcsingle.usps2$data.clust$clust)
#0.001
ARI(single.usps$Best.partition,hcpcsingle.usps2$data.clust$clust)
#0.0002
plot(pca.usps$ind$coord,col=single.usps$Best.partition)


hcpcav.usps2=HCPC(pca.usps,method = "average",nb.clust = 10)
table(average.usps$Best.partition,hcpcav.usps2$data.clust$clust)
NMI(average.usps$Best.partition,hcpcav.usps2$data.clust$clust)
#0.089
ARI(average.usps$Best.partition,hcpcav.usps2$data.clust$clust)
#0.04


NMI(hcpcward.usps2$data.clust$clust,usps$class)
#0.373
ARI(hcpcward.usps2$data.clust$clust,usps$class)
#0.284
NMI(hcpccomp.usps2$data.clust$clust,usps$class)
#0.376
ARI(hcpccomp.usps2$data.clust$clust,usps$class)
#0.289
NMI(hcpcsingle.usps2$data.clust$clust,usps$class)
#0.374
ARI(hcpcsingle.usps2$data.clust$clust,usps$class)
#0.286
NMI(hcpcav.usps2$data.clust$clust,usps$class)
#0.373
ARI(hcpcav.usps2$data.clust$clust,usps$class)
#0.284



###7
pc.US=prcomp(usps[,-257])
plot(as.data.frame(pc.US$x[,1:2]), col=usps$class)
strategie=mixmodStrategy(algo="EM",initMethod="smallEM",nbTry=10,epsilonInInit=0.00001,seed=42)
#mix.usps=mixmodCluster(data=as.data.frame(pc.MF$x[,1:2]),nbCluster=10, strategy=strategie,dataType="quantitative",models=mixmodGaussianModel())
#Gaussian_pk_Lk_Bk
#VVI avec proportion egales
mix.usps=mixmodCluster(data=as.data.frame(pc.US$x[,1:10]),nbCluster=10, strategy=strategie,dataType="quantitative",models=mixmodGaussianModel(listModels=c("Gaussian_pk_Lk_Bk")))

mix.usps@bestResult@partition
table(mix.usps@bestResult@partition,usps$class)
NMI(mix.usps@bestResult@partition,usps$class)
#0.566
ARI(mix.usps@bestResult@partition,usps$class)
#0.320
plot(as.data.frame(pc.US$x[,1:2]), col=mix.usps@bestResult@partition)


#2 composantes
mc.usps=Mclust(data= pc.US$x[,1:2],G=10, modelNames = c("VVV"))
table( mc.usps$classification,usps$class)
NMI(mc.usps$classification,usps$class)
#0.428
ARI(mc.usps$classification,usps$class)
#0.317
#10 composantes
mc.usps=Mclust(data= pc.US$x[,1:10],G=10, modelNames = c("VEV"))
table( mc.usps$classification,usps$class)
NMI(mc.usps$classification,usps$class)
#0.655
ARI(mc.usps$classification,usps$class)
#0.493

#8
NMI(mix.usps@bestResult@partition,mc.usps$classification)
#0.627



###9

#MclustDR est une fonction de reduction de dimension pour visualiser des cluster obtenu a partir d'une densité gaussienne
mdr.usps=MclustDR(mc.usps)
#numdir=10
plot(mdr.usps)


#11


tsne.usps=Rtsne(usps[,-257],dim=2,perplexity=50,check_duplicates = FALSE)

plot(tsne.usps$Y,col=usps$class,xlab="1ere composante", ylab="2eme composante", main="Vraie partition de usps avec t-sne") #true

plot(tsne.usps$Y,col=km.usps$Best.partition, xlab="1ere composante", ylab="2eme composante", main="Kmean NbClust avec t-sne") #kmean NbClust

plot(tsne.usps$Y,col=ward.usps$Best.partition, xlab="1ere composante", ylab="2eme composante", main="Ward NbClust avec t-sne") #ward NbClust

plot(tsne.usps$Y,col=complete.usps$Best.partition, xlab="1ere composante", ylab="2eme composante", main="CAH Complete NbClust avec t-sne") #CAH complete NbClust

plot(tsne.usps$Y,col=single.usps$Best.partition, xlab="1ere composante", ylab="2eme composante", main="CAH single NbClust avec t-sne") #CAH single NbClust

plot(tsne.usps$Y,col=average.usps$Best.partition, xlab="1ere composante", ylab="2eme composante", main="CAH average NbClust avec t-sne") #CAH average NbClust


plot(tsne.usps$Y,col=hcpcward.usps2$data.clust$clust, xlab="1ere composante", ylab="2eme composante", main="CAH HCPC ward NbClust avec t-sne") #HCPC ward

plot(tsne.usps$Y,col=hcpccomp.usps2$data.clust$clust, xlab="1ere composante", ylab="2eme composante", main="CAH HCPC complete NbClust avec t-sne") #HCPC complete

plot(tsne.usps$Y,col=hcpcsingle.usps2$data.clust$clust, xlab="1ere composante", ylab="2eme composante", main="CAH HCPC single NbClust avec t-sne") #HCPC single

plot(tsne.usps$Y,col=hcpcav.usps2$data.clust$clust, xlab="1ere composante", ylab="2eme composante", main="CAH HCPC average NbClust avec t-sne") #HCPC average


plot(tsne.usps$Y,col=mix.usps@bestResult@partition, xlab="1ere composante", ylab="2eme composante", main="GMM Rmixmod avec t-sne") #GMM Rmixmod

plot(tsne.usps$Y,col=mc.usps$classification, xlab="1ere composante", ylab="2eme composante", main="GMM mclust avec t-sne") #GMM mclust


##12
library(keras)
install_keras()
usps_indice=createDataPartition(usps$class,p=0.9, list=F)
ux_train=usps[usps_indice,-257]
uy_train=usps[usps_indice,257]
ux_test=usps[-usps_indice,-257]
uy_test=usps[-usps_indice,257]
ux_train=((ux_train- min(ux_train)) /(max(ux_train)-min(ux_train)))
ux_test=((ux_train- min(ux_train)) /(max(ux_train)-min(ux_train)))

write.csv(ux_train,"ux_train.csv",row.names=FALSE)
write.csv(ux_test,"ux_test.csv",row.names=FALSE)
write.csv(usps[,-257],"usps.csv",row.names=FALSE)

auto4=read.csv("auto4.csv", header = F, skip=-1)
plot(auto4, col=usps$class,, main="Reduction de usps avec un autoencoder")
Automc.usps=Mclust(data=auto4,G=10,c("VVV"))
NMI(Automc.usps$classification,usps$class)
#0.695
ARI(Automc.usps$classification,usps$class)
#0.522
plot(auto4, col=Automc.usps$classification,, main="Mclust de usps autoencoder")

