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

j=readMat("jaffe.mat")
jaffe=lapply(j, unlist, use.names=FALSE)
jaffe=as.data.frame(jaffe$X)
jaffe
class=as.factor(t(as.data.frame(j$y)))
jaffe=data.frame(j$X,class)
rm(class)
dim(jaffe)
table(jaffe$class)
#213 677
View(head(jaffe))
pca.jaffe=PCA(jaffe,ncp=2,quali.sup = 677)
plot(pca.jaffe, habillage=677)
#mds.jaffe=cmdscale(dist(jaffe[,-677]),k=2)
#plot(mds.jaffe, col=jaffe$class)
#lle.jaffe=lle(as.matrix(jaffe[,-677]),m=2, k=40,p=0.3)
#plot(lle.jaffe$Y, col=jaffe$class)
#sum(is.na(jaffe[,-677]))

###4
#elbow(jaffe[,-677])
set.seed(42)
#km.jaffe=kmeans(jaffe[,-677],10)
#plot(pca.jaffe$ind$coord,col=km.jaffe$cluster)
#table(km.jaffe$cluster,jaffe$class)
#NMI(km.jaffe$cluster,jaffe$class)
#0.78
#average.jaffe=hclust(as.matrix(jaffe[,-677]),method = "average")
#wss <- sapply(2:30, function(k){kmeans(jaffe[,-677], k, nstart=50,iter.max = 15 )$tot.withinss})
#plot(2:30,wss, type="b",xlab="Nombre de clusters",ylab="inertie",main = "La méthode du coude")


km.jaffe=NbClust(jaffe[,-677], method = "kmeans", distance = "euclidean", index = "silhouette",max.nc = 20)
#best nc = 13
km.jaffe=NbClust(jaffe[,-677], method = "kmeans", distance = "euclidean", index = "silhouette",min.nc = 10,max.nc = 10)
plot(pca.jaffe$ind$coord,col=km.jaffe$Best.partition)
table(km.jaffe$Best.partition,jaffe$class)
NMI(km.jaffe$Best.partition,jaffe$class)
#0.884


ward.jaffe=NbClust(jaffe[,-677],method = "ward.D",index = "silhouette", max.nc = 30)
#best nc 20
ward.jaffe=NbClust(jaffe[,-677],method = "ward.D",index = "silhouette", max.nc = 10, min.nc = 10)
table(ward.jaffe$Best.partition,jaffe$class)
NMI(ward.jaffe$Best.partition,jaffe$class)
#0.883
plot(pca.jaffe$ind$coord,col=ward.jaffe$Best.partition,main = "CAH Ward sur jaffe")


average.jaffe=NbClust(jaffe[,-677],method = "average", max.nc = 50,index = "silhouette")
#43 cluster
average.jaffe=NbClust(jaffe[,-677],method = "average",min.nc = 10, max.nc = 10,index = "silhouette")
plot(pca.jaffe$ind$coord,col=average.jaffe$Best.partition,main = "CAH average sur jaffe")
table(average.jaffe$Best.partition,jaffe$class)
NMI(average.jaffe$Best.partition,jaffe$class)
#0.608






single.jaffe=NbClust(jaffe[,-677],method = "single",index = "silhouette", max.nc = 50)
#43
single.jaffe=NbClust(jaffe[,-677],method = "single",index = "silhouette", max.nc = 10,min.nc = 10)

plot(pca.jaffe$ind$coord,col=single.jaffe$Best.partition,main = "CAH single sur jaffe")
table(single.jaffe$Best.partition,jaffe$class)
NMI(single.jaffe$Best.partition,jaffe$class)
#0.39


complete.jaffe=NbClust(jaffe[,-677],method = "complete",index = "silhouette", max.nc = 50)
#28
complete.jaffe=NbClust(jaffe[,-677],method = "complete",index = "silhouette", max.nc = 10, min.nc = 10)
plot(pca.jaffe$ind$coord,col=complete.jaffe$Best.partition,main = "CAH complete sur jaffe")
#NbClust(jaffe, method = "kmeans", distance = "euclidean")
table(complete.jaffe$Best.partition,jaffe$class)
NMI(complete.jaffe$Best.partition,jaffe$class)
#0.66







###5
hcpcward.jeffe=HCPC(pca.jaffe,method = "ward")
#4
hcpccomp.jeffe=HCPC(pca.jaffe,method = "complete")
#4
hcpcsingle.jeffe=HCPC(pca.jaffe,method = "single")
#3
hcpcav.jeffe=HCPC(pca.jaffe,method = "average")
#3


###6
#Comparaison avec le nombre de classes conseillée par HCPC
ward.jaffe.r2=NbClust(jaffe[,-677],method = "ward.D",index = "silhouette", max.nc = 4, min.nc = 4)$Best.partition
table(ward.jaffe.r2,hcpcward.jeffe$data.clust$clust)
NMI(ward.jaffe.r2,hcpcward.jeffe$data.clust$clust)
#0.7514
plot(pca.jaffe$ind$coord,col=ward.jaffe.r2)

complete.jaffe.r2=NbClust(jaffe[,-677],method = "complete",index = "silhouette", max.nc = 4, min.nc = 4)$Best.partition
table(complete.jaffe.r2,hcpccomp.jeffe$data.clust$clust)
NMI(complete.jaffe.r2,hcpccomp.jeffe$data.clust$clust)
#0.429
plot(pca.jaffe$ind$coord,col=complete.jaffe.r2)

single.jaffe.r2=NbClust(jaffe[,-677],method = "single",index = "silhouette", max.nc = 3, min.nc = 3)$Best.partition
table(single.jaffe.r2,hcpcsingle.jeffe$data.clust$clust)
NMI(single.jaffe.r2,hcpcsingle.jeffe$data.clust$clust)
#0.230
average.jaffe.r2=NbClust(jaffe[,-677],method = "average",index = "silhouette", max.nc = 3, min.nc = 3)$Best.partition
table(average.jaffe.r2,hcpcav.jeffe$data.clust$clust)
NMI(average.jaffe.r2,hcpcav.jeffe$data.clust$clust)
#0.162

##Comparaison avec le vrais nombre de classes 
hcpcward.jeffe2=HCPC(pca.jaffe,method = "ward",nb.clust = 10)
table(ward.jaffe$Best.partition,hcpcward.jeffe2$data.clust$clust)
NMI(ward.jaffe$Best.partition,hcpcward.jeffe2$data.clust$clust)
#0.79
plot(pca.jaffe$ind$coord,col=ward.jaffe$Best.partition)

hcpccomp.jeffe2=HCPC(pca.jaffe,method = "complete", nb.clust = 10)
table(complete.jaffe$Best.partition,hcpccomp.jeffe2$data.clust$clust)
NMI(complete.jaffe$Best.partition,hcpccomp.jeffe2$data.clust$clust)
#0.69
plot(pca.jaffe$ind$coord,col=complete.jaffe.r)

hcpcsingle.jeffe2=HCPC(pca.jaffe,method = "single",nb.clust = 10)
table(single.jaffe$Best.partition,hcpcsingle.jeffe2$data.clust$clust)
NMI(single.jaffe$Best.partition,hcpcsingle.jeffe2$data.clust$clust)
#0.331
hcpcav.jeffe2=HCPC(pca.jaffe,method = "average",nb.clust = 10)
table(average.jaffe$Best.partition,hcpcav.jeffe2$data.clust$clust)
NMI(average.jaffe$Best.partition,hcpcav.jeffe2$data.clust$clust)
#0.624


###7
pc=prcomp(jaffe[,-677])
plot(as.data.frame(pc$x[,1:2]), col=jaffe$class)
strategie=mixmodStrategy(algo="EM",initMethod="smallEM",nbTry=10,epsilonInInit=0.00001,seed=42)
mix.jaffe=mixmodCluster(data=as.data.frame(pc$x[,1:10]),nbCluster=10, strategy=strategie,dataType="quantitative",models=mixmodGaussianModel(listModels=c("Gaussian_p_Lk_C")))
#EVE
mix.jaffe@bestResult@partition
table(mix.jaffe@bestResult@partition,jaffe$class)
NMI(mix.jaffe@bestResult@partition,jaffe$class)
plot(as.data.frame(pc$x[,1:2]), col=mix.jaffe@bestResult@partition)
#0.90

mc.jaffe=Mclust(data= jaffe[,-677],G=10)
table( mc.jaffe$classification,jaffe$class)
NMI(mc.jaffe$classification,jaffe$class)
#0.95


#8
NMI(mix.jaffe@bestResult@partition,mc.jaffe$classification)
#0.92



###9

#MclustDR est une fonction de reduction de dimension pour visualiser des cluster obtenu a partir d'une densité gaussienne
mdr.jaffe=MclustDR(mc.jaffe)
plot(mdr.jaffe)






#11


tsne.jaffe=Rtsne(jaffe[,-677],dim=2,perplexity=50)

plot(tsne.jaffe$Y,col=jaffe$class,xlab="1ere composante", ylab="2eme composante", main="Vraie partition avec t-sne") #true

plot(tsne.jaffe$Y,col=km.jaffe$Best.partition, xlab="1ere composante", ylab="2eme composante", main="Kmean NbClust avec t-sne") #kmean NbClust

plot(tsne.jaffe$Y,col=ward.jaffe$Best.partition, xlab="1ere composante", ylab="2eme composante", main="Ward NbClust avec t-sne") #ward NbClust

plot(tsne.jaffe$Y,col=complete.jaffe$Best.partition, xlab="1ere composante", ylab="2eme composante", main="CAH Complete NbClust avec t-sne") #CAH complete NbClust

plot(tsne.jaffe$Y,col=single.jaffe$Best.partition, xlab="1ere composante", ylab="2eme composante", main="CAH single NbClust avec t-sne") #CAH single NbClust

plot(tsne.jaffe$Y,col=average.jaffe$Best.partition, xlab="1ere composante", ylab="2eme composante", main="CAH average NbClust avec t-sne") #CAH average NbClust


plot(tsne.jaffe$Y,col=hcpcward.jeffe2$data.clust$clust, xlab="1ere composante", ylab="2eme composante", main="CAH HCPC ward NbClust avec t-sne") #HCPC ward

plot(tsne.jaffe$Y,col=hcpccomp.jeffe2$data.clust$clust, xlab="1ere composante", ylab="2eme composante", main="CAH HCPC complete NbClust avec t-sne") #HCPC complete

plot(tsne.jaffe$Y,col=hcpcsingle.jeffe2$data.clust$clust, xlab="1ere composante", ylab="2eme composante", main="CAH HCPC single NbClust avec t-sne") #HCPC single

plot(tsne.jaffe$Y,col=hcpcav.jeffe2$data.clust$clust, xlab="1ere composante", ylab="2eme composante", main="CAH HCPC average NbClust avec t-sne") #HCPC average


plot(tsne.jaffe$Y,col=mix.jaffe@bestResult@partition, xlab="1ere composante", ylab="2eme composante", main="GMM Rmixmod avec t-sne") #GMM Rmixmod

plot(tsne.jaffe$Y,col=mc.jaffe$classification, xlab="1ere composante", ylab="2eme composante", main="GMM mclust avec t-sne") #GMM mclust


##12
install_keras()
jaffe_indice=createDataPartition(jaffe$class,p=0.5, list=F)
jx_train=jaffe[jaffe_indice,-677]
jy_train=jaffe[jaffe_indice,677]
jx_test=jaffe[-jaffe_indice,-677]
jy_test=jaffe[-jaffe_indice,677]
jx_train=jx_train/255
jx_test=jx_test/255

write.csv(jx_train,"jx_train.csv",row.names=FALSE)
write.csv(jx_test,"jx_test.csv",row.names=FALSE)
write.csv(jaffe[,-677],"jaffe.csv",row.names=FALSE)

auto=read.csv("auto.csv", header = F, skip=-1)
plot(auto, col=jaffe$class,, main="Reduction de jaffe avec un autoencoder")
Automc.jaffe=Mclust(data= auto,G=10)
NMI(Automc.jaffe$classification,jaffe$class)
plot(auto, col=Automc.jaffe$classification,, main="Mclust de jaffe autoencoder")

#0.807

#plot(ward.jaffe,main = "CAH Ward sur jaffe")MclustDR
#NbClust(jaffe, method = "kmeans", distance = "euclidean")
#ward.jaffe.r=cutree(ward.jaffe,k=10)
#table(ward.jaffe.r,jaffe$class)
#NMI(ward.jaffe.r,jaffe$class)

#ward.jaffe=hclust(dist(jaffe[,-677]),method = "ward.D")
#plot(ward.jaffe,main = "CAH Ward sur jaffe")
#NbClust(jaffe, method = "kmeans", distance = "euclidean")
#ward.jaffe.r=cutree(ward.jaffe,k=10)
#table(ward.jaffe.r,jaffe$class)
#NMI(ward.jaffe.r,jaffe$class)
#0.883






#average.jaffe=hclust(dist(jaffe[,-677]),method = "average")
#plot(average.jaffe,main = "CAH average sur jaffe")
#NbClust(jaffe, method = "kmeans", distance = "euclidean")
#average.jaffe.r=cutree(average.jaffe,k=10)
#table(average.jaffe.r,jaffe$class)
#NMI(average.jaffe.r,jaffe$class)
#0.608






#single.jaffe=hclust(dist(jaffe[,-677]),method = "single")
#plot(single.jaffe,main = "CAH single sur jaffe")
#NbClust(jaffe, method = "kmeans", distance = "euclidean")
#single.jaffe.r=cutree(single.jaffe,k=10)
#table(single.jaffe.r,jaffe$class)
#NMI(single.jaffe.r,jaffe$class)
#0.39


#complete.jaffe=hclust(dist(jaffe[,-677]),method = "complete")
#plot(complete.jaffe,main = "CAH complete sur jaffe")
#NbClust(jaffe, method = "kmeans", distance = "euclidean")
#complete.jaffe.r=cutree(complete.jaffe,k=10)
#table(complete.jaffe.r,jaffe$class)
#NMI(complete.jaffe.r,jaffe$class)
#0.66
