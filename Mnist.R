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

m=readMat("MNIST5.mat")
mnist=lapply(m, unlist, use.names=FALSE)
mnist=as.data.frame(mnist$X)
mnist
class=as.factor(t(as.data.frame(m$y)))
mnist=data.frame(m$X,class)
rm(class)
dim(mnist)
table(mnist$class)
#213 785
View(head(mnist))
pca.mnist=PCA(mnist,ncp=2,quali.sup = 785)
plot(pca.mnist, habillage=785)
#mds.mnist=cmdscale(dist(mnist[,-785]),k=2)
#plot(mds.mnist, col=mnist$class)
#lle.mnist=lle(as.matrix(mnist[,-785]),m=2, k=40,p=0.3)
#plot(lle.mnist$Y, col=mnist$class)
#sum(is.na(mnist[,-785]))

###4
#elbow(mnist[,-785])
set.seed(42)
#km.mnist=kmeans(mnist[,-785],10)
#plot(pca.mnist$ind$coord,col=km.mnist$cluster)
#table(km.mnist$cluster,mnist$class)
#NMI(km.mnist$cluster,mnist$class)
#0.78
#average.mnist=hclust(as.matrix(mnist[,-785]),method = "average")
#wss <- sapply(2:30, function(k){kmeans(mnist[,-785], k, nstart=50,iter.max = 15 )$tot.withinss})
#plot(2:30,wss, type="b",xlab="Nombre de clusters",ylab="inertie",main = "La méthode du coude")


km.mnist=NbClust(mnist[,-785], method = "kmeans", distance = "euclidean", index = "silhouette",max.nc = 10)
#best nc = 8
km.mnist=NbClust(mnist[,-785], method = "kmeans", distance = "euclidean", index = "silhouette",min.nc = 10,max.nc = 10)
plot(pca.mnist$ind$coord,col=km.mnist$Best.partition)
table(km.mnist$Best.partition,mnist$class)
NMI(km.mnist$Best.partition,mnist$class)
#0.486


ward.mnist=NbClust(mnist[,-785],method = "ward.D",index = "silhouette", max.nc = 10)
#best nc 2
ward.mnist=NbClust(mnist[,-785],method = "ward.D",index = "silhouette", max.nc = 10, min.nc = 10)
table(ward.mnist$Best.partition,mnist$class)
NMI(ward.mnist$Best.partition,mnist$class)
#0.594
plot(pca.mnist$ind$coord,col=ward.mnist$Best.partition,main = "CAH Ward sur mnist")


average.mnist=NbClust(mnist[,-785],method = "average", max.nc = 10,index = "silhouette")
#2 cluster
average.mnist=NbClust(mnist[,-785],method = "average",min.nc = 10, max.nc = 10,index = "silhouette")
plot(pca.mnist$ind$coord,col=average.mnist$Best.partition,main = "CAH average sur mnist")
table(average.mnist$Best.partition,mnist$class)
NMI(average.mnist$Best.partition,mnist$class)
#0.129






single.mnist=NbClust(mnist[,-785],method = "single",index = "silhouette", max.nc = 10)
#2
single.mnist=NbClust(mnist[,-785],method = "single",index = "silhouette", max.nc = 10,min.nc = 10)

plot(pca.mnist$ind$coord,col=single.mnist$Best.partition,main = "CAH single sur mnist")
table(single.mnist$Best.partition,mnist$class)
NMI(single.mnist$Best.partition,mnist$class)
#0.0025


complete.mnist=NbClust(mnist[,-785],method = "complete",index = "silhouette", max.nc = 10)
#2
complete.mnist=NbClust(mnist[,-785],method = "complete",index = "silhouette", max.nc = 10, min.nc = 10)
plot(pca.mnist$ind$coord,col=complete.mnist$Best.partition,main = "CAH complete sur mnist")
#NbClust(mnist, method = "kmeans", distance = "euclidean")
table(complete.mnist$Best.partition,mnist$class)
NMI(complete.mnist$Best.partition,mnist$class)
#0.289







###5
hcpcward.mnist=HCPC(pca.mnist,method = "ward")
#3
hcpccomp.mnist=HCPC(pca.mnist,method = "complete")
#3
hcpcsingle.mnist=HCPC(pca.mnist,method = "single")
#3
hcpcav.mnist=HCPC(pca.mnist,method = "average")
#3


###6
#Comparaison avec le nombre de classes conseillée par HCPC
ward.mnist.r2=NbClust(mnist[,-785],method = "ward.D",index = "silhouette", max.nc = 3, min.nc = 3)$Best.partition
table(ward.mnist.r2,hcpcward.mnist$data.clust$clust)
NMI(ward.mnist.r2,hcpcward.mnist$data.clust$clust)
#0.134
ARI(ward.mnist.r2,hcpcward.mnist$data.clust$clust)
#-0.014
plot(pca.mnist$ind$coord,col=ward.mnist.r2)

complete.mnist.r2=NbClust(mnist[,-785],method = "complete",index = "silhouette", max.nc = 3, min.nc = 3)$Best.partition
table(complete.mnist.r2,hcpccomp.mnist$data.clust$clust)
NMI(complete.mnist.r2,hcpccomp.mnist$data.clust$clust)
#0.1012
ARI(complete.mnist.r2,hcpccomp.mnist$data.clust$clust)
#0.137
plot(pca.mnist$ind$coord,col=complete.mnist.r2)

single.mnist.r2=NbClust(mnist[,-785],method = "single",index = "silhouette", max.nc = 3, min.nc = 3)$Best.partition
table(single.mnist.r2,hcpcsingle.mnist$data.clust$clust)
NMI(single.mnist.r2,hcpcsingle.mnist$data.clust$clust)
#0.009
ARI(single.mnist.r2,hcpcsingle.mnist$data.clust$clust)
#0.0008

average.mnist.r2=NbClust(mnist[,-785],method = "average",index = "silhouette", max.nc = 3, min.nc = 3)$Best.partition
table(average.mnist.r2,hcpcav.mnist$data.clust$clust)
NMI(average.mnist.r2,hcpcav.mnist$data.clust$clust)
#0.0019
ARI(average.mnist.r2,hcpcav.mnist$data.clust$clust)
#0.0024

##Comparaison avec le vrais nombre de classes 
hcpcward.mnist2=HCPC(pca.mnist,method = "ward",nb.clust = 10)
table(ward.mnist$Best.partition,hcpcward.mnist2$data.clust$clust)
NMI(ward.mnist$Best.partition,hcpcward.mnist2$data.clust$clust)
#0.291
ARI(ward.mnist$Best.partition,hcpcward.mnist2$data.clust$clust)
#0.147
plot(pca.mnist$ind$coord,col=ward.mnist$Best.partition)

hcpccomp.mnist2=HCPC(pca.mnist,method = "complete", nb.clust = 10)
table(complete.mnist$Best.partition,hcpccomp.mnist2$data.clust$clust)
NMI(complete.mnist$Best.partition,hcpccomp.mnist2$data.clust$clust)
#0.224
ARI(complete.mnist$Best.partition,hcpccomp.mnist2$data.clust$clust)
#0.213
plot(pca.mnist$ind$coord,col=complete.mnist$Best.partition)

hcpcsingle.mnist2=HCPC(pca.mnist,method = "single",nb.clust = 10)
table(single.mnist$Best.partition,hcpcsingle.mnist2$data.clust$clust)
NMI(single.mnist$Best.partition,hcpcsingle.mnist2$data.clust$clust)
#0.0038
ARI(single.mnist$Best.partition,hcpcsingle.mnist2$data.clust$clust)
#0.0011

hcpcav.mnist2=HCPC(pca.mnist,method = "average",nb.clust = 10)
table(average.mnist$Best.partition,hcpcav.mnist2$data.clust$clust)
NMI(average.mnist$Best.partition,hcpcav.mnist2$data.clust$clust)
#0.117
ARI(average.mnist$Best.partition,hcpcav.mnist2$data.clust$clust)
#0.070

###7
pc.M=prcomp(mnist[,-785])
plot(as.data.frame(pc.M$x[,1:2]), col=mnist$class)
strategie=mixmodStrategy(algo="EM",initMethod="smallEM",nbTry=10,epsilonInInit=0.00001,seed=42)
#mix.mnist=mixmodCluster(data=as.data.frame(pc.M$x[,1:2]),nbCluster=10, strategy=strategie,dataType="quantitative",models=mixmodGaussianModel())
#Gaussian_p_Lk_Dk_A_Dk
mix.mnist=mixmodCluster(data=as.data.frame(pc.M$x[,1:10]),nbCluster=10, strategy=strategie,dataType="quantitative",models=mixmodGaussianModel(listModels=c("Gaussian_p_Lk_Dk_A_Dk")))
#VEV
mix.mnist@bestResult@partition
table(mix.mnist@bestResult@partition,mnist$class)
NMI(mix.mnist@bestResult@partition,mnist$class)
#0.60
ARI(mix.mnist@bestResult@partition,mnist$class)
#0.48
plot(as.data.frame(pc.M$x[,1:2]), col=mix.mnist@bestResult@partition)

mc.mnist=Mclust(data= pc.M$x[,1:2],G=10, modelNames = c("VEV"))
table( mc.mnist$classification,mnist$class)
NMI(mc.mnist$classification,mnist$class)
#0.34
ARI(mc.mnist$classification,mnist$class)
#0.20

#8
NMI(mix.mnist@bestResult@partition,mc.mnist$classification)
#0.39



###9

#MclustDR est une fonction de reduction de dimension pour visualiser des cluster obtenu a partir d'une densité gaussienne
mdr.mnist=MclustDR(mc.mnist)
plot(mdr.mnist)






#11


tsne.mnist=Rtsne(mnist[,-785],dim=2,perplexity=50)

plot(tsne.mnist$Y,col=mnist$class,xlab="1ere composante", ylab="2eme composante", main="Vraie partition de Mnist avec t-sne") #true

plot(tsne.mnist$Y,col=km.mnist$Best.partition, xlab="1ere composante", ylab="2eme composante", main="Kmean NbClust avec t-sne") #kmean NbClust

plot(tsne.mnist$Y,col=ward.mnist$Best.partition, xlab="1ere composante", ylab="2eme composante", main="Ward NbClust avec t-sne") #ward NbClust

plot(tsne.mnist$Y,col=complete.mnist$Best.partition, xlab="1ere composante", ylab="2eme composante", main="CAH Complete NbClust avec t-sne") #CAH complete NbClust

plot(tsne.mnist$Y,col=single.mnist$Best.partition, xlab="1ere composante", ylab="2eme composante", main="CAH single NbClust avec t-sne") #CAH single NbClust

plot(tsne.mnist$Y,col=average.mnist$Best.partition, xlab="1ere composante", ylab="2eme composante", main="CAH average NbClust avec t-sne") #CAH average NbClust


plot(tsne.mnist$Y,col=hcpcward.mnist2$data.clust$clust, xlab="1ere composante", ylab="2eme composante", main="CAH HCPC ward NbClust avec t-sne") #HCPC ward

plot(tsne.mnist$Y,col=hcpccomp.mnist2$data.clust$clust, xlab="1ere composante", ylab="2eme composante", main="CAH HCPC complete NbClust avec t-sne") #HCPC complete

plot(tsne.mnist$Y,col=hcpcsingle.mnist2$data.clust$clust, xlab="1ere composante", ylab="2eme composante", main="CAH HCPC single NbClust avec t-sne") #HCPC single

plot(tsne.mnist$Y,col=hcpcav.mnist2$data.clust$clust, xlab="1ere composante", ylab="2eme composante", main="CAH HCPC average NbClust avec t-sne") #HCPC average


plot(tsne.mnist$Y,col=mix.mnist@bestResult@partition, xlab="1ere composante", ylab="2eme composante", main="GMM Rmixmod avec t-sne") #GMM Rmixmod

plot(tsne.mnist$Y,col=mc.mnist$classification, xlab="1ere composante", ylab="2eme composante", main="GMM mclust avec t-sne") #GMM mclust


##12
library(keras)
install_keras()
mnist_indice=createDataPartition(mnist$class,p=0.8, list=F)
mx_train=mnist[mnist_indice,-785]
my_train=mnist[mnist_indice,785]
mx_test=mnist[-mnist_indice,-785]
my_test=mnist[-mnist_indice,785]
mx_train=mx_train/255
mx_test=mx_test/255

write.csv(mx_train,"mx_train.csv",row.names=FALSE)
write.csv(mx_test,"mx_test.csv",row.names=FALSE)
write.csv(mnist[,-785],"mnist.csv",row.names=FALSE)

auto2=read.csv("auto2.csv", header = F, skip=-1)
plot(auto2, col=mnist$class,, main="Reduction de mnist avec un autoencoder")
Automc.mnist=Mclust(data= auto2,G=10)
NMI(Automc.mnist$classification,mnist$class)
#0.45
ARI(Automc.mnist$classification,mnist$class)
#0.32
plot(auto2, col=Automc.mnist$classification,, main="Mclust de mnist autoencoder")
