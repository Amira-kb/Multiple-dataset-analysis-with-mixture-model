library(R.matlab)
library(Rmixmod)
library(mclust)
library(NbClust)
library(FactoMineR)
library(lle)
library(cluster)
library(aricode)

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
elbow(jaffe[,-677])
km.jaffe=kmeans(jaffe[,-677],10)
plot(pca.jaffe$ind$coord,col=km.jaffe$cluster)
table(km.jaffe$cluster,jaffe$class)
NMI(km.jaffe$cluster,jaffe$class)
#0.75
average.jaffe=hclust(as.matrix(jaffe[,-677]),method = "average")
wss <- sapply(2:30, function(k){kmeans(jaffe[,-677], k, nstart=50,iter.max = 15 )$tot.withinss})
plot(2:30,wss, type="b",xlab="Nombre de clusters",ylab="inertie",main = "La méthode du coude")

ward.jaffe=hclust(dist(jaffe[,-677]),method = "ward.D")
plot(ward.jaffe,main = "CAH Ward sur jaffe")
#NbClust(jaffe, method = "kmeans", distance = "euclidean")
ward.jaffe.r=cutree(ward.jaffe,k=10)
table(ward.jaffe.r,jaffe$class)
NMI(ward.jaffe.r,jaffe$class)
#0.883
average.jaffe=hclust(dist(jaffe[,-677]),method = "average")
plot(average.jaffe,main = "CAH average sur jaffe")
#NbClust(jaffe, method = "kmeans", distance = "euclidean")
average.jaffe.r=cutree(average.jaffe,k=10)
table(average.jaffe.r,jaffe$class)
NMI(average.jaffe.r,jaffe$class)
#0.608


single.jaffe=hclust(dist(jaffe[,-677]),method = "single")
plot(single.jaffe,main = "CAH single sur jaffe")
#NbClust(jaffe, method = "kmeans", distance = "euclidean")
single.jaffe.r=cutree(single.jaffe,k=10)
table(single.jaffe.r,jaffe$class)
NMI(single.jaffe.r,jaffe$class)
#0.39
View(head(go))

complete.jaffe=hclust(dist(jaffe[,-677]),method = "complete")
plot(complete.jaffe,main = "CAH complete sur jaffe")
#NbClust(jaffe, method = "kmeans", distance = "euclidean")
complete.jaffe.r=cutree(complete.jaffe,k=10)
table(complete.jaffe.r,jaffe$class)
NMI(complete.jaffe.r,jaffe$class)
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
ward.jaffe.r2=cutree(ward.jaffe,k=4)
table(ward.jaffe.r2,hcpcward.jeffe$data.clust$clust)
NMI(ward.jaffe.r2,hcpcward.jeffe$data.clust$clust)
#0.75
plot(pca.jaffe$ind$coord,col=ward.jaffe.r2)

complete.jaffe.r2=cutree(complete.jaffe,k=4)
table(complete.jaffe.r2,hcpccomp.jeffe$data.clust$clust)
NMI(complete.jaffe.r2,hcpccomp.jeffe$data.clust$clust)
#0.42
plot(pca.jaffe$ind$coord,col=complete.jaffe.r2)

single.jaffe.r2=cutree(single.jaffe,k=3)
table(single.jaffe.r2,hcpcsingle.jeffe$data.clust$clust)
NMI(single.jaffe.r2,hcpcsingle.jeffe$data.clust$clust)
#0.23
average.jaffe.r2=cutree(average.jaffe,k=3)
table(average.jaffe.r2,hcpcav.jeffe$data.clust$clust)
NMI(average.jaffe.r2,hcpcav.jeffe$data.clust$clust)
#0.16

##Comparaison avec le vrais nombre de classes 
hcpcward.jeffe2=HCPC(pca.jaffe,method = "ward",nb.clust = 10)
table(ward.jaffe.r,hcpcward.jeffe2$data.clust$clust)
NMI(ward.jaffe.r,hcpcward.jeffe2$data.clust$clust)
#0.79
plot(pca.jaffe$ind$coord,col=ward.jaffe.r)

hcpccomp.jeffe2=HCPC(pca.jaffe,method = "complete", nb.clust = 10)
table(complete.jaffe.r,hcpccomp.jeffe2$data.clust$clust)
NMI(complete.jaffe.r,hcpccomp.jeffe2$data.clust$clust)
#0.69
plot(pca.jaffe$ind$coord,col=complete.jaffe.r)

hcpcsingle.jeffe2=HCPC(pca.jaffe,method = "single",nb.clust = 10)
table(single.jaffe.r,hcpcsingle.jeffe2$data.clust$clust)
NMI(single.jaffe.r,hcpcsingle.jeffe2$data.clust$clust)
#0.33
hcpcav.jeffe2=HCPC(pca.jaffe,method = "average",nb.clust = 10)
table(average.jaffe.r,hcpcav.jeffe2$data.clust$clust)
NMI(average.jaffe.r,hcpcav.jeffe2$data.clust$clust)
#0.62

