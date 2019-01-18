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
hcpcsingle.jeffe=HCPC(pca.jaffe,method = "single")
hcpcav.jeffe=HCPC(pca.jaffe,method = "average")
