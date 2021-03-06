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
###2
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


####3
pca.jaffe=PCA(jaffe,ncp=2,quali.sup = 677)
plot(pca.jaffe, habillage=677)


###4

set.seed(42)


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
#43 cluster
single.jaffe=NbClust(jaffe[,-677],method = "single",index = "silhouette", max.nc = 10,min.nc = 10)

plot(pca.jaffe$ind$coord,col=single.jaffe$Best.partition,main = "CAH single sur jaffe")
table(single.jaffe$Best.partition,jaffe$class)
NMI(single.jaffe$Best.partition,jaffe$class)
#0.39


complete.jaffe=NbClust(jaffe[,-677],method = "complete",index = "silhouette", max.nc = 50)
#28 cluster
complete.jaffe=NbClust(jaffe[,-677],method = "complete",index = "silhouette", max.nc = 10, min.nc = 10)
plot(pca.jaffe$ind$coord,col=complete.jaffe$Best.partition,main = "CAH complete sur jaffe")
#NbClust(jaffe, method = "kmeans", distance = "euclidean")
table(complete.jaffe$Best.partition,jaffe$class)
NMI(complete.jaffe$Best.partition,jaffe$class)
#0.66


#Le nombre de classes varie beaucoup trop en fonction des variables
#


###5
hcpcward.jeffe=HCPC(pca.jaffe,method = "ward")
#4 cluster
hcpccomp.jeffe=HCPC(pca.jaffe,method = "complete")
#4 cluster
hcpcsingle.jeffe=HCPC(pca.jaffe,method = "single")
#3 cluster
hcpcav.jeffe=HCPC(pca.jaffe,method = "average")
#3 cluster

#On pourrait proposer 3 ou 4 classes
#

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


##On peut dire que plus l'ACP est performante sur le datat
##plus les resultats de l'HCPC et Nclust sont proches

##Comparaison entre le resultat de HCPC avec 10 classes et
##la table des vraie classes
NMI(hcpcward.jeffe2$data.clust$clust,jaffe$class)
#0.744
ARI(hcpcward.jeffe2$data.clust$clust,jaffe$class)
#0.574

NMI(hcpccomp.jeffe2$data.clust$clust,jaffe$class)
#0.734
ARI(hcpccomp.jeffe2$data.clust$clust,jaffe$class)
#0.5715

NMI(hcpcsingle.jeffe2$data.clust$clust,jaffe$class)
#0.663
ARI(hcpcsingle.jeffe2$data.clust$clust,jaffe$class)
#0.453

NMI(hcpcav.jeffe2$data.clust$clust,jaffe$class)
#0.669
ARI(hcpcav.jeffe2$data.clust$clust,jaffe$class)
#0.434



###7
pc=prcomp(jaffe[,-677])
plot(as.data.frame(pc$x[,1:2]), col=jaffe$class)
strategie=mixmodStrategy(algo="EM",initMethod="smallEM",nbTry=10,epsilonInInit=0.00001,seed=42)
mix.jaffe=mixmodCluster(data=as.data.frame(pc$x[,1:10]),nbCluster=10, strategy=strategie,dataType="quantitative",models=mixmodGaussianModel(listModels=c("Gaussian_p_Lk_C")))
#EVE
mix.jaffe@bestResult@partition
table(mix.jaffe@bestResult@partition,jaffe$class)
NMI(mix.jaffe@bestResult@partition,jaffe$class)
#0.90

plot(as.data.frame(pc$x[,1:2]), col=mix.jaffe@bestResult@partition)

mc.jaffe=Mclust(data= jaffe[,-677],G=10)
table( mc.jaffe$classification,jaffe$class)
NMI(mc.jaffe$classification,jaffe$class)
#0.95

#8
NMI(mix.jaffe@bestResult@partition,mc.jaffe$classification)
#0.92

#Les deux partitions sont très proches


###9

#MclustDR est une fonction de reduction de dimension 
#pour visualiser des cluster obtenu à partir 
#d'une densité gaussienne
mdr.jaffe=MclustDR(mc.jaffe)
plot(mdr.jaffe)






#11


tsne.jaffe=Rtsne(jaffe[,-677],dim=2,perplexity=50)

plot(tsne.jaffe$Y,col=jaffe$class,xlab="1ere composante", ylab="2eme composante", main="Vraie partition avec t-sne") #true
#On remarque qu'en general t-sne donne une bonne representation 
#des different dataset du projet

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

#Dans cette partie nous allons separer le jeu de données en deux
#avec un jeu d'entraine et de test pour entrainer 
#un auto encodeur
#Pour des raison pratique nous avons préféré sauvegarder ces
#donnée sous forma csv pour ensuite pouvoir les faire migner
#entre R et Python
jaffe_indice=createDataPartition(jaffe$class,p=0.5, list=F)
jx_train=jaffe[jaffe_indice,-677]
jy_train=jaffe[jaffe_indice,677]
jx_test=jaffe[-jaffe_indice,-677]
jy_test=jaffe[-jaffe_indice,677]
jx_train=scale(jx_train)
jx_test=scale(jx_test)

write.csv(jx_train,"jx_train.csv",row.names=FALSE)
write.csv(jx_test,"jx_test.csv",row.names=FALSE)
write.csv(jaffe[,-677],"jaffe.csv",row.names=FALSE)

#Voir autoencodeur.ipynb pour voir le code de l'autoencodeur


auto=read.csv("auto.csv", header = F, skip=-1) #jeu reduit par l'auto-encodeur
plot(auto, col=jaffe$class, main="Reduction de jaffe avec un autoencoder")
Automc.jaffe=Mclust(data= auto,G=10)
NMI(Automc.jaffe$classification,jaffe$class)
plot(auto, col=Automc.jaffe$classification,, main="Mclust de jaffe autoencoder")
#0.807
ARI(Automc.jaffe$classification,jaffe$class)
#0.60



#Testons Mclust en prenant le resultat de la tsne pour entrée
tsnemc.jaffe=Mclust(data= tsne.jaffe$Y,G=10, modelNames = c("VVV"))
NMI(tsnemc.jaffe$classification,jaffe$class)
#0.9
ARI(tsnemc.jaffe$classification,jaffe$class)
#0.774
plot(tsne.jaffe$Y, col=tsnemc.jaffe$classification,, main="Mclust de jaffe a partir de la tsne")



## MFEA.R, USPS.R, optidigits.R et Mnist.R sont quasiment
## indentique niveau code à la difference près
## qu'ils utilisé d'autre jeu de données que Jaffe
