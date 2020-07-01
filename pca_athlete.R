library(FactoMineR)
decathlon<-read.csv("C:/Users/KEERTHIVASAN/Desktop/Big Mart Sales/decathlon.csv")

decathlon<- data.frame(decathlon[,-1], row.names=decathlon[,1])

res.pca=PCA(decathlon[,1:10],scale.unit=TRUE,ncp=5,graph=T)

#Adding supplementary variables
res.pca=PCA(decathlon[,1:12],scale.unit=TRUE,ncp=5,quanti.sup=c(11:12),graph=T)

#Adding qualitative supplementary variable
res.pca=PCA(decathlon,scale.unit=TRUE,ncp=5,quanti.sup=c(11:12),quali.sup=13,graph=T)
plot.PCA(res.pca,axes=c(1,2),choix="ind",habillage=13)
