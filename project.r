library(readr)
project_ <- read_csv("project.csv")
View(project_)
summary(project_)
#sans id 
update_project<- project_[,2:13]
summary(update_project$domain)
# connaitre le nombre de projet de chaque type de domaine (caractere )
project2 <- update_project
project2$domain <- as.character(project2$domain)
library(purrr)
project3 <- as.data.frame(map_if(project2, is.character, as.factor))
summary(project3)
#kmeans
install.packages("factoextra")
library(factoextra)
#convert le budget double en integer
project3$budget<- as.integer(project3$budget)
View(project3)
library(dplyr)
#remove date fin et debut et tous les carateres
project4 <- subset(project3, select = -c(1,2,3,4,9,10,12) )
 kmeans(project4,4)
summary(kmProject)
#acp
library("FactoMineR")
library("factoextra")
library("pander")
res <- prcomp(project4, center = TRUE, scale = TRUE)
pander(factoextra::get_eig(res))
centrage_reduction<-function(x){return((x-mean(x))/sqrt(var(x)))}

project.centred<-apply(project4,2,centrage_reduction)
project.actif<-scale(project4,center = T, scale=T)  
acp_princomp<-princomp(project.centred,cor=T, score=T)  
summary(acp_princomp)
apply(project.actif,2,var)
apply(project.actif,2,mean)
valeur_propre<-acp_princomp$sdev^2
valeur_propre
c1<-acp_princomp$loadings[,1]*acp_princomp$sdev[1]
c2<-acp_princomp$loadings[,2]*acp_princomp$sdev[2]
c1
cor<-rbind (c1,c2)

cor
print(cor^2,digits = 2)
plot(c1,c2,xlim = c(-1,+1),ylim = c(-1,+1),type = "n")
abline(h=0,v=0)
text(c1,c2,labels =colnames(project4),cex=0.5)

fviz_pca_ind (res)

symbols(0,0,circles = 1, inches = F, add=T)  
abline(h=0,v=0)
a1<-acp_princomp$scores[,1]
a2<-acp_princomp$scores[,2]
plot(acp_princomp$scores[,1],acp_princomp$scores[,2],type="n",xlab="compl1 -74%", ylab = "comp2 -14%")
text(acp_princomp$scores[,1],acp_princomp$scores[,2],labels =rownames(acp_princomp),cex=0.5)
biplot(acp_princomp,cex=0.75)
fviz_pca_var(res, col.var = "black")
