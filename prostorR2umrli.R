# PODATKI V PROSTORU R^2

# knjižnice
library(readr)
library(rvest)
library(gsubfn)
library(dplyr)
library(stringr)
library(reshape2)
library(ggplot2)
library(tidyverse)

# uvoz podatkov
umrl2 <- read_delim("umrl2.csv",  ";",
                    locale = locale(encoding = "Windows-1250", decimal_mark = "."))

colnames(umrl2) <- c("starost","1","2")  # preimenovanje: moški = 1, ženske = 2
umrl2 <- umrl2[-1,] 

umrljivost <- melt(umrl2, id.vars ="starost", variable.name = "spol", value.name = "stevilo") %>% mutate(starost = parse_number(starost)) # v data frame

umrljivost$stevilo <- as.numeric(umrljivost$stevilo)

# graf prvotnih podatkov umrljivosti, ločen po spolu
graf2 <- ggplot(umrljivost, aes(x=starost, y=stevilo)) + geom_point()  + facet_grid(~ spol)
graf2

# GRUPIRANJE PO METODI NAJMANJŠIH KVADRATOV
set.seed(20)
dist(umrljivost, method = "euclidean") # vrne matriko razdalj po MNK
umrli <- kmeans(umrljivost[,c(1,3)], 3, nstart = 20)
umrli

grupe <- umrli$cluster
umrljivost$grupe <- as.factor(grupe)

# graf
graf_MNK <- ggplot(umrljivost, aes(x=starost, y=stevilo, color = grupe)) + 
  geom_point()  + facet_grid(~ spol) +
  ggtitle("Število smrti v letu 2017 po starosti") + xlab("Starost") + ylab("Stevilo") +
  geom_point() +
  scale_color_manual(name="Kritične starosti:", values=c("red","green","blue"), labels=c("najbolj", "srednje", "najmanj"))
graf_MNK

# OUTLIERS
centri_starost <- umrli$centers[c(1,2,3), 1]
umrljivost$centri_starost <- centri_starost[umrljivost$grupe]
centri_stevilo <- umrli$centers[c(1,2,3), 2]
umrljivost$centri_stevilo <- centri_stevilo[umrljivost$grupe]
razdalje <- sqrt((umrljivost[1] - umrljivost[5])^2 + (umrljivost[3] - umrljivost[6])^2)
umrljivost$razdalje <- as.vector(razdalje)

# v vseh naslednjih vrsticah so poračunane vsote kvadratov razdalj znotraj posamezne grupe
umrljivostG1 <- umrljivost %>% filter(stevilo, grupe == "1")
sum(umrljivostG1$razdalje) # 1660.51
umrljivostG2 <- umrljivost %>% filter(stevilo, grupe == "2")
sum(umrljivostG2$razdalje) # 3089.899
umrljivostG3 <- umrljivost %>% filter(stevilo, grupe == "3")
sum(umrljivostG3$razdalje) # 3089.899

odstopanja <- order(razdalje, decreasing=T)[1:20] # 202 podatkov, torej 10% od 201 = 20 ?!
umrljivost.brez.odstopanja <- umrljivost[-c(odstopanja),] # odstranim 10% najbolj odstopajoče vrednosti

umrljG1 <- umrljivost.brez.odstopanja %>% filter(stevilo, grupe == "1")
sum(umrljG1$razdalje) # 715.3376
umrljG2 <- umrljivost.brez.odstopanja %>% filter(stevilo, grupe == "2")
sum(umrljG2$razdalje) # 2840.258
umrljG3 <- umrljivost.brez.odstopanja %>% filter(stevilo, grupe == "3")
sum(umrljG3$razdalje) # 1745.705

# graf
graf_MNK_outliers <- ggplot(umrljivost.brez.odstopanja, aes(x=starost, y=stevilo, color = grupe)) + 
  geom_point()  + facet_grid(~ spol) +
  ggtitle("Število smrti v letu 2017 po starosti") + xlab("Starost") + ylab("Stevilo") +
  geom_point() +
  scale_color_manual(name="Kritične starosti:", values=c("red","green","blue"), labels=c("najbolj", "srednje", "najmanj"))
graf_MNK_outliers


# GRUPIRANJE PO MANHATTAN METODI 
set.seed(20)
dist(umrljivost, method = "manhattan") # vrne matriko razdalj Manhattan metodi
umrli

grupe <- umrli$cluster

# graf
graf_Manhattan <- ggplot(umrljivost, aes(x=starost, y=stevilo, color = grupe)) + 
  geom_point()  + facet_grid(~ spol) +
  ggtitle("Število smrti v letu 2017 po starosti") + xlab("Starost") + ylab("Stevilo") +
  geom_point() +
  scale_color_manual(name="Kritične starosti:", values=c("red","green","blue"), labels=c("najbolj", "srednje", "najmanj"))
graf_Manhattan

# OUTLIERS
razdalje1 <- abs(umrljivost$starost - umrljivost$centri_starost) + abs(umrljivost$stevilo - umrljivost$centri_stevilo)
umrljivost$razdalje1 <- as.vector(razdalje1)

UmrljG1 <- umrljivost %>% filter(grupe == "1")
sum(UmrljG1$razdalje1) # 1758.385
UmrljG2 <- umrljivost %>% filter(grupe == "2")
sum(UmrljG2$razdalje1) # 4781.426
UmrljG3 <- umrljivost %>% filter(grupe == "3")
sum(UmrljG3$razdalje1) #  2788.809

odstopanja1 <- order(razdalje1, decreasing=T)[1:20] # 202 podatkov, torej 10% od 201 = 20 
umrljivost.brez.odstopanja1 <- umrljivost[-c(odstopanja1),] # odstranim 10% najbolj odstopajoče vrednosti

UG1 <- umrljivost.brez.odstopanja1 %>% filter(grupe == "1")
sum(UG1$razdalje1) # 859.5385
UG2 <- umrljivost.brez.odstopanja1 %>% filter(grupe == "2")
sum(UG2$razdalje1) # 4164.729
UG3 <- umrljivost.brez.odstopanja1 %>% filter(grupe == "3")
sum(UG3$razdalje1) # 2187.936

# graf
graf_Manhattan_outliers <- ggplot(umrljivost.brez.odstopanja1, aes(x=starost, y=stevilo, color = grupe)) + 
  geom_point()  + facet_grid(~ spol) +
  ggtitle("Število smrti v letu 2017 po starosti") + xlab("Starost") + ylab("Stevilo") +
  geom_point() +
  scale_color_manual(name="Kritične starosti:", values=c("red","green","blue"), labels=c("najbolj", "srednje", "najmanj"))
graf_MNK_outliers


# vORONOJEV DIAGRAM 1

library(deldir)

voronoi <- deldir(umrljivost$starost, umrljivost$stevilo)

graf_voronoi1 <- ggplot(data=umrljivost, aes(x=starost,y=stevilo)) +
  # nariše črte
  geom_segment(
    aes(x = x1, y = y1, xend = x2, yend = y2),
    size = 2,
    data = voronoi$dirsgs,
    linetype = 1,
    color= "#FFB958") + 
  # nariše točke oz centre
  geom_point(
    fill=rgb(70,130,180,255,maxColorValue=255),
    pch=21,
    size = 4,
    color="#333333") 
graf_voronoi1

# VORONOJEV DIAGRAM 2

podatki <-data.matrix(umrljivost[,c(1,3)])
umrli <- kmeans(podatki,3)

library(tripack)
library(RColorBrewer)

CL5 <- brewer.pal(3, "Pastel1")
V <- voronoi.mosaic(umrli$centers[,1],umrli$centers[,2])
P <- voronoi.polygons(V)
plot(podatki,pch = 19,xlab= "Starost", ylab= "Število",col = CL5[umrli$cluster])
points(umrli$centers[,1], umrli$centers[,2],pch = 3, cex = 1.5, lwd = 2)
plot(V,add = TRUE)


# ISKANJE OPTIMALNEGA k - z Elbow method

library(factoextra)
library(cluster)
library(NbClust)

set.seed(123)  
k.max <- 15 # največje število grup
data <- umrljivost[,c(1,3)] #stolpca starost in število
wss <- sapply(1:k.max, 
              function(k){kmeans(data, k, nstart=10 )$tot.withinss})  ## izračuna kmeans$tot.withinss za k = 1:15

plot(1:k.max, wss,   ### narišemo graf optimalnega k
     type="b", pch = 19, frame = FALSE, 
     xlab="število množic k",
     ylab="Total within-cluster sum of squares",
     main = "Optimalno število množic")
abline(v = 3, lty =2)





