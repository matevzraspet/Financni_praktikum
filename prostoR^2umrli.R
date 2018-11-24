# PODATKI V PROSTORU R^"

# knjižnice
library(readr)
library(rvest)
library(gsubfn)
library(dplyr)
library(stringr)
library(reshape2)
library(ggplot2)

# uvoz podatkov
umrl2 <- read_delim("umrl2.csv",  ";",
                    locale = locale(encoding = "Windows-1250", decimal_mark = "."))

colnames(umrl2) <- c("starost","1","2")  ### preimenovanje: moški = 1, ženske = 2
umrl2 <- umrl2[-1,] 

umrljivost <- melt(umrl2, id.vars ="starost", variable.name = "spol", value.name = "stevilo") %>% mutate(starost = parse_number(starost)) # v data frame

umrljivost$stevilo <- as.numeric(umrljivost$stevilo)

# graf
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
umrljivost$razdalje <- as.factor(razdalje)
odstopanja <- order(razdalje, decreasing=T)[1:20] # 202 podatkov, torej 10% od 201 = 20 ?!
umrljivost.brez.odstopanja <- umrljivost[-c(odstopanja),] # odstranim 10% najbolj odstopajoče vrednosti

graf_MNK_outliers <- ggplot(umrljivost.brez.odstopanja, aes(x=starost, y=stevilo, color = grupe)) + 
  geom_point()  + facet_grid(~ spol) +
  ggtitle("Število smrti v letu 2017 po starosti") + xlab("Starost") + ylab("Stevilo") +
  geom_point() +
  scale_color_manual(name="Kritične starosti:", values=c("red","green","blue"), labels=c("najbolj", "srednje", "najmanj"))
graf_MNK_outliers


# GRUPIRANJE PO MANHATTAN METODI 
set.seed(20)
dist(umrljivost, method = "manhattan") # vrne matriko razdalj Manhattan metodi
umrli <- kmeans(umrljivost[,c(1,3)], 3, nstart = 20)
umrli

grupe <- umrli$cluster
umrljivost$grupe <- as.factor(grupe)

# graf
graf_Manhattan <- ggplot(umrljivost, aes(x=starost, y=stevilo, color = grupe)) + 
  geom_point()  + facet_grid(~ spol) +
  ggtitle("Število smrti v letu 2017 po starosti") + xlab("Starost") + ylab("Stevilo") +
  geom_point() +
  scale_color_manual(name="Kritične starosti:", values=c("red","green","blue"), labels=c("najbolj", "srednje", "najmanj"))
graf_Manhattan

# OUTLIERS
razdalje1 <- abs(umrljivost[1] - umrljivost[5]) + abs(umrljivost[3] - umrljivost[6])
umrljivost$razdalje1 <- razdalje1
odstopanja1 <- order(razdalje1, decreasing=T)[1:20] # 202 podatkov, torej 10% od 201 = 20 ?!
umrljivost.brez.odstopanja1 <- umrljivost[-c(odstopanja1),] # odstranim 10% najbolj odstopajoče vrednosti

graf_Manhattan_outliers <- ggplot(umrljivost.brez.odstopanja1, aes(x=starost, y=stevilo, color = grupe)) + 
  geom_point()  + facet_grid(~ spol) +
  ggtitle("Število smrti v letu 2017 po starosti") + xlab("Starost") + ylab("Stevilo") +
  geom_point() +
  scale_color_manual(name="Kritične starosti:", values=c("red","green","blue"), labels=c("najbolj", "srednje", "najmanj"))
graf_MNK_outliers


# vORONOJEV DIAGRAM 1
library(deldir)

voronoi <- deldir(umrljivost$starost, umrljivost$stevilo)

#Now we can make a plot
ggplot(data=umrljivost, aes(x=starost,y=stevilo)) +
  #Plot the voronoi lines
  geom_segment(
    aes(x = x1, y = y1, xend = x2, yend = y2),
    size = 2,
    data = voronoi$dirsgs,
    linetype = 1,
    color= "#FFB958") + 
  #Plot the points
  geom_point(
    fill=rgb(70,130,180,255,maxColorValue=255),
    pch=21,
    size = 4,
    color="#333333") 

# VORONOJEV DIAGRAM 2
aaa <-data.matrix(umrljivost[,c(1,3)])
umrli <- kmeans(aaa,3)

library(tripack)
library(RColorBrewer)

CL5 <- brewer.pal(3, "Pastel1")
V <- voronoi.mosaic(umrli$centers[,1],umrli$centers[,2])
P <- voronoi.polygons(V)
plot(aaa,pch = 19,xlab= "Starost", ylab= "Število",col = CL5[umrli$cluster])
points(umrli$centers[,1], umrli$centers[,2],pch = 3, cex = 1.5, lwd = 2)
plot(V,add = TRUE)


############## iskanje optimalnega K z Elbow method

library(factoextra)
library(cluster)
library(NbClust)



set.seed(123)
# Compute and plot wss for k = 2 to k = 15
k.max <- 15 # Maximal number of clusters
data <- umrljivost[,c(1,3)]
wss <- sapply(1:k.max, 
              function(k){kmeans(data, k, nstart=10 )$tot.withinss})

plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")
abline(v = 3, lty =2)


fviz_nbclust(umrljivost[,c(1,3)], kmeans, method = "wss") +
  geom_vline(xintercept = 3, linetype = 2)














