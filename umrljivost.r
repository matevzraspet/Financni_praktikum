library(readr)
library(rvest)
library(gsubfn)
library(dplyr)
library(stringr)
library(reshape2)
library(ggplot2)

umrl2 <- read_delim("umrl2.csv",  ";",
                    
                    locale = locale(encoding = "Windows-1250", decimal_mark = "."))

colnames(umrl2) <- c("starost","1","2")  ### Preimenoval moški-1, ženske- 2
umrl2 <- umrl2[-1,] 
umrljivost <- melt(umrl2, id.vars ="starost", variable.name = "spol", value.name = "stevilo") %>% mutate(starost = parse_number(starost))
umrljivost$stevilo <- as.numeric(umrljivost$stevilo)
ggplot(umrljivost, aes(x=starost, y=stevilo)) + geom_point()  + facet_grid(cols=vars(spol))


set.seed(20)
umrli <- kmeans(umrljivost[,c(1,3)], 3, nstart = 20)
umrli

umrli$cluster <- as.factor(umrli$cluster)
ggplot(umrljivost, aes(x=starost, y=stevilo,color =umrli$cluster)) + geom_point()  + facet_grid(cols=vars(spol)) +
  ggtitle("Število smrti v letu 2017 po starosti") + xlab("Starost") + ylab("Stevilo") +
  geom_point() +
  scale_color_manual(name="Kritične starosti:", values=c("red","green","blue"), labels=c("najbolj", "srednje", "najmanj"))

########### voronojev diagram kot opcija
library(deldir)
library(ggplot2)

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
