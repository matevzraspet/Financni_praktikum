# PODATKIV PROSTORU R^2

library(ggplot2)
library(rvest)
library(gsubfn)
library(readr)
library(dplyr)
library(stringr)
library(reshape2)

# uvoz podatkov
uvozi.igralce <- function(stran) {
  link <- sprintf("https://www.transfermarkt.com/spieler-statistik/wertvollstespieler/marktwertetop?land_id=0&ausrichtung=alle&spielerposition_id=alle&altersklasse=alle&jahrgang=0&plus=1&page=%d", stran)
  tabela <- html_session(link) %>% read_html() %>% html_nodes(xpath="//table") %>% .[[2]]
  podrobno <- tabela %>% html_table(dec = ",", fill = TRUE) %>%
    .[, -c(1, 2, 3, 7, 8)] %>% na.omit()
  
  colnames(podrobno) <- c("igralec", "pozicija", "starost", "vrednost", "odigrane.tekme",
                          "zadeti.goli", "avtogoli", "asistence", "rumeni.kartoni", "drugi.rumeni.karton",
                          "rdeci.karton", "prisel.kot.menjava", "odsel.na.klop")
  
  podrobno$drzava <- tabela %>% html_nodes(xpath="./tbody/tr/td[4]/img[1]") %>% html_attr("alt")
  
  podrobno$klub <- tabela %>% html_nodes(xpath="./tbody/tr/td[5]/a/img[1]") %>% html_attr("alt")
  
  return(podrobno)
  
}

igralci <- lapply(1:4, uvozi.igralce) %>% bind_rows()
igralci <- igralci[, -c(3,4,5,7, 9,10,11,12,13,14,15)]

# graf za dejanske pozicije glede na statistiko
graf1 <- ggplot(igralci, aes(asistence, zadeti.goli, color = pozicija)) +
  ggtitle("Igralne pozicije") + xlab("Asistence") + ylab("Zadeti goli") +
  geom_point() 
graf1

# GRUPIRANJE PO METODI NAJMANJŠIH KVADRATOV
set.seed(20)
dist(igralci, method = "euclidean") # vrne matriko razdalj po MNK
pozicije <- kmeans(igralci[,c(3,4)], 3)
pozicije

grupe <- pozicije$cluster
igralci$grupe <- as.factor(grupe)

### OPOMBA : 
# kot napadalne pozicije interpretiramo: Centre forward, Second striker;
# kot sredina : attacking midfield, central midfield, left winger, right winger;
# kot obramba : centre back, defensive midfield, goalkeeper, right back, left back

# graf
graf_MNK <- ggplot(igralci, aes(asistence, zadeti.goli, color = grupe)) +
  ggtitle("Izračun igralnih pozicij s pomočjo grupiranja") + xlab("Asistence") + ylab("Zadeti goli") +
  geom_point() +
  scale_color_manual(name="Tip igralne pozicije", values=c("red","green","blue"), labels=c("obramba", "sredina", "napad"))
graf_MNK

# OUTLIERS
dist(igralci, method = "euclidean") # vrne matriko razdalj po MNK
centri_zadeti.goli <- pozicije$centers[c(1,2,3), 1]
igralci$centri_zadeti.goli <- centri_zadeti.goli[igralci$grupe]
centri_asistence <- pozicije$centers[c(1,2,3), 2]
igralci$centri_asistence <- centri_asistence[igralci$grupe]
razdalje <- sqrt((igralci[3] - igralci[6])^2 + (igralci[4] - igralci[7])^2)
igralci$razdalje <- as.factor(razdalje)
odstopanja <- order(razdalje, decreasing=T)[1:10] # 100 podatkov, torej 10% od 296 = 29 ?!
igralci.brez.odstopanja <- igralci[-c(odstopanja),] # odstranim 10% najbolj odstopajoče vrednosti

graf_MNK_outliers <- ggplot(igralci.brez.odstopanja, aes(x = asistence, y = zadeti.goli, color = grupe)) +
  ggtitle("Izračun igralnih pozicij s pomočjo grupiranja") + xlab("Asistence") + ylab("Zadeti goli") +
  geom_point() +
  scale_color_manual(name="Tip igralne pozicije", values=c("red","green","blue"), labels=c("obramba", "sredina", "napad"))
graf_MNK_outliers


# GRUPIRANJE PO METODI MANHATTAN
dist(igralci, method = "manhattan") # vrne matriko razdalj po Manhattan metodi
pozicije <- kmeans(igralci[,c(3,4)], 3)
pozicije

grupe1 <- pozicije$cluster
igralci$grupe1 <- as.factor(grupe1)

# graf
graf_Manhattan <- ggplot(igralci, aes(asistence, zadeti.goli, color = grupe)) +
  ggtitle("Izračun igralnih pozicij s pomočjo grupiranja") + xlab("Asistence") + ylab("Zadeti goli") +
  geom_point() +
  scale_color_manual(name="Tip igralne pozicije", values=c("red","green","blue"), labels=c("obramba", "sredina", "napad"))
graf_Manhattan

# OUTLIERS
razdalje1 <- abs(igralci[3] - igralci[6]) + abs(igralci[4] - igralci[7])
igralci$razdalje1 <- razdalje1
odstopanja1 <- order(razdalje1, decreasing=T)[1:10] # 100 podatkov, torej 10% od 296 = 29 ?!
igralci.brez.odstopanja1 <- igralci[-c(odstopanja1),] # odstranim 10% najbolj odstopajoče vrednosti

graf_Manhattan_outliers <- ggplot(igralci.brez.odstopanja1, aes(x = asistence, y = zadeti.goli, color = grupe)) +
  ggtitle("Izračun igralnih pozicij s pomočjo grupiranja") + xlab("Asistence") + ylab("Zadeti goli") +
  geom_point() +
  scale_color_manual(name="Tip igralne pozicije", values=c("red","green","blue"), labels=c("obramba", "sredina", "napad"))
graf_Manhattan_outliers

##### VORONOJ PRAVI

aaa <-data.matrix(igralci[,c(3,4)])
pozicije <- kmeans(aaa,3)

library(tripack)
library(RColorBrewer)

CL5 <- brewer.pal(3, "Pastel1")
V <- voronoi.mosaic(pozicije$centers[,1],pozicije$centers[,2])
P <- voronoi.polygons(V)
plot(aaa, pch =19, xlab = "Asistence",ylab = "Zadeti goli",col = CL5[pozicije$cluster])
points(pozicije$centers[,1], pozicije$centers[,2],pch = 3, cex = 1.5,lwd = 2)
plot(V,add = TRUE)




## VORONOJEV DIAGRAM

library(deldir)
library(ggplot2)

voronoi <- deldir(igralci$asistence, igralci$zadeti.goli)


ggplot(data=igralci, aes(x=asistence,y=zadeti.goli)) +
  
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
