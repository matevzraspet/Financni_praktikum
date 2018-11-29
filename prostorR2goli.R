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
igralci$razdalje <- as.vector(razdalje)

# v vseh naslednjih vrsticah so poračunane vsote kvadratov razdalj znotraj posamezne grupe
igralciG1 <- igralci %>% filter(grupe == "1")
sum(igralciG1$razdalje) # 94.84586
igralciG2 <- igralci %>% filter(grupe == "2")
sum(igralciG2$razdalje) # 69.68478
igralciG3 <- igralci %>% filter(grupe == "3")
sum(igralciG3$razdalje) # 50.90808

odstopanja <- order(razdalje, decreasing=T)[1:10] # 100 podatkov, torej 10% od 100 = 10
igralci.brez.odstopanja <- igralci[-c(odstopanja),] # odstranim 10% najbolj odstopajoče vrednosti

iG1 <- igralci.brez.odstopanja %>% filter(grupe == "1")
sum(iG1$razdalje) # 64.64388 MANJ
iG2 <- igralci.brez.odstopanja %>% filter(grupe == "2")
sum(iG2$razdalje) # 43.98983 MANJ - največja razlika
iG3 <- igralci.brez.odstopanja %>% filter(grupe == "3")
sum(iG3$razdalje) # 50.90808 MANJ

graf_MNK_outliers <- ggplot(igralci.brez.odstopanja, aes(x = asistence, y = zadeti.goli, color = grupe)) +
  ggtitle("Izračun igralnih pozicij s pomočjo grupiranja") + xlab("Asistence") + ylab("Zadeti goli") +
  geom_point() +
  scale_color_manual(name="Tip igralne pozicije", values=c("red","green","blue"), labels=c("obramba", "sredina", "napad"))
graf_MNK_outliers


# GRUPIRANJE PO METODI MANHATTAN
dist(igralci, method = "manhattan") # vrne matriko razdalj po Manhattan metodi
pozicije

# graf
graf_Manhattan <- ggplot(igralci, aes(asistence, zadeti.goli, color = grupe)) +
  ggtitle("Izračun igralnih pozicij s pomočjo grupiranja") + xlab("Asistence") + ylab("Zadeti goli") +
  geom_point() +
  scale_color_manual(name="Tip igralne pozicije", values=c("red","green","blue"), labels=c("obramba", "sredina", "napad"))
graf_Manhattan

# OUTLIERS
razdalje1 <- abs(igralci$zadeti.goli - igralci$centri_zadeti.goli) + abs(igralci$asistence - igralci$centri_asistence)
igralci$razdalje1 <- as.vector(razdalje1)

IgralciG1 <- igralci %>% filter(grupe == "1")
sum(IgralciG1$razdalje1) # 121
IgralciG2 <- igralci %>% filter(grupe == "2")
sum(IgralciG2$razdalje1) # 90.09091
IgralciG3 <- igralci %>% filter(grupe == "3")
sum(IgralciG3$razdalje1) # 65.14286

odstopanja1 <- order(razdalje1, decreasing=T)[1:10] # 100 podatkov, torej 10% od 296 = 29 ?!
igralci.brez.odstopanja1 <- igralci[-c(odstopanja1),] # odstranim 10% najbolj odstopajoče vrednosti

IgG1 <- igralci.brez.odstopanja1 %>% filter(grupe == "1")
sum(IgG1$razdalje1) # 83
IgG2 <- igralci.brez.odstopanja1 %>% filter(grupe == "2")
sum(IgG2$razdalje1) # 55.63636
IgG3 <- igralci.brez.odstopanja1 %>% filter(grupe == "3")
sum(IgG3$razdalje1) # 65.14286

# graf
graf_Manhattan_outliers <- ggplot(igralci.brez.odstopanja1, aes(x = asistence, y = zadeti.goli, color = grupe)) +
  ggtitle("Izračun igralnih pozicij s pomočjo grupiranja") + xlab("Asistence") + ylab("Zadeti goli") +
  geom_point() +
  scale_color_manual(name="Tip igralne pozicije", values=c("red","green","blue"), labels=c("obramba", "sredina", "napad"))
graf_Manhattan_outliers

# VORONOJEV DIAGRAM 1

podatki <-data.matrix(igralci[,c(3,4)])
pozicije <- kmeans(podatki,3)

library(tripack)
library(RColorBrewer)

CL5 <- brewer.pal(3, "Pastel1")
V <- voronoi.mosaic(pozicije$centers[,1],pozicije$centers[,2])
P <- voronoi.polygons(V)
plot(podatki, pch =19, xlab = "Asistence",ylab = "Zadeti goli",col = CL5[pozicije$cluster])
points(pozicije$centers[,1], pozicije$centers[,2],pch = 3, cex = 1.5,lwd = 2)
plot(V,add = TRUE)

# VORONOJEV DIAGRAM 2

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

