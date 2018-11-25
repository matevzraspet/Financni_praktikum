# PODATKI V PROSTOR R

# knjižnice
library(rvest)
library(gsubfn)
library(readr)
library(dplyr)
library(stringr)
library(reshape2)
library(ggplot2)

# uvoz podatkov -> Wikipedija, 1.tabela 
link <- "https://en.wikipedia.org/wiki/Blood_type_distribution_by_country"
stran <- html_session(link) %>% read_html()
tabela <- stran %>% html_nodes(xpath="//table[@class='wikitable sortable']") %>% .[[1]] %>%
  html_table(dec = ",", fill = TRUE)

tabela <- tabela[,-c(2)] # brez 2.stolpca
tabela <-na.omit(tabela) # pobriše vrstice z NA

tabela <- tabela[c(4,7,9,11,19,21,22,23,28,31,32,33,34,38,39,44,46,51,54,55,56,58,
                   67,71,76,77,79,80,82,84,85,88,90,91,95,97,99),] # samo vrstice z evropskimi državami

colnames(tabela) <- c("drzave","0+","A+","B+","AB+","0-","A-","B-","AB-") # preimenovanje stolpcev

konec <- melt(tabela,id.vars ="drzave", variable.names = c("0+","A+","B+","AB+","0-","A-","B-","AB-"),
              value.name ="delez") # pretvorba v data frame obliko

konec$delez <- parse_number(konec$delez)/100 # iz % v delež

konec$drzave <- konec$drzave %>% strapplyc("^([^[/[]*)") %>% unlist() # v celicah ni znakov ^([^[/[]*)


# GRUPIRANJE PO METODI NAJMANJŠIH KVADRATOV
set.seed(20)
dist(konec, method = "euclidean") # vrne matriko razdalj po MNK
kmeans.rezultat <- kmeans(konec[,c(3)], centers = 8)
grupe <- kmeans.rezultat$cluster
konec$grupe <- as.factor(grupe)

# graf
graf_MNK <- ggplot(konec, aes(x = delez, y = variable, color = grupe)) +
  ggtitle("Delež krvnih skupin v izbranih evropskih državah") + xlab("Delež")  +
  geom_point()
graf_MNK

# OUTLIERS
dist(konec, method = "euclidean") # vrne matriko razdalj po MNK
centri <- kmeans.rezultat$centers
konec$centri <- centri[konec$grupe]
razdalje <- sqrt((konec[3] - konec[5])^2)
konec$razdalje <- as.vector(razdalje)

konec1 <- konec
konec2 <- konec
konec3 <- konec
konec4 <- konec
konec5 <- konec
konec6 <- konec
konec7 <- konec
konec8 <- konec

# v vsaki naslednji vrstici so poračunane vsote kvadratov razdalj znotraj posamezne grupe
konecG1 <- konec %>% filter(grupe == "1")
sum(konecG1$razdalje) # 0.06794737
konecG2 <- konec %>% filter(grupe == "2")
sum(konecG2$razdalje) # 0.1398545
konecG3 <- konec %>% filter(grupe == "3")
sum(konecG3$razdalje) # 0.4308476
konecG4 <- konec %>% filter(grupe == "4")
sum(konecG4$razdalje) # 0.6257831
konecG5 <- konec %>% filter(grupe == "5")
sum(konecG5$razdalje) # 0.01566667
konecG6 <- konec %>% filter(grupe == "6")
sum(konecG6$razdalje) #  0.1538057
konecG7 <- konec %>% filter(grupe == "7")
sum(konecG7$razdalje) # 0.06794737
konecG7 <- konec %>% filter(grupe == "1")
sum(konecG1$razdalje) # 0.5432
konecG8 <- konec %>% filter(grupe == "8")
sum(konecG8$razdalje) # 0.8658182

odstopanja <- order(razdalje, decreasing=T)[1:29] # 296 podatkov, torej 10% od 296 = 29
konec.brez.odstopanja <- konec[-c(odstopanja),] # odstranim 10% najbolj odstopajoče vrednosti

k1 <- konec.brez.odstopanja
k2 <- konec.brez.odstopanja
k3 <- konec.brez.odstopanja
k4 <- konec.brez.odstopanja
k5 <- konec.brez.odstopanja
k6 <- konec.brez.odstopanja
k7 <- konec.brez.odstopanja
k8 <- konec.brez.odstopanja

kG1 <- k1 %>% filter(grupe == "1")
sum(kG1$razdalje) # 0.06794737 ISTO!
kG2 <- k2 %>% filter(grupe == "2")
sum(kG2$razdalje) # 0.1398545 ISTO!
kG3 <- k3 %>% filter(grupe == "3")
sum(kG3$razdalje) # 0.2108952
kG4 <- k4 %>% filter(grupe == "4")
sum(kG4$razdalje) # 0.5642169
kG5 <- k5 %>% filter(grupe == "5")
sum(kG5$razdalje) # 0.01566667 ISTO!
kG6 <- k6 %>% filter(grupe == "6")
sum(kG6$razdalje) #  0.1538057 ISTO!
kG7 <- k7 %>% filter(grupe == "7")
sum(kG7$razdalje) # 0.2302
kG8 <- k8 %>% filter(grupe == "8")
sum(kG8$razdalje) # 0.34275

# graf
graf_MNK_outliers <- ggplot(konec.brez.odstopanja, aes(x = delez, y = variable, color = grupe)) +
  ggtitle("Delež krvnih skupin v izbranih evropskih državah") + xlab("Delež")  + 
  geom_point()
graf_MNK_outliers


# GRUPIRANJE Z METODO MANHATTAN
dist(konec,method = "manhattan") # vrne matriko razdalj po Manhattan metodi
grupe1 <- kmeans.rezultat$cluster
konec$grupe1 <- as.factor(grupe)

# graf 
graf_Manhattan <- ggplot(konec, aes(x = delez, y = variable, color = grupe1)) +
  ggtitle("Delež krvnih skupin v izbranih evropskih državah") + xlab("Delež")  +
  geom_point()
graf_Manhattan

# OUTLIERS
razdalje1 <- abs(konec[3] - konec[8])
konec$razdalje1 <- razdalje1 # ?????????????? razdalje1.delez
odstopanja1 <- order(razdalje1, decreasing=T)[1:29] # 296 podatkov, torej 10% od 296 = 29
konec.brez.odstopanja1 <- konec[-c(odstopanja1),] # odstranim 10% najbolj odstopajoče vrednosti

graf_Manhattan_outliers <- ggplot(konec.brez.odstopanja1, aes(x = delez, y = variable, color = grupe1)) +
  ggtitle("Delež krvnih skupin v izbranih evropskih državah") + xlab("Delež")  +
  geom_point()
graf_Manhattan_outliers

