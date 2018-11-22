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
set.seed(20)
#dist(konec, method = "euclidean") # vrne matriko razdalj po MNK
centri <- kmeans.rezultat$centers
konec$centri <- centri[konec$grupe]
razdalje <- sqrt((konec[3] - konec[5])^2)
konec$razdalje <- as.factor(razdalje)
odstopanja <- order(razdalje, decreasing=T)[1:29] # 296 podatkov, torej 10% od 296 = 29 ?!
konec.brez.odstopanja <- konec[-c(odstopanja),] # odstranim 10% najbolj odstopajoče vrednosti

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
set.seed(20)
#dist(konec, method = "euclidean") # vrne matriko razdalj po MNK
centri1 <- kmeans.rezultat$centers
konec$centri1 <- centri1[konec$grupe1]
razdalje1 <- abs(konec[3] - konec[8])
konec$razdalje1 <- razdalje1 # ?????????????? razdalje1.delez
odstopanja1 <- order(razdalje1, decreasing=T)[1:29] # 296 podatkov, torej 10% od 296 = 29 ?!
konec.brez.odstopanja1 <- konec[-c(odstopanja1),] # odstranim 10% najbolj odstopajoče vrednosti

graf_Manhattan_outliers <- ggplot(konec.brez.odstopanja1, aes(x = delez, y = variable, color = grupe1)) +
  ggtitle("Delež krvnih skupin v izbranih evropskih državah") + xlab("Delež")  +
  geom_point()
graf_Manhattan_outliers


### OPOMBA : graf nama je na premici izrisal delež osmih razliènih krvnih skupin v vseh državah sveta.
### Na podlagi deležev pojavitev posameznih krvnih skupin, smo pogrupirali podatke v osem razliènih skupin.
### Vidimo, da se najveè pojavljata krvni skupini 0+ in A+, najmanj pa B- in AB- .
