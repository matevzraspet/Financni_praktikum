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

#set.seed(20)
#dist(konec, method = "euclidean") # vrne matriko razdalj po MNK
#krvne_skupine <- kmeans(konec[,c(3)], 8, nstart = 20) # uporabi k-means algoritem na 3.stolpcu tabele konec
#krvne_skupine


##############
# OUTLIERS
set.seed(20)
#dist(konec, method = "euclidean") # vrne matriko razdalj po MNK
kmeans.rezultat <- kmeans(konec[,c(3)], centers = 8)
grupe <- kmeans.rezultat$cluster
konec$grupe <- as.factor(grupe)
centri <- kmeans.rezultat$centers

#for (i in konec$grupe){
#  konec$centri[i] 
#}
# PROBLEM: pri vsaki državi določimo cluster oz js sm jo poimenovala grupa. in zdej je potrebno samo center i-te grupe 
# dodat kot nov stolpec centri z vrednostjo centra i-te grupe
# ker js znam samo tko da mi po 8 različnih centrov zapisuje en pod drizga, in pol 9-16.vrstica ma isti vrstni red centrov kot vrstice 1-8 
# ne znam tko da mi bo program prepoznal u lej v 7.grupi smo torej center znaša 0.300706667 (= center za grupo 7)


konec <- inner_join(grupe, centri) # NE DELA PRAvILNO :()

konec$centri <- as.factor(centri) # EDINI PROBLEM JE KER CENTRE NE DODA K PRAVI GRUPI AMPAK SAMO VSAKIH 8 KOPIRA NAPREJ
razdalje <- sqrt((konec[c(3)] - centri)^2)
konec$razdalje <- as.factor(razdalje)
odstopanja <- order(razdalje, decreasing=T)[1:29] # 296 podatkov, torej 10% od 296 = 29 ?!
konec.brez.odstopanja <- konec[-c(odstopanja),] # odstranim najbolj odstopajoče vrednosti

graf_MNK <- ggplot(konec.brez.odstopanja, aes(x = delez, y = variable, color = grupe)) +
  ggtitle("Delež krvnih skupin v izbranih evropskih državah") + xlab("Delež")  +
  geom_point()
graf_MNK

###############


#konec$cluster <- as.factor(krvne_skupine$cluster) # v tabeli konec doda stolpec cluster s številko podgrupe (1,2...8)

# graf
#graf_MNK <- ggplot(konec, aes(x = delez, y = variable, color = cluster)) +
  #ggtitle("Delež krvnih skupin v izbranih evropskih državah") + xlab("Delež")  +
  #geom_point()
#graf_MNK


# GRUPIRANJE Z METODO MANHATTAN

#set.seed(20)
#dist(konec,method = "manhattan") # vrne matriko razdalj po Manhattan metodi
#krvne_skupine1 <- kmeans(konec[,c(3)], 8, nstart = 20)
#krvne_skupine1

##############
# OUTLIERS
#set.seed(20)
#dist(konec, method = "euclidean") # vrne matriko razdalj po MNK
#kmeans.rezultat1 <- kmeans(konec[,c(3)], centers = 8)
#grupe1 <- kmeans.rezultat1$cluster
#konec$grupe1 <- as.factor(grupe1)
#centri1 <- kmeans.rezultat1$centers

#for (i in konec$grupe){
#  konec$centri[i] 
#}
# PROBLEM: pri vsaki državi določimo cluster oz js sm jo poimenovala grupa. in zdej je potrebno samo center i-te grupe 
# dodat kot nov stolpec centri z vrednostjo centra i-te grupe
# ker js znam samo tko da mi po 8 različnih centrov zapisuje en pod drizga, in pol 9-16.vrstica ma isti vrstni red centrov kot vrstice 1-8 
# ne znam tko da mi bo program prepoznal u lej v 7.grupi smo torej center znaša 0.300706667 (= center za grupo 7)

#konec$centri1 <- as.factor(centri1) # EDINI PROBLEM JE KER CENTRE NE DODA K PRAVI GRUPI AMPAK SAMO VSAKIH 8 KOPIRA NAPREJ

razdalje1 <- abs(konec[c(3)] - centri)
konec$razdalje1 <- razdalje1
odstopanja1 <- order(razdalje1, decreasing=T)[1:29] # 296 podatkov, torej 10% od 296 = 29 ?!
konec.brez.odstopanja1 <- konec[-c(odstopanja1),] # odstranim najbolj odstopajoče vrednosti

graf_manhattan <- ggplot(konec.brez.odstopanja1, aes(x = delez, y = variable, color = grupe)) +
  ggtitle("Delež krvnih skupin v izbranih evropskih državah") + xlab("Delež")  +
  geom_point()
graf_manhattan

###############


#krvne_skupine1$cluster <- as.factor(krvne_skupine1$cluster)

# graf # mogoče y = 0 vse na eni premici
#graf_Manhattan <- ggplot(konec, aes(x = delez, y = variable, color = krvne_skupine1$cluster)) +
 # ggtitle("Delež krvnih skupin v izbranih evropskih državah") + xlab("Delež")  +
#  geom_point()
#graf_Manhattan

### OPOMBA : graf nama je na premici izrisal delež osmih razliènih krvnih skupin v vseh državah sveta.
### Na podlagi deležev pojavitev posameznih krvnih skupin, smo pogrupirali podatke v osem razliènih skupin.
### Vidimo, da se najveè pojavljata krvni skupini 0+ in A+, najmanj pa B- in AB- .
