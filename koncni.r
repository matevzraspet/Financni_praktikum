library(rvest)
library(gsubfn)
library(readr)
library(dplyr)
library(stringr)
library(reshape2)

##podatki za dimenzijo R¡1

link <- "https://en.wikipedia.org/wiki/Blood_type_distribution_by_country"
stran <- html_session(link) %>% read_html()
tabela <- stran %>% html_nodes(xpath="//table[@class='wikitable sortable']") %>% .[[1]] %>%
  html_table(dec = ",", fill = TRUE)
tabela <- tabela[,-c(2)]
tabela <-na.omit(tabela)
tabela <- tabela[c(4,7,9,11,19,21,22,23,28,31,32,33,34,38,39,44,46,51,54,55,56,58,67,71,76,77,79,80,82,84,85,88,90,91,95,97,99),]
colnames(tabela) <- c("drzave","0+","A+","B+","AB+","0-","A-","B-","AB-")
konec <- melt(tabela,id.vars ="drzave", variable.names = c("0+","A+","B+","AB+","0-","A-","B-","AB-"),
              value.name ="delez")
konec$delez <- parse_number(konec$delez)/100
konec$drzave <- konec$drzave %>% strapplyc("^([^[/[]*)") %>% unlist()


############### na podlagi evklidske metode
set.seed(20)
dist(konec, method = "euclidean")
krvne_skupine <- kmeans(konec[,c(3)], 8, nstart = 20)
krvne_skupine

konec$cluster <- as.factor(krvne_skupine$cluster)
aaa <- ggplot(konec, aes(x = delez, y = variable, color = cluster)) +
  ggtitle("Delež krvnih skupin v državah sveta") + xlab("Delež")  +
  geom_point() 
  ###legend(name="Tip igralne pozicije",rename(c("1","2","3","4","5","6","7","8"),c("AB+", "A+", "0-","AB-","A-","B+","B-","0+")))


############### na podlagi metode najmanjših kvadratov
set.seed(20)
dist(konec,method = "manhattan")
krvne_skupine <- kmeans(konec[,c(3)], 8, nstart = 20)
krvne_skupine

krvne_skupine$cluster <- as.factor(krvne_skupine$cluster)
aaa <- ggplot(konec, aes(x = delez, y = 0, color = krvne_skupine$cluster)) +
  ggtitle("Delež krvnih skupin v državah sveta") + xlab("Delež")  +
  geom_point()
### +rename(c("1","2","3","4","5","6","7","8"),c("AB-", "B-", "A-","0-","AB+","B+","A+","0+")) treba zamenjati imena v legendi

### Opomba : graf nama je na premici izrisal delež osmih razliènih krvnih skupin v vseh državah sveta.
### Na podlagi deležev pojavitev posameznih krvnih skupin, smo pogrupirali podatke v osem razliènih skupin.
### Vidimo, da se najveè pojavljata krvni skupini 0+ in A+, najmanj pa B- in AB- .





