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









set.seed(20)

pozicije <- kmeans(igralci[,c(6,8)], 3, nstart = 20)

pozicije



pozicije$cluster <- as.factor(pozicije$cluster)

ggplot(igralci, aes(asistence, zadeti.goli, color = pozicije$cluster)) +
  
  ggtitle("Izračun igralnih pozicij s pomočjo grupiranja") + xlab("Asistence") + ylab("Zadeti goli") +
  
  geom_point() +
  
  scale_color_manual(name="Tip igralne pozicije", values=c("red","green","blue"), labels=c("obramba", "sredina", "napad"))



### Opomba : kot napadalne pozicije interpretiramo: Centre forward, Second striker;

# kot sredina : attacking midfield, central midfield, left winger, right winger;

# kot obramba : centre back, defensive midfield, goalkeeper, right back, left back



### Dejanske pozicije glede na statistiko



ggplot(igralci, aes(asistence, zadeti.goli, color = pozicija)) +
  
  ggtitle("Igralne pozicije") + xlab("Asistence") + ylab("Zadeti goli") +
  
  geom_point() 



library(deldir)

library(ggplot2)





voronoi <- deldir(igralci$asistence, igralci$zadeti.goli)



#Now we can make a plot

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
