library(plotly)

##### avti

### Hotela sva analizirati kateri vrsti avtomobilov so najbolj prijazni za različne tipe voznikov. 
## Primerjala sva različne znamke avtomobilov glede na razdaljo, ki jo lahko prevozijo z eno galono, številom konjskih moči in pa čas v katerem lahko prevozilo 1/4 milje.
## Sam sem najbolj zainteresiran za znamke modrih pik spodaj( relativno veliko prevozijo z eno galono, glede na njihov nizek čas na 1/4 milje in sorazmerno veliko količino konjev).

# tukaj sva izvedla še kmeans algoritem na trorazsežnih podatkih

data("mtcars")
tabela <- na.omit(mtcars)

###tukaj izvajamo kmeans algoritem po evklidski metodi
set.seed(20)  
dist(tabela, method = "euclidean") # vrne matriko razdalj po MNK
avti<- kmeans(tabela[,c(1,4,7)], 4, nstart = 20) #kmeans algoritem bova naredila glede na parametre mpg,hp,qsec; tukaj pripravimo kmeans grupiranje v 4 množice
avti

avti1<- avti$cluster   ## vsebuje podatke kateri množici od 1-4 pripada določen tip avta
tabela$avti1 <- as.factor(avti1) ##samo po vrsti številsko napiše množice od 1-4 katerim po vrsti pripadajo podatki

data("mtcars")
tabela <- na.omit(mtcars)  #izbriši vrstice ki vsebujejo NA podatke
a <- plot_ly(tabela, x = ~mpg, y = ~hp, z = ~qsec,color =avti1, text = ~rownames(tabela)) %>%   ## narišemo trorazsežni graf
  add_markers() %>% 
  layout(scene = list(xaxis = list(title = 'prevožene milje z 1 galono'), 
                      yaxis = list(title = 'stevilo konjskih moči'), 
                      zaxis = list(title = 'cas na 1/4 milje')))

a



set.seed(20)  ### izvedemo še kmeans algoritem po metodi manhattan
dist(tabela, method = "manhattan") # vrne matriko razdalj po MNK
avti3<- kmeans(tabela[,c(1,4,7)], 4, nstart = 20)
avti3

avti4<- avti3$cluster
tabela$avti4 <- as.factor(avti4)

data("mtcars")
tabela <- na.omit(mtcars)
b <- plot_ly(tabela, x = ~mpg, y = ~hp, z = ~qsec,color =avti1) %>%
  add_markers() %>% 
  layout(scene = list(xaxis = list(title = 'prevožene milje z 1 galono'), 
                      yaxis = list(title = 'stevilo konjskih moči'), 
                      zaxis = list(title = 'cas na 1/4 milje')))


b







p <- plot_ly(USArrests, x = ~Murder, y = ~Assault, z = ~Rape,
             marker = list(color = ~UrbanPop, colorscale = c('#FFE1A1', '#683531'), showscale = TRUE)) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'stevilo umorov'), #stevilo umorov  na 100000 ljudi
                      yaxis = list(title = 'stevilo napadov'), #stevilo napadov na 100000 ljudi
                      zaxis = list(title = 'stevilo posilstev')),#stevilo posilstev  na 100000 ljudi
         annotations = list(
           x = 1.13,
           y = 1.05,
           text = 'Delež urbane populacije (v %)',
           xref = 'paper',
           yref = 'paper',
           showarrow = FALSE
         ))
p
chart_link <- api_create(p, filename="scatter3d-colorscale")
chart_link
# Create a shareable link to your chart
# Set up API credentials: https://plot.ly/r/getting-started






