library(plotly)

##### avti

### Hotela sva analizirati kateri vrsti avtomobilov so najbolj prijazni za različne tipe voznikov. 
## Primerjala sva različne znamke avtomobilov glede na razdaljo, ki jo lahko prevozijo z eno galono, številom konjskih moči in pa čas v katerem lahko prevozilo 1/4 milje.
## Sam sem najbolj zainteresiran za znamke modrih pik spodaj( relativno veliko prevozijo z eno galono, glede na njihov nizek čas na 1/4 milje in sorazmerno veliko količino konjev).
data("mtcars")
tabela <- na.omit(mtcars)

set.seed(20)
dist(tabela, method = "euclidean") # vrne matriko razdalj po MNK
avti<- kmeans(tabela[,c(1,4,7)], 4, nstart = 20)
avti

avti1<- avti$cluster
tabela$avti1 <- as.factor(avti1)

data("mtcars")
tabela <- na.omit(mtcars)
a <- plot_ly(tabela, x = ~mpg, y = ~hp, z = ~qsec,color =avti1) %>%
  add_markers() %>% 
  layout(scene = list(xaxis = list(title = 'prevožene milje z 1 galono'), 
                      yaxis = list(title = 'stevilo konjskih moči'), 
                      zaxis = list(title = 'cas na 1/4 milje')))

a



set.seed(20)
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






