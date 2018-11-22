library(plotly)

p <- plot_ly(USArrests, x = ~Murder, y = ~Assault, z = ~Rape,
             marker = list(color = ~UrbanPop, colorscale = c('#FFE1A1', '#683531'), showscale = TRUE)) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'stevilo umorov na 100000 ljudi'),
                      yaxis = list(title = 'stevilo napadov na 100000 ljudi'),
                      zaxis = list(title = 'stevilo posilstev na 100000 ljudi')),
         annotations = list(
           x = 1.13,
           y = 1.05,
           text = 'Delež urbane populacije',
           xref = 'paper',
           yref = 'paper',
           showarrow = FALSE
         ))

chart_link <- api_create(p, filename="scatter3d-colorscale")
chart_link
# Create a shareable link to your chart
# Set up API credentials: https://plot.ly/r/getting-started


# tukaj bova predstavla korelacijo med številom umorov, napadov in posilstev v ameriki po zveznih državah
# število gledano na 100000 ljudi
