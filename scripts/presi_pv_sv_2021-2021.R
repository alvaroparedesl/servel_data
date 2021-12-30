library(data.table)
library(readxl)
library(igraph)
library(rayshader)
library(quadmesh)
# library(sysfonts)
library(showtext)
# library(rayshader)
# library(ggplot2)

# font_add_google(name='Archivo') #, family='Roboto')
showtext_auto()
par(family='Archivo')  # default = ''


source('scripts/funciones/functions.R', encoding="UTF-8")
source('scripts/funciones/calculos.R', encoding="UTF-8")
source('scripts/funciones/process.R', encoding="UTF-8")

data_folder = 'data'
root = 'website/images/plots'

# Diccionario de datos, para las funciones auxiliares
DICTIO <- read_excel_allsheets('scripts/dict.xlsx')
reg_orden <- c(15, 1:5, 13, 6:7, 16, 8:9, 14, 10:12)
comunas <- fread('scripts/comunas.csv', encoding='UTF-8')
comunas[, Comuna:=tolower(nom_com)]
# TODO parcial: calcular pendiente de cambio y cambio absoluto con puntaje Z.
# TODO: mapa 3D shiny relieve the 1 + 2 (perc izquierda + porc votación como altura)
# TODO: serie tiempo con Chile, regiones/comunas eje X, eje Y la tendencia, y el área número votantes [área chart] = https://r-statistics.co/Top50-Ggplot2-Visualizations-MasterList-R-Code.html

# Archivos
e2021_pv = prep_table(data_folder, '2021_presidencial_1v', dict = DICTIO)
e2021_sv = prep_table(data_folder, '2021_presidencial_2v', dict = DICTIO)

elecciones_lista <- nlist(e2021_pv, e2021_sv)

el <- procesar_electoral(elecciones_lista, DICTIO, comunas, reg_orden,
                         nombres=c('e2021_pv', 'e2021_sv'), 
                         nombre_nube='Presidenciales 2021: Primer vuelta vs Segunda vuelta')







election <- '2021_presidencial_1v'
dt <- readRDS('scripts/data.rds')[[election]]

library(waffle)
d_ <- DICTIO$tendencia[DICTIO$tendencia$Elección == election, ]
d_$Valor <- tools::toTitleCase(tolower(d_$Valor))

##---- NACIONAL-----
nacv <- dt$resumen_nacional$votos
nacv <- rbind(nacv, list('Nulos/Blancos', nacv[opcion %in% c('Nulos', 'Blancos'), sum(N)]))[!opcion %in% c('Nulos', 'Blancos')]
nacv[, per:=round(N/sum(N)*100, 2)]

nacv <- merge(nacv, d_, all.x=T, by.x='opcion', by.y='Valor')
nacv[is.na(Nombre), Nombre:='Nulos/Blancos']

p_ <- setNames(nacv$per, nacv$Nombre)

waffle(round(p_), rows = 10, colors = c(nacv$color[-nrow(nacv)], "#A9A9A9")) #,
       # title = 'Resultado'), legend_pos="bottom")
nacv[, c('Nombre', 'N', 'per')]


nacp <- dt$resumen_nacional$habilitados
nacp[, no_votos:=habilitados-votos]
nacp <- rbind(nacp, as.list(round(nacp/nacp$habilitados*100, 2)) )

p_ <- setNames(nacp[2, c('votos', 'no_votos')], c("Sufragaron", "No sufragaron"))

waffle(round(p_), rows = 10, colors = c("#006D2C", "#a1d99b"))

nacp[, c('votos', 'habilitados')]


##---- Regional
library(plotly)
library(sf)

regv <- dt$resumen_regional$votos
regv <- merge(regv, d_, all.x=T,  by.x='opcion', by.y='Valor', sort=F)
regv[is.na(Nombre), Nombre:=opcion]
p_ <- reshape2::dcast(as.data.frame(regv), Nombre~Reg_cod, value.var="N")
rownames(p_) <- p_[, 1]
p_ <- p_[, -1]
nulos_blancos <- colSums(p_[c('Blancos', 'Nulos'), ])
# p_ <- as.matrix(p_[-which(rownames(p_) %in% c('Blancos', 'Nulos')), ])
p_ <- p_[-which(rownames(p_) %in% c('Blancos', 'Nulos')), ]
p_ <- p_[na.omit(d_$Nombre), ]
p_ <- as.data.frame(t(rbind(p_, `Nulos/Blancos`=nulos_blancos)))
p_['Región'] <- rownames(p_)

# barplot(p_[, rev(colnames(p_))], horiz=T, col=c(na.omit(d_$color), "#A9A9A9"), las=1, beside=F) #, log="x")
colores <- setNames(c(na.omit(d_$color), "#A9A9A9"), colnames(p_)[-ncol(p_)])
orden <- list(categoryorder = "array", categoryarray = rev(p_$Región))
font_ <- list(family='Archivo')

for (i in rev(1:(ncol(p_) - 1))) {
  col_ <- sprintf("marker = list(color = '%s')", colores[i])
  # if (i==(ncol(p_) - 1)) {
  #   eval(parse(text=sprintf("fig <- plot_ly(p_, x = ~`Región`, y =~`%s`, type='bar', name='%s', orientation = 'v', %s)", colnames(p_)[i], colnames(p_)[i], col_)) )
  # } else {
  #   eval(parse(text=sprintf("fig <- fig |> add_trace(y = ~`%s`, name = '%s', %s)", colnames(p_)[i], colnames(p_)[i], col_)) )  
  # }
  if (i==(ncol(p_) - 1)) {
    eval(parse(text=sprintf("fig <- plot_ly(p_, x = ~`%s`, y =~`Región`, type='bar', name='%s', orientation = 'v', %s)", colnames(p_)[i], colnames(p_)[i], col_)) )
  } else {
    eval(parse(text=sprintf("fig <- fig |> add_trace(x = ~`%s`, name = '%s', %s)", colnames(p_)[i], colnames(p_)[i], col_)) )  
  }
}
fig <- fig |> layout(barmode = 'stack', yaxis = orden, xaxis=list(title='Número de votos'),
                     legend = list(x = .7, y = 1), font=font_)
fig

p_

regp <- dt$resumen_regional$habilitados
regp[, per:=sprintf("%0.2f%%", votos/habilitados*100)]
regp


##---- Comunal
comunas <- fread('scripts/comunas.csv', encoding='UTF-8')
comunas[, CUT_COM:=sprintf('%05d', CUT)]
comunas[, nom_com2:=tools::toTitleCase(tolower(nom_com))]
comunas[, which(colnames(comunas) %in% c('Reg_cod')):=NULL]

comv <- dt$resumen_comunal$votos
comv <- merge(comv, comunas, by.x='Comuna', by.y='nom_com2')
setorder(comv, Reg_cod)
comv <- merge(comv, d_, all.x=T,  by.x='opcion', by.y='Valor', sort=F)
comv[is.na(Nombre.y), Nombre.y:=opcion]
comv[, por:=round(N/sum(N)*100, 2), by=CUT_COM]
comv[, lab:=sprintf('%s votos [%s%%]', trimws(format(N, big.mark=".", decimal.mark=",")), trimws(format(por, big.mark=".", decimal.mark=",")) )]

com_per <- comv[!is.na(tendencia), list(per=round(sum(N[tendencia==-1]) / sum(N) * 100, 1) ), by=c("CUT_COM", "nom_com", "Región", "Provincia")]

com <- sf::st_read('scripts/COMUNAS_2020.geojson', quiet=T)
com_ <- merge(com[, 'CUT_COM'], com_per, all.x=T)

comv_ <- as.data.frame(dcast(comv, CUT_COM ~ Nombre.y, value.var="lab"))
rownames(comv_) <- comv_[, 1]
comv_ <- comv_[, c(nacv$Nombre[-nrow(nacv)], "Blancos", "Nulos")]
sprin <- paste('<b>', c(colnames(comv_)), '</b>: %s', sep="", collapse='<br>')
hoverlabels <- apply(comv_, 1, function(x) do.call(sprintf, c(list(sprin), as.list(x))))

com_per[, header:=sprintf('<b>Región de </b>%s<br><b>Provincia de </b>%s<br><b>Comuna de </b>%s<br><br><b>Votación de Izquierda:</b> %s%%<br><br>', `Región`, `Provincia`, `nom_com`, per)]
header_ <- setNames(com_per$header, com_per$CUT_COM)

hoveri <- setNames(paste(header_[com_per$CUT_COM], hoverlabels[com_per$CUT_COM]), com_per$CUT_COM)


Noax <- list(
  title = "",
  zeroline = FALSE,
  showline = FALSE,
  showticklabels = FALSE,
  showgrid = FALSE
)
ltemplate <- "<b>Comuna de %s</b><br><br><b>Porcentaje Izquierda</b>: %s%%<br>"
paleta1 <- rev(brewer.pal(10, 'RdBu'))
# plot(com_['per'])
# g <- ggplot(data=com_[1:30, ]) +
g <- ggplot(data=com_) +
  geom_sf(aes(fill=per, 
              text=CUT_COM
              ),
          ) +
  theme_void() +
  coord_sf() +
  scale_fill_gradientn(colours = paleta1, limits = c(0, 100)) +
  theme(
    text = element_text(color = "#22211d"),
    # plot.background = element_rect(fill = "#f5f5f2", color = NA),
    # panel.background = element_rect(fill = "#f5f5f2", color = NA),
    # legend.background = element_rect(fill = "#f5f5f2", color = NA),
  ) + labs(fill='Votación Izquierda (%)') 
g <- ggplotly(g, tooltip='text') |>  style(hoveron='fill', hoverlabel = list(bgcolor = "white", font=font_),
                                           xaxis = Noax, yaxis = Noax)
for (i in 1:length(g$x$data)) {
  if (!is.null(g$x$data[[i]]$line)) g$x$data[[i]]$line$width <- 0.3
  if (!is.null(g$x$data[[i]]$text)) {
    if (length(g$x$data[[i]]$text > 1)) {
      g$x$data[[i]]$text <- g$x$data[[i]]$text[1]
    }
    loc <- g$x$data[[i]]$text
    g$x$data[[i]]$text <- hoveri[loc]
  }
}
g




# plotly_json(g)
fig <- plotly_build(g)
fig




if (F) {
  com <- rjson::fromJSON(file='scripts/COMUNAS_2020.json')
  for (i in 1:length(com$features)){
    com$features[[i]]$id <- com$features[[i]]$properties$CUT_COM
  }
  
  
  fig <- plot_ly()
  fig <- fig %>% add_trace(
    type="choropleth",
    geojson=com,
    locations=com_per$CUT_COM,
    z=com_per$per,
    colorscale="Viridis",
    zmin=0,
    zmax=12,
    marker=list(line=list(
      width=0)
    )
  )
  fig
  
  
  hovertemplate = 'Comuna: %{COMUNA}<extra></extra>'
  hovertemplate = 'Comuna: ~COMUNA<extra></extra>'
  paleta1 <- rev(brewer.pal(10, 'RdBu'))
  fig <- plot_ly(data=com_[1:30, ], type = "scatter", mode='lines', name=~COMUNA,
                 split=~CUT_COM, color=~per, colors=paleta1, showlegend=F, alpha_stroke = 0.8, alpha=.9)
  # zauto = FALSE, zmin = 0, zmax = 1,
  # marker=list(cmin=0, cmax=1, line=list(dash='solid')),
  # hoverinfo='text',
  # hovertemplate=hovertemplate)
  # fig <- fig |> layout(coloraxis=list(cauto=F, cmax=1, cmin=0))
  fig
  # schema()
  plotly_json(fig)
}