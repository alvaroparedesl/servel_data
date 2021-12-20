library(data.table)
library(readxl)
library(igraph)
library(RColorBrewer)
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
comunas <- fread('scripts/comunas.csv')
comunas[, Comuna:=tolower(nom_com)]
# TODO parcial: calcular pendiente de cambio y cambio absoluto con puntaje Z.
# TODO: mapa 3D shiny relieve the 1 + 2 (perc izquierda + porc votación como altura)
# TODO: serie tiempo con Chile, regiones/comunas eje X, eje Y la tendencia, y el área número votantes [área chart] = https://r-statistics.co/Top50-Ggplot2-Visualizations-MasterList-R-Code.html

# Archivos
e2017_pp = prep_table(data_folder, '2017_primarias_presidenciales', dict = DICTIO, sheets=sprintf('%02d', 1:15))
e2021_pp = prep_table(data_folder, '2021_primarias_presidenciales', dict = DICTIO)

elecciones_lista <- nlist(e2017_pp, e2021_pp)

el <- procesar_electoral(elecciones_lista, DICTIO, comunas, reg_orden,
                         nombres=c('e2017_pp', 'e2021_pp'), 
                         nombre_nube='Primarias presidenciales: 2017 vs 2021')

