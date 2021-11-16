library(data.table)
library(readxl)
library(igraph)
library(RColorBrewer)
library(rayshader)
library(quadmesh)
# library(rayshader)
# library(ggplot2)

source('scripts/functions.R', encoding="UTF-8")
source('scripts/calculos.R', encoding="UTF-8")

# Diccionario de datos, para las funciones auxiliares
DICTIO <- read_excel_allsheets('scripts/dict.xlsx')
reg_orden <- c(15, 1:5, 13, 6:7, 16, 8:9, 14, 10:12)
comunas <- fread('scripts/comunas.csv')
comunas[, Comuna:=tolower(nom_com)]
# TODO parcial: calcular pendiente de cambio y cambio absoluto con puntaje Z.
# TODO: mapa 3D shiny relieve the 1 + 2 (perc izquierda + porc votación como altura)
# TODO: serie tiempo con Chile, regiones/comunas eje X, eje Y la tendencia, y el área número votantes [área chart] = https://r-statistics.co/Top50-Ggplot2-Visualizations-MasterList-R-Code.html

# Archivos
e2017_pp = prep_table('data/06-Elecciones Primarias 2017/Resultados_Presidente_Primarias2017_Tricel_nacional_DEF.xlsx', sheets=sprintf('%02d', 1:15))
# e2017_1v = prep_table('data/07-Elecciones Presidencial, Parlamentarias y de Cores 2017/Resultados_Mesa_PRESIDENCIAL_Tricel_1v_DEF.xlsx')
# e2017_2v = prep_table('data/07-Elecciones Presidencial, Parlamentarias y de Cores 2017/Resultados_Mesa_PRESIDENCIAL_Tricel_2v_DEF.xlsx')
# e2020_cp = prep_table('data/08-Plebiscito Nacional 2020/Resultados Plebiscito Constitucion Politica 2020_DEF.xlsx')
e2021_pp = prep_table('data/12-Primarias Presidenciales 2021/Resultados_Primarias_Presidenciales_2021_CHILE.xlsx')

elecciones_lista <- nlist(e2017_pp, e2021_pp)

# Columnas
# names(e2017_1v)
# names(e2017_2v)
# names(e2020_cp)


#---- Obtener ids agrupados entre mesas de diferentes periodos
all <- ids_mesa(elecciones_lista)

#--- Tendencias por mesa
ans <- tendencia_mesas(all)


#-------- Cálculos de índices
elec_cols <- c("NA", "-1", "1")   # elec_cols <- c("NA", "-1", "0", "1")
group_cols <- c("db", "group", "Comuna", "Reg_cod")

cindx <- calcular_indices(df=ans, elec_cols=elec_cols, group_cols=group_cols, 
                          comparar=c('e2017_1v', 'e2021_pp'))



#------- plot?
dbs <- unique(ans$db)
dbs_name <- paste(dbs, collapse="-")
ratio_ <- 40
vertical <- T
paleta1 <- brewer.pal(10, 'RdBu')
paleta2 <- brewer.pal(9, 'Greens')

for (db_ in dbs) {
  m_ <- rastPlot(cindx[['proporcion_intra_izq_der']][db==db_],
                 outname=sprintf('proporcion_intra_izq_der_%s.png', db_), 
                 vertical=vertical, 
                 ratio_=ratio_, 
                 paleta1=paleta1)
}

# ----
for (db_ in dbs) {
  m_ <- rastPlot(cindx[['proporcion_intra_vot_hab']][db==db_], 
                 outname=sprintf('proporcion_intra_vot_hab_%s.png', db_), 
                 vertical=vertical, 
                 ratio_=ratio_, 
                 paleta1=paleta2)  
}

# ----
for (db_ in dbs) {
  m_ <- rastPlot(cindx[['proporcion_intra_izq_der']][db==db_], 
                 cindx[['proporcion_intra_vot_hab']][db==db_], 
                 outname=sprintf('eleccion_%s_both.png', db_), 
                 vertical=vertical, 
                 ratio_=ratio_, 
                 paleta1=paleta1, 
                 paleta2=paleta2)
}


m_ <- rastPlot(cindx[['pendiente_extra_izq_der']], 
               outname=sprintf('pendiente_extra_izq_de_%s.png', dbs_name), 
               vertical=vertical, 
               ratio_=ratio_, 
               paleta1=rev(paleta1), 
               breaks1=-5:5)


m_ <- rastPlot(cindx[['proporcion_extra_izq_der']], 
               outname=sprintf('proporcion_extra_izq_der_%s.png', dbs_name), 
               vertical=vertical, 
               ratio_=ratio_, 
               paleta1=paleta1)


m_ <- rastPlot(cindx[['diferencia_extra_izq']], 
               cindx[['diferencia_extra_der']], 
               outname=sprintf('diferencia_extra_izq_der_%s.png', dbs_name), 
               vertical=vertical, 
               ratio_=ratio_, 
               paleta1=paleta2, 
               paleta2=paleta2,
               breaks1=1:10*2,
               breaks2=1:10*2)



#--- plots 3d??
(m_*10) |> sphere_shade(texture = "imhof1") |>
  plot_3d((m_*10), zscale = 10, fov = 0, theta = 135, zoom = 0.75, phi = 45, windowsize = c(1000, 800))

qmesh <- quadmesh(m_*10)
rgl::shade3d(qmesh)




# ----------- Mesas por región
ansg[db==db_, list(N=length(unique(group))), by=c("Reg_cod")]
ansg[, list(N=.N), by=db]
setdiff(ansg[db==1]$group, ansg[db==2]$group)
setdiff(ansg[db==1]$group, ansg[db==3]$group)
ansg[group == 2039]
setdiff(ansg[db==3]$group, ansg[db==1]$group)

#----
ncol(m_$out1)
nrow(m_$out1)

ansg[, split:=1:.N %/% nrow(m_$out1), by="db"]
ansg[, split2:=1:.N %% nrow(m_$out1), by="db"]
gg = ggplot(ansg[db==1]) +
  geom_raster(aes(x=split, y=split2, fill=per)) +
  scale_fill_gradientn(colours=paleta1, limits=c(0,1)) +
  theme_void()

gg
plot_gg(gg, multicore=TRUE, height=5, width=6, scale=100)
render_snapshot(gg)



# --- Extras
ans[group==5228]
hist(ansp$per[ansp$Comuna=='las condes'], breaks=200, xlim=c(-5, 5))
hist(anst$per, breaks=200, xlim=c(-5, 5))

v1 <- dcast(ansg[Comuna=='las condes' & db %in% c(1, 4)], group ~ db, fun.aggregate = mean, value.var="per")

v2 <- merge(v1, ansp[, c("group", "per")]) |> merge(ans[db %in% c(1,4), c("group", "-1", "1", "db")])
setorder(v2, per)
v2[per < 0]
