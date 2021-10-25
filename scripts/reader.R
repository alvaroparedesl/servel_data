library(data.table)
library(readxl)
library(igraph)
library(RColorBrewer)
library(rayshader)
library(quadmesh)
# library(rayshader)
# library(ggplot2)

source('scripts/functions.R')

# Diccionario de datos, para las funciones auxiliares
DICTIO <- read_excel_allsheets('scripts/dict.xlsx')
reg_orden <- c(15, 1:5, 13, 6:7, 16, 8:9, 14, 10:12)
comunas <- fread('scripts/comunas.csv')
comunas[, Comuna:=tolower(nom_com)]
# TODO parcial: calcular pendiente de cambio y cambio absoluto con puntaje Z.
# TODO: mapa 3D shiny relieve the 1 + 2 (perc izquierda + porc votación como altura)
# TODO: serie tiempo con Chile, regiones/comunas eje X, eje Y la tendencia, y el área número votantes [área chart] = https://r-statistics.co/Top50-Ggplot2-Visualizations-MasterList-R-Code.html

# Archivos
e2017_1v = prep_table('data/07-Elecciones Presidencial, Parlamentarias y de Cores 2017/Resultados_Mesa_PRESIDENCIAL_Tricel_1v_DEF.xlsx')
e2017_2v = prep_table('data/07-Elecciones Presidencial, Parlamentarias y de Cores 2017/Resultados_Mesa_PRESIDENCIAL_Tricel_2v_DEF.xlsx')
e2020_cp = prep_table('data/08-Plebiscito Nacional 2020/Resultados Plebiscito Constitucion Politica 2020_DEF.xlsx')

elecciones_lista <- list(e2017_1v, e2017_2v, e2020_cp)


# Columnas
names(e2017_1v)
names(e2017_2v)
names(e2020_cp)


#---- Obtener ids agrupados entre mesas de diferentes periodos
all <- ids_mesa(elecciones_lista)
all_ <- all[, list(N=sum(`Votos TRICEL`)), by=c('db', 'Nro. Región', 'Comuna', 'group', 'Mesa', 'Electores', 'tendencia')]
all_ <- all_[, list(electores=sum(Electores), N=sum(N)), by=c('db', 'Nro. Región', 'Comuna', 'group', 'tendencia')]

#--- tendencia por mesa y elección??
ans <- dcast(all_, 
             `Nro. Región` + Comuna + group + db + electores ~ tendencia, value.var=c("N"), fun.aggregate=sum)
ans[, Comuna:=tolower(iconv(Comuna, from='UTF-8', to = 'ASCII//TRANSLIT'))]

com_renames <- setNames(c('aysen', 'la calera', "marchihue", "o'higgins", 'llay-llay', "cabo de hornos", "til til", "treguaco"), 
                        c('aisen', 'calera', "marchigue", "ohiggins", "llaillay", "cabo de hornos(ex-navarino)", "tiltil", "trehuaco"))
ans[Comuna %in% names(com_renames), Comuna:=com_renames[Comuna]]
ans <- merge(ans, comunas[, c("Reg_cod", "Comuna", "Latitud")], by="Comuna")
ans[, Reg_cod:=factor(Reg_cod, levels=reg_orden)]
ans[, `Nro. Región`:=NULL]
# 
# setdiff(unique(ans$Comuna), unique(comunas$Comuna))
# setdiff(unique(comunas$Comuna), unique(ans$Comuna))
# 
# ans[group == 22903]
# ans[db==1, list(N=.N), by='Comuna']
# ans[db==1, list(N=.N), by='Reg_cod']

#-------- Cálculos
elec_cols <- c("NA", "-1", "0", "1")
group_cols <- c("db", "group", "Comuna", "Reg_cod")
group_cols__db <- group_cols[-which(group_cols == 'db')]

# Proporción izquierda vs derecha 
ansg <- ans[ , list(per=`-1`/(`-1` + `1`)), by=group_cols]
ansg <- merge(ansg, comunas[, c("Comuna", "Latitud")], by="Comuna")
setorder(ansg, Reg_cod, -Latitud)

# Proporción de votantes vs habilitados
ansv <- ans[ , list(per=sum(.SD) / electores), by=group_cols, .SDcols=elec_cols]
ansv <- merge(ansv, comunas[, c("Comuna", "Latitud")], by="Comuna")
setorder(ansv, Reg_cod, -Latitud)

# Pendiente de cambio izquierda vs derecha
cast_ = dcast(ans[db %in% c(2, 3), ], Reg_cod + Comuna + Latitud + group ~ db, value.var=elec_cols)
form <- sprintf('(`-1_%2$s` - `-1_%1$s`) / (`1_%2$s` - `1_%1$s`)', 2, 3)
ansp <- eval(parse(text=sprintf('cast_[, list(per=%s), by=group_cols__db]', form)) )
ansp <- merge(ansp, comunas[, c("Comuna", "Latitud")], by="Comuna")
setorder(ansp, Reg_cod, -Latitud)

#------- plot?
ratio_ <- 15
vertical <- T
paleta1 <- brewer.pal(10, 'RdBu')
paleta2 <- brewer.pal(9, 'Greens')

for (db_ in 1:3) {
  m_ <- rastPlot(ansg[db==db_], sprintf('eleccion_%s.png', db_), vertical, ratio_, paleta1)
}

# ----
for (db_ in 1:3) {
  m_ <- rastPlot(ansv[db==db_], sprintf('eleccion_%s_n.png', db_), vertical, ratio_, paleta2)  
}


for (db_ in 1:3) {
  m_ <- rastPlotDual(ansg[db==db_], ansv[db==db_], sprintf('eleccion_%s_both.png', db_), vertical, ratio_, paleta1, paleta2)
}


m_ <- rastPlot(ansp, sprintf('eleccion_%s_p.png', '23'), vertical, ratio_, rev(paleta1), breaks=-5:5)

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
