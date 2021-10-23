library(data.table)
library(readxl)
library(igraph)

source('scripts/functions.R')

# Diccionario de datos, para las funciones auxiliares
DICTIO <- read_excel_allsheets('scripts/dict.xlsx')
reg_orden <- c(15, 1:5, 13, 6:7, 16, 8:9, 14, 10:12)
comunas <- fread('scripts/comunas.csv')
comunas[, Comuna:=tolower(nom_com)]
# TODO: usar las regiones de las comunas para asignar región, en el agregado total

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
ans[, `Nro. Región`:=factor(`Nro. Región`, levels=reg_orden)]

com_renames <- setNames(c('aysen', 'la calera', "marchihue", "o'higgins", 'llay-llay', "cabo de hornos", "til til", "treguaco"), 
                        c('aisen', 'calera', "marchigue", "ohiggins", "llaillay", "cabo de hornos(ex-navarino)", "tiltil", "trehuaco"))
ans[Comuna %in% names(com_renames), Comuna:=com_renames[Comuna]]
# 
# setdiff(unique(ans$Comuna), unique(comunas$Comuna))
# setdiff(unique(comunas$Comuna), unique(ans$Comuna))
# 
# ans[group == 22903]
# ans[db==1, list(N=.N), by='Comuna']
# ans[db==1, list(N=.N), by='Nro. Región']


#------- plot?
ansg <- ans[ , list(per=`-1`/(`-1` + `1`)), by=c("db", "group", "Comuna", "Nro. Región")]
ansg <- merge(ansg, comunas[, c("Comuna", "Latitud")], by="Comuna")
setorder(ansg, `Nro. Región`, -Latitud)
ratio_ <- 15
vertical <- T

for (db_ in 1:3) {
  rastPlot(ansg[db==db_], sprintf('eleccion_%s.png', db_), vertical, ratio_)  
}

ansv <- ans[ , list(per=sum(.SD) / electores), by=c("db", "group", "Comuna", "Nro. Región"), .SDcols=c("NA", "-1", "0", "1")]
ansv <- merge(ansv, comunas[, c("Comuna", "Latitud")], by="Comuna")
setorder(ansv, `Nro. Región`, -Latitud)

for (db_ in 1:3) {
  rastPlot(ansv[db==db_], sprintf('eleccion_%s_n.png', db_), vertical, ratio_)  
}

# ----------- Mesas por región
ansg[db==db_, list(N=length(unique(group))), by=c("Nro. Región")]
