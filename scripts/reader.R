library(data.table)
library(readxl)
library(igraph)
library(RcolorBrewer)

source('scripts/functions.R')

# Diccionario de datos, para las funciones auxiliares
DICTIO <- read_excel_allsheets('scripts/dict.xlsx')
reg_orden <- c(15, 1:5, 13, 6:7, 16, 8:9, 14, 10:12)
comunas <- fread('scripts/comunas.csv')
comunas[, Comuna:=tolower(nom_com)]

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
all_ <- all[, list(N=sum(`Votos TRICEL`)), by=c('db', 'Nro. Región', 'Comuna', 'group', 'Electores', 'tendencia')]

#--- tendencia por mesa y elección??
ans <- dcast(all_, 
             `Nro. Región` + Comuna + group + db + Electores ~ tendencia, value.var=c("N"), fun.aggregate=sum)
ans[, Comuna:=tolower(iconv(Comuna, from='UTF-8', to = 'ASCII//TRANSLIT'))]

com_renames <- setNames(c('aysen', 'la calera', "marchihue", "o'higgins", 'llay-llay', "cabo de hornos", "til til", "treguaco"), 
                        c('aisen', 'calera', "marchigue", "ohiggins", "llaillay", "cabo de hornos(ex-navarino)", "tiltil", "trehuaco"))

ans[Comuna %in% names(com_renames), Comuna:=com_renames[Comuna]]

setdiff(unique(ans$Comuna), unique(comunas$Comuna))
setdiff(unique(comunas$Comuna), unique(ans$Comuna))

ans[group == 22903]
ans[db==1, list(N=.N), by='Comuna']
ans[db==1, list(N=.N), by='Nro. Región']


# plot?
ansg <- ans[ , list(per=`-1`/(`-1` + `1`)), by=c("db", "group", "Comuna", "Nro. Región")]
ansg <- merge(ansg, comunas[, c("Comuna", "Latitud")], by="Comuna")
setorder(ansg, Latitud)

n <- sum(ansg$db == 1)
n_ <- ceiling(sqrt(n))
pl_ <- matrix(unlist(ansg[db==1, 'per']), nrow=n_, byrow=F)
pl_[(n + 1):(n_^2)] <- NA
pl_[1, 1] <- .99
pl_[n_, n_] <- .01

mcolor <- c("#67001f", "#b2182b", "#d6604d", "#f4a582", "#fddbc7", "#d1e5f0", "#92c5de", "#4393c3", "#2166ac", "#053061")
image(pl_, breaks=0:10/10, col=mcolor, useRaster=T, ylim=c(1, 0), axes=F)
