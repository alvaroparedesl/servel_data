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


source('scripts/functions.R', encoding="UTF-8")
source('scripts/calculos.R', encoding="UTF-8")

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
e2017_pv = prep_table(data_folder, '2017_presidencial_1v', dict = DICTIO)
e2021_pv = prep_table(data_folder, '2021_presidencial_1v', dict = DICTIO)

elecciones_lista <- nlist(e2017_pv, e2021_pv)


#---- Obtener ids agrupados entre mesas de diferentes periodos
all <- ids_mesa(elecciones_lista)

#--- Tendencias por mesa
ans <- tendencia_mesas(all)


#-------- Cálculos de índices
elec_cols <- c("NA", "-1", "1")   # elec_cols <- c("NA", "-1", "0", "1")
group_cols <- c("db", "group", "nom_com", "Reg_cod", "Latitud")

cindx <- calcular_indices(df=ans, elec_cols=elec_cols, group_cols=group_cols, 
                          comparar=c('e2017_pv', 'e2021_pv'))



#------- plot

ccol <- c(brewer.pal(4, 'Purples'), 
          brewer.pal(4, 'Blues'),
          brewer.pal(4, 'Greens'),
          brewer.pal(4, 'Reds'))


mang <- cindx$magnitud_angulo
mang[, color:=cut(angle, c(-181, -90, 0, 90, 180), labels=1:4)]  # labels=c('--', '-+', '++', '+-')
intensity_cutpoints <- round(quantile(mang$magnitud, na.rm=T))
mang[, intensidad:=cut(magnitud, intensity_cutpoints, labels=1:4)]
mang[, per:=as.numeric(color)*10 + as.numeric(intensidad)]
cvals <- rep(1:4*10, each=4) + rep(1:4, 4) 
ccuts <- c(cvals -.5, 45)



dbs <- unique(ans$db)
dbs_name <- paste(dbs, collapse="-")
res <- 20
ratio <- 10
vertical <- T
paleta1 <- rev(brewer.pal(10, 'RdBu'))
paleta2 <- brewer.pal(9, 'Greens')

c_ <- 0:length(paleta1)/length(paleta1)
# plot(c_, c_, col=paleta1, pch=16, cex=4)
# abline(h=c_, v=c_)


##-------------------------------------

png(sprintf('%s/nube_puntos_%s.png', root, dbs_name), width=1200, height=800)
pointPlot(cindx,
          back_colors = ccol[c(10, 14, 2, 6)],
          main_title = 'Presidenciales: 2017 vs 2021 (Primera vuelta)',
          log=T)
dev.off()



for (db_ in dbs) {
  m_ <- rastPlot(cindx[['proporcion_intra_izq_der']][db==db_],
                 outname=sprintf('%s/proporcion_intra_izq_der_%s.png', root, db_), 
                 vertical=vertical, 
                 ratio_=ratio, res_=res,
                 paleta1=paleta1)
}

# ----
for (db_ in dbs) {
  m_ <- rastPlot(cindx[['proporcion_intra_vot_hab']][db==db_], 
                 outname=sprintf('%s/proporcion_intra_vot_hab_%s.png', root, db_), 
                 vertical=vertical, 
                 ratio_=ratio, res_=res,
                 paleta1=paleta2)  
}

# ----
for (db_ in dbs) {
  m_ <- rastPlot(cindx[['proporcion_intra_izq_der']][db==db_], 
                 cindx[['proporcion_intra_vot_hab']][db==db_], 
                 outname=sprintf('%s/proporcion_intra_izq_der__vot_hab_%s.png', root, db_), 
                 vertical=vertical, 
                 ratio_=ratio, res_=res,
                 paleta1=paleta1, 
                 paleta2=paleta2)
}


m_ <- rastPlot(cindx[['pendiente_extra_izq_der']], 
               outname=sprintf('%s/pendiente_extra_izq_der_%s.png', root, dbs_name), 
               vertical=vertical, 
               ratio_=ratio, res_=res,
               paleta1=rev(paleta1), 
               breaks1=-5:5)


m_ <- rastPlot(cindx[['proporcion_extra_izq_der']], 
               outname=sprintf('%s/proporcion_extra_izq_der_%s.png', root, dbs_name), 
               vertical=vertical, 
               ratio_=ratio, res_=res,
               paleta1=paleta1)


m_ <- rastPlot(cindx[['diferencia_extra_izq']], 
               cindx[['diferencia_extra_der']], 
               outname=sprintf('%s/diferencia_extra_izq__der%s.png', root, dbs_name), 
               vertical=vertical, 
               ratio_=ratio, res_=res,
               paleta1=paleta2, 
               paleta2=paleta2,
               breaks1=0:9*2,
               breaks2=0:9/9*2)


m_ <- rastPlot(cindx$magnitud_angulo, 
               outname=sprintf('%s/angulo_magnitud_%s.png', root, dbs_name), 
               vertical=vertical, 
               ratio_=ratio, res_=res,
               paleta1=ccol, 
               breaks1=ccuts)

# Leyenda
or_mar <- par("mar")
par(mar=c(4, 1, 1, 10))
leyenda <- (matrix(cvals, nrow=4))
image(leyenda, useRaster=F, axes=F, breaks=ccuts, col=ccol)
mtext('Magnitud cambio (N)', 1, 2, cex=1.5)
labels_ <- c("Izquierda+ | Derecha-", "Izquierda+ | Derecha+", "Izquierda | Derecha+", "Izquierda- | Derecha-")
inlabels <- sprintf('(%s - %s]', intensity_cutpoints[-length(intensity_cutpoints)], intensity_cutpoints[-1])
axis(4, 0:3/3, las=1, labels_, col="white")
text(rep(0:3/3, 4), rep(0:3/3, each=4), labels=rep(inlabels, 4))
par(mar=or_mar)


##------------------------------------
mts <- lapply(c(14, 24, 34, 44), function(x) {
  mt <- matrix(x, nrow=4, ncol=4)
  mt[1:3, 1:3] <- x-1
  mt[1:2, 1:2] <- x-2
  mt[1, 1] <- x-3
  mt
})
rotate <- function(x) t(apply(x, 2, rev))

ley <- rbind(cbind(rotate(rotate(mts[[4]])), rotate(rotate(rotate(mts[[3]])))), 
             cbind(rotate(mts[[1]]), mts[[2]]) )

png(sprintf('%s/angulo_magnitud_leyenda_%s.png', root, dbs_name), width=800, height=800)
par(mar=c(6, 6, 2, 2))
image(rotate(ley), useRaster=F, axes=F, breaks=ccuts, col=ccol)
axisTickx <- 0.5:7/7
abline(h=.5, v=.5, lwd=2)
abline(h=axisTickx, v=axisTickx, lwd=1, col=rgb(0, 0, 0, .3))
axisticks_ <- c(-axisTickx[1], axisTickx, 1 + axisTickx[1])
mtext('Magnitud de cambio en el Nº de Votos', 1, 2.5)
mtext('Magnitud de cambio en el Nº de Votos', 2, 4)
axis(1, axisticks_, c(-1*rev(intensity_cutpoints[-1]), 0, intensity_cutpoints[-1]))
axis(2, axisticks_, c(-1*rev(intensity_cutpoints[-1]), 0, intensity_cutpoints[-1]), las=2)
mtext('Izquierda- | Derecha-', 1, -1, at=0.1, col='white')
mtext('Izquierda- | Derecha+', 1, -1, at=.9, col='white')
mtext('Izquierda+ | Derecha-', 3, -1, at=0.1, col='white')
mtext('Izquierda+ | Derecha+', 3, -1, at=.9, col='white')
dev.off()
par(mar=or_mar)
##-------------------------------------


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



#---- Dummy data

dummy <- data.table(x=c(rep(100, 6), c(110, 90, 80, 90, 110, 120)), 
                    y=c(rep(100, 6), c(90, 110, 90, 80, 120, 110)),
                    caso=rep(LETTERS[1:6], 2),
                    tiempo=rep(c("t1", "t2"), each=6)
)

with(dummy[caso=='A'], plot(x, y, col=c("red", "blue"), pch=16))
# dummy[, c("xn", "yn"):=list(x-min(x), y-min(y)), by='caso']
dummy[, c("xn", "yn"):=list(x-x[1], y-y[1]), by='caso']
dum <- dummy[, list(xdif=xn[2] - xn[1],
                    ydif=yn[2] - yn[1]),
             by='caso']
dum[, slope:=ydif/xdif]
dum[, magnitud:=sqrt(xdif^2 + ydif^2)]
dum[, angle:=atan2(ydif, xdif)*180/pi]

with(dummy, plot(NA, xlim=range(xn), ylim=range(yn), xlab='-1 (izquierda)', ylab='1 (derecha)'))
abline(h=0, v=0)
for (i in unique(dummy$caso)) {
  with(dummy[caso==i], points(xn, yn, col=c("red", "blue"), pch=16))
  with(dummy[caso==i], text(xn[2], yn[2], i))
}
