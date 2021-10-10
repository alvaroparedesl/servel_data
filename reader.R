library(data.table)
library(readxl)
library(igraph)

setwd('G:/GIT/servel_data')

prep_table <- function(df){
  tipo_mesa <- 'Tipo mesa'
  mesa <- "Mesa"
  t_ <- data.table(df)
  eval(parse(text=sprintf("t_[is.na(`%s`), `%s`:='']", tipo_mesa, tipo_mesa)))
  t_[, mesa_:=do.call(paste0, .SD), .SDcols=c(mesa, tipo_mesa)]
  return(t_)
}

e2017_1v = prep_table(read_excel('07-Elecciones Presidencial, Parlamentarias y de Cores 2017/Resultados_Mesa_PRESIDENCIAL_Tricel_1v_DEF.xlsx'))
e2017_2v = prep_table(read_excel('07-Elecciones Presidencial, Parlamentarias y de Cores 2017/Resultados_Mesa_PRESIDENCIAL_Tricel_2v_DEF.xlsx'))
e2018 = prep_table(read_excel('08-Plebiscito Nacional 2020/Resultados Plebiscito Constitucion Politica 2020_DEF.xlsx'))
e2020_cp = prep_table(read_excel('08-Plebiscito Nacional 2020/Resultados Plebiscito Constitucion Politica 2020_DEF.xlsx'))

# Columnas
names(e2017_1v)
names(e2017_2v)
names(e2018)
names(e2020_cp)

# --------------
cols_fil <- c('Local', 'Mesa', 'Tipo mesa', 'Mesas Fusionadas')

# ¿qué circunscripción?
head(unique(e2017_1v[grep('-', `Mesas Fusionadas`), c('Circ.Electoral', ..cols_fil)])[, list(N=.N), by='Circ.Electoral'][order(-N)], 20)

c_elec <- 'PUTRE'  # 'PEÑALOLEN' 

t1 <- lapply(list(unique(e2017_1v[Circ.Electoral == c_elec, ..cols_fil]),
                  unique(e2018[Circ.Electoral == c_elec, ..cols_fil]),
                  unique(e2020_cp[Circ.Electoral == c_elec, ..cols_fil])),
             function(x) {
               x[, id:=1:.N]
               x[, grupo:=0]
             })

tt <- lapply(t1, function(x) {
         fus1 <- x[grepl('-', `Mesas Fusionadas`), id]
         # x <- x[, lapply(.SD, function(x) unlist(tstrsplit(x, "-"))), .SDcols = "Mesas Fusionadas", by = c("Local", "Mesa", "Tipo mesa", "id")]
         x <- x[, lapply(.SD, function(y) strsplit(y, "-")), .SDcols = "Mesas Fusionadas", by = c("Local", "Mesa", "Tipo mesa", "id")]
         x[, fus:=0]
         x[id %in% fus1, fus:=1]
         x[fus==1, ]
       })

tt <- lapply(1:length(tt), function(x) {
  p <- tt[[x]]
  p[, db:=x]  # database group
  p
})

tt_ <- do.call('rbind', tt)

gm_ <- 1  # grupo de mesas, empezando en 1
out_ <- c()
grps <- list()
ids <- list()  # no es realmente necesario, pero para el registro
for (i in 1:nrow(tt_)) {
  if (!i %in% out_) {                                 # si la fila ya no está, fuera
    mesas <- unlist(tt_[i, `Mesas Fusionadas`])       # mesas iniciales
    if (all(mesas %in% unlist(grps))) next            # si alguna de estas mesas fusionadas ya está en la lista, saltarla
    dbg <- tt_[i, db]                                 # Grupo de la database
    ids_ <- list()
    ids_[[dbg]] <- tt_[i, id]                         # ids iniciales
    if (i == nrow(tt_)) break                         # salir del loop si ya estamos en la última fila (no podemos seguir j + 1)
    for (j in (i+1):nrow(tt_)) {                      # itero por el resto de las filas que hay
      if (!j %in% out_) {                             # si la fila ya no está, fuera
        dbg_ <- tt_[j, db]                            # grupo de la db de esta fila
        if (dbg != dbg_ ) {                           # si no estoy en el mismo grupo, avanzo
          mes_ <- unlist(tt_[j, `Mesas Fusionadas`])  # mesas de la fila j
          if (any(mesas %in% mes_)) {                 # si hay mesas en común
            out_ <- c(out_, j)                        # retiro esta fila de una futura iteración
            mesas <- unique(c(mesas, mes_))           # concateno las mesas de esta fila con el resto (aunque sea más de una)
            # ids_ <- (c(ids_, tt_[j, id] ))
            ids_[[dbg_]] <- tt_[j, id]
          }
        }
      }
    }
    grps[[gm_]] <- mesas
    ids[[gm_]] <- ids_
    gm_ <- gm_ + 1
  }
}

gm_ <- 1  # grupo de mesas, empezando en 1
grps <- c()
for (mesas in unique(tt_[, `Mesas Fusionadas`])) {                         # si la fila ya no está, fuera
  # mesas <- unlist(tt_[i, `Mesas Fusionadas`])       # mesas iniciales
  # if (all(mesas %in% names(grps))) next             # si alguna de estas mesas fusionadas ya está en la lista, saltarla
  # if (i == nrow(tt[[1]])) break                         # salir del loop si ya estamos en la última fila (no podemos seguir j + 1)
  for (dbg in 2:length(tt)) {
    for (j in 1:nrow(tt[[dbg]])) {                      # itero por el resto de las filas que hay
      mes_ <- unlist(tt[[dbg]][j, `Mesas Fusionadas`])  # mesas de la fila j
      if (any(mesas %in% mes_)) {                 # si hay mesas en común
        mesas <- unique(c(mesas, mes_))           # concateno las mesas de esta fila con el resto (aunque sea más de una)
      }
    }
  }
  for (m in mesas) {
    if (m %in% names(grps)) {
      
    } else {
      grps[m] <- gm_
    }
  }
  ids[[gm_]] <- ids_
  gm_ <- gm_ + 1
}

grps_ <- split(names(grps), grps)


l <- unique(tt_[, `Mesas Fusionadas`])
df <- data.frame(id = rep(seq_along(l), lengths(l)), l = unlist(l))

gmap = unique(stack(df))
gmap$node = seq_len(nrow(gmap))

oldcols = unique(gmap$ind)
newcols = paste0("node_", oldcols)
df[ newcols ] = lapply(oldcols, function(i)  with(gmap[gmap$ind == i, ], 
                                                  node[ match(df[[i]], values) ]
))

g = graph_from_edgelist(cbind(df[, newcols[1]], df[,newcols[2]]), directed = FALSE)
gmap$group = components(g)$membership

df$group = gmap$group[ match(df[, newcols[1]], gmap$node) ]

grps_ <- split(df$l, df$group)


mgroup <- -1   # negativo, para no topar con la otra numeración de grupos
for (k in 1:length(grps_)) {
  regx_ <- sprintf('(^|-)(%s)($|-)', paste(grps_[[k]], collapse="|"))
  for (l in 1:length(t1)) {
    t1[[l]][grep(regx_, t1[[l]]$`Mesas Fusionadas`), grupo:=mgroup]
  }
  mgroup <- mgroup - 1
}

t1
