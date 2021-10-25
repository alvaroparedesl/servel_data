read_excel_allsheets <- function(filename, tibble = FALSE) {
  # I prefer straight data.frames
  # but if you like tidyverse tibbles (the default with read_excel)
  # then just pass tibble = TRUE
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}

#' Parsea excel de datos de entrada a data.frame de data.table
#'
#' @param path path a un archivo excel
#'
#' @return un data.frame de data.table en el formato apropiado
#' @export
#'
prep_table <- function(path){
  df <- readxl::read_excel(path)
  
  tipo_mesa <- 'Tipo mesa'
  mesa <- "Mesa"
  t_ <- data.table(df)
  eval(parse(text=sprintf("t_[is.na(`%s`), `%s`:='']", tipo_mesa, tipo_mesa)))
  t_[, mesa_:=do.call(paste0, .SD), .SDcols=c(mesa, tipo_mesa)]
  
  #-------------------------- DICTIO ---------------------------------
  #- Columnas
  idx_cols <- unlist(lapply(DICTIO$columnas$Archivo, function(x) grepl(x, path)))
  if (any(idx_cols)) {
    dict_names <- DICTIO$columnas[idx_cols, c("old", "new")]
    data.table::setnames(t_, dict_names$old, dict_names$new)
  }
  
  #- Renombrando  opcion
  t_[, opcion:=trimws(tolower(opcion))]
  idx_opc <- t_$opcion %in% DICTIO$valores$old
  if (any(idx_opc)) {
    Map <- setNames(DICTIO$valores$new, DICTIO$valores$old)
    t_[idx_opc, opcion:=Map[opcion]]
  }
  
  #- Asignando tendencia
  idx_tend <- unlist(lapply(DICTIO$tendencia$Archivo, function(x) grepl(x, path)))
  if (any(idx_tend)) {
    Map <- with(DICTIO$tendencia[idx_tend, ], setNames(tendencia, tolower(trimws(opcion))))
    t_[, tendencia:=Map[opcion]]
  }
  
  
  return(t_[!is.na(Mesa)])
}


#' Generar un id en común entre mesas de diferentes elecciones
#' 
#' @description Entre elecciones, no siempre se agrupan las mesas de la misma forma. Esta función busca las agrupaciones correspondientes entre elecciones de diferentes periodos.
#'
#' @param blist una lista de data.frames data.table, leídos por prep_table
#'
#' @return un único data.frame data.table, con una columna de grupos por la cuál se puede hacer seguimiento a través del tiempo
#' @export
#'
#' @examples
ids_mesa <- function(blist) {
  
  cols_fil <- c('Circ.Electoral', 'Local', 'Mesa', 'Tipo mesa', 'Mesas Fusionadas')
  
  t1 <- lapply(1:length(blist),
               function(y) {
                 x <- blist[[y]]
                 x[, db:=y]  # database group
                 x[, id:=.GRP + db*10^5, by=c('Circ.Electoral', 'Mesas Fusionadas')]
               })
  
  tt <- lapply(1:length(t1), function(n) {
    y <- t1[[n]]
    unique(y[, c(..cols_fil, "id")])
    # x <- x[, lapply(.SD, function(x) unlist(tstrsplit(x, "-"))), .SDcols = "Mesas Fusionadas", by = c("Local", "Mesa", "Tipo mesa", "id")]
    # x <- x[, lapply(.SD, function(y) strsplit(y, "-")), .SDcols = "Mesas Fusionadas", by = c("Local", "Mesa", "Tipo mesa", "id")]
  })
  
  tt_ <- rbindlist(tt, fill=TRUE)
  t1_ <- rbindlist(t1, fill=TRUE)
  # https://stackoverflow.com/questions/13773770/split-comma-separated-strings-in-a-column-into-separate-rows
  df <- setDT(tt_)[, strsplit(as.character(`Mesas Fusionadas`), "-", fixed=TRUE), by = c("id", cols_fil)] #[, .(`Mesas Fusionadas` = V1, id)] |> as.data.frame()
  setnames(df, "V1", "Mesa individual")
  
  ## Agrupando mesas
  # from: https://stackoverflow.com/questions/37216927/r-group-elements-of-a-list-that-share-at-least-one-value
  # from: https://stackoverflow.com/questions/36659114/using-two-grouping-designations-to-create-one-combined-grouping-variable
  df[, m_code:=paste(Circ.Electoral, `Mesa individual`, sep="-")]
  gmap <-  unique(stack(df[, c("id", "m_code")]))
  gmap$node = seq_len(nrow(gmap))
  
  oldcols = as.character(unique(gmap$ind))
  newcols = paste0("node_", oldcols)
  
  aux1 <- function(i) with(gmap[gmap$ind == i, ], node[ match(df[[i]], values)])
  df[, (newcols):=lapply(oldcols, aux1)]
  
  g = graph_from_edgelist(cbind(df[[newcols[1]]], df[[newcols[2]]]), directed = FALSE)
  gmap$group = components(g)$membership
  
  df$group = gmap$group[ match(df[[newcols[1]]], gmap$node) ]
  
  ans <- merge(t1_, unique(df[, c('id', 'group')]), on="id")  
  ans[, id:=NULL]
  
  return(ans)
}


#' Generar un mapa, estilo raster, con cada agrupación de mesas como un pixel.
#'
#' @param df1
#' @param outname 
#' @param vertical 
#' @param ratio_ 
#' @param paleta un vector con la paleta de colores a usar
#' @param breaks_
#'
#' @return
#' @export
#'
#' @examples
rastPlot <- function(df1, outname, vertical=F, ratio_=15, 
                     paleta=c("#67001f", "#b2182b", "#d6604d", "#f4a582", "#fddbc7", "#d1e5f0", "#92c5de", "#4393c3", "#2166ac", "#053061"),
                     breaks_=0:length(paleta)/length(paleta)) {
  
  n <- nrow(df1)
  wh <- .computeHeightWidth(n=n, ratio=ratio_, vertical)
  width <- wh[['width']]; height <- wh[['height']]; cols=wh[['cols']]; rows=wh[['rows']]
  
  png(outname, width=width, height=height)
  par(mar=c(4, 5, 4, 5))

  out <- .rastPlot(df1)
  
  dev.off()
  
  return(out)
}


rastPlotDual <- function(df1, df2, outname, vertical=F, ratio_=15, 
                         paleta1=c("#67001f", "#b2182b", "#d6604d", "#f4a582", "#fddbc7", "#d1e5f0", "#92c5de", "#4393c3", "#2166ac", "#053061"),
                         paleta2=c("#F7FCF5", "#E5F5E0", "#C7E9C0", "#A1D99B", "#74C476", "#41AB5D", "#238B45", "#006D2C", "#00441B"),
                         breaks1=0:length(paleta1)/length(paleta1),
                         breaks2=0:length(paleta2)/length(paleta2)) {
  
  n <- nrow(df1)
  wh <- .computeHeightWidth(n=n, ratio=ratio_, vertical)
  width <- wh[['width']]; height <- wh[['height']]; cols=wh[['cols']]; rows=wh[['rows']]
  
  png(outname, width=width, height=height/2)
  par(mar=c(4, 6, 4, 6))
  if (vertical) {
    par(mfcol=c(1, 2))
  } else {
    par(mfcol=c(2, 1))
  }
  
  out1 <- .rastPlot(df1, paleta=paleta1, breaks_=breaks1)
  out2 <- .rastPlot(df2, paleta=paleta2, breaks_=breaks2)
  
  dev.off()
  
  return(list(plot1=out1, plot2=out2))
}



# AUX Functions
.computeHeightWidth <- function(n, ratio, vertical) {
  cols <- round(sqrt(n/10))
  rows <- ceiling(n/cols)
  
  if (vertical) {
    width <- cols*ratio; height <- rows*ratio
  } else {
    width <- rows*ratio; height <- cols*ratio
  }
  
  return(list(width=width, height=height, cols=cols, rows=rows))
}

.rastPlot <- function(mt, env = parent.frame(), ...){
  envn <- c(as.list(env), list(...), list(mt=mt))
  with(envn, {
    pl_ <- matrix(NA, nrow=cols, ncol=rows, byrow=F)
    pl_[1:n] <- unlist(mt[, 'per'])
    
    reg_cut <- mt[, list(N=.N, lat=mean(Latitud)), by=c("Reg_cod")]
    setorder(reg_cut, Reg_cod)
    reg_cut[, Nc:=cumsum(N)/sum(N)]
    com_cut <- mt[, list(N=.N, lat=mean(Latitud)), by=c("Reg_cod", "Comuna")]
    setorder(com_cut, Reg_cod, -lat)
    com_cut[, Nc:=cumsum(N)/sum(N)]
    
    if (vertical) {
      ax <- 2; ylim <- c(1, 0); xlim <- c(0, 1); pl__ <- pl_; com_las <- 1
    } else {
      ax <- 1; ylim <- c(1, 0); xlim <- c(0, 1); pl__ <- t(apply(pl_, 2, rev)); com_las <- 2
    }
    
    image(pl__, useRaster=F, ylim=ylim, xlim=xlim, axes=F, breaks=breaks_, col=paleta)
    # axis(ax, (c(0, reg_cut$Nc[-nrow(reg_cut)]) + reg_cut$Nc) / 2, labels=reg_cut$Reg_cod, las=1, cex.axis=2, main="Región")
    mtext(reg_cut$Reg_cod, side=ax, line=1, outer=F, cex=2, las=1,
          at = (c(0, reg_cut$Nc[-nrow(reg_cut)]) + reg_cut$Nc) / 2)
    mtext(com_cut$Comuna, side=ax+2, line=1, outer=F, cex=.7, las=com_las,
          at = (c(0, com_cut$Nc[-nrow(com_cut)]) + com_cut$Nc) / 2)
    if (vertical) {
      abline(h=round(reg_cut$Nc * rows)/rows, lwd=3)
    } else {
      abline(v=round(reg_cut$Nc * rows)/rows, lwd=3)
    }
    return(pl_)
  })
}
