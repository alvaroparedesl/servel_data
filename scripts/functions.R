#' Parsea excel de datos de entrada a data.frame de data.table
#'
#' @param df A data.frame
#'
#' @return un data.frame de data.table en el formato apropiado
#' @export
#'
prep_table <- function(df){
  tipo_mesa <- 'Tipo mesa'
  mesa <- "Mesa"
  t_ <- data.table(df)
  eval(parse(text=sprintf("t_[is.na(`%s`), `%s`:='']", tipo_mesa, tipo_mesa)))
  t_[, mesa_:=do.call(paste0, .SD), .SDcols=c(mesa, tipo_mesa)]
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

