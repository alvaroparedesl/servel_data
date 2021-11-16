calcular_indices <- function(df, group_cols, elec_cols, cindex=NULL) {
  
  if (is.null(cindex)) {
    out <- list()
  } else{
    out <- cindex
  }
    
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
  cast_ = dcast(ans[db %in% c(1, 4), ], Reg_cod + Comuna + Latitud + group ~ db, value.var=elec_cols)
  form <- sprintf('(`-1_%2$s` - `-1_%1$s`) / (`1_%2$s` - `1_%1$s`)', 1, 4)
  ansp <- eval(parse(text=sprintf('cast_[, list(per=%s), by=group_cols__db]', form)) )
  ansp <- merge(ansp, comunas[, c("Comuna", "Latitud")], by="Comuna")
  setorder(ansp, Reg_cod, -Latitud)
  
  # Proporción de votos entre elecciones izq/der
  cast_ = dcast(ans[db %in% c(1, 4), ], Reg_cod + Comuna + Latitud + group ~ db, value.var=elec_cols)
  form <- sprintf('(`-1_%2$s` + `-1_%1$s`) / (`1_%2$s` + `1_%1$s` + `-1_%2$s` + `-1_%1$s`)', 1, 4)
  anst <- eval(parse(text=sprintf('cast_[, list(per=%s), by=group_cols__db]', form)) )
  anst <- merge(anst, comunas[, c("Comuna", "Latitud")], by="Comuna")
  setorder(anst, Reg_cod, -Latitud)
  
  
  # Diferencia votaciones izq
  cast_ = dcast(ans[db %in% c(1, 4), ], Reg_cod + Comuna + Latitud + group ~ db, value.var=elec_cols)
  form <- sprintf('(`-1_%2$s` / `-1_%1$s`)', 1, 4)
  ansi <- eval(parse(text=sprintf('cast_[, list(per=%s), by=group_cols__db]', form)) )
  ansi <- merge(ansi, comunas[, c("Comuna", "Latitud")], by="Comuna")
  setorder(ansi, Reg_cod, -Latitud)
  
  # Diferencia votaciones der
  cast_ = dcast(ans[db %in% c(1, 4), ], Reg_cod + Comuna + Latitud + group ~ db, value.var=elec_cols)
  form <- sprintf('(`1_%2$s` / `1_%1$s`)', 1, 4)
  ansd <- eval(parse(text=sprintf('cast_[, list(per=%s), by=group_cols__db]', form)) )
  ansd <- merge(ansd, comunas[, c("Comuna", "Latitud")], by="Comuna")
  setorder(ansd, Reg_cod, -Latitud)
  
  class(out) <- 'cindex'
  out
}