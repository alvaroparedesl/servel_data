calcular_indices <- function(df, group_cols, elec_cols, comparar=NULL, cindex=NULL) {
  
  if (is.null(cindex)) {
    out <- list()
  } else{
    out <- cindex
  }
  
  if (is.null(comparar)) {
    comp_ <- comparar
  } else {
    comp_ <- unique(df$db)[1:2]
  }
    
  group_cols__db <- group_cols[-which(group_cols == 'db')]
  c1 <- comp_[1]
  c2 <- comp_[2]
  
  # Proporción izquierda vs derecha 
  if (!'proporcion_intra_izq_der' %in% names(out)) {
    ansg <- ans[ , list(per=`-1`/(`-1` + `1`)), by=group_cols]
    ansg <- merge(ansg, comunas[, c("Comuna", "Latitud")], by="Comuna")
    setorder(ansg, Reg_cod, -Latitud)
    out[['proporcion_intra_izq_der']] <- ansg
  }

  # Proporción de votantes vs habilitados
  if (!'proporcion_intra_vot_hab' %in% names(out)) {
    ansv <- ans[ , list(per=sum(.SD) / electores), by=group_cols, .SDcols=elec_cols]
    ansv <- merge(ansv, comunas[, c("Comuna", "Latitud")], by="Comuna")
    setorder(ansv, Reg_cod, -Latitud)
    out[['proporcion_intra_vot_hab']] <- ansg
  }

  # Pendiente de cambio izquierda vs derecha
  cast_ = dcast(ans[db %in% c(c1, c2), ], Reg_cod + Comuna + Latitud + group ~ db, value.var=elec_cols)
  form <- sprintf('(`-1_%2$s` - `-1_%1$s`) / (`1_%2$s` - `1_%1$s`)', c1, c2)
  ansp <- eval(parse(text=sprintf('cast_[, list(per=%s), by=group_cols__db]', form)) )
  ansp <- merge(ansp, comunas[, c("Comuna", "Latitud")], by="Comuna")
  setorder(ansp, Reg_cod, -Latitud)
  out[['pendiente_extra_izq_der']] <- ansp
  
  # Proporción de votos entre elecciones izq/der
  cast_ = dcast(ans[db %in% c(c1, c2), ], Reg_cod + Comuna + Latitud + group ~ db, value.var=elec_cols)
  form <- sprintf('(`-1_%2$s` + `-1_%1$s`) / (`1_%2$s` + `1_%1$s` + `-1_%2$s` + `-1_%1$s`)', c1, c2)
  anst <- eval(parse(text=sprintf('cast_[, list(per=%s), by=group_cols__db]', form)) )
  anst <- merge(anst, comunas[, c("Comuna", "Latitud")], by="Comuna")
  setorder(anst, Reg_cod, -Latitud)
  out[['proporcion_extra_izq_der']] <- ansp
  
  
  # Diferencia votaciones izq
  cast_ = dcast(ans[db %in% c(c1, c2), ], Reg_cod + Comuna + Latitud + group ~ db, value.var=elec_cols)
  form <- sprintf('(`-1_%2$s` / `-1_%1$s`)', c1, c2)
  ansi <- eval(parse(text=sprintf('cast_[, list(per=%s), by=group_cols__db]', form)) )
  ansi <- merge(ansi, comunas[, c("Comuna", "Latitud")], by="Comuna")
  setorder(ansi, Reg_cod, -Latitud)
  out[['diferencia_extra_izq']] <- ansi
  
  # Diferencia votaciones der
  cast_ = dcast(ans[db %in% c(c1, c2), ], Reg_cod + Comuna + Latitud + group ~ db, value.var=elec_cols)
  form <- sprintf('(`1_%2$s` / `1_%1$s`)', c1, c2)
  ansd <- eval(parse(text=sprintf('cast_[, list(per=%s), by=group_cols__db]', form)) )
  ansd <- merge(ansd, comunas[, c("Comuna", "Latitud")], by="Comuna")
  setorder(ansd, Reg_cod, -Latitud)
  out[['diferencia_extra_der']] <- ansd
  
  
  # Angulo y magnitud
  am <- ans[, list(xdif=`1`[2] - `1`[1],
                   ydif=`-1`[2] - `-1`[1]),
            by=c('Reg_cod', 'Comuna', 'group')]
  am[, slope:=ydif/xdif]
  am[, magnitud:=sqrt(xdif^2 + ydif^2)]
  am[, angle:=atan2(ydif, xdif)*180/pi]
  am[, xdif_log := ifelse(xdif < 0, -1, 1) * ifelse(log10(abs(xdif) + .1) == -1, 0, log10(abs(xdif) + .01) )]
  am[, ydif_log := ifelse(ydif < 0, -1, 1) * ifelse(log10(abs(ydif) + .1) == -1, 0, log10(abs(ydif) + .01) )]
  am <- merge(am, comunas[, c("Comuna", "Latitud")], by="Comuna")
  setorder(ansd, Reg_cod, -Latitud)
  out[['magnitud_angulo']] <- am
  
  
  class(out) <- 'cindex'
  out
}