library(data.table)
library(readxl)
library(igraph)

source('scripts/functions.R')

# Diccionario de datos, para las funciones auxiliares
DICTIO <- read_excel_allsheets('scripts/dict.xlsx')

# Archivos
e2017_1v = prep_table('data/07-Elecciones Presidencial, Parlamentarias y de Cores 2017/Resultados_Mesa_PRESIDENCIAL_Tricel_1v_DEF.xlsx')
e2017_2v = prep_table('data/07-Elecciones Presidencial, Parlamentarias y de Cores 2017/Resultados_Mesa_PRESIDENCIAL_Tricel_2v_DEF.xlsx')
e2020_cp = prep_table('data/08-Plebiscito Nacional 2020/Resultados Plebiscito Constitucion Politica 2020_DEF.xlsx')

elecciones_lista <- list(e2017_1v, e2017_2v, e2020_cp)


# Columnas
names(e2017_1v)
names(e2017_2v)
names(e2018)
names(e2020_cp)


#---- Obtener ids agrupados entre mesas de diferentes periodos
all <- ids_mesa(elecciones_lista)


