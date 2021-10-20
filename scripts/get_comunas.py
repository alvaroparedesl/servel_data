import pandas as pd
import ssl
ssl._create_default_https_context = ssl._create_unverified_context

def conv_coord(x):
    return float(x[0]) - float(x[1])/60 - float(x[2])/60**2

com = pd.read_html('http://es.wikipedia.org/wiki/Anexo:Comunas_de_Chile')[0]
com.rename(columns={'CUT (Código Único Territorial)': 'CUT'}, inplace=True)
com['Latitud'] = com.Latitud.str.replace('[°\'"]', ',', regex=True).str.split(',').apply(conv_coord)
com['Longitud'] = com.Longitud.str.replace('[°\'"]', ',', regex=True).str.split(',').apply(conv_coord)
com['Reg_cod'] = (com.CUT/1000 // 1).astype(int)

com.to_csv('comunas.csv', index=False)