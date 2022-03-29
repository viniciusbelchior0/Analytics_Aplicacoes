import pandas as pd
import numpy as np
import joblib

sheet_id = "17ADvhZuI3HJj1YS8wFsBxLo5YauoefujwyxwF9y39K4"
sheet_name = "SerieA_Atual"
url = f"https://docs.google.com/spreadsheets/d/{sheet_id}/gviz/tq?tqx=out:csv&sheet={sheet_name}"

dados_cba = pd.read_csv(url, encoding="utf8", decimal=",")
dados_cba.info()

#remover 3,4,13,14,15,16
# remover data, horario, publico, capitao, arbitro, formacao
dados_cba = dados_cba.drop(columns=['Data','Horário','Publico','Capitao','Formacao','Arbitro'])
dados_cba = dados_cba.loc[(dados_cba['Equipe'] == 'Flamengo') | (dados_cba['Equipe'] == 'Palmeiras')]

#criar novas variaveis: pts, saldo gols,%golspen, cardscore, diffaltas, %goslcontra
conditions = [(dados_cba['Resultado'] == 'V'),
              (dados_cba['Resultado'] == 'E'),
              (dados_cba['Resultado'] == 'D')]
values = [3,1,0]


dados_cba = dados_cba.assign(pts = np.select(conditions,values),
                             saldo_gols = dados_cba['GP'] - dados_cba['GC'],
                             golsPen = dados_cba['PenFeitos']/dados_cba['GP'],
                             cardScore = dados_cba['CrtsA'] + 5*dados_cba['CrtV'],
                             difFaltas = dados_cba['FaltasCometidas'] - dados_cba['FaltasSofridas'],
                             pgolsContra = dados_cba['GolsContra']/dados_cba['GC'])

dados_cba.replace([np.inf, -np.inf], np.nan, inplace=True)
media_posse = dados_cba['Posse'].mean()
dados_cba['Posse'] = dados_cba['Posse'].fillna(media_posse)
dados_cba['pgolsContra'] = dados_cba['pgolsContra'].fillna(0.00)
dados_cba['golsPen'] = dados_cba['golsPen'].fillna(0.00)

# filtrar os dados
home = dados_cba.loc[(dados_cba['Equipe'] == 'Flamengo') & (dados_cba['Rodada'] >= 25) & (dados_cba['Rodada'] < 28)]
away = dados_cba.loc[(dados_cba['Equipe'] == 'Palmeiras') & (dados_cba['Rodada'] >= 25) & (dados_cba['Rodada'] < 28)]

# Agregar os dados
away = away.groupby('Equipe').agg({'GP': np.sum, 'GC': np.sum, 'Posse': np.mean,
                                   'TotalChutes': np.mean, 'ChGol%': np.mean, 'G/Ch': np.mean,
                                   'CaGC': np.mean, '%Defesas': np.mean, 'CleanSheet': np.sum,
                                   'difFaltas':np.mean, 'Impedimentos': np.mean, 'Cruzamentos': np.mean,
                                   'Cortes': np.mean, 'RoubadasDeBola': np.mean, 'pts': np.sum,
                                   'saldo_gols': np.sum, 'cardScore': np.mean, 'golsPen': np.mean,
                                   'pgolsContra': np.sum})
away = away.assign(key='1')

home = home.groupby('Equipe').agg({'GP': np.sum, 'GC': np.sum, 'Posse': np.mean,
                                   'TotalChutes': np.mean, 'ChGol%': np.mean, 'G/Ch': np.mean,
                                   'CaGC': np.mean, '%Defesas': np.mean, 'CleanSheet': np.sum,
                                   'difFaltas':np.mean, 'Impedimentos': np.mean, 'Cruzamentos': np.mean,
                                   'Cortes': np.mean, 'RoubadasDeBola': np.mean, 'pts': np.sum,
                                   'saldo_gols': np.sum, 'cardScore': np.mean, 'golsPen': np.mean,
                                   'pgolsContra': np.sum})
home = home.assign(key='1')

df_SerieA = pd.merge(home, away, on='key')
df_SerieA = df_SerieA.drop('key', axis=1)
df_SerieA.columns = ['gp','gc','posse','totalchutes','%chgol','g_ch','cagc','%defesas','cleansheets','dif_faltas',
                     'impedimentos','cruzamentos','cortes','roubadas','pts','saldo_gols','cardscore','%gols_pen',
                     '%gols_contra','opp_gp','opp_gc','opp_posse','opp_totalchutes','opp_%chgol','opp_g_ch','opp_cagc',
                     'opp_%defesas','opp_cleansheets','opp_dif_faltas','opp_impedimentos','opp_cruzamentos','opp_cortes',
                     'opp_roubadas','opp_pts','opp_saldo_gols','opp_cardscore','opp_%gols_pen','%opp_gols_contra']

df_SerieA['pts'] = df_SerieA['pts'].astype('int64')
df_SerieA['opp_pts'] = df_SerieA['opp_pts'].astype('int64')
df_SerieA.info()

classificador = joblib.load('classificador-seriea.joblib')
predicao = classificador.predict(df_SerieA)
print(predicao)

# NAO ESQUECER DE PROCURAR A FUNCAO TOP_N(3)