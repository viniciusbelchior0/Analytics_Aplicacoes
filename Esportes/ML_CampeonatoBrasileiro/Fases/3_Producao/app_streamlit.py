import streamlit as st
import pandas as pd
import numpy as np
import joblib
import plotly.express as px

#para rodar o app, digitar no terminal: streamlit run app_streamlit.py
# Titulo
st.image('Campeonato_Brasileiro_Série_A_logo.png')
st.write("""
_ \n
Dados originalmente coletados de: [FBRef](https://fbref.com/en/) |
Dados individuais para cada time: [dados para Equipes (ex: Atlético Mineiro)](https://fbref.com/en/squads/422bb734/2021/matchlogs/s10986/schedule/Atletico-Mineiro-Scores-and-Fixtures-Serie-A). \n

Os dados utilizados refletem capacidade ofensivas e defensivas dos times; estatísticas auxiliares também são utilizadas.
Clique aqui para conferir o [dicionário de dados](https://google.com).
""")

# fazendo as tabelas
url = f"https://docs.google.com/spreadsheets/d/17ADvhZuI3HJj1YS8wFsBxLo5YauoefujwyxwF9y39K4/gviz/tq?tqx=out:csv&sheet=SerieA_Atual"
dados_brasileirao = pd.read_csv(url, encoding="utf8", decimal=",")
tab_1 = dados_brasileirao.groupby('Equipe',as_index=False).agg({'GP': np.mean, 'TotalChutes': np.mean, 'G/Ch': np.mean,
                                                 'CaGC': np.mean,'%Defesas': np.mean, 'GC': np.mean})
tab_1 = tab_1.round(decimals = 2)
st.table(tab_1)

st.subheader("Capacidade ofensiva e defensiva")
#pegando variavies para ajustar as linhas do grafico
v_of = tab_1['TotalChutes'].median()
h_of = tab_1['G/Ch'].median()
v_def = tab_1['CaGC'].median()
h_def = tab_1['%Defesas'].median()

# graficos de desempenho
fig1 = px.scatter(tab_1, x="TotalChutes", y="G/Ch", size = "GP", hover_name = "Equipe", color="GP",color_continuous_scale="algae",
       labels={"TotalChutes": "Total de Chutes por Partida","G/Ch": "Gols por Chute",
               "GP": "Gols por Jogo"}, title= "Capacidade Ofensiva das Equipes") #ofensivo
fig1.add_hline(y = h_of, line_width=1.3, line_dash="dash",line_color="gray")
fig1.add_vline(x = v_of, line_width=1.3, line_dash="dash",line_color="gray")
fig1.add_hrect(y0 =h_of, y1= 0.14,line_width=0, fillcolor="green",opacity =0.1, annotation_text="Ataques sem volume eficientes",annotation_position="top left")
fig1.add_hrect(y0 =0.05, y1=h_of, line_width=0, fillcolor="red", opacity=0.1, annotation_text="Ataques com volume ineficientes", annotation_position="bottom right")
fig1.add_vrect(x0 =8, x1= v_of,line_width=0, fillcolor="palevioletred",opacity =0.1, annotation_text="Ataques sem volume ineficientes",annotation_position="bottom left")
fig1.add_vrect(x0 =v_of, x1= 17,line_width=0, fillcolor="green",opacity =0.08, annotation_text="Ataques com volume eficientes",annotation_position="top right")
fig1.update_layout(title_font_color = "forestgreen", legend_title_font_color="gray", title={'x':0.5, 'xanchor': 'center', 'yanchor': 'top'})
fig1.update_layout(title_font_color = "forestgreen", legend_title_font_color="gray", title={'x':0.5, 'xanchor': 'center', 'yanchor': 'top'})
st.plotly_chart(fig1, use_container_width=True)

st.write("""
Em relação as capacidades ofensivas de uma equipe, três métricas são importantes: 'Chutes por partida',
'Gols por Chute' e 'Gols por Jogo'. \n

O quanto uma equipe chuta ao gol é um indicativo do seu volume ofensivo, no entanto, esse volume deve-se converter em
gols; assim, os gols por chute indicam a efetividade da equipe em transformar esse volume em gols. Gols por jogo
sintetiza a capacidade ofensiva.
""")

fig2 = px.scatter(tab_1, x="CaGC", y="%Defesas", size = "GC", hover_name = "Equipe", color="GC",color_continuous_scale="Oryel",
       labels={"CaGC": "Chutes sofridos por Jogo","%Defesas": "% de Defesas dos Chutes",
               "GC": "Gols sofridos por Jogo"}, title= "Capacidade Defensiva das Equipes")
fig2.add_hline(y = h_def, line_width=1.3, line_dash="dash",line_color="gray")
fig2.add_vline(x = v_def, line_width=1.3, line_dash="dash",line_color="gray")
fig2.add_hrect(y0 =h_def, y1= 0.90,line_width=0, fillcolor="green",opacity =0.1, annotation_text="Defesa sólida e goleiro eficiente",annotation_position="top left")
fig2.add_hrect(y0 =0.55, y1=h_def, line_width=0, fillcolor="red", opacity=0.1, annotation_text="Defesa frágil e goleiro falho", annotation_position="bottom right")
fig2.add_vrect(x0 =2, x1= v_def,line_width=0, fillcolor="green",opacity =0.1, annotation_text="Defesa sólida e goleiro falho",annotation_position="bottom left")
fig2.add_vrect(x0 =v_def, x1= 6,line_width=0, fillcolor="palevioletred",opacity =0.08, annotation_text="Defesa frágil e goleiro eficiente",annotation_position="top right")
fig2.update_layout(title_font_color = "darkslategray", legend_title_font_color="gray", title={'x':0.5, 'xanchor': 'center', 'yanchor': 'top'})
st.plotly_chart(fig2, use_container_width=True)

st.write("""
Se tratando da defesa, 'Chutes sofridos por partida', '% de Defesas' e 'Gols sofridos por jogo' são indicadores
do potencial da equipe nesse aspecto. \n

A quantidade de chutes sofridos indica o quanto a defesa permite que o adversário chegue próximo ao seu objetivo -
quanto menos chutes levar, mais solida é a defesa - o goleiro, também deve ser avaliado; sua capacidade em defender os
chutes sofridos é medida pela sua % de Defesas. Gols sofridos por jogo é uma síntese da capacidade defensiva da equipe.
""")

# Prevendo uma partida
st.header('Predição do Resultado da partida')
st.write("""
O objetivo foi tentar predizer o resultado das partidas baseado no desempenho recente dos times (3 últimas rodadas). Dessa maneira,
o desempenho das equipes - altamente variável, que pode apresentar oscilações durante períodos de tempo - recebe correções,
gerando uma estimativa mais fiel da sua capacidade. \n

""")

# Dados do campeonato
def get_dados_seriea():
    st.sidebar.header("Parâmetros")
    home = st.sidebar.selectbox('Time da Casa',['América (MG)','Atl Goianiense','Atl Paranaense','Atlético Mineiro','Bahia','Bragantino','Ceará','Chapecoense','Corinthians','Cuiabá','Flamengo','Fluminense','Fortaleza','Grêmio','Internacional','Juventude','Palmeiras','Santos','São Paulo','Sport Recife'])
    away = st.sidebar.selectbox('Time Visitante',['América (MG)','Atl Goianiense','Atl Paranaense','Atlético Mineiro','Bahia','Bragantino','Ceará','Chapecoense','Corinthians','Cuiabá','Flamengo','Fluminense','Fortaleza','Grêmio','Internacional','Juventude','Palmeiras','Santos','São Paulo','Sport Recife'])
    n_rodada = st.sidebar.slider('Rodada Atual (a ser predita)',4,38,4)

    home_t = home
    away_t = away
    rodada = n_rodada - 3
    rodada_max = n_rodada

    sheet_id = "17ADvhZuI3HJj1YS8wFsBxLo5YauoefujwyxwF9y39K4"
    sheet_name = "SerieA_Atual"
    url = f"https://docs.google.com/spreadsheets/d/{sheet_id}/gviz/tq?tqx=out:csv&sheet={sheet_name}"

    dados_cba = pd.read_csv(url, encoding="utf8", decimal=",")

    # remover 3,4,13,14,15,16
    # remover data, horario, publico, capitao, arbitro, formacao
    dados_cba = dados_cba.drop(columns=['Data', 'Horário', 'Publico', 'Capitao', 'Formacao', 'Arbitro'])
    dados_cba = dados_cba.loc[(dados_cba['Equipe'] == home_t) | (dados_cba['Equipe'] == away_t)]

    # criar novas variaveis: pts, saldo gols,%golspen, cardscore, diffaltas, %goslcontra
    conditions = [(dados_cba['Resultado'] == 'V'),
                  (dados_cba['Resultado'] == 'E'),
                  (dados_cba['Resultado'] == 'D')]
    values = [3, 1, 0]

    dados_cba = dados_cba.assign(pts=np.select(conditions, values),
                                 saldo_gols=dados_cba['GP'] - dados_cba['GC'],
                                 golsPen=dados_cba['PenFeitos'] / dados_cba['GP'],
                                 cardScore=dados_cba['CrtsA'] + 5 * dados_cba['CrtV'],
                                 difFaltas=dados_cba['FaltasCometidas'] - dados_cba['FaltasSofridas'],
                                 pgolsContra=dados_cba['GolsContra'] / dados_cba['GC'])

    dados_cba.replace([np.inf, -np.inf], np.nan, inplace=True)
    media_posse = dados_cba['Posse'].mean()
    dados_cba['Posse'] = dados_cba['Posse'].fillna(media_posse)
    dados_cba['pgolsContra'] = dados_cba['pgolsContra'].fillna(0.00)
    dados_cba['golsPen'] = dados_cba['golsPen'].fillna(0.00)

    # filtrar os dados
    home = dados_cba.loc[(dados_cba['Equipe'] == home_t) & (dados_cba['Rodada'] >= rodada) & (dados_cba['Rodada'] < rodada_max)]
    away = dados_cba.loc[(dados_cba['Equipe'] == away_t) & (dados_cba['Rodada'] >= rodada) & (dados_cba['Rodada'] < rodada_max)]

    # Agregar os dados
    away = away.groupby('Equipe').agg({'GP': np.sum, 'GC': np.sum, 'Posse': np.mean,
                                       'TotalChutes': np.mean, 'ChGol%': np.mean, 'G/Ch': np.mean,
                                       'CaGC': np.mean, '%Defesas': np.mean, 'CleanSheet': np.sum,
                                       'difFaltas': np.mean, 'Impedimentos': np.mean, 'Cruzamentos': np.mean,
                                       'Cortes': np.mean, 'RoubadasDeBola': np.mean, 'pts': np.sum,
                                       'saldo_gols': np.sum, 'cardScore': np.mean, 'golsPen': np.mean,
                                       'pgolsContra': np.sum})
    away = away.assign(key='1')

    home = home.groupby('Equipe').agg({'GP': np.sum, 'GC': np.sum, 'Posse': np.mean,
                                       'TotalChutes': np.mean, 'ChGol%': np.mean, 'G/Ch': np.mean,
                                       'CaGC': np.mean, '%Defesas': np.mean, 'CleanSheet': np.sum,
                                       'difFaltas': np.mean, 'Impedimentos': np.mean, 'Cruzamentos': np.mean,
                                       'Cortes': np.mean, 'RoubadasDeBola': np.mean, 'pts': np.sum,
                                       'saldo_gols': np.sum, 'cardScore': np.mean, 'golsPen': np.mean,
                                       'pgolsContra': np.sum})
    home = home.assign(key='1')

    df_SerieA = pd.merge(home, away, on='key')
    df_SerieA = df_SerieA.drop('key', axis=1)
    df_SerieA.columns = ['gp', 'gc', 'posse', 'totalchutes', '%chgol', 'g_ch', 'cagc', '%defesas', 'cleansheets',
                         'dif_faltas','impedimentos', 'cruzamentos', 'cortes', 'roubadas', 'pts', 'saldo_gols',
                         'cardscore','%gols_pen','%gols_contra', 'opp_gp', 'opp_gc', 'opp_posse',
                         'opp_totalchutes', 'opp_%chgol', 'opp_g_ch','opp_cagc','opp_%defesas', 'opp_cleansheets',
                         'opp_dif_faltas', 'opp_impedimentos', 'opp_cruzamentos','opp_cortes','opp_roubadas',
                         'opp_pts', 'opp_saldo_gols', 'opp_cardscore', 'opp_%gols_pen','%opp_gols_contra']

    df_SerieA['pts'] = df_SerieA['pts'].astype('int64')
    df_SerieA['opp_pts'] = df_SerieA['opp_pts'].astype('int64')

    return df_SerieA

df_SerieA = get_dados_seriea()

# carregando o modelo
classificador = joblib.load('classificador-seriea.joblib')

# Predicao
predicao = classificador.predict(df_SerieA)

st.write('Estatísticas das equipes:')
st.dataframe(df_SerieA)
st.subheader('Resultado')
st.text(f"O resultado predito da partida é: {predicao} para o time da casa")

