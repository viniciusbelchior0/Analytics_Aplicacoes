import smtplib
import email.message
import pandas as pd
import numpy as np
import win32com.client as win32

def enviar_email():

    sheet_id = "1vqKnT5Sux9NNiV2hPlim4N13SJsc-SaIXq_zdhaCpCY"
    sheet_name = "db_clientesinfo"
    url = f"https://docs.google.com/spreadsheets/d/{sheet_id}/gviz/tq?tqx=out:csv&sheet={sheet_name}"
    base_clientes = pd.read_csv(url, decimal=",")
    lista = base_clientes[base_clientes.Churn == "Sim"]

    qtd_clientes = (base_clientes['Churn'] == "Sim").sum()
    ltv_estimado = lista['LTV'].sum().round(decimals = 0)

    corpo_email = f"""
    <p>Novas atualizações sob o sistema foram feitas, com a classificação de risco dos novos clientes.</p>
    <p>{qtd_clientes} clientes receberam rotulação de risco. </p>
    <p>Para conferir os clientes e suas estimativas, acesse aqui a <a href="https://datastudio.google.com/reporting/2adb10de-31b6-480d-bf2a-201caeabf989"> dashboard</a> </p>
    <p> Essa é uma mensagem automatizada. </p>
    """

    msg = email.message.Message()
    msg['Subject'] = (f"Alerta - {qtd_clientes} clientes sob risco, em um valor total de {ltv_estimado}")
    msg['From'] = 'bot.analytics.auto@gmail.com'
    msg['To'] = 'viniciusbelchior0@gmail.com'
    password = 'VFzISsBM8L'
    msg.add_header('Content-Type', 'text/html')
    msg.set_payload(corpo_email)

    s = smtplib.SMTP('smtp.gmail.com: 587')
    s.starttls()
    # Login credentials
    s.login(msg['From'], password)
    s.sendmail(msg['From'], [msg['To']], msg.as_string().encode('utf-8'))
    print('Email enviado')

enviar_email()

