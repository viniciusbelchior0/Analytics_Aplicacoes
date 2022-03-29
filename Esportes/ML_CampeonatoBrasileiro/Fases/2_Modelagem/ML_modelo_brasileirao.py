import pandas as pd
import numpy as np
from sklearn.model_selection import train_test_split
from sklearn.metrics import classification_report, confusion_matrix, accuracy_score
from sklearn.model_selection import GridSearchCV
from sklearn.ensemble import RandomForestClassifier
from sklearn.svm import SVC

dfSerieA_3 = pd.read_csv(r'C:\Users\NOTEBOOK CASA\Desktop\CampeonatoBrasileiro\Dados\df_SerieA_3.csv', encoding="latin-1")
dfSerieA_3.info()
dfSerieA_3 = dfSerieA_3.drop(columns=['Unnamed: 0','equipe','ano','rodada','dia','local','oponente','formacao', \
                                      'id_rodada','id_opp','opp_formacao'])
dfSerieA_3.info()

# Tipo 1 - Três resultados possíveis (Vitória, Empate e Derrota)
X_train, X_test, y_train, y_test = train_test_split(dfSerieA_3.drop('resultado',axis=1), \
                                                    dfSerieA_3['resultado'], test_size=0.30, \
                                                    random_state=81)

# Modelos
svc = SVC()
svc.fit(X_train, y_train)
predict1 = svc.predict(X_test)
print(classification_report(y_test,predict1))
print(confusion_matrix(y_test,predict1))
print(accuracy_score(y_test,predict1))

rfc = RandomForestClassifier()
rfc.fit(X_train, y_train)
predict2 = rfc.predict(X_test)
print(classification_report(y_test,predict2))
print(confusion_matrix(y_test,predict2))
print(accuracy_score(y_test,predict2))

# Tipo 2 - Dois resultados (Vitória, Não Vence - empate ou derrota)
dfSerieA_3_bin = dfSerieA_3
dfSerieA_3_bin.resultado = dfSerieA_3_bin.resultado.map({'V':1,'E':0,'D':0})
X_train2, X_test2, y_train2, y_test2 = train_test_split(dfSerieA_3_bin.drop('resultado',axis=1), \
                                                    dfSerieA_3_bin['resultado'], test_size=0.30, \
                                                    random_state=81)

svc.fit(X_train2, y_train2)
predict3 = svc.predict(X_test2)
print(classification_report(y_test2,predict3))
print(confusion_matrix(y_test2,predict3))
print(accuracy_score(y_test2,predict3))

rfc.fit(X_train2, y_train2)
predict4 = rfc.predict(X_test2)
print(classification_report(y_test2,predict4))
print(confusion_matrix(y_test2,predict4))
print(accuracy_score(y_test2,predict4))

# Otimizando  as Random Forests para cada tipo
param_grid = {
    "max_depth": [4,5,6],
    "max_features": ["auto","sqrt"],
    "n_estimators": [20,50,100]
}

rfc_cv = GridSearchCV(estimator =rfc, param_grid=param_grid, cv=3)
rfc_cv.fit(X_train, y_train)
print(rfc_cv.best_params_)

rfc_cv.fit(X_train2, y_train2)
print(rfc_cv.best_params_)

# inserindo os parâmetros nos modelos
rfc_bp =RandomForestClassifier(max_features ="sqrt" ,n_estimators=100, max_depth=6)

rfc_bp.fit(X_train, y_train)
predict5 = rfc_bp.predict(X_test)
print(classification_report(y_test,predict5))
print(confusion_matrix(y_test,predict5))
print(accuracy_score(y_test,predict5))

# exportando o classificador
import joblib
joblib.dump(rfc_bp, 'classificador-seriea.joblib')



