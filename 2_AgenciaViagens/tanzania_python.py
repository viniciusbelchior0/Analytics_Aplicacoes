import pandas as pd
import numpy as np
from pandas-profiling import ProfileReport

tanzania = pd.read_csv("/content/tanzania_train.csv")

countries = pd.read_csv("/content/countries.csv", sep=";")

#Semelhante ao dfSummary(summarytools) do R
profile = ProfileReport(tanzania)
profile.to_file("profile.html") # gera um arquivo HTML

tanzania.head()

tanzania.info()

#checando colunas com NA - proporções
na_cols = [col for col in tanzania if (tanzania[col].isna().sum()>0)]
tanzania[na_cols].isna().mean().sort_values(ascending =False).mul(100)

#checando valores absolutos

tanzania = tanzania.dropna(subset=['total_female','total_male'])

tanzania['travel_with'] = tanzania['travel_with'].fillna("Unknown")

tanzania['most_impressing'] = tanzania['most_impressing'].fillna("Unknown")

# checando se os NAs se mantiveram
na_cols = [col for col in tanzania if (tanzania[col].isna().sum()>0)]
tanzania[na_cols].isna().mean().sort_values(ascending =False).mul(100)

tanzania = tanzania.assign(total_people = tanzania['total_female']+tanzania['total_male'], \
                      total_night = tanzania['night_mainland']+tanzania['night_zanzibar'])

tanzania = tanzania.assign(propFemale = tanzania['total_female']/tanzania['total_people'], \
                           propMainland = tanzania['night_mainland']/tanzania['total_night'])

tanzania = pd.merge(tanzania, countries, how = 'left',on='country')

tanzania.info()

tanzania = tanzania.drop(columns = ['ID','country','purpose','payment_mode','country_corrected','continent','most_impressing'])

"""# Modelando"""

tanzania.package_transport_int = tanzania.package_transport_int.map({'Yes':1, 'No':0})
tanzania.package_accomodation = tanzania.package_accomodation.map({'Yes':1, 'No':0})
tanzania.package_food = tanzania.package_food.map({'Yes':1, 'No':0})
tanzania.package_transport_tz = tanzania.package_transport_tz.map({'Yes':1, 'No':0})
tanzania.package_sightseeing = tanzania.package_sightseeing.map({'Yes':1, 'No':0})
tanzania.package_guided_tour = tanzania.package_guided_tour.map({'Yes':1, 'No':0})
tanzania.package_insurance = tanzania.package_insurance.map({'Yes':1, 'No':0})

tanzania_train = pd.get_dummies(tanzania, drop_first = True)

tanzania_train.info()

"""####1.1 - Linear Regression"""

from sklearn.model_selection import train_test_split
from sklearn.linear_model import LinearRegression
from sklearn.metrics import mean_squared_error, r2_score

X = tanzania_train.drop(columns = ['total_cost'])
y = tanzania_train['total_cost']

lreg = LinearRegression().fit(X,y)

print('R-squared score (training): {:.3f}'
.format(lreg.score(X, y)))
print('R-squared score (test): {:.3f}'
.format(lreg.score(X, y)))

lreg_pred = lreg.predict(X)

lreg_pred

# Commented out IPython magic to ensure Python compatibility.
print('Mean squared error: %.2f'
#       % mean_squared_error(lreg_pred, y))

# Commented out IPython magic to ensure Python compatibility.
print('Coefficient of determination: %.2f'
#       % r2_score(lreg_pred, y))

"""###3. StatsModel"""

import statsmodels.api as sm
from scipy import stats

X2 = sm.add_constant(X)
est = sm.OLS(y, X2)
est2 = est.fit()
print(est2.summary())
