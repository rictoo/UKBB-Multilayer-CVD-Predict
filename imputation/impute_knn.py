import pandas as pd
import time

import numpy as np
import pandas as pd
from sklearn.preprocessing import LabelEncoder
from sklearn.preprocessing import OneHotEncoder
from sklearn.preprocessing import OrdinalEncoder
from sklearn.impute import KNNImputer
from sklearn.experimental import enable_iterative_imputer
from sklearn.impute import IterativeImputer
from sklearn.ensemble import ExtraTreesRegressor

input_filename = 'sens_nonimputed.csv'
output_filename = 'sens_imputed.csv'

#df = pd.read_csv('/rds/general/user/dba22/home/tds/Group1/tds-proj/files/data_iter9_incidentCVD_scored_cc.csv')
df = pd.read_csv('/rds/general/user/dba22/home/tds/Group1/tds-proj/final/' + input_filename)

outcome_tte = df['outcome_tte']
outcome_incident_case = df['outcome_incident_case']
eid = df['eid']

iter9_predictors = df.drop(columns=['eid', 'outcome_tte', 'outcome_incident_case'])

#iter9_predictors_subset = iter9_predictors.iloc[1:20000]
iter9_predictors_subset = iter9_predictors

factor_cols = ['bl_sex.0.0', 'fhx_heartdisease.0.0', 'fhx_stroke.0.0', 'fhx_diabetes.0.0',
               'be_alcohol_freq.0.0_cat', 'be_days_weeks_walking.0.0_cat', 'be_days_weeks_mod_phys.0.0_cat',
               'be_days_weeks_vig_phys.0.0_cat', 'se_highest_quali.0.0_cat', 'be_smoking_status.0.0_cat',
               'bl_ethnic_bckg.0.0.White_cat', 'be_sleep_score.0.0', 'be_diet_score']

# convert factor columns to 'category' data type
for col in factor_cols:
    iter9_predictors_subset[col] = iter9_predictors_subset[col].astype('category')

fixt = iter9_predictors_subset

st = time.time()
encoder = OneHotEncoder(sparse=False, handle_unknown='ignore')

#categorical_columns = [col for col in fixt.columns if fixt[col].nunique() >= 3 and fixt[col].dtype == 'category']
categorical_columns = ['se_highest_quali.0.0_cat']
onehot_encoded = encoder.fit_transform(fixt[categorical_columns])

encoded_df = pd.DataFrame(onehot_encoded, columns=encoder.get_feature_names_out(categorical_columns))

cols_to_drop = []

for col in encoded_df.columns:
    if col.endswith("_nan"):
        original_col_name = col[:-4]
        nan_rows = encoded_df[encoded_df[col] == 1].index
        related_cols = [c for c in encoded_df.columns if c.startswith(original_col_name)]
        encoded_df.loc[nan_rows, related_cols] = np.nan
        cols_to_drop.append(col)

# Drop the _nan column
encoded_df = encoded_df.drop(columns=cols_to_drop)

df_catdropped = fixt.drop(columns=categorical_columns, inplace=False)

df_catdropped.reset_index(inplace=True)
encoded_df.reset_index(inplace=True)
df_encoded = pd.concat([df_catdropped, encoded_df], axis=1)

#df_encoded['bl_ethnic_bckgd.0.0_cat'] = df_encoded['bl_ethnic_bckgd.0.0_cat'].map({'Other': 0, 'White': 1})

df2 = df_encoded #.iloc[:, :-4]

df2
cols = df2.columns
num_cols = df2.select_dtypes(include=np.number).columns

cat_cols = list(set(cols) - set(num_cols))

df2[cat_cols] = df2[cat_cols].astype('object')

#df2['be_smoking_status.0.0_cat'] = df2['be_smoking_status.0.0_cat'].replace({'Never': 0, 'Previous': 1, 'Current': 2})
#df2['be_alcohol_freq.0.0_cat'] = df2['be_alcohol_freq.0.0_cat'].replace({'Non-drinker': 0, 'Social drinker': 1, 'Moderate drinker': 2, 'Daily drinker': 3})

# instantiate both packages to use
encoder = OrdinalEncoder()
imputer = IterativeImputer(ExtraTreesRegressor())

def encode(data):
    '''function to encode non-null data and replace it in the original data'''
    #retains only non-null values
    nonulls = np.array(data.dropna())
    #reshapes the data for encoding
    impute_reshape = nonulls.reshape(-1,1)
    #encode date
    impute_ordinal = encoder.fit_transform(impute_reshape)
    #Assign back encoded values to non-null values
    data.loc[data.notnull()] = np.squeeze(impute_ordinal)
    return data

#create a for loop to iterate through each column in the data
for columns in cat_cols:
    encode(df2[columns])

# Detect columns with missing values
df2 = df2.drop('index', axis=1, errors='ignore')

missing_cols = df2.columns[df2.isnull().any()].tolist()

# Separate input and output features
input_cols = [col for col in df2.columns if col not in missing_cols]
#input_cols = [col for col in input_cols if col != 'index']
output_cols = missing_cols

# Apply KNN imputation
imputer = KNNImputer()
df_imputed = pd.DataFrame(imputer.fit_transform(df2[output_cols + input_cols]), columns=output_cols + input_cols)

df_imputed = df_imputed[df2.columns]

df_imputed = df_imputed.assign(eid=eid, outcome_incident_case=outcome_incident_case, outcome_tte=outcome_tte)

# Output the imputed dataset
df_imputed.to_csv('/rds/general/project/hda-22-23/live/TDS/Group1/tds-proj/final/' + output_filename, index=False)

# end the timer

et = time.time()
elapsed_time = et - st
print('Execution time:', elapsed_time, 'seconds')

