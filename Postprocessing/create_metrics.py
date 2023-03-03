#Necessary imports
import pandas as pd
import os 

#Read full csv with DOM sensitivity from year 2010-2020
df = pd.read_csv(os.path.join("Tables", "Pools_DOM_Sensitivity_full.csv"))

#DataFrame for the metrics json
#Statistical measures used - Mean and standard deviation 
result = pd.DataFrame()
result['pool_tc_per_ha_mean'] = df.groupby(["LifeZone", "indicator"])['pool_tc_per_ha'].mean()
result['pool_tc_per_ha_std'] = df.groupby(["LifeZone", "indicator"])['pool_tc_per_ha'].std()
result['pool_tc_sum_mean'] = df.groupby(["LifeZone", "indicator"])['pool_tc_sum'].mean()
result['area_sum_mean'] = df.groupby(["LifeZone", "indicator"])['area_sum'].mean()

result = result.round(4)

#Necessary formatting
#Remove 'forest' for shorter names

#For a dataframe, replace does full replacement searches, unless you turn on the regex switch
#Use regex=True for partial replacements
#For a Series, that is not required
result.reset_index(inplace=True)
result.replace({' forest': '', ' ': '_'}, inplace=True, regex=True)

#Save metrics to json files for dvc
for i in result.index:
    result.loc[i][2:].to_json(os.path.join("Metrics", f"{result.LifeZone[i]}-{result.indicator[i]}.json"))