import seaborn as sns 
import numpy as np
import matplotlib as plt
import sklearn
import pandas as pd

first_df = pd.read_csv("FINAL_RUNS/2025-03-14_11_33_FIRST/FIRST_result.csv")
second_df = pd.read_csv("FINAL_RUNS/2025-03-20_21_24_SECOND/SECOND_result.csv")
third_df = pd.read_csv("FINAL_RUNS/2025-03-17_19_43_THIRD/THIRD_result.csv")
fourth_df = pd.read_csv("FINAL_RUNS/2025-03-24_10_54_FOURTH/FOURTH_result.csv")
print(first_df.columns)

#np.mean(first_df[""])