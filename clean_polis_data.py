import os
import pandas as pd

polis: pd.DataFrame = pd.read_excel("input_data/DataFile_MultipleMyeloma_Polis.xlsx",
                                    sheet_name="DataFile_MultipleMyeloma")

polis["Birth Date"] = pd.to_datetime(polis["Birth Date"], errors='coerce')
polis["YearBirth"] = polis["Birth Date"].dt.year
polis["MonthBirth"] = polis["Birth Date"].dt.month
polis["DayBirth"] = polis["Birth Date"].dt.day

polis["Date of Diagnosis"] = pd.to_datetime(polis["Date of Diagnosis"])
polis["YearOfDiagnosis"] = polis["Date of Diagnosis"].dt.year
polis["MonthOfDiagnosis"] = polis["Date of Diagnosis"].dt.month
polis["DayOfDiagnosis"] = polis["Date of Diagnosis"].dt.day

polis["Date of initial diagnosis"] = pd.to_datetime(polis["Date of initial diagnosis"])
polis["YearInitDiagnosis"] = polis["Date of initial diagnosis"].dt.year
polis["MonthInitDiagnosis"] = polis["Date of initial diagnosis"].dt.month
polis["DayInitDiagnosis"] = polis["Date of initial diagnosis"].dt.day

polis["Date of last contact"] = pd.to_datetime(polis["Date of last contact"])
polis["YearLastContact"] = polis["Date of last contact"].dt.year
polis["MonthLastContact"] = polis["Date of last contact"].dt.month
polis["DayLastContact"] = polis["Date of last contact"].dt.day

polis.to_csv("input_data/data_mm_polis.csv", index=False, header=True, sep=',')
print("Done")
