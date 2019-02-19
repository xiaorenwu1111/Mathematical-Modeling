import csv
import numpy as np
import pandas as pd

f = open('C:\competition\第十五届“华为杯”中国研究生数学建模竞赛—C题\附件1\附件1.csv','rb')

data = pd.read_csv(f)

def count(year,country):
    count = 0
    for i in range(data.iloc[:,0].size):
        if data.iyear[i]==year and data.country_txt[i]==country and data.success[i]==1:
            count+=1
    return count

rows1 = []
for i in range(1998,2018):
        row = (i,'India',count(i,'India'))
        rows1.append(row)

rows2 = []
for i in range(1998, 2018):
    row = (i, 'China', count(i, 'China'))
    rows2.append(row)

rows3 = []
for i in range(1998, 2018):
    row = (i, 'Brazil', count(i, 'Brazil'))
    rows3.append(row)

rows4 = []
for i in range(1998, 2018):
    row = (i, 'Russia', count(i, 'Russia'))
    rows4.append(row)


with open("C:\competition\\India.csv","w",newline='') as csvfile:
    writer = csv.writer(csvfile)
    writer.writerow(["iyear","country","count"])
    writer.writerows(rows1)

with open("C:\competition\\China.csv","w",newline='') as csvfile:
    writer = csv.writer(csvfile)
    writer.writerow(["iyear","country","count"])
    writer.writerows(rows2)

with open("C:\competition\\Brazil.csv","w",newline='') as csvfile:
    writer = csv.writer(csvfile)
    writer.writerow(["iyear","country","count"])
    writer.writerows(rows3)

with open("C:\competition\\Russia.csv","w",newline='') as csvfile:
    writer = csv.writer(csvfile)
    writer.writerow(["iyear","country","count"])
    writer.writerows(rows4)


