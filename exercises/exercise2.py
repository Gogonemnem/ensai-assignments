# Exercise 2
import pandas as pd
import matplotlib.pyplot as plt


df = pd.read_csv("stats-collecte-dechets.csv", delimiter=";")
df = df.astype({'ANNEE':'int'})
df = df.sort_values('ANNEE')


plt.figure(1, figsize=(14, 9))
plt.suptitle("Waste Collection Analysis in Rennes Métropole")

plt.subplot(2,2,1)
plt.plot(df.ANNEE, df.POPULATION, color="red")
plt.title("Population Evolution of Rennes Métropole")
every_other_year = [year for year in df.ANNEE if year % 2 == 0]
plt.xticks(every_other_year)

plt.subplot(2,2,2)
plt.bar(df.ANNEE, df.COLL_OM_HAB, color="grey", edgecolor="blue", label="Household Waste")
plt.bar(df.ANNEE, df.COLL_DMREC_HAB, color="yellow", edgecolor="green", label="Recyclable Waste", bottom=df.COLL_OM_HAB)
plt.ylabel("Weight in kg per inhabitant") 
plt.title("Evolution of Household and Recyclable Waste per Inhabitant")
plt.xticks(every_other_year)
plt.legend()

plt.subplot(2,2,3)
plt.bar(df.ANNEE, df.COLL_DECHETS_ENS, color="blue")
plt.ylim([x * 10_000 for x in [17, 23]])
plt.title("Total Weight of Collected Waste in Tons")
plt.xticks(every_other_year)

plt.subplot(2,2,4)
df_2021 = df.query("ANNEE == 2021")
waste_2021 = df_2021.loc[:, ~df_2021.columns.isin(["ANNEE", "POPULATION", "COLL_DECHETS_ENS"])]
waste_labels = ["Household Waste", "Recyclable Waste", "Green Waste", "Other Waste Deposited at Recycling Center"]
plt.pie(waste_2021.values[0], autopct="%.2f%%")
plt.title("Distribution of Waste per Inhabitant in 2021")
plt.legend(labels=waste_labels)

plt.subplots_adjust(wspace=0.5, hspace=0.5)


plt.show()
