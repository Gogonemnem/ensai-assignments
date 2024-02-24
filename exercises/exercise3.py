import pandas as pd

df = pd.read_excel("pyramide2022.xlsx")
df.columns = ["Birthyear", "Age", "Females", "Males", "Total"]
y_ticks = [year for i, year in enumerate(df.Birthyear.values) if i % 10 == 0]
df.at[100, "Birthyear"] = 1922
df.at[100, "Age"] = 100

import matplotlib.pyplot as plt
plt.barh(df.Age, df.Females, color="red")
plt.barh(df.Age, -df.Males, color="blue")
plt.xticks(list(-i for i in reversed(range(0, max(df.Males), 200_000))) + list(range(0, max(df.Females), 200_000)), list(reversed(range(0, max(df.Males), 200_000))) + list(range(0, max(df.Females), 200_000)))
plt.yticks([year for i, year in enumerate(df.Age.values) if i % 10 == 0], y_ticks)
plt.title("Age Pyramid\nFrench Population on January 1, 2023, by Year of Birth")
plt.show()