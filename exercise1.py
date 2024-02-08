import matplotlib.pyplot as plt

#Exercise 1
#Average temperatures in Rennes
months = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]

T_avg = [5.8,5.9,8.1,10.6,13.8,17.1,18.7,18.4,16.3,13.1,9,6.3]
T_avg_min = [3.1,2.8,4.3,6.2,9.5,12.5,14.3,14.1,12.2,9.8,6.3,3.7]
T_avg_max = [8.4,9.2,12.1,15,18,21.5,23.2,22.9,20.7,16.6,11.8,8.]

month_initials = [month[0] for month in months]

plt.plot(months, T_avg, color="red")
plt.plot(months, T_avg_min, color="blue")
plt.fill_between(months, T_avg, T_avg_min, color="blue", alpha=0.5)
plt.plot(months, T_avg_max, color="orange")
plt.fill_between(months, T_avg, T_avg_max, color="orange", alpha=0.5)
plt.xticks(months, month_initials)
plt.ylabel("Temperature")
plt.title("Average Temperatures in Rennes")
plt.legend()
plt.show()
