import os
import numpy as np
import json

# Function definitions
# Converts decimal longitude/latitude to Web Mercator format
def coor_wgs84_to_web_mercator(lon, lat):
    k = 6378137
    x = lon * (k * np.pi/180.0)
    y = np.log(np.tan((90 + lat) * np.pi/360.0)) * k
    return (x, y)

# Objective: create a Json file containing:
# City
# Number of photovoltaic sites
# Number of hydraulic sites
# Number of bioenergy sites
# Number of cogeneration sites
# Converted point x
# Converted point y
# Converted zone x
# Converted zone y
# Only keep data from 2021!

os.chdir("Exercise4_Source")

fp = open("production-annuelle.json", "r", encoding='utf-8')
productions = json.load(fp)

# First structure: dictionary that associates a dictionary with each city name

dicoCity = {}

for prod in productions:
    # Only keep data from 2021
    if prod["annee"] == "2021":

        # Is the city seen for the first time?
        name = prod["nom_commune"]
        if name not in dicoCity.keys():
            mycom = {}
            mycom["city"] = name
            mycom["photo"] = prod.get("nb_sites_photovoltaique_enedis", 0)
            mycom["bio"] = prod.get("nb_sites_bio_energie_enedis", 0)
            mycom["hydrau"] = prod.get("nb_sites_hydraulique_enedis", 0)
            mycom["cogen"] = prod.get("nb_sites_cogeneration_enedis", 0)

            # Retrieve 2D coordinates
            coords = prod["centroid"]
            X, Y = coor_wgs84_to_web_mercator(coords["lon"], coords["lat"])
            mycom["pointx"] = X
            mycom["pointy"] = Y

            # Retrieve zone coordinates
            zone = prod["geom"]["geometry"]["coordinates"][0][0]
            coord = [coor_wgs84_to_web_mercator(c[0], c[1]) for c in zone]
            mycom["zonex"] = [c[0] for c in coord]
            mycom["zoney"] = [c[1] for c in coord]

            dicoCity[name] = mycom

        else:
            mycom = dicoCity[name]
            mycom["photo"] += prod.get("nb_sites_photovoltaique_enedis", 0)
            mycom["bio"] += prod.get("nb_sites_bio_energie_enedis", 0)
            mycom["hydrau"] += prod.get("nb_sites_hydraulique_enedis", 0)
            mycom["cogen"] += prod.get("nb_sites_cogeneration_enedis", 0)

final = list(dicoCity.values())
print(final)

with open("production_2021.json", "w", encoding='utf-8') as jsonFile:
    jsonFile.write(json.dumps(final, indent=4))
