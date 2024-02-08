from urllib import request
import json
import pandas  as pd
import numpy as np


def access_data_online():
    full_url = "https://data.rennesmetropole.fr/api/records/1.0/search/?dataset=etat-du-trafic-en-temps-reel&rows=3000"
    with request.urlopen(full_url) as response:
        data = json.load(response)
    return data


# Converts decimal longitude/latitude to Web Mercator format
def coor_wgs84_to_web_mercator(lon, lat):
    k = 6378137
    x = lon * (k * np.pi / 180.0)
    y = np.log(np.tan((90 + lat) * np.pi / 360.0)) * k
    return (x, y)


def analyze_data(data):
    # Constructing a dataframe: one column for denomination, one for traffic, one for coordinates
    # Initialize lists for data
    x_coords = []
    y_coords = []
    traffic_statuses = []
    locations = []
    datetimes = []

    # Extract data and coordinates
    for p in data['records']:
        geo_shape = p['fields']['geo_shape']
        traffic = p['fields'].get('trafficstatus', 'Unknown')
        location = p['fields'].get('denomination', 'Unknown')
        datetime = p['fields'].get('datetime', 'Unknown')

        if geo_shape['type'] == 'MultiLineString':
            for line in geo_shape['coordinates']:
                coord = [coor_wgs84_to_web_mercator(c[0], c[1]) for c in line]
                x_coords.append([point[0] for point in coord])
                y_coords.append([point[1] for point in coord])
                traffic_statuses.append(traffic)
                locations.append(location)
                datetimes.append(datetime)

    # Create DataFrame
    traffic_df = pd.DataFrame({
        'x': x_coords,
        'y': y_coords,
        'traffic': traffic_statuses,
        'location': locations,
        'datetime': datetimes
    })

    status_to_num = {'freeFlow': 'green', 'unknown': 'gray', 'heavy': 'orange', 'congested': 'red'}
    traffic_df['traffic_color'] = traffic_df['traffic'].map(status_to_num)
    return traffic_df


def main():
    # To regenerate a file etat-du-trafic
    traffic_data = access_data_online()
    # print(traffic_data['records'][0])
    df_trafic = analyze_data(traffic_data)
    print(df_trafic)


if __name__ == "__main__":
    main()
