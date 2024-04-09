# Gas Prices Analysis using PySpark and Folium

## Overview

This project leverages PySpark for processing and analyzing gas price data from 2019 to 2023. The analysis includes data preparation, visualization, and modeling to forecast gas prices.

## Project Structure

- **Data Collection**: Download and prepare gas price data from 2019 to 2023.
- **Data Preparation**: Merge gas files, process dates, and prepare geographic data for mapping.
- **Data Visualization**: Visualize the weekly evolution of average gas prices and generate geo heat maps.
- **Price Prediction**: Model and forecast gas prices using machine learning techniques in PySpark.

## Getting Started

To dive into the gas price analysis, follow these simple steps:

1. **Clone the Repository**: 
   Start by cloning the repository to your local machine. This can be done by running the following command in your terminal:
   ```
   git clone https://github.com/RobDrie/IT-Tools-Spark
   ```

2. **Install Dependencies**:
   Navigate to the cloned repository and install the necessary dependencies by running:
   ```
   pip install -r requirements.txt
   ```

3. **Explore the Notebooks**:
   Open the Jupyter notebooks provided in the repository to explore the analysis. These notebooks contain step-by-step instructions and comments for ease of understanding. You can view the notebooks through Jupyter Notebook or JupyterLab by running:
   ```
   jupyter notebook
   ```
   or 
   ```
   jupyter lab
   ```
   This will allow you to interact with the data, visualizations, and models directly.

Feel free to explore the data, tweak the models, and generate your own insights from the gas price data.

## Key Technologies

- PySpark
- Folium
- Seaborn
- Matplotlib
- Geopandas

## Repository Structure

- `Notebooks`: Contains Jupyter notebooks with the complete analysis and visualizations.
- `GasPrices`: The gas price data from 2019 to 2023 along with station and service details.
- `Data`: The geographic data of the departments of France, needed for the generation of the geo heat maps
- `Maps`: Folium-generated HTML files showcasing the geo heat maps for different gas types.

## Analysis Summary

- The project starts by cleaning and transforming the gas price data.
- Visualizations reveal the weekly trend in gas prices and provide a geographical perspective on price variations.
- The modeling phase focuses on predicting gas prices using machine learning, with an emphasis on understanding the accuracy and error distribution of the predictions.

## Usage

Navigate to the `Notebooks` directory to access the Jupyter notebooks. The notebooks explain the steps taken for the analysis.


## Acknowledgments

- Data Source: [rvm-courses/GasPrices](https://github.com/rvm-courses/GasPrices)
- Project Contributors: [List of Contributors](https://github.com/RobDrie/IT-Tools-Spark/graphs/contributors)