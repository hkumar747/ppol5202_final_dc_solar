# DC Solar rooftop dashboard
## Final project for PPOL-5202 2023
Repository of code and data for the Shiny dashboard.

### 1. Set working directory to ‘raw_data’. Then run the Python notebook 1_dataset_creation.ipynb.
The Python script begins by setting the working directory and loading solar panel data from CSV files. Geographic coordinates are cleaned and converted to tuples, then to Point geometries for spatial analysis. Building footprint data is loaded and joined with the solar data to associate panels with buildings. Various datasets, including residential, commercial, and other property data, are merged for a comprehensive analysis. Address standardization is performed for accurate data merging, and duplicate entries are processed. Spatial joins with ward and zip code data are conducted, followed by the calculation of solar panel statistics.
The processed data is then saved into CSV files for further analysis.

### 2. Run R Script 2_rds_convert.R:
The R script loads packages for data manipulation, spatial analysis, and visualization.
It reads solar data from CSV files, focusing on geographic levels like wards and zip codes.
The script adjusts solar capacity measurements and standardizes address data.
A custom rounding function is applied to datasets, and spatial geometries are converted for mapping purposes. Spatial dataframes are created and visualized using Leaflet maps. The script concludes by saving spatial and time-series data in RDS format for future use.

### 3.  Export .RDS files to app directory
