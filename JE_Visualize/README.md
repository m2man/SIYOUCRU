# Shiny JE Interactive Map

## Summary
This project is to create an interactive map of Japanese Encephalitis. The application layout includes 6 tabs in which 2 of them is not finished yet (Methodology and About tab). The 4 remaining tabs are described as followed:
1. **FOI Map**: Visualize the modelled FOI distribution of every study collected from the systematic review.
2. **Estimates of Vaccine Coverage**: Visualize the total population of each endemic regions and their vaccinated people (coverage) by age and by year
3. **Estimates of JE Burden**: Visualize the estimated burden (cases, deaths) and averted people (cases, deaths) in a vaccination scenario. Also illustrate the number of vaccinated people. The graphs will be ploted by region, agegroup, and by year.
4. **Burden Map**: Visualize the estimated cases at regions mentioned in the previous estimation study in 2 scenario (No vaccination, Past vaccination). The map is also illustrated by years.

The structure of this folder is:
1. app.R: main script to create Shiny application
2. **_Data_** folder: All of data used in **_app.R_** script
3. **_Supporting_Function_**: Functions used to create data in **_Data_** folder


## Core Function
Indeed, we only have 1 R code file (**app.R**) including 3 main parts: Read and processing data, Design UI, and Internal server to run functions.

Regarding **_Data/_** folder, there are following files in the folder:
- **Burden_Cases_Map.rds**: Spatial polygon dataframe (This type of variable is usually used for a Map) containing data about cases in 2 scenarios: No vaccination and Past vaccination. It includes 30 endemic regions and last 66 years from 1950 to 2015. This data is used in Tab 4 in the Shiny application.
- **Burden_IR_Map.rds**: Same as above but this is Incidence rate (**_however we dont use this for mapping_**)
- **lambda_every_catchment_areas.rds**: A dataframe containing catalytic modelled FOI posterior of each study from systemtic review. This data is used in Tab 1.
- **marker_data.rds**: Some note about endemic regions which will be showed on the map in Tab 1.
- **shapefiles_FOI_data_merged_region.rds**: Spatial polygon dataframe containing the coodinates of endemic regions and the mean FOI posterior of each region that will be used to create the FOI map in Tab 1.
- **Pop_Total.rds**: A dataframe with the dimension of 3000 x 67 containing the population of entire endemic regions. The rows are 30 regions x 100 age group for each region (age 0 to age 99). The columns are 1 column for region + 66 year columns from 1950 to 2015. This dataframe combined below with Pop_UnVaccine dataframe can produce vaccinated people dataframe and will be used in Tab 2.
- **Pop_UnVaccine.rds**: Same as above but containing data of unvaccinated people. This will be used in Tab 2.
- **vac_cases_agegroup.rds**: a list containing the cases, in Past Vaccination scenario, of 2 type of age groups: children (age 0 to age 14), and adult (age 15 to age 99). The list includes 30 endemic regions. Each endemic region will be a matrix of 1600 x 132 values. 1600 rows for 1600 simulation of catalytic modelled FOI posterior. 132 columns are about 66 years x 2 age groups. This will be used in Tab 3.
- **no_vac_cases_agegroup.rds**: Same as above but cases in No vaccination scenario.
- **vac_deaths_agegroup.rds**: Save as above but deaths in Past vaccination scenario.
- **no_vac_deaths_agegroup.rds**: Save as above but deaths in No vaccination scenario.

## Supporting functions
The supporting functions are in **_Supporting_Functions/_** folder. These functions are used to create the data that us neccessary for Shiny (as discussed above). There are **_Data/_** folder in the supporting folder that will store the original data (but really intensive), and **_Generate/_** folder that contains the result after running supporting functions.

There are 3 funtions in this folder:
1. **Reduce_size_shpmap.R**
- Use this to create data needed in Tab 1(**shapefiles_FOI_data_merged_region.rds**, **marker_data.rds**). Basically it will split the original shapefiles **shapefiles_FOI_data_merged_region.rds** in **_Supporting_Functions/Data/_** folder into 2 above distinct file in order to reduce the size.
- Use this to create data needed in Tab 2(**Pop_Total.rds**, **Pop_UnVaccine.rds**). It will read **Naive_pop_24ende_1950_2015.rds** and **After_vac_pop_24ende_or_1950_2015.rds** (both are stored in **_Supporting_Functions/Data/_** folder), then extract data in endemic regions and calculate Naive population and unvaccinated people.
2. **Reduce_size_burdenmap.R**
- Use this to create **Burden_Cases_Map.rds**
- Also can use this to calculate Incidence Rate or Deaths (instead of Cases)
- Need to have data from Tab 2, and Tab 3 (as described above) and a spatial polygon dataframe indicating the endemic regions 
3. **Reduce_size_cases_deaths.R**
- Use this to create burden agegroup data in Tab 3 (**vac_cases_agegroup.rds**, **no_vac_cases_agegroup.rds**, **vac_deaths_agegroup.rds**, **no_vac_deaths_agegroup.rds**)
- Need to calculate burden in agegroup since the original data about burden at each agegroup is too intensive  (and cannot upload on Github)
