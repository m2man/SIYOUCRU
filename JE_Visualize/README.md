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


