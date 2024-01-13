# Project Repository

This repository will contain **ALL** the code, analysis, document and presentation for our group project.

**To run a code file, set the corresponding folder as the Working Directory.**

- Codes  
 This folder contains all the .R files written for scrapping, plotting etc.
  - **tokyo.R** - code of intial attempt of scrapping (for a single Olympic).
  - **Data.R** - code of function to scrap data from all the Olympics.
  - **GDP_SCRAPING.R** - code to scrap data on GDP.
  - **plots.R** - code to produce all kinds of plots done in this project\
  
- Data  
 This folder contains all the .Rdata files
  - **1984** - **2020** - these files contain data of Olympics in corresponding years.
  - **all_data_list** - a list contains all the dataframe above.
  - **full_df** - all data of 10 Olympics in one dataframe.
  - **GDP_COUNTRY_CODE** - data of GDP of each countries in those years.
  - **GDP_final_data** - yearwise data of GDP of each countries with their latitude and longitude and number of medals won by them.
- Presentation  
 This folder contains the PPT file.
- Report  
 This folder contains the .rmd file (Project_Report.rmd) and the PDF (Project_Report.pdf) of the project report and the required .Rdata files to run the .rmd file.
  - The **'athleteEvents.csv'** file contains data on all the modern Olympics.
  - **flow.jpeg** is a picture file pasted in the project report.
- Shiny  
 This folder contains the .R file named **Olympic_Shiny_app.R** and the required .Rdata files to run the .R file.
 
- Libraries Required  
  - readr
  - ggplot2
  - GGally
  - plotly
  - dplyr
  - rvest
  - shiny
  - shinythemes
  - tidyverse
  - eeptools
  - lubridate
  - stringi
  - data.table
  - patchwork
  - maps

