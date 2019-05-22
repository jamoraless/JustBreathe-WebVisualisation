Project 1 - Just Breathe - Developed by Sai Krishan - CS 424 Visualization Analytics - Spring'19

The project aims at analysing the Air Quality data for different states and counties across the 
United States of America from 1980-2018. The project is developed using R, Shiny and 
various R packages and is hosted in shinyapps.io. 

To run the project, download and install R (20178-12-20 Eggshell Igloo) from a site such as https://cran.cnr.berkeley.edu/
Then you should download and install R-Studio (1.1.463) from https://www.rstudio.com/products/rstudio/download/

Launch R Studio once you're done installing it on your machine. 

Now using setwd(), go to the downloaded JustBreathe_SaiKrishnan directory and then using the folder icon on the top load the app.R file.

Then you need to install the packages that are used in the project. You can install them from the R console with the command install.packages()
List of libraries usesd:
shiny - library(shiny)
shinydashboard - library(shinydashboard)
ggplot2 - library(ggplot2)
lubridate - library(lubridate)
DT - library(DT)
grid - library(grid)
leaflet - library(leaflet)
scales - library(scales)
shinycssloaders - library(shinycssloaders)

After installing all the package, Click the 'Run App' on the top right corner to run the project. Click to dropdown to run the porject externally in the web browser.

The project analyses the Air Quality data for different states and counties across the US from 1980-2018.  
Data source - https://aqs.epa.gov/aqsweb/airdata/download_files.html
For Data - Download all the Annual AQI by county files from 1980-2018 (put the files inside root folder)
For Map - Download the aqi_sites file (under the Site Listing link) and put it inside a folder named leaflet/ (create folder inside root folder)
