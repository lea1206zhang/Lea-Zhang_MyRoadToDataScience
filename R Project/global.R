# Installing package if it is not present already

package_list <- c('readxl','dplyr','ggplot2',
                  'leaflet','plotly','shinythemes',
                  'DT','scales','htmltools',
                  'rgdal')

for (package in package_list) {
  if (!require(package, character.only = T, quietly = T)) {
    install.packages(package, repos = "http://cran.us.r-project.org")
    library(package, character.only = T)
  }
}

# global
library(readxl)
library(dplyr)
library(ggplot2)
library(leaflet)
library(plotly)
library(shinythemes)
library(DT)
library(scales)
library(htmltools)
library(rgdal)

# if you don't have the following datasets in your environment
# excuate the following two lines of codes

alldt <- read.csv(file = 'alldt.csv')
price <- read.csv(file = 'price.csv')
top_n <- read.csv(file = 'top_n.csv')
all_zp <- read.csv(file='all_zp.csv')
alldt$zipcode <- as.factor(alldt$zipcode)
alldt$borough <- as.character(alldt$borough)
alldt$neighbourhood <- as.character(alldt$neighbourhood)
price$time <- as.Date(price$time, format = "%Y-%m-%d")
price$RegionName <- as.factor(price$RegionName)
all_zp$zipcode <- as.factor(all_zp$zipcode)
all_zp$neighbourhood <- as.character(all_zp$neighbourhood)


# read Geojson file
# setwd("to the directory where you put nola_geojson")

map_data <- readOGR("nola_geojson", 'OGRGeoJSON', verbose = F)
# if you cannot input the geojson using the code above and it return error of 
# like this Error in ogrInfo(dsn = dsn, layer = layer, encoding = encoding, use_iconv = use_iconv,  : Cannot open layer
# please try 
# readOGR(dsn ='~/Desktop/apply for job!!!/company/Capital One/airbnb-zillow-data-challenge-master/NYC_Property', layer ="nola_geojson")
# or readOGR(dsn ='~/Desktop/apply for job!!!/company/Capital One/airbnb-zillow-data-challenge-master/NYC_Property/nola_geojson', layer ="nola_geojson")
# it may due to the version of rgdal you have 

# create variables for choice
element = c('Volume', 'Median Price')
borough = c('All',unique(all_zp$neighbourhood))

# # intitialize data frame for donut chart
donutAll = 
  alldt %>%
  group_by(property_type) %>%
  dplyr::summarise(n=n(),price=median(price_new))

donut = 
  alldt %>%
  group_by(borough,property_type) %>% #further bread down by neighborhood
  dplyr::summarise(n=n(),price=median(price_new))

# #initialzie data frame for bar chart
chartZipCodeAll = alldt %>%
  group_by(zipcode) %>%
  dplyr::summarise(n=n(),price=median(price_new),avgPrice=mean(price_new))

chartZipCode = alldt %>%
  group_by(borough,zipcode) %>%
  dplyr::summarise(n=n(),price=median(price_new),avgPrice=mean(price_new))  

# #initialzie data frame for line chart
chartAll <- price

chartAll %>% plot_ly( x = ~time, y = ~price,mode = 'lines',color = ~RegionName) 
plot_ly(price, x = ~time, y = ~price,mode = 'lines',color = ~RegionName)

selectTime = chartAll %>% filter(time < as.Date('2017-05-11'), borough == 'Manhattan') 
plot_ly(selectTime, x = ~time, y = ~price,mode = 'lines',color = ~RegionName)

# a custom table container
sketch = htmltools::withTags(table(
  class = 'display',
  thead(
    tr(
      th(colspan = 8, 'Top Zip Codes for Individual Metric')
    ),
    tr(
      lapply(c('Rank','Annual Return on Investment', 'Breakeven Period', 'Total Number of Reviews'), th)
    )
  )
))

