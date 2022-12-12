## R script for georeferencing workshop 13-12-2022 ##
# questions: r.schalk@uu.nl 

# to run a line or block of code, put your cursor somewhere in it, and press CTRL+ENTER or select "Run" here at the top right ^

# install required packages

install.packages(c("data.table", "leaflet", "viridis", "ggplot2"))

# load packages

library(data.table)
library(viridis)
library(leaflet)
library(ggplot2) # this one is new!

# import dataset from github

migrants <- fread("https://raw.githubusercontent.com/RubenSchalk/historicalmaps/main/data/migrants_geocoded.csv")

# or add your own dataset from your harddisk like this (Windows):
  
  # dataset_name <- fread("C:/Path/To/File/filename.csv") 

# inspect the dataset, what does each line of code show you?

head(migrants)

tail(migrants)

colnames(migrants)

sapply(migrants, class) 

# you can also click on the dataset in the "Data" panel on the top right to inspect it

  # let's see how many coordinates we can use, and what we miss
  # we will use the data.table package to inspect and subset the data. 
  # more on this here: https://cran.r-project.org/web/packages/data.table/vignettes/datatable-intro.html

setDT(migrants)

migrants[!is.na(pob_lat) & !is.na(pob_long), .N] # gives the count of complete coordinates (latitude & longitude)

migrants[!is.na(pob_lat) & !is.na(pob_long), .N] / migrants[, .N] # gives the share of migrants for which we have complete coordinates

migrants[is.na(pob_lat) & is.na(pob_long), .N, list(pob)][order(-N)] # missing coordinates grouped by place of birth in descending order

# let's remove empty cells as NA 

migrants[pob == "", pob := NA,]

migrants <- migrants[!is.na(pob_lat),]

# let's see where they came from

migrants[, .N, list(pob_modern)][order(-N)]

# because we are also interested in their occupations, let's inspect those as well

migrants[!is.na(occupation), .N] # gives the count of migrant occupations

migrants[is.na(hisco), .N, list(occupation)][order(-N)] # gives the share of coded occupations into HISCO (classification scheme)

# ah, we also have empty cells here, let's fix that

migrants[occupation == "", occupation := NA,]

migrants[is.na(hisco), .N, list(occupation)][order(-N)] # check if we've fixed it and see which occupations are not coded still

# before we select an old map, we need to see where our migrants came from
# visualize spatial data the simplest way:

world <- map_data("world")

ggplot() +
    geom_map(data = world, map = world, aes(long, lat, map_id = region), color = "lightgrey", fill = "white", size = 0.2) +
    geom_point(data = migrants, aes(x = pob_long, y = pob_lat), color= "red") 

# let's select European countries instead

some.countries <- c("Austria","Belgium","Bulgaria","Croatia","Cyprus",
                          "Czech Rep.","Denmark","Estonia","Finland","France",
                          "Germany","Greece","Hungary","Ireland","Italy","Latvia",
                          "Lithuania","Luxembourg","Malta","Netherlands","Poland",
                          "Portugal","Romania","Slovakia","Slovenia","Spain",
                          "Sweden", "Norway", "UK", "Russia", "Turkey", "Ukraine", "Belarus",
                          "Moldova")

europe <- map_data("world", region = some.countries)

ggplot() +
  geom_map(data = europe, map = europe, aes(long, lat, map_id = region),     color = "lightgrey", fill = "white", size = 0.2) +
  geom_point(data = migrants, aes(x = pob_long, y = pob_lat), color= "red") +
  scale_x_continuous(limits = c(-25, 50)) +
  scale_y_continuous(limits = c(35, 70))


# now we know what our region of focus is, we move to https://uu.georeferencer.com/compare#  to select a historical map
# go to the landing page of the map by clicking the title in the viewer, make sure you're logged in to georeferencer and copy the XYZ link

# preselected map: https://uu.georeferencer.com/maps/6992ab78-a092-5872-886b-3b5f898f6eb1/
# XYZ link: https://maps.georeferencer.com/georeferences/628a2266-3b45-58b8-a107-ea5f204ccb87/2020-01-27T10:50:16.591365Z/map/{z}/{x}/{y}.png?key=gRNP1HB3RfkLmp7UMOjj

# add this to our Data Environment

map_url <- ("https://maps.georeferencer.com/georeferences/628a2266-3b45-58b8-a107-ea5f204ccb87/2020-01-27T10:50:16.591365Z/map/{z}/{x}/{y}.png?key=gRNP1HB3RfkLmp7UMOjj")

# plot our historical map using Leaflet

leaflet() %>% setView(lng = 15, lat = 45, zoom = 4) %>%
  addTiles(map_url)

# now we'll add our migrant dataset to it  

leaflet(data=migrants) %>% setView(lng = 15, lat = 45, zoom = 4) %>%
  addTiles(map_url) %>%
  addCircleMarkers(lat = ~pob_lat, lng = ~pob_long, color = "black", radius = 5)

# ok, but now we lose sight on migrants from the same location (remember Amsterdam?). Let's fix this by clustering the markers

leaflet(data=migrants) %>% setView(lng = 15, lat = 45, zoom = 4) %>%
  addTiles(map_url) %>%
  addCircleMarkers(lat = ~pob_lat, lng = ~pob_long, color = "red", radius = 5,
                   clusterOptions = markerClusterOptions())

# that's already better! Let's now make the markers display information about the migrants with a recorded occupation

leaflet(data=migrants[!is.na(occupation)]) %>% setView(lng = 15, lat = 45, zoom = 4) %>%
  addTiles(map_url) %>%
  addCircleMarkers(lat = ~pob_lat, lng = ~pob_long, color = "blue", radius = 8,
                   label =  ~occupation,
                   clusterOptions = markerClusterOptions())

# now we'll use other variables to color the markers according to HISCAM (status): more red = higher status

qpal <- colorBin("Reds", migrants$hiscam, bins = 8) # define our color scale

leaflet(data=migrants[!is.na(migrants$hiscam)]) %>% setView(lng = 15, lat = 45, zoom = 4) %>%
  addTiles(map_url) %>%
  addCircleMarkers(lat = ~pob_lat, lng = ~pob_long, color = ~qpal(hiscam), radius = 8,
                   label =  ~hiscam)

# and now with clustering

leaflet(data=migrants[!is.na(migrants$hiscam)]) %>% setView(lng = 15, lat = 45, zoom = 4) %>%
  addTiles(map_url) %>%
  addCircleMarkers(lat = ~pob_lat, lng = ~pob_long, color = ~qpal(hiscam), radius = 8,
                   label =  ~hiscam,
                    clusterOptions = markerClusterOptions())


# we would also like to know their occupations again, but keep their HISCAM score and color

leaflet(data=migrants[!is.na(migrants$hiscam)]) %>% setView(lng = 15, lat = 45, zoom = 4) %>%
  addTiles(map_url) %>%
  addCircleMarkers(lat = ~pob_lat, lng = ~pob_long, color = ~qpal(hiscam),
                   label =  ~as.character(paste0(occupation, ", HISCAM = ", hiscam)), radius = 8,
                   clusterOptions = markerClusterOptions())


# exporting your map and keep it interactive
# note you can also export it as an image using the plot pane, but then you'll lose the labels

install.packages("htmlwidgets")
library(htmlwidgets)

myMap <- leaflet(data=migrants[!is.na(migrants$hiscam)]) %>% setView(lng = 15, lat = 45, zoom = 4) %>%
  addTiles(map_url) %>%
  addCircleMarkers(lat = ~pob_lat, lng = ~pob_long, color = ~qpal(hiscam),
                   label =  ~as.character(paste0(occupation, ", HISCAM = ", hiscam)), radius = 8,
                   clusterOptions = markerClusterOptions())

htmlwidgets::saveWidget(myMap, "C:/FOLDER/FOLDER/index.html", selfcontained = TRUE)   

# this html file will open in your browser for viewing

# if you want to have this as a website, you need to publish it online, for instance using a github repository:
# 1: make a github repo
# 2: change the filename to 'index' if not already done so
# 3: upload it to Github
# 4: go to repository > settings > pages > deploy from branch > main > save
# 5: give it some time and visit your webpage e.g. https://rubenschalk.github.io/onlinemap


# if you want to add more text, figures etc., you need to do all this (text + R code) in a Markdown file
# you can work in markdown files in R studio, and save them as a html. 
# publishing that works the same as the one map
# for instance see here: https://rubenschalk.github.io/historicalmaps/
# and the corresponding markdown file: https://github.com/RubenSchalk/historicalmaps/blob/main/historical_maps.Rmd


# more on markdown here: https://rmarkdown.rstudio.com/ 


# last, if you don't want to use an old map but do want to demonstrate all info for each marker:

install.packages("mapview")
library(mapview)

migrants$lat <- jitter(migrants$pob_lat, factor = 0.8) # mapview has no cluster option so we use jitter to spread the coordinates
migrants$lon <- jitter(migrants$pob_long, factor = 0.8)

mapview(migrants, xcol = "lon", ycol = "lat", crs = 4326)


