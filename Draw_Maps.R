# Draw PCR maps
library(rworldmap)
plot(getMap(resolution = "li"))
library(ggplot2)
map.world <- map_data(map = "world")
p1 <- ggplot(map.world, aes(x = long, y = lat, group = group))
p1 <- p1 + geom_polygon() # fill areas
p1 <- p1 + labs(title = "World, plain")
print(p1)
p2 <- ggplot(map.world, aes(x = long, y = lat, group = group, colour = region))
p2 <- p2 + geom_polygon() # fill areas
p2 <- p2 + theme(legend.position="none") # remove legend with fill colours
p2 <- p2 + labs(title = "World, colour borders")
print(p2)
p3 <- ggplot(map.world, aes(x = long, y = lat, group = group, fill = region))
p3 <- p3 + geom_polygon() # fill areas
p3 <- p3 + theme(legend.position="none") # remove legend with fill colours
p3 <- p3 + labs(title = "World, filled regions")
print(p3)
p4 <- ggplot(map.world, aes(x = long, y = lat, group = group, colour = region))
p4 <- p4 + geom_path() # country outline, instead
p4 <- p4 + theme(legend.position="none") # remove legend with fill colours
p4 <- p4 + labs(title = "World, path outlines only")
library(gridExtra)
grid.arrange(p1,p2,p3,p4, nrow = 2, main = "World Maps Sample")

# USA
map.world <- map_data(map = "world")
map_usa <- map_data(map = "usa")
p <- ggplot(map_usa, aes(x = long, y = lat, group = group, colour = region))
p <- p + geom_polygon()
p <- p + theme(legend.position = "none")
p <- p + labs(title = "USA Main Land")
print(p)

# BEST API ever!!
library(ggmap)
library(mapproj)
UCB <- get_map(location = "University of California, Berkeley", zoom = 17, maptype = "terrain")
ucb <- ggmap(UCB)
dat <- read.table(text = "
                  location   long      lat
                  SatherGate 122.25955 37.87
                  Evans      122.25905 37.87375
                  ", header = TRUE)
ucb <- ucb + geom_point(data = dat, aes(x = long, y = lat, shape = location, colour = location), size = 7)
ucb <- ucb + geom_text(data = dat, aes(x = long, y = lat, label = location), hjust = -0.2)
print(ucb)

# Crime Map(Learned to use subset)
str(crime)
violent <- subset(crime, ((offense != "auto theft")
                          & (offense != "theft")
                          & (offense != "burglary")))
violent$offense <- factor(violent$offense, levels = c("robbery","aggravated assault","rape","murder"))
violent <- subset(violent, ((-95.39681 <= lon)
                           & (lon <= -95.34188)
                           & (29.73631 <= lat)
                           & (lat <= 29.784)))
Houston <- get_map(location = "Houston TX", zoom = 14, maptype = "roadmap", color = "bw")
crime_map <- ggmap(Houston)
crime_map <- crime_map + geom_point(data = violent, aes(x = lon, y = lat, size = offense, colour = offense))
print(crime_map)
crime_map <- crime_map + theme( legend.position = c(0.0, 0.7) 
                              , legend.justification = c(0, 0)
                              , legend.background = element_rect(colour = F, fill = "white")
                              , legend.key = element_rect(fill = F, colour = F)
                              , panel.grid.major = element_blank()
                              , panel.grid.minor = element_blank()
                              , axis.text = element_blank()
                              , axis.title = element_blank()
                              , axis.ticks = element_blank())
print(crime_map)
# Density Plot
crime_den <- ggmap(Houston)
overlay <- stat_density2d(data = violent, aes(x = lon, y = lat, fill = ..level.., alpha = ..level..), size = 2, bins = 4, geom = "polygon")
crime_den <- crime_den + overlay
crime_den <- crime_den + scale_fill_gradient("Violent\nCrime\nDensity") # add title for scaling bins
crime_den <- crime_den + scale_alpha(range = c(0.4, 0.75), guide = FALSE)
crime_den <- crime_den + guides(fill = guide_colorbar(barwidth = 1.5, barheight = 10))
print(crime_den)

# By month

crime_den <- crime_den + facet_wrap(~ month, nrow = 4)
print(crime_den)

# SF example
SF <- get_map("San Francisco", zoom = 14, maptype = "roadmap", color = 'color')
crimemap_SF <- ggmap(SF)
print(crimemap_SF)
SF_crime <- read.csv("~/BigData/SF_crime.csv"
                                , header = FALSE, skip = 11, stringsAsFactors = FALSE
                                , col.names = c("Type", "Location", "City", "Date"))
# Add week transformation
day.temp <- weekdays(as.Date(SF_crime$Date, format = "%m/%d/%Y %H:%M"))
SF_crime$day <- factor(day.temp, levels = rev(unique(day.temp)), ordered = TRUE)
# Get geolocation
SF_address <- paste(SF_crime$Location, SF_crime$City)
Geocode <- geocode(SF_address)
SF_crime <- cbind(SF_crime, Geocode)

# Draw the point
crimemap_SF <- crimemap_SF + geom_point(data = SF_crime, aes(x = lon, y = lat))
print(crimemap_SF)

# Density map
overlay_SF <- stat_density2d(data = SF_crime, aes(x = lon, y = lat, fill = ..level.., alpha = ..level..), size = 2, bins = 4, geom = "polygon")
crimeden_SF <- ggmap(SF)
crimeden_SF <- crimeden_SF + overlay_SF
crimeden_SF <- crimeden_SF + scale_alpha(range = c(0.4, 0.75), guide = FALSE)
crimeden_SF <- crimeden_SF + labs(title = "San Francisco Downtown Crime Density Plot")
print(crimeden_SF)

crimeden_SF_week <- crimeden_SF + facet_wrap(~ day, nrow = 2)
crimeden_SF_week <- crimeden_SF_week + labs(title = "San Francisco Downtown Crime Density Plot")
print(crimeden_SF_week)
