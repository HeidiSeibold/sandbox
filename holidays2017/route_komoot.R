library("plotKML")
library("ggmap")
library("plyr")

### Locations where we want to stay the night
hotels <- read.csv("hotels.csv", stringsAsFactors = FALSE)


### Read GPX
track0 <- readGPX("2017-05-07_14112579_von-helsinki-tallinn-mit-hotels_export.gpx")
track <- track0$tracks[[1]][[1]]

### get the base map using a bounding box
box <- c(min(track$lon) - 0.3, min(track$lat) - 0.1, 
         max(track$lon) + 0.5, max(track$lat) + 0.05)
# estonia <- get_map(location = box, source="stamen", maptype="watercolor")
estonia <- get_map(location = box, source="stamen", maptype="toner")
plot_map <- ggmap(estonia) 
plot_map + 
  geom_path(data = track, aes(x = lon, y = lat), 
            color = "red", size = 1.5) +
  geom_point(data = hotels, aes(x = long_booking, y = lat_booking),
             color = "forestgreen", size = 3) +
  geom_label(data = hotels, aes(x = long_booking, y = lat_booking, label = title), 
             color = "blue", hjust = 0.5, vjust = -0.5)





### calculate distances in km
library("geosphere")

a <- track[, c("lon", "lat")]
b <- a[c(2:NROW(a), 1), ]
dab <- cbind(a = a, b = b)

track$dist <- apply(dab, 1, function(x) distm(x[c("a.lon", "a.lat")], 
                                              x[c("b.lon", "b.lat")]) )
track$dist[which.max(track$dist)] <- NA
track$cumdist <- cumsum(track$dist)

# total distance (without the part from Tallinn to Helsinki)
sum(track$dist, na.rm = TRUE) / 1000
tail(track$cumdist) / 1000


# distance every day
loc_track <- apply(hotels[, c("long_booking", "lat_booking")], 1,
                   function(cp) which.min(distm(x = track[ , c("lon", "lat")], 
                                                y = cp)))
hotels$loc_track <- unlist(loc_track)
hotels <- hotels[order(hotels$loc_track), ]
hotels$cumdist <- track$cumdist[hotels$loc_track]
hotels$dist <- c(hotels$cumdist[1], diff(hotels$cumdist)) 

cbind(hotels$title, hotels$cumdist / 1000, hotels$dist / 1000)

write.csv(hotels, file = "hotels_with_distance.csv")

plot_map + 
  geom_path(data = track, aes(x = lon, y = lat), 
            color = "red", size = 1.5) +
  geom_point(data = hotels, aes(x = long_booking, y = lat_booking),
             color = "forestgreen", size = 3) +
  geom_label(data = hotels, aes(x = long_booking, y = lat_booking, label = round(dist/1000)), 
             color = "blue", hjust = 0.5, vjust = -0.5)
