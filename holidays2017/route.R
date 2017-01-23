library("plotKML")
library("ggmap")
library("plyr")

### Locations where we want to stay the night
checkpoints <- read.csv("data.csv", stringsAsFactors = FALSE)
cpf <- checkpoints$from
cpt <- checkpoints$to
cpf[cpf == "Selenogorsk"] <- "Selenogorsk, Sankt Petersburg"
cpt[cpt == "Selenogorsk"] <- "Selenogorsk, Sankt Petersburg"
gc <- geocode(cpf)
checkpoints$lon_from <- gc$lon
checkpoints$lat_from <- gc$lat
gc_to <- geocode(cpt)
checkpoints$lon_to <- gc_to$lon
checkpoints$lat_to <- gc_to$lat

### GPX info on the route
gpxfiles <- list.files(pattern = "gpx-track-download")
gpx <- lapply(gpxfiles, readGPX)
track_list <- lapply(seq_len(length(gpxfiles)), function(i) {
  x <- gpx[[i]]
  tr <- x$tracks[[1]][[1]]
  tr$id <- i
  return(tr)
})
track0 <- rbind.fill(track_list)
# reorder so it starts in Helsinki and ends in Tallin
track <- track0[rev(rownames(track0)), ]

### get the base map using a bounding box
box <- c(min(track$lon) - 0.3, min(track$lat) - 0.1, 
         max(track$lon) + 0.5, max(track$lat) + 0.05)
# estonia <- get_map(location = box, source="stamen", maptype="watercolor")
estonia <- get_map(location = box, source="stamen", maptype="toner")
plot_estonia <- ggmap(estonia) 


### plot
plot_route <- plot_estonia + 
  geom_path(data = track, aes(x = lon, y = lat), 
            color = "red", size = 1.5) +
  geom_point(data = checkpoints, aes(x = lon_from, y = lat_from),
             color = "forestgreen", size = 3) +
  geom_label(data = checkpoints, aes(x = lon_from, y = lat_from, label = from), 
            hjust = 0.5, vjust = -0.5)
plot_route

hotels <- read.csv("hotels.csv")
plot_route +
  geom_point(data = hotels, aes(x = lon, y = lat), 
             color = "blue")


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
loc_track <- apply(checkpoints[, c("lon_to", "lat_to")], 1,
                   function(cp) which.min(distm(x = track[ , c("lon", "lat")], 
                                                y = cp)))
checkpoints$loc_track <- unlist(loc_track)
checkpoints$cumdist <- track$cumdist[checkpoints$loc_track]
checkpoints$dist <- c(checkpoints$cumdist[1], diff(checkpoints$cumdist)) 

cbind(checkpoints$route, checkpoints$dist_wb, checkpoints$dist / 1000)


