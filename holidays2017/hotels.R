library("ggmap")

########### wishlist  
## read wishlist website
sl <- readLines("https://secure.booking.com/mywishlist.html?aid=356980;tflv=0;wl=kVflMX6AP5PY0MnCffl81ZCuZk8&")

## extract part with urls needed
raw <- grep("20rooms.html", sl, value = TRUE)
rawsp <- strsplit(raw, split = ",")[[1]]

## extract part with b_url 
b_url <- grep("b_url", rawsp, value = TRUE)
b_urlsp <- strsplit(b_url, split = c('"'))

## extract link
grep("http", b_urlsp, value = TRUE)
url_wishlist <- sapply(X = b_urlsp, FUN = grep, pattern = "http", value = TRUE)


############ single websites
sites <- lapply(url_wishlist, readLines)

site <- sites[[1]]
grep('"hasMap"', site, value = TRUE)

coord_raw <- sapply(sites, grep, pattern = '"hasMap"', value = TRUE)
cr <- strsplit(coord_raw, split = '&')
coord_mark <- sapply(cr, grep, pattern = "markers=", value = TRUE)
coord_char0 <- sub("^(.+?)7c", "", coord_mark)
coord_char <- strsplit(coord_char0, split = ",")
coord <- t(sapply(coord_char, as.numeric))
colnames(coord) <- c("lat_booking", "long_booking")

addresses_raw <- sapply(sites, grep, pattern = "streetAddress", value = TRUE)
ar <- strsplit(addresses_raw, split = '"')
addresses <- sapply(ar, grep, pattern = ", ", value = TRUE)
towns <- sapply(strsplit(addresses, ","), function(x) {
  x[[length(x) - 1]]
})
gc_addresses <- geocode(addresses)
is_na_add <- is.na(gc_addresses$lon)
gc_towns <- geocode(towns)

gc_addresses[is_na_add, ] <- gc_towns[is_na_add, ]


## get title
title <- sapply(sites, function(site) {
  title_id <- grep("<title>", site) + 1
  gsub(pattern = " - Booking.com", replacement = "", site[[title_id]])
})

hotels <- data.frame(title, url_wishlist, addresses, gc_addresses, 
                     good_address = !is_na_add, coord)
write.csv(hotels, "hotels.csv", row.names = FALSE)
