library("ggmap")

########### wishlist  
## read wishlist website
sl <- readLines("https://secure.booking.com/mywishlist.html?aid=356980;tflv=0;wl=kVflMX6AP5PY0MnCffl81ZCuZk8&")

## extract part with urls needed
raw <- grep("country-house-in-kipen.html", sl, value = TRUE)
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
grep("streetAddress", site, value = TRUE)

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

hotels <- data.frame(url_wishlist, addresses, gc_addresses, good_address = !is_na_add)
write.csv(hotels, "hotels.csv", row.names = FALSE)
