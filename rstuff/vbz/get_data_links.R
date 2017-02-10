library("rvest")

get_data_links <- function(url = "https://data.stadt-zuerich.ch/dataset/vbz_fahrzeiten_ogd_2016") {
  
  ## read website
  pg <- read_html(url)
  
  ## extract links from website
  all_links <- pg %>% html_nodes("a") %>% html_attr("href")
  
  ## get data links (specific pattern)
  data_links <- grep("https://data.stadt-zuerich.ch/dataset/.*.csv", all_links, value = TRUE)
  
  ## links to delay data
  is_delay_link <- grepl("/fahrzeitensollist", data_links)
  delay_links <- data_links[is_delay_link]
  
  ## links to other data
  other_links <- data_links[!is_delay_link]
  
  return(list(delay = delay_links, other = other_links))
  
}

links_2015 <- get_data_links("https://data.stadt-zuerich.ch/dataset/vbz_fahrzeiten_ogd_2015")
links_2016 <- get_data_links("https://data.stadt-zuerich.ch/dataset/vbz_fahrzeiten_ogd_2016")
links_2017 <- get_data_links("https://data.stadt-zuerich.ch/dataset/vbz-fahrzeiten-ogd")
