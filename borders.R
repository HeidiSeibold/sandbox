library("cshapes")
library("ggplot2")
library("animation")

draw_world_map <- function(date = as.Date("1980-1-1")) {
  
  world <- cshp(date = date)
  
  # world.df = fortify(world, region="SP_ID")
  # names(world.df)[6] <- "SP_ID"
  # 
  # world.df <- join(world.df, world@data)
  
  p <- ggplot() + 
    geom_polygon(data = world, 
                 aes(x = long, y = lat, 
                     group = group), color = "white") +
    theme_dark() + ggtitle(date) + coord_quickmap() 
  
  print(p)
}
  


loop_dwm <- function(dates) {
  lapply(dates, draw_world_map)
}

years <- seq(1955, 2015, by = 10)
datechar <- paste0(years, "-1-1") 
dates <- as.Date(datechar)
saveGIF(loop_dwm(dates), interval = .5, movie.name="borders.gif")

draw_world_map(dates[3])
