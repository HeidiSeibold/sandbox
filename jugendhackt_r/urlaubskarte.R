# Software Paket für huebsche Grafiken laden
library("ggplot2")

# Karte erstellen
karte <- ggplot() + 
  borders("world", colour="gray50", fill="gray50") + 
  coord_quickmap()

# Karte anzeigen
karte


# Urlaubsdaten laden
load("urlaub.rda")

# Urlaubsdaten auf die Karte plotten
karte_urlaub <- karte + 
  geom_point(data = my_visits, aes(x = lon, y = lat))

# Urlaubskarte anzeigen
karte_urlaub




