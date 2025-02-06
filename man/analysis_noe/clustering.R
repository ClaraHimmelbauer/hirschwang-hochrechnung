# clustering -------------------------------------------------------#

packages <- c("sf", "readxl", "dplyr", "leaflet", "leafem", "htmltools", "htmlwidgets", "xml2")
lapply(packages, library, character.only=TRUE)

rm(list = ls()); gc()

#-------------------------------------------------------------------#

shape <- st_read("X:/Geodaten/aaa_Österreich/Verwaltungsgrenzen_vereinfacht", "Gem_simple")
shape <- shape[shape$BL == 3, ]

historisch <- read_xlsx("wahlergebnisse.xlsx")

names_new <- paste0(colnames(historisch)[c(5:11, 13:19, 21:28)], "_proz")
historisch[names_new] <- NA

historisch[, 29:35] <- historisch[, 5:11] / historisch$wber18 * 100
historisch[, 36:42] <- historisch[, 13:19]/ historisch$wber19 * 100
historisch[, 43:50] <- historisch[, 21:28]/ historisch$wber22 * 100

# nur mit ltw und NRW --> für heute mal mein bevorzugtes Clustering
# es macht Sinn, dass mehr Gemeinden im VP-Cluster sind
# macht auch Sinn, dass die Gemeinden nicht (ca) halbe-halbe verteilt sind
distanz <- as.matrix (dist (historisch [, 29:42], method="euclidean"))
h1 <- hclust (as.dist(distanz), method="ward.D", members=NULL)
plot (h1)
cluster2 <- cutree (h1, k=2)
gemc <- cbind (historisch, cluster2)
table(gemc$cluster2)
round (aggregate (gemc [, 29:42], list(gemc$cluster2), mean), digits = 1)

graph <- left_join(shape, gemc, by = c("GKZ" = "gkz"))
leaflet(graph) %>%
  addProviderTiles(providers$OpenStreetMap) %>%
  addPolygons(weight = 0, opacity = 0, fillOpacity = 1,
              fillColor = ~colorNumeric("YlOrRd", graph$cluster2)(graph$cluster2))

# die meisten der Gemeinden im SP-Cluster haben zwischen 1.000 und 2.000 Einwohner
# also sollte es sich bei der Hochrechnung auch mit ausgezählten Gemeinden ausgehen
hist(gemc$wber22[gemc$cluster2 == 1], breaks = 50)
export <- gemc[, c("gem", "cluster2")]
write.csv2(export, "cluster2.csv", row.names = F)

# ltw und nrw ohne NW
distanz <- as.matrix (dist (historisch [, c(29:34, 36:41)], method="euclidean"))
h1 <- hclust (as.dist(distanz), method="ward.D", members=NULL)
plot (h1)
cluster2 <- cutree (h1, k=2)
gemc <- cbind (historisch, cluster2)
table(gemc$cluster2)
round (aggregate (gemc [, c(29:34, 36:41)], list(gemc$cluster2), mean), digits = 1)

graph <- left_join(shape, gemc, by = c("GKZ" = "gkz"))
leaflet(graph) %>%
  addPolygons(weight = 0, opacity = 0, fillOpacity = 1,
              fillColor = ~colorNumeric("YlOrRd", graph$cluster2)(graph$cluster2))
hist(gemc$wber22[gemc$cluster2 == 1], breaks = 50)

# nicht so viel Unterschiede bei BP-Ergebnissen in Clustern --> ohne BPW clustern
# kein VP-Kandidat bei BP-Wahl also vielleicht BP-Wahl nicht die beste Vergleichswahl?
{# mit ltw, NRW und BP
distanz <- as.matrix (dist (historisch [, 29:50], method="euclidean"))
h1 <- hclust (as.dist(distanz), method="ward.D", members=NULL)
plot (h1)
cluster2 <- cutree (h1, k=2)
gemc <- cbind (historisch, cluster2)
table(gemc$cluster2)
round (aggregate (gemc [, 29:50], list(gemc$cluster2), mean), digits = 1)

# ltw und nrw und bp ohne NW
distanz <- as.matrix (dist (historisch [, c(29:34, 36:41, 43:49)], method="euclidean"))
h1 <- hclust (as.dist(distanz), method="ward.D", members=NULL)
plot (h1)
cluster2 <- cutree (h1, k=2)
gemc <- cbind (historisch, cluster2)
table(gemc$cluster2)
round (aggregate (gemc [, c(29:34, 36:41, 43:49)], list(gemc$cluster2), mean), digits = 1)
}

# nur mit ltw und NRW plus NW BP-Wahl --> quasi ähnlich wie wenn ohne NW
distanz <- as.matrix (dist (historisch [, c(29:42, 50)], method="euclidean"))
h1 <- hclust (as.dist(distanz), method="ward.D", members=NULL)
plot (h1)
cluster2 <- cutree (h1, k=2)
gemc <- cbind (historisch, cluster2)
table(gemc$cluster2)
round (aggregate (gemc [, c(29:42, 50)], list(gemc$cluster2), mean), digits = 1)

graph <- left_join(shape, gemc, by = c("GKZ" = "gkz"))
leaflet(graph) %>%
  addPolygons(weight = 0, opacity = 0, fillOpacity = 1,
              fillColor = ~colorNumeric("YlOrRd", graph$cluster2)(graph$cluster2))
hist(gemc$wber22[gemc$cluster2 == 1], breaks = 50)

