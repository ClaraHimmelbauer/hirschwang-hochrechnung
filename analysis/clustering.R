#--------------------------------------------------------------------------------------------------
# Clustering
#--------------------------------------------------------------------------------------------------

rm(list = ls()); gc()

# packages
packages = c("tidyverse", "readxl", "sf", "leaflet")
sapply(packages, library, character.only = T)

# source functions
files.sources = list.files(path = "R", full.names = T)
sapply(files.sources, source, encoding = "utf-8")

#--------------------------------------------------------------------------------------------------
# Hochrechnung hier in R machen
#--------------------------------------------------------------------------------------------------

# import ------------------------------------------------------------------------------------------

gem19 <- readxl::read_xlsx("data/prep_nrw19.xlsx", sheet = "gemeinden")
gemshp <- sf::st_read("data/gemeinden_simple.geojson")
blshp <- sf::st_read("data/bundeslaender_simple.geojson")

# clustering --------------------------------------------------------------------------------------

# colnames, um später die richtigen colums easy zu subsetten
names <- c("vp", "sp", "fp", "gr", "ne", "so", "nw")

# nichtwähler als eigene partei
gem19$nw <- gem19$wber - gem19$gueltig

# prozente ausrechnen
X <- gem19[ , names] / gem19$wber * 100

# Clustering: Distanzmatrix
# hclust methode ward.D. fragt mich nicht was das macht, ist halt eine hierarchische clustering art
distanz <- as.matrix (dist (X, method="euclidean"))
h1 <- hclust (as.dist(distanz), method="ward.D", members=NULL)
plot (h1)

# am Plot sieht man dass wenn man 2 cluster macht, diese relativ gleich groß sind
# ab 3 clustern ist einer davon super klein
# zwei cluster it is

cluster2 <- cutree (h1, k=2)
gem19$clust <- cluster2

# wodurch zeichnen sich die clsuter aus?
round (aggregate (X, list(gem19$clust), mean), digits = 1)
# Cluster 1: 2019 viel SP, NW 
# Clsuter 2: 2019 viel VP
# (# Anmerkung: NW sind hier auch inkl Briefwählerinnen)

# Die Hochrechnung wird dann für diese beiden Cluster separat gerechnet
# die Intuition dahinter: Wählerströme in Gemeinden, wo 2019 viel VP gewählt wurde verhalten
# sich anders als in Gemeinden in denen viele SP und Nichtwähler sind.

# plotting ----------------------------------------------------------------------------------------

graph <- left_join(gemshp, gem19, by = "gkz")

# leaflet
leaflet(graph) %>%
  addProviderTiles(providers$OpenStreetMap) %>%
  addPolygons(data = graph,
              weight = 1, opacity = 1, fillOpacity = 0.5,
              # fillColor = ~colorNumeric("YlOrRd", graph$clust)(graph$clust),
              fillColor = ~colorNumeric(c("red", "turquoise"), graph$clust)(graph$clust), 
              color = "#5d5555",
              label = ~name,
              highlightOptions = highlightOptions(color = "white", weight = 2),
              ) %>% 
  addPolylines(data = blshp,
               weight = 3, opacity = 1, color = "black")
