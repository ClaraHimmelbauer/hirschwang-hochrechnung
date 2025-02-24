#--------------------------------------------------------------------------------------------------
# Datensatz erstellen: NRW 2024 zum Einlesen
#--------------------------------------------------------------------------------------------------

rm(list = ls()); gc()

# packages
packages = c("tidyverse", "readxl", "sf")
sapply(packages, library, character.only = T)

# source functions
files.sources = list.files(path = "R", full.names = T)
sapply(files.sources, source, encoding = "utf-8")

#--------------------------------------------------------------------------------------------------
# Wenn um 17:00 alle Wahllokale schließen, sind nur wenige Gemeinden ausgezählt
# Das sind vor allem kleine Gemeinden
# Hier wird ein Beispieldatensatz erstellt, wo nur kleine Gemeinden ausgezählt sind
#--------------------------------------------------------------------------------------------------

# data --------------------------------------------------------------------------------------------

gem <- read_xlsx("data/prep_nrw24.xlsx", sheet = "gemeinden")
gemshp <- st_read("data/gemeinden_simple.geojson")
blshp  <- st_read("data/bundeslaender_simple.geojson")

# wrangling ---------------------------------------------------------------------------------------

u2000 <- which(gem$wber < 2000)

# how many communities have less than 2000 eligible voters?
length(u2000) / nrow(gem)

# sample 300 communities. that's about 14% of all communities
set.seed(420)
s300 <- sample(u2000, 300)

# rows where results should be replaced
rep <- setdiff(1:nrow(gem), s300)

# columns where results should be replaced
names <- c("abg", "ung", "gueltig", "vp", "sp", "fp", "gr", "ne", "so")

# new dataframe where things are going to be replaced
df <- gem
df[rep, names] <- NA

# look at new dataframe
View(df)

# export ------------------------------------------------------------------------------------------

write.csv2(df, "data/in_results-filtered.csv", row.names = F)

# plot: welche Gemeinden sind schon ausgezählt ----------------------------------------------------

gemshp <- left_join(gemshp, df, by = "gkz")
gemshp$ausgezaehlt <- ifelse(is.na(gemshp$abg), "nein", "ja")

ggplot(gemshp) +
  geom_sf(aes(fill = ausgezaehlt), color = "black") +
  geom_sf(data = blshp, color = "black", lwd = 2, fill = NA) +
  scale_fill_manual(values = c("#a10b0b", "white"),
                    name = "Ausgezählt") +
  theme_void() +
  labs(title = "Ausgezählte Gemeinden") +
  theme(legend.position = c(0.15, 0.75),
        plot.title = element_blank())

