#--------------------------------------------------------------------------------------------------
# Datensatz erstellen: NRW 2024 zum Einlesen
#--------------------------------------------------------------------------------------------------

rm(list = ls()); gc()

# packages
packages = c("tidyverse", "readxl")
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
