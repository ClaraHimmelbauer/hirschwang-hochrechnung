rm(list = ls())

packages <- c("tidyverse", "readxl", "httr", "rvest", "gridExtra")
lapply(packages, library, character.only = T)

#--------------------------------------------------------------------------------------------------

# Auf der X-Achse einmal die Impfquote der Gemeinde (Daten beim Gesundheitsministerium), einmal der Logarithmus der Gemeindegröße
# Auf der Y-Achse jeweils das Verhältnis Prozent der Partei 2023 / Prozent der Partei 2018

#--------------------------------------------------------------------------------------------------

source("analysis/00b_names.R", encoding = "UTF-8")

ltw23 <- read_xlsx("data/ltw23_final.xlsx")
covid <- read.csv2("data/covid_impfung_gemeinden_20230329.csv")
merge <- read_xlsx("data/import_vergleichsdaten.xlsx")

# proz 2018 ausrechnen ----------------------------------------------------------------------------
names_proz_ltw <- paste0("proz_", names_ltw)
sum_ltw <- rowSums(merge[names_ltw])
merge[names_proz_ltw] <- lapply(merge[names_ltw], function(x){x/sum_ltw})

merge <- merge[c("gkz", names_proz_ltw)]

# daten ltw23 vorbereiten -------------------------------------------------------------------------
ltw23$nw <- ltw23$`Wahl-berecht.` - ltw23$vp - ltw23$sp - ltw23$fp - ltw23$gr - ltw23$ne - ltw23$so
names_proz <- paste0("proz_", names)
ltw23[names_proz] <- sapply(ltw23[names], function(x){x/rowSums(ltw23[names])})

ltw23$log_wb <- log(ltw23$`Wahl-berecht.`)
colnames(ltw23)[1] <- "gkz"

df <- ltw23[c("gkz", "Bezeichnung", "log_wb", names_proz)]

# impfdaten vorbereiten ---------------------------------------------------------------------------
colnames(covid) <- c("date", "gkz", "name", "pop", "vac1", "vac2", "vac3", "vac4")

covid$proz_vac1 <- covid$vac1 / covid$pop
covid$proz_vac3 <- covid$vac3 / covid$pop

covid <- covid[c("gkz", "pop", "proz_vac1", "proz_vac3")]

# merge -------------------------------------------------------------------------------------------
df <- left_join(df, merge)
df <- left_join(df, covid)

names_dif <- paste0(names, "_dif")
df[names_dif] <- df[names_proz] / df[names_proz_ltw]

# plots -------------------------------------------------------------------------------------------
p1 <- ggplot(df, aes(x = proz_vac1, y=ne_dif)) +
  geom_point() +
  geom_smooth(method = "lm", se = F) +
  theme_minimal() +
  ylim(0.5, 3.5)
p2 <- ggplot(df, aes(x = log_wb, y=ne_dif)) +
  geom_point() +
  geom_smooth(method = "lm", se = F) +
  theme_minimal() +
  ylim(0.5, 3.5)
p3 <- grid.arrange(p1, p2, ncol = 2)
p3
ggsave("plots/plots_impfung_wber/ne.png", p3, width = 8, height = 4)
