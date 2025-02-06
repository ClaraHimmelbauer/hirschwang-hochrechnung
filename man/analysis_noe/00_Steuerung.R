# Steuerung


# install.packages("RDCOMClient", repos = "http://www.omegahat.net/R", type="win.binary")


rm(list = ls()); gc()

library(readxl)
library(tidyverse)
library(jsonlite)
library(httr)
library(rvest)
library(RDCOMClient)


source("analysis/00b_names.R", encoding = "UTF-8")
source("analysis/00c_functions.R", encoding = "UTF-8")


# Nullstellen am Wahltag noch einmal, dann auskommentieren!
source("analysis/00d_api_null.R", encoding = "UTF-8")


# Iterationen für die Schwankungsbreite
sb_iterations <- 100


# Am Wahltag um 16:59 auf TRUE stellen!
gueltig <- TRUE

source("analysis/01a_einlesen_bmi.R", encoding = "UTF-8")
# Hier noch den Zugriff auf die Testdaten entfernen (2. Zeile)
source("analysis/02_aufbereiten_merge.R", encoding = "UTF-8")
source("analysis/03_model.R", encoding = "UTF-8")
# check!


source("analysis/04_schwankungsbreite.R", encoding = "UTF-8")


# Anpassung der Schwankungsbreite
sb <- 0.0
source("analysis/05_Mandate_und_Regierungsmitglieder.R", encoding = "UTF-8")


source("analysis/06_export.R", encoding = "UTF-8")

# vor Sendungsbeginn: auskommentierte bcc-Zeile für Versand einkommentieren!
source("analysis/07_mailversand_presse_parteien.R", encoding = "UTF-8")


