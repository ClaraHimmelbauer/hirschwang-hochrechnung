# export api

parteien <- c("ÖVP", "SPÖ", "FPÖ", "GRÜNE", "NEOS", "Sonstige")
proz_num <- check3$ohne.NW[1:6]
proz_str <- paste0(format(round(proz_num, 1), decimal.mark = ",", nsmall = 1), "%")
proz_str <- gsub(" ", "", proz_str)
mandate  <- sitze[rownames(sitze) == "mandate", ]
regierung <- sitze[rownames(sitze) == "regierung", ]
proz_num_alt <- c(49.6, 23.9, 14.8, 6.4, 5.2, 0.1)
proz_str_alt <- c("49,6%", "23,9%", "14,8%", "6,4%", "5,2%", "0,1%")
mandate_alt <- c(29, 13, 8, 3, 3, 0)
regierung_alt <- c(6, 2, 1, 0, 0, 0)

time <- format(Sys.time(), "%H:%M")
sb_str <- paste0(format(round(sb, 1), decimal.mark = ",", nsmall = 1), "%")
ag_str <- paste0(format(round(auszaehlungsgrad, 1), decimal.mark = ",", nsmall = 1), "%")

list_wahl <- list("partei" = 0, "prozent_num" = 0, "prozent_str" = 0, "mandate" = 0, "prozent_num_alt" = 0, "prozent_str_alt" = 0, "mandate_alt" = 0, "regierung" = 0, "regierung_alt" = 0)
list_wahl <- list(list_wahl, list_wahl, list_wahl, list_wahl, list_wahl, list_wahl)

for(i in 1:6){
  list_wahl[[i]][1] <- parteien[i]
  list_wahl[[i]][2] <- round(proz_num[i], 1)
  list_wahl[[i]][3] <- proz_str[i]
  list_wahl[[i]][4] <- mandate[i]
  list_wahl[[i]][5] <- proz_num_alt[i]
  list_wahl[[i]][6] <- proz_str_alt[i]
  list_wahl[[i]][7] <- mandate_alt[i]
  list_wahl[[i]][8] <- regierung[i]
  list_wahl[[i]][9] <- regierung_alt[i]
}

list_gebiet <- list("gebiet" = "Niederösterreich", "gueltig" = gueltig, "schwankungsbreite" = sb_str, "auszaehlungsgrad" = ag_str,
                    "wahl" = list_wahl)

outlist <- list("typ" = "Hochrechnung", "wahl_id" = "LTW_NOE_2023", "uhrzeit" = time, "gebiete" = list_gebiet)


# export lokal mit timestamp

name <- paste0("save/json_export/", Sys.time(), ".json")
name <- gsub(" ", "-", name)
name <- gsub(":", "-", name)
write_json(outlist, name, pretty = T, auto_unbox = T)

# api post

outjson <- toJSON(outlist, pretty = T, auto_unbox = T)

POST(url = "https://ogm-election-data-api.ew.r.appspot.com/api/v1/area-projections",
     add_headers("Authorization" = paste0("Api-Key ", "P6yWpK9qPy4XEVMjGgafxbfU")),
     content_type_json(),
     body = outjson)

# test
file <- GET(url = "https://ogm-election-data-api.ew.r.appspot.com/api/v1/area-projections?typ=Hochrechnung&limit=1&sort=-createdAt",
            add_headers("Authorization" = paste0("Api-Key ", "P6yWpK9qPy4XEVMjGgafxbfU")))
write_json(content(file), "analysis/test_im.json", pretty = T, auto_unbox = T)
