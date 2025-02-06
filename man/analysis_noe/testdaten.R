# testdaten machen

source("11b_einlesen_txt.R")

names <- c("vp", "sp", "fp", "gr", "ne", "so")
df <- df[, 1:5]

prob <- c(rnorm(nrow(df), 50, 10), rnorm(nrow(df)*5, 10,2)) / 100
stimmen <- round(prob * rep(df$gueltig, 6), 0)
stimmen <- matrix(stimmen, nrow = nrow(df), ncol = 6)

df[names] <- stimmen
df[names] <- round(df[names]/rowSums(df[names]) * df$gueltig, 0)
df$gueltig <- rowSums(df[names])

dfx <- df
dfx[dfx$wber > 2000, 4:11] <- NA
nas <- sample(nrow(dfx), 200)
dfx[nas, 4:11] <- NA

nrow(dfx[!is.na(dfx$gueltig), ])

write.csv2(dfx, "testdaten_noe.csv", row.names = F)



dfx$ausgezaehlt <- ifelse(!is.na(dfx$gueltig), 1, 0)
dfx$abg <- dfx$gueltig + dfx$ung
dfx$nw <- dfx$wber - dfx$abg



# testdaten LTW13

library(readxl)
df <- read_xlsx("data/test_ltw13.xlsx")
df[is.na(df)] <- 0
df[df$wber >= 10000, 4:11] <- 0
x <- sample(1:nrow(df), 200)
df[x, 4:11] <- 0
write.csv2(df, "data/testdaten_ltw13_3.csv", row.names = F)


# testdaten NRW17

library(readxl)
df <- read_xlsx("data/test_nrw17.xlsx")
df[is.na(df)] <- 0
df[df$wber >= 5000, 4:11] <- 0
x <- sample(1:nrow(df), 300)
df[x, 4:11] <- 0
write.csv2(df, "data/testdaten_nrw17_2.csv", row.names = F)

# bp16
library(readxl)
df <- read_xlsx("data/test_bp16.xlsx")
df[is.na(df)] <- 0
df[df$wber >= 10000, 4:11] <- 0
x <- sample(1:nrow(df), 200)
df[x, 4:11] <- 0
write.csv2(df, "data/testdaten_bp16_3.csv", row.names = F)


# ltw23
df <- read.csv2("data/testdaten_ltw23.csv")
df[is.na(df)] <- 0
df[df$wber >= 10000, 5:16] <- 0
x <- sample(1:nrow(df), 200)
df[x, 5:16] <- 0
write.csv2(df, "data/testdaten_ltw23_3.csv", row.names = F)
