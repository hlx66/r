
year <- read.csv(file='~/Downloads/20230219-30261945-umsatz.CSV', sep=';', stringsAsFactors=TRUE)

stadt <- subset(year, Mandatsreferenz == "ZEN100000459968")

b <- stadt$Betrag

schloss <- subset(year, grepl("chloss", year$Beguenstigter.Zahlungspflichtiger))
# Miete Januar 2023 entfernt
hoffmann <- head(subset(year, grepl("offmann", year$Beguenstigter.Zahlungspflichtiger)), -1)
udarcev <- subset(year, grepl("darcev", year$Beguenstigter.Zahlungspflichtiger))



miete <- data.frame(schloss$Betrag, hoffmann$Betrag, udarcev$Betrag)


format(as.Date(dates), "%Y-%m")

