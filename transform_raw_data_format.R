library(readr)
# GDP transform
rawGDP <- read_csv("raw_data/API_NY.GDP.PCAP.KD_DS2_en_csv_v2_4770398_GDP2015.csv", skip = 4)
rawGDP <- rawGDP[,1:(ncol(rawGDP)-1)]
firstyearcol <- 5
names(rawGDP)[firstyearcol:ncol(rawGDP)] <- as.integer(names(rawGDP)[firstyearcol:ncol(rawGDP)])
rawGDP <- as.data.frame(rawGDP)
for (i in firstyearcol:ncol(rawGDP)){
  rawGDP[,i] <- as.numeric(rawGDP[,i])
}
write.table(rawGDP,"input_data/GDP2015_WB.csv",na = "",sep="\t",row.names = FALSE)

rawGNI <- read_csv("raw_data/API_NY.GNP.PCAP.CD_DS2_en_csv_v2_4771162_GNIAtlas.csv", skip = 4)
rawGNI <- rawGNI[,1:(ncol(rawGNI)-1)]
names(rawGNI)[firstyearcol:ncol(rawGNI)] <- as.integer(names(rawGNI)[firstyearcol:ncol(rawGNI)])
rawGNI <- as.data.frame(rawGNI)
for (i in firstyearcol:ncol(rawGNI)){
  rawGNI[,i] <- as.numeric(rawGNI[,i])
}
write.table(rawGNI,"input_data/GNIS.csv",na = "",sep=",",row.names = FALSE)
