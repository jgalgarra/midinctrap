library(ggplot2)
library(cowplot)

lcriterio <- c("GNI")

for (criterio in lcriterio){
  all_speeds_GDP <- read.csv(paste0("data/all_speeds_",criterio,".csv"), stringsAsFactors=TRUE)
  
  data_country <- all_speeds_GDP[all_speeds_GDP$Country == "Brazil",]  
  #Eliminar NAs
  NArecentyear <- max(data_country[is.na(data_country$dratio_dt),]$Year)
  firstyear <- min(data_country[data_country$Year > NArecentyear,]$Year)
  lastyear <- max(data_country$Year )
  data_country_valid <- data_country[data_country$Year > NArecentyear,]
  data_country_valid$AR1 <- NA
  #data_country_valid$ratio_normalized <- data_country_valid$ratio - mean(data_country_valid$ratio)
  data_country_valid$val_normalized <- data_country_valid$ratio 
  #
  # https://stats.stackexchange.com/questions/184425/fitting-model-ar1-with-r
  lwindow <- 5
  for (k in 1:(nrow(data_country_valid)-lwindow)){
    print
    print(data_country_valid$val_normalized[k:(k+5)])
    wnF=arima(data_country_valid$val_normalized[k:(k+5)], order=c(1,0,0), method="ML")
    data_country_valid$AR1[k] <- wnF$coef["ar1"]
  }
  pClosingGapSpeed <- ggplot(data= data_country_valid, aes(x=Year,y=ratio))+
    geom_point(size=2,col="blue",alpha=0.5)+#ylab(paste("Ratio",criterio))+
    theme_bw()
  pClosingGapSpeedmmov <- ggplot(data= data_country_valid, aes(x=Year,y=-dratio_dt_mmov))+
    geom_point(size=2,col="blue",alpha=0.5)+ylab(paste("Convergence speed MM",criterio))+
    theme_bw()
  
  pAR1 <- ggplot(data= data_country_valid, aes(x=Year,y=AR1))+
    geom_point(size=2,col="blue",alpha=0.5)+ylab(paste("AR1",criterio))+
    theme_bw()
  ptot <- plot_grid(
    pClosingGapSpeedmmov, pClosingGapSpeed, pAR1,
    ncol = 1
  )
  
}
