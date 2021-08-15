# Script to plot average convergence speed and ratios of all countries
# List lcriteria allows to choose GDP, GNI or both
#
# Author: Javier Garcia-Algarra
# August 2021
# 
# Inputs:  data/countires_msci.csv
#          data/all_speeds_criteria.csv"
# Results: results/ALL_DISTANCES.png Errors by model


library(ggplot2)
library(cowplot)
library(factoextra)

criteria <- read.table("criteria.txt", quote="\"", comment.char="")
lcriteria <- criteria$V1

tdir <- "figs"
if (!dir.exists(tdir))
  dir.create(tdir)

start_year <- 1980
end_year <- 2020

# lexclude <- c("Korea, Rep.","Spain","Portugal","Morocco","Italy","France")
# lexclude <- c()


countries_msci <- read.csv("data/countries_msci.csv")
ratio_breaks <- c(0.01,0.05,0.1,0.25,0.8,1,1.1)
my_breaks <- c(100,1000,10000,25000,50000)
speed_breaks=c(-sqrt(0.5),0,sqrt(0.5),1,sqrt(1.5))
speed_labels = c(-0.5,0,0.5,1,1.5)

onlyMSCI <- TRUE

for (criteria in lcriteria)
{
  datos_all <- read.csv(paste0("data/all_speeds_",criteria,".csv"))
  datos_all <- datos_all[(datos_all$Year>=start_year) & (datos_all$Year<=end_year),]

  lp <- unique(datos_all$CountryCode)
  #lp <- lp[!is.element(lp,lexclude)]
  datadist <- data.frame("Country"=c(),"CountryCode"=c(),"Region"=c(),"MCSI Class" = c(),"distX"=c(),"distY"=c())
  for (k in lp){
    clean_data <- datos_all[(datos_all$CountryCode == k) & !is.na(datos_all$ratio) & 
                            !is.na(datos_all$dratio_dt_mmov),]
    datosx <- datos_all[(datos_all$CountryCode == k) & !is.na(datos_all$ratio),]$ratio
    print(paste(k,"number of values",length(datosx)))
    datosy <- datos_all[(datos_all$Country == k) & !is.na(datos_all$dratio_dt_mmov),]$dratio_dt_mmov
    datos_MCSI <- countries_msci[countries_msci$ISOCode == k,]
    # Only if the series has more than 25 values and is not a rich country
    if ((length(datosx)>30) & (max(clean_data$ratio)<max(ratio_breaks)))
      if ((onlyMSCI) & (datos_MCSI$Category != "None"))
      datadist <- rbind(datadist,data.frame("Country"=k,"CountryCode"= clean_data$CountryCode[1],"Region"=datos_MCSI$Region,
                                            "MCSI Class"= datos_MCSI$Category, "distX"=mean(clean_data$ratio),
                                            "distY"=mean(clean_data$dratio_dt_mmov)))
    
  }
   
  
  datadist$distYtrans <- sqrt(abs(datadist$distY))*(-2*as.numeric(datadist$distY < 0)+1) 
  for (m in 1:nrow(datadist))
    datadist$Magnitude[m] <- mean(datos_all[datos_all$CountryCode==datadist$CountryCode[m],]$Magnitude,na.rm=TRUE)
  distp <- ggplot(data=datadist) + 
    geom_point(aes(y = distYtrans, x = distX, shape=MCSI.Class, fill=Region, color=Region), alpha=0.5,size=3.5)+
    geom_text(data=datadist,
             aes(label=CountryCode,y = distYtrans, x = distX, vjust=0.1+is.element(CountryCode,c("PHL","MYS","ISR","BGD","JOR")), hjust=-0.2),alpha=0.4,
             size=4,angle=10) + xlab("Avg. ratio") + ylab ("Avg. convergence speed")+
    scale_x_log10(breaks = ratio_breaks, labels = ratio_breaks)+
    scale_y_continuous(breaks=speed_breaks,labels=speed_labels)+
    scale_shape_manual(values=seq(21,25)) +
    # scale_color_gradientn(name=criteria,colours=c("violet","blue","green","yellow","orange","red"),trans="log",
    #                       breaks = my_breaks, labels = my_breaks,limits=c(100,100000))+
    theme_bw()+theme(panel.grid.minor = element_blank(),
                     panel.grid.major = element_line(size=0.4,linetype = 3, color="ivory3"),
                     axis.text.y = element_text(face="bold", size=12),
                     axis.text.x = element_text(face="bold", size=12),
                     axis.title.x = element_text(face="bold", size=14),
                     axis.title.y  = element_text(face="bold", size=14))
  
  ppi <- 300
  png(paste0(tdir,"/ALL_DISTANCES.png"), width=10*ppi, height=8.5*ppi, res=ppi)
  print(distp)
  dev.off()
  
  datak <- datadist
  row.names(datak) <- datadist$CountryCode
  datak <- subset(datak,select=c(distX,distY))
  names(datak) <- c("Dim1","Dim2")
  fviz_nbclust(datak, FUN = kmeans, method = "wss")
  clust <- kmeans(datak, 5, iter.max = 20, nstart = 1)
  fviz_cluster(clust, data = datak, ggtheme = theme_minimal())
}