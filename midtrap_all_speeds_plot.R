# Script to plot average convergence speed and ratios of all countries
# List lcriteria allows to choose GDP, GNI or both
#
# Author: Javier Garcia-Algarra
# August 2021
# 
# Inputs:  data/countires_msci.csv
#          data/all_speeds_criteria.csv"
# Results: results/ALL_DISTANCES.png .tiff


library(ggplot2)
library(ggrepel)
library(cowplot)
library(factoextra)
library(Rcpp)

criteria <- read.table("criteria.txt")
lcriteria <- criteria$V1

tdir <- "figs"
if (!dir.exists(tdir))
  dir.create(tdir)

start_year <- 1980
end_year <- 2020

# lexclude <- c("Korea, Rep.","Spain","Portugal","Morocco","Italy","France")
# lexclude <- c()


countries_msci <- read.csv("data/countries_msci.csv")
group_ASIA <- read.csv(paste0("data/group_ASIA.csv"))
group_LATAM <- read.csv(paste0("data/group_LATAM.csv"))
group_AFRICA <- read.csv(paste0("data/group_AFRICA.csv"))
group_EUROPA <- read.csv(paste0("data/group_EUROPA.csv"))

ratio_breaks <- c(0.01,0.05,0.1,0.25,0.8,1,1.1)
my_breaks <- c(100,1000,10000,25000,50000)
speed_breaks <-c(-sqrt(0.5),0,sqrt(0.5),1,sqrt(1.5))
speed_labels <- c(-0.5,0,0.5,1,1.5)

onlyMSCI <- TRUE

for (criteria in lcriteria)
{
  print(criteria)
  datos_all <- read.csv(paste0("data/all_speeds_",criteria,".csv"))
  datos_all <- datos_all[(datos_all$Year>=start_year) & (datos_all$Year<=end_year),]

  lp <- unique(datos_all$CountryCode)
  #lp <- lp[!is.element(lp,lexclude)]
  datadist <- data.frame("Country"=c(),"CountryCode"=c(),"Region"=c(),"MSCI Category" = c(),"distX"=c(),"distY"=c())
  for (k in lp){
    clean_data <- datos_all[(datos_all$CountryCode == k) & !is.na(datos_all$ratio) & 
                            !is.na(datos_all$dratio_dt_mmov),]
    datosx <- datos_all[(datos_all$CountryCode == k) & !is.na(datos_all$ratio),]$ratio
    print(paste(k,"number of values",length(datosx)))
    datosy <- datos_all[(datos_all$Country == k) & !is.na(datos_all$dratio_dt_mmov),]$dratio_dt_mmov
    datos_MSCI <- countries_msci[countries_msci$ISOCode == k,]
    # Only if the series has more than 25 values and is not a rich country
    if ((length(datosx)>30) & (max(clean_data$ratio)<max(ratio_breaks)))
      if ((onlyMSCI) & (datos_MSCI$Category != "None"))
      datadist <- rbind(datadist,data.frame("Country"=k,"CountryCode"= clean_data$CountryCode[1],"Region"=datos_MSCI$Region,
                                            "MSCI Category"= datos_MSCI$Category, "distX"=mean(clean_data$ratio),
                                            "distY"=mean(clean_data$dratio_dt_mmov)))
    
  }
   
  # Y transform to plot over a squared root scale
  datadist$distYtrans <- sqrt(abs(datadist$distY))*(-2*as.numeric(datadist$distY < 0)+1) 
  for (m in 1:nrow(datadist))
    datadist$Magnitude[m] <- mean(datos_all[datos_all$CountryCode==datadist$CountryCode[m],]$Magnitude,na.rm=TRUE)
  distp <- ggplot(data=datadist) + 
    geom_point(aes(y = distYtrans, x = distX, shape=MSCI.Category, fill=Region, color=Region), alpha=0.5,size=3.5)+
    xlab(paste("Avg.",criteria,"ratio")) + ylab ("Avg. convergence speed")+
    geom_text_repel(data=datadist,aes(label=CountryCode,y = distYtrans, x = distX),
                    alpha=0.6,size=4)+
    scale_x_log10(breaks = ratio_breaks, labels = ratio_breaks)+
    scale_y_continuous(breaks=speed_breaks,labels=speed_labels)+
    scale_shape_manual(values=seq(21,25)) +
    theme_bw()+theme(panel.grid.minor = element_blank(),
                     panel.grid.major = element_line(size=0.4,linetype = 3, color="ivory3"),
                     legend.title = element_text(face="bold", size=15),
                     legend.text = element_text( size=13),
                     axis.text.y = element_text(face="bold", size=15),
                     axis.text.x = element_text(face="bold", size=15),
                     axis.title.x = element_text(face="bold", size=16),
                     axis.title.y  = element_text(face="bold", size=16))
  
  
  # Regression group lines for selected areas
  datosreg <- datadist[is.element(datadist$CountryCode,c(group_ASIA$CountryCode,
                                                        group_LATAM$CountryCode,
                                                        group_AFRICA$CountryCode,
                                                        group_EUROPA$CountryCode)),]
  distpreg <- distp + geom_point(data=datosreg, aes(y = distYtrans, x = distX, color=Region),
                                size=5,alpha=0.8,shape=21,stroke=1) +
              geom_smooth(data=datosreg,method=lm, 
                          aes(fill=Region,color=Region,y = distYtrans, x = distX), 
                          alpha=0.1,size=0.1)
  
  wplot <- 10
  hplot <- 8.5
  nfile <- paste0(tdir,"/ALL_DISTANCES_",criteria)
  ppi <- 300

  tiff(paste0(nfile,".tiff"), width=wplot*ppi, height=hplot*ppi,res=ppi)
  print(distp)
  dev.off()
  
  png(paste0(nfile,".png"), width=wplot*ppi, height=hplot*ppi, res=ppi)
  print(distp)
  dev.off()
  
  png(paste0(nfile,"_REGIONS.png"), width=wplot*ppi, height=hplot*ppi, res=ppi)
  print(distpreg)
  dev.off()
  
  png(paste0(nfile,"_SIDEBYSIDE.png"), width=2*wplot*ppi, height=hplot*ppi, res=ppi)
  todo <- plot_grid(
    distp,distpreg,labels=c("A","B"),
    label_size = 16,
    ncol = 2
  )
  print(todo)
  dev.off()
}