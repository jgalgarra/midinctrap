# Plots avg speed and ratio evolution, using hop years period
# List lcriteria allows to choose GDP, GNI or both
#
# Author: Javier Garcia-Algarra
# August 2021
# 
# Inputs:  data/all_speeds_criteria.csv"
# Results: results/AVG_EVOLUTION.png .tiff

library(ggplot2)
library(ggrepel)

criteria <- read.table("criteria.txt")
lcriteria <- criteria$V1

tdir <- "figs"
if (!dir.exists(tdir))
  dir.create(tdir)

hop <- 20
lstart_year <- seq(1960,2020-hop,by=hop)
ratio_breaks <- c(0.01,0.05,0.1,0.25,0.8,1,1.1)
ppi <- 300
scountry <- c("China","Brazil","Argentina","Chile","Portugal","Korea, Rep.")
for (criteria in lcriteria)
{
  datosdec <- data.frame("Country"=c(),"CountryCode"=c(),"distX"=c(),"distY"=c(),"PeriodStart" =c())
  datos_raw <- read.csv(paste0("data/all_speeds_",criteria,".csv"))
  lp <- unique(datos_raw$Country)
  for(start_year in lstart_year)
  {
    end_year <- start_year + hop
    
    datos_all <- datos_raw[(datos_raw$Year>=start_year) & (datos_raw$Year<=end_year),]
    datadist <- data.frame("Country"=c(),"CountryCode"=c(),"distX"=c(),"distY"=c())
    for (k in lp){
      if (is.element(k,scountry))
      {
        clean_data <- datos_all[(datos_all$Country == k) & !is.na(datos_all$ratio) & 
                                !is.na(datos_all$dratio_dt_mmov),]
        datosx <- datos_all[(datos_all$Country == k) & !is.na(datos_all$ratio),]$ratio
        datosy <- datos_all[(datos_all$Country == k) & !is.na(datos_all$dratio_dt_mmov),]$dratio_dt_mmov
        datadist <- rbind(datadist,data.frame("Country"=k,"CountryCode"= clean_data$CountryCode[1],
                                              "distX"=mean(clean_data$ratio),
                          "distY"=mean(clean_data$dratio_dt_mmov)))
        datosdec <- rbind(datosdec,
                          data.frame("Country"=k,"CountryCode"= clean_data$CountryCode[1],
                                     "distX"=mean(clean_data$ratio),
                                     "distY"=mean(clean_data$dratio_dt_mmov),
                                     "PeriodStart" =start_year)
                         )
        
      }
    }
    datosdec <- datosdec[!is.na(datosdec$CountryCode),]
    allp <- ggplot(data=datosdec)+ 
            geom_point(aes(y = distY, x = distX, color=CountryCode), alpha=0.8,size=2.5)+
            geom_path(aes(y = distY, x = distX,color=CountryCode),alpha=0.4,
            size = 0.5, arrow = arrow(length = unit(0.3, "cm"),type="closed"))+
            geom_text_repel(
                            aes(label=paste0(CountryCode," ",PeriodStart,"/",PeriodStart+hop," "),
                                y = distY, x = distX,color=CountryCode),alpha=0.8,size=4)+
            xlab(paste(criteria,"ratio")) + ylab ("Convergence speed")+
              theme_bw()+
              theme(panel.grid.minor = element_blank(),
                    panel.grid.major = element_line(size = 0.1),
                    legend.position = "none",
                    axis.text.y = element_text(face="bold", size=14),
                    axis.text.x = element_text(face="bold", size=14),
                    axis.title.x = element_text(face="bold", size=16),
                    axis.title.y  = element_text(face="bold", size=16))
    
    wplot <- 7
    hplot <- 6
    nfile <- paste0(tdir,"/AVG_EVOLUTION_",criteria)
    
    tiff(paste0(nfile,".tiff"), width=wplot*ppi, height=hplot*ppi,res=ppi)
    print(allp)
    dev.off()
    
    png(paste0(nfile,".png"), width=wplot*ppi, height=hplot*ppi, res=ppi)
    print(allp)
    dev.off()
  }
}