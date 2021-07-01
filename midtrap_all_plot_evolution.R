# Plots avg speed and ratio evolution, using hop years period

library(ggplot2)

lcriterio <- c("GDP","GNI")
lcriterio <- c("GNI")

tdir <- "figs"
if (!dir.exists(tdir))
  dir.create(tdir)

hop <- 20
lstart_year <- seq(1960,2020-hop,by=hop)

datosdec <- data.frame("Country"=c(),"CountryCode"=c(),"distX"=c(),"distY"=c(),"PeriodStart" =c())
scountry <- c("China","Brazil","Argentina","Chile","Portugal","Korea, Rep.")
for (criterio in lcriterio)
{
  datos_raw <- read.csv(paste0("data/all_speeds_",criterio,".csv"))
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
            geom_point(aes(y = distY, x = distX, color=Country), alpha=0.8,size=2)+
            geom_path(aes(y = distY, x = distX,color=Country),alpha=0.5,
            size = 0.7, arrow = arrow(length = unit(0.2, "cm"),type="closed"))+
            scale_x_log10()+
            geom_text(data=datosdec,
              aes(label=paste0(PeriodStart,"-",PeriodStart+hop," ",CountryCode),y = distY, x = distX, vjust=1, hjust=-.2, color=Country),
              size=3) + 
              theme_bw()+theme(panel.grid.minor = element_blank(),
                               panel.grid.major = element_line(size = 0.1))
    
    ppi <- 300
    png(paste0(tdir,"/AVG_EVOLUTION.png"), width=8*ppi, height=6*ppi, res=ppi)
    print(allp)
    dev.off()
  }
}