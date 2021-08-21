# Script to generate all magnitudes
# 
# List lcriteria allows to choose GDP, GNI or both
#
# Author: Javier Garcia-Algarra
# August 2021
# 
# set print_indiv to TRUE to procude all plots
# set print_tiff to print the ALL plot in that format
#
# Inputs:  data/GDP2010_WB.csv    GNIS.csv
#          config_data/config_plots.csv
#          config_data/criteria.txt
# Results: data/all_speeds_criteria.csv
#          figs/countries

library(ggplot2)
library(ggrepel)
library(cowplot)
library(patchwork)
library(zoo)

# Function to format axis labels
scaleFUN <- function(x) sprintf("%2.1f ", x)

criteria <- read.table("config_data/criteria.txt")
lcriteria <- criteria$V1

configuration_file <- read.table("config_data/config_plots.csv", sep=";", header=TRUE)
if (configuration_file$CountryCode == "MSCI"){           # Plot MSCI countries
  countries_msci <- read.csv("data/countries_msci.csv")
  lcountrycode <- countries_msci$ISOCode
} else 
  lcountrycode <- configuration_file$CountryCode

print_indiv <- configuration_file$print_indiv      # Print individual files
print_tiff <- configuration_file$print_tiff        # Produce tiff files. Be careful, each tiff file weights 24 MB!

USA_perc_middle_GNI <- 0.3
USA_perc_middle_GDP <- 0.5
gap_widening <- 0
mmovper <- 5
last_year_Ecihengreen <- 2013
tdir1 <- "figs"
tdir <- paste0(tdir1,"/countries")
if (!dir.exists(tdir))
  dir.create(tdir1)
  dir.create(tdir)

for (criteria in lcriteria)
{
  count_countries <- 0
  if (criteria == "GDP")
    USA_perc_middle <- USA_perc_middle_GDP
  else
    USA_perc_middle <- USA_perc_middle_GNI
  for (countrycode in lcountrycode)
  {
    count_countries <- count_countries + 1
    print(paste(countrycode,criteria))
    if (criteria == "GNI"){
      DATA_WB <- read.csv("data/GNIS.csv")
    } else
      DATA_WB <- read.delim2("data/GDP2010_WB.csv")
    rawdata <- DATA_WB[DATA_WB$Country.Code==countrycode,]
    usadata <- DATA_WB[DATA_WB$Country.Name=="United States",]
    country <- DATA_WB[DATA_WB$Country.Code==countrycode,]$Country.Name
    datos_pais <- data.frame("Year"=seq(1960,2019))
    datos_pais$Magnitude = 0
    datos_pais$growth = 0
    datos_pais$gb = 0
    datos_pais$ga = 0
    datos_pais$dif = 0
    datos_pais$dgrowth_dt = 0
    datos_pais$USAMagnitude = 0
    datos_pais$ratio = 0
    datos_pais$dratio_dt = 0
    datos_pais$dratio_dt_mmov = 0
    datos_pais$dratio_dt2_mmov = 0
    period_calc = 7
    for (i in 1960:2020){
      datos_pais$Magnitude[i-(1960)] <- as.numeric(rawdata[,4+i-1960])
      datos_pais$USAMagnitude[i-(1960)] <- as.numeric(usadata[,4+i-1960]) # USA GDP or GNI
      datos_pais$ratio[i-(1960)] <- datos_pais$Magnitude[i-(1960)]/datos_pais$USAMagnitude[i-(1960)]
    }
    for (j in 2:nrow(datos_pais))
      datos_pais$growth[j] <- 100*(datos_pais$Magnitude[j] - datos_pais$Magnitude[j-1])/
                              (datos_pais$Magnitude[j-1])  # Magnitude growth
    for (j in 2:(nrow(datos_pais))){
      datos_pais$dgrowth_dt[j] <- (datos_pais$growth[j+1] - datos_pais$growth[j])
      # Gap closing speed
      datos_pais$dratio_dt[j] <- 100*(datos_pais$ratio[j] - datos_pais$ratio[j-1])
      }
    datos_pais$dratio_dt_mmov <- rollmean(datos_pais$dratio_dt, mmovper, fill = NA,  align = "right")
    #datos_pais$dratio_dt2_mmov<- rollmean(datos_pais$dratio_dt2, mmovper_acc, fill = NA,  align = "right")
    
    for (j in 2:(nrow(datos_pais)))
      datos_pais$dratio_dt2_mmov[j] <- (datos_pais$dratio_dt_mmov[j] - datos_pais$dratio_dt_mmov[j-1])
    # criteria Eichengreen
    for (j in period_calc:nrow(datos_pais))
      datos_pais$gb[j] <- sum(datos_pais$growth[(j-(period_calc-1)):j])/period_calc
    for (j in 1:(nrow(datos_pais)-period_calc))
      datos_pais$ga[j] <- sum(datos_pais$growth[(j+1):(j+(period_calc))])/period_calc
    for (j in 1:nrow(datos_pais))
      datos_pais$dif[j] <- datos_pais$ga[j]-datos_pais$gb[j]
    datosplot <- datos_pais[period_calc:(nrow(datos_pais)-period_calc),]
    datosplot$Trapped = FALSE
    mingb = 3.5
    mindif = -2
    minMagnitude = 5000
    limityears <- c(1960,2020)
    yearlabels <- seq(1960,2020,by=10) 
    datosplot$gb = (floor(datosplot$gb* 10)+1)/10
    datosplot$dif = (floor(datosplot$dif* 10)+1)/10
    for (k in 1:nrow(datosplot)){
      datosplot$Trapped[k] = (datosplot$Magnitude[k]>minMagnitude) & (datosplot$gb[k]>=mingb) & (datosplot$dif[k] <= mindif)
    }
    pEichen <- ggplot(data= datosplot, aes(x=Year,y=dif,color=Trapped))+
         geom_point(size=2)+ylab(paste("Dif Eichengreen"))+xlab("")+
         scale_color_discrete(limits = c('TRUE', 'FALSE'))+
         scale_x_continuous(limits=limityears,breaks=yearlabels,labels=yearlabels)+ 
         scale_y_continuous(labels=scaleFUN)+
         theme_bw()+ 
         theme(legend.position="top",
               legend.title = element_text(face="bold", size=12),
               legend.text = element_text( size=12),
            axis.text.y = element_text(face="bold", size=12),
            axis.text.x = element_text(face="bold", size=12),
            axis.title.x = element_text(face="bold", size=13),
            axis.title.y  = element_text(face="bold", size=13))
    
    datosEich <- datos_pais[(datos_pais$Year >= min(datosplot$Year))&
                              (datos_pais$Year<last_year_Ecihengreen),]
    datosEich <- datos_pais 
    pconvergencespeed <- ggplot(data= datosEich, aes(x=Year,y=dratio_dt_mmov))+
      geom_point(size=2,col="black",alpha=0.5)+ylab(paste("Conv. speed"))+
      scale_x_continuous(limits=limityears,breaks=yearlabels,labels=yearlabels)+
      scale_y_continuous(labels=scaleFUN)+
      theme_bw()+ xlab("")+
      theme(axis.text.y = element_text(face="bold", size=12),
            axis.text.x = element_text(face="bold", size=12),
            axis.title.x = element_text(face="bold", size=13),
            axis.title.y  = element_text(face="bold", size=13))
    
    pratio <- ggplot(data= datosEich, aes(x=Year,y=ratio))+
      geom_point(size=2,col="black",alpha=0.5)+ylab(paste("Ratio"))+
      scale_x_continuous(limits=limityears, breaks=yearlabels,labels=yearlabels)+
      theme_bw() + xlab("Year")+
      theme(axis.text.y = element_text(face="bold", size=12),
                                axis.text.x = element_text(face="bold", size=12),
                                axis.title.x = element_text(face="bold", size=13),
                                axis.title.y  = element_text(face="bold", size=13))
    
    datosEich$signacc <- datosEich$dratio_dt2_mmov > gap_widening
    pacc <- ggplot(data= datosEich, aes(x=Year,y=dratio_dt2_mmov,col=signacc))+
      geom_point(size=2)+ylab(paste("Conv. accel."))+
      scale_x_continuous(limits=limityears, breaks=yearlabels,labels=yearlabels)+
      scale_y_continuous(labels=scaleFUN)+
      geom_hline(yintercept=gap_widening, color="red",linetype = "dotted",size=0.6) +
      theme_bw() + xlab("Year")+
      theme(legend.position = "none",
            axis.text.y = element_text(face="bold", size=12),
            axis.text.x = element_text(face="bold", size=12),
            axis.title.x = element_text(face="bold", size=13),
            axis.title.y  = element_text(face="bold", size=13))
    
    pizda <- plot_grid(
      pEichen, pconvergencespeed, pratio, labels=c("A","B","C"),
      label_size = 15,
      ncol = 1
    )
    
    datos_pais$Country = country
    datos_pais$CountryCode = rawdata$Country.Code[1]
    if (print_indiv){
      ppi <- 300

      my_breaks <- c(100,500,1000,2500,5000,10000,20000,50000)
      my_breaks <- c(100,500,5000,50000)
      datos_speed <- datos_pais[!is.na(datos_pais$dratio_dt_mmov),]

      pratiospeed <- ggplot(data=datos_speed) +
        geom_path(
          aes(y = dratio_dt_mmov, x = ratio, colour=Magnitude),alpha=0.4,size = 0.6,
          arrow = arrow(length = unit(0.2, "cm"),type="closed"))+
        geom_point(aes(y = dratio_dt_mmov, x = ratio, colour=Magnitude), size = 2.5,
                  alpha=0.8) +
        geom_point(data=datos_speed[1,],aes(y = dratio_dt_mmov, x = ratio, colour=Magnitude), size = 3,
                   alpha=1,shape=23, stroke=1) +
        geom_point(data=datos_speed[nrow(datos_speed),],aes(y = dratio_dt_mmov, x = ratio, colour=Magnitude), size = 3,
                   alpha=1,shape=23, stroke=1) +
        scale_color_gradientn(name=criteria,colours=c("violet","blue","green","orange","red"),trans="log",
                                                      breaks = my_breaks, labels = my_breaks,limits=c(100,100000))+
       # geom_hline(yintercept=gap_widening, color="red",linetype = "dotted",size=0.3) +
        geom_text_repel(data=subset(datos_speed,(Year%%5 == 0) | (Year==max(Year)) | (Year==min(Year))),
                    aes(label=sprintf("'%02d",Year%%100),y = dratio_dt_mmov, x = ratio), 
                    hjust=-2,vjust=-0.2, size=3.5, color= "black" ) + 
       # scale_x_sqrt(breaks = c(0.01, 0.1, 0.3, 0.5,1))+
        scale_y_continuous(position = "right")+
        ylab("Convergence speed\n") + xlab(paste(criteria,"ratio"))+
        theme_bw()+theme(panel.grid.minor = element_blank(),
                         panel.grid.major = element_line(size = 0.8,linetype = "dotted"),
                         legend.title = element_text(face="bold", size=13),
                         legend.text = element_text( size=11),
                         legend.position = "left",
                         plot.title = element_text(hjust = 0.5,size=16),
                         axis.text.y = element_text(face="bold", size=14),
                         axis.text.x = element_text(face="bold", size=14),
                         axis.title.x = element_text(face="bold", size=13),
                         axis.title.y  = element_text(face="bold", size=13))
      if ((min(datos_speed$ratio) < 1.1*USA_perc_middle) & (max(datos_speed$ratio) > USA_perc_middle))
         pratiospeed <- pratiospeed + geom_vline(xintercept=USA_perc_middle, color="red",linetype = "dotted",size=0.3)
     
      wplot <- 13
      hplot <- 7
      nfile <- paste0(tdir,"/ALL_",country,"_",criteria,"_",mmovper)
      prattitle <- pratiospeed+ ggtitle(country)
      png(paste0(nfile,".png"), width=wplot*ppi, height=hplot*ppi, res=ppi)
      todo <- plot_grid(
        pizda,prattitle,labels=c(" ","D"),
        label_size = 14,
        ncol = 2
        )#+plot_annotation(title = paste(country,criteria),
        #                  theme = theme(plot.title = element_text(size = 18)))
      print(todo)
      dev.off()
      
      if (print_tiff){
        tiff(paste0(nfile,".tiff"), width=wplot*ppi, height=hplot*ppi,res=ppi)
        print(todo)
        dev.off()
      }
      
      wplot <- 8
      hplot <- 7
      nfile <- paste0(tdir,"/PHASE_",country,"_",criteria,"_",mmovper)
      png(paste0(nfile,".png"), width=wplot*ppi, height=hplot*ppi, res=ppi)
      print(prattitle)
      dev.off()
      
      if (print_tiff){
        tiff(paste0(nfile,".tiff"), width=wplot*ppi, height=hplot*ppi,res=ppi)
        print(prattitle)
        dev.off()
      }
      
      wplot <- 7
      hplot <- 6
      nfile <- paste0(tdir,"/TIMELINE_",country,"_",criteria,"_",mmovper)
      png(paste0(nfile,".png"), width=wplot*ppi, height=hplot*ppi, res=ppi)
      ptimes <- plot_grid(
        pEichen, pconvergencespeed+xlab("Year"), labels=c("A","B"),
        label_size = 15,
        ncol = 1
      )      
      
      if (print_tiff){
        tiff(paste0(nfile,".tiff"), width=wplot*ppi, height=hplot*ppi,res=ppi)
        print(ptimes)
        dev.off()
      }
      
      
      print(ptimes)
      dev.off()
    }
    if (count_countries == 1)
      datos_all <- datos_pais
    else
      datos_all <- rbind(datos_all,datos_pais)
  }

  refx <- USA_perc_middle
  refy <- 0
  lp <- unique(datos_all$Country)
  datadist <- data.frame("Country"=c(),"CountryCode"=c(),"distX"=c(),"distY"=c())
  for (k in lp){
    clean_data <- datos_all[(datos_all$Country == k) & !is.na(datos_all$ratio) & 
                            !is.na(datos_all$dratio_dt_mmov),]
    datosx <- datos_all[(datos_all$Country == k) & !is.na(datos_all$ratio),]$ratio
    datosy <- datos_all[(datos_all$Country == k) & !is.na(datos_all$dratio_dt_mmov),]$dratio_dt_mmov
    datadist <- rbind(datadist,data.frame("Country"=k,"CountryCode"= clean_data$CountryCode[1],
                                          "distX"=mean(clean_data$ratio),
                      "distY"=mean(clean_data$dratio_dt_mmov)))
  }
  
  datos_total <- subset(datos_all,select=-c(ga,gb))
  if (length(lcountrycode)>1)  # to avoid accidental overwriting when testing with a short list
     write.csv(datos_total,paste0("data/all_speeds_",criteria,".csv"),row.names = FALSE)
     remove("datos_total")

  
}
