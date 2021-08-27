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
# Inputs:  input_data/GDP2010_WB.csv    GNIS.csv
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

# Function to generate a phase plot
phase_plot <- function(datos,xlabel="",ylabel="",xint=NA,yint=NA,
                       axisright=FALSE,legendpos="none",mbreaks=c(),pais="")
{      
  pphase <- ggplot(data=datos) +
    geom_path(
      aes(y = Y, x = X, colour=Magnitude),alpha=0.4,size = 0.6,
      arrow = arrow(length = unit(0.2, "cm"),type="closed"))+
    geom_point(aes(y = Y, x = X, colour=Magnitude), size = 2.5,
               alpha=0.8) +
    geom_point(data=datos[1,],aes(y = Y, x = X, colour=Magnitude), size = 3,
               alpha=1,shape=23, stroke=1) +
    geom_point(data=datos[nrow(datos),],aes(y = Y, x = X, colour=Magnitude), size = 3,
               alpha=1,shape=23, stroke=1) +
    scale_color_gradientn(name=criteria,colours=c("violet","blue","green","orange","red"),trans="log",
                          breaks = mbreaks, labels = mbreaks,limits=c(100,100000))+
    geom_text_repel(data=subset(datos,(Year%%4 == 0) | (Year==max(Year)) | (Year==min(Year))),
                    aes(label=sprintf("'%02d",Year%%100),y = Y, x = X),
                    hjust=-2,vjust=-0.2, size=3.5, color= "black" )+
    xlab(paste(pais,xlabel)) + ylab(ylabel)+ theme_bw()+
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_line(size = 0.8,linetype = "dotted"),
          legend.title = element_text(face="bold", size=13),
          legend.text = element_text( size=11),
          legend.position = legendpos,
          axis.text.y = element_text(face="bold", size=14),
          axis.text.x = element_text(face="bold", size=14),
          axis.title.x = element_text(face="bold", size=13),
          axis.title.y  = element_text(face="bold", size=13),
          plot.title = element_text(hjust = 0.5,size=16))
  if (!is.na(xint))
    pphase <- pphase + geom_vline(xintercept=minconvspeed, color="red",linetype = "dotted",size=0.5)
  if (!is.na(yint))
    pphase <- pphase + geom_hline(yintercept=0, color="red",linetype = "dotted",size=0.5)
  if (axisright)
    pphase <- pphase + scale_y_continuous(position = "right")
  return(pphase)
}



criteria <- read.table("config_data/criteria.txt")
lcriteria <- criteria$V1

configuration_file <- read.table("config_data/config_plots.csv", sep=";", header=TRUE)
if (configuration_file$CountryCode == "MSCI"){           # Plot MSCI countries
  countries_msci <- read.csv("input_data/countries_msci.csv")
  lcountrycode <- countries_msci$ISOCode
} else 
  lcountrycode <- configuration_file$CountryCode

print_indiv <- configuration_file$print_indiv      # Print individual files
print_tiff <- configuration_file$print_tiff        # Produce tiff files. Be careful, each tiff file weights 24 MB!

USA_perc_middle_GNI <- 0.3
USA_perc_middle_GDP <- 0.5
gap_widening_GDP <- 0
gap_widening_GNI <- 0
mmovper <- 5
last_year_Ecihengreen <- 2013
mingb_GDP <- 3.5
mindif_GDP <- -2
minMagnitude_GDP <- 10000
mingb_GNI <- 3.5
mindif_GNI <- -2
minMagnitude_GNI <- 10000
minconvspeed_GDP <- 0.7
minconvspeed_GNI <- 2
tdir1 <- "figs"
tdir <- paste0(tdir1,"/countries")
if (!dir.exists(tdir)){
  dir.create(tdir1)
  dir.create(tdir)
}
ppi <- 300
my_breaks <- c(100,500,1000,2500,5000,10000,20000,50000)
my_breaks <- c(100,500,5000,50000)
initial_year <- 1960
final_year <- 2020


for (criteria in lcriteria)
{
  count_countries <- 0
  if (criteria == "GDP"){
    USA_perc_middle <- USA_perc_middle_GDP
    mingb <- mingb_GDP
    mindif <- mindif_GDP
    minMagnitude <- minMagnitude_GDP
    gap_widening <- gap_widening_GDP
    minconvspeed <- minconvspeed_GDP
  }
  else {
    USA_perc_middle <- USA_perc_middle_GNI
    mingb <- mingb_GNI
    mindif <- mindif_GNI
    minMagnitude <- minMagnitude_GNI
    gap_widening <- gap_widening_GNI
    minconvspeed <- minconvspeed_GNI
  }
  for (countrycode in lcountrycode)
  {
    count_countries <- count_countries + 1
    print(paste(countrycode,criteria))
    if (criteria == "GNI"){
      DATA_WB <- read.csv("input_data/GNIS.csv")
    } else
      DATA_WB <- read.delim2("input_data/GDP2010_WB.csv")
    rawdata <- DATA_WB[DATA_WB$Country.Code==countrycode,]
    usadata <- DATA_WB[DATA_WB$Country.Name=="United States",]
    country <- DATA_WB[DATA_WB$Country.Code==countrycode,]$Country.Name
    datos_pais <- data.frame("Year"=seq(initial_year,final_year-1))
    datos_pais$Magnitude = 0
    datos_pais$growth = 0
    datos_pais$gb = 0      # Average growth backwards, for Eichengreen criterium
    datos_pais$ga = 0      # Average growth forward
    datos_pais$dif = 0     # Difference among ga and gb for Eichengreen criterium
    datos_pais$dgrowth_dt = 0
    datos_pais$USAMagnitude = 0
    datos_pais$ratio = NA
    datos_pais$drat4rio_dt = 0
    datos_pais$dratio_dt_mmov = 0
    datos_pais$dratio_dt2_mmov = NA
    period_calc = 7
    for (i in initial_year:final_year){
      datos_pais$Magnitude[i-(initial_year)] <- as.numeric(rawdata[,4+i-initial_year])
      datos_pais$USAMagnitude[i-(initial_year)] <- as.numeric(usadata[,4+i-initial_year]) # USA GDP or GNI
      datos_pais$ratio[i-(initial_year)] <- datos_pais$Magnitude[i-(initial_year)]/datos_pais$USAMagnitude[i-(initial_year)]
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
    limityears <- c(initial_year,final_year)
    yearlabels <- seq(initial_year,final_year,by=10) 
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
    datosEich$signacc <- (datosEich$dratio_dt2_mmov < gap_widening) & (datosEich$dratio_dt_mmov > minconvspeed)
    datos_speed <- datosEich[!is.na(datosEich$dratio_dt_mmov),]
    datos_speed[is.na(datos_speed$dratio_dt2_mmov),]$signacc <- FALSE
    pconvergencespeed <- ggplot(data= datos_speed, aes(x=Year,y=dratio_dt_mmov,color=signacc))+
     geom_point(size=2)+ylab(paste("Conv. speed"))+
      scale_x_continuous(limits=limityears,breaks=yearlabels,labels=yearlabels)+
      scale_y_continuous(labels=scaleFUN)+
      scale_color_discrete(limits = c('TRUE', 'FALSE'))+
      theme_bw()+ xlab("")+labs(colour="Trapped")+
      theme(axis.text.y = element_text(face="bold", size=12),
            axis.text.x = element_text(face="bold", size=12),
            axis.title.x = element_text(face="bold", size=13),
            axis.title.y  = element_text(face="bold", size=13))

    pratio <- ggplot(data= datosEich, aes(x=Year,y=ratio))+
      geom_point(size=2,col="black",alpha=0.5)+ylab(paste(criteria,"Ratio"))+
      scale_x_continuous(limits=limityears, breaks=yearlabels,labels=yearlabels)+
      theme_bw() + xlab("Year")+
      theme(axis.text.y = element_text(face="bold", size=12),
                                axis.text.x = element_text(face="bold", size=12),
                                axis.title.x = element_text(face="bold", size=13),
                                axis.title.y  = element_text(face="bold", size=13))
    
    pacc <- ggplot(data= datosEich, aes(x=Year,y=dratio_dt2_mmov))+
      geom_point(size=2)+ylab(paste("Conv. accel."))+
      scale_x_continuous(limits=limityears, breaks=yearlabels,labels=yearlabels)+
      scale_y_continuous(labels=scaleFUN)+
      geom_hline(yintercept=gap_widening, color="red",linetype = "dotted",size=0.6) +
      theme_bw() + 
      theme(legend.position = "none",
            axis.text.y = element_text(face="bold", size=12),
            axis.text.x = element_text(face="bold", size=12),
            axis.title.x = element_text(face="bold", size=13),
            axis.title.y  = element_text(face="bold", size=13))
    if (criteria == "GDP")
      pizda <- plot_grid(
        pEichen, pconvergencespeed+theme(legend.position="none"), pratio, labels=c("A","B","C"),
        label_size = 15,rel_heights = c(0.38,0.31,0.31),
        ncol = 1
      )
    else
      pizda <- plot_grid(
        pconvergencespeed+theme(legend.position="top"), pratio, labels=c("A","B"),
        label_size = 15,rel_heights = c(0.55,0.45),
        ncol = 1
      )
    datos_pais$Country = country
    datos_pais$CountryCode = rawdata$Country.Code[1]
    if (print_indiv){
      datos_speed <- datos_pais[!is.na(datos_pais$dratio_dt_mmov),]
      datos_acc <- datos_speed[!is.na(datos_speed$dratio_dt2_mmov),]
      datos_speed$X <- datos_speed$ratio
      datos_speed$Y <- datos_speed$dratio_dt_mmov
      pratiospeed <- phase_plot(datos_speed,xlabel=paste(criteria,"Ratio"),ylabel="Convergence speed",
                        xint=NA, yint=NA,axisright = TRUE,legendpos = "left",mbreaks = my_breaks)
      pratiospeedleft <- phase_plot(datos_speed,xlabel=paste(criteria,"ratio"),ylabel="Convergence speed",
                                xint=NA, yint=NA,axisright = FALSE,legendpos = "none",
                                mbreaks = my_breaks,pais = country)
      datos_acc$X <- datos_acc$dratio_dt_mmov
      datos_acc$Y <- datos_acc$dratio_dt2_mmov
      pspeedacc <- phase_plot(datos_acc,xlabel="Convergence speed",ylabel="Acceleration",
                              xint=minconvspeed, yint=0,axisright = FALSE,
                              legendpos = "right",mbreaks = my_breaks)
  
      
      pphases <- plot_grid(
        pratiospeedleft, pspeedacc, labels=c("A","B"),
        label_size = 15,
        ncol = 2, rel_widths = c(0.47,0.53)
      )
      
      wplot <- 13
      hplot <- 8
      nfile <- paste0(tdir,"/ALL_",country,"_",criteria,"_",mmovper)
      prattitle <- pratiospeed+ ggtitle(paste("\n",country))
      png(paste0(nfile,".png"), width=wplot*ppi, height=hplot*ppi, res=ppi)
      if (criteria == "GDP"){
        labelphase <- "D"
      } else
        labelphase <- "C"
      todo <- plot_grid(
        pizda,prattitle,labels=c(" ",labelphase),
        label_size = 14,rel_widths = c(0.45,0.55),
        ncol = 2
        )
      print(todo)
      dev.off()
      
      if (print_tiff){
        tiff(paste0(nfile,".tiff"), width=wplot*ppi, height=hplot*ppi,res=ppi)
        print(todo)
        dev.off()
      }
      
      wplot <- 7
      hplot <- 7
      nfile <- paste0(tdir,"/RATIOSvsSPEED_",country,"_",criteria,"_",mmovper)
      png(paste0(nfile,".png"), width=wplot*ppi, height=hplot*ppi, res=ppi)
      print(pratiospeedleft)
      dev.off()
      
      if (print_tiff){
        tiff(paste0(nfile,".tiff"), width=wplot*ppi, height=hplot*ppi,res=ppi)
        print(prattitle)
        dev.off()
      }
      
      nfile <- paste0(tdir,"/SPEEDvsACC_",country,"_",criteria,"_",mmovper)
      png(paste0(nfile,".png"), width=wplot*ppi, height=hplot*ppi, res=ppi)
      print(pspeedacc)
      dev.off()
      
      
      wplot <- 14
      hplot <- 6
      nfile <- paste0(tdir,"/PHASES_",country,"_",criteria,"_",mmovper)
      png(paste0(nfile,".png"), width=wplot*ppi, height=hplot*ppi, res=ppi)
      print(pphases)
      dev.off()
      
      if (print_tiff){
        tiff(paste0(nfile,".tiff"), width=wplot*ppi, height=hplot*ppi,res=ppi)
        print(pphases)
        dev.off()
      }
      
      wplot <- 7
      hplot <- 9
      nfile <- paste0(tdir,"/TIMELINE_",country,"_",criteria,"_",mmovper)
      png(paste0(nfile,".png"), width=wplot*ppi, height=hplot*ppi, res=ppi)
      if (criteria == "GDP")
        ptimes <- plot_grid(
          pEichen, pconvergencespeed+
                   geom_hline(yintercept=minconvspeed, color="red",linetype = "dotted",size=0.6)+
                   theme(legend.position="none"),
          pacc+xlab("Year"),pratio, labels=c("A","B","C","D"),
          label_size = 15,
          ncol = 1
        )
      else
        ptimes <- plot_grid(
          pconvergencespeed+
            geom_hline(yintercept=minconvspeed, color="red",linetype = "dotted",size=0.6)+
            theme(legend.position="top"),
          pacc+xlab("Year"),pratio, labels=c("A","B","C"),rel_heights = c(0.36,0.32,0.32),
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
  
  if (length(lcountrycode)>5)  # to avoid accidental overwriting when testing with a short list
     write.csv(datos_all,paste0("output_data/all_speeds_",criteria,".csv"),row.names = FALSE)
     remove("datos_all")

}
