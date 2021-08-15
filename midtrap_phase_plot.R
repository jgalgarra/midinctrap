# Script to generate all magnitudes
# 
# List lcriteria allows to choose GDP, GNI or both
#
# Author: Javier Garcia-Algarra
# August 2021
# 
# Inputs:  data/GDP2010_WB.csv    GNIS.csv
# Results: data/all_speeds_criteria.csv
#          figs/countries

library(ggplot2)
library(cowplot)
library(patchwork)
library(zoo)


lcriteria <- c("GDP","GNI")
lcriteria <- c("GNI")
lcountry <- c("Brazil","Chile","China","Colombia","Mexico","Morocco","Malaysia","Korea, Rep.","Puerto Rico",
              "Nigeria","Vietnam","Peru","Bulgaria", "Thailand", "Paraguay","Bolivia","Lebanon",
              "India","Argentina","Pakistan","Philippines","Indonesia","Turkey","Ecuador","Guatemala","France","Italy",
              "South Africa","Uruguay","Botswana","Costa Rica","Panama","Nicaragua","El Salvador","Portugal","Spain")
#lcountry <- c("Poland","Romania","Bulgaria","Hungary","Belarus","Serbia")

criteria <- read.table("criteria.txt")
lcriteria <- criteria$V1

countries_msci <- read.csv("data/countries_msci.csv")
lcountrycode <- countries_msci$ISOCode

#lcountrycode <- c("ARG")   # Pick one country to test

USA_perc_middle <- 0.3
gap_widening <- -0.7
mmovper <- 5
print_indiv <- TRUE      # Print individual files
last_year_Ecihengreen <- 2013
tdir1 <- "figs"
tdir2 <- paste0(tdir1,"/countries")
if (!dir.exists(tdir2))
  dir.create(tdir1)
  dir.create(tdir2)

for (criteria in lcriteria)
{
  count_countries <- 0  
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
    period_calc = 7
    for (i in 1960:2020){
      datos_pais$Magnitude[i-(1960)] <- as.numeric(rawdata[,4+i-1960])
      datos_pais$USAMagnitude[i-(1960)] <- as.numeric(usadata[,4+i-1960]) # USA GDP or GNI
      datos_pais$ratio[i-(1960)] <- datos_pais$Magnitude[i-(1960)]/datos_pais$USAMagnitude[i-(1960)]
    }
    for (j in 2:nrow(datos_pais))
      datos_pais$growth[j] <- 100*(datos_pais$Magnitude[j] - datos_pais$Magnitude[j-1])/
                              (datos_pais$Magnitude[j-1])  # Magnitude growth
    for (j in 2:nrow(datos_pais)){
      datos_pais$dgrowth_dt[j] <- (datos_pais$growth[j] - datos_pais$growth[j-1])
      # Gap closing speed
      datos_pais$dratio_dt[j] <- 100*(datos_pais$ratio[j] - datos_pais$ratio[j-1])
    }
    datos_pais$dratio_dt_mmov <- rollmean(datos_pais$dratio_dt, mmovper, fill = NA,  align = "right")
    
    for (j in 2:nrow(datos_pais))
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
    datosplot$gb = (floor(datosplot$gb* 10)+1)/10
    datosplot$dif = (floor(datosplot$dif* 10)+1)/10
    for (k in 1:nrow(datosplot)){
      datosplot$Trapped[k] = (datosplot$Magnitude[k]>minMagnitude) & (datosplot$gb[k]>=mingb) & (datosplot$dif[k] <= mindif)
    }
    pEichen <- ggplot(data= datosplot, aes(x=Year,y=dif,color=Trapped))+
         geom_point(size=2)+ylab(paste("Dif Eichengreen",criteria))+
         scale_color_discrete(limits = c('TRUE', 'FALSE'))+
         xlim(limityears)+theme_bw()+theme(legend.position="top")
    
    datosEich <- datos_pais[(datos_pais$Year >= min(datosplot$Year))&
                              (datos_pais$Year<last_year_Ecihengreen),]
    datosEich <- datos_pais 
    pClosingGapSpeed <- ggplot(data= datosEich, aes(x=Year,y=-dratio_dt_mmov))+
      geom_point(size=2,col="black",alpha=0.5)+ylab(paste("Convergence speed",criteria))+
      xlim(limityears)+theme_bw()
    
    pratio <- ggplot(data= datosEich, aes(x=Year,y=ratio))+
      geom_point(size=2,col="black",alpha=0.5)+ylab(paste("Ratio",criteria))+
      xlim(limityears)+theme_bw()
    
    pizda <- plot_grid(
      pEichen, pClosingGapSpeed, pratio,
      ncol = 1
    )
    
    datos_pais$Country = country
    datos_pais$CountryCode = rawdata$Country.Code[1]
    if (print_indiv){
      ppi <- 300
      nfile <- paste0(tdir,"/ALL_",country,"_",criteria,"_",mmovper)
      png(paste0(nfile,".png"), width=12*ppi, height=7*ppi, res=ppi)

      my_breaks <- c(100,500,1000,2500,5000,10000,20000,50000)
      my_breaks <- c(100,500,5000,50000)
      datos_speed <- datos_pais[!is.na(datos_pais$dratio_dt_mmov),]
      pratiospeed <- ggplot(data=datos_speed) +
        geom_path(
          aes(y = -dratio_dt_mmov, x = ratio, colour=Magnitude),alpha=0.5,size = 1)+
        geom_point(aes(y = -dratio_dt_mmov, x = ratio, colour=Magnitude),size=2,
                 alpha=1) +
        scale_color_gradientn(name=criteria,colours=c("violet","blue","green","yellow","orange","red"),trans="log",
                                                      breaks = my_breaks, labels = my_breaks,limits=c(100,100000))+
        geom_hline(yintercept=gap_widening, color="red",linetype = "dotted",size=0.3) +
        geom_vline(xintercept=USA_perc_middle, color="red",linetype = "dotted",size=0.3) +
        geom_text_repel(data=subset(datos_speed,(Year%%5 == 0) | (Year==max(Year)) | (Year==min(Year))),
                    aes(label=Year,y = -dratio_dt_mmov, x = ratio, vjust=0.1),
                    size=4 ,color="black") + 
        scale_x_log10()+
        ylab("Convergence speed") + xlab(paste(criteria,"ratio"))+
        theme_bw()+theme(panel.grid.minor = element_blank(),
                         panel.grid.major = element_line(size = 0.3,linetype = "dashed"),
                         legend.title = element_text(face="bold", size=15),
                         legend.text = element_text( size=11),
                         axis.text.y = element_text(face="bold", size=14),
                         axis.text.x = element_text(face="bold", size=14),
                         axis.title.x = element_text(face="bold", size=16),
                         axis.title.y  = element_text(face="bold", size=16))


      pacc <- ggplot(data=datos_pais) +
        geom_path(
          aes(y = -dratio_dt2_mmov, x = -dratio_dt_mmov),color="blue",alpha=0.3,
          linetype = "dotted", size = 0.5)+
        geom_point(aes(y = -dratio_dt2_mmov, x = -dratio_dt_mmov, size=ratio),
                   color="purple",alpha=0.2) +
        geom_text(data=subset(datos_pais,(Year%%5 == 0)),
                  aes(label=Year,y = -dratio_dt2_mmov, x = -dratio_dt_mmov, vjust=1, hjust=1.2),
                  size=2,color="purple") + xlab("Gap closing speed") + ylab("Gap closing acceleration") +
        theme_bw()+theme(legend.position="bottom",
                         panel.grid.minor = element_blank(),
                         panel.grid.major = element_line(size = 0.25))
      
      pdcha <- plot_grid(
        pratiospeed, pacc,
        ncol = 1
      )
      todo <- plot_grid(
        pizda,pratiospeed,
        ncol = 2
        )+plot_annotation(title = paste(country,criteria))
      print(todo)
      dev.off()
      
      
      nfile <- paste0(tdir,"/PHASE_",country,"_",criteria,"_",mmovper)
      png(paste0(nfile,".png"), width=8*ppi, height=7*ppi, res=ppi)
      print(pratiospeed)
      dev.off()
      
      nfile <- paste0(tdir,"/TIMELINE_",country,"_",criteria,"_",mmovper)
      png(paste0(nfile,".png"), width=7*ppi, height=10*ppi, res=ppi)
      ptimes <- plot_grid(
        pEichen, pClosingGapSpeed, pratio,labels=c("A","B","C"),
        label_size = 16,
        ncol = 1
      )
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
  write.csv(datos_total,paste0("data/all_speeds_",criteria,".csv"),row.names = FALSE)

  
}
