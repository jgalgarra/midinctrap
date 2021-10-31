# Script to plot a comparison of GDPs
# 
# Author: Javier Garcia-Algarra
# October 2021
# 

# Inputs:  input_data/GDP2010_WB.csv
# Results: figs/COMPARISON_GDPS.png
#          output_data/data_comparison_GDP.csv

library(ggplot2)
library(ggrepel)
library(cowplot)
library(patchwork)
library(zoo)
source("auxiliar_plot_functions.R")

criteria <- "GDP"

initial_year <- 1980
final_year <- 2020

lcountrycode <- c("ESP","ITA","BRA","ZAF","ITA","KOR","CHN")


tdir <- "figs"
if (!dir.exists(tdir)){
  dir.create(tdir)
}
ppi <- 300
my_breaks <- c(100,500,1000,2500,5000,10000,20000,50000)
my_breaks <- c(100,500,5000,50000)
lplotsize <- 17 # Labels for plots
ncount <- 0

for (countrycode in lcountrycode)
  {
    DATA_WB <- read.delim2("input_data/GDP2010_WB.csv")
    rawdata <- DATA_WB[DATA_WB$Country.Code==countrycode,]
    country <- DATA_WB[DATA_WB$Country.Code==countrycode,]$Country.Name
    datos_pais <- data.frame("Year"=seq(initial_year,final_year-1))
    datos_pais$Magnitude = 0
    for (i in initial_year:final_year){
      datos_pais$Magnitude[i-(initial_year)] <- as.numeric(rawdata[,24+i-initial_year])
    }
    datos_pais$Country=country
    datos_pais$countrycode=countrycode
    if (ncount == 0)
      datos_all <- datos_pais
    else
      datos_all <- rbind(datos_all,datos_pais)
    ncount <- ncount + 1
}
data_comp = data.frame("Country"=datos_all$Country,"Year"=datos_all$Year,"GDP_PPP_USD2010"=round(datos_all$Magnitude))
write.csv(data_comp,paste0("output_data/data_comparison_GDP.csv"),row.names = FALSE)
q <- ggplot(datos_all,aes(x=Year,y=Magnitude,color=Country),size=2)+geom_line(alpha=0.5)+
     geom_point(size=1)+
     ylab("GDP per capita (PPP constant 2010 US$)\n")+
     xlim(c(initial_year,final_year-1))+ylim(0,40000)+
     coord_cartesian( expand = FALSE )+theme_bw()+
     theme(panel.border = element_blank(),
           legend.position = "none",
           axis.line = element_line(colour = "black"),
           axis.text.y = element_text(face="bold", size=11),
           axis.text.x = element_text(face="bold", size=11),
           axis.title.x = element_text(face="bold", size=13),
           axis.title.y  = element_text(face="bold", size=13))
data_end <- datos_all[datos_all$Year==2008,]
data_end$Magnitude <- data_end$Magnitude+800
q <- q +   geom_text(
  aes(label = countrycode,color=Country), data = data_end,
  fontface ="bold",  size = 4)
ppi=300
nfile <- paste0(tdir,"/COMPARISON_GDPS")
print_2_png(q,paste0(nfile,".png"),10,6)
print_2_tiff(TRUE,q,paste0(nfile,".tiff"),10,6)

