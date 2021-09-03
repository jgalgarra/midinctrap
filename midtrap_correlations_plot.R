# Script ------------
# List lcriteria allows to choose GDP, GNI or both
#
# Author: Javier Garcia-Algarra
# August 2021
# 
# Inputs:  input_data/countries_msci.csv
#          input_data/<countries by region for regression>.csv
#          output_data/all_speeds_criteria.csv"
# Results: figs/ALL_DISTANCES*.png .tiff

#          output_data/KSdist_speed_criteria.csv

library(reshape2)
library(ggplot2)
library(ggrepel)
library(cowplot)
library(seewave)
library(patchwork)
library(tidyverse)
library(corrplot)
library(grid)
library(gridExtra)
library(igraph)


myfun <- function(M){
  corrplot(M,order="hclust",tl.col="black",tl.cex = 0.8,lg.cex=1,
           type = 'upper', diag = FALSE)
  recordPlot() # record the latest plot
}

networkdist <- function(dframe)
{
  npaises <- ncol(dframe)
  MKol <- matrix(nrow = npaises,ncol = npaises)
  rownames(MKol) <- names(dframe)
  colnames(MKol) <- names(dframe)
  for (i in 1:nrow(MKol))
    for (j in 1:ncol(MKol))
      MKol[i,j] <- ks.dist(dframe[,i],dframe[,j])$D
  
  melted_Kol <- melt(MKol, na.rm = TRUE)
  melted_Kol <- melted_Kol[melted_Kol$Var1 != melted_Kol$Var2,]
  write.csv(melted_Kol,paste0("output_data/KSdist_speed_",criteria,"_",start_year,"_",end_year,".csv"))
  dKol <- dist(melted_Kol)
  g1 <- graph_from_adjacency_matrix(MKol, mode = "undirected", weighted = TRUE) %>%
    set_vertex_attr("color", value = "blue")  %>%
    set_vertex_attr("Category", value = "Developed") %>%
    set_edge_attr("color", value = "white")
  colorvertex <- c("lightblue","orange")
  for (i in 1:length(V(g1)))
    V(g1)$color[i] <- colorvertex[(countries_msci[countries_msci$ISOCode == names(V(g1)[i]),]$Category == "Developed")+1]
  dframe[,is.element(names(dframe),countries_msci[countries_msci$Category == "Developed",]$ISOCode)]
  l <- layout_with_kk(g1)
  par(mar=c(1,1,1,1))
  vsizes <- rep(0,length(V(g1)))
  for (i in 1:length(vsizes))
    vsizes[i] <- sqrt(mean(datos_all[datos_all$CountryCode==names(V(g1)[i]),]$ratio))
  plot(g1,layout=l, diag=FALSE,
       vertex.size=7+9*(vsizes),
       vertex.color = adjustcolor(V(g1)$color, alpha.f = .4),
       vertex.label.cex=0.7,
       vertex.label.color="black",
       vertex.label.family="Helvetica",
       vertex.frame.color="transparent")
  recordPlot() # record the latest plot
}



criteria <- read.table("config_data/criteria.txt")
lcriteria <- criteria$V1

tdir <- "figs"
if (!dir.exists(tdir))
  dir.create(tdir)

lstart_year <- c(1980,2000)
end_year <- 2019
countries_msci <- read.csv("input_data/countries_msci.csv")
group_ASIA <- read.csv(paste0("input_data/group_ASIA.csv"))
group_LATAM <- read.csv(paste0("input_data/group_LATAM.csv"))
group_AFRICA <- read.csv(paste0("input_data/group_AFRICA.csv"))
group_EUROPA <- read.csv(paste0("input_data/group_EUROPA.csv"))

lmagnitud <- c("ratio","speed")

onlyMSCI <- TRUE


for (criteria in lcriteria)
{
  datos_all <- read.csv(paste0("output_data/all_speeds_",criteria,".csv"))
  print(criteria)
  for (start_year in lstart_year)
  {
    for (magnitud in lmagnitud)
    {
        print(magnitud)
        
        datos_all <- datos_all[(datos_all$Year>=start_year) & (datos_all$Year<=end_year),]
      
        lp <- unique(datos_all$CountryCode)
        np <- 1
        datadist <- data.frame("Country"=c(),"CountryCode"=c(),"Region"=c(),"MSCI Category" = c(),"distX"=c(),"distY"=c())
        for (k in lp){
          clean_data <- datos_all[(datos_all$CountryCode == k) & !is.na(datos_all$ratio) & 
                                  !is.na(datos_all$dratio_dt_mmov),]
          datosx <- datos_all[(datos_all$CountryCode == k) & !is.na(datos_all$dratio_dt_mmov),]$dratio_dt_mmov
          datosy <- datos_all[(datos_all$Country == k) & !is.na(datos_all$dratio_dt_mmov),]$dratio_dt_mmov
          datos_MSCI <- countries_msci[countries_msci$ISOCode == k,]
          if ((length(datosx)==end_year-start_year+1) ){
            if ((onlyMSCI) & (datos_MSCI$Category != "None")){
            datadist <- rbind(datadist,data.frame("Country"=k,"CountryCode"= clean_data$CountryCode[1],"Region"=datos_MSCI$Region,
                                                  "MSCI Category"= datos_MSCI$Category, "distX"=mean(clean_data$dratio_dt_mmov),
                                                 "distY"=mean(clean_data$ddratio_dt_mmov_dt_mmov)))
            if (np == 1)
              datosseries <- clean_data
            else
              datosseries <- rbind(datosseries,clean_data)
            np <- np + 1
            }
          } 
          else
            if ((onlyMSCI) & (datos_MSCI$Category != "None"))
              print(paste("Excluded",k))
        }
         
      namesp <- unique(datosseries$CountryCode)
      ncolumns <- length(namesp)
      nullcol <- rep(0,end_year-start_year+1)
      datoscol <- data.frame("1"=nullcol)
      datosframe <- datoscol
      for (k in 2:ncolumns){
        datosframe <- cbind(datosframe,data.frame(k=nullcol))
      }
      names(datosframe)=namesp
      
      for (k in 1:ncolumns){
        print(names(datosframe)[k])
        if (magnitud == "ratio"){
        if (length(datosseries[datosseries$CountryCode==names(datosframe)[k],]$dratio_dt_mmov)==length(datosframe[,k]))
           datosframe[,k] <- datosseries[datosseries$CountryCode==names(datosframe)[k],]$dratio_dt_mmov
        }
        else{
        if (length(datosseries[datosseries$CountryCode==names(datosframe)[k],]$ratio)==length(datosframe[,k]))
          datosframe[,k] <- datosseries[datosseries$CountryCode==names(datosframe)[k],]$ratio
        }
      }
      removecols <- c()
      for (k in 1:ncolumns)
        if (sum(datosframe[,k] == 0))
          removecols <- c(removecols,k)
      if (!is.null((removecols)))
        datosframe <- datosframe[,-removecols]
  
      M<-cor(datosframe,use="complete.obs",method="spearman")
      p <- myfun(M)
      if (magnitud=="speed")
       pspeed <- p
      else
       pratio <- p
      wplot <- 8.5
      hplot <- 8
      nfile <- paste0(tdir,"/CORRELATION_",criteria,"_",magnitud,"_",start_year,"_",end_year)
      ppi <- 300
      png(paste0(nfile,".png"), width=wplot*ppi, height=hplot*ppi, res=ppi)
      print(p)
      dev.off()
      png(paste0(nfile,".tiff"), width=wplot*ppi, height=hplot*ppi, res=ppi)
      print(p)
      dev.off()
    } # for magnitud
    
    pnspeedall <- networkdist(datosframe)
    wplot <- 8
    hplot <- 8
    nfile <- paste0(tdir,"/NETWORKSPEED_",criteria,"_",magnitud,"_",start_year,"_",end_year)
    ppi <- 300
    png(paste0(nfile,".png"), width=wplot*ppi, height=hplot*ppi, res=ppi)
    print(pnspeedall)
    dev.off()
    tiff(paste0(nfile,".tiff"), width=wplot*ppi, height=hplot*ppi,res=ppi)
    print(pnspeedall)
    dev.off()
  }  # for start_year
  
}  # for criteria 
