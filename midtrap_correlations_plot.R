# Script ------------
# List lcriteria allows to choose GDP, GNI or both
#
# Author: Javier Garcia-Algarra
# August 2021
# 
# Inputs:  input_data/countires_msci.csv
#          input_data/<countries by region for regression>.csv
#          output_data/all_speeds_criteria.csv"
# Results: figs/ALL_DISTANCES*.png .tiff

library(reshape2)
library(ggplot2)
library(ggrepel)
library(cowplot)

library(patchwork)


library(corrplot)
library(grid)
library(gridExtra)


myfun <- function(M){
  corrplot(M,order="hclust",tl.col="black",tl.cex = 0.8,
           type = 'upper', diag = FALSE)
  recordPlot() # record the latest plot
}


criteria <- read.table("config_data/criteria.txt")
lcriteria <- criteria$V1

tdir <- "figs"
if (!dir.exists(tdir))
  dir.create(tdir)

start_year <- 1980
end_year <- 2019
countries_msci <- read.csv("input_data/countries_msci.csv")
group_ASIA <- read.csv(paste0("input_data/group_ASIA.csv"))
group_LATAM <- read.csv(paste0("input_data/group_LATAM.csv"))
group_AFRICA <- read.csv(paste0("input_data/group_AFRICA.csv"))
group_EUROPA <- read.csv(paste0("input_data/group_EUROPA.csv"))

lmagnitud <- c("ratio","speed")

onlyMSCI <- TRUE

#for (criteria in lcriteria)
for (magnitud in lmagnitud)
{
    print(criteria)
    datos_all <- read.csv(paste0("output_data/all_speeds_",criteria,".csv"))
    datos_all <- datos_all[(datos_all$Year>=start_year) & (datos_all$Year<=end_year),]
  
    lp <- unique(datos_all$CountryCode)
    #lp <- c("CHN","ARG","IRL","ESP","PRT","SGP","ZWE")
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
  nfile <- paste0(tdir,"/CORRELATION_",criteria,"_",magnitud)
  ppi <- 300
  png(paste0(nfile,".png"), width=wplot*ppi, height=hplot*ppi, res=ppi)
  print(p)
  dev.off()
}

todo <- plot_grid(
  pratio,pspeed,labels=c("A","B"),
  label_size = 16,
  ncol = 2
)
# corrplot(M,order="hclust",tl.col="black",tl.cex = 0.6,
#          type = 'upper', diag = FALSE)

#npaises <- ncol(datosframe)
# MKol <- matrix(nrow = npaises,ncol = npaises)
# rownames(MKol) <- names(datosframe)
# colnames(MKol) <- names(datosframe)
# for (i in 1:nrow(MKol))
#   for (j in 1:ncol(MKol))
#     MKol[i,j] <- ks.dist(datosframe[,i],datosframe[,j])$D
# 
# auxorden <- data.frame("CountryCode" = rownames(MKol))
# auxorden$sbyrows <- rep(0,npaises)
# for (k in 1:npaises)
#   auxorden$sbyrows[k] <- sum(MKol[k,])
# ordMKol <- (order(auxorden$sbyrows))
# MKol <- MKol[rev(ordMKol),ordMKol]
# melted_Kol <- melt(MKol, na.rm = TRUE)
# 
# melted_Kol$sqvalue <- sqrt(melted_Kol$value)
# 
# p <- ggplot(data = melted_Kol, aes(Var2, Var1, fill = sqvalue))+
#   geom_tile(color = "white")+
#   scale_fill_gradient(low = "white", high = "red", 
#                        limit = c(0,max(melted_Kol$sqvalue)), space = "Lab", 
#                        name="KS distance") +
#   theme_minimal()+ 
#   theme(axis.text = element_text(vjust = 1, 
#                                    size = 6, hjust = 1),
#         axis.text.x = element_text(angle = 90))+
#   coord_fixed()
# 
# plot(p)
# MPKol <- (1-MKol)/max(1-MKol)
# corrplot(MPKol,tl.col="black",tl.cex = 0.5, #cl.lim=c(min(MPKol),max(MPKol)),
#          #col=colorRampPalette(c("grey","white","blue"))(2000),
#          cl.labels = c("a","b","c"),
#          order="hclust")#,
#          #is.corr = FALSE)