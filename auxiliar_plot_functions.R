# Function to format axis labels
scaleFUN <- function(x) sprintf("%2.1f ", x)

# Save plot to tiff file
print_2_tiff <- function(print_tiff,plot,nfile,wplot,hplot)
{
  if (print_tiff){
    tiff(paste0(nfile,".tiff"), width=wplot*ppi, height=hplot*ppi,res=ppi)
    print(plot)
    dev.off()
  }
}

# Save plot to png file
print_2_png <- function(plot,nfile,wplot,hplot)
{
  png(paste0(nfile,".png"), width=wplot*ppi, height=hplot*ppi, res=ppi)
  print(plot)
  dev.off()
}
