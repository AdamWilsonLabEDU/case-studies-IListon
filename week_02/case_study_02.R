#load tidyverse
#install.packages("tidyverse")
library("tidyverse")
library(ggplot2)

#link data
dataurl="C:/Users/I.Liston/OneDrive/Documents/Grad School/Fall 2024/GEO 511 (SDS)/Geo 511/Buff_Temp_Data.csv"
dataurl

#load csv
Temp_Data <- read_csv(dataurl, 
                      na="999.90", # tell R that 999.90 means missing in this dataset
                      skip=1, # we will use our own column names as below so we'll skip the first row
                      col_names = c("YEAR","JAN","FEB","MAR", # define column names 
                                    "APR","MAY","JUN","JUL",  
                                    "AUG","SEP","OCT","NOV",  
                                    "DEC","DJF","MAM","JJA",  
                                    "SON","metANN"))

#plot average annual temperature for JJA
Avg_Temp_JJA_Plot <- ggplot(data=Temp_Data, aes(x=YEAR, y=JJA)) + 
  geom_line() + 
  geom_smooth(method="loess", color = "red")+
  ggtitle("Average Temperature during June, July, and August in Buffalo, NY")+
  xlab("Year")+
  ylab("Temperature (in degrees Celcius)")


plot(Avg_Temp_JJA_Plot)

#save plot
ggsave("Avg_Temp_JJA_Plot.png",width = 10,height = 6)

#Class Notes 9-12

#scale_x_continuous()
#theme()
