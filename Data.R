library(data.table)
library(tidyverse)
library(ggplot2)

list_dataframe = list.files(path = "Data", pattern = "*.csv") %>%
  paste("Data/",.,sep = "") %>%
  lapply(.,function(x) fread(x))
###############################################################################
Import.Data=function(list.of.dataframe){
 
  Final_Data=NULL
  
  for(i in 1:length(list.of.dataframe)){
    Data=list.of.dataframe[[i]] %>% 
      spread(key = value_type,value = value,drop = T)
    
    Data$timestamp=format(Data$timestamp,"%d/%m/%y %H:%M")
    Data$Date=as.Date(Data$timestamp,format = "%d/%m/%y")
    Data$Hour=hour(Data$timestamp)
    Data$Minute=minute(Data$timestamp)
    
    Data1=split(Data,list(Data$sensor_type))
    Data1=lapply(Data1,function(x)x %>% select(where(~!all(is.na(.)))))
    
    Data2=merge(Data1$DHT22,Data1$pms5003,
                by=c("Date","Hour","Minute","location","lat","lon")) %>% 
      select("Date","location","lat","lon","temperature","humidity","P0",
             "P1","P2")
    Final_Data=rbind(Final_Data,Data2)
  } 
  return(Final_Data)
}

a=Sys.time()
Data=Import.Data(list_dataframe)
b=Sys.time()

difftime(b,a)
###############################################################################
Data=Import.Data(list_dataframe)

average=aggregate(cbind(temperature,humidity,P0,P1,P2)~location,
                  data = Data,FUN = mean)
average
median=aggregate(cbind(temperature,humidity,P0,P1,P2)~location,
                 data = Data,FUN = median)
median
###############################################################################
Data1=Data[,c(2,5,6)] %>% gather("value_type","value",-location)

ggplot(Data1,aes(x=factor(location),y=value,fill=value_type)) + 
  geom_boxplot() +
  labs(title = "Boxplot of Temperature and Humidity",x="Location")
###############################################################################
subset(Data,location==33)[1,]
subset(Data,location==3576)[1,]
###############################################################################
