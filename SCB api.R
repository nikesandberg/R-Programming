install.packages('pxweb')
library(pxweb)
library(ggplot2)
install.packages("dplyr")
library(dplyr)
vignette(topic="pxweb")
Sys.setlocale(locale = "UTF-8")
d <- pxweb_interactive()


# PXWEB query 
pxweb_query_list <- 
  list("Region"=c("14"),
       "Fordonsslag"=c("10"),
       "ContentsCode"=c("TK1001AC"),
       "Tid"=c("2002","2003","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019","2020","2021","2022","2023"))
# Download data 
px_data <- 
  pxweb_get(url = "https://api.scb.se/OV0104/v1/doris/en/ssd/TK/TK1001/TK1001A/FordonTrafik",
            query = pxweb_query_list)

# Convert to data.frame 
px_data_frame <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text")

## Exploring the data
summary(px_data_frame)

colnames(px_data_frame) <- c('region', 'vechicletype', 'year', 'cars')


plot(cars~year, px_data_frame, type = "o", xlab = "Year", ylab = "Number of Cars in Traffic in Västra Götaland", main = "Cars in Traffic in Västra Götaland 2002-2023", lwd = 2)

px_data_frame[22, 4]/px_data_frame[1, 4]

# PXWEB query 
pxweb_query_list_2 <- 
  list("Region"=c("00"),
       "Fordonsslag"=c("10"),
       "ContentsCode"=c("TK1001AC"),
       "Tid"=c("2002","2003","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019","2020","2021","2022","2023"))

# Download data 
px_data_swe <- 
  pxweb_get(url = "https://api.scb.se/OV0104/v1/doris/en/ssd/TK/TK1001/TK1001A/FordonTrafik",
            query = pxweb_query_list_2)

# Convert to data.frame 
px_data_frame_swe <- as.data.frame(px_data_swe, column.name.type = "text", variable.value.type = "text")

colnames(px_data_frame_swe) <- c('region', 'vechicletype', 'year', 'cars')

plot(cars~year, px_data_frame_swe, type = "o", xlab = "Year", ylab = "Number of Cars in Traffic in Sweden", main = "Cars in Traffic in Sweden 2002-2023", lwd = 2)

px_data_frame_swe[22, 4]/px_data_frame_swe[1, 4]

