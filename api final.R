

## API för data från SCB över personbilar i trafik. Används i inledningen i Rapporten.
## Vi skapar även ett linjediagram för att illustrera förändringen.


install.packages("pxweb")
library(pxweb)
library(dplyr)
library(tidyverse)



#pxweb_interactive("https://api.scb.se/OV0104/v1/doris/sv/ssd/START/TK/TK1001/TK1001A/PersBilarA")

# Download data 
px_data <- 
  pxweb_get(url = "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/TK/TK1001/TK1001A/PersBilarA",
            query = "C:/Users/Matil/ec_utbildning/R-programming/r_prog_ds23-main/r_prog_ds23-main/api.json")

# Convert to data.frame 
px_data_frame <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text")

# Get pxweb data comments 
px_data_comments <- pxweb_data_comments(px_data)
px_data_comments_df <- as.data.frame(px_data_comments)

# Cite the data as 
pxweb_cite(px_data)

names(px_data_frame)[names(px_data_frame) == "Personbilar i trafik"] <- "Bilar"

str(px_data_frame)

view(px_data_frame)

attach(px_data_frame)
table(år, Bilar)
plot(år, Bilar, type = "l", col = "red", main = "Bilar i trafik 2002 till 2022")
