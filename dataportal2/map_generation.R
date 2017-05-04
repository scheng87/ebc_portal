#Isolate country data and add region column
country_data <- select(map_data_final,aid,Study_country)
reg <- read.csv("data/allcountries.csv",header=TRUE)
#create blank data matrix
rows <- c(1:nrow(country_data))
reg_data <- matrix(nrow=nrow(country_data),ncol=2)
rownames(reg_data) <- rows
colnames(reg_data) <- c("region","subregion")

#Assign regions
for (i in rows){
  country <- country_data$Study_country[i]
  if (country == "Unknown" | country == ""){
    reg_data[i,"region"] <- "Unknown"
    reg_data[i,"subregion"] <- "Unknown"
  } else if (country == "Global") {
    reg_data[i,"region"] <- "Global"
    reg_data[i,"subregion"] <- "Global"
  } else {
    sub <- filter(reg,Country == country)
    reg_data[i,"region"] <- as.character(sub$Region)
    reg_data[i,"subregion"] <- as.character(sub$Subregion)
  }
}

reg_data <- as.data.frame(reg_data)
country_data_final <- bind_cols(country_data,reg_data)
country_data_final <- select(country_data_final,-aid,-Study_country)

#Bind with map_data
map_data_final <- select(map_data_final,-region,-subregion)
map_data_final <- bind_cols(map_data_final,country_data_final)

saveRDS(map_data_final,file="data/map_data_final.rds")
