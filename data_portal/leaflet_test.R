library(sp)
library(leaflet)
library(maps)
library(rgdal)

world <- readOGR(dsn = "data/ne_50m_admin_0_countries",
                 layer="ne_50m_admin_0_countries",encoding="UTF-8", verbose=FALSE)

regions <- read.csv("data/country_list2.csv",header=TRUE)
colnames(regions) <- c("COUNTRY", "REGION","CODE","SUBREGION")

country_count <- matrix(nrow=nrow(regions), ncol=2)
rownames(country_count) <- regions$COUNTRY
colnames(country_count) <- c("COUNTRY", "COUNTS")

for (c in regions$COUNTRY){
  subset <- filter(map_data_final, Study_country == c)
  country_count[c,1] <- c
  country_count[c,2] <- as.numeric(n_distinct(subset$aid))
}

rownames(country_count) = NULL
country_count <- as.data.frame(country_count)
country_count <- inner_join(country_count,regions,by="COUNTRY")

oecd2 <- oecd
colnames(oecd2) <- c("COUNTRY","OECD","REGION","CODE","SUBREGION")
oecd2[is.na(oecd2)] <- "OECD"

FINALDATA <- bind_rows(country_count, oecd)

world@data = data.frame(world@data, FINALDATA[match(world@data[,"iso_a3"], FINALDATA[,"CODE"]),])

map <- leaflet(world)

pal <- colorNumeric(
  palette = "Greens",
  domain = world$COUNTS.1[world$COUNTS.1 > 0]
  )

oecdpal <- colorFactor("grey50", domain=world$COUNTS.1[world$COUNTS.1 %in% c("OECD")])

zeropal <- colorFactor("white", domain=world$Z[world$Z %in% c(0)])

map %>%
  addPolygons(
    stroke=FALSE, fillOpacity=1, smoothFactor=0.2,
    color= ~pal(COUNTS)
    ) %>%
  addPolygons(
    stroke=FALSE, fillOpacity=1, smoothFactor=0.2, color= ~oecdpal(OECD.1)
    )

dat <- WDI(country = "all", 
           indicator = "BN.GSR.FCTY.CD.ZS", 
           start = 1960, 
           end = 2012)

dat[["BN.GSR.FCTY.CD.ZS"]] <- round(dat[["BN.GSR.FCTY.CD.ZS"]], 1)

countries2 <- merge(world, 
                    dat, 
                    by.x = "iso_a2", 
                    by.y = "iso2c",                    
                    sort = FALSE)
