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
country_count$COUNTS <- as.numeric(as.vector(country_count$COUNTS))
country_count <- bind_rows(country_count, oecd)

#zeros <- filter(country_count, COUNTS == 0)
#colnames(zeros) <- c("COUNTRY","N","REGION","CODE","SUBREGION")
# oecd2 <- oecd
# colnames(oecd2) <- c("COUNTRY","OECD","REGION","CODE","SUBREGION")
# nonzeros <- filter(country_count, COUNTS != 0)
# nonzeros$COUNTS <- as.numeric(as.vector(nonzeros$COUNTS))

l <- list(color = toRGB("grey"), width = 0.5)
g <- list(
  scope = "south africa",
  resolution = 50,
  showframe = FALSE,
  projection = list(type = 'Mercator'),
  showcoastlines = F,
  showcountries = T,
  countrycolor = toRGB("white"),
  coastlinecolor = toRGB("white")
)

col <- colorRampPalette(c("white", "#74c476","#41ab5d", "#238b45", "#006d2c", "#00441b")) (34)

plot_ly(country_count, z=COUNTS, type = 'choropleth', locations = CODE, 
        text = COUNTRY,
        color = COUNTS, colors=col, inherit = F, marker=list(line=l), colorbar = list(title="No. of unique articles")) %>%
  layout(title = 'Global distribution of evidence base<br>1970-2014',
         geo = g)
