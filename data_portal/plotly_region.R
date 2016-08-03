input <- "Eastern Africa"

subreg <- filter(regions,SUBREGION == input)

geoRegScope <- reactive({
  if (input$eregi_region == "Africa"){
    scope = "africa"
  } else if (input$eregi_region == "Asia"){
    scope = "asia"
  }
})

test_coor <- data.frame(minlat=-28,maxlat=23,minlon=21,maxlon=52)
g <- list(
  #scope = "north america",
  showframe = F,
  showland = T,
  landcolor = toRGB("grey90")
)

g1 <- c(
  g,
  resolution = 50,
  showcoastlines = T,
  countrycolor = toRGB("white"),
  coastlinecolor = toRGB("white"),
  projection = list(type = 'Mercator'),
  list(lonaxis = list(range = c(test_coor[1,3],test_coor[1,4]))),
  list(lataxis = list(range = c(test_coor[1,1],test_coor[1,2])))
  )


dat <- filter(map_data_final,subregion == input)
dat <- distinct(dat)
int <- select(dat,aid,Study_country,Int_type)
int <- distinct(int)
out <- select(dat,aid,Study_country,Outcome)
out <- distinct(out)
bio <- select(dat,aid,Study_country,Biome.)
bio <- distinct(bio)

countries <- as.list(dat$Study_country)
countries <- unique(countries)

n <- length(countries)
summary <- matrix(nrow=n,ncol=(length(out_type)+2))
rownames(summary) <- countries
colnames(summary) <- c(out_type,"Country","Code")
for (c in countries){
  for (o in out_type){
    subset <- filter(out, Study_country == c, Outcome == o)
    test <- filter(regions, COUNTRY == c)
    code <- as.character(as.vector(test$CODE))
    summary[c,"Country"] <- c
    summary[c,"Code"] <- code
    summary[c,o] <- as.numeric(n_distinct(subset$aid))
  }
}
summary <- as.data.frame(summary)
summary <- gather(summary,out,counts,1:(ncol(summary)-2))
summary$counts <- as.numeric(as.vector(summary$counts))
summary <- filter(summary, out != "env")
summary <- summary %>% group_by(Country) %>% arrange(desc(counts))
top <- top_n(summary,3,counts)
t <- as.vector(top$out)
t.t <- recode(t, "gov" = '"Governance & empowerment"; "soc_rel" = "Social relations"; "education" = "Education"; "eco_liv_std" = "Economic Living Stds"; "mat_liv_std" = "Material Living Stds"')

allcountries <- filter(reg, Subregion == input)
oecdcountries <- filter(oecd,SUBREGION == input)
oecdcountries <- select(oecdcountries,COUNTS,CODE,SUBREGION)
colnames(oecdcountries) <- c("n","Code","country")
allc <- as.list(as.character(allcountries$Country))
nc <- length(allc)

articles <- matrix(nrow=nc,ncol=(2))
rownames(articles) <- allc
colnames(articles) <- c("n","Code")
list <- select(map_data_final,aid,Study_country)
for (c in allc){
  subset <- filter(list,Study_country == c)
  subset <- distinct(subset)
  test <- filter(allcountries, Country == c)
  code <- as.character(as.vector(test$code))
  articles[c,1] <- as.numeric(n_distinct(subset$aid))
  articles[c,2] <- code
}
articles <- as.data.frame(articles)
articles$n <- as.numeric(as.vector(articles$n))
articles$country <- rownames(articles)
rownames(articles) <- NULL
articles <- bind_rows(articles,oecdcountries)


col <- colorRampPalette(c("white", "#74c476","#41ab5d", "#238b45", "#006d2c", "#00441b")) (200)
col2 <- RColorBrewer::brewer.pal(n_distinct(top$out),"Set1")

plot_ly(articles, type = 'choropleth', locations = Code, z = n, colors = col, text=paste(country,":<br>",n,"total articles"), showscale = F, geo = 'geo') %>%
  add_trace(type="scattergeo",locations=Code, text = paste(Country,":<br>",counts, "articles"), color = out, colors = col2, marker = list(size = counts*2),data=top, geo="geo") %>%
layout(title = 'Distribution of outcome types in Eastern Africa<br> Source: <a href="https://environmentalevidencejournal.biomedcentral.com/articles/10.1186/2047-2382-3-16">Bottrill et al. (in press)</a>', geo = g1)
