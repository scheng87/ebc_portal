library(rcrossref)

forest_doi <- forests[grep("^http://dx.doi.org", forests$DOI), ]
row <- c(1:nrow(forest_doi))
all <- c()
for (i in row){
  doi <- forest_doi[i,4]
  p <- unlist(strsplit(doi,"http://dx.doi.org/"))
  bib <- cr_cn(dois = p[2], format="bibentry")
  
  all <- c(all,bib)
}
capture.output(utils:::print.bibentry(all,style="Bibtex"), file="forest.bib")
