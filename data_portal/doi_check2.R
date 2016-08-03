library(rcrossref)
library(dplyr)
library(tidyr)
library(RefManageR)
citations <- select(map_data_final,aid,DOI)
citations <- distinct(citations)
citations_filt <- citations[grep("^http://dx.doi.org/",citations$DOI),]
citations_manual <- anti_join(citations,citations_filt,by="aid")

##Manually move articles w/ errors to manual search list
lookup_fail <- slice(citations_filt,2)
citations_filt <- filter(citations_filt,aid != 2)
citations_filt$cit_count <- c("")
row <- c(1:nrow(citations_filt))

bib <- c()
for (i in row){
  doi <- citations_filt[i,2]
  p <- unlist(strsplit(doi,"http://dx.doi.org/"))
  c <- try(GetBibEntryWithDOI(p[2]))
  if('try-error' %in% class(c)){
    print(i) 
    next
  } else bib <- c(bib,c)
}

lookup_fail2 <- slice(citations_filt,c(29,65,76,77,184,200,253,339,378,456,459,476,495,605,623,629,652,700,715,813,823))
lookup_fail <- bind_rows(lookup_fail,lookup_fail2)

manual_lookup <- c()
row2 <- c(1:nrow(lookup_fail))
for (i in row2){
  doi <- as.character(lookup_fail[i,2])
  p <- try(unlist(strsplit(doi,"http://dx.doi.org/")))
  if('try-error' %in% class(p)) next
  c <- try(cr_cn(dois=p[2],format="bibentry"))
  if('try-error' %in% class(c)){
    print(i) 
    next
  } else manual_lookup <- c(manual_lookup,c)
}

fail2 <- slice(lookup_fail,c(2,4,7,10,11,14,17,19,22))
bib_info <- select(map_data_final,aid,Authors,Title,Pub_year,Journal)
fail2 <- left_join(fail2,bib_info,by="aid")
fail2 <- distinct(fail2)


final_bib <- c(bib,manual_lookup)
WriteBib(final_bib,file="test.bib",biblatex=TRUE,append=FALSE,verbose=TRUE)

