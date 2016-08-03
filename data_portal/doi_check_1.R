library(dplyr)
library(tidyr)
library(rcrossref)
library(stringr)
library(rvest)

##Need to check DOI/aid information

DOI <- read.csv("~/Desktop/DOI_check.csv")

D <- select(DOI,Dois)
nrow <- c(1:nrow(D))

DOIs <- data.frame(V1=c(""),V2=c(""))
for (n in nrow){
  test <- as.data.frame(str_split_fixed(D[n,1],"http://dx.doi.org/",2))
  DOIs <- bind_rows(DOIs,test)
}

DOIs <- slice(DOIs,-1)
DOIs <- bind_cols(as.data.frame(DOI$aid),DOIs)
colnames(DOIs) <- c("aid","URL","DOI")
DOIs <- distinct(DOIs)

DOI_only <- filter(DOIs,DOI != "")
q <- as.list(DOI_only[,3])

oa <- cr_works(dois=q)
z <- as.data.frame(oa$data)
open <- filter(z, license_url == "http://creativecommons.org/licenses/by/3.0/")

oa <- cr_works(filter = list(has_full_text = TRUE,license_url = "http://creativecommons.org/licenses/by/3.0/"))
t <- oa$data$DOI

##Wiley-Blackwell
doi1 <- read_html("http://onlinelibrary.wiley.com/doi/10.1111/1755-0998.12521/full")
open <- doi1 %>% html_nodes(".freeAccess div span")
t <- grep("freeAccess",open,value=TRUE)

doi2 <- read_html("http://onlinelibrary.wiley.com/doi/10.1111/j.1541-0064.2007.00179.x/full")
open2 <- doi2 %>% html_nodes("div")
t2 <- grep("freeAccess",open2,value=TRUE)

doi3 <- read_html("http://www.ingentaconnect.com/contentone/cfa/ifr/2015/00000017/A00404s4/art00001")
open3 <- doi3 %>% html_nodes("span img")
t3 <- grep("Open Access",open3,value=TRUE)
