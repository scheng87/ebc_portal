library(shiny)
library(tidyr)
library(shinythemes)
library(shinyBS)
library(gplots)
library(RColorBrewer)
library(maptools)
library(ggplot2)
library(shinydashboard)
library(plotly)
library(scales)
library(lattice)
library(car)
library(shinyjs)
library(grid)
library(googlesheets)
library(rgdal)
library(leaflet)
library(htmltools)
library(raster)
library(sp)
library(reshape2)
library(stringr)
library(dplyr)

setwd("~/Documents/github/wildlife_trade_portal/")
#setwd("/srv/shiny-server/iwt_portal/")
load("data/IWT_Evidence_Map.RData")
map_data_final <- readRDS("data/map_data_final.rds")
source("functions.R")

definitions <- read.csv("data/definitions.csv",header=TRUE)
definitions <- as.data.frame(definitions)

var.labels <- read.csv("data/codes.csv",header=TRUE)
var.labels <- as.data.frame(var.labels)

biomelabels <- read.csv("data/biomelabels.csv",header=TRUE)
biomelabels <- as.data.frame(biomelabels)

intlabels <- read.csv("data/int_labels.csv",header=TRUE)
intlabels <- as.data.frame(intlabels)

oecd <- read.csv("data/oecd.csv",header=TRUE)
regions <- read.csv("data/country_list2.csv", header=TRUE)
colnames(regions) <- c("COUNTRY", "REGION","CODE","SUBREGION","POINT")
reg <- read.csv("data/allcountries.csv",header=TRUE)

coordinates <- read.csv("data/country_lat_lon.csv",header=TRUE)
countries_shape <- read.csv("data/country_coordinates.csv",header=TRUE)

parts_type <- c("Whole animal","Organs","Tissue","Bones","Extracts","Meat","Horn","Wool","Skin","Whole plant","Other")
purpose_type <- c("food","fuel","medicine","decoration","pets","construction","clothing","trophy","other")
supply_type <- c("supply","trade","consumer")
impl_type <- c("Academic","Public sector","Research institute","Consultant","Non-profit","Private sector/industry","International Convention")
design_type <- c("Experimental","Quasi experimental","Non-experimental","Systematic review")
comp_type <- c("Control-intervention site comparisons","Before-after","Spatial comparator","Interrupted time series","Continuous time series","Group comparators between populations of people, ecosystems or species","Presence/absence of intervention")
int_type <- c("laws","policies","detection","prosecution","civil","substitution","awareness","market","disincentive","stewardship","conflict","spat_protect","harvest_reg","culture")
int_labels <- c("Laws, regulations & codes","Policies & regulations", "Detection","Prosecution","Civil action","Substitutions","Awareness raising","Market-based incentives","Disincentives for illegal behavior","Incentives for stewardship of wildlife","Decrease human-wildlife conflict","Spatial areas of protection","Regulate harvest","Culturing of species")
out_type <- c("Management","Protection","Trade","Behavior change","Population","Species","Economic living standards","Material living standards","Health","Education","Social relations","Security and safety","Governance and empowerment","Subjective well-being","Culture & spirituality","Freedom of choice & action")
int_group <- c("Enforcement/compliance","Establish/refine laws & policies","Reduce demand/consumption","Reduce threats to species","Support livelihoods")
out_group <- c("Behavioural","Biological","Human well-being")
study_labels <- c("Experimental","Quasi-experimental", "Non-experimental","Non-systematic review","Systematic review")
countries <- as.vector(reg$Country)

# Define the fields we want to save from the form
fields <- c("name", "email","organization", "agree")

# Define functions
table <- "wildlife_portal_registration"

