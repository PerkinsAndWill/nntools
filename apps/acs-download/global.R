library(tidyverse)
library(sf)
library(tidycensus)
library(tigris)
library(DBI)
library(RPostgreSQL) 
library(janitor)
library(readxl)
library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinyWidgets)
# library(leaflet)
library(shinyBS)
library(DT)
library(shinycssloaders)
library(waiter)
library(zip)
library(openxlsx)
library(reactlog)
library(parsnip)
library(ranger)
library(mapgl)
#library(rgdal)
#reactlog::reactlog_enable()

#define every year's variable names:
options(tigris_use_cache = TRUE)
acs_year_name = "5-year ACS Data (2010 - 2024)"

#Load ACS Querying functions into environment
source("functions/acs-query.R")

#Define API Key
CENSUS_API_KEY = Sys.getenv("CENSUS_API_KEY")
# MAPBOX_API_KEY = Sys.getenv("MAPBOX_PUBLIC_TOKEN")
# census_api_key(CENSUS_API_KEY, install = TRUE, overwrite = TRUE)

#Bring in some sys sleep time when querying
safe_get_acs <- function(...) {
  
  Sys.sleep(0.5)
  
  tryCatch(
    get_acs(...),
    error = function(e) {
      message("ACS request failed: ", e$message)
      return(NULL)
    }
  )
}

# Fill color expression: nn_blue for selected geoids, grey otherwise
selection_fill_color <- function(geoids) {
  list(
    "case",
    list("in", list("get", "geoid"), list("literal", as.list(geoids))),
    unname(nn_blue),
    "grey"
  )
}

#Database connection
reconnect_db = function(){
  con <<- dbConnect(
    RPostgres::Postgres(),
    host = Sys.getenv("PGHOST"),
    port = Sys.getenv("PGPORT"),
    dbname = Sys.getenv("PGDATABASE"),
    user = Sys.getenv("PGUSER"),
    password = Sys.getenv("PGPASSWORD")
  )
}
reconnect_db()

voi_ref0 = tbl(con,"variables_of_interest") %>% collect()

trained_rf_model = read_rds("model_training/trained_model.rds")

#U.S. State reference
state_meta = tbl(con,"states") %>%
  distinct(region,division,statefp,geoid,stusps,name) %>%
  collect() %>%
  arrange(name)

#Table defining which variables of interest we will allow users to request
voi_ref = tbl(con,"variables_of_interest") %>% collect() %>%
  mutate(voi_label = paste0(voi_id,") ",variable_of_interest," (",table_number,")"))

census_voi_ref <- tbl(con,"census_voi") %>% collect() %>%
  mutate(voi_label = paste0(voi_id,") ",variable_of_interest," (",table_number,")"))

voi_col_name_ref <- tbl(con,"voi_col_name_ref") %>% collect()

#Cross reference of which tables are available in which years of the 5-year ACS so that we are not requesting tables that do not exist
table_year_ref = tbl(con,"table_year_ref") %>% collect() %>%
  mutate(available=TRUE) %>%
  filter(year >=2010)

years_available = table_year_ref %>%
  distinct(year) %>%
  pull(year) %>% 
  sort() %>% 
  as.character()

# Write out Census variable definitions to RDS file for help parsing variable names
census_vars = read_rds("data/1_original/2020_census_vars.rds")

# Standard colors
nn_blue = "#007DB5"

#Parent geog levels
parent_geog_levels <- c(
  "Counties (includes tracts, block groups, & blocks)",
  "Census Designated Places" #,
  #"Core Based Statistical Areas"
)


