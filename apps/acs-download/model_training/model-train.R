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
library(leaflet)
library(shinyBS)
library(DT)
library(shinycssloaders)
library(waiter)
library(zip)
library(openxlsx)
library(reactlog)
library(parsnip)

source("functions/acs-query.R")

#Census API Key
census_api_key(Sys.getenv("CENSUS_API_KEY"))

#Database connection
reconnect_db = function(){
  con <<- dbConnect(
    RPostgres::Postgres(),
    host = Sys.getenv("PGHOST"),
    port = Sys.getenv("PGHOST"),
    dbname = Sys.getenv("PGDATABASE"),
    user = Sys.getenv("PGUSER"),
    password = Sys.getenv("PGPASSWORD")
  )
}
reconnect_db()

acs_query_records = tbl(con,"acs_query_records") %>% collect() %>%
  arrange(acs_query_id)

model_data = acs_query_records %>%
  select(seconds_elapsed,geog_level,
         num_counties,num_states,num_vars,geom_include,num_years) %>%
  mutate(geog_level = factor(geog_level)) %>%
  mutate(num_years = replace_na(num_years,1))

trained_model = rand_forest(mtry = 3, trees = 2000) %>%
  set_mode("regression") %>%
  fit(seconds_elapsed ~ ., data = model_data)

write_rds(trained_model,"model_training/trained_model.rds")
