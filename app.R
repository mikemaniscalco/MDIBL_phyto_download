#
# This is a Shiny web application. You can run the application by clicking

library(shiny)
library(dplyr)

googlesheets4::gs4_auth(path = '.secrets/enduring-coil-349821-3832e38b5b43.json')
# 
# df_event <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1BaCEGePEYx9wbduccHytb9uRVbz3447GEDCE8Hpm0I8/edit#gid=1448123747",
#                                       sheet="Event")
# 
# df_phyto <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1BaCEGePEYx9wbduccHytb9uRVbz3447GEDCE8Hpm0I8/edit#gid=1550025152",
#                                       sheet="Occurrence")
# 
# df_envir <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1BaCEGePEYx9wbduccHytb9uRVbz3447GEDCE8Hpm0I8/edit#gid=1448123747",
#                                       sheet="Extended")
# df_phyto_wide <- df_phyto %>%
#   select(-c(occurrenceID:kingdom)) %>%
#   tidyr::pivot_wider(.,names_from = organismName,values_from = organismQuantity)
# 
# df_envir_wide <- df_envir %>%
#   select(-c(measurementID:measurementRemarks)) %>%
#   tidyr::pivot_wider(.,names_from = measurementType,values_from = measurementValue)
# 
# historical_wide <- left_join(df_event,df_envir_wide) %>%
#   left_join(.,df_phyto_wide) %>%
#   relocate(monitor_names, .after="eventRemarks")%>%
#   relocate(descending_transparency:ascending_transparency, .before="transparency_depth_mean") %>%
#   mutate(across(c(air_temp:water_temp, wind_speed_knots,
#                   tide_height:bod, pH, bottom_depth_m,
#                    wind_direction), ~as.numeric(.x))) %>%
#   mutate(time_high_tide=as.POSIXct(time_high_tide,
#                                    format="%Y-%m-%d %H:%M:%S",
#                                    tz ="UTC"),
#          time_low_tide=as.POSIXct(time_low_tide,
#                                   format="%Y-%m-%d %H:%M:%S",
#                                   tz ="UTC")) %>%
#   mutate(time_high_tide=as.POSIXct(time_high_tide,
#                                    format="%Y-%m-%d %H:%M:%S",
#                                    tz ="America/New_York"),
#          time_low_tide=as.POSIXct(time_low_tide,
#                                   format="%Y-%m-%d %H:%M:%S",
#                                   tz ="America/New_York"),
#          eventDate=as.POSIXct(eventDate,
#                                   format="%Y-%m-%d %H:%M:%S",
#                                   tz ="America/New_York")) %>%
#   select(-c(id,eventID,geodeticDatum,countryCode))
# rm(df_event, df_phyto, df_envir, df_phyto_wide ,df_envir_wide)
# 
# 
# #
# save(historical_wide, file = "historical_data.Rdata")
load("historical_data.Rdata")

df_new <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1JGXLAvG_U9dWn7Ya_NorUU3mcgpi7aX2uanszfhXP8c/edit?sharingaction=ownershiptransfer#gid=0",
                                    sheet = "processed") %>%
  dplyr::mutate(across(c(water_temp:air_temp,rainfall_mm,
                         wind_speed_knots,
                         ascending_transparency:nutrient_vial_id,
                         wind_direction),
                       ~as.numeric(.x)))%>%
  dplyr::select(-c(Copepods, Other_zooplankton, Total_zooplankton, sampling_method)) %>%
  dplyr::mutate(maximumDepthInMeters=ifelse(is.na(maximumDepthInMeters)==T,
                                            1,
                                            maximumDepthInMeters),
                minimumDepthInMeters=ifelse(is.na(minimumDepthInMeters)==T,
                                            0.5,
                                            minimumDepthInMeters))

### wide
df_full <- full_join(historical_wide, df_new) 
rm(df_new, historical_wide)


## select tables
## if more than one left_join with event
######## maybe just have enviro and counts with event info joined already
### Select variables similar to plots shinyApp

### select site(s)
####### similar to graphs?
### select date range
####### same as graphs
### select taxa of interest?
####### filter
### select environmental variables
####### filter
# 
# # Define UI for data download app ----
# ui <- fluidPage(
# 
#   # App title ----
#   titlePanel("Downloading Data"),
# 
#   # Sidebar layout with input and output definitions ----
#   sidebarLayout(
# 
#     # Sidebar panel for inputs ----
#     sidebarPanel(
# 
#       # Input: Choose dataset ----
#       selectInput("dataset", "Choose a dataset:",
#                   choices = c("rock", "pressure", "cars")),
# 
#       # Button
#       downloadButton("downloadData", "Download")
# 
#     ),
# 
#     # Main panel for displaying outputs ----
#     mainPanel(
# 
#       tableOutput("table")
# 
#     )
# 
#   )
# )
# 
# # Define server logic to display and download selected file ----
# server <- function(input, output) {
# 
#   # Reactive value for selected dataset ----
#   datasetInput <- reactive({
#     switch(input$dataset,
#            "rock" = rock,
#            "pressure" = pressure,
#            "cars" = cars)
#   })
# 
#   # Table of selected dataset ----
#   output$table <- renderTable({
#     datasetInput()
#   })
# 
#   # Downloadable csv of selected dataset ----
#   output$downloadData <- downloadHandler(
#     filename = function() {
#       paste(input$dataset, ".csv", sep = "")
#     },
#     content = function(file) {
#       write.csv(datasetInput(), file, row.names = FALSE)
#     }
#   )
# 
# }
# 
# # Create Shiny app ----
# shinyApp(ui, server)

# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
library(DT)

test <- df_full %>% 
  select(locationID, all_of(enviro_variables),all_of(phyto_variables),all_of(hab_taxa))

location_Id <- c("MDIBL_Dock_22G", "Bar_Harbor_Town_Pier_22D", "Bass_Harbor_21B" )

enviro_variables <- c("eventDate","locationID","eventRemarks",                
                      "monitor_names", "decimalLatitude", "decimalLongitude",           
                      "minimumDepthInMeters", "maximumDepthInMeters", "coordinateUncertaintyInMeters",
                      "time_low_tide", "time_high_tide", "air_temp",                    
                      "water_temp", "weather", "wind_speed_knots",            
                      "water_surface", "water_current", "tide_stage",                   
                      "tide_height", "salinity_ppt", "DO1_ppm",                 
                      "DO2_ppm", "DOavg_ppm", "descending_transparency",
                      "ascending_transparency", "transparency_depth_mean", "rainfall_mm",                  
                      "DO1_day5_ppm", "DO2_day5_ppm", "DO_day5_avg", "bod", 
                      "wind_direction", "bottom_depth_m", "wind_description", "pH" )

phyto_variables <- c("Alexandrium","Chaetoceros_spp", "Coscinodiscus",
                     "Dinophysis_spp","Leptocylindrus", "Navicula", "Prorocentrum_spp",
                     "Chaetoceros_socialis" , "Gyrosigma", "Rhizosolenia", "Other_phytoplankton",          
                     "Pleurosigma", "Gonyaulax",   "Guinardia", "Nitzschia",   "Prorocentrum_lima", 
                     "Phaeocystis" ,  "Protoperidinium","Ditylum", "Melosira", "Thalassiosira",
                     "Dictyocha",   "Eucampia", "Thalassionema","Licmophora",  "Ceratium", 
                     "Pseudo_nitzschia_small", "Pseudo_nitzschia_large", "Pseudo_nitzschia_spp",
                     "Dinophysis_acuminata", "Dinophysis_norvegica", "Karenia",
                     "Margalefidinium_polykrikoides", "Gymnodinium", 
                     "id", "eventID",     "geodeticDatum","countryCode" , "nutrient_vial_id",
                     "Scrippsiella","Other_diatoms", "Other_dinoflagellates","Total_phytoplankton")

hab_taxa <- c("Alexandrium","Chaetoceros_spp", "Coscinodiscus",
              "Dinophysis_spp","Leptocylindrus", "Navicula", "Prorocentrum_spp",
              "Chaetoceros_socialis" , "Gyrosigma", "Rhizosolenia", "Other_phytoplankton",          
              "Pleurosigma", "Gonyaulax",   "Guinardia", "Nitzschia",   "Prorocentrum_lima", 
              "Phaeocystis" ,  "Protoperidinium","Ditylum", "Melosira", "Thalassiosira",
              "Dictyocha",   "Eucampia", "Thalassionema","Licmophora",  "Ceratium", 
              "Pseudo_nitzschia_small", "Pseudo_nitzschia_large", "Pseudo_nitzschia_spp",
              "Dinophysis_acuminata", "Dinophysis_norvegica", "Karenia",
              "Margalefidinium_polykrikoides", "Gymnodinium", 
              "id", "eventID",     "geodeticDatum","countryCode" , "nutrient_vial_id",
              "Scrippsiella","Other_diatoms", "Other_dinoflagellates","Total_phytoplankton" )


# shiny app 
ui <- fluidPage(
  h1("Data Download Dashboard"),
  sidebarLayout(
    sidebarPanel(selectInput("Site", label = "Choose site", location_Id, multiple = TRUE, selected = location_Id),
                 downloadButton("download1","Download entire Table  as csv")),
    mainPanel(h4("Frenchmans Bay phytoplankton monitoring"),
              dataTableOutput("iris_dto")
    )
  ))

server <- function(input, output, session) {
  
  thedata <- reactive({
    df_full %>% 
      filter(locationID %in% input$Site)
  })
  
  output$iris_dto <- renderDataTable({
    thedata()  %>% 
      datatable(extensions = 'Buttons',
                options = list(
                  pageLength = nrow(thedata()),
                  #Each letter is a dif element of a datatable view, this makes buttons the last thing that's shown.
                  dom = 'lfrtipB',
                  buttons = c("copy", "csv", "pdf")),
                filter = list(
                  position = 'top'),
                rownames = FALSE)
  })
  
  
  output$download1 <- downloadHandler(
    filename = function() {
      paste("iris_", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(thedata(), file)
    }
  )
}






# 
# 
# 
# 
# 
# 
# 
# 
# 
# #>
# #> Attaching package: 'DT'
# #> The following objects are masked from 'package:shiny':
# #>
# #>     dataTableOutput, renderDataTable
# 
# 
# species <- iris %>% select(Species) %>% distinct() %>% pull()
# 
# 
# # shiny app 
# ui <- fluidPage(
#   h1("Data Download Dashboard"),
#   sidebarLayout(
#     sidebarPanel(selectInput("Species", label = "Choose species", species, multiple = TRUE, selected = species),
#                  downloadButton("download1","Download entire Table  as csv")),
#     mainPanel(h4("Table 1: Iris"),
#               dataTableOutput("iris_dto")
#     )
#   ))
# 
# server <- function(input, output, session) {
#   
#   thedata <- reactive({
#     iris %>% 
#       filter(Species == input$Species)
#   })
#   
#   output$iris_dto <- renderDataTable({
#     thedata()  %>% 
#       datatable(extensions = 'Buttons',
#                 options = list(
#                   pageLength = nrow(thedata()),
#                   #Each letter is a dif element of a datatable view, this makes buttons the last thing that's shown.
#                   dom = 'lfrtipB',
#                   buttons = c("copy", "csv", "pdf")),
#                 filter = list(
#                   position = 'top'),
#                 rownames = FALSE)
#   })
#   
#   
#   output$download1 <- downloadHandler(
#     filename = function() {
#       paste("iris_", Sys.Date(), ".csv", sep="")
#     },
#     content = function(file) {
#       write.csv(thedata(), file)
#     }
#   )
# }
#   
#   
shinyApp(ui, server)
