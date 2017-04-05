library(tidyverse)
library(broom)
library(forcats)
library(stringr)
library(scales)
library(choroplethr)
library(rgdal)
library(gpclib)
library(mapproj)
library(maptools)
library(R6)
library(gridExtra)
library(shinydashboard)
library(shiny)
library(leaflet)


gpclibPermit()

district_crosswalk <- tibble(
  DistrictID =c('H1','H2','H3','H4','H5','H6','H7','H8','H9','H10','H11','H12','H13','H14','H15','H16','H17','H18','H19','H20','H21','H22','H23','H24','H25','H26','H27','H28','H29','H30','H31','H32','H33','H34','H35','H36','H37','H38','H39','H40','H41','H42','H43','H44','H45','H46','H47','H48','H49','H50','H51','H52','H53','H54','H55','H56','H57','H58','H59','H60','H61','H62','H63','H64','H65','H66','H67','H68','H69','H70','H71','H72','H73','H74','H75','H76','H77','H78','H79','H80','H81','H82','H83','H84','H85','H86','H87','H88','H89','H90','H91','H92','H93','H94','H95','H96','H97','H98','H99','H100','S1','S2','S3','S4','S5','S6','S7','S8','S9','S10','S11','S12','S13','S14','S15','S16','S17','S18','S19','S20','S21','S22','S23','S24','S25','S26','S27','S28','S29','S30','S31','S32','S33','S34','S35','S36','S37','S38'),
  DISTRICT = c(37,38,39,41,40,4,43,42,44,88,86,45,87,49,46,47,48,52,51,50,54,5,53,56,55,9,89,90,71,67,7,70,74,68,66,72,93,92,81,94,96,95,97,91,82,69,79,73,98,99,57,59,76,60,75,80,8,77,78,24,19,18,27,28,29,25,3,30,26,12,63,22,20,11,84,21,85,23,83,61,64,62,58,35,6,31,32,2,65,33,34,36,100,1,10,14,13,16,15,17,16,15,18,17,21,19,4,2,22,23,8,37,38,25,26,24,5,14,32,35,28,36,9,7,1,34,12,6,10,11,13,20,3,27,31,33,29,30),
  chamber = c('H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','S','S','S','S','S','S','S','S','S','S','S','S','S','S','S','S','S','S','S','S','S','S','S','S','S','S','S','S','S','S','S','S','S','S','S','S','S','S')
)

votes <- read_csv('https://query.data.world/s/8ta9wcevg9crkp319k8knal2c')
sponsors <- read_csv('https://query.data.world/s/3pgz5fv2aqxnuoflie0dw290j')
legislature <- read_csv('https://query.data.world/s/drw2g54peak3frtxtsb3zu2ua')
legislators <- read_csv('https://query.data.world/s/9f49ewzedjslrh23g96v53ha5') %>%
  dplyr::rename(DistrictID = District) %>% 
  mutate(chamber = map_chr(DistrictID, function(i) str_sub(i, 1, 1))) %>% 
  left_join(select(district_crosswalk, -chamber))
bills <- read_csv('https://query.data.world/s/e5wtswfrqfwsiqzuqs977dx5j') %>% 
  mutate(num = map_dbl(BillNumber,function(i){
    ifelse(is.na(substr(i,3,str_length(i)) %>% as.numeric()),
           substr(i,2,str_length(i)) %>% as.numeric(),
           substr(i,3,str_length(i)) %>% as.numeric())
  }))

sponsors_party <- left_join(sponsors, select(legislators, legislator_id, Party))

bills_party <- mutate(bills, party = map_chr(bill_id, function(i){
  b <- filter(sponsors_party, bill_id == i)$Party %>% unique()
  if(!('Republican' %in% b)) 'Democratic'
  else if(!('Democratic' %in% b)) 'Republican'
  else 'Mixed'
}))  

votes_party <- left_join(votes, select(bills_party, bill_id, party))

house_map <- readOGR(dsn = './shapefiles/house_f', layer = 'house_map_shp')
house_map@data$id <- rownames(house_map@data)
house_map@data <- merge(house_map@data, filter(legislators, chamber == 'H'), by='DISTRICT')

senate_map <- readOGR(dsn = './shapefiles/senate_f', layer = 'senate_map_shp')
senate_map@data$id <- rownames(senate_map@data)
senate_map@data <- merge(senate_map@data, filter(legislators, chamber == 'S'), by='DISTRICT')

make_leaflet <- function(chamber, bill, the_vote){
  if(chamber == 'House') shape <- house_map
  else shape <- senate_map
  
  shape <- merge(shape, votes %>% 
                   filter(bill_id == bill) %>% 
                   select(legislator_id, Vote),
                 by = 'legislator_id')
  
  shape$color <- map2_chr(shape$Vote, shape$Party, function(Vote, Party){
    if(Vote != the_vote) 'lightgray'
    else if(Party == 'Democratic') 'blue'
    else 'red'
  })
  
  shape$opacity <- map_dbl(shape$color, function(i) ifelse(i == 'lightgray', 0.0, 0.66))
  
  pop <- paste0('<strong>',shape$Initial.Name,'</strong><br>Gender: ',shape$Gender,
                '<br>Race: ',shape$Race,'<br>First Elected: ',shape$First.Elected)
  
  leaflet(data = shape) %>% 
    addTiles() %>% 
    addPolygons(data = shape, stroke = F, smoothFactor=0.2, fillOpacity = shape$opacity, color=shape$color, popup=pop) %>% 
    addPolylines(weight=2, color='black')
}

bill_selector <- bills$bill_id  %>% factor() %>% fct_reorder(bills$num) %>% levels()
names(bill_selector) <- map_chr(bill_selector, function(i) bills$BillNumber[which(i == bills$bill_id)[1]])

# Set UI in Shiny App
ui<-dashboardPage(
  dashboardHeader(title = '2017 Kentucky General Assembly',
                  titleWidth = 600),
  dashboardSidebar(
    selectInput('bill_selection', 'Bills', bill_selector, selected = 'HB001'),
    radioButtons('chamber_selection', 'Chamber', c('House','Senate'), selected = 'House'),
    radioButtons('vote_selection', 'Vote', c('YEA' = 'YEA', 'NAY' = 'NAY', 'No Vote' = 'NV'), selected = 'YEA')
  ),
  dashboardBody(
    # Style tag helps make leaflet take up more space.
    tags$style(type = "text/css", 
               "#selected_vote {height: calc(100vh - 200px) !important;}",
               ".shiny-output-error { visibility: hidden; }",
               ".shiny-output-error:before { visibility: hidden; }"),
    leafletOutput('selected_vote'),
    includeMarkdown('./kyleg.md')
  )
)

server <- function(input, output){
  output$selected_vote <- renderLeaflet({
    make_leaflet(input$chamber_selection, input$bill_selection, input$vote_selection)
  })
}

shinyApp(ui,server)