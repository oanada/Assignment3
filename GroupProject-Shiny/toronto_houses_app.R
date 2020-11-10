#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# Authors: 
#           Group A - Autumn Class
#               Oana Damian
#               Junylou Daniel
#               Robin Mathew
#               Torsten Mayer

rm(list = ls())
library(shiny)
library(shinyBS)
library(shinyWidgets)
library(shinyjs)
# library(maps)
library(ggmap)
# library(ggplot2)
library(leaflet)
library(scales)
library(sp)
# library(lattice)
# library(dplyr)
# library(tidyr)
# library(caret)

# library(shinythemes)
# library(plotly)
# library(ggplot2)
# library(gridExtra)
# library(dplyr)
# library(data.table)
library(stringr)
library(strex)
# library(ggplot2)
# library(ggthemes) 
# library(FactoMineR)
# library(factoextra)
# library(tidyr)
# library(reshape2)
# library(cluster)
# library(class)

# library(rgdal)

#library(htmltools)

leaflet(options = leafletOptions(minZoom = 0, maxZoom = 18))  # Set value for the minZoom and maxZoom settings.
load("./www/shinyenvdata.RData")
register_google(key = "AIzaSyDsdDbObjXpT0bpDK50DKVIYCbbGptbYCk")
distchoices <- c("All Districts", sort(to_neigh$AREA_NAME))

betas <- c(b0 <- -507305.3772,   #(Intercept)
           b1 <- 3.7372,         #mean_district_income
           b2 <- -401.9158,      #district_code
           b3 <- 590.0003,       #sqft
           b4 <- 1254.1058,      #TO_rank_safety
           b5 <- 2779.7949,      #TO_rank_transit
           b6 <- 749.9727,       #TO_rank_shopping
           b7 <- 487.0452,       #TO_rank_health
           b8 <- 1627.2697,      #TO_rank_entertainment
           b9 <- -176.5268,      #TO_rank_community
           b10 <- -145.0480,     #TO_rank_diversity
           b11 <- 24.3140,       #TO_rank_education
           b12 <- 1056.6620)     #TO_rank_employment

#browser()
# Define UI 
ui <- bootstrapPage( theme = "styles.css",
            useShinyjs(),
            tags$style(HTML(".tooltip > .tooltip-inner {
                    width: 200px;
                    color: black;
                    background-color: yellow;
                }")),
            div( class = "outer",
                 # map in the background
                 leafletOutput("map", width="100%", height="100%"),
                 absolutePanel( id = "controls", class = "sidedash-box", 
                    # Application title
                    tags$h1("Toronto Houses"),
                    radioButtons(inputId = "mytask", "What do you want to do today?",
                                 c("Sell/Bid a house"= "sellbid",
                                   "Find a house"= "findhouse"), inline=FALSE),
                    bsTooltip("mytask", "Sell/Bid a house, will return the property predicted price.\\nFind a house, will return listed properties for sale based on entered criteria","bottom"),
                    conditionalPanel(
                        condition = "input.mytask == 'sellbid'",
                        searchInput(inputId = "addr1", label = "Enter no. and Street Name", placeholder = "100 Queen St W", 
                                    btnSearch = icon("search")),
                        textInput(inputId = "distvar1", label = "District"),
                        bsTooltip("addr1", "Search the number and street name first.\\nLocation will be marked on the map and the District is determined automatically.\\nThe application assumes Toronto address and will indicate if the address is not found on any District.","right"),
                    ),
                    conditionalPanel(
                        condition = "input.mytask == 'findhouse'",
                        numericInputIcon(inputId = "budget1", label = "Enter budget amount", value = "1000000",
                                icon = list(NULL, icon("dollar-sign"))),
                        selectInput(inputId = "distvar2", label = "District", choices = distchoices,
                                    selected = "All Districts"),
                        bsTooltip("budget1", "This is the maximum amount you are willing to spend.","right"),
                        bsTooltip("distvar2", "You may choose a specific district to find listed properties.","right"),
                    ),
                    
                    tags$h4(HTML("</br>House Description:")),
                    column(width=5,selectInput(inputId = "bedvar", label = "No. of Bedrooms", choices = 0:20)),
                    column(width=5,selectInput(inputId = "bathvar", label = "No. of Bathrooms", choices = seq(1, 20, by=0.5))),
                    column(width=5,selectInput(inputId = "parkvar", label = "Parking", choices = 0:15)),
                    column(width=5,numericInput(inputId = "sqftvar", label = "Area (sqft)", "1000")),
                    column(width=8,selectInput(inputId = "typevar", label = "House Type", choices = c("All Types",
                                "Condo Apt", "Semi-Detached", "Detached", "Condo Townhouse", "Plex", "Att/Row/Twnhouse",
                                "Comm Element Condo", "Link", "Co-Ownership Apt", "Co-Op Apt", "Store W/Apt/Offc"))),
                    bsTooltip("bedvar", "Indicate the minimum number of bedrooms on the property.","right"),
                    bsTooltip("bathvar", "Indicate the minimum number of bathrooms on the property.","right"),
                    bsTooltip("parkvar", "Indicate the minimum number of parking space.","right"),
                    bsTooltip("sqftvar", "Indicate the minimum living space in Square Foot.","right"),
                    bsTooltip("typevar", "You may choose a specific property type.","right"),
            ),
            
            conditionalPanel(
                condition = "input.mytask == 'sellbid'",
                absolutePanel( class = "prediction-box", h4("Predicted price"), textOutput("houseprice")),
                bsTooltip("houseprice", "Predicted Price of the Property","bottom"),
            ),
            
            # conditionalPanel(
            #     condition = "input.mytask == 'findhouse'",
            #     absolutePanel( class = "prediction-panel", h4(""), textOutput("Predicted price"))
            # )
            
            #  textInput("var9", "Address", "1000 Kingston Ave"),
            #  #textInput("var7", "Average House Price of District", "700000"),
            # leafletOutput(outputId = "mymap"),
            #leafletOutput(outputId = "mymap2")
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    pal <- colorNumeric("viridis", NULL)
    #set.seed(919)
    labels <- sprintf("<strong>%s</strong>", to_neigh$AREA_NAME) %>% lapply(htmltools::HTML)
    
    # Reactive expression to create data frame of all input values ----
    inputData <- reactive({
        #browser()
        if(input$mytask == "sellbid") {
            lat <- 43.6534
            lon <- -79.3841
            to_addr <- "Toronto City Hall"
            district_code <- 76
            city_district <- "Bay Street Corridor"
            geoAddress <- to_addr
            if(input$addr1 != "") {
                #  geocode lookup
                to_addr <- paste0(input$addr1,", Toronto, Ontario")
                addrcoord <- geocode(to_addr, output = "latlona", source = "google")
                lon <- as.numeric(addrcoord[1])
                lat <- as.numeric(addrcoord[2])
                geoAddress <- as.character(addrcoord[3])
                
                #  find the district
                #browser()
                j <- 1
                inpoly <- 0
                while (inpoly == 0 & j <= 140) {
                    neighnames <- file_js$features[[j]]$properties$AREA_NAME
                    inpoly <- point.in.polygon(lat, lon, file_js$features[[j]]$geometry$coordinates[,2],
                                               file_js$features[[j]]$geometry$coordinates[,1], mode.checked=FALSE)
                    j <- j + 1
                }
                if(inpoly > 0) { 
                    neighcodname <- str_split(neighnames,"\\(")
                    district_code <- as.numeric(gsub("[^0-9.]", "", neighcodname[[1]][2]))
                    city_district <- str_trim(neighcodname[[1]][1])
                } else {
                    #Address cannot be found within Toronto    
                    city_district <- "Address NOT found in Toronto"
                }
            }
            #browser()
            
            col_header <- c("address", "bedroom",  "bathroom", "type", "parking", "sqft", "district_code", "city_district", "long", "lat")
            value = list(geoAddress, as.numeric(input$bedvar), as.numeric(input$bathvar), input$typevar, as.numeric(input$parkvar),
                        input$sqftvar, district_code, city_district, lon, lat)
            temp_df <- data.frame(matrix(ncol = length(col_header), nrow = 1))
            for(i in 1:length(col_header)) { temp_df[1, i] <- value[i] }
            #temp_df <- rbind(temp_df, value)
            colnames(temp_df) <- col_header
            temp_df <- merge(temp_df,neigh_rank, by="district_code")
            
            return(temp_df)
        } else if(input$mytask == "findhouse") {
            #browser()
            
            houses_df1 <- subset(houses_df, houses_df$bedrooms_ag >= as.numeric(input$bedvar) &
                                            houses_df$bathrooms >= as.numeric(input$bathvar) &
                                            houses_df$bathrooms >= as.numeric(input$parkvar) &
                                            houses_df$list_price <= input$budget1 &
                                            houses_df$sqft >= input$sqftvar
            )
            if(input$distvar2 != "All Districts") {
                distcode <- str_split_by_numbers((str_split(input$distvar2,"\\("))[[1]][2])
                houses_df1 <- subset(houses_df1, houses_df1$district_code == distcode[[1]][1])
            }
            if(input$typevar != "All Types") {
                houses_df1 <- subset(houses_df1, houses_df1$type == input$typevar)
            }
            
            # new features from keywords in "description"
            v1 <- as.data.frame(grepl("Fitness",houses_df1$description))
            houses_df1$fitness <- v1
            colnames(houses_df1$fitness) <- "fitness"

            v2 <- as.data.frame(grepl("Swimming",houses_df1$description))
            houses_df1$swimming_pool <- v2
            colnames(houses_df1$swimming_pool) <- "swimming_pool"

            houses_df1$fitness <- ifelse(houses_df1$fitness == TRUE, "Yes", "No")
            houses_df1$swimming_pool <- ifelse(houses_df1$swimming_pool == TRUE, "Yes", "No")
            
            if(nrow(houses_df1)!=0) {
                houses_df1$labels <- paste("<strong>", "City","District:", houses_df1$city_district, "</strong>",
                                            "<p>", "Type:", houses_df1$type, "</p>",
                                            "<p>", "List", "Price:", "$", houses_df1$list_price, "</p>",
                                            "<p>", "Area:", houses_df1$sqft, "</p>",
                                            "<p>", "Number","of","bedrooms:", houses_df1$bedrooms, "</p>",
                                            "<p>", "Number","of","bathroooms:", houses_df1$bathrooms, "</p>",
                                            "<p>", "Fitness", "center:", houses_df1$fitness, "</p>",
                                            "<p>", "Swimming", "pool:", houses_df1$swimming_pool, "</p>",
                                            "<p>", "Address:", houses_df1$full_address, "</p>"
                )
            }
            return(houses_df1)
        }
        
    })
    
    observeEvent(input$mytask, {
        inpvars <- c("bedvar", "bathvar", "parkvar", "typevar", "sqftvar")
        if(input$mytask == "sellbid") {
            for(x in inpvars) { 
                disable(x)
                disable("distvar1")
            }
        } else if(input$mytask == "findhouse") {
            for(x in inpvars) { 
                enable(x)
            }
        }
    })
    
    observeEvent(input$addr1, {
        #browser()
        mydata <- inputData()
        inpvars <- c("bedvar", "bathvar", "parkvar", "typevar", "sqftvar")
        if(input$addr1 == "") {
            updateTextInput(session, "distvar1", value = "Bay Street Corridor")
            for(x in inpvars) { 
                disable(x)
            }
        } else {
            mydistrict <- mydata$city_district
            updateTextInput(session, "distvar1", value = mydistrict)
            if (mydistrict != "Address NOT found in Toronto") {
                for (x in inpvars) {
                    enable(x)
                }
            } else {
                for (x in inpvars) {
                    disable(x)
                } 
            }
        }
    })
    
    # observeEvent(input$budget1, {
    #     updateNumericInputIcon(session, "budget1", value = prettyNum(input$budget1, big.mark=",", scientific=FALSE))
    # })
    
    output$houseprice <- reactive({
        #browser()
        if (input$mytask == "sellbid") {
            mydata <- inputData()
            # calculate predicted house price here...
            # neigh_ndx <- match(mydata$district_code, neigh_rank$district_code)
            inpvar <- c(1, mydata$mean_district_income, mydata$district_code, mydata$sqft, mydata$safety,
                           mydata$transit, mydata$shopping, mydata$health, mydata$entertainment, mydata$community,
                           mydata$diversity, mydata$education, mydata$employment)
            predprice <- dollar(sum(betas*inpvar))
            #predprice <- as.character(dollar(600000 + 40000*mydata$bedroom + 23283*mydata$bathroom + 21870*mydata$parking))
            return(predprice)
        } else if(input$mytask == "findhouse") {
            
        }
    })
    
    output$map <- renderLeaflet({
        #browser()
        mydata <- inputData()
        
        if(input$mytask == "sellbid") {
            to_map <- leaflet(to_neigh) %>% setView(lng = mydata$long, mydata$lat, zoom = 13 ) %>% addTiles() %>%
                        addMarkers(lng = mydata$long, lat = mydata$lat, popup=mydata$address)
            to_map %>% addPolygons(fillColor = ~pal(AREA_SHORT_CODE), weight = 2, opacity = .1, color = "blue", 
                        dashArray = "3", fillOpacity = 0.2, highlight = highlightOptions(weight = 5, color = "red", dashArray = "",
                        fillOpacity = 0.7, bringToFront = TRUE), label = labels,
                        labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
                        textsize = "15px", direction = "auto")) 
        } else if(input$mytask == "findhouse") {
            #browser()
            if(nrow(mydata) != 0) {
                df_select1 <- subset(mydata, type == "Detached" | type == "Semi-Detached")
                df_select2 <- subset(mydata, type == "Condo Apt" | type == "Condo Townhouse")

                if(input$distvar2 == "All Districts") {
                    long <- -79.40603941
                    lat <- 43.7172117
                } else {
                    distndx <- match(mydata$district_code[1], to_neigh$AREA_SHORT_CODE)
                    long <- to_neigh$LONGITUDE[distndx]
                    lat <- to_neigh$LATITUDE[distndx]
                }
                to_map <- leaflet(to_neigh) %>% setView(lng = long, lat, zoom = 12 ) %>% addTiles()
            
                #browser()
                if(nrow(df_select1) != 0) {
                    to_map <- to_map %>% addCircleMarkers(lng = df_select1$long, lat=df_select1$lat, color = "red", radius = ~ 3,
                                 group = "Houses", label = lapply(df_select1$labels, HTML), labelOptions = (textsize = "25px"))
                }
                if(nrow(df_select2) != 0) {
                    to_map <- to_map %>% addCircleMarkers(lng = df_select2$long, lat = df_select2$lat, color = "blue", radius = ~ 3,
                                 group = "Condos", label = lapply(df_select2$labels, HTML), labelOptions = (textsize = "25px"))
                }
                #browser()
                to_map %>% addLayersControl(overlayGroups = c("Houses", "Condos"), options = layersControlOptions(collapsed = FALSE))
            } else {
                long <- -79.40603941
                lat <- 43.7172117
                to_map <- leaflet(to_neigh) %>% setView(lng = long, lat, zoom = 12 ) %>% addTiles()
                to_map
            }
            
        }
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
