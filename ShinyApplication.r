##################################################################################
# Student Name : Vishal Pattabiraman
# StudentID : 31131441
# Tutor : Niranjan Nanjunda, Jeffery Chieh Liu
##################################################################################

##################################################################################
# Reference List:
# 1. Wilke, C. O. (2021, January 06). Retrieved from https://cran.r-project.org/web/packages/ggridges/vignettes/introduction.html
# 2. A Word Cloud Geom for ggplot2. (n.d.). Retrieved from https://lepennec.github.io/ggwordcloud/
# 3. Hahn, N. (2020, July 31). Making Maps with R. Retrieved from https://bookdown.org/nicohahn/making_maps_with_r5/docs/leaflet.html
# 4. Leaflet. (n.d.). Retrieved from https://rstudio.github.io/leaflet/
# 5. Benny, R. (2019, June 18). Adding custom markers. Retrieved from https://community.rstudio.com/t/adding-custom-markers/33355
# 6. Darkly themes. (n.d.). Retrieved from https://bootswatch.com/darkly/
# 7. Shiny Dashboard. (n.d.). Retrieved from https://rstudio.github.io/shinythemes/
# 8. Set group effects using class. (n.d.). Retrieved from https://rinterface.com/shiny/shinyEffects/
# 9. Abhimotgi. (2020, August 20). Abhimotgi/dataslice. Retrieved from https://github.com/abhimotgi/dataslice/blob/master/R/Interactive Graphs.Rmd
# 10.Buttons. (n.d.). Retrieved from https://plotly.com/r/custom-buttons/
###################################################################################


## Load libraries
library(ggplot2) # plotting bar graphs and histograms
library(tidyverse) # wrangle , filter and summarize data.
library(leaflet) # create plots on map.
library(shiny)# create interactive plots
library(shinydashboard)
library(maps)
library(lubridate)
library(hrbrthemes)
library(plotly)
library(ggwordcloud)
library(ggridges)
library(leaflet.extras)

## Create a custom theme to use for plots
mytheme<-theme(
  axis.title.x = element_text(size = 16), # update Title of x font size
  axis.text.x = element_text(size = 14), # update text of x font size
  axis.text.y = element_text(size = 14), # update text of y font size
  axis.title.y = element_text(size = 16), # update Title of y font size
  strip.text.x = element_text(size = 16), # update facet text font size
  legend.text = element_text(size = 16), # update legend text font size
  legend.title =element_text(size = 16),
  title = element_text(size = 16)) # update legend Title font size


## Read the file
wea.aus<-read.csv('weatherAUS.csv')
storm<- read.csv('Storm.csv')
aus.cities <- world.cities[world.cities$country.etc == "Australia",]

# update the datatypes.
wea.aus$WindGustDir<-as.factor(wea.aus$WindGustDir)
wea.aus$WindDir9am<-as.factor(wea.aus$WindDir9am)
wea.aus$WindDir3pm<-as.factor(wea.aus$WindDir3pm)
wea.aus$RainToday<-as.factor(wea.aus$RainToday)
wea.aus$RainTomorrow<-as.factor(wea.aus$RainTomorrow)
wea.aus$Date<-as.Date(wea.aus$Date,"%d-%m-%Y")
wea.aus$Date<-ymd(wea.aus$Date)


# Fetch Australian Cities and their Lat Long using world cities.
wea.aus.city<-unique(wea.aus$Location)
aus.cities<-as.data.frame(aus.cities)
aus.cities %>% filter( name %in% wea.aus.city)->aus.cities


# Data for plots Average Rainfall plot.
wea.aus %>% select(Date,Location,Rainfall,MinTemp,MaxTemp,RainToday,RainTomorrow) %>% na.omit(wea.aus)->clean.aus
clean.aus$Year<-year(clean.aus$Date)
# average rain distribution through years 

# Average year and cities
clean.aus %>% select(Location,Year,Rainfall)%>% group_by(Location,Year) %>% 
  summarise(Average=mean(Rainfall))->avg.rain
merge(avg.rain,aus.cities,by.x="Location",by.y="name") %>% select(Location,Average,lat,long,Year)->avg.year.cities

# show top 10 rainfall cities.
avg.rain %>% group_by(Location) %>% summarise(Max.Rain=sum(Average)) %>% 
  arrange(desc(Max.Rain)) %>% head(10)->top10.rain

# storm data
storm$Date.Time<-as.Date(storm$Date.Time,"%d-%m-%Y")
storm$Date.Time<-ymd(storm$Date.Time)
storm$Year<-year(storm$Date.Time)

# merge storm dataset
merge(wea.aus,storm,by.x = "Date",by.y="Date.Time")->rain.storm
na.omit(rain.storm)->rain.storm

# Color palette
colorNumeric(palette = "viridis",domain = avg.year.cities$Average,reverse = T)->cpal

# Icons
icons <- awesomeIcons(
  icon = 'ios-close',
  iconColor = 'black',
  library = 'ion',
  markerColor = 'green',
  spin = T
)

cloud<- icons(makeIcon("cloud.png",25,25))


## plotly font and labels

ply.font = list(
  #family = "DM Sans",
  size = 20,
  color = "white"
)
ply.label = list(
  bgcolor = "#232F34",
  bordercolor = "transparent",
  font = ply.font
)

##################################################################################
# Create the DashBoard.

# Complete layout as provided in the question can be implemented using sidebar and mainpanel.
# This layout is done twice to achieve the exact orientation.

# UI definition for Shiny Application.
ui <- dashboardPage(
  # layout of webpage
  skin = "blue",
  title = 'AUSTRALIAN WEATHER',
  dashboardHeader(
    titleWidth="100%",
    title = span(
      tags$img(src="rain_image.jpeg",width="100%"), 
      column(12, class="title-box",
             tags$h1(class="primary-title", style='margin-top:10px;', 'Australia Rainfall'), 
             tags$h2(class="primary-subtitle", style='margin-top:10px;', 'Understanding Trends and Patterns')
      )
    )
  ),
  # ,icon = icon("fas fa-location-arrow")
  dashboardSidebar(
    sidebarMenu(
      menuItem("About", tabName = "about", icon = icon("info")),
      menuItem("Rainfall", tabName = "rainfall",startExpanded = T,
               menuSubItem("Overall",tabName = "overall"),
               menuSubItem("Top 10 Cities",tabName = "top10")),
      menuItem("Temperature", tabName = "temp_main")
    )
  ),
  dashboardBody(
    # style.css
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "mystyle.css")
    ),
    
    # side bar menu content
    tabItems(
      # About shiny application
      tabItem(tabName = "about",
              box(
                title = "More information about this Shiny Application and Datasource",status="warning",width = 4, solidHeader = T,h4(
                tags$ol(h4("This application uses the weather information from Australian Bureau of Meteorology(BOM).
                Bureau of Meteorology of Australia collects various aspects of weather across stations located throughout the country.
                Weather enthusiast and people with basic knowledge on Australian cities can get benefited from this application."),
                br(),tags$li(strong("Rainfall Tab - Overall"),br(),"This application explores trends and rainfall in three novel views. Rainfall tab provides exploring overall 
                rainfall along with Storm Australia. Application is designed to provide users to analyze Average Rainfall with respect to Year
                from 2008 to 2017. Optionally, stations on the map can be selected to understand overall distribution through months in the given
                year. Storm along with Rainfall helps users to explore trends between these datasets. Bar View provides stations with their distribution
                for further understanding for a given year. Users could still use play button to automate the trends in both these views."),br(),
                tags$li(strong("Rainfall Tab - Top 10 Cities"),br()," This view gives an understanding on the Top 10 cities with the most rainfall
                for a given year. This is handy to understand the distribution of Rainfall for a given year and the cities that most contributed.
                Word Cloud helps the users understand the top 10 cities based on their average rainfall shown with the size and color palette. To further
                expand on the knowledge on Top 10 cities distribution of rainfall, bar view provides interactive information. Monthly view
                provides plots that uses Density to show months that observed the most rainfall amoungst these Top 10 cities."),br(),
                tags$li(strong("Temperature Tab"),br(), "Users can explore interactive plots that corresponds to Maximum, Minimum temperature
                        and understand how days with consequtive rainfall is distributed through various weather stations. Temperature can be 
                        selected with a range of values corresponding to a Year. Treemaps interactively provides Rainfall along with Temperature."),
                  )
                )
              ),
              box(
                title = "Cities with Rainfall and Weather information", background = "light-blue",solidHeader = T,
                leafletOutput("map_cities",height = 500),height = 570,width = 5
                ),
              box(
                title = "Table Summary of Weather Stations", solidHeader = TRUE,width = 2, status = "info",
                tableOutput("city_table")
              )
            ),
      # Rainfall
      tabItem
      (
        tabName = "overall",
              box(
                  title = "Interesting trends amoung Rainfall and Storm",status="warning",width = 4, solidHeader = T,h4(tags$ol(
                  tags$li(strong("Which part of Australia had the most rainfall?"),br(),"Northern Territory and Queensland states in Australia had received the most rainfall
                          based on both the storm and rainfall data. Cities including Cairns, Darwin and Townsville shows consistency through
                          the years."),br(),
                  tags$li(strong("Which part of Australia had least rainfall?"),br(),"Central Northern Territory state and East of New south wales state had the least rainfall.
                          Cities including Alice Springs, Mildura had the least average rainfall through all the years."),br(),
                  tags$li(strong("When was storm most extensive?"),br()," Years 2014 to 2017 have shown major storms through out Australia.
                          It is evident that coastal areas had major impact from these storms which is shown based on the heatmaps.")
                  ))
                ),
              fluidPage
              (
                box(
                    title="Analysis of Average Rainfall across Australia through Years 2008 to 2017",status="warning",width = 8,
                    
                tabBox(
                       title = "",
                       id = "tab1",
                       tabPanel("Map View",
                                  h4("Leaflet plot shows the geospatial information containing the Year corresponding Average rainfall for various cities.
                                    The color palatte shown in the Legend helps to understand the color and corresponding precipitation pattern. Optionally users can 
                                    hover on top of the circles on the map to view the city name and by clicking on the circles will reveal the city name and corresponding
                                    average precipitation value. Users can select the Type, Rainfall or Storm to update the Leaflet.
                                    Use the Clear button to reset any previously selected cities."),
                                  leafletOutput("leaf_year_rain",height = 700),
                                  absolutePanel(id = "controls",
                                                draggable = F,
                                                top = 220, left = 742, right = 100, bottom = "auto",
                                                width = 260, height = "700",
                                                h2("Rainfall & Storm Explorer"),
                                                # Slider input for year with loop play option.
                                                sliderInput("year","Choose the Year by dragging slider(click on play to animate)",
                                                min =2008,max=2017,value = 2008,sep = "",animate =animationOptions(interval = 1500,
                                                loop = FALSE)),
                                                # check box for variable selection
                                                checkboxGroupInput(inputId = "type_selection",choices = c('Rainfall','Storm'),
                                                                   label = "Type",
                                                                   selected = c("Rainfall","Storm")),
                                                # select a city station
                                                h4("Selected City",textOutput("city_selected")),
                                                actionButton("reset","Clear"),
                                                plotOutput("month_select_city",width =240 ,height = 240)
                                                
                                                )
                         
                       ),
                       tabPanel("Bar View",
                                h4(sliderInput("year_bar","Choose the Year by dragging slider(click on play to animate)",min =2008,max=2017,value = 2008,
                                            sep = "",animate =animationOptions(interval = 1500, loop = FALSE))),
                                  plotlyOutput("bar_year_rain",height = 600)
                                
                       ),
                       height = 950 , width = 12)
                )
              )
      ),
      # Top 10
      tabItem(tabName = "top10",
              box(
                title = "Interesting Trends in Top 10 cities",status="warning",width = 4, solidHeader = T,h4(tags$ol(
                  tags$li(strong("Which Cities had consistent Rainfall through 2008 to 2017?"),br(),"Cairns, Darwin, Coffs Harbour had the most
                          rainfall and consitent throughout the years. Cairns had a consistent rainfall average of 6 mm, Coffs Harbour
                          had a average of 5 mm and Darwin with 4 mm."),br(),
                  tags$li(strong("Determine the Relationship between Most rainfall months and Rainy season in Australia"),br(),"Rainy season 
                          in Australia is usually between Novemeber to April. Based on the Monthly distribution plot we can confirm the Wet season are indeed between November 
                          to April. Due to diversity in locations there are occasional anamolies where months June, October have also seen
                          major precipitations.")
                  
                )
                )
              ),
              fluidPage(
                box(
                  title="Analysis of Average Rainfall for TOP 10 cities through 2008 to 2017",status="warning",width = 8,
                  h4(sliderInput("year1","Choose the Year(optionally click on play to animate)",min =2008,max=2017,value = 2008,
                                 sep = "",animate =animationOptions(interval = 1500, loop = FALSE))),
                  
                  
                  tabBox(title = "",
                         id = "tab2",
                         tabPanel("Word Cloud",
                                  h4("The size and color gradient of the cities corresponds to the overall average rainfall for a given year.
                                     This plot can be used to infer the top 10 cities for which there was highest amount of precipitation received.
                                     Optionally while using the play option, users can quickly infer the top cities for years 2008 to 2017."),
                                  plotOutput("word_year_rain",height = 500)
                                  
                                  
                         ),
                         tabPanel("Year Bar plot",
                                  h4("This Bar plot provides the information about the top 10 cities for a selected user and their corresponding
                                     Average Rainfall. We can infer statistical information for the wordcloud that was shown in the previous tab."),
                                  plotlyOutput("top10_bar",height = 600)
                                  
                                  
                         ),
                         tabPanel("Monthly Distribution",
                                  h4("The size and color gradient of the cities corresponds to the overall average rainfall for a given year.
                                     This plot can be used to infer the months for which there was highest amount of precipitation received.
                                     Optionally while using the play option users can quickly get the overall picture of high and low precipitation
                                     months for these Top 10 cities."),
                                  plotOutput("month_rain",height = 600)
                                  
                                  
                         ),
                         height = 800 , width = 12)
                )
              )
      ),
      # Temperature
      tabItem(tabName = "temp_main",
              
              box(
                title = "Interesting Trends in Temperature",status="warning",width = 4, solidHeader = T,h4(tags$ol(
                  tags$li(strong("Which Cities with minimum temperature below 5 degree celsius received consistent rainfall?"),br(),"Mount Ginini had temperatures
                          below 5 degree celsius and had an average rainfall of 2.5 mm throughout the years. However most cities with temperatures around 8 to 10 degree celsius received 
                          an average of 3.5 mm rainfall."),br(),
                  tags$li(strong("What Maximum temperatures is most suitable for rainfall?"),br(),"Temperatures between 31 to 34 degree celsius shows
                                 significant rainfall amoungst most cities in Australia. These also includes the Top 10 cities that recieved the Most
                                 rainfall for a given year.Usually Temperatures above 37 degree celsius is least suitable for rainfall."),br(),
                  tags$li(strong("which city had the most consecutive days rainfall?"),br(),"Sydney had a consistent frequency of 50 of consecutive days rainfall through all the years.
                          Darwin and Cairns also showed consistency around a frequency of 40. It is important to note that these cities belong to the Top 10 cities.")
                  
                  ))
                ),
              
              fluidPage(
                box(
                  title="Analysis of Temperature with respect to Rainfall",status="warning",width = 8,height = 1200,
                  h4(sliderInput("year2","Choose the Year(optionally click on play to animate)",min =2008,max=2017,value = 2008,
                                 sep = "",animate =animationOptions(interval = 1500, loop = FALSE))),
                  
                  tabBox(title = "",
                         id = "tab2",
                         tabPanel("Maximum Temperature",
                                  
                                  h4("In this section users can select the range of Maximum temperature for which the analysis is shown using the
                                     Treemap. Users can select a city to zoom in or hover to reveal their related information",br()),
                                  h4(sliderInput("maxtemp_slide","Select the Maximum temperature Range",min =30,max=49,value = c(32,45),post="°C"),br()),
                                  plotlyOutput("tree_map_maxtemp",height = 700)
                                  
                                  
                         ),
                         tabPanel("Minimum Temperature",
                                  h4("In this section users can select the range of Minimum temperature for which the analysis is shown using the
                                     Treemap. Users can select a city to zoom in or hover to reveal their related information",br()),
                                  h4(sliderInput("mintemp_slide","Select the Minimum temperature Range",min =-8,max=25,value = c(-4,10),post="°C"),br()),
                                     plotlyOutput("tree_map_mintemp",height = 700)
                                  
                                  
                         ),
                         tabPanel("Consecutive days Rainfall",
                                  h4("In this section the Consecutive days rainfall frequency for a corresponding year is shown. By changing the year the cities
                                     with their corresponding precipitation on Consecutive days can be infered."),
                                  plotlyOutput("Cons_rain_bar",height = 700)
                                  
                                  
                         ),height = 800 , width = 12)
                )
              )
            )
         ) # tabitems
      ) # dashboard body
) # dashboardpage


# Server definition for the Shiny Application.
# Defines all the functions for the UI objects.
server <- function(input, output) {

# Marker with stations in info tab
output$map_cities<- renderLeaflet({
  
  icons <- awesomeIcons(
    icon = 'ios-close',
    iconColor = 'black',
    library = 'ion',
    markerColor = 'green'
  )
  
  aus.cities %>% leaflet() %>%
    addTiles() %>% 
    setView(lat = -25.8418, lng = 134.5706, zoom = 4) %>% 
    addProviderTiles("Esri.WorldStreetMap") %>%
    addAwesomeMarkers(lng=~long, lat=~lat, icon=icons,
                      popup=~name,
                      label=~as.character(name))
  
  })

# Station columns in table of info tab

output$city_table<-renderTable({
  
  wea.aus %>% select(Stations=Location) %>% unique() %>% as.data.frame()
  
},align = 'l')

observe({
  
  city.click<-input$leaf_year_rain_marker_click
  if(is.null(city.click))
    return()
  output$city_selected<-renderText({city.click$id})
  
  if(city.click$id %in% clean.aus$Location){
    
    output$month_select_city<-renderPlot({
      
      clean.aus %>% filter(Year==input$year, Location==city.click$id) %>% 
      mutate(Month= month(Date,label = T,abbr = T)) %>% group_by(Month) %>% 
      summarise(Average=mean(Rainfall)) %>% 
      ggplot(aes(x = Month, y = Average)) +
      geom_bar(fill="orange",stat = "identity")+theme_minimal()+
      labs(title=paste0("Distribution of months"),
             x= "Average Rainfall in mm",y="Months")
    })
    
  }
  
})

# Observe the clear event to reset selected city
observeEvent(input$reset,{
  
  output$month_select_city<-NULL
  output$city_selected<-NULL
  
})

# Leaflet for average Rainfall 
output$leaf_year_rain<- renderLeaflet({
  
  # icons for storm
  cloud <- icons("cloud.png","cloud.png@48px.png",24,24)
  
  # Default leaflet
  leaflet()  %>% 
    addTiles() %>% 
    addProviderTiles("Esri.WorldStreetMap") %>% 
    # Set center view for the map.
    setView(lat = -25.8418, lng = 140.5706, zoom = 4)->leaf.plot
  
  # plot for Storm data
  if("Storm" %in% input$type_selection){
    leaf.plot<-leaf.plot %>% addMarkers(
      data= storm %>% filter(Year==input$year & Latitude!=0 & Longitude!=0),
      lat = ~Latitude,
      lng = ~Longitude,
      popup = ~as.character(paste0("<strong>",Nearest.town,"</strong><br/>","Date: ",Date.Time,"<br/>Rain ID: ",Rain.ID)),
      label = ~as.character(Nearest.town),
      icon=cloud,
      layerId = ~Nearest.town  
      ) %>% addHeatmap(
                        data=storm %>% filter(Year==input$year & Latitude!=0 & Longitude!=0),
                        lng=~Longitude,
                        lat=~Latitude,
                        intensity= ~Total.precipitation,
                        max=100,
                        radius=20,
                        blur=10,
                        
                        )
    
    
  }
  
  # Data for Rainfall plot
  avg.year.cities %>% filter(Year==input$year)->temp.year
  
  # plot for Rainfall data
  if("Rainfall" %in% input$type_selection){
    leaf.plot <-leaf.plot %>% 
      addCircleMarkers(
        data = temp.year,
        lng=~long, 
        lat=~lat,
        popup=~as.character(paste0("<strong>",Location,"</strong><br/>","Average Rainfall: <br/>",round(Average,2))),
        label=~as.character(paste0(Location,"\nAverage Rainfall:",round(Average,2))),
        fillColor = ~cpal(Average),
        weight = 1,
        fillOpacity = 0.8,
        stroke = T,
        color="black",
        layerId = ~Location
        
      ) %>% 
      addLegend(position = "topleft",pal = cpal,values=temp.year$Average,title = "Average <br/> Rainfall<br/>(in mm)",opacity = 1)
  }
  
  # Add reset button on mapview
  leaf.plot<-leaf.plot %>%  addResetMapButton() # for Reset button.
  
leaf.plot 
  
})
# Bar char for average rainfall
output$bar_year_rain<-renderPlotly({
  
  avg.year.cities %>% filter(Year==input$year_bar) %>%ggplot(aes(y = round(Average,2), x =Location,text=paste0("Rainfall:",round(Average,2)))) +
    geom_bar(aes(fill=round(Average,2)),stat="identity")+coord_flip()+
    scale_fill_distiller(direction = 1)+
    labs(x="Locations", y="Average Rainfall in mm",fill="Average\nRainfall",
         title=paste0("Average rainfall distribution of various locations in Australia for year ",input$year_bar)
         )+theme(plot.background = element_rect(fill = "beige"),panel.background = element_rect(fill = "#000000ad"))->pl
    ggplotly(pl, tooltip=c("x","text")) %>%
    style(hoverlabel = ply.label) %>%
    layout(font=ply.font,
           yaxis = list(fixedrange = TRUE),
           xaxis = list(fixedrange = TRUE)) %>%
    config(displayModeBar = FALSE)
    
  
})

# Word cloud with top 10 cities
output$word_year_rain<-renderPlot({
  set.seed(42)
  avg.year.cities %>% filter(Year==input$year1) %>%  group_by(Location) %>% summarise(Max.Rain=sum(Average)) %>% 
  arrange(desc(Max.Rain)) %>% head(10)%>% 
    ggplot(aes(label = Location, size = Max.Rain,label_content = Max.Rain,color=Max.Rain,text=paste0("Average Rainfall:",Max.Rain))) +
    geom_text_wordcloud_area(rstep = .07,tstep = .001,shape = "diamond") +
    scale_size_area(max_size = 20) +
    theme_minimal()
  
})

# Monthly distribution
output$month_rain<-renderPlot({
  
    clean.aus %>% filter(Year==input$year1) %>%  group_by(Location,month(Date,label = T,abbr = T)) %>% summarise(Average=mean(Rainfall)) %>% 
    rename(Month=`month(Date, label = T, abbr = T)`) %>% filter(Location %in% top10.rain$Location)%>% group_by(Location,Month) %>% 
    summarise(Max.Rain=mean(Average)) %>% arrange(desc(Max.Rain)) %>% ggplot(aes(x = Max.Rain, y = Month,fill=stat(x))) +
    geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
    scale_fill_viridis_c(name = "Average Rainfall(mm)", option = "C") +
    labs(title="Average Rainfall Distribution of Top 10 cities to corresponding months.",
         subtitle=paste0("Data through year ",input$year1),x= "Average Rainfall in mm",y="Months") +
    theme(legend.position="bottom")+mytheme
  
})

# Bar plot for Top 10
output$top10_bar<- renderPlotly({
  
    avg.year.cities %>% filter(Year==input$year1) %>%  group_by(Location) %>% summarise(Max.Rain=sum(Average)) %>% 
    arrange(desc(Max.Rain)) %>% head(10) -> avg.top10.temp
    avg.top10.temp %>% ggplot( aes(x=Location, y=Max.Rain, fill=Max.Rain,text=paste0("Rainfall:",round(Max.Rain,2)))) + 
    geom_bar(stat="identity",position = "dodge") +
    scale_fill_distiller(palette = "Blues",limits=c(min(avg.top10.temp$Max.Rain),max(avg.top10.temp$Max.Rain)),direction = 1)+
    labs(title=paste0("Average Rainfall Distribution of Top 10 cities to corresponding Years ",input$year1)
         ,x= "Locations",y="Average Rainfall in mm",fill="Average Rainfall") +
    theme_minimal()+mytheme+
    theme(plot.background = element_rect(fill = "white"),
          panel.background = element_rect(fill = "#000000ad"),title = element_text(size = 10))+coord_flip()->yr
    ggplotly(yr, tooltip=c("x","text")) %>%
      style(hoverlabel = ply.label) %>%
      layout(font=ply.font,
             yaxis = list(fixedrange = TRUE),
             xaxis = list(fixedrange = TRUE)) %>%
      config(displayModeBar = FALSE)
  
  
})

# Treemap of Maxtemp
output$tree_map_maxtemp<-renderPlotly({
    clean.aus %>% filter(MaxTemp>input$maxtemp_slide[1] & MaxTemp<input$maxtemp_slide[2]& Year==input$year2) %>%
    group_by(Location) %>% summarise(avg_rain=round(mean(Rainfall),2),avg_temp=round(mean(MaxTemp),1))%>%
    filter(avg_rain>0.1)->max.temp.df
    plot_ly(data = max.temp.df,
            type='treemap',
            text=~paste0(Location,"\nTemperature : ",avg_temp,"\nAverage Rainfall: ",avg_rain),
            ids=~Location,
            labels=~avg_rain,
            parents=NA,
            values= ~avg_temp,
            marker=list(colorscale='Reds'),
            textinfo="text",
            hovertemplate = "Location: %{id}<br>Temperature: %{value}<br>Average Rainfall: %{label}<extra></extra>"
    ) %>% layout(title = paste0("Average Rainfall Distribution Cities with Maximum temperature between ",input$maxtemp_slide[1],
                                " and ",input$maxtemp_slide[2]," data through year ",input$year2)) %>% 
      style(hoverlabel = ply.label)
})

# Treemap of Mintemp
output$tree_map_mintemp<-renderPlotly({
  
  clean.aus %>% filter(MinTemp>input$mintemp_slide[1] & MinTemp<input$mintemp_slide[2]& Year==input$year2) %>%
    group_by(Location) %>% summarise(avg_rain=round(mean(Rainfall),2),avg_temp=round(mean(MinTemp),1))%>%
    filter(avg_rain>0.1)->min.temp.df
  
  plot_ly(data = min.temp.df,
          type='treemap',
          text=~paste0(Location,"\nTemperature : ",avg_temp,"\nAverage Rainfall: ",avg_rain),
          ids=~Location,
          labels=~avg_rain,
          parents=NA,
          values= ~avg_temp,
          marker=list(colorscale='Blues'),
          textinfo="text",
          hovertemplate = "Location: %{id}<br>Temperature: %{value}<br>Average Rainfall: %{label}<extra></extra>"
  ) %>% layout(title = paste0("Average Rainfall Distribution Cities with Minimum temperature between ",input$mintemp_slide[1],
                              " and ",input$mintemp_slide[2]," data through year ",input$year2)) %>% 
    style(hoverlabel = ply.label)
  

})

# Consecutive Days plot
output$Cons_rain_bar<-renderPlotly({
  
  clean.aus %>% filter(Year == input$year2 & RainToday=="Yes"& RainTomorrow=="Yes") %>% group_by(Location)%>% 
  summarise(Count = n())->conse.city

  # Plot Consecutive days rain cities
  conse.city %>% ggplot( aes(x=Location, y=Count, fill=Count)) + 
    geom_bar(stat="identity") +
    scale_fill_distiller(palette = "YlGnBu",limits=c(min(conse.city$Count),max(conse.city$Count)),direction = 1)+
    labs(title="Consecutive days Rainfall accross cities in Australia",
         subtitle=paste0("Data through year ",input$year2),x= "Locations",y="Frequency of Consecutive day Rainfall") +
    theme_grey()+coord_flip()+theme(plot.background = element_rect(fill = "white"),
                                    panel.background = element_rect(fill = "#000000ad"))->p
    ggplotly(p, tooltip=c("x","y")) %>%
    style(hoverlabel = ply.label) %>%
    layout(font=ply.font,
           yaxis = list(fixedrange = TRUE),
           xaxis = list(fixedrange = TRUE)) %>%
    config(displayModeBar = FALSE)
  
})


}



# 4. Deploy Shiny App
shinyApp(ui,server)

##################################################################################

