# Shiny app designed to explore Tahoe data
# Heili Lowman
# January 9, 2024

# The following script will create a Shiny app with which to explore the raw
# datasets collected in Lake Tahoe between 2021-2023. The rough structure is:
# Shiny application = Packages/Data + User Interface + Server + shinyApp()

#### Setup ####
# Load packages.
library(tidyverse)
library(shiny)
library(shinythemes)
library(lubridate)
library(shinyWidgets)
library(patchwork)

# Load data.
dat <- readRDS("do_sat_shiny_010924.rds")
weather_long <- readRDS("weather_aggregated_long_110223.rds")
discharge <- readRDS("discharge_aggregated_121223.rds")

#### User interface ####
ui <- navbarPage("Nearshore Lake Tahoe Shiny App", # title
  theme = shinytheme("slate"),
  
  ##### Tab 1 #####
  tabPanel("Shiny Application Instructions",
           h2("Instructions", align = "left"),
           p("This web application displays data collected in Lake Tahoe as part of the research conducted by the Blaszczak lab members between 2021 and 2023. This page provides instructions on how to navigate the rest of the pages in this shiny application. Any further questions should be directed to Heili Lowman (hlowman@unr.edu).", align = "left"),
           br(),
           h3("First Deployment", align = "left"),
           p("This page displays data collected between June 2021 and May 2023, when instruments were deployed in a 'T'-shaped arrangement offshore of Glenbrook Creek on the east shore and Blackwood Creek on the west shore.", align = "left"),
           br(),
           p("Selections pertaining to the data you would like to visualize may be made using the selection tools in the left panel. Once you have chosen your desired data, click the 'Filter Data' button, wait just a moment, and the resulting data should appear on the screen. If you would like to export an image you have created, click the 'Download Figure' button and it will download the image onto your local computer.", align = "left"),
           br(),
           h3("Second Deployment", align = "left"),
           p("This page displays data collected between June 2023 and September 2023, when instruments were deployed parallel to shore in locations near and far from stream mouths. For this deployment, two sites are located on the east shore (Glenbrook and Slaughterhouse) and two on the west shore (Blackwood and Sunnyside).", align = "left"),
           br(),
           p("Selections pertaining to the data you would like to visualize may be made using the selection tools in the left panel. Once you have chosen your desired data, click the 'Filter Data' button, wait just a moment, and the resulting data should appear on the screen. If you would like to export an image you have created, click the 'Download Figure' button and it will download the image onto your local computer.", align = "left")),
  
  ##### Tab 2 #####
  tabPanel("First Deployment",
           sidebarLayout( # creates a sidebar
            sidebarPanel("Make data filtering selections here.",
                         
                 # dropdown menu widget #1
                 selectInput(inputId = "site_select", # variable name
                             label = "Choose either Blackwood (BW) or Glenbrook (GB):", # user title
                             choices = c("BW", "GB") # choices displayed
                 ),
                 
                 # dropdown menu widget #2
                 selectInput(inputId = "depth_select", # variable name
                             label = "Choose nearshore (~3m), shallow littoral (~10m), mid-depth littoral (~15m), or deep littoral (~20m) water depth:", # user title
                             choices = c("nearshore", "shallow littoral",
                                         "mid-depth littoral", "deep littoral") # choices displayed
                 ),
                 
                 # range selection widget
                 sliderTextInput(inputId = "date_select", 
                                 label = "Choose a date range:", 
                                 # create standard sequence of date times to choose from
                                 choices = seq(ymd_hms("2021-06-01 00:00:00"), 
                                               ymd_hms("2023-05-25 00:00:00"), by = "hour"), 
                                 selected = c("2022-03-01 00:00:00", "2023-03-01 00:00:00") # choices selected
                 ), 
                 
                 # action button to trigger the selections
                 actionButton("goFilter1", "Filter Data")
                 
            ),

    mainPanel("Plot based upon data filters imposed.", # created a main panel

              plotOutput(outputId = "Fig1"),

    )
    )
    ),
  
  ##### Tab 3 #####
  tabPanel("Second Deployment")

)

#### Server ####
server <- function(input, output){
  
##### Tab 1 #####
  
  # Empty of server functions
  
##### Tab 2 #####
  
  # Create a new filtered DO dataset
  dat_clean_site <- eventReactive(input$goFilter1, { # monitor when "Filter" button is clicked
    dat %>%
      # filter for site selected by the user
      filter(site == input$site_select) %>%
      # filter for water depth selected by the user
      filter(location_f == input$depth_select) %>%
      # filter for the dates selected by the user
      filter(Pacific_Standard_Time >= input$date_select[1]) %>%
      filter(Pacific_Standard_Time <= input$date_select[2])
  })
  
  # Create a new filtered Q dataset
  discharge_site <- eventReactive(input$goFilter1, { # monitor when "Filter" button is clicked
    discharge %>%
      # filter for site selected by the user
      filter(Site == input$site_select) %>%
      # filter for the dates selected by the user
      filter(datePST >= input$date_select[1]) %>%
      filter(datePST <= input$date_select[2])
    })
  
  # Create new dates for plotting too
  begin_date <- eventReactive(input$goFilter1, {
    ymd_hms(input$date_select[1])
  })
  end_date <- eventReactive(input$goFilter1, {
    ymd_hms(input$date_select[2])
  })
  
  # Display data
  output$Fig1 <- renderPlot({
    
    # Create new plots
    do_panel <- ggplot(dat_clean_site(), aes(x = Pacific_Standard_Time, 
                                           y = Dissolved_O_mg_L,
                                           group = date(Pacific_Standard_Time),
                                           color = replicate)) +
      geom_line() +
      scale_color_manual(values = c("#3B7D6E","#4CA49E","#7AC9B7")) +
      # force x axes bounds in case data is missing
      xlim(begin_date(), end_date()) +
      labs(x = "Date",
           y = "DO (mg/L)") +
      theme_bw() +
      theme(legend.position = "none")
    
    temp_panel <- ggplot(dat_clean_site(), aes(x = Pacific_Standard_Time, 
                                             y = Temperature_deg_C,
                                             group = date(Pacific_Standard_Time),
                                             color = replicate)) +
      geom_line() +
      scale_color_manual(values = c("#5A7ECB","#4B8FF7","#59A3F8")) +
      # force x axes bounds in case data is missing
      xlim(begin_date(), end_date()) +
      labs(x = "Date",
           y = "Temperature (C)") +
      theme_bw() +
      theme(legend.position = "none")
    
    q_panel <- ggplot(discharge_site(), aes(x = datePST, 
                                               y = dischargeCMS,
                                               group = date(datePST))) +
      geom_line(color = "#336887") +
      # force x axes bounds in case data is missing
      xlim(begin_date(), end_date()) +
      labs(x = "Date",
           y = "Discharge (cms)") +
      theme_bw()
    
    do_panel / temp_panel / q_panel
    
  })
  
  # Storing failed code for downloading button here for later use...
  
  # Downloadable png of figure
  # downloadablePlot("Fig1",
  #                  filenameroot = "mydownload1",
  #                  aspectratio = 1.33,
  #                  downloadfxns = list(png = download_Fig1),
  #                  visibleplot = download_Fig1)
  
  # output$downloadFig1 <- downloadHandler(
  #   # enter filename generated above
  #   filename = function(){
  #     "Fig1.jpg"
  #     #paste(input$site_select, input$date_select[1], input$date_select[2], "Tahoe.jpg", sep = "_")
  #   }
  #   # choose content to be downloaded
  #   content = function(file) {
  #     file.copy("Fig1.jpg", file, overwrite=TRUE)
  #   }
  # )
  
##### Tab 3 #####
  
  
  
}

# Combine the user interface and server:
shinyApp(ui = ui, server = server)

# Additional Shiny resources:
# https://shiny.rstudio.com/gallery/widget-gallery.html
# http://shinyapps.dreamrs.fr/shinyWidgets/
# https://shiny.rstudio.com/gallery/#user-showcase

# End of script.
