library(shiny) #to run the app
library(DT) #for the searchable/filterable data table
library(vroom) #allows files to be read into this app
library(tidyverse) #for most data wrangling/visualization functions
library(lubridate) #for the conversion of dates and times


ui <- fluidPage(
  titlePanel(title= "Exploration of long-term EXO Sonde data at MBT"),
  h1("Introduction"),
  
 sidebarPanel(div(img(src="ORCA_logo.jpg", height = 200, width = 200), style="text-align: center;")),
              
mainPanel(
  p("My name is Karrin Leazer, and I attended the Ocean Research College Academy (ORCA) from 2013-2015 as a member of Cohort 10. I am creating this app as a project for completion of my Master's Degree and Data Science Certificate at Western Washington University. I wanted to do something that will help current and future ORCA students, as ORCA holds a very special place in my heart and is the reason I am involved in science to this day. ")),

  p("This app lets you interactively explore data from the long-term deployed EXO Sonde at Mount Baker Terminal. You will need to download a .csv file from the 'MBT EXO3 Deployment Downloads' google drive available at ORCA. I have compiled all the data collected from 2020-09-01 to the present (the most recent data available was up until 2021-10-20) into one .csv file. You can use this file (which I have built-in to the app) or you can upload your own. It is important that any .csv file you upload keep the format you see in the example data...be sure to remove all units from the column headers and copy the names I used for each column...those names are what this app recognizes. If there is no data for a particular column, keep the column anyway and fill it with 'NA'. The source code for this app, as well as the entire .csv file for the example data ('AllMBT.csv') is available to view via my GitHub repository:"),
  a(href="https://github.com/knleazer/App-for-ORCA", "https://github.com/knleazer/App-for-ORCA"),
  
  hr(), #horizontal line separating the previous row from this one
  h1("Load Data"),
  p("To begin, first upload a file or use the provided example file.  The example file includes data from MBT from 2020-09-01 through 2021-10-20. An interactive data table that includes all of the data will be printed for whichever option you choose. Note that this app will be using the 'Combined' date and time column as the index...all variables will be viewed over time, and this column will be used on the x-axis in every case. If you compare the way your raw .csv file looks with the way the data table looks on this app, you'll notice that the dates and times look different. That is OK...I have programmed the data that way so that R will recognize actual dates and times and be able to function properly. Also, blank spaces in this data table are actually filled with 'NA' values in the .csv file."),
  
  fluidRow(
    fileInput(inputId="user_upload", 
              label=NULL,
              multiple = FALSE,
              accept = c(
                'text/csv',
                'text/comma-separated-values',
                '.csv')),
    checkboxInput(inputId="UseExampleData", label="Or use example data",
                  value=FALSE)),
  
  hr(),
  fluidRow(
    DTOutput('aDataTable') #print a data table for the uploaded data
  ),
  
  
  
  #Have the user select the date range they wish to view
  hr(), #horizontal line separating the previous row from this one
  h1("Select Date Range"),
  p("Next, select the date range you want to view. Note that if you used the example data, the app will only allow you to select dates between 2020-09-01 and 2021-10-20."),
  fluidRow(
    column(6,
           dateRangeInput("dateRange", "Date range:", #label appears above
                          start  = NULL,
                          end    = NULL, 
                          min    = NULL, #no minimum limits on the date
                          max    = NULL), #no maximum limits on the date
    )
  ),
  
  hr(), #horizontal line separating the previous row from this one
  h1("Time Series Plot"),
  p("Select the parameter you want to plot over the time period you selected above. Note that you have to input a dataset and select a date range in order for the plot to appear, and it may take some time to load at first (there are a total of 25,596 rows of data in the example data). If a blank plot appears, that means that there is no data for that variable over the time period you specified."),
  fluidRow(
    column(8, #user input to select the variable to plot
           selectInput("y", "Select Y-axis:",
                       choices= c("Conductivity (µS/cm)", "Depth (m)", "DO (% sat.)","DO (mg/L)", "Salinity (ppt)", "Turbidity (FNU)", "Wiper Position", "pH", "Temperature (°C)", "Battery V"), width = "100%")),
    #output from server as a plot
    column(12, plotOutput("time_series"))
  )
)


# Define server logic
server <- function(input, output, session) {
  GetData <- reactive({
    if (input$UseExampleData) {#if the user clicked the checkbox, return the example data (loaded here)
      dat <- read.csv("AllMBT.csv", check.names = F)
      dat$Date <- parse_date(dat$Date,"%m/%d/%y") #make a "Date" class
      dat$Combined <- parse_datetime(dat$Combined, #make a "POSIXt" class
                                     format = "%m/%d/%y %H:%M", 
                                     locale= locale(tz="US/Pacific"))
      return(dat)
    }
    
    inFile <- input$user_upload #create new object with the uploaded file
    if (is.null(inFile)) { #if the user didn't upload a file, return nothing
      return(NULL)
    }
    else{ #if the user uploaded a file, return that file
      dat <- read.csv(inFile$datapath)
      dat$Date <- parse_date(dat$Date,"%m/%d/%y") #make a "Date" class
      dat$Combined <- parse_datetime(dat$Combined, #make a "POSIXt" class
                                     format = "%m/%d/%y %H:%M", 
                                     locale= locale(tz="US/Pacific"))
      return(dat)
    }
    
  })
  
  #spit out a data table of the data that the user uploads
  output$aDataTable = renderDT({
    req(GetData()) # only run if the data have been gotten
  })
  
  
  # We will now subset the data to include the date range specified by the user using observe() and updateDateRangeInput()
  observe({
    req(GetData()) # only run this code if the data have been gotten
    theData <- GetData() #re-name data within this reactive
    updateDateRangeInput(session = session,
                         inputId = "dateRange", #look to this user input
                         start = min(theData$Date), #the lower limit set by the user 
                         end = max(theData$Date), #the upper limit set by the user
                         min = min(theData$Date),
                         max = max(theData$Date))
  })
  
  # Filter the data by the date range specified by the user
  GetDataByDate <- reactive({
    dat <- GetData() %>%
      filter(Date >= input$dateRange[1] & Date <= input$dateRange[2]) #filter by the "Date" column, and include all the dates greater than or equal to the start of the input date range, and all the dates less then or equal to the end of the input date range
    dat #new filtered data object that gets returned every time the GetDataByDate() reactive is called
  })
  
  #Output a time series plot, but make it conditional on the input that the user selects for the y - axis
  output$time_series <- renderPlot({
    req(GetData())
    req(GetDataByDate())
    if ("Temperature (°C)" %in% input$y) {
      GetDataByDate() %>% #look in the filtered data (including only the date range selected by the user)
        ggplot() + #"Combined" date and time column is the index
        geom_line(mapping = aes(x = Combined, y= Temperature), 
                  na.rm = TRUE) + #remove the "NAs" from the plot
        labs(y = "Temperature (°C)", x = "Date") +
        theme(axis.text=element_text(size=12),axis.title=element_text(size=14))
    } 
    else if ("Salinity (ppt)" %in% input$y) { GetDataByDate() %>% #look in the filtered data (including only the date range selected by the user)
        ggplot() + #"Combined" date and time column is the index
        geom_line(mapping = aes(x = Combined, y= Salinity), 
                  na.rm = TRUE) + #remove the "NAs" from the plot
        labs(y = "Salinity (ppt)", x = "Date") +
        theme(axis.text=element_text(size=12),axis.title=element_text(size=14))
    }
    else if ("Conductivity (µS/cm)" %in% input$y) { GetDataByDate() %>% #look in the filtered data (including only the date range selected by the user)
        ggplot() + #"Combined" date and time column is the index
        geom_line(mapping = aes(x = Combined, y= Conductivity), 
                  na.rm = TRUE) + #remove the "NAs" from the plot
        labs(y = "Conductivity (µS/cm)", x = "Date") +
        theme(axis.text=element_text(size=12),axis.title=element_text(size=14))
    }
    else if ("Depth (m)" %in% input$y) { GetDataByDate() %>% #look in the filtered data (including only the date range selected by the user)
        ggplot() + #"Combined" date and time column is the index
        geom_line(mapping = aes(x = Combined, y= Depth), 
                  na.rm = TRUE) + #remove the "NAs" from the plot
        labs(y = "Depth (m)", x = "Date") +
        theme(axis.text=element_text(size=12),axis.title=element_text(size=14))
    }
    else if ("DO (% sat.)" %in% input$y) { GetDataByDate() %>% #look in the filtered data (including only the date range selected by the user)
        ggplot() + #"Combined" date and time column is the index
        geom_line(mapping = aes(x = Combined, y= DO.percentSAT), 
                  na.rm = TRUE) + #remove the "NAs" from the plot
        labs(y = "DO (% saturation)", x = "Date") +
        theme(axis.text=element_text(size=12),axis.title=element_text(size=14))
    }
    else if ("DO (mg/L)" %in% input$y) { GetDataByDate() %>% #look in the filtered data (including only the date range selected by the user)
        ggplot() + #"Combined" date and time column is the index
        geom_line(mapping = aes(x = Combined, y= DO.mgL), 
                  na.rm = TRUE) + #remove the "NAs" from the plot
        labs(y = "DO (mg/L)", x = "Date") +
        theme(axis.text=element_text(size=12),axis.title=element_text(size=14))
    }
    else if ("Turbidity (FNU)" %in% input$y) { GetDataByDate() %>% #look in the filtered data (including only the date range selected by the user)
        ggplot() + #"Combined" date and time column is the index
        geom_line(mapping = aes(x = Combined, y= Turbidity), 
                  na.rm = TRUE) + #remove the "NAs" from the plot
        labs(y = "Turbidity (FNU)", x = "Date") +
        theme(axis.text=element_text(size=12),axis.title=element_text(size=14))
    }
    else if ("pH" %in% input$y) { GetDataByDate() %>% #look in the filtered data (including only the date range selected by the user)
        ggplot() + #"Combined" date and time column is the index
        geom_line(mapping = aes(x = Combined, y= pH), 
                  na.rm = TRUE) + #remove the "NAs" from the plot
        labs(y = "pH", x = "Date") +
        theme(axis.text=element_text(size=12),axis.title=element_text(size=14))
    }
    else if ("Wiper Position" %in% input$y) { GetDataByDate() %>% #look in the filtered data (including only the date range selected by the user)
        ggplot() + #"Combined" date and time column is the index
        geom_line(mapping = aes(x = Combined, y= Wiper.Position), 
                  na.rm = TRUE) + #remove the "NAs" from the plot
        labs(y = "Wiper Position (volts)", x = "Date") +
        theme(axis.text=element_text(size=12),axis.title=element_text(size=14))
    }
    else if ("Battery V" %in% input$y) { GetDataByDate() %>% #look in the filtered data (including only the date range selected by the user)
        ggplot() + #"Combined" date and time column is the index
        geom_line(mapping = aes(x = Combined, y= Battery.V), 
                  na.rm = TRUE) + #remove the "NAs" from the plot
        labs(y = "Battery (V)", x = "Date") +
        theme(axis.text=element_text(size=12),axis.title=element_text(size=14))
    }
    
  }, res = 96)
  
} #end of server


# Run the application 
shinyApp(ui = ui, server = server)
