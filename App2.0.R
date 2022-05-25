library(shiny)
library(DT)
library(vroom)
library(tidyverse)
library(zoo) #to convert to a time series (.zoo object)
library(lubridate) #for the strptime() function


ui <- fluidPage(
  h1("Introduction"),
  p("My name is Karrin Leazer, and I attended ORCA from 2013-2015 as a member of Cohort 10. I am creating this app as a project for completion of my Master's Degree and Data Science Certificate at Western Washington University. I wanted to do something that will help current and future ORCA students, as ORCA holds a very special place in my heart and is the reason I am involved in science to this day. "),
  h1("This App"),
  p("This app lets you interactively explore data from the long-term deployed EXO Sonde at Mount Baker Terminal. You will need to download a .csv file from the 'MBT EXO3 Deployment Downloads' google drive available at ORCA. I have compiled all the data collected from 2020-09-01 to the present (the most recent data available was up until 2021-10-20) into one .csv file. You can use this file (which I have built-in to the app) or you can upload your own. Some helpful units to keep in mind: 'Conductivity' has units of (μS/cm),'Depth' has units of (m), 'DO.percentSAT' has units of (% saturation), 'DO.mgL' has units of (mg/L), 'Salinity' has units of (ppt), 'Turbidity' has units of (FNU), 'Temperature' has units of (°C), and 'Wiper.Position' has units of (volts). It is important that any .csv file you upload keep the format you see in the example data...be sure to remove all units from the column headers and copy the names I used for each column...those names are what the app recognizes. If there is no data for a particular column, keep the column anyway and fill it with 'NA'."),
  
  hr(), #horizontal line separating the previous row from this one
  h1("Load Data"),
  p("To begin, first upload a file or use the provided example file.  The example file includes data from MBT from 2020-09-01 through 2021-10-20. An interactive data table that includes all of the data will be printed for whichever option you choose. Note that this app will be using the 'Combined' date and time column as the index...all variables will be viewed over time, and this column will be used on the x-axis in every case. If you compare the way your raw .csv file looks with the way the data table looks on this app, you'll notice that the dates and times look different. That is OK...I have programmed the data that way so that R will recognize actual dates and times and be able to function properly. Also, blank spaces in this data table are actually filled with 'NA' in the .csv file."),
  
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
  p("Select the parameter you want to plot over the time period you selected above. NOTE...I have only gotten the temperature plot to work (currently, the plot will not change when you pick a different option for the y-axis! I am still working on that.)"),
  fluidRow(
    column(8, #user input to select the variable to plot
           selectInput("y", "Y-axis",
                       choices= c("Conductivity", "DO (% sat.)","DO (mg/L)", "Salinity (ppt)", "Turbidity (FNU)", "Wiper Position", "pH", "Temperature (°C)", "Battery V"), width = "100%")),
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
  
  
  output$time_series = renderPlot({
    req(GetData()) # only run if the data have been gotten (uploaded or example data used)
    
      GetDataByDate() %>% #look in the filtered data (including only the date range selected by the user)
      ggplot() + #"Combined" date and time column is the index
      geom_line(mapping = aes(x = Combined, y= Temperature), 
                na.rm = TRUE) + #remove the "NAs" from the plot
      labs(y = "Temperature (°C)", x = "Date") +
        theme(axis.text=element_text(size=12),axis.title=element_text(size=14))
})
  
  
  
} #end of server


# Run the application 
shinyApp(ui = ui, server = server)
