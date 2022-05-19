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
  p("This app lets you interactively explore data from the long-term deployed EXO Sonde at Mount Baker Terminal. You will need to download a .csv file from the 'MBT EXO3 Deployment Downloads' google drive available at ORCA. I have compiled all the data collected from 2020-09-01 to the present (the most recent data available was up until 2021-10-20) into one .csv file. You can use this file (which I have built-in to the app) or you can upload your own. In the original .csv file, 'Date' has units of (MM/DD/YYYY), 'Time' has units of (HH:mm:ss), and 'Combined' is the combined date and time for that data point.  I have converted the 'Combined' column into a date/time format that R understands (what the Date.Value column attempted to do in Excel). Some helpful units to keep in mind: 'Conductivity' has units of (μS/cm),'Depth' has units of (m), 'DO.percentSAT' has units of (% saturation), 'DO.mgL' has units of (mg/L), 'Salinity' has units of (ppt), 'Turbidity' has units of (FNU), 'Temperature' has units of (°C), and 'Wiper.Position' has units of (volts)."),
    
  hr(), #horizontal line separating the previous row from this one
  p("To begin, first upload a file or use the provided example file.  The example file includes data from MBT from 2020-09-01 through 2021-10-20. The data summary will be printed for whichever option you choose."),

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
  # placeholder -- summary text verbatim on app
  verbatimTextOutput("spitOutSummary")
),

#Want to print a data table (may move this to be after I clean/filter the data)
hr(), #horizontal line separating the previous row from this one
h1("Data Table"),
p("I cannot currently get this to work...am still trying. I also may want to move this down to show the data after it has been filtered (using the dates the user selects in the next section."),
fluidRow(
  column(8, tableOutput("data_table"))
),

#What is the date range?
hr(), #horizontal line separating the previous row from this one
h1("Select Dates"),
p("Next, select the date range you want to view. Note that if you used the example data, the app will only work if you select dates between 2020-09-01 and 2021-10-20."),
fluidRow(
  column(6,
         dateInput(inputId= "start_date", 
                        label= "Start of date range?", 
                        min = "1990-01-01",
                        max = "3000-01-01", 
                        format = "yyyy-mm-dd", 
                        startview = "month", 
                        weekstart = 0,
                        language = "en", 
                        width = NULL)),
  column(6,
         dateInput(inputId= "end_date", 
                   label= "End of date range?", 
                   min = "1990-01-01",
                   max = "3000-01-01", 
                   format = "yyyy-mm-dd", 
                   startview = "month", 
                   weekstart = 0,
                   language = "en", 
                   width = NULL)), 
),


hr(), #horizontal line separating the previous row from this one
h1("Time Series Plot"),
p("Select the parameter you want to plot over the time period you selected above."),

#the time series plot
fluidRow(
  #user input to select the variable to plot
  column(8, 
         selectInput("y", "Y-axis",
                     choices= c("Conductivity", "DO (% sat.)","DO (mg/L)", "Salinity (ppt)", "Turbidity (FNU)", "Wiper Position", "pH", "Temperature (°C)", "Battery V"), width = "100%")),
  #output from server as a plot
  column(12, plotOutput("time_series"))
  )
)


# Define server logic
server <- function(input, output, session) {
  Data <- reactive({
    if (input$UseExampleData) { #if the user clicked the checkbox, return the example data (loaded here)
      dat <- read.csv("AllMBT.csv", check.names = F)
      return(dat)
    }
  inFile <- input$user_upload #create new object with the uploaded file
    if (is.null(inFile)) { #if the user didn't upload a file, return nothing
      return(NULL)
    }
    else{ #if the user uploaded a file, return that file
      dat <- read.csv(inFile$datapath)
      return(dat)
    }
  })
  
  # simple output of data summary
  output$spitOutSummary <- renderPrint({
    # return a summary that will get printed
    summary(Data())
  })
  
  
  #This isn't working/my data is not visible...
#output$data_table<- renderDataTable(Data())
  
#Now need to clean my data...don't know if this is doable as I've written it.  I tried to use the reactive from up above inside this new reactive (need to change the class to a .zoo object after having converted certain columns to date/time)...
# I'm hoping to make my cleaned_data() reactive a .zoo object that can then be subset in the next step
cleaned_data <- reactive({Data() %>% 
                            strptime(Data()$Combined, format = "%m/%d/%y %H:%M", tz="US/Pacific") %>% #Convert the combined date and time column to date-time that R will understand
                            as.Date(Data()$Date, format = "%m/%d/%y") %>% #Convert the date column to date format
    zoo(Data(), order.by = Data()$Combined) #convert to a .zoo object (can now be subset)
    })

output$data_table <- renderDataTable(cleaned_data())

#create a subset of the data for plotting...the date range comes from the user inputs where they selected dates). Then, fortify it from a .zoo object back to a data frame for easy plotting in ggplot
subsetted_data <- reactive ({cleaned_data() %>% 
    window(cleaned_data(), start = as.Date(input$start_date), end = as.Date(input$end_date)) %>% 
    fortify.zoo(cleaned_data())
    })
}


# Run the application 
shinyApp(ui = ui, server = server)



#NOTES:
#output$data_table<- DT::renderDataTable({
# df <- df_upload()
#DT::datatable(df)
#})

