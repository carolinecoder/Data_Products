library(shiny)
library(ggplot2)
library(markdown)

# Load and view data
theURL <- "http://databits.io/static_content/challenges/airbnb-user-pathways-challenge/airbnb_session_data.txt";
airbnb_data <- read.table(file = theURL, header = TRUE, sep = "|", na.strings = "NULL")

# Make date objects
airbnb_data$ts_min <- as.POSIXct(airbnb_data$ts_min)
airbnb_data$ts_max <- as.POSIXct(airbnb_data$ts_max)
airbnb_data$next_ts_min <- as.POSIXct(airbnb_data$next_ts_min)
airbnb_data$next_ts_max <- as.POSIXct(airbnb_data$next_ts_max)

# Separate device and application
library(tidyr)
airbnb_data <- separate(airbnb_data, dim_device_app_combo, into = c("device", "application"), sep = "-") 
airbnb_data <- separate(airbnb_data, next_dim_device_app_combo, into = c("next_device", "next_application"), sep = "-") 

# Create duration of session variable
airbnb_data$ts_total <- airbnb_data$ts_max - airbnb_data$ts_min
airbnb_data$ts_total <- round(airbnb_data$ts_total/60,0)
airbnb_data$next_ts_total <- airbnb_data$next_ts_max - airbnb_data$next_ts_min
airbnb_data$next_ts_total <- round(airbnb_data$next_ts_total/60,0)

# Get rid of the next session variables
airbnb_data <- airbnb_data[,1:12]

# Create day of the week for the session variable
airbnb_data$ts_weekday <- weekdays(airbnb_data$ts_min)

# Create a duration variable
airbnb_data$duration_in_min <- round(((airbnb_data$ts_max - airbnb_data$ts_min)/60) , 0)

# Create a super device variable
airbnb_data$super_device <- ifelse(grepl("Phone", airbnb_data$device)==T, "Phone", airbnb_data$device)
airbnb_data$super_device <- ifelse(grepl("Tablet", airbnb_data$device)==T, "Tablet", airbnb_data$super_device)
airbnb_data$super_device <- ifelse(grepl("iPad", airbnb_data$device)==T, "Tablet", airbnb_data$super_device)
airbnb_data$super_device <- ifelse(grepl("Desktop", airbnb_data$device)==T, "Desktop", airbnb_data$super_device)
airbnb_data$super_device <- ifelse(grepl("Other", airbnb_data$device)==T, "Other", airbnb_data$super_device)
airbnb_data$super_device <- ifelse(grepl("Unknown", airbnb_data$device)==T, "Other", airbnb_data$super_device)


# Make actions categorical
airbnb_data$did_search <- ifelse(airbnb_data$did_search==0, "NO", "YES")
airbnb_data$sent_message <- ifelse(airbnb_data$sent_message==0, "NO", "YES")
airbnb_data$sent_booking_request <- ifelse(airbnb_data$sent_booking_request==0, "NO", "YES")

# Get rid of the ID vars
airbnb_data <- airbnb_data[,c(3,5:7, 10:15)]

# Make correct data types
airbnb_data$device <- as.factor(airbnb_data$device)
airbnb_data$application <- as.factor(airbnb_data$application)
airbnb_data$date <- as.Date(airbnb_data$ds)
airbnb_data$did_search <- as.factor(airbnb_data$did_search)
airbnb_data$sent_message <- as.factor(airbnb_data$sent_message)
airbnb_data$sent_booking_request <- as.factor(airbnb_data$sent_booking_request)
airbnb_data$ts_weekday <- as.factor(airbnb_data$ts_weekday)
airbnb_data$duration_in_min <- as.numeric(airbnb_data$duration_in_min)
airbnb_data$super_device <- as.factor(airbnb_data$super_device)

# Relevel the factors
levels(airbnb_data$super_device) <- c("Desktop", "Phone", "Tablet", "Other")

# Define UI for application that draws the scatterplots
# Create the UI object
ui <- shinyUI(fluidPage(

# Main title
navbarPage("Airbnb User Pathways",
           
           # Tab1 title
           tabPanel("Plot",
                    
                    # Tab1 layout type 
                    sidebarLayout(
                        
                        # Tab1 layout container 1 (for inputs)
                        sidebarPanel(
                            
                            # Tab1 inputs content (device)
                            selectInput('device', 'Device Type', 
                                        levels(airbnb_data$super_device)),
                            
                            # Tab1 inputs content (action type)
                            selectInput('pick_action', 'User Action', 
                                        c("did_search", "sent_message", "sent_booking_request"))
                        ),
                        
                        # Tab1 layout contrainer 2 (for plots)
                        mainPanel(
                            
                            # Tab1 plots content
                            plotOutput("action")
                        )
                    )
           ),
           
           # Tab 2 title
           tabPanel("About",
                    
                    # Tab2 text content
                    textOutput("about")
           )
)
))

# Define server logic required to draw the scatterplots
server <- shinyServer(function(input, output) {
    
    output$action<- renderPlot({
        # draw the plot
        subset <- airbnb_data [airbnb_data$super_device==input$device, ]
        p <- ggplot(subset, aes_string(y = "duration_in_min", x = "dim_session_number", color=input$pick_action))
        p <- p + geom_point()
        p <- p + labs(x="Session Duration (in min)",y="Unique User Session Number") 
        print(p)
    })
    
    output$about <- renderText({"This Shiny app is based on Airbnb's User Pathways dataset. The data is a sample representing user sessions on the Airbnb site in the past year in a US city. It tracks the activities that users took in each session (did search, sent message, sent booking request). User sessions can be long, lasting over 10 hours, and a user can intiate many different sessions. The app allows the user to visualize when users take different actions, both in terms of the duration of a session and the session number, broken down by device type (e.g. desktop, phone)."})
    
    
})

# Run the application 
shinyApp(ui = ui, server = server)