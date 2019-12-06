######################
# Demo R-code Club 8 #
######################

list.of.packages <- c("shiny","ggplot2","DT","ggalt")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(shiny)
library(DT)
library(ggplot2)
library(ggalt)

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Demo progressbars and notifications"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30),
            actionButton("button","generate data"),
            plotOutput("analysisplot")
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("distPlot"),
            DT::dataTableOutput("errors")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    #Observeevent for the action button
    observeEvent(input$button,{
        #make lists that will be used later in the code for making table or scatterplot:
        timestamps <- c()
        process <- c()
        parts <- c()
        reasons <- c()
        randonumbers <- c()
        processingpower <- c()
        rondonumbersplot <- c()
        
        #make the 1st progress indicator
        withProgress(message = 'Performing analysis:', value = 0, {
            partstodo <- 30
            for (x in 1:partstodo) {
                #update the progressbar based on the progress that has been made
                incProgress(1/partstodo, detail = paste("Doing part", x,"/",partstodo))
                Sys.sleep(0.25)
                
                if (x%%10 == 0) {
                    #make the nested progress indicator
                    withProgress(message = 'Performing subaction:', value = 0, {
                        partssubaction <- 10
                        for (y in 1:partssubaction) {
                            incProgress(1/partssubaction, detail = paste("Doing part", y,"of subaction"))
                            Sys.sleep(0.5) 
                        }
                    })
                }
                
                if (x%%20 == 0) {
                    withProgress(message = 'Performing Looooooooooong and difficult subaction:', value = 0, {
                        partslonsubaction <- 45
                        for (z in 1:partslonsubaction) {
                            
                            #add points to plot
                            randonumber <- runif(1,1,10)
                            processpower <- runif(1,1,100)
                            processingpower <- c(processingpower,processpower)
                            rondonumbersplot <- c(rondonumbersplot,randonumber)
                            
                            #make/update plot
                                #make the dataframe for usage 
                            plotdataframe <- data.frame(processingpower,rondonumbersplot) 
                            error_select <- plotdataframe[plotdataframe$rondonumbersplot > 5 & 
                                                          plotdataframe$processingpower >= 50, ]
                            
                            warning_select <- plotdataframe[plotdataframe$rondonumbersplot > 2 & 
                                                            plotdataframe$rondonumbersplot < 4.5 &
                                                                plotdataframe$processingpower <= 50 &
                                                                plotdataframe$processingpower >= 25, ]
                            
                            output$analysisplot <- renderPlot({
                            ggplot(plotdataframe, aes(x=rondonumbersplot, y=processingpower)) + 
                                geom_point(aes()) +   # draw points
                                xlim(c(0, 10)) + 
                                ylim(c(0, 100)) +   # draw smoothing line
                                geom_encircle(aes(x=rondonumbersplot, y=processingpower), 
                                              data=error_select, 
                                              color="red", 
                                              size=2, 
                                              expand=0.02) +
                                geom_encircle(aes(x=rondonumbersplot, y=processingpower), 
                                              data=warning_select, 
                                              color="orange", 
                                              size=2, 
                                              expand=0.08) + # encircle
                                labs(subtitle="Difficult Task Analysis", 
                                     y="Processing power used (%)", 
                                     x="Number of hidden layers", 
                                     title="Scatterplot + Encircle", 
                                     caption="Source: Task analysis")
                            })
                            
                            timeremaining <- (partslonsubaction*1)-((z-1)*1)
                            incProgress(1/partslonsubaction, detail = paste("Doing part", z,"/",partslonsubaction,"of subaction. \n Time remaining:",timeremaining,"seconds"))
                            Sys.sleep(1) 
                            
                            #each time part gets done there is a -+50% chance there will be a notification
                            if (randonumber > 2 & processpower > 25 & processpower < 50 & randonumber <= 5) {
                                showNotification(type = "warning",paste0("Systemwarning ot Part ",z,"."))
                                timestamps <- c(timestamps,format(Sys.time()))
                                process <- c(process,"Long diff analysis")
                                randonumbers <- c(randonumbers,randonumber)
                                parts <- c(parts,z)
                                if (randonumber > 3 & processpower > 45 & processpower < 49) {
                                    reasons <- c(reasons,"warning: High processing power used")
                                }else{reasons <- c(reasons,"warning: Generic warning")}
                                
                            }
                            
                            
                            if (randonumber > 5 & processpower > 50) {
                                showNotification(type = "error",paste0("Part ",z," failed."))
                                timestamps <- c(timestamps,format(Sys.time()))
                                process <- c(process,"Long diff analysis")
                                randonumbers <- c(randonumbers,randonumber)
                                parts <- c(parts,z)
                                if (randonumber > 6 & randonumber < 7) {reasons <- c(reasons,"Error: Beyond comprehension")}
                                else if (randonumber > 7 & randonumber < 8) {reasons <- c(reasons,"Error: To much takata")}
                                else if (randonumber > 8 & randonumber < 9) {reasons <- c(reasons,"Error: Spanish Inquisition")}
                                else if (randonumber > 9 & randonumber < 10) {reasons <- c(reasons,"Error: Low ammount of chocolate")}
                                else if (randonumber > 5 & randonumber < 6) {reasons <- c(reasons,"Error: Task failed successfully")}
                            }
                            
                        }
                    })
                }
            }
            showNotification(type = "message","Analysis done.")
            #make dataframe from the errors:
            errorframe <- data.frame(timestamps,process,parts,reasons,randonumbers)
            
            #Make the histogram
            output$distPlot <- renderPlot({
                # generate bins based on input$bins from ui.R
                x    <- faithful[, 2]
                bins <- seq(min(x), max(x), length.out = input$bins + 1)
                
                # draw the histogram with the specified number of bins
                hist(x, breaks = bins, col = 'darkgray', border = 'white')
            })
            
            #make the DT-table
            output$errors <- renderDataTable(
                errorframe, options = list(lengthChange = FALSE,
                                           scrollX = TRUE,
                                        pageLength = 50,
                                        dom = 'Bfrtip',
                                        buttons = c('csv', 'excel',I('colvis')),
                                        colReorder = TRUE),
                #columnDefs = list(list(visible=TRUE, targets=c(24,16,5,10)))),
                extensions = c('Buttons','Responsive',"ColReorder"),
                escape = FALSE
            )
            
        })
    })
    
    
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)

