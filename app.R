#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Linear Transformation: Rotation"),
   
   fluidRow(
     column(4,
        fluidRow(
          column(6, textOutput("random_vector_text"), tableOutput("random_vector_table")),
          column(6, textOutput("rotation_matrix_text"), tableOutput("rotation_matrix"))
        ),
        sliderInput("theta", "Degrees of Rotation:",
                    min = -360,
                    max = 360,
                    value = 0)
            
            ),
     column(4, textOutput("vector_plot_original_text"),
            plotOutput("vector_plot_original")),
     column(4,          textOutput("vector_plot_rotated_text"),
            plotOutput("vector_plot_rotated"))
     
   ),
   fluidRow(
     column(4,           textOutput("another_vector_text"),
            tableOutput("another_vector_table")),
     column(4, textOutput("added_rotation_text"),
            tableOutput("added_rotation_table")),
     column(4,          textOutput("multiplied_rotation_text"),
            tableOutput("multiplied_rotation_table"))
   ),
   
   fluidRow(
     column(12, h1(textOutput("warn_the_innocent"))))
   
   # Sidebar with a slider input for number of bins 
)



# Define server logic required to draw a histogram
server <- function(input, output) {
  create_rotation_matrix <- function(theta){
    radians <- theta * pi/180
    return(matrix(c(cos(radians), -sin(radians), sin(radians), cos(radians)), ncol = 2))
  }
  
  #random_matrix <- data.frame(rnorm(input$number_of_points, mean = 5, sd = 2.5), 
  #                            rnorm(input$number_of_points, mean = 5, sd = 2.5))
  #colnames(random_matrix) <- c("x", "y")
  random_vector <- rnorm(2, mean = 5, sd = 2.5)
  other_random_vector <- rnorm(2, mean = 5, sd = 2.5)
  
  added_vector <- random_vector + other_random_vector
  
  random_scalar <- rnorm(1, mean = 5, sd = 2.5)
  multiplied_vector <- random_scalar * random_vector
  
  
  global_lim <- c(-max(abs(random_vector)) - 0.5, max(abs(random_vector)) + 0.5)
  
  global_added_lim <- c(-max(abs(added_vector)) - 0.5, max(abs(added_vector)) + 0.5)
  
  global_multiplied_lim <- c(-max(abs(multiplied_vector)) - 0.5, max(abs(multiplied_vector)) + 0.5)
  
   #output$random_matrix_text <- renderText("A random matrix with 2 columns:")
  output$random_vector_text <- renderText("A random 2-dimensional vector:")
   #output$random_matrix_table <- renderTable(random_vector)
  output$random_vector_table <- renderTable(data.frame(random_vector))
  
  output$rotation_matrix_text <- renderText("The rotation matrix")
  output$rotation_matrix <- renderTable(create_rotation_matrix(input$theta))
  
   #output$matrix_plot_original_text <- renderText("The Original Matrix")
   output$vector_plot_original_text <- renderText("The Original Vector")
   output$vector_plot_original <- renderPlot({
     plot(t(matrix(c(c(0,0), random_vector), ncol=2)), xlim=global_lim, ylim=global_lim)
     arrows(0,0,random_vector[1], random_vector[2])
   })
   output$vector_plot_rotated_text <- renderText(paste("The plot, rotated ", input$theta, " degrees"))
   output$vector_plot_rotated <- renderPlot({
     rotated_vector <- create_rotation_matrix(input$theta) %*% random_vector
     plot(t(matrix(c(c(0,0), rotated_vector), ncol=2)), xlim=global_lim, ylim=global_lim)
     arrows(0,0,rotated_vector[1], rotated_vector[2])
   })
   
   output$another_vector_text <- renderText("Let's add another vector")
   output$another_vector_table <- renderTable(data.frame(other_random_vector))
   
   output$add_them_text <- renderText("A + B = ")
   add_them <- data.frame(random_vector, other_random_vector, added_vector)
   colnames(add_them) <- c("first vector", "second vector", "a + b")
   output$add_them_table <- renderTable(add_them)
   
   output$added_rotation_text <- renderText("Does T(A+B) = T(A) + T(B)?")
   
   output$added_rotation_table <- renderTable({
     rotate_addition <- data.frame(create_rotation_matrix(input$theta) %*% random_vector, 
                                   create_rotation_matrix(input$theta) %*% added_vector,
                                   create_rotation_matrix(input$theta) %*% random_vector + 
                                     create_rotation_matrix(input$theta) %*% added_vector,
                                   create_rotation_matrix(input$theta) %*% (random_vector + added_vector))
     colnames(rotate_addition) <- c("T(A)", "T(B)", "T(A)+ T(B)", "T(A+B)")
     rotate_addition})
   
   output$multiplied_rotation_text <- renderText("Does T(xA) = x * T(A)?")
   
   output$multiplied_rotation_table <- renderTable({
     rotate_multiply <- data.frame(create_rotation_matrix(input$theta) %*% (random_scalar * random_vector),
                                   random_scalar * (create_rotation_matrix(input$theta) %*% random_vector))
     colnames(rotate_multiply) <-rotate_multiply <- data.frame(create_rotation_matrix(input$theta) %*% (random_scalar * random_vector),
                                                               random_scalar * (create_rotation_matrix(input$theta) %*% random_vector))
     colnames(rotate_multiply) <- c("T(xA)", "x* T(A)")
     rotate_multiply})
   
   output$warn_the_innocent <- renderText(paste("If you think that writing a Shiny app for your presentation is a good idea: ",
                                                "Stop and get help. Nothing good comes of this."))
}

# Run the application 
shinyApp(ui = ui, server = server)

