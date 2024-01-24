# Shiny App basics
# Always has a UI and server, in that order
# 
# Steps
# 1. Sketch app
# 2. Add the inputs to the UI
# 3. Add the outputs in the UI and server. 
# 4. Then, modify the app layout in the UI
# 5. Finally, update the outputs in the server to incorporate user inputs.

# The two fundamental components in reactive programming are reactive sources, and endpoints. A reactive source is typically a user input that comes through a browser interface. A reactive source can be connected to multiple endpoints, and vice versa. For example we might have a UI input widget to make a selection of our data, and the selected data can be used in multiple outputs like plots and summaries.
# 
# 4. Reactive endpoint
# A reactive endpoint is something that appears in browser window, such as a plot or a table. Endpoints are notified when the underlying value of one of its source dependencies change, and it updates in response to this signal. An example of reactive endpoints is observers. For example, an output object is a reactive observer. Under the hood a render function returns a reactive expression, and when you assign this reactive expression to an output dollar value, Shiny automatically creates an observer that uses the reactive expression.
# 
# 5. Reactive conductor
# There is a third type of reactive component called reactive conductor, that is dependent on one or more reactive sources, and is also a dependency of one or more reactive endpoints. It is often used to encapsulate repeated computations, that can be expensive. In this app, notice how we filter the babynames data twice, once in the plot output, and again in the table output. There are two issues at hand here. First, the code is repeated twice. Second, the code is evaluated twice.
# 
# 6. Reactive expressions
# We can solve both issues using an implementation of reactive conductors, called reactive expressions, to compute the filtered data. A reactive expression behaves just like a function, but with two key differences: 1. It is lazy, meaning that it is evaluated only when a reactive endpoint calls it. 2. It is cached, meaning that it is evaluated only when the value of one of its underlying reactive sources changes. Note that code inside a reactive needs to be wrapped inside curly braces.
# 

# Shiny App
# Always has a UI and server, in that order
library(shiny)

shinyWidgets()

# Simple app
ui <- fluidPage(
  selectInput('greeting_type', 'Select greeting', c("Hello", "Bonjour")),
  textInput('name', 'Enter your name'),
  textOutput('greeting')
)

server <- function(input, output, session) {
  output$greeting <- renderText({
    paste(input$greeting_type, input$name, sep = ", ")
  })
}

shinyApp(ui = ui, server = server)

################

ui <- fluidPage(
  # CODE BELOW: Build the UI layout keeping the
  #   inputs in the sidebar, outputs on the right, title on the top
  titlePanel("Most Popular Names"),
  sidebarLayout(
    sidebarPanel(
      # CODE BELOW: Add an input to let the user select sex (M / F)
      selectInput('sex', 'Select Sex', ___),
      # CODE BELOW: Add a slider to let the user select a year (1880 to 2017)
      sliderInput('year', 'Select Year', ___, ___, ___)
    ),
    mainPanel(
      plotOutput('plot')
    )
  )
)
server <- function(input, output, session) {
  output$plot <- renderPlot({
    top_names_by_sex_year <- get_top_names(input$___, input$___) 
    ggplot(top_names_by_sex_year, aes(x = ___, y = ___)) +
      geom_col()
  })
}
shinyApp(ui = ui, server = server)

##############
ui <- fluidPage(
  titlePanel("Baby Name Explorer"),
  shinythemes::themeSelector() #see selector, then add the theme in the ()
  # CODE BELOW: Add a sidebarLayout, sidebarPanel, and mainPanel
  sidebarLayout(
    sidebarPanel(textInput('name', 'Enter Name', 'David')),
    mainPanel(plotOutput('trend'))
    
  selectInput(
      'animal', 
      'Select Animal', 
      selected = 'Cat', 
      choices = c('Dog', 'Cat')
    )  
  #all input must have a unique name/id
  )
)

server <- function(input, output, session) {
  output$trend <- renderPlot({
    ggplot()
    #refer to your user input with input$uniqueid or whatever it's called
  })
}

shinyApp(ui = ui, server = server)



########leaflett example
ui <- fluidPage(
  # CODE BELOW: Add an appropriate title
  titlePanel("2014 Mental Health in Tech Survey"),
  sidebarPanel(
    # CODE BELOW: Add a checkboxGroupInput
    checkboxGroupInput(
      inputId = "mental_health_consequence",
      label = "Do you think that discussing a mental health issue with your employer would have negative consequences?",
      choices = c("Maybe", "Yes", "No"),
      selected = "Maybe"
    ),
    # CODE BELOW: Add a pickerInput
    pickerInput(
      inputId = "mental_vs_physical",
      label = "Do you feel that your employer takes mental health as seriously as physical health?",
      choices = c("Don't Know", "No", "Yes"),
      multiple = TRUE
    )
  ),
  mainPanel(
    # CODE BELOW: Display the output
    plotOutput("age")
  )
)

server <- function(input, output, session) {
  # CODE BELOW: Build a histogram of the age of respondents
  # Filtered by the two inputs
  output$age <- renderPlot({
    mental_health_survey %>%
      filter(
        mental_health_consequence %in% input$mental_health_consequence,
        mental_vs_physical %in% input$mental_vs_physical
      ) %>%
      ggplot(aes(Age)) +
      geom_histogram()
  })
}

shinyApp(ui, server)
