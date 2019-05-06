
library(dplyr)
library(ggplot2)
library(readr)
library(shiny)
library(ggthemes)

df  <- read_csv("avm_transtype_clean.csv") %>%
  select(ACADR_NAME,UNADR_KEY, PROP_PURPRICE,PROP_VALUATION_AVM,PROP_VALUATION_SVUR,PROP_AVMCONFIDENCE_CODE,PROP_TRANSTYPE_NAME,MUNI_NAME,
         PROP_VALUATION_AVMMODA,PROP_VALUATION_AVMMODB,PROP_VALUATION_AVMMODC)

municipality <- factor(df$MUNI_NAME)
#df <- df %>% select(-MUNI_NAME)
#trans_type <- factor(df$PROP_TRANSTYPE_NAME)

# Define UI for application that plots features of movies
ui <- fluidPage(
  
  titlePanel("AVM check: Transaction Type"),
  
  # Sidebar layout with a input and output definitions
  sidebarLayout(
    
    # Inputs
    sidebarPanel(
      
      selectInput(inputId = "muni",
                  label   = "Municipality:",
                  choices = levels(municipality),
                  selected = "København"),
      
      selectInput(inputId = "x",
                  label   = "X-axis:",
                  choices = c("PROP_PURPRICE","PROP_VALUATION_AVM","PROP_VALUATION_SVUR","PROP_VALUATION_AVMMODA","PROP_VALUATION_AVMMODB","PROP_VALUATION_AVMMODC"),
                  selected = "PROP_VALUATION_SVUR"),
      
      selectInput(inputId = "y",
                  label   = "Y-axis:",
                  choices = c("PROP_PURPRICE","PROP_VALUATION_AVM","PROP_VALUATION_SVUR","PROP_VALUATION_AVMMODA","PROP_VALUATION_AVMMODB","PROP_VALUATION_AVMMODC"),
                  selected = "PROP_VALUATION_AVM"),
      
      selectInput(inputId = "colour",
                  label   = "Colour by:",
                  choices = c("PROP_AVMCONFIDENCE_CODE", "PROP_TRANSTYPE_NAME"),
                  selected = "PROP_TRANSTYPE_NAME"),
      
      sliderInput(inputId = "alpha",
                  label   = "Alpha:",
                  min = .01,max=1,step=.01,
                  value = .5)),
    
    # Outputs
    mainPanel(
      plotOutput(outputId = "scatterplot",height=600,
                 brush = brushOpts(id = "plot_brush")),
      textOutput(outputId = "corr"),
      h4("Selected points"),
      verbatimTextOutput("brush_info")
      
      )))
      
  

# Define server function required to create the scatterplot
server <- function(input, output) {
  
  # Create the scatterplot object the plotOutput function is expecting
  
  output$scatterplot <- renderPlot({
    
    df  <- read_csv("avm_transtype_clean.csv") %>%
      select(ACADR_NAME,UNADR_KEY, PROP_PURPRICE,PROP_VALUATION_AVM,PROP_VALUATION_SVUR,PROP_AVMCONFIDENCE_CODE,PROP_TRANSTYPE_NAME,MUNI_NAME,
             PROP_VALUATION_AVMMODA,PROP_VALUATION_AVMMODB,PROP_VALUATION_AVMMODC)
    
    municipality <- factor(df$MUNI_NAME)
    #df <- df %>% select(-MUNI_NAME)
    
    df <- df %>% filter(MUNI_NAME == input$muni)
      
      ggplot(data = df, aes_string(x = input$x, y = input$y, colour = input$colour)) +
        geom_point(alpha = input$alpha, size = 3) +
        theme_minimal() +
        theme(plot.title = element_text(size=24),
              axis.title = element_text(size=18),
              axis.text = element_text(size=16)) +
        ggtitle(input$muni) +
        scale_colour_tableau() +
        guides(colour = guide_legend(override.aes = list(alpha = 1)))
    
  })
  
  
  output$corr <- renderText({
    
    df <- df %>% dplyr::filter(MUNI_NAME == input$muni)
    
    r <- round(cor(df[, input$x], df[, input$y], use = "pairwise"), 3)
    paste0("Correlation = ", r)
    
  })
  
  output$brush_info <- renderPrint({
    
    df <- df %>% 
      dplyr::filter(MUNI_NAME == input$muni)
    
    brushedPoints(df, input$plot_brush)
  })
}

# Create a Shiny app object
shinyApp(ui = ui, server = server)

