library(shiny)
library(semantic.dashboard)
library(ggplot2)
library(plotly)
library(DT)

# Define UI ----

ui <- dashboardPage(
  dashboardHeader(color = "blue", title = "Glassdoor", inverted = TRUE),
  dashboardSidebar(
    size = "thin", color = "teal", 
    sidebarMenu(
      menuItem(tabName = "main", "Main"), 
      menuItem(tabName = "tabSubRatings", "Sub Ratings"),
      menuItem(tabName = "extras", "Extras"),
      menuItem(tabName = "data", "Data")
    )
  ),
  dashboardBody(
    tabItems(
      selected = 1,
      tabItem(
        tabName = "main", 
        fluidRow(
          box(width = 8,
              title = "Answers",
              color = "green", ribbon = TRUE, title_sdie = "top right",
              column(width = 8, plotOutput("boxplot1"))
          ),
          box(width = 8,
              title = "Overall Rating",
              color = "green", ribbon = TRUE, title_sdie = "top right",
              column(width = 8, plotOutput("plotOverall"))
          )    
        )
      ),
      tabItem(
        tabName = "tabSubRatings",
        fluidRow(
          box(width = 8,
              title = "Work Life Balance",
              color = "green", ribbon = TRUE, title_sdie = "top right",
              column(width = 8, plotOutput("plotWorkLifeBalance"))
          ),
          box(width = 8,
              title = "Culture & Values",
              color = "green", ribbon = TRUE, title_sdie = "top right",
              column(width = 8, plotOutput("plotCultureValues"))
          ),
          box(width = 8,
              title = "Carrer Oportunities",
              color = "green", ribbon = TRUE, title_sdie = "top right",
              column(width = 8, plotOutput("plotCarrerOportunities"))
          ),
          box(width = 8,
              title = "Compensation & Benefits",
              color = "green", ribbon = TRUE, title_sdie = "top right",
              column(width = 8, plotOutput("plotCompBenefits"))
          ),
          box(width = 8,
              title = "Senior Management",
              color = "green", ribbon = TRUE, title_sdie = "top right",
              column(width = 8, plotOutput("plotSeniorManagement"))
          )
        )
      ),
      tabItem(
        tabName = "extras",
        box(width = 8,
            title = "Recommendation",
            color = "green", ribbon = TRUE, title_sdie = "top right",
            column(width = 8, plotOutput("plotRecommendation"))
        ),
        box(width = 8,
            title = "Outlook",
            color = "green", ribbon = TRUE, title_sdie = "top right",
            column(width = 8, plotOutput("plotOutlook"))
        ),
        box(width = 8,
            title = "CEO",
            color = "green", ribbon = TRUE, title_sdie = "top right",
            column(width = 8, plotOutput("plotCEO"))
        )
      ),
      tabItem(
        tabName = "data",
        fluidRow(
          dataTableOutput("glassdoor")
        )
      )
    )
  ),theme = "cerulean"
)

# Define server logic ----
server <- shinyServer(function(input, output,session) {
  data("glassdoor")
  colscale <- c(semantic_palette[["red"]], semantic_palette[["green"]], semantic_palette[["blue"]])

  output$boxplot1 <- renderPlot({
    ggplot(glassdoor, aes(x = Company)) +
      geom_bar(fill = semantic_palette[["blue"]]) + 
      coord_flip() + xlab ("") + ylab("Qtd. Answers") 
  })
  
  output$plotOverall <- renderPlot({
    ggplot(glassdoor, aes(x = Company, y = Rating)) +
      geom_boxplot(fill = semantic_palette[["blue"]]) + 
      coord_flip() + xlab ("") + ylab("Rating")
  })
  
  output$plotWorkLifeBalance <- renderPlot({
    ggplot(glassdoor, aes(x = Company, y = WorkLifeBalance)) +
      geom_boxplot(fill = semantic_palette[["blue"]]) + 
      coord_flip() + xlab ("") + ylab("Rating")
  })

  output$plotCultureValues <- renderPlot({
    ggplot(glassdoor, aes(x = Company, y = CultureValues)) +
      geom_boxplot(fill = semantic_palette[["blue"]]) + 
      coord_flip() + xlab ("") + ylab("Rating")
  })

  output$plotCarrerOportunities <- renderPlot({
    ggplot(glassdoor, aes(x = Company, y = CarrerOportunities)) +
      geom_boxplot(fill = semantic_palette[["blue"]]) + 
      coord_flip() + xlab ("") + ylab("Rating")
  })
  
  output$plotCompBenefits <- renderPlot({
    ggplot(glassdoor, aes(x = Company, y = CompBenefits)) +
      geom_boxplot(fill = semantic_palette[["blue"]]) + 
      coord_flip() + xlab ("") + ylab("Rating")
  })
  
  output$plotSeniorManagement <- renderPlot({
    ggplot(glassdoor, aes(x = Company, y = SeniorManagement)) +
      geom_boxplot(fill = semantic_palette[["blue"]]) + 
      coord_flip() + xlab ("") + ylab("Rating")
  })
  
  output$plotRecommendation <- renderPlot({
    ggplot(glassdoor) +
      geom_bar(mapping = aes(x=Company, fill = Recommendation), position = "fill") +
      coord_flip() + theme(legend.position = "bottom") +
      xlab ("") + ylab("")
  })

  output$plotOutlook <- renderPlot({
    ggplot(glassdoor) +
      geom_bar(mapping = aes(x=Company, fill = Outlook), position = "fill") +
      coord_flip() + theme(legend.position = "bottom") +
      xlab ("") + ylab("")
  })
  
  output$plotCEO <- renderPlot({
    ggplot(glassdoor) +
      geom_bar(mapping = aes(x=Company, fill = AdviceCEO), position = "fill") +
      coord_flip() + theme(legend.position = "bottom") +
      xlab ("") + ylab("")
  })
    
  output$glassdoor <- renderDataTable(glassdoor, rownames = FALSE,
                                      extensions = c("FixedColumns", "Scroller", "Buttons"),
                                      options = list(width='200px',
                                                     rowid=FALSE,
                                                     scroller = TRUE,
                                                     scrollX = TRUE,
                                                     scrollY = "500px",
                                                     fixedColumns = list(leftColumns = 3),
                                                     dom = "Bftip",
                                                     buttons = c('csv', 'excel')
                                                     ),
                                      class = 'dt-wrap'
                                      )
})

# Run the app ----
shinyApp(ui = ui, server = server)