bodyGOI <- fluidRow(box(
  status = "warning", width = 4,
  tabBox(
    # The id lets us use input$tabset1 on the server to find the current tab
    id = "goi_plot", height = "100%", width = "100%",
    
    tabPanel("Search",
             helpText("Enter the Accession number of the gene",
                      tags$br(),
                      "Enter only one Accession number at a time",
                      tags$br(),
                      "Search function is NOT case-sensitive"),
             textInput("text", "Enter Accession number", 
                       value = "WP_000653994.1"),
             actionButton(inputId = "action", label = "Submit"),
             hr(),
             DT::dataTableOutput("gene_info"),
             hr(),
             downloadButton("downloadData", "Download All")
    )
  )
),
box(
  status = "warning", width = 8, height = "100%",
  plotOutput("WT"),
  plotOutput("DLON"),
  plotOutput("CLPP")
)
)

## Body content ####
shinyUI(dashboardPage(
  title="Chienlab_Vibrio_TMTbrowser - an interactive Shiny app for exploring Vibrio TMT data",
  skin = "black",
  dashboardHeader(
    # Set height of dashboardHeader
    title = tags$a(href='https://sites.biochem.umass.edu/chienlab/',
                   "Chienlab"),
    titleWidth = 150),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    bodyGOI
  )
)
)