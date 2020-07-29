#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinydashboard
#' @import plotly
#' @import igvShiny
#' @import htmlwidgets 
#' @import shinycssloaders
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here 
    dashboardPage(skin="red",
      # Application title
      dashboardHeader(title="SQANTI Explorer"),
      
      # Sidebar with a slider input for number of bins
      dashboardSidebar(width=350,
                       sidebarMenu(id="tabs",
                         menuItem("Data", tabName="data", icon=icon("database")),
                         menuItem("Plots", tabName="plots", icon=icon("chart-bar")),
                         menuItem("Genome Browser", tabName = "browser", icon=icon("dna")),
                         hr(),
                         h4("Filter data: ", style = "margin-left: 1.3rem"),
                         checkboxInput("polyexonic", "Only Polyexonic"),
                         checkboxInput("monoexonic", "Only Monoexonic"),
                         checkboxInput("noRTS", "No RTS"),
                         checkboxInput("noIntraPriming", "No intra-priming"),
                         checkboxInput("allCanonical", "All Canonical SJs"),
                         checkboxInput("minCovGTZero", "min_cov > 0"),
                         checkboxInput("onlyGenes", "Only Genes (does not accumulate values from transcripts)"),
                         uiOutput("updateButton"),
                         bookmarkButton()
                       )
      ),
      dashboardBody(
        tabItems(
          tabItem(tabName="data",
                  fluidRow(
                    box(
                      title="Add Classification Files",
                      fileInput("classification_file", with_red_star("Classification File: "), accept = c(".txt")),
                      fileInput("gtf_file", with_red_star("GTF (GFF v2) File: "), accept = c(".gtf")),
                      textInput("name", with_red_star("Name: ")),
                      hr(),
                      h4("Genome (only required for genome browser)"),
                      selectInput("genome", label="Genome: ", choices=igvShiny::getSupportedGenomes()),
                      actionButton("addClassification", label = "Add Classification File"),
                      hr(),
                      with_red_star("required field = ")
                    ),
                    box(title = "Your Inputs", withSpinner(DT::dataTableOutput('inputTable')), downloadButton("downloadData", "Download Data")),
                  )
          ),
          tabItem(tabName="plots",
                  fluidRow(
                    column(width=4,
                           box(width=NULL,
                               title="Group By",
                               selectInput("groupBy", "Group By:", list(
                                 "SQANTI Filter" = "SQANTI_filter", "Novel Transcript" = "novel_transcript", "Novel Gene" = "novel_gene", "All (no grouping)" = "name")
                               )
                           ),
                           valueBoxOutput(width=NULL, "selected_transcript_count"),
                           box(width = NULL, title="Download",
                               downloadButton("downloadFilteredData", "Download Filtered Classification", style = "margin-bottom: 1rem"),
                               downloadButton("downloadSelectedData", "Download Selected Classification", style = "margin-bottom: 1rem"),
                               downloadButton("downloadFilteredGTF", "Download Filtered GTF", style = "margin-bottom: 1rem"),
                               downloadButton("downloadSelectedGTF", "Download Selected GTF", style = "margin-bottom: 1rem")
                           )
                    ),
                    column(width=8,
                           tabBox(width=NULL,title = "Plot",height = 500,
                                  tabPanel("Summary",
                                           withSpinner(plotlyOutput("pie_chart")),
                                           p("Hover over segments to see raw counts.")
                                  ),
                                  tabPanel("Basic Count", withSpinner(plotlyOutput("count_plot"))),
                                  tabPanel("Basic %", withSpinner(plotlyOutput("perc_plot"))),
                                  tabPanel("% Monoexonic", withSpinner(plotlyOutput("mono_plot"))),
                                  tabPanel("% Artifacts", withSpinner(plotlyOutput("arti_plot"))),
                                  tabPanel("% Novel Transcipts", withSpinner(plotlyOutput("novel_trans_plot"))),
                                  tabPanel("% Novel Genes", withSpinner(plotlyOutput("novel_genes_plot"))),
                                  tabPanel("Gene Expression", withSpinner(plotOutput("gene_expression")))
                           )
                    )
                  )
          ),
          tabItem(tabName="browser",
                  fluidRow(
                    box(
                      width=6,
                      collapsible = TRUE,
                      title="Choose Datasets to Browser",
                      uiOutput("primaryDataset"),
                      uiOutput("secondaryDataset")
                    ),
                    box(
                      width=6,
                      collapsible = TRUE,
                      title="Selected Datasets",
                      uiOutput("selectedBrowserData")
                    )
                  ),
                  fluidRow(
                    id="load_genome_msg",
                    box(
                      width=12,
                      p("Please select a sample.")
                    )
                  ),
                  fluidRow(
                    id="error_igv_msg",
                    style="display:none",
                    box(
                      width=12,
                      p("No data for IGV. Make sure your filters don't remove all data.")
                    )
                  ),
                  fluidRow(
                    id="igv_loading",
                    style="display:none",
                    box(
                      width=12,
                      withSpinner(uiOutput("spinnerDummyID1"))
                    )
                  ),
                  fluidRow(
                    id="igv",
                    style="visibility:hidden; overflow:hidden",
                    box(
                      width=12,
                      height="700px",
                      style="overflow-y: scroll; height:650px",
                      title="Genome Browser",
                      withSpinner(igvShinyOutput("igv"))
                    )
                  ),
          )
        ),
      )
    )
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'SQANTIExplorer'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

