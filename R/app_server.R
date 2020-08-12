#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import readr
#' @import purrr
#' @import tibble
#' @import fs
#' @import stringr
#' @import magrittr
#' @import tidyr
#' @import dplyr
#' @import rtracklayer
#' @import shinycssloaders
#' @import ggplot2
#' @import ggthemr
#' @noRd
app_server <- function( input, output, session ) {
  # Temporary data storage per session
  # ===================================
  base_tmp <- "tmp"
  resource_path <- paste0(getwd(), "/inst/app/www/", base_tmp)
  shiny::addResourcePath(base_tmp, resource_path)
  session_hard_tmp <- paste0(resource_path, "/", session$token, "/") # Where tmp data is actually stored on disk
  session_public_tmp <- paste0(base_tmp, "/", session$token, "/") # The public URL to the tmp data

  if(!dir.exists(resource_path)) {
    dir.create(resource_path) 
  }
  dir.create(session_hard_tmp)
  
  session$onSessionEnded(function() {
    # Remove on session end
    unlink(session_hard_tmp, recursive=TRUE, force=TRUE)
  })

  # THEMING
  # ======
  color_primary <- "#EF543B"
  color_primary_dark <- "#B03E2C"
  color_primary_light <- "#FF9170"
  color_secondary <- "#2470F0"
  color_secondary_dark <- "#1A51B0" 
  color_secondary_light <- "#5891FF"
  color_alert <- "#F0DD54" 
  
  # ggplot
  ggthemr(
    define_palette(
      swatch = structure(
        c(
          
          color_primary_light, color_secondary, 
          color_primary, color_primary_dark,
          color_secondary_light, color_secondary_dark
        )
      ),
      gradient = c(lower=color_primary_light, upper=color_primary_dark),
      background = '#ecf0f1'
    )
  )
  options(shiny.maxRequestSize = 300*1024^2, spinner.color="#2470F0")
  
  classifications <- reactiveVal()
  
  observeEvent(input$show_help, {
    showModal(modalDialog(
      size="l",
      includeMarkdown("README.md")
    ))
  })
  
  chain_file <- reactiveVal()
  observeEvent(input$chain_file, {
    chain_file(input$chain_file)
  })
  
  observeEvent(input$addClassification, {
    req(input$classification_file, input$gtf_file, input$name)
    
    if(input$addClassification > 0) {
      isolate({
        datapath <- input$classification_file[["datapath"]]
        new_row <- tibble(
          name = input$name,
          file = input$classification_file[["name"]],
          gtf_path = input$gtf_file[["datapath"]],
          chain_file = ifelse(is.null(chain_file()), "NONE", chain_file()[["datapath"]]),
          genome = input$genome,
          path = datapath,
          classification = list(read_tsv(datapath))) %>%
          unnest(cols=c(classification)) %>%
          mutate(polyexonic = if_else(exons > 1, "Polyexonic", "Monoexonic")) %>%
          mutate(novel_transcript = if_else(associated_transcript == "novel", "Novel", "Annotated")) %>%
          mutate(novel_gene = if_else(grepl("novelGene", associated_gene), "Novel", "Annotated")) %>%
          mutate(log_gene_exp = log(gene_exp + 0.01))
        
        if (is.null(classifications())) {
          classifications(new_row)
        }
        else {
          classifications(classifications() %>% add_row(new_row))
        }
        
        chain_file(NULL)
        golem::invoke_js("reset_form", "data-form")
      })
    }
  })
  
  data_to_plot <- reactive({
    validate_classifications(classifications)
    val <- classifications() %>% group_by_at(c("name", input$groupBy))
    
    if (input$polyexonic) {
      val <- filter(val, exons > 1)
    }
    
    if (input$monoexonic) {
      val <- filter(val, exons == 1)
    }
    
    if(input$noRTS) {
      val <- filter(val, RTS_stage == FALSE)
    }
    
    if(input$noIntraPriming) {
      val <- filter(val, `intra-priming` == FALSE)
    }
    
    if(input$allCanonical) {
      val <- filter(val, all_canonical == "canonical")
    }
    
    if (input$minCovGTZero) {
      val <- filter(val, min_cov > 0)
    }
    
    if (input$onlyGenes) {
      val <- distinct(val, associated_gene, .keep_all = TRUE)
    }
    
    if (input$novelGenes) {
      val <- filter(val, novel_gene == "Novel")
    }
    
    if (input$annotatedGenes) {
      val <- filter(val, novel_gene == "Annotated")
    }
    
    if (input$novelTranscripts) {
      val <- filter(val, novel_transcript == "Novel")
    }
    
    if (input$annotatedTranscripts) {
      val <- filter(val, novel_transcript == "Annotated")
    }
    
    return(val)
  })
  
  selected_data <- reactive({
    d <- event_data(event = "plotly_click")
    if (is.null(d)) return(NULL)
    data_to_plot() %>%
      filter((!!as.symbol(input$groupBy)) == d$customdata) %>%
      filter(name %in% sort(unique(data_to_plot()$name))[d$pointNumber + 1])
  })

  output$inputTable <-  DT::renderDataTable(DT::datatable({
    validate_classifications(classifications)
    classifications() %>% 
      select(name, file) %>% 
      distinct(name, .keep_all = TRUE) %>%
      mutate(delete = purrr::pmap(list(name), ~ as.character(
        actionButton(
          paste(.x), label = NULL, icon = icon('trash'),
          onclick = 'Shiny.setInputValue(\"deletePressed\",  this.id, {priority: "event"})')
        )
      ))
  }, escape=FALSE))
  
  observeEvent(input$deletePressed,{
    classifications(
      classifications() %>% filter(name != input$deletePressed)
    )
  })
  
  output$pie_chart <- renderPlotly({
    validate_classifications(classifications)
    count_times = 0
    
    get_counts <- function(data, name) {
      data <- data %>% ungroup()
      get_n <- function(data) { data %>% count() %>% pull(n)}
      
      # Create a mapping between categories and the method of obtaining data of that category
      categories = tibble(
        name = c("Artifact", "Isoform", "All Canonical", "Non Canonical", "Monoexonic", "Polyexonic", "RTS", "Intra-Priming"),
        func = c(
          function(data) filter(data, SQANTI_filter == "Artifact"), 
          function(data) filter(data, SQANTI_filter == "Isoform"),
          function(data) filter(data, all_canonical == "canonical"),
          function(data) filter(data, all_canonical == "non_canonical"),
          function(data) filter(data, exons == 1),
          function(data) filter(data, exons > 1),
          function(data) filter(data, RTS_stage == TRUE),
          function(data) filter(data, `intra-priming` == TRUE)
        )
      )
      
      combinations <- tibble(expand.grid(categories$name, categories$name)) %>% 
        filter(Var1 == "Artifact" | Var1 == "Isoform") %>%
        filter(!(Var2 == "Artifact" | Var2 == "Isoform"))
      
      df <- tibble(
        ids = c("Name", paste0(unique(combinations$Var1)), paste0(combinations$Var1, "-", combinations$Var2)),
        labels = c(pull(name, name), paste0(unique(combinations$Var1)), paste0(combinations$Var2)),
        parents = c("", rep("Name", length(unique(combinations$Var1))), paste0(combinations$Var1)),
        values = c(
          get_n(data),
          # Map through all categories, get the relevent accessor function, execute it, get_n
          unique(combinations$Var1) %>% map(~ get_n(categories$func[[match(.x, categories$name)]](data))),
          combinations$Var2 %>% map(~ get_n(categories$func[[match(.x, categories$name)]](data)))
        )
      )
      return(df)
    }
    
    fig <- plot_ly()
    group_map(classifications() %>% group_by(name), function(group_df, name) {
      df <- get_counts(group_df, name)
      fig <<- fig %>% add_trace(
        ids = df$ids,
        labels = df$labels,
        values = df$values,
        parents = df$parents,
        type = "sunburst",
        maxdepth = 3,
        customdata = name,
        domain = list(column = count_times)
      )
      count_times <<- count_times + 1
    })
    
    
    fig <- fig %>% layout(grid=list(columns=count_times, rows=1),
                          sunburstcolorway = c(
                            color_primary, color_secondary
                          ),
                          extendsunburstcolors = TRUE)
    
    return(fig)
  })
  
  output$count_plot <- renderPlotly({
    data_to_plot() %>% count() %>%
      ggplot(., aes_string(x="name", y="n", fill=input$groupBy, customdata=input$groupBy)) + geom_bar(position="dodge", stat="identity") +
      xlab("Name") + ylab("N")
  })
  
  output$perc_plot <- renderPlotly({
    data_to_plot() %>% group_by(name) %>% count_(input$groupBy) %>% mutate(perc = n / sum(n)) %>%
      ggplot(., aes_string(x="name", y="perc", fill=input$groupBy, customdata=input$groupBy)) + geom_bar(position="fill", stat="identity") +
      geom_perc_y + xlab("Name")
  })
  
  output$mono_plot <- renderPlotly({
    data_to_plot() %>% count(polyexonic) %>% mutate(perc = n/(sum(n))) %>%  filter(polyexonic == "Monoexonic") %>%
      ggplot(., aes_string(x="name", y="perc", fill=input$groupBy, customdata=input$groupBy)) + geom_bar(position="dodge", stat="identity") +
      geom_perc_y +xlab("Name")
  })
  
  output$arti_plot <- renderPlotly({
    data_to_plot() %>% count(SQANTI_filter) %>% mutate(perc = n / sum(n)) %>% filter(SQANTI_filter == "Artifact") %>%
      ggplot(., aes_string(x="name", y="perc", fill=input$groupBy, customdata=input$groupBy)) + geom_bar(position="dodge", stat="identity") +
      geom_perc_y + xlab("Name")
  })
  
  output$novel_trans_plot <- renderPlotly({
    data_to_plot() %>% count(novel_transcript) %>% mutate(perc = n / sum(n)) %>% filter(novel_transcript == "Novel") %>%
      ggplot(., aes_string(x="name", y="perc", fill=input$groupBy, customdata=input$groupBy)) + geom_bar(position="dodge", stat="identity") +
      geom_perc_y + xlab("Name")
  })
  
  output$novel_genes_plot <- renderPlotly({
    data_to_plot() %>% count(novel_gene) %>% mutate(perc = n / sum(n)) %>% filter(novel_gene == "Novel") %>%
      ggplot(., aes_string(x="name", y="perc", fill=input$groupBy, customdata=input$groupBy)) + geom_bar(position="dodge", stat="identity") +
      geom_perc_y + xlab("Name")
  })
  
  output$gene_expression <- renderPlot({
    data_to_plot() %>% distinct(associated_gene, .keep_all=TRUE) %>%
      ggplot(., aes_string(x="name", y="log_gene_exp", fill=input$groupBy, customdata=input$groupBy)) +
      geom_violin(position=position_dodge(0.9), stat="ydensity", trim = TRUE) +
      geom_boxplot(width = 0.15, position=position_dodge(0.9), outlier.shape=NA, ) +
      xlab("Species") + ylab("Log(TPM)")
  })
  
  output$selected_transcript_count <- renderValueBox({
    text <- "0"
    if (!is.null(selected_data())) {
      text <- paste(selected_data() %>% ungroup() %>% count() %>% dplyr::first())
    }
    valueBox(
      text, "Selected Transcripts", icon=icon("calculator"), color="yellow"
    )
  })
  
  # Genome Browser functionality
  # ============================
  output$primaryDataset <- renderUI({
    validate_classifications(classifications)
    div(
      selectInput("primary_dataset_choice", "Select Data To Visualize: ", choices = classifications() %>% distinct(name) %>% pull(name)),
      actionButton("select_primary_dataset", "Select Dataset")
    )
  })
  
  genome_name <- reactiveVal()
  datasets_to_browse <- reactiveVal(c())
  
  output$secondaryDataset <- renderUI({
    req(datasets_to_browse(), genome_name())
    available_datasets <- classifications() %>% 
      filter(
        genome == genome_name(), !name %in% datasets_to_browse()
      ) %>% 
      distinct(name) %>% pull(name)

    if(length(available_datasets) == 0) {
      return(div())
    }
    
    div(
      hr(),
      selectInput(
        "secondary_dataset_choice",
        "Add Another Track: ", 
        choices = available_datasets
      ),
      actionButton("add_secondary_dataset", "Add Dataset", icon=icon("add"))
    )
  })

  output$selected_browser_data_table <- renderTable(
    tibble(Name = datasets_to_browse()), width="100%"
  )
  
  output$selectedBrowserData <- renderUI({
    validate(need(datasets_to_browse(), "Please select data to view."))
    span(
      tableOutput("selected_browser_data_table"),
      actionButton("clear_datasets", "Clear", icon=icon("trash")),
      actionButton("render_igv", "Load Selected Data", style="color: #fff; background-color: #2470F0; border-color: #2e6da4") 
    )
  })
  
  observeEvent(input$select_primary_dataset, {
    if(input$select_primary_dataset > 0) {
      isolate({
        genome_name(data_to_plot() %>% ungroup() %>% filter(name == input$primary_dataset_choice) %>% pull(genome) %>% dplyr::first())
        datasets_to_browse(input$primary_dataset_choice)
      })
    }
  })
  
  observeEvent(input$add_secondary_dataset, {
    req(datasets_to_browse())
    
    if(input$add_secondary_dataset > 0) {
      isolate({
        datasets_to_browse(c(
          datasets_to_browse(),
          input$secondary_dataset_choice
        ))
      })   
    }
  })
  
  observeEvent(input$clear_datasets, {
    if(input$clear_datasets > 0) {
      datasets_to_browse(c())
      golem::invoke_js("make_invisible", "igv")
    }
  })
  
  output$igv <- renderIgvShiny({
     igvShiny(list(
       genomeName="hg38",
       initialLocus="all"
     ))
  })
  
  refresh_igv <- function(region) {
    genome_browser_data <- data_to_plot() %>% 
      ungroup() %>%
      filter(name %in% datasets_to_browse())
    
    if(nrow(genome_browser_data) == 0) {
      golem::invoke_js("showid", "error_igv_msg")
      golem::invoke_js("make_invisible", "igv")
    }
    else {
      golem::invoke_js("hideid", "error_igv_msg")
      golem::invoke_js("make_invisible", "igv")
      golem::invoke_js("showid", "igv_loading")
      
      genome_browser_data %>% group_by(name) %>% group_map(function(group_df, name) {
        tmp_filename <- paste0(session_public_tmp, name, ".gff")
        
        write_isoforms(
          group_df, paste0(session_hard_tmp, name, ".gff"), "gff", cut=TRUE
        )
        
        igvShiny::loadGffTrackUrl(
          session,
          trackName = name$name,
          url = tmp_filename,
          deleteTracksOfSameName=TRUE,
          color = color_secondary)
      })
      
      igvShiny::showGenomicRegion(session, region)
      golem::invoke_js("make_visible", "igv")
      golem::invoke_js("hideid", "igv_loading")
    }
  }
  
  observeEvent(input$render_igv, {
    if(input$render_igv > 0) {
      igvShiny::loadGenome(session, list(genomeName = genome_name()))
      refresh_igv("all")
    }
  })
  
  output$updateButton <- renderUI({
    if(input$tabs == "browser" && length(datasets_to_browse()) > 0) {
      return(
        tagList(
          hr(),
          actionButton("refreshIgv", "Update Genome Browser", icon= icon("sync"))
        )
      )
    }
    
    hr()
  })
  
  observeEvent(input$refreshIgv, {
    if(input$refreshIgv > 0) {
      isolate({
        igvShiny::getGenomicRegion(session)
        refresh_igv(input$currentGenomicRegion)
      })   
    }
  })
  
  output$downloadData <- downloadHandler(
    filename = "SQANTI_explorer.csv",
    content = function(file) {
      write_classification(classifications(), file)
    }
  )
  
  output$downloadFilteredData <- downloadHandler(
    filename = "SQANTI_explorer_filtered.csv",
    content = function(file) {
      write_classification(data_to_plot(), file)
    }
  )
  
  output$downloadSelectedData <- downloadHandler(
    filename = "SQANTI_explorer_selected.csv",
    content = function(file) {
      write_classification(selected_data(), file)
    }
  )
  
  output$downloadSelectedGTF <- downloadHandler(
    filename = "SQANTI_explorer_selected.gtf",
    content = function(file) {
      write_isoforms(selected_data(), file)
    }
  )
  
  output$downloadFilteredGTF <- downloadHandler(
    filename = "SQANTI_explorer_filtered_gtf.zip",
    content = function(file){
      #go to a temp dir to avoid permission issues
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      files <- data_to_plot() %>% ungroup() %>% distinct(name) %>% pull(name) %>%
        walk(function(row) {
          write_isoforms(data_to_plot() %>% filter(name == row), paste0(row, ".gtf") )
        }) %>%
        map_chr(function(row) {
          paste0(row, ".gtf") 
        })
      
      #create the zip file
      zip(file, files)
    }
  )
  
  output$downloadSelectedBED <- downloadHandler(
    filename = "SQANTI_explorer_selected.bed",
    content = function(file) {
      write_isoforms(selected_data(), file)
    }
  )
}
