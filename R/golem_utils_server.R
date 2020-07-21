require(ggplot2)
require(rtracklayer)
require(dplyr)
require(purrr)
require(magrittr)
require(tidyr)
require(stringr)
require(tibble)
require(readr)
require(ggthemr)

#' Inverted versions of in, is.null and is.na
#' 
#' @noRd
#' 
#' @examples
#' 1 %not_in% 1:10
#' not_null(NULL)
`%not_in%` <- Negate(`%in%`)

not_null <- Negate(is.null)

not_na <- Negate(is.na)

#' Removes the null from a vector
#' 
#' @noRd
#' 
#' @example 
#' drop_nulls(list(1, NULL, 2))
drop_nulls <- function(x){
  x[!sapply(x, is.null)]
}

#' If x is `NULL`, return y, otherwise return x
#' 
#' @param x,y Two elements to test, one potentially `NULL`
#' 
#' @noRd
#' 
#' @examples
#' NULL %||% 1
"%||%" <- function(x, y){
  if (is.null(x)) {
    y
  } else {
    x
  }
}

#' If x is `NA`, return y, otherwise return x
#' 
#' @param x,y Two elements to test, one potentially `NA`
#' 
#' @noRd
#' 
#' @examples
#' NA %||% 1
"%|NA|%" <- function(x, y){
  if (is.na(x)) {
    y
  } else {
    x
  }
}

#' Typing reactiveValues is too long
#' 
#' @inheritParams reactiveValues
#' @inheritParams reactiveValuesToList
#' 
#' @noRd
rv <- shiny::reactiveValues
rvtl <- shiny::reactiveValuesToList

# Write isoform files to download
write_isoforms <- function(data, filename, type = "gtf", cut=FALSE) {
  gtf_file <- data %>% ungroup() %>% distinct(gtf_path)
  # Ensure data contains only one file
  if (gtf_file %>% count() %>% dplyr::first() > 1) {
    print("Has more than one GTF file in input")
    return()
  }
  orig_transcripts <- rtracklayer::import.gff2(gtf_file %>% dplyr::first())
  transcripts_in_data <- NULL
  if(cut) {
    transcripts_in_data <-  data %>% ungroup() %>% dplyr::slice(0:1000) %>% pull(isoform)
  }
  else {
    transcripts_in_data <- data %>% ungroup() %>% pull(isoform)
  }
  filtered_gtf <- subset(orig_transcripts, transcript_id %in% transcripts_in_data)
  if(type=="gtf"){
    return(rtracklayer::export.gff2(filtered_gtf, filename))   
  }
  if(type=="gff"){
    return(rtracklayer::export.gff3(filtered_gtf, filename))   
  }
  if(type=="bed"){
    return(rtracklayer::export.bed(filtered_gtf, filename))  
  }
}

# Write classification files to download
write_classification <- function(data, filename) {
  write.csv(
    data %>% select(-file, -path, -gtf_path, -genome),
    filename, row.names = FALSE
  )
}

# GGPlot helpers
geom_perc_y <- scale_y_continuous(labels = scales::percent_format(), name="Percentage (%)")
geom_label_stacked = geom_text(size = 3, position = position_stack(vjust = 0.5))
geom_label_dodge = geom_text(size=3, position=position_dodge(width=0.9), vjust=1.5)

bar_plot = function(data, input) {
  ggplot(data, aes_string(x="name", y="n", fill=input$groupBy, customdata=input$groupBy)) + geom_bar(position="dodge", stat="identity")
}

validate_classifications <- function(data) {
  msg <- "Please add a classification file."
  if(is.null(data())) {
    return(validate(
      need(data(), msg)
    ))
  }
  return(validate(
    need(data() %>% count() %>% pull("n") > 0, msg),
    need(data(), msg)
  ))
}
