#configuration and library loading

#set cran here 
r <- getOption("repos")
r["CRAN"] <- "https://cloud.r-project.org"
options(repos = r)

#load the dependencies 
suppressMessages({
  #core libraries
  library(tidyverse)
  library(data.table)
  library(cowplot)
  
  #shiny-related
  library(shiny)
  library(bslib)
  library(waiter)
  library(shinyjs)
  library(plotly)
  library(DT)
  library(fontawesome)
  library(shinyBS)
  library(viridis)
  library(ggridges)
  library(ggtree)
  library(ape)
  library(treeio)
  library(data.table)
  library(RColorBrewer)
})

#load the new RData file with HOG-based orthogroups
load("../data/RData_perSpecies_HOG_clean_08292025_gene_lookup_tree.RData")

if (!is.null(all_species_data$gene_lookup)) {
  if (!is.data.table(all_species_data$gene_lookup)) {
    all_species_data$gene_lookup <- as.data.table(all_species_data$gene_lookup)
  }
  #keys 4 faster searching
  setkey(all_species_data$gene_lookup, gene_id, gene_name, species)
}