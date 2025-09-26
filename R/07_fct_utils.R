#utility and helper functions

#function to create a download handler for plots
create_plot_download_handler <- function(plot_function, filename_base) {
  downloadHandler(
    filename = function() {
      paste0(filename_base, "_", Sys.Date(), ".png")
    },
    content = function(file) {
      #create the plot
      p <- plot_function()
      
      #save as png
      ggsave(file, plot = p, width = 10, height = 8, dpi = 300)
    }
  )
}

#function to create a download handler for data
create_data_download_handler <- function(data_function, filename_base) {
  downloadHandler(
    filename = function() {
      paste0(filename_base, "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      #get the data
      data <- data_function()
      
      #write to csv
      write.csv(data, file, row.names = FALSE)
    }
  )
}

#function to format numbers for display
format_number <- function(x, digits = 2) {
  if (is.numeric(x)) {
    if (abs(x) < 0.01 && x != 0) {
      return(formatC(x, format = "e", digits = digits))
    } else {
      return(round(x, digits))
    }
  }
  return(x)
}

#function to create session info for reproducibility
create_session_info <- function() {
  info <- sessionInfo()
  
  session_text <- paste(
    "R Version:", info$R.version$version.string, "\n",
    "Platform:", info$platform, "\n",
    "Date:", Sys.Date(), "\n",
    "Time:", format(Sys.time(), "%H:%M:%S"), "\n\n",
    "Loaded Packages:\n"
  )
  
  #add package versions
  for (pkg in names(info$loadedOnly)) {
    pkg_info <- info$loadedOnly[[pkg]]
    session_text <- paste0(session_text, 
                          "  ", pkg, " (", pkg_info$Version, ")\n")
  }
  
  return(session_text)
}

#function to safely read uploaded files
safe_read_upload <- function(file_path, file_type = "auto") {
  tryCatch({
    if (file_type == "auto") {
      #detect file type from extension
      ext <- tolower(tools::file_ext(file_path))
      file_type <- switch(ext,
                         csv = "csv",
                         tsv = "tsv",
                         txt = "tsv",
                         xlsx = "excel",
                         xls = "excel",
                         "csv")  #default
    }
    
    #read based on type
    data <- switch(file_type,
                  csv = read.csv(file_path, stringsAsFactors = FALSE),
                  tsv = read.table(file_path, sep = "\t", header = TRUE, 
                                  stringsAsFactors = FALSE),
                  excel = readxl::read_excel(file_path),
                  read.csv(file_path, stringsAsFactors = FALSE))
    
    return(list(success = TRUE, data = data, error = NULL))
    
  }, error = function(e) {
    return(list(success = FALSE, data = NULL, error = e$message))
  })
}

#function to create a waiter loading screen
create_waiter <- function(message = "Loading...") {
  waiter <- Waiter$new(
    html = tagList(
      spin_flower(),
      h4(message)
    ),
    color = "#2C3E50"
  )
  return(waiter)
}

#function to validate gene IDs
validate_gene_ids <- function(gene_ids, valid_ids) {
  gene_ids <- trimws(gene_ids)
  gene_ids <- gene_ids[gene_ids != ""]
  
  valid <- gene_ids %in% valid_ids
  
  return(list(
    valid_genes = gene_ids[valid],
    invalid_genes = gene_ids[!valid],
    n_valid = sum(valid),
    n_invalid = sum(!valid),
    percent_valid = round(100 * sum(valid) / length(gene_ids), 1)
  ))
}

#function to create summary statistics table
create_summary_table <- function(data, group_var = NULL) {
  if (is.null(group_var)) {
    #overall summary
    summary_df <- data.frame(
      Variable = colnames(data),
      Mean = sapply(data, mean, na.rm = TRUE),
      SD = sapply(data, sd, na.rm = TRUE),
      Min = sapply(data, min, na.rm = TRUE),
      Max = sapply(data, max, na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  } else {
    #grouped summary
    summary_df <- data %>%
      group_by(!!sym(group_var)) %>%
      summarise(across(where(is.numeric), 
                      list(mean = ~mean(., na.rm = TRUE),
                           sd = ~sd(., na.rm = TRUE),
                           min = ~min(., na.rm = TRUE),
                           max = ~max(., na.rm = TRUE))))
  }
  
  return(summary_df)
}

#function to handle missing data
handle_missing_data <- function(data, method = "remove", fill_value = 0) {
  if (method == "remove") {
    #remove rows with any NA
    return(na.omit(data))
  } else if (method == "fill") {
    #fill with specific value
    data[is.na(data)] <- fill_value
    return(data)
  } else if (method == "interpolate") {
    #linear interpolation for numeric columns
    for (col in colnames(data)) {
      if (is.numeric(data[[col]])) {
        data[[col]] <- approx(data[[col]], xout = seq_along(data[[col]]))$y
      }
    }
    return(data)
  }
  
  return(data)
}