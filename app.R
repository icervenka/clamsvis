# CLAMS-VIS app
# TODO implement non agregated measurements
# TODO if separator is decimal point, it doesn't work

library(shiny)
library(pryr)
library(dplyr)
library(reshape2)
library(stringr)
library(readr)
library(xlsx)
library(readxl)
library(chron)
library(lubridate)
library(ggplot2)


# ************************************************
# constants
# ************************************************

# map of parameter names to text outputs in shiny UI selection box
nameParameterMap <- as.data.frame(matrix( data = c( 
  c("X_Ambulatory", "X ambulatory movement"),
  c("Volume_CO2", "CO2 volume"),
  c("Z_Total", "Z total movement"),
  c("RER", "Resting energy rate"),
  c("Feed_Weight_1", "Food weight"),
  c("Y_Total", "Y total movement"),
  c("Y_Ambulatory", "Y ambulatory movement"),
  c("X_Total", "X total movement"),
  c("Heat", "Heat"),
  c("Volume_O2", "O2 volume"),
  c("Drink_Weight_1", "Drink volume"),
  c("XY_Ambulatory", "XY ambulatory movement"),
  c("X_Non_Ambulatory", "X non-ambulatory movement"),
  c("Y_Non_Ambulatory", "Y non-ambulatory movement")
  ), ncol = 2, byrow = TRUE
), stringsAsFactors = FALSE)

colnames(nameParameterMap) <- c("parameter", "name")
coltypes <-  c("text", "numeric", "text", "text", "numeric", rep("numeric", times = 22), "text")

# vector indicating paramters which should be summarized by sum instead of mean
agg_sum_vector <- c("X_Ambulatory", "Y_Ambulatory", "XY_Ambulatory", "X_Total", "Y_Total", "Z_Total", "X_Non_Ambulatory", "Y_Non_Ambulatory")

# text constants to identify startof Fasting and Refeeding  periods
fasting <- "Fasting"
refeeding <- "Refeeding"

# indices of columns which contain descriptor data
descriptor_column_index <- c(2, 3, 4, 27)

# indices of columns which contain measurement data
data_column_index <- c(5, 10, 15:16, 18:26)

# color palette for graphs
cbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
palette <- list(scale_linetype_manual(values = c(rep("solid", 8), rep("dashed", 8), rep("twodash", 8))),
                scale_color_manual(values = c(rep(cbPalette, 3))))

# standard height of graph [px]
graph_height = 650

#common options for graph appearance
graph_options <- list(xlab(""), ylab(""), geom_line(size=1.05), theme(legend.text=element_text(size=16), legend.key.size = unit(0.7, "cm"), axis.text = element_text(size = 12)))

night_duration = 12

# ************************************************
# function declaration
# ************************************************

# adds id and time columns to data frame converted from zoo object
# Arguments: data frame with dates as rownames
# Value: data frame with added id and time columns
add_id_time <- function(df) {
  df$id <- 1:dim(df)[1]
  #df$time <- rownames(df)
  return(df)
}

# function adds item to the first empty position of character vector
add_to_char_vector <- function(vec, item) {
  index <- which(vec == "")[1]
  stopifnot(index <= length(vec))
  vec[index] <- item
  return(vec)
}

# create intervals for different time aggregations
agg_time <- function(x, init_crop, intervals, records_count) {
  c(rep(1, each = (init_crop[x] - 1)), rep(2:(records_count/intervals[x]+2), each = intervals[x]))[1:records_count]
}

#arrange variables based on supplied order (named vector)
arrange.vars <- function(data, vars){
  ##stop if not a data.frame (but should work for matrices as well)
  stopifnot(is.data.frame(data))
  
  ##sort out inputs
  data.nms <- names(data)
  var.nr <- length(data.nms)
  var.nms <- names(vars)
  var.pos <- vars
  ##sanity checks
  stopifnot( !any(duplicated(var.nms)), 
             !any(duplicated(var.pos)) )
  stopifnot( is.character(var.nms), 
             is.numeric(var.pos) )
  stopifnot( all(var.nms %in% data.nms) )
  stopifnot( all(var.pos > 0), 
             all(var.pos <= var.nr) )
  
  ##prepare output
  out.vec <- character(var.nr)
  out.vec[var.pos] <- var.nms
  out.vec[-var.pos] <- data.nms[ !(data.nms %in% var.nms) ]
  stopifnot( length(out.vec)==var.nr )
  
  ##re-arrange vars by position
  data <- data[ , out.vec]
  return(data)
}

# function takes a numeric vector and returns vector of same size with signs of number reversed
change_sign <- function(vector) {
  vector - (vector * 2)
}

# Function generates series of x coordinates that can be displayed as Dark rectangles on graph
# Arguments: number of phases before first night, amount of intervals, number of Dark phases
# Value: data frame of start-end values for dark phases
dark_plot_rect <- function(initial_phase_no, len, dark_intervals) {
  st <- seq(from = initial_phase_no, to = len, by = 2*dark_intervals) - 0.5
  en <- seq(from = initial_phase_no + dark_intervals, to = len, by = 2*dark_intervals) - 0.5
  return(data.frame(start = st, end = en))
}

# Arguments: takes data frame of x coordinates to generate Dark/Night retangles
# Value: gglplot aes with list of rectangles to plot
dark_rect_aes <- function(rects) {
  return(annotate("rect", xmin = rects$start, xmax = rects$end, ymin = -Inf, ymax = Inf, alpha=0.04, fill='blue'))
}

# vectorized function that takes list of chron time objects, and return a list of Light/Dark strings
# based in night start and night duration parameters
date_to_phase_arr <- function(x, night_start = night_start, night_duration = night_duration) {
  x <- chron::hours(x)
  night_end <- (night_start + night_duration) %% 24
  return(ifelse(x >= night_start | x < night_end, "Dark", "Light"))
}

# function accepts an integer and returns a list of factors (divisors)
factors <- function(x) {
  x <- as.integer(x)
  div <- seq_len(abs(x))
  factors <- div[x %% div == 0L]
  return(factors)
}

# function takes a list and returns number of occurences of first element until it changes
first_n_ocurrence <- function(list) {
  return(grep(list[1], list, invert = TRUE)[1] - 1)
}


gen_agg_parameter <- function(dat, func, phase_vec, int_df) {
  # create empty list where time aggregations will be stored
  agg <- vector("list", length(names(int_df)))  
  
  for(i in seq_along(names(int_df))) {
    agg[[i]] <- aggregate(dat, by = int_df[i], func)
    agg[[i]] <- data.frame(aggregate(phase_vec, int_df[i], first)[2], agg[[i]])
    names(agg[[i]])[1] <- "phase"
  }
  
  return(agg)
  
}

# generates hour-by-hour aggregations for supplied measured parameter
# ignores data from fasting-refeeding periods
gen_hour_agg <- function(data, fasting_index, initial_start) {
  return(as.data.frame(t(sapply(0:23, function(x) {colMeans(filter(data, hour == x, row_number() < fasting_index & row_number() >= initial_start))}))))
}


gen_results <- function(dat, agg_function_vec, ...) {
  results <- list()
  names <- names(dat)
  funcs <- lapply(names, function(x) {if(x %in% agg_function_vec) {return(sum)} else {return(mean)}})
  results <- mapply(gen_agg_parameter, dat, funcs, MoreArgs = list(...), SIMPLIFY = FALSE)
  return(results)
}

# generates summary data frame by averaging selected groups of columns
group_data <- function(plot_data, columns) {
  df <- do.call(cbind, lapply(1:length(columns), function(x) {
    data.frame(rowMeans(select(plot_data, matches(columns[x])
    )))
  }))
  return(df)
}

# returns group names with incremented numbers
name_groups <- function(len) {
  return(vapply(1:len, function (x) {paste("Group ", x)}, character(1)))
}

# paste array of character objects together without space
paste0_array_text <- function(array, text) {
  return(paste0(array, text))
}

# sanitizing function that removes spaces, dashes and dots from string
sanitize_header_string <- function(string) {
  vapply(string, gsub, pattern = " ", character(1), replace = "_") %>% 
    vapply(gsub, pattern = "-", character(1), replace = "_") %>%
    vapply(gsub, pattern = "\\.", character(1), replace = "") %>%
    return()
}

# creates regular expression pattern to exactly match items in supplied list of strings
# used to select colums/subjects from data for analysis of means
str_group_sel <- function(lst) {
  lst <- lst[lst != ""]
  temp <- rapply(lapply(lst, strsplit, ","), str_trim, how = "list") %>%
    lapply(unlist)
  temp <- Map(function(x) {paste("^(", paste(x , collapse = "|"), ")$", sep = "")}, temp) %>%
    unlist()
  return(temp)
}

# ************************************************
# shiny server UI
# ************************************************

ui <- shinyUI(fluidPage(
  #includeScript("functions.js"),
  
  # Application title
  titlePanel("CLAMS-VIS"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      
      fileInput("file1", "Choose file to upload (xls, xlsx)", accept = c(".xls", ".xlsx", ".csv")),
      
      # conditional panel to supply groups for mean calculations
      conditionalPanel(
        condition = "input.tabs1 == 'Means'",
        tags$hr(),
        numericInput("no_groups", "Choose the number of groups", value = 2, min = 1, width = '100%'),
        uiOutput("groupChar")
      ),
      
      textInput("night_start", label = "Night Start", value = "18:00:00"),
      
      tags$hr(),
      
      # parameter selector
      selectInput("parameterSelect", "Select Parameter",
                  sort(nameParameterMap$name)),
      
      # conditional panel - time aggregation selector
      conditionalPanel(
        condition = "input.tabs1 == 'Time' | input.tabs1 == 'Means'",
        selectInput("timeAggregation", "Select time aggregation [hrs]", c(1, 2, 3, 4, 6, 12), selected = 1)
      ),
      
      conditionalPanel(
        condition = "input.tabs1 == 'Time' | input.tabs1 == 'Means'",
        checkboxInput("cumulative", "Show cumulative data")
      ),
      
      tags$hr(),
      
      downloadButton("download", "Download data"),
      
      # width in colums for the whole side panel
      width = 2
      
    ),
    
    mainPanel(
      tags$head(tags$script(src="functions.js")),
      div(
        
          tabsetPanel(id = "tabs1",
                    tabPanel("Time", plotOutput("timePlot")),
                    tabPanel("Means", plotOutput("meanPlot")),
                    tabPanel("Hour", plotOutput("hourPlot"))
          ), class="delParentClass"
      )
    )
  )
))

# ************************************************
# shiny server logic
# ************************************************

server <- shinyServer(function(input, output) {
  
  # dynamically render textInputs for selections of groups for mean calculations
  output$groupChar <- renderUI({
    lapply(1:as.integer(input$no_groups), function(i) {
      isolate(textInput(paste0("n_input_", i), label = paste0("Group: ", i), value = input[[paste0("n_input_", as.character(i))]]))
    })
  })
  
  #globally accessible reactive values
  globalValues <- reactiveValues()
  
  # main part for parsing and computing data from file
  resultsInput <- reactive({
    
    file <- input$file1
    ext <- tools::file_ext(file)[1]
    
    # if file is not uploaded provide temp file to show results
    if(is.null(file)) {
      file <- read_excel("temp.xlsx")
    } else if(toupper(ext) == "CSV" | toupper(ext) == "TXT") {
      file.rename(file$datapath,
                  paste(file$datapath, ext, sep="."))
      file <- read_csv(paste(file$datapath, ext, sep="."))
    } else {
      # needs to extract extension from file and the rename
      file.rename(file$datapath,
                  paste(file$datapath, ext, sep="."))
      file <- read_excel(paste(file$datapath, ext, sep="."))
    }
    
    # add night start input
    # this is only used to prettify time data, has no effect on Dark/Light phase
    globalValues$night_start = input$night_start
    
    # sanitize headers for easier processing
    names(file) <- sanitize_header_string(names(file))
    file$Subject <- sanitize_header_string(file$Subject)
    
    #change NA values in Event log for empty strings
    file$Event_Log[which(is.na(file$Event_Log))] <- ""
    
    # create XY Ambulatory movement parameter as a composite of X and Y in Euclidean metrics
    # add new column to data column indices
    file <- mutate(file, XY_Ambulatory = sqrt((X_Ambulatory ** 2) + (Y_Ambulatory ** 2))) %>%
      mutate(X_Non_Ambulatory = X_Total - X_Ambulatory) %>%
      mutate(Y_Non_Ambulatory = Y_Total - Y_Ambulatory)

    # update data column indices
    data_column_index <- c(data_column_index, 28:30)
  
    # store column names and subject names in vectors
    header <- names(file)
    subs <- unique(file$Subject)
    
    # create lexicographic ordering for subjects
    # create named vector for ordering to supply to arrange.vars function
    subject_order <- 1:length(subs)
    names(subject_order) <- sort(subs)
    
    # variable expressing offset in columns of another aggregation of time series when exporting to excel file
    globalValues$exp_column_offset <- length(subject_order) + 3
    
    #split file into list of individual subjects
    subjects <- split(file, file$Subject)
    num_records <- sapply(subjects, nrow)
    
    # compute number of measurements in the initial Light/Dark phase for individual subjects
    light_dark <-
      sapply(sapply(subjects, function (x) {
        select(x, 4)
      }), first_n_ocurrence)
    
    # create vectors for removing initial and end elements from time series that don't have corresponding values in all subjects
    light_diff_start <- change_sign(light_dark - min(light_dark))
    light_diff_end <- change_sign((num_records + light_diff_start) - min(num_records + light_diff_start))

    # remove non-corresponding measurements
    for (i in 1:length(subjects)) {
      if (light_diff_start[[i]] != 0) {
        subjects[[i]] <- tail(subjects[[i]], light_diff_start[[i]])
      }
      if (light_diff_end[[i]] != 0) {
        subjects[[i]] <- head(subjects[[i]], light_diff_end[[i]])
      }
    }
    
    # create descriptor object
    time <- subjects[[1]][descriptor_column_index]
    
    # parse date time information to descriptor object
    date_time <- as.POSIXct(time$`Date/Time`, format = "%m/%d/%Y %I:%M:%S %p")
    
    dttm <- str_split_fixed(time$`Date/Time`, pattern = " ", n = 2)
    dts <- chron(dates. = dates(dttm[,1]))
    tms <- chron(times. = times(format(strptime(dttm[,2], "%I:%M:%S %p"), format = '%H:%M:%S')))
    
    first_phase_change_dt <- tms[min(light_dark) + 1 ]
    # phase_change_time <- paste(night_start, "00", "00", sep = ":")
    phase_change_time = globalValues$night_start
    
    # calculate prettified date-time object for first phase change
    tms <- tms - as.character(first_phase_change_dt - phase_change_time)
    
    dts <- chron(dts, out.format = "%m/%d/%Y") #"%d/%m/%Y %H:%M:%S"
    date_time <- chron(dts, tms)
    date_time <- format(date_time, "%d/%m/%Y %H:%M:%S", enclosed = c("", ""))
    
    # calculate time interval in minutes
    #time_interval_min <- as.numeric(difftime(date_time[2], date_time[1]))
    time_interval_min <- round(as.numeric(tms[2] - tms[1])*24*60)
    
    
    # calculate aggregator times in hours by taking common factors of day and night durations
    globalValues$com_fact <- Reduce(intersect, list(factors(night_duration), factors(24), factors(24-night_duration)))
   
    # vector: amount of measurement intervals per specific time aggregation
    intervals = globalValues$com_fact * 60 / time_interval_min
    
    # vector: number of initial phases before first phase change for specific time aggregations
    init_crop <- (min(light_dark) %% intervals) + 1
    
    # initial phase
    init_phase <- min(light_dark)
    
    # global value containing amount of records
    globalValues$records_count <- dim(subjects[[1]])[1]
    
    # add columns with id (index) of interval
    time$Interval <- c(1:globalValues$records_count)
    
    # substitute column with prettified time 
    time$`Date/Time` <- date_time
    
    # add columns containg hour integer
    time$hour <- as.numeric(chron::hours(tms))
    
    # identify interval when fasting starts, otherwise take the last interval
    if(length(grep(fasting, time$Event_Log)) > 0) {
      fasting_index <- grep(fasting, time$Event_Log)
    } else {
      fasting_index <- globalValues$records_count
    }
    
    # compute the refeeding index
    refeeding_index <- grep(refeeding, time$Event_Log)
    
    # transpose data in parameter first format to be used by aggregator script
    dat_frame <- list()
    
    
    for(i in 1:length(data_column_index)) {
      dat_frame[[header[data_column_index[i]]]] <- do.call(cbind.data.frame, lapply(subjects, function (x) {select(x, data_column_index[i])}))
      names(dat_frame[[i]]) <- names(subject_order)
    }
    
    # create data frame with columns corresponding to vectors with aggregation intervals
    int_df <- data.frame(lapply(1:length(intervals), agg_time, init_crop, intervals,  globalValues$records_count))
    
    # prettify names for interval aggregation columns as: 'I#'
    names_int_df <- vapply(c(1:length(intervals)), function(x) {paste0("I", x)}, character(1))
    names(int_df) <- names_int_df
    
    
    # ************************************************
    # generate results
    # ************************************************
    
    # generate results data frame
    # input: data, vector with parameters that are summed, vector with datetimes, data frame containing aggregation intervals
    results <- gen_results(dat_frame, agg_sum_vector, time$`Light/Dark`, int_df)
    
    # generate hour-by-hour aggregation
    dat_hour <- lapply(dat_frame, cbind.data.frame, hour = time$hour)
    
    # assing hour-by-hour aggregation to globally visible variable
    globalValues$dat_hour_mean <- lapply(dat_hour, gen_hour_agg, fasting_index, initial_start = 1)
    
    return(results)
    
  })
  
  
  # render download button and parse data for downloading
  output$download <- downloadHandler(
    
    filename = function() { 
      paste0(format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), "_clams", ".xlsx") # file name
    },
    
    content = function(file){
      results <- resultsInput()
      
      clams_wb <- createWorkbook()
      
      # put data into individual sheets with one parameter per sheet and time aggregations side by side
      for (i in seq_along(names(results))) {
        sheet <- createSheet(clams_wb, sheetName = names(results)[i])
        for(j in seq_along(results[[i]])) {
          # displace next time aggregation by offset equal to number of subjects + date + empty column
          # data starts on row 2
          addDataFrame(results[[i]][[j]][,-2], sheet, startRow = 2, startCol = (globalValues$exp_column_offset*j - (globalValues$exp_column_offset-1)))
        }
        
        row <- createRow(sheet = sheet, rowIndex = 1) # create empty header row to display time aggregations
        vec <- (globalValues$exp_column_offset*c(1:length(globalValues$com_fact)) - (globalValues$exp_column_offset-1)) # vector of displacements
        cells <- createCell(row, colIndex=vec) # create cells based on displacement vector
        mapply(setCellValue, cells, paste0_array_text(globalValues$com_fact, " hour aggregation")) # fill with corresponding title of time aggregation
        
      }
      saveWorkbook(clams_wb, file)
    }
    
  )
  
  # 1. time plot
  #-----------------------------------------------------------------------------------
  # render time series plot for all subjects
  output$timePlot <- renderPlot({
    results <- resultsInput()
    

    results_cum <- lapply(results, function (x) {lapply(x, function (y) {cbind.data.frame(y[1:2], cumsum(y[3:length(colnames(y))]))})})
    
    # index that selects paramter based on selection from UI
    indd_i <- nameParameterMap[which(nameParameterMap$name == input$parameterSelect), 1]
    
    # index that selects respective time aggregation based on UI
    indd_j <- match(as.numeric(input$timeAggregation), globalValues$com_fact)

    # create data frame based on selected indices
    # display normal time course or cumulative sums if the cumulative tickbox is selected
    if(input$cumulative == FALSE) {
      plot_data <- data.frame(results[[indd_i]][[indd_j]], check.names = FALSE)
    } else {
      plot_data <- data.frame(results_cum[[indd_i]][[indd_j]], check.names = FALSE)
    }
    
    
    # add ID and TIME columns and convert to data frame
    phase <- plot_data[1]
    plot_data[1] <- NULL
    names(plot_data)[1] <- "id"
    plot_data_melt <- melt(plot_data, id.vars = c("id"))
    
    # generate rectangle coordinates to display 'Dark/Light' phases in the background of the graph
    rect_start <- first_n_ocurrence(unlist(phase)) + 1
    rects <- dark_plot_rect(rect_start, dim(plot_data)[1], (12/globalValues$com_fact[indd_j]))
    
    # display graph
    ggplot() %+% plot_data_melt + aes(x = id, y = value, group = variable, colour=variable, linetype = variable) + graph_options + palette +  xlab("interval") +
      dark_rect_aes(rects)
    
  }, height = graph_height) # height of the first graph
  
  
  # 2. hour plot
  #-----------------------------------------------------------------------------------
  #render hour-by-hour plot for all subjects
  output$hourPlot <- renderPlot({
    dat_hour_mean <- globalValues$dat_hour_mean
    
    # index that selects paramter based on selection from UI
    indd <- which(nameParameterMap$name == input$parameterSelect)
    
    #display graph
    ggplot() %+% 
      melt(dat_hour_mean[[indd]], id = "hour") +
      aes(x = hour, y = value, color = variable, linetype = variable) +
      palette +
      graph_options + 
      xlab("Hour")
  }, height = graph_height) # height of the second graph
  
  
  # 3. group plot
  #-----------------------------------------------------------------------------------
  # render plot for comparing means
  output$meanPlot <- renderPlot({
    results <- resultsInput()
    
    # index that selects paramter based on selection from UI
    indd_i <- nameParameterMap[which(nameParameterMap$name == input$parameterSelect), 1]
    # index that selects respective time aggregation based on UI
    indd_j <- match(as.numeric(input$timeAggregation), globalValues$com_fact)
    
    # create temporary data frame which contais all the data for selected parameter and time aggregation
    # coercion from zoo object to facilitate the column selection
    plot_data <- data.frame(results[[indd_i]][[indd_j]], check.names = FALSE)
    
    phase <- plot_data[1]
    # number of dynamically generated inputs for means calculation
    num <- as.integer(input$no_groups)
    
    # gather contents of groups from dynamically generated textInputs
    # report as a nested list of subject names
    # adding new input generates NULL - is changed to empty string not to generate errors
    input_list <- lapply(1:num, function(x) {input[[paste0("n_input_", x)]]})
    input_list[sapply(input_list, is.null)] <- list("")
  
    # generate regex expression from nested list, one string per group
    columns <- str_group_sel(input_list)
    
    # amount of groups defined by user
    # if no groups are defined yet, display empty graph to avoid errors
    if(length(columns) == 0) {
      group_count <- 0
    } else {
      group_count <- sum(sapply(1:length(columns), function(x) {dim(select(plot_data, matches(columns[x])))[2]}))
    }

    if(group_count > 0) {
      
      # create data frame with means to display from plot_data
      # data frame contains row means for all groups of observations specified by regex in the columns variable
      plot_df <- group_data(plot_data, columns)
      
      # prettify group names
      names(plot_df) <- name_groups(length(columns))
      
      # add ID and TIME columns and convert to data frame
      plot_df <- add_id_time(plot_df)
      
      # generate rectangle coordinates to display 'Dark/Light' phases in the background of the graph
      rect_start <- first_n_ocurrence(unlist(phase)) + 1
      rects <- dark_plot_rect(rect_start, dim(plot_df)[1], (12/globalValues$com_fact[indd_j]))
      
      plot_df_melt <- melt(plot_df, id.vars = c("id"))
      
      # display graph
      ggplot() %+% plot_df_melt + 
        aes(x = id, y = value, group = variable, color = variable, linetype = variable) +
        dark_rect_aes(rects) + 
        scale_x_continuous(breaks = pretty(1:dim(plot_df)[1], n = 20)) + 
        graph_options + palette + 
        xlab("interval")
    }
    
  }, height = graph_height) # height of the third graph
  
})

# Run the application 
shinyApp(ui = ui, server = server)
