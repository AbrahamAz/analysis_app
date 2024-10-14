library(shiny)
library(dplyr)
library(readxl)
library(scales)
library(stringr)
library(DT)
library(tidyverse)
library(writexl)
library(openxlsx)
library(randomcoloR)
library(sf)
library(anytime)
library(cluster) 
library(survey) 
library(srvyr) 
library(webshot) 
library(docstring) 
library(tcltk)
library(shinycssloaders)
library(knitr)
library(DescTools)
library(corrplot)
library(vcd)
library(RColorBrewer)
library(PerformanceAnalytics)
library(ggplot2)
library(viridis)
library(hrbrthemes)
library(shinybusy)

rm(list=ls())
# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# source("www/src/utils/utils_analysis.R")
source("www/src/utils/misc_utils.R")
source("www/src/utils/kobo_utils.R")
# source("www/src/utils/check_kobo.R")
source("www/src/utils/tabular_analysis_utils.R")
options(shiny.maxRequestSize=30*1024^2)
# Define UI for application that draws a histogram
ui <- fluidPage(
    # Setting up styling
    tags$head(
      HTML('<meta name="viewport content="width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=no"/>'), includeCSS("www/style.css")
    ),
    add_busy_bar(
      height = "5px",
      color = "#EE5759"
    ),
    navbarPage(
      windowTitle = "ANALYSIS TABULAR APP",
      HTML('<a style="padding-left:10px;" class="navbar-brand" href= "https://www.reach-initiative.org/" target="_blank"><img src="reach.jpg" height = "50"></a><span class="navbar-text" style="font-size: 16px; color: #FFFFFF"><strong>ANALYSIS TABULAR APP</strong></span>'),
      tabPanel("Read Me",
               div(class ="title-message",
                   h2("Brief overview of the tool")),
               br(),
               column(class = "column1",
                      style = "margin-left: 220px;",
                      width = 4,
                      br(),
                      h3(class = "title-message","Introduction"),
                      p("This analysis tool will provide various tools and functionalities to analyse and interpret data across your different assessment and tools. It was built in a way to provide a user-friendly interface and a range of tools tailored to specific IMPACT needs."),
                      hr(),
                      h3(class = "title-message","Characteristics"),
                      tags$ul(
                        tags$li(em("Data input:")," The tool allows you to download your finalized/cleaned data and KoBo Tools (XLS format)."),
                        tags$li(em("Analysis Options:")," The tool provides various selection of analysis methods and techniques depending on the desired outcomes. These can include ",strong("single tabular analysis, visualizations, correlations, and key highlight findings.")),
                        tags$li(em("Customization:")," Users will be able to customize their output depending on different analysis parameters and settings according to their specific requirements. Some of these parameters are",strong(" adding weights, strata, and others."))
                      ),
                      hr(),
                      h3(class = "title-message","Parameters"),
                      h4(style = "font-size:20px;",strong("Singular Table Output")),
                      tags$ul(
                        tags$li(em(strong("Data: ")),"Clean dataset."),
                        tags$li(em(strong("Tool: ")),"Surver tool (XLS Format)."),
                        tags$li(em(strong("Weighting: ")),"[Yes] or [No] to include weighting to data."),
                        tags$li(em(strong("Add Total: ")),"[Selected] will add a total row combining all disaggregated values together."),
                        tags$li(em(strong("Include NA: ")),"[Selected] will add all the NA values that are originally omitted."),
                        tags$li(em(strong("Overall: ")),"[Selected] will consider the calculations to the overall population."),
                        tags$li(em(strong("Strata: ")),"[Selected] will consider the calculations by different popluation groups (Strata).")
                      ),
               ),
               column(class = "column2",
                      style = "margin-left: 130px;",
                      width = 4,
                      br(),
                      h3(class = "title-message","Methodology"),
                      h4(style = "font-size:20px;",strong("Singular Table Output")),
                      p("The tool will take as input your cleaned data and the Kobo tool, then it will show you 3 dropdown lists. First, you will be able to select the respective sheet from your dataset (main sheet or other loops). Second, the other dropdown lists are the targeted variable for the analysis from the selected sheet as well as the disaggregated variabled to be added towards the targeted variable."),
                      p("The ",em("variable and disaggregation"),"are mainly the select_one, select_multiple, and different numeric columns from your dataset. In case some of these columns are", strong("empty"),"in your data, it will not be shown in the dropdown list."),
                      p("The output is an interactive table that will be updated automatically depending on your different selections in the Parameters section on the left or even the variable or disagregation."),
                      p("If the selected variable is a select_one or select_multiple, the table will output percentages of the different categories of the selected variable. If the selected variable is numeric, then the table will mainly show the mean, median, min, and max values in the data."),
                      hr(),
                      h3(class = "title-message","Requirements"),
                      h4(style = "font-size:20px;",strong("Singular Table Output")),
                      p("The Data downloaded should be of an Excel format. If the dataset is only including one tab (no loops included in the data), the sheet should be named ", strong('main.'),"If the Data include many tabs (Data with loops), please ensure that the first tab is named ", strong('main,'),"and then the others kept as downloaded from the Kobo Server (as named by the name value of the respective begin_repeat row)."),
                      hr(),
                      p("The tool should include the survey and the choices tab."),
                      hr(),
                      p("For the weighting to be captured and calculated, the data should include a column named ",strong("weight."),"Please make sure that in case data is consiting of loops as well, weight column should also be included in every tab."),
                      hr(),
                      p("For the strata to be captured and calculated, the data should include a column named ", strong("strata."),"Please make sure that in case data is consiting of loops as well, strata column should also be included in every tab."),
                      )
               ),
    tabPanel("Singular Table Output",
             sidebarPanel(
               fileInput("dataInput", "Upload your data here",
                         multiple = F,
                         accept = c(".xlsx")),
               fileInput("toolInput", "Upload your tool here",
                         multiple = F,
                         accept = c(".xlsx")),
               uiOutput("sheetInput"),
               uiOutput("variable"),
               uiOutput("dis_variables"),
               HTML("<h3><strong>Parameters</strong></h3>"),
               radioButtons("weightBTN","Weighting (only if weight is available in Data)",
                            choices = c(Yes = "yes",
                                        No = "no"),
                            selected = "no"),
               uiOutput("weight_input"),
               radioButtons("calculation", "Calculations",
                             choices = c(None = NA,
                                         `Add Total` = "add_total",
                                         `Include NA` = "include_na")),
               radioButtons("admin", "Level (only if strata is available in Data)",
                             choices = c(Overall ="overall",
                                         Strata = "strata"))
             ),
             mainPanel(
               uiOutput("title_table"),
               uiOutput("subtitle_table"),
               uiOutput("allNA"),
               uiOutput("table"),
               uiOutput("test"),
               uiOutput("test_table"),
               plotOutput("test_plot", height = "600px")
             )),
    # ,
    tabPanel("Correlation",
    #          sidebarPanel(
    #            # uiOutput("variable_correlation_1"),
    #            # uiOutput("variable_correlation_2"),
    #            uiOutput("variable"),
    #            uiOutput("dis_variables"),
    #            span("The Goodman and Kruskal tau measure is an asymmetric association measure between two categorical variables, based on the extent to which variation in one variable can be explained by the other. This function returns an S3 object of class 'GKtauMatrix' that gives the number of levels for each variable on the diagonal of the matrix and the association between variables in the off-diagonal elements. Note that this matrix is generally NOT symmetric, in contrast to standard correlation matrices."),
    #          ),
             mainPanel(
    #            uiOutput("test2"),
    #            uiOutput("test3"),
    #            uiOutput("test4"),
               h4(style = "font-size:20px;",strong("Feature to be added later"))

    #            h4(style = "font-size:20px;",strong("Heat")),
    #            plotOutput("test_plot_heat"),
    #            h4(style = "font-size:20px;",strong("Performance Analystics")),
    #            plotOutput("test_corrplot"),
    #            p("In the above plot:"),
    #            tags$ul(
    #              tags$li(em(strong("The distribution of each variable is shown on the diagonal."))),
    #              tags$li(em(strong("On the bottom of the diagonal : the bivariate scatter plots with a fitted line are displayed"))),
    #              tags$li(em(strong("On the top of the diagonal : the value of the correlation plus the significance level as stars"))),
    #              tags$li(em(strong("Each significance level is associated to a symbol : p-values(0, 0.001, 0.01, 0.05, 0.1, 1) <=> symbols('***', '**', '*', '.',)")))
    #            )
             ))
    )
    # Sidebar with a slider input for number of bins 
)
# Define server logic required to draw a histogram
server <- function(input, output) {

  ########## Getting the Label_colname ##########
  label_colname <- reactive({load.label_colname(input$toolInput$datapath)})
  
  ########## Getting the Survey ##########
  tool.survey <- reactive({
    req(input$toolInput$datapath,
        input$dataInput$datapath)
    # sheet_names <- excel_sheets(input$dataInput$datapath)
    tool.survey <- read_xlsx(input$toolInput$datapath, sheet = "survey", col_types = "text") %>%
      filter(!is.na(type)) %>%
      mutate(q.type=as.character(lapply(type, function(x) str_split(x, " ")[[1]][1])),
             list_name=as.character(lapply(type, function(x) str_split(x, " ")[[1]][2])),
             list_name=ifelse(str_starts(type, "select_"), list_name, NA))
  
  
    # select only the relevant (English) labels, hints etc.
    lang_code <- str_split(label_colname(), "::", 2, T)[2]
    lang_code <- str_replace(str_replace(lang_code, "\\(", "\\\\("), "\\)", "\\\\)")
    cols <- colnames(tool.survey)
    cols_to_keep <- cols[str_detect(cols, paste0("((label)|(hint)|(constraint_message))::",lang_code)) |
                           !str_detect(cols, "((label)|(hint)|(constraint_message))::")]
  
    tool.survey <- select(tool.survey, all_of(cols_to_keep))
  
    # Find which data sheet question belongs to:
    tool.survey <- tool.survey %>% mutate(datasheet = NA)
    sheet_name <- "main"
    for(i in 1:nrow(tool.survey)){
      toolrow <- tool.survey %>% slice(i)
      if(str_detect(toolrow$type, "begin[ _]repeat")) sheet_name <- toolrow$name
      else if(str_detect(toolrow$type, "end[ _]repeat")) sheet_name <- "main"  # watch out for nested repeats (Why would you even want to do that?)
      else if(str_detect(toolrow$type, "((end)|(begin))[ _]group", T)) tool.survey[i, "datasheet"] <- sheet_name
    }
    return(tool.survey)
  })
  
  ########## Getting the Choices ##########
  tool.choices <- reactive({
    req(input$toolInput$datapath)
    read_xlsx(input$toolInput$datapath, sheet = "choices", col_types = "text") %>%
      filter(!is.na(list_name)) %>%
      select(all_of(c("list_name", "name")), !!sym(label_colname())) %>% distinct()
  })
  
  ########## Getting the Sheet input ##########
  output$sheetInput <- renderUI({
    req(input$toolInput$datapath,
        input$dataInput$datapath)
    tool_var <- excel_sheets(input$dataInput$datapath)
    # tool.survey() %>% filter(!is.na(datasheet)) %>% pull(datasheet) %>% unique()
    selectInput("sheet","Sheet",
                choices = c(tool_var),
                selected = NULL,
                multiple = F)
  })
  
  ########## Getting the Variable Input ##########
  observeEvent(input$sheet, {
    output$variable <- renderUI({
      req(input$toolInput$datapath,
          input$dataInput$datapath)
      df <- read_excel(input$dataInput$datapath,sheet = input$sheet)
      tool_var <- tool.survey() %>%
        filter(q.type %in% c("integer","select_one","select_multiple","decimal") & datasheet == input$sheet & name %in% colnames(df)) %>% pull(name) %>% unique
      selectInput("variable","Variable",
                  choices = c(tool_var),
                  selected = NULL,
                  multiple = F)
    })
  })
  
  ########## Getting the Disaggregated Variable Input ##########
  observeEvent(input$sheet, {
    output$dis_variables <- renderUI({
      req(input$toolInput$datapath,
          input$dataInput$datapath)
      df <- read_excel(input$dataInput$datapath,sheet = input$sheet)
      tool_var <- tool.survey() %>%
        filter(q.type %in% c("integer","select_one","select_multiple","decimal") & datasheet == input$sheet & name %in% colnames(df) & name != input$variable) %>% pull(name) %>% unique
      selectInput("dis_variables","Disaggregation",
                  choices = c("No disaggregation", tool_var),
                  selected = "No disaggregation",
                  multiple = F)
    })
  })
  
  ########## Getting the entry ##########
  entry <- reactive({
    req(input$toolInput$datapath,
        input$dis_variables)
    entry <- list()
    entry$variable <- input$variable
    not_in_tool <- input$variable[!input$variable %in% tool.survey()$name]
    if(length(var) > 0){
      warning(paste("vars not found in tool.survey:", paste0(not_in_tool, collapse = ", ")))
    }
    if (any(str_detect(input$variable, "/"))) input$variable <- str_split(input$variable, "/", 2, T)[,1]
    res <- data.frame(name = input$variable) %>% 
      left_join(select(tool.survey(), name, !!sym(label_colname())), by = "name", na_matches = "never") %>% 
      pull(label_colname())
    if(is.na(res)){
      entry$label <- "No label"
    } else{
      entry$label <- res
    }
    if (input$dis_variables == "No disaggregation"){
      entry$disaggregate.variables <- NA
      entry$disaggregations <- NA
    } else{
      entry$disaggregate.variables <- input$dis_variables
      entry$disaggregations <- input$dis_variables
    }

    if (is.na(input$calculation)){
      entry$calculation <- NA
    } else{
      entry$calculation <- input$calculation
    }
    entry$func <- tool.survey() %>%
      filter(name == input$variable) %>% pull(q.type)

    if(entry$func %in% c("calculate","integer","decimal")) entry$func <- "numeric"

    entry$var_type <- input$func
    entry$admin <- input$admin
    entry$list_name <- tool.survey() %>%
      filter(name == input$variable) %>% pull(list_name)
    entry$comments <- ''
    entry$datasheet<- tool.survey() %>%
      filter(name == input$variable) %>% pull(datasheet)
    if (input$calculation == "include_na"){
      entry$omit_na <- F
    } else{
      entry$omit_na <- T
    }
    if (input$calculation == "add_total"){
      entry$add_total <- T
    } else{
      entry$add_total <- F
    }
    return(entry)
    })


  ########## Getting the Data List ##########
  data.list <- reactive({
    req(input$dataInput$datapath,
        input$weightBTN)
    tool.survey <- tool.survey()
    sheet_names <- excel_sheets(input$dataInput$datapath)
    sheet_names[1] <- "main"
    data.list <- list("main" = read_excel(input$dataInput$datapath, sheet=1, col_types = "text"))
    for(sheet in sheet_names[-1])
      data.list[[sheet]] <- read_excel(input$dataInput$datapath, sheet=sheet, col_types = "text")
    tool_datasheets <- tool.survey %>% distinct(datasheet) %>% filter(!is.na(.)) %>% pull
    for (sheet in sheet_names[-1]) {
      # take the first column from this sheet and find it in tool.survey
      i <- 1
      data_cnames <- data.list[[sheet]] %>% select(-contains("/"), -starts_with("_")) %>% names
      first_col <- data_cnames[i]
      while (!first_col %in% tool.survey()$name) {
        i <- i + 1
        first_col <- data_cnames[i]
      }
      old_sheetname <- tool.survey %>% filter(name == first_col) %>% pull(datasheet)
      # change all occurences of `old_sheetname` to `sheet`
      tool.survey <- tool.survey %>% mutate(datasheet = ifelse(datasheet %==na% old_sheetname, sheet, datasheet))
    }
    if(input$weightBTN == "no") {
      for (sheet in names(data.list)){
        data.list[[sheet]] <- data.list[[sheet]] %>%
          mutate(overall = "overall",
                 weight = 1)
      }
    } else {
      for (sheet in names(data.list)){
        data.list[[sheet]] <- data.list[[sheet]] %>%
          mutate(overall = "overall",
                 weight = as.numeric(weight))
      }
    }
    tryCatch(
      return(data.list), error = function(err) return(data.frame())
      )
  })

  ########## Creating the Title Table Output ##########
  output$title_table <- renderUI({
    req(input$variable)

    HTML("<h3>",entry()[["label"]],"</h3>")
  })
  
  ########## Creating the Sub-Title Table Output ##########
  output$subtitle_table <- renderUI({
    req(input$variable)
    HTML("<h5>Variable name: <em><strong>", entry()[["variable"]], "</strong></em></h5>")
  })
  
  ########## Creating the Srvyr Design List ##########
  srvyr.designs <- reactive({
    req(input$dataInput$datapath,
        input$toolInput$datapath,
        input$sheet,
        input$variable,
        input$weightBTN)
    data.list <- data.list()
    srvyr.designs <- list()
    entry <- entry()
    tool.choices <- tool.choices()
    sheet <- input$sheet
    col <- input$variable
    if(!all(is.na(entry$disaggregate.variables))){
      for(disagg.var in entry$disaggregate.variables){
        if(!disagg.var %in% colnames(data.list[[sheet]])){
          # disagg.var was not found, but maybe it's located in main? let's try to fix!
          if(sheet == "main") stop("Disaggregation variable ", disagg.var, " was not found in main!!\n")
          if(disagg.var %in% colnames(data.list$main)){
            # cat("... Disaggregation variable", disagg.var,"was not found in sheet",sheet,"but it exists in main. Will attempt to simply left_join by uuid... ")
            join_attempt <- data.list()$main %>% select(uuid, !!sym(disagg.var))
            data.list[[sheet]] <- data.list[[sheet]] %>% left_join(join_attempt, by = "uuid")
            # cat(" success!")
          }else stop(paste("Disaggregation variable", disagg.var, "not found in sheet",sheet,"nor in main!!\n"))
        }
        data.list[[sheet]][[disagg.var]] <- as.character(data.list[[sheet]][[disagg.var]])  # as character, not factor!
      }
    }
    if(entry$func == "select_multiple"){
      # not converting to label here. instead just replace "/" with "___" and convert to numeric
      choice_cols <- colnames(data.list[[sheet]])[colnames(data.list[[sheet]]) %>% str_starts(paste0(col, "/"))]
      data.list[[sheet]] <- data.list[[sheet]] %>%
        mutate(across(all_of(choice_cols), as.numeric)) %>%
        rename_with(~str_replace(., "/", "___"), choice_cols)
      if(!entry$omit_na){
        # change NAs from all other choice columns to 0
        data.list[[sheet]] <- data.list[[sheet]] %>%
          mutate(across(starts_with(paste0(col,"___")), ~replace_na(., 0)))
        # create a new NA column
        na_colname <- paste0(col,"___NA")
        data.list[[sheet]][[na_colname]] <- is.na(data.list[[sheet]][[col]]) %>% as.numeric
        data.list[[sheet]] <- data.list[[sheet]] %>% relocate(all_of(na_colname), .after = !!sym(col))
      }
    }else {
      if(entry$func == "select_one") {
        # try to convert to label:
        choice_names <- tool.choices %>% filter(list_name == entry$list_name) %>% pull(name)
        not_in_choices <- data.list[[sheet]] %>% filter(!(!!sym(col) %in% choice_names) & !isna(!!sym(col))) %>%
          pull(!!sym(col)) %>% unique
        if(length(not_in_choices) > 0){
          warning(paste0("These choices from column ", col, " were not found among the options in tool.choices! (list_name = ",entry$list_name,")\n\t",
                         paste(not_in_choices, collapse = ", "),
                         "\n\t\tSkipping conversion to label for this column!"))
          conv_vec <- data.list[[sheet]][[col]]
        }else{
          if(!entry$list_name %in% tool.choices$list_name) stop(paste("list",entry$list_name, "not found in tool.choices!"))
          
          res <- data.frame(name = unlist(data.list[[sheet]][[col]])) %>%
            left_join(select(tool.choices, name, list_name, label_colname()) %>% filter(list_name == entry$list_name),
                      by = "name", na_matches = "never")
          if(any(is.na(res[[label_colname()]]))){
            culprits <- paste0(filter(res, is.na(!!sym(label_colname()))) %>%
                                 pull(name), collapse = ", ")
            warning(paste0("Choices not in the list (", entry$list_name, "):", culprits))
          }
          if(nrow(res) == 0) stop("All choices not in the list!")
          
          conv_vec <- pull(res, label_colname())
        }
        if(entry$omit_na) {
          data.list[[sheet]][[col]] <- factor(conv_vec, exclude = NA)
        }else{
          data.list[[sheet]][[col]] <- factor(conv_vec, exclude = NULL)
        }
        rm(conv_vec, choice_names, not_in_choices)
      }
      else if(entry$func %in% c("mean", "median", "integer", "numeric","decimal")) data.list[[sheet]][[col]] <- as.numeric(data.list[[sheet]][[col]])
    }
    survey_data <- data.list[[sheet]]
    srvyr.designs[[sheet]] <- as_survey_design(survey_data, weights = weight)
    return(srvyr.designs)
  })
  
  ########## Creating the All NA Output ##########
  output$allNA <- tryCatch({
    renderUI({
    req(input$admin,
        input$sheet,
        input$variable)
    entry <- entry()
    if(input$admin == "strata") entry$admin <- "strata"
    srvyr.designs <- srvyr.designs()
    if(length(srvyr.designs[[input$sheet]][["variables"]][[input$variable]][!is.na(srvyr.designs[[input$sheet]][["variables"]][[input$variable]])]) == 0){
      HTML("<h4><em>No data for this variable (all NA)</em></h4>")
    } else{
      HTML("<h4><em>", as_perc((length(srvyr.designs[[input$sheet]][["variables"]][[input$variable]][!is.na(srvyr.designs[[input$sheet]][["variables"]][[input$variable]])])/nrow(data.list()[[input$sheet]]))),"of respondents answered this question.</em></h4>")
    }
    })
  },
  error = function(err) {
    renderUI({HTML("Did you check the Requirement? If yes and still an error, please contact Senior Data Team :)")})
  })
  
  ########## Creating the table ##########
  table <- reactive({
    req(input$dataInput$datapath,
        input$toolInput$datapath,
        input$sheet,
        input$variable)
    entry <- entry()
    srvyr.design <- srvyr.designs()
    if(entry$omit_na) srvyr.design[[input$sheet]] <- srvyr.design[[input$sheet]] %>% filter(!is.na(!!sym(input$variable)))
    srvyr.design[[input$sheet]]  <- srvyr.design[[input$sheet]] %>% group_by(!!sym(entry$admin))
    for (disagg.var in entry$disaggregate.variables) {
      res <- make_table(srvyr.design[[input$sheet]], entry, disagg.var) %>% ungroup %>% select(-any_of("overall"))
      # add overall
      if(entry$admin != "overall"){
        entry.ovrl <- entry
        entry.ovrl$admin <- "overall"
        if(!"overall" %in% (srvyr.design[[input$sheet]] %>% variable.names)) srvyr.design[[input$sheet]] <- srvyr.design[[input$sheet]] %>% mutate(overall = "overall")
        res.overall <- make_table(srvyr.design[[input$sheet]] %>% ungroup %>% group_by(overall),
                                  entry.ovrl, disagg.var)  %>%
          mutate(!!sym(entry$admin) := "overall") %>%
          ungroup %>% select(-any_of("overall"))
        res <- res %>% bind_rows(res.overall) %>% distinct
      }
    }
    return(res)
  })
  
  ########## Creating the table Output ##########
  output$table <- tryCatch({
    renderUI({
      renderDataTable(table(), extensions= 'Buttons', options = tableFormat)
    })
    },
    error = function(err) {
      renderUI({HTML("Did you check the Requirement? If yes and still an error, please contact Senior Data Team :)")})
    },
    warning = function(warning) {
      renderUI({HTML("Did you check the Requirement? If yes and still an error, please contact Senior Data Team :)")})
    }
  )
  
  # ########## Creating the first variable in correlation ##########
  # observeEvent(input$sheet, {
  #   output$variable_correlation_1 <- renderUI({
  #     req(input$toolInput$datapath,
  #         input$dataInput$datapath)
  #     df <- read_excel(input$dataInput$datapath,sheet = input$sheet)
  #     tool_var <- tool.survey() %>%
  #       filter(q.type %in% c("integer","select_one","select_multiple","decimal") & datasheet == input$sheet & name %in% colnames(df)) %>% pull(name) %>% unique
  #     selectInput("variable","Variable",
  #                 choices = c(tool_var),
  #                 selected = NULL,
  #                 multiple = F)
  #   })
  # })
  # 
  # ########## Creating the second variable in correlation ##########
  # observeEvent(input$sheet, {
  #   output$variable_correlation_2 <- renderUI({
  #     req(input$toolInput$datapath,
  #         input$dataInput$datapath)
  #     df <- read_excel(input$dataInput$datapath,sheet = input$sheet)
  #     tool_var <- tool.survey() %>%
  #       filter(q.type %in% c("integer","select_one","select_multiple","decimal") & datasheet == input$sheet & name %in% colnames(df)) %>% pull(name) %>% unique
  #     selectInput("dis_variables","Disaggregation",
  #                 choices = c("No disaggregation", tool_var),
  #                 selected = "No disaggregation",
  #                 multiple = F)
  #   })
  # })
  # 
  ########## Creating the correlation table ##########
  correlation_table <- reactive({
    req(input$variable,
        input$dis_variables)
    cols <- colnames(table())
    if(input$admin == "strata"){
      cols <- cols[!cols %in% c(input$dis_variables,"num_samples","strata")]
    } else{
      cols <- cols[!cols %in% c(input$dis_variables,"num_samples")]
    }
    if(entry()$func != "numeric"){
      table <- table() %>%
        mutate_at(cols, ~gsub("%","",.)) %>%
        mutate_at(cols, as.numeric) %>%
        mutate_at(cols, ~round((. * num_samples)/100),0) %>%
        select(-num_samples)
      if(input$admin == "strata"){
        table <- table %>%
          pivot_longer(cols = cols,names_to = "names", values_to = "value") %>%
          filter(value != 0,strata != "overall")
      }else{
        table <- table %>%
          pivot_longer(cols = cols,names_to = "names", values_to = "value") %>%
          filter(value != 0)
      }
    } else{
      table <- table() %>% 
         select(-num_samples)
      if(input$admin == "strata"){
        table <- table %>%
          pivot_longer(cols = cols,names_to = "names", values_to = "value") %>%
          filter(value != 0,strata != "overall")
      }else{
        table <- table %>%
          pivot_longer(cols = cols,names_to = "names", values_to = "value") %>%
          filter(value != 0)
      }
    }

    return(table)
  })

  output$test_plot <- renderPlot({
    req(input$variable,
        input$dis_variables)
    correlation_table <- correlation_table()
    if(input$dis_variables == "No disaggregation"){
      if(input$admin == "strata"){
        if(entry()$func == "numeric"){
          correlation_table <- filter(correlation_table, names == "mean")
          visualizeR::bar(correlation_table, x = names, y= value, group = strata,percent = F,flip = F, theme = visualizeR::theme_reach(text_font_face = "bold", axis_y = F))


          
          # ggplot(data = correlation_table,aes(x=names,y=value, group=strata,fill=strata))+
          #   geom_bar(stat="identity",position = "dodge", width = 0.2)+
          #   theme_ipsum()+
          #   xlab("")+
          #   ylab("")+
          #   theme(axis.text.x = element_text(angle = 45, vjust = 0.5))+
          #   geom_hline(yintercept = mean(correlation_table$value, na.rm=TRUE), color = 'blue', lty= 'dashed')+
          #   geom_text(aes(label=paste0("mean: ",mean(value, na.rm=TRUE)), x=0, y=mean(value, na.rm=TRUE)), vjust=-0.5, hjust=-0.1, col='blue', size=3)
            
        }else{
          visualizeR::bar(correlation_table, x = names, y= value, group = strata, percent = F, alpha = 0.6)
          # ggplot(data = correlation_table,aes(x=names,y=value,group=strata,fill=strata))+
          #   geom_bar(stat = "identity", position = "dodge", width = 0.2)+
          #   theme_ipsum()+
          #   xlab("")+
          #   ylab("")+
          #   theme(axis.text.x = element_text(angle = 45, vjust = 0.5))
        }
      }else{
        if(entry()$func == "numeric"){
          correlation_table <- filter(correlation_table, names == "mean")
          ggplot(data = correlation_table,aes(x=names,y=value))+
            geom_bar(stat="identity", width = 0.2, fill = "#ee5859")+
            theme_ipsum()+
            xlab("")+
            ylab("")+
            theme(axis.text.x = element_text(angle = 45, vjust = 0.5))
        }else{
          ggplot(data = correlation_table,aes(x=names,y=value))+
            geom_bar(stat = "identity", width = 0.2, fill = "#ee5859")+
            theme_ipsum()+
            xlab("")+
            ylab("")+
            theme(axis.text.x = element_text(angle = 45, vjust = 0.5))
        }
      }
    } else{
      if(input$admin == "strata"){
        if(entry()$func == "numeric"){
          correlation_table <- filter(correlation_table, names == "mean")
          visualizeR::bar(correlation_table, x = !!sym(input$dis_variables), y= value, group = strata,flip = F,percent = F,add_text = TRUE, theme = visualizeR::theme_reach(text_font_face = "bold", axis_y = F, text_color = "black"))
          
          # visualizeR::point(correlation_table,x = !!sym(input$dis_variables), y=value, group = strata, size = 1.5,theme = visualizeR::theme_reach(palette = "artichoke_3",, grid_major_x = TRUE,  title_position_to_plot = FALSE))
          
          # ggplot(data = correlation_table,aes(x=!!sym(input$dis_variables),y=value, fill =strata, group =strata))+
          #   geom_bar(stat = "identity", position = "dodge")+
          #   facet_wrap(~names)+
          #   theme_ipsum()+
          #   xlab("")+
          #   theme(axis.text.x = element_text(angle = 45, vjust = 0.5))+
          #   geom_hline(yintercept = mean(correlation_table$value, na.rm=TRUE), color = 'blue', lty= 'dashed')+
          #   geom_text(aes(label=paste0("mean: ",mean(value, na.rm=TRUE)), x=0, y=mean(value, na.rm=TRUE)), vjust=-0.5, hjust=-0.1, col='blue', size=3)
            
        }else{
          ggplot(data = correlation_table,aes(x=!!sym(input$dis_variables),y=value, fill =strata, group =strata))+
            geom_bar(stat = "identity", position = "dodge")+
            facet_wrap(~names)+
            theme_ipsum()+
            xlab("")+
            theme(axis.text.x = element_text(angle = 45, vjust = 0.5))
        }

      }else{
        if(entry()$func=="numeric"){
          # correlation_table <- filter(correlation_table, names == "mean")
          # visualizeR::bar(correlation_table, x = !!sym(input$dis_variables), y= value,flip = F,add_text = TRUE,percent = F, theme = visualizeR::theme_reach(text_font_face = "bold", axis_y = F, text_color = visualizeR::cols_reach()[2]))
          correlation_table <-filter(correlation_table, names %in% c("min", "max"))
          visualizeR::dumbbell(correlation_table,value,names, !!sym(input$dis_variables), theme = visualizeR::theme_reach(legend_position =  "bottom",
                                                                                                                          legend_direction = "horizontal",
                                                                                                                          legend_title_font_face = "bold",
                                                                                                                          palette = "primary",
                                                                                                                          title_position_to_plot = FALSE,
                                                                                                                          legend.title.align = 0.5))
          
          # ggplot(data = correlation_table,aes(x=!!sym(input$dis_variables),y=value))+
          #   geom_bar(stat = "identity", position = "dodge",fill = "#ee5859")+
          #   facet_wrap(~names)+
          #   theme_ipsum()+
          #   theme(legend.position = "none")+
          #   xlab("")+
          #   theme(axis.text.x = element_text(angle = 45, vjust = 0.5)) +
          #   geom_hline(yintercept = mean(correlation_table$value, na.rm=TRUE), color = 'blue', lty= 'dashed')+
          #   geom_text(aes(label=paste0("mean: ",mean(value, na.rm=TRUE)), x=0, y=mean(value, na.rm=TRUE)), vjust=-0.5, hjust=-0.1, col='blue', size=3)
            
        }else{
          ggplot(data = correlation_table,aes(x=!!sym(input$dis_variables),y=value))+
            geom_bar(stat = "identity", position = "dodge",fill = "#ee5859")+
            facet_wrap(~names)+
            theme_ipsum()+
            theme(legend.position = "none")+
            xlab("")+
            theme(axis.text.x = element_text(angle = 45, vjust = 0.5))
        }
      }
    }
    
  })
  # 
  # ########## Calculating Lambda ##########
  # calculations <- reactive({
  #   req(input$variable,
  #       input$dis_variables)
  #   lam <- round(Lambda(correlation_table()),2)
  #   assoc <- vcd::assocstats(correlation_table())
  #   cram <- round(assoc$cramer, 2)
  #   phi <- round(assoc$phi,2)
  #   cont <- round(assoc$cont,2)
  #   calculations <- list("lam" = lam,
  #                        "cram" = cram,
  #                        "phi" = phi,
  #                        "cont" = cont)
  #   return(calculations)
  # })
  # 

  # output$test2 <- renderUI({
  #   renderText(HTML("Lambda = ", calculations()$lam))
  # })
  # output$test3 <- renderUI({
  #   renderText(HTML("Phi-Coefficient = ", calculations()$phi))
  # })
  # output$test4 <- renderUI({
  #   renderText(HTML("Contingency Coefficient = ", calculations()$cont))
  # })
  # output$test_plot <- renderPlot({
  #   gktau <- GoodmanKruskal::GKtauDataframe(correlation_table())
  #   plot(gktau)
  # })
  # output$test_plot_heat <- renderPlot({
  #   cor <- cor(correlation_table())
  #   col <- colorRampPalette(c("blue","white","red"))(20)
  #   heatmap(x = cor,col = col, symn = T)
  # })
  # output$test_corrplot <- renderPlot({
  #   chart.Correlation(correlation_table(),histogram = T,pch = 19)
  # })
}

# Run the application 
shinyApp(ui = ui, server = server)