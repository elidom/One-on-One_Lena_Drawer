## app.R ##

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Specify the minimum number of rows needed for sampling:
# e.g., to be able to sample 20% a minimum of 5 rows is needed (1 sample = 20%) #
n <- 5
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# libraries ---------------------------------------------------------------
library(shiny)
library(shinydashboard)
library(tidyverse)
library(reactable)
library(readxl)
library(shinyWidgets)


df <- read_xlsx("LENAfiltered_all_coded_master.xlsx") %>% 
  mutate(child_id = factor(paste("ch_", child_id, sep = "")),
         lang = factor(lang),
         spkr = factor(spkr),
         age = factor(age))

# UI ----------------------------------------------------------------------
ui <- dashboardPage(
  
  ## Header -------------------------------
  dashboardHeader(title = "Speaker Drawer"),

  ## Sidebar -------------------------------  
  dashboardSidebar(
    
    sidebarMenu(
      menuItem(text = "Sampling",
               tabName = "sampling_tab",
               icon = icon("dashboard")),
      
      menuItem(text = "Selected Samples",
               tabName = "selected_tab",
               icon = icon("list"))
      
    ), #close sidebar menu
    
    
    selectInput(
      inputId = "select_child",
      label = "Select Child",
      choices = unique(df$child_id),
      multiple = F
    ), #close speaker select
    
    selectInput(
      inputId = "select_speaker",
      label = "Select Speaker",
      choices = unique(df$spkr),
      multiple = F
    ), #close speaker select
    
    selectInput(
      inputId = "select_lang",
      label = "Select Language",
      choices = unique(df$lang),
      multiple = F
    ), #close speaker select
    
    selectInput(
      inputId = "select_age",
      label = "Select Age",
      choices = unique(df$age),
      multiple = F
    ), #close speaker select
    
    hr(),
    
    actionButton(
      inputId = "action",
      label = "Sample!"
    ),
    
    h3(textOutput("results"), style="color:red"),
    
    h4(textOutput("n_sampled"))
    
  ), #close sidebar
  
  ## Body -------------------------------
  dashboardBody(
    
    tabItems(
      
      ### Tab 1 - Sampling ---------
      tabItem(
        tabName = "sampling_tab", 
        
        box(title = "Summary Table",
            status = "info",
            solidHeader = TRUE,
            width = 6,
            #height = "400px",
            reactableOutput(outputId = "table")
        ), #close box
        
        box(
          title = "Currently Sampled Row",
          status = "success",
          solidHeader = TRUE,
          height = 150,
          reactableOutput("sampled_row",
                          height = "90px")
        ), #close box
        
        box( 
          div(style = "text-align:center;", 
              actionBttn(
              inputId = "accept_bttn",
              label = "Accept Sample",
              icon = icon("thumbs-up"),
              color = "success",
              size = "lg",
              style = "material-flat"
        ),
        actionBttn(
          inputId = "reject_bttn",
          label = "Reject Sample",
          icon = icon("thumbs-down"),
          color = "danger",
          size = "lg",
          style = "material-flat"
        )
        )), #close bttn
        
        box(
          div(style = "text-align: center;",
              h2(
                textOutput(
                  "percent_accepted"
                )
              )
           )
        )
        
        
        
      ),
      
      ### Tab 2 - Selected ---------
      tabItem(
       tabName = "selected_tab",
       
       box(
         title = "Accepted Samples",
         status = "success",
         solidHeader = TRUE,
         reactableOutput(outputId = "selected_table")
       ),
       
       downloadBttn(
         outputId = "download_bttn"
       )
        
        
      )
    )
    
    
    
  ) #close body
) #close dashboard



# server ------------------------------------------------------------------
server <- function(input, output, session) {
  
  # Filter the tibble according to user selection
  df_filtered <- reactive(
    df %>% filter(child_id %in% input$select_child,
                  spkr %in% input$select_speaker,
                  lang %in% input$select_lang,
                  age %in% input$select_age)
    ) # close reactive
  
  # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
  # note for future me: I think I could this exact same thing (^) for the #
  # rejected and accepted sets so that I don't repeat it on every action. #
  # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
  
  # Initialize an empty data frame
  selected_rows <- reactiveVal(data.frame())
  rejected_rows <- reactiveVal(data.frame())
  sampled_rows <- reactiveVal(data.frame())
  
  
  # Render table
  output$table <- renderReactable(
    reactable(df_filtered(),
              filterable = T,
              compact = T)
  )
  
  n_dropped <- reactiveVal(0)
  
  # show warning if insufficient number of rows
  output$results <- renderText({
    # check if there are rejected rows
    if (nrow(rejected_rows()) > 0) {
      # if there are, filter and count how many of this set have been rejected
      current_rj <- rejected_rows()
      rej <- current_rj %>% 
        filter(child_id %in% input$select_child,
               spkr %in% input$select_speaker,
               lang %in% input$select_lang,
               age %in% input$select_age)
      
      avl_rows <- nrow(df_filtered()) - nrow(rej)
      
    } else { # else avl rows is just df_filtered
      avl_rows <- nrow(df_filtered())
    }
    
    print(
      paste(
        ifelse(
          avl_rows < n,
          "NOT ENOUGH ROWS!",
          ""
          )
        )
      )
  })

  # Store the sampled row
  sampledRow <- reactiveVal(data.frame(Empty = numeric())) 
  
  # initialize bouncer
  allowSampling <- reactiveVal(TRUE)
  
  # allow rejection
  allowReject <- reactiveVal(FALSE)
  
  # allow accept
  allowAccept <- reactiveVal(FALSE)
  
  # initialize vector that keeps track of sampled indices  
  sampled_indices <- reactiveVal(numeric())
  
  # React to button press
  observeEvent(
    input$action, {
      
      
      if (allowSampling()) { 
        
        allowSampling(FALSE) # don't allow double sampling
        allowReject(TRUE)    # enable reject button
        allowAccept(TRUE)    # enable accept button
        
        current_df <- df_filtered()
        
        if (nrow(current_df) > length(sampled_indices())) {
          repeat {
            sampled_index <- sample(nrow(current_df), 1)
            if (!(sampled_index %in% sampled_indices())) {
              break
            }
          }
          
          # Update the reactive value for sampledRow
          sampledRow(data.frame(current_df[sampled_index, , drop = F]))
          
          # Update the sampled rows record
          current_sampled <- sampled_rows()
          all_sampled <- bind_rows(current_sampled, sampledRow())
          sampled_rows(all_sampled)
          
          # Append the newly sampled index to the vector of sampled indices
          sampled_indices(c(sampled_indices(), sampled_index))
        } else {
          showNotification("All rows have been sampled!", type = "warning")
        }
        
      } else {
        
        showNotification("Please click on Accept or Reject before sampling again.", type = "warning")
        
      }

    } # close action
  ) # close Event
  
  observe({
    # reset indexes if new filters are applied
    input$select_child
    input$select_speaker
    input$select_lang
    input$select_age
    
    # Reset sampled indices
    sampled_indices(numeric())
    n_dropped(0)
    allowSampling(TRUE)
    sampledRow(data.frame(Empty = numeric()))
  })
  
  
  output$n_sampled <- renderText({
    current_sampled <- sampled_rows()
    if (nrow(current_sampled > 0)) {
      sampled <- current_sampled %>% 
        filter(child_id %in% input$select_child,
               spkr %in% input$select_speaker,
               lang %in% input$select_lang,
               age %in% input$select_age)  
    } else {
      sampled <- current_sampled
    }
    
    print(
      paste(
        nrow(sampled), 
               "rows sampled out of", 
               nrow(df_filtered())))
      }
    )
  

  output$sampled_row <- renderReactable({
    reactable(sampledRow(),
              filterable = F,
              compact = T)
  })
  
  # # # # # # # # # # ACCEPT BUTTON
  observeEvent(
    input$accept_bttn, {
      
      # 1. see if there is a sample, warn otherwise
      new_row <- sampledRow()
      current_df <- selected_rows()
      current_rj <- rejected_rows()
      
      if (nrow(new_row) > 0) { # open 1
        
        # 2. check if there are accepted rows
        if (nrow(current_df) > 0) {
          # 2.1. filter
          flt <- current_df %>% 
            filter(child_id %in% input$select_child,
                   spkr %in% input$select_speaker,
                   lang %in% input$select_lang,
                   age %in% input$select_age)
          
          # 2.2. check if rejected df has rows
          if (nrow(current_rj) > 0) {
            rej <- current_rj %>% 
              filter(child_id %in% input$select_child,
                     spkr %in% input$select_speaker,
                     lang %in% input$select_lang,
                     age %in% input$select_age)
            
            # 2.2.3. if new row is not in flt or rej, add!
            if (!(new_row$recording %in% flt$recording) &
                !(new_row$recording %in% rej$recording)) {
              
              updated_appended <- bind_rows(current_df, new_row)
              selected_rows(updated_appended)
              showNotification("Sampled Added!", duration = 2, type = "message")
              
              allowReject(FALSE)   # Disable reject button until new sample
              allowAccept(FALSE)   # Disable accept button
              allowSampling(TRUE)  # allow sampling again
            } # close 2.2.3.
            else {
              showNotification("You have already accepted or rejected this sample.", duration = 2, type = "warning")
            }
          } # close 2.2.
          else { # 2.3. else don't care about rej
            # 2.3.check if not in flt
            if (!(new_row$recording %in% flt$recording)) {
              
              updated_appended <- bind_rows(current_df, new_row)
              selected_rows(updated_appended)
              showNotification("Sampled Added!", duration = 2, type = "message")
              
              allowReject(FALSE)   # Disable reject button until new sample
              allowAccept(FALSE)   # Disable accept button
              allowSampling(TRUE)  # allow sampling again
            } # close 2.3
            else {
              showNotification("You have already accepted this sample", duration = 2, type = "warning")
            }
          }
          
        } # close 2. 
        else { # 3. check if not in rejected
          # 3.1. check if rejected df has rows
          if (nrow(current_rj) > 0) {
            rej <- current_rj %>% 
              filter(child_id %in% input$select_child,
                     spkr %in% input$select_speaker,
                     lang %in% input$select_lang,
                     age %in% input$select_age)
            # 3.1.1. check if already rejected
            if (!(new_row$recording %in% rej$recording)) {
              
              updated_appended <- bind_rows(current_df, new_row)
              selected_rows(updated_appended)
              showNotification("Sampled Added!", duration = 2, type = "message")
              
              allowReject(FALSE)   # Disable reject button until new sample
              allowAccept(FALSE)   # Disable accept button
              allowSampling(TRUE)  # allow sampling again
            } # close 3.1.1.
            else {
              showNotification("You have already rejected this sample", duration = 2, type = "warning")
            }
          } # close 3.1.
          else { # 3.2. If it does not have rows either, just accept
            updated_appended <- bind_rows(current_df, new_row)
            selected_rows(updated_appended)
            showNotification("Sampled Added!", duration = 2, type = "message")
            
            allowReject(FALSE)   # Disable reject button until new sample
            allowAccept(FALSE)   # Disable accept button
            allowSampling(TRUE)  # allow sampling again
          }
        } # close 3.
      } else {
        showNotification("Please take a sample first.", duration = 2, type = "warning")
      } # close 1
    } # close action
  ) # close event observer
  
  
  
  
  # # # # # # # # # # REJECT BUTTON
  observeEvent(
    input$reject_bttn, {
      
      # 1. see if there is a sample, warn otherwise
      new_row <- sampledRow()
      current_df <- selected_rows()
      current_rj <- rejected_rows()
      
      if (nrow(new_row) > 0) { # open 1
        
        # 2. check if there are accepted rows
        if (nrow(current_df) > 0) {
          # 2.1. filter
          flt <- current_df %>% 
            filter(child_id %in% input$select_child,
                   spkr %in% input$select_speaker,
                   lang %in% input$select_lang,
                   age %in% input$select_age)
          
          # 2.2. check if rejected df has rows
          if (nrow(current_rj) > 0) {
            rej <- current_rj %>% 
              filter(child_id %in% input$select_child,
                     spkr %in% input$select_speaker,
                     lang %in% input$select_lang,
                     age %in% input$select_age)
            
            # 2.2.3. if new row is not in flt or rej, add!
            if (!(new_row$recording %in% flt$recording) &
                !(new_row$recording %in% rej$recording)) {
              
              updated_appended <- bind_rows(current_rj, new_row)
              rejected_rows(updated_appended)
              showNotification("Sampled Rejected", duration = 2, type = "warning")
              
              allowReject(FALSE)   # Disable reject button until new sample
              allowAccept(FALSE)   # Disable accept button
              allowSampling(TRUE)  # allow sampling again
            
            } # close 2.2.3.
            else {
              showNotification("You have already accepted or rejected this sample.", duration = 2, type = "warning")
            }
          } # close 2.2.
          else { # 2.3. else don't care about rej
            # 2.3.check if not in flt
            if (!(new_row$recording %in% flt$recording)) {
              
              updated_appended <- bind_rows(current_rj, new_row)
              rejected_rows(updated_appended)
              showNotification("Sampled Rejected", duration = 2, type = "warning")
              
              allowReject(FALSE)   # Disable reject button until new sample
              allowAccept(FALSE)   # Disable accept button
              allowSampling(TRUE)  # allow sampling again
            } # close 2.3
            else {
              showNotification("You have already accepted this sample", duration = 2, type = "warning")
            }
          }
          
        } # close 2. 
        else { # 3. check if not in rejected
          # 3.1. check if rejected df has rows
          if (nrow(current_rj) > 0) {
            rej <- current_rj %>% 
              filter(child_id %in% input$select_child,
                     spkr %in% input$select_speaker,
                     lang %in% input$select_lang,
                     age %in% input$select_age)
            # 3.1.1. check if already rejected
            if (!(new_row$recording %in% rej$recording)) {
              
              updated_appended <- bind_rows(current_rj, new_row)
              rejected_rows(updated_appended)
              showNotification("Sampled Rejected", duration = 2, type = "warning")
              
              allowReject(FALSE)   # Disable reject button until new sample
              allowAccept(FALSE)   # Disable accept button
              allowSampling(TRUE)  # allow sampling again
            } # close 3.1.1.
            else {
              showNotification("You have already rejected this sample", duration = 2, type = "warning")
            }
          } # close 3.1.
          else { # 3.2. If it does not have rows either, just accept
            updated_appended <- bind_rows(current_rj, new_row)
            rejected_rows(updated_appended)
            showNotification("Sampled Rejected", duration = 2, type = "warning")
            
            allowReject(FALSE)   # Disable reject button until new sample
            allowAccept(FALSE)   # Disable accept button
            allowSampling(TRUE)  # allow sampling again
          }
        } # close 3.
      } else {
        showNotification("Please take a sample first.", duration = 2, type = "warning")
      } # close 1
    } # close action
  ) # close event observer
  
  
  
  # Display how many have been accepted
  n_accepted <- reactiveVal()
  
  output$percent_accepted <- renderText({
    if (nrow(selected_rows()) > 0 & nrow(rejected_rows()) < 1) {
      
      # accepted df: flt
      current_df <- selected_rows()
      flt <- current_df %>% 
        filter(child_id %in% input$select_child,
               spkr %in% input$select_speaker,
               lang %in% input$select_lang,
               age %in% input$select_age)
  
      print(
        paste("You have accepted ",nrow(flt)," out of ",nrow(df_filtered())," (",
          round(nrow(flt)/(nrow(df_filtered()))*100, 2),
          "%).",
          sep = ""))
      
      } else if (nrow(selected_rows()) > 0 & nrow(rejected_rows()) > 0) {
        
        # accepted df: flt
        current_df <- selected_rows()
        flt <- current_df %>% 
          filter(child_id %in% input$select_child,
                 spkr %in% input$select_speaker,
                 lang %in% input$select_lang,
                 age %in% input$select_age)
        
        # rejected df: rej
        current_rej <- rejected_rows()
        rej <- current_rej %>% 
          filter(child_id %in% input$select_child,
                 spkr %in% input$select_speaker,
                 lang %in% input$select_lang,
                 age %in% input$select_age)
        
        print(
          paste("You have accepted ",nrow(flt)," out of ",nrow(df_filtered())-nrow(rej)," (",
                round(nrow(flt)/(nrow(df_filtered())-nrow(rej))*100, 2),
                "%).",
                sep = ""))
        
      } else {
        print("You have accepted 0 rows from this dataset")
      }
    }
  )
  
  # Render selected table
  output$selected_table <- renderReactable(
    reactable(selected_rows())
  ) #close table
  
  
  # Download the data
  output$download_bttn <- downloadHandler(
    filename = function() { 
      paste("dataset-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(selected_rows(), file)
    })

  }

shinyApp(ui, server)

# DEPRECATED: 
# observeEvent(
#   input$accept_bttn, {
#     
#     if (allowAccept()) {
#     
#       if (!is.null(sampledRow())) {
#         new_row <- sampledRow()
#         current_appended <- selected_rows()
#         current_rejected <- rejected_rows()
#         if (is.null(selected_rows())){
#           # Append the new row to the existing data frame
#           updated_appended <- bind_rows(current_appended, new_row)
#           selected_rows(updated_appended)
#           showNotification("Sampled Added!", duration = 2, type = "message")
#           
#           allowReject(FALSE)   # Disable reject button until new sample
#           allowAccept(FALSE)   # Disable accept button
#           allowSampling(TRUE)  # allow sampling again
#           
#         } else if (!(new_row$recording %in% current_appended$recording)) {
#             
#           if (nrow(current_rejected) < 1) {
#             updated_appended <- bind_rows(current_appended, new_row)
#             selected_rows(updated_appended)
#             showNotification("Sampled Added!", duration = 2, type = "message")
#             
#             allowReject(FALSE)   # Disable reject button until new sample
#             allowAccept(FALSE)   # Disable accept button
#             allowSampling(TRUE)  # allow sampling again
#           } else {
#             
#             if (!(new_row$recording %in% current_rejected$recording)) {
#               
#               updated_appended <- bind_rows(current_appended, new_row)
#               selected_rows(updated_appended)
#               showNotification("Sampled Added!", duration = 2, type = "message")
#               
#               allowReject(FALSE)   # Disable reject button until new sample
#               allowAccept(FALSE)   # Disable accept button
#               allowSampling(TRUE)  # allow sampling again
#             
#             } else {
#                 
#               showNotification("You've already made a decision on this sample", duration = 2, type = "warning")
#               
#               }
#           }
#           
#         } else {
#           showNotification("Already added!", duration = 2, type = "warning")
#         } #close if-else
#       } #close inner if
#     } else {
#       showNotification("You already made a decision on this sample", duration = 2, type = "warning")
#     } # close outer if
#   } #close action
# ) #close event


# DEPRECATED: 
# # Observe event for reject button
# observeEvent(input$reject_bttn, {
#   
#   if (allowReject()) {
#   
#     if (nrow(selected_rows()) > 0 & nrow(rejected_rows()) < 1) {
#     
#       # accepted df: flt
#       current_df <- selected_rows()
#       flt <- current_df %>% 
#         filter(child_id %in% input$select_child,
#                spkr %in% input$select_speaker,
#                lang %in% input$select_lang,
#                age %in% input$select_age)
#       
#       # rejected df: rej
#       current_rej <- rejected_rows()
#       rej <- current_rej 
#     
#     } else if (nrow(selected_rows()) > 0 & nrow(rejected_rows()) > 0) {
#       
#       # accepted df: flt
#       current_df <- selected_rows()
#       flt <- current_df %>% 
#         filter(child_id %in% input$select_child,
#                spkr %in% input$select_speaker,
#                lang %in% input$select_lang,
#                age %in% input$select_age)
#       
#       # rejected df: rej
#       current_rej <- rejected_rows()
#       rej <- current_rej %>% 
#         filter(child_id %in% input$select_child,
#                spkr %in% input$select_speaker,
#                lang %in% input$select_lang,
#                age %in% input$select_age)
#       
#     } else {
#       
#       current_df <- selected_rows()
#       flt <- current_df
#       
#       current_rej <- rejected_rows()
#       rej <- current_rej 
#       
#     }
#     
#     if (round(nrow(flt)/(nrow(df_filtered())-nrow(rej))*100, 2) < 100) {
#       
#       
#         # n_dropped(n_dropped()+1)
#         
#         # # # # # # # # # # # # # # # # # # # # # # 
#         if (!is.null(sampledRow())) {
#           new_row <- sampledRow()
#           current_appended <- rejected_rows()
#           if (is.null(rejected_rows())){
#             # Append the new row to the existing data frame
#             updated_appended <- bind_rows(current_appended, new_row)
#             rejected_rows(updated_appended)
#             showNotification("Sampled Rejected", duration = 2, type = "warning")
#             
#             allowReject(FALSE)   # Disable reject button until new sample
#             allowAccept(FALSE)   # Disable accept button
#             allowSampling(TRUE)  # allow sampling again
#             
#           } else if (!(new_row$recording %in% current_appended$recording)) {
#             updated_appended <- bind_rows(current_appended, new_row)
#             rejected_rows(updated_appended)
#             showNotification("Sampled Rejected", duration = 2, type = "warning")
#             
#             allowReject(FALSE)   # Disable reject button until new sample
#             allowAccept(FALSE)   # Disable accept button
#             allowSampling(TRUE)  # allow sampling again
#             
#           } else {
#             showNotification("Already rejected!", duration = 2, type = "error")
#           } #close if-else
#         } #close inner if
#         # # # # # # # # # # # # # # # # # # # # # # 
#         
#     } else {
#       
#       showNotification("You can't reject any more samples.", duration = 2, type = "error")
#       
#     }
#   } else {
#     
#     showNotification("You already made a decision on this sample", duration = 2, type = "warning")
#     
#   }
#   
# })
