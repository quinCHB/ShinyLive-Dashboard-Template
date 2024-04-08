# Shiny Dashboard
# Hastag (# ---------------------------------------------------------------) means Primary Header
# Hastag (### *** Definition ###) means Secondary Header
# Hold  alt o to collapse all
# Hold shift alt o to expand all

# Load Packages ---------------------------------------------------------------
library(shiny)
library(shinydashboard)
# library(shinyjs)  # This is used to help hide and show tabs and narratives. However, my experience as of 3/20/24 is it is slow to load for shinyLive
# Please load it as needed which is why it is commented out

# Loading data since this is a template, no data is actually pulled from somewhere.

# Create the data frame
df <- data.frame(
  #id can represent something like a region and var1 can represent something like a county (1 to many relationship)
  #By referencing the region in the var1 label than there is no confusion as to what "county" belongs to what "region"
  id = c(1, 1, 1, 2, 2, 2, 3, 3),
  var1 = c("a (1)", "b (1)", "c (1)", "d (2)", "e (2)", "f (2)", "g (3)", "h (3)")
)

# User Interface ----------------------------------------------------------
ui <- dashboardPage(
  dashboardHeader(
    title = "Basic Dashboard Setup",
    titleWidth = 400
    #disable = TRUE #uncomment if the header should be hid
  ),
  dashboardSidebar(
    width = 350, #This makes the sidebar wider. However, the input boxes seem to have a set dimension resulting in long names still wrapping
    #The input parameters are global parameters
    checkboxInput(inputId = "parGlobal_hideTabPanel", label = "HIDE TAB PANEL MENU ONE"),
    checkboxInput(inputId = "parGlobal_hideNarrative", label = "HIDE NARRATIVE MENU TWO"),
    
    # Other input elements...
    selectInput(
      "parGlobal_inputOne",
      label= "Select Letter of Interest",
      choices= sort(unique(df$var1)),
      selected= "c", #Default selection
      multiple= FALSE,
      width= 350 
    ),
    #The following two parameters are set in the server based on the previous parameter (parGlobal_inputOne)
    selectInput(
      "parGlobal_casscadeFromInputOneRestrictive",
      label= "Select Cascade Prompt (Restrictive Options)",
      choices= NULL,
      selected= NULL,
      multiple= FALSE,
      width= 350 
    ),
    selectInput(
      "parGlobal_casscadeFromInputOneNonRestrictive",
      label= "Select Cascade Prompt (Non Restrictive Options)",
      choices= NULL,
      selected= NULL,
      multiple= FALSE,
      width= 350 
    ),
    selectInput(
      "parGlobal_casscadeFromInputOneOneOption",
      label= "Select Cascade Prompt (Only Dependent Option)",
      choices= NULL,
      selected= NULL,
      multiple= FALSE,
      width= 350 
    ),
    #Sidebar is required to have sub menus because it requires the tabName to reference
    sidebarMenu(
      menuItem(text = "Menu One", tabName = "tn_menuOne"), # text is the menu label and tabName can be referenced like an unique index
      menuItem(text = "Menu Two", tabName = "tn_menuTwo") # text is the menu label and tabName can be referenced like an unique index
    )
  ),
  dashboardBody(
    shinyjs::useShinyjs(), #For performance, shinyLive seems to load faster when it isn't loading the entier shinyjs package
    fluidRow(
      column(
        12,
        tabItems(
          tabItem(
            tabName = "tn_menuOne", #This is where when the sidebar is selected it is mapped to the tab menu
            tabsetPanel(
              id= "tspID_hideTabPanel", #This is required so the tab can be referenced so it can be hid or shown
              tabPanel(
                "Menu One Tab Panel (Hide Tab Panel)", #Tab Panel name
                fluidRow(
                  # Narrative section explaining the purpose of the dashboard
                  column(
                    width = 4, #fluid rows max value is 12
                    h1("Small header"),
                    h2("Bigger Header"),
                    h3("Bigger Header"),
                    h4("Bigger Header"),
                    h5("Bigger Header"),
                    h6("Largest Header"),
                    p("smaller font"),
                    #The next line inserts a line between the narrative and the data by using css into application
                    #Not really scallable if the line is placed in the fluid row, refer to Tab Two Panel (Hide Narrative) for more elegant process
                    tags$hr(style = "border-top: 1px solid #ccc; margin-top: 20px; margin-bottom: 20px;")
                  ),
                  column(
                    width = 4, #fluid rows max value is 12
                    p("Refer to menuTwo R code to see why it is important to put the line below the narrative"),
                    #The next line inserts a line between the narrative and the data by using css into application
                    tags$hr(style = "border-top: 1px solid #ccc; margin-top: 20px; margin-bottom: 20px;")
                  ),
                  column(
                    width = 4, #fluid rows max value is 12
                    p("The line is not consistent due to it being applied on each column"),
                    #The next line inserts a line between the narrative and the data by using css into application
                    tags$hr(style = "border-top: 1px solid #ccc; margin-top: 20px; margin-bottom: 20px;")
                  )
                ),
                fluidRow(
                  h3("Another row")
                )
              ),
              tabPanel(
                #id= id is not required but if you want to reference the tabPanel this is where you would do it
                "Menu One Tab Panel (Will Not Hide Tab Panel)",
                fluidRow(
                  # Narrative section explaining the purpose of the dashboard
                  column(
                    width = 12,
                    h3(HTML("Using HTML to change font color from black to <font color=red>red</font> as well as <b>bolding</b> text.")),
                  )
                )
              )
            )
          ),
          tabItem(
            tabName = "tn_menuTwo", #This is where when the sidebar is selected it is mapped to the tab menu
            tabsetPanel(
              #id= id is not required but if we want to reference the tabPanel this is where you would do it
              tabPanel(
                "Menu Two Tab Panel (Hide Narrative)", #Tab Panel name
                fluidRow(
                  # Narrative section explaining the purpose of the dashboard
                  column(
                    width = 1, #fluid rows max value is 12
                    selectInput(
                      "tpID_parLocal_hideNarrative",
                      label= "This is a local Narratie",
                      choices= c("Local", "Options"),
                      selected= NULL,
                      multiple= FALSE
                    )
                  ),
                  column(
                    width = 11, #fluid rows max value is 12
                    id = "tpID_hideNarrative", #id is required if the column within the tab panel has to be referenced
                    h1("Wide Border")
                  )
                ),
                fluidRow(
                  #The next line inserts a line between the narrative and the data.
                  #It is added here instead of in the narrative since the narrative can have multiple columns. 
                  #If there are multiple narrative columns and the line is added there than the line consists of multiple breaks 
                  tags$hr(style = "border-top: 1px solid #ccc; margin-top: 20px; margin-bottom: 20px;"),
                  # For plots and different sections shiny uses the box function
                  box(h3("Another row")),
                  box(h3("Another row"))
                )
              ),
              #id= id is not required but if we want to reference the tabPanel this is where you would do it
              tabPanel(
                "Menu Two Tab Panel (Hide Narrative)", #Tab Panel name
                fluidRow(
                  # Narrative section explaining the purpose of the dashboard
                  column(
                    width = 1, #fluid rows max value is 12
                    selectInput(
                      "tpID_parLocal_WillNotHideNarrative", # This isn't tied to anything.
                      label= "This is a local Narratie",
                      choices= c("Local", "Options"),
                      selected= NULL,
                      multiple= FALSE
                    )
                  ),
                  column(
                    width = 11, #fluid rows max value is 12
                    id = "tpID_hideNarrative", #id is required if the column within the tab panel has to be referenced
                    h1("Wide Border")
                  )
                ),
                fluidRow(
                  #The next line inserts a line between the narrative and the data.
                  #It is added here instead of in the narrative since the narrative can have multiple columns. 
                  #If there are multiple narrative columns and the line is added there than the line consists of multiple breaks 
                  tags$hr(style = "border-top: 1px solid #ccc; margin-top: 20px; margin-bottom: 20px;"),
                  # For plots and different sections shiny uses the box function
                  box(h3("Another row")),
                  box(h3("Another row"))
                )
              ),
            )
          )
        )
      )
      
    )
  )
)

# To improve performance load items once and don't run them every time in the server. This is my understanding as of 4/4/2024  --------
# Although this is a simple example, it can be and should be applied to more complex calculations that occur only once



server <- function(input, output, session) {
  
  # When parGlobal_inputOne changes, this will return a non-restrictive list (User can still change options)
  # Use Case could be results shown by a region shouldn't just be based on the county selected
  # Utilized MS Copilot AI to get this to work 
  observeEvent(input$parGlobal_inputOne, {
    if(input$parGlobal_inputOne %in% df$var1) {
      # What the following is doing is taking the df id values and ordering them in the same order based on the selection
      updateSelectInput(
        session, "parGlobal_casscadeFromInputOneRestrictive",
        choices =  unique(df$id[order(df$var1 == input$parGlobal_inputOne, decreasing = TRUE)]),
        selected = unique(df$id[order(df$var1 == input$parGlobal_inputOne, decreasing = TRUE)])[1] #Default to the first choice, which will automatically change the cascading results when the parameter is mapped to a ggplot
      )
    }
    else {
      updateSelectInput(
        session, "parGlobal_casscadeFromInputOneRestrictive",
        choices = "",
        selected = ""
      )
    }
  }
  )
  
  # When parGlobal_inputOne changes, this will return a restrictive list
  # Worked with MS Copilot AI to get this to work 
  observeEvent(input$parGlobal_inputOne, {
    if(input$parGlobal_inputOne %in% df$var1) {
      
      #Reorder the ids based on selection 
      orderedallIds <- df$id[order(df$var1 == input$parGlobal_inputOne, decreasing = TRUE)]
      
      # Reorder the var1 based on selection
      orderedVar1 <- df$var1[order(df$var1 == input$parGlobal_inputOne, decreasing = TRUE)]
      
      #Combine var1 with id
      labeledOptions <-  paste0(orderedVar1, " (extra label because the label is included in orginal par (", orderedallIds, ") )")
      
      # Update the choices to represent a list of two combined parameters
      updateSelectInput(
        session, "parGlobal_casscadeFromInputOneNonRestrictive",
        choices = labeledOptions,
        selected = labeledOptions[1]
      )
    }
    else {
      updateSelectInput(
        session, "parGlobal_casscadeFromInputOneNonRestrictive",
        choices = "",
        selected = ""
      )
    }
  }
  )
  
  # Cascading prompt that only shows dependent result
  observeEvent(input$parGlobal_inputOne, {
    if(input$parGlobal_inputOne %in% df$var1) {
      updateSelectInput(
        session, "parGlobal_casscadeFromInputOneOneOption",
        choices = df$id[df$var1 == input$parGlobal_inputOne],
        selected = df$id[df$var1 == input$parGlobal_inputOne]
      )
    }
    else {
      updateSelectInput(
        session, "parGlobal_casscadeFromInputOneOneOption",
        choices = "",
        selected = ""
      )
    }
  }
  )
  
  # Hide Tab when checkbox is selected
  #https://stackoverflow.com/questions/60054418/shiny-tab-hide-show
  observeEvent(
    input$parGlobal_hideTabPanel, ignoreNULL = FALSE, ignoreInit = TRUE, {
      if(isTRUE(input$parGlobal_hideTabPanel)) 
        #inputId is the input parameter and target is the tab panel id
      {
        hideTab(inputId = "tspID_hideTabPanel" , target ="Menu One Tab Panel (Hide Tab Panel)")
      } 
      else  {
        showTab(inputId = "tspID_hideTabPanel" , target ="Menu One Tab Panel (Hide Tab Panel)")
      }
      
    }
  )
  
  # Observe the input value of the checkbox
  #shinyjs has a function called hide, show, and toggle. Toggle is suppose to work as hide and show but I couldn't get it to function correctly with a checkbox 
  #Js stands for javascript
  observeEvent(
    input$parGlobal_hideNarrative, ignoreNULL = FALSE, ignoreInit = TRUE, {
      if (isTRUE(input$parGlobal_hideNarrative)) {
        # Enable the commented-out portion
        shinyjs::hide("tpID_parLocal_hideNarrative")
        shinyjs::hide("tpID_hideNarrative")
      } 
      else {
        # Disable the portion when checkbox is unchecked
        shinyjs::show("tpID_parLocal_hideNarrative")
        shinyjs::show("tpID_hideNarrative")
      }
    }
  )
}

shinyApp(ui=ui, server=server)
