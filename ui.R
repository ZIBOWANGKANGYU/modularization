# The UI should have two parts: 
# (1) a dropdown menu, in the sidebar, that lets the user select a census subdivision,
# To simplify the app, we will only include census subdivisions with population greater than 50,000.
# (2) and a table, in the main panel, that shows the dwelling type data for the selected census subdivision.
fluidPage(
  
  # Application title
  titlePanel("Dwelling types in census subdivisions"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      selectizeInput("subdivision", "Census subdivision:",
                     choices = NULL
                     , selected = NULL
                     # Allow selection of up to 5 census subdivisions
                     , multiple = TRUE,
                     options = list(maxItems = 5)
      )
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      # Use the subdivision_dwellingsUI module to display the dwelling types of the selected census subdivision
      
      tagList(
        # Create a title for the module
        tags$h3("Dwelling types in the selected census subdivision"),
        # Create a table to display the dwelling types of the selected census subdivision
        dataTableOutput("table")
      ),
      
      # We want the first column of the table to be clickble. What function should we use?
      
      # Here, we will place the UI portion of a module, named subdivision_details, that we will create later. 
      # The purpose of this module is to show more details about the selected census subdivision in the table. 
      
      tagList(
        # Create a title for the module
        tags$h3("Demographic details of the selected census subdivision"),
        # (1) Table for demographic portrait
        tags$h4("Demographic portrait"),
        DTOutput("table_portrait"),
        # (2) Table for mother tongue
        tags$h4("Mother tongue"),
        DTOutput("table_mother_tongue"),
        # (3) Table for education
        tags$h4("Education"),
        DTOutput("table_education"),
        # (4) Table for immigration
        tags$h4("Immigration"),
        DTOutput("table_immigiration")
      )
    )
  )
)