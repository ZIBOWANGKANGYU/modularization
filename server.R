library(shiny)
library(DT)
# The purpose of this app is to let user see the dwelling type data 
# for each census subdivision in the data.
function(input, output, session) {
  
  # Read and prepare data
  data <- read.csv("data/data_cleaned.csv")
  data_details <- read.csv("data/data_details_wide.csv")
  
  subdivisions <- data %>%
    # Only pick the census subdivisions with population greater than 50,000
    filter(Population..2021 > 50000) %>%
    # Sort the census subdivisions from the highest population to the lowest
    arrange(- Population..2021) %>%
    pull(GEO_NAME)
  
  # Update selectizeInput. 
  # Its choices should be from the subdivisions variable.
  # By default, the first two subdivisions should be selected.
  
  updateSelectizeInput(session, "subdivision", choices = subdivisions, selected = subdivisions[1:2])
  
  # Create a reactive expression to filter the data based on user input
  shown_subdivisions <- reactive({
    data %>%
      filter(GEO_NAME %in% input$subdivision)
  })
  
  # In the selectizeInput function, we want to keep at least one census subdivision selected at all times.
  observeEvent(input$subdivision, {
    if (length(input$subdivision) == 0) {
      updateSelectizeInput(session, "subdivision", selected = subdivisions[1])
    }
  }, ignoreInit = TRUE, ignoreNULL = FALSE)
  
  # Call the module subdivision_dwellingsUI
  selected_subdivision <- reactive({
    row_id_selected <- input$table_rows_selected
    shown_subdivisions()[row_id_selected, "DGUID"]
  })
  
  dwelling_columns <-
    c(
      "Single.detached.house",
      "Semi.detached.house",
      "Row.house",
      "Apartment.or.flat.in.a.duplex",
      "Apartment.in.a.building.that.has.fewer.than.five.storeys",
      "Apartment.in.a.building.that.has.five.or.more.storeys"
    )
  # Use the code in server.R to create the data table
  
  output$table <- renderDataTable({
    data <- shown_subdivisions() %>%
      select(GEO_NAME, all_of(c("Total.private.dwellings", dwelling_columns))) %>%
      transform_to_proportions("Total.private.dwellings", remove_total = FALSE) %>%
      rename(
        Subdivision = GEO_NAME,
        `Total private dwellings` = Total.private.dwellings,
        `Single detached house` = Single.detached.house,
        `Semi detached house` = Semi.detached.house,
        `Row house` = Row.house,
        Duplex = Apartment.or.flat.in.a.duplex,
        `Apartment (less than 5 storeys)` = Apartment.in.a.building.that.has.fewer.than.five.storeys,
        `Apartment (5 or more storeys)` = Apartment.in.a.building.that.has.five.or.more.storeys
      )
    
    data %>%
      datatable(selection = list(mode = "single", selected = 1, target = "row"), options = list(
        initComplete = JS(
          change_font_size(11)
        )
      )) %>%
      DT_percentage_format(percentage_cols = colnames(data)[!colnames(data) %in% c("Subdivision", "Total private dwellings")])
  })
  
  
  # Here, we will place the server portion of a module, named subdivision_details, that we will create later. 
  # The purpose of this module is to show more details about the selected census subdivision in the table.
  # There is one parameter that we need to pass to the module: the selected census subdivision.

  filtered_data_portrait <- reactive({
    # This table contains age, household size and income data
    data_details %>%
      filter(DGUID %in% selected_subdivision()) %>%
      select(
        `Subdivision name` = GEO_NAME,
        `2021 Population` = `Population..2021`,
        `2016-21 Population Percentage Growth` = `Population.percentage.change..2016.to.2021`,
        `Average age` = Average.age.of.the.population,
        `Average household size` = Average.household.size,
        `Average income` = `Average.total.income.in.2020.among.recipients....`
      )
  })
  
  # Render the portrait able with the filtered data
  output$table_portrait <- renderDataTable({
    datatable(filtered_data_portrait(), options = list(
      initComplete = JS(
        change_font_size(11)
      )
    )) 
  })
  
  filtered_data_mother_tongue <- reactive({
    # This table contains mother tongue data
    data_details %>%
      filter(DGUID %in% selected_subdivision()) %>%
      transform_to_proportions("Population..2021") %>%
      select(`Subdivision name` = GEO_NAME, English, French, Arabic, Tagalog = `Tagalog..Pilipino..Filipino.`, Punjabi = `Punjabi..Panjabi.`, Urdu, Italian, Spanish, Mandarin, Cantonese = `Yue..Cantonese.`)
  })
  
  # Render the mother tongue table with the filtered data
  output$table_mother_tongue <- renderDataTable({
    datatable(filtered_data_mother_tongue(), options = list(
      initComplete = JS(
        change_font_size(11)
      )
    )) %>%
      DT_percentage_format()
  })
  
  filtered_data_education <- reactive({
    # This table contains education data
    data_details %>%
      filter(DGUID %in% selected_subdivision()) %>%
      transform_to_proportions("Population..2021") %>%
      select(`Subdivision name` = GEO_NAME, `Less than high school` = `No.high.school.diploma.or.equivalency.certificate`, `High school or more` = `With.high.school.diploma.or.equivalency.certificate`)
  })
  
  output$table_education <- renderDataTable({
    datatable(filtered_data_education(), options = list(
      initComplete = JS(
        change_font_size(11)
      )
    )) %>%
      DT_percentage_format()
  })
  
  filtered_data_immigration <- reactive({
    # This table contains immigration data
    data_details %>%
      filter(DGUID %in% selected_subdivision()) %>%
      transform_to_proportions("Population..2021") %>%
      select(`Subdivision name` = GEO_NAME, Immigrants = Immigrants)
  })
  
  output$table_immigiration <- renderDataTable({
    datatable(filtered_data_immigration(), options = list(
      initComplete = JS(
        change_font_size(11)
      )
    )) %>%
      DT_percentage_format()
  })
}