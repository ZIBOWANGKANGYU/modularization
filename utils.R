# A function that transforms counts in a data frame to proportions.
# This function will be used in the following reactive expressions:
# - filtered_data_mother_tongue
# - filtered_data_education
# - filtered_data_immigration
# The function should have two parameters:
# - data: A data frame with counts. One of the columns in the data frame is the total count.
# - total_col: The name of the column that contains the total count.
# - remove_total: A logical value indicating whether the total count column should be removed from the output data frame. The default value is TRUE.
# The function only calculates the proportions for columns that are numeric: either integer or double.
# If a column is not numeric, it should be left as is.
# The function should return a data frame with the same columns as the input data frame, but with the counts transformed to proportions.
# The total count column should be removed if remove_total is TRUE.

transform_to_proportions <- function(data, total_col, remove_total = TRUE) {
  # Check that data is a data frame
  stopifnot(is.data.frame(data))
  # Check that total_col is a single string
  stopifnot(is.character(total_col) && length(total_col) == 1)
  # Check that total_col is a column in data
  stopifnot(total_col %in% colnames(data))
  
  data <- data %>%
    mutate(across(c(where(is.numeric), -all_of(total_col)), ~ . / !!sym(total_col)))
  
  if(remove_total) {
    data <- data %>%
      select(-all_of(total_col))
  }
  
  return(data)
}

# In the app, we use the datatable package to display the data frames.
# Some columns are numeric and should be displayed as percentages.
# We can use the formatPercentage function from the DT package to format these columns.
# Here, create a function that uses the formatPercentage function to format the DT table. 
# The function should have three parameters:
# - DT_object: A DT table to add formatting to.
# - percentage_cols: A character vector with the names of the columns to format as percentages.
# - decimal_places: The number of decimal places to display for the percentages. The default value is 0.
# By default, the function should format all numeric columns as percentages.
# The function should return a DT table with the specified columns formatted as percentages.
DT_percentage_format <- function(DT_object, percentage_cols = NULL, decimal_places = 0) {
  
  # Stop if DT_object is not a DT object
  stopifnot(inherits(DT_object, "datatables"))
  
  if(is.null(percentage_cols)) {
    percentage_cols <- DT_object$x$data %>%
      select(where(is.numeric)) %>%
      colnames()
  }
  
  DT_object <- DT_object %>%
    formatPercentage(percentage_cols, digits = decimal_places)
  
  return(DT_object)
}

# Create a JS function that generates jQuery code to change the font size of all elements in a DT table.
# The function should have one parameter:
# - font_size: The font size to set for the elements in the DT table.
# The function should return a character string with the jQuery code to change the font size.
# The returned function should be used in the initComplete option of the datatable function.
# "function(settings, json) {"
# "$(this.api().table().container()).css('font-size', '10px');"
# "}"
change_font_size <- function(font_size) {
  # Check that font_size is a single numeric value
  stopifnot(is.numeric(font_size) && length(font_size) == 1)
  
return(
  paste0(
    "function(settings, json) {",
    "$(this.api().table().container()).css('font-size', '", font_size, "px');",
    "}"
  )
)}