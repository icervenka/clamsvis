selectInput(
  "select",
  h5("Select parameter"),
  choices = list(
    "VO2" = 1,
    "VCO2" = 2,
    "Heat" = 3
  ),
  selected = 1
),

selectInput(
  "select",
  h5("Time aggregation [min]"),
  choices = list("2" = 1, "4" = 2,
                 "6" = 3),
  selected = 1
),
checkboxGroupInput(
  "checkGroup",
  h5("Select subjects"),
  choices = list(
    "Choice 1" = 1,
    "Choice 2" = 2,
    "Choice 3" = 3
  ),
  selected = 1
),

