library(shiny)
library(DT)
library(tidyverse)
library(plotly)
library(here)
library(glue)
library(shinyWidgets)

# once you've prepared the data uncomment this line
tidy_fuels <- read_csv(here("data", "cooking.csv"))
# you might want to use highlight_key here
tidy_fuels$tooltip <- glue_data(tidy_fuels,
                                "Country: {country}",
                                "\nPopulation: {scales::comma(total_population, accuracy = 1)}",
                                "\nAccess Percentage: {scales::percent(cooking, scale = 1, accuracy = 1)}",
                                "\nGDP per capita: {scales::dollar(gdp_per_capita)}")

test <- tidy_fuels %>% mutate(gdp_per_capita = log10(gdp_per_capita))

ui <- fluidPage(
  setSliderColor("Red", 1),
  title = "Indoor Air Pollution",
  tabsetPanel(
    tabPanel("chart",
      icon = icon("line-chart"),
      fluidRow(
        column(
          2,
          checkboxInput("linear_scale",
            "Linearize x-axis",
            value = FALSE
          )
        ),
        column(
          6,
          offset = 1,
          # also possible to use plotly here
          selectizeInput("countries", "Select Countries",
            choices = tidy_fuels$country,
            multiple = TRUE
          )
        ),
        column(
          2,
          offset = 1,
          checkboxInput("small_countries",
            "Hide countries < 1 million",
            value = FALSE
          )
        )
      ),
      plotlyOutput("chart", width = "100%"),
      sliderInput("year",
        "Year",
        min = 2000,
        max = 2016,
        value = 2016,
        sep = "",
        width = "100%"
      )
    ),
    tabPanel("table", br(), dataTableOutput("table"), icon = icon("table")),
    tabPanel("about", icon = icon("question"),
             h1(tags$span(style="color:red; font-family: Impact, Charcoal, sans-serif", "Author")),
             h2("This app was created by", strong("Ibrahim Al-Hindi")),
             br(),
             h1(tags$span(style="color:red; font-family: Impact, Charcoal, sans-serif", "Chart Source")),
             h2("The source chart can be located", a(href = "https://ourworldindata.org/grapher/access-to-clean-fuels-for-cooking-vs-gdp-per-capita",
               "here")),
             br(),
             h1(tags$span(style="color:red; font-family: Impact, Charcoal, sans-serif", "Data Source")),
             h2("The data can be located", a(href = "https://datacatalog.worldbank.org/dataset/world-development-indicators",
                                             "here"))
             )
  ),
  includeCSS("styles.css")
)

server <- function(input, output, session) {
  # Define reactive expressions here for filtering data
  lin_x <- reactive({
    if(input$linear_scale) {
      country_select() +
        scale_x_continuous(labels = scales::label_dollar(),
                           breaks = seq(0, 100000, 20000))
    } else{
      country_select() +
        scale_x_log10(labels = scales::label_dollar(),
                      breaks = c(1000, 2000, 5000, 10000, 20000, 100000)) +
        scale_size_continuous(trans = "log10")
    }
  })

  hide_million <- reactive({
    if(input$small_countries){
      year_select() %>% filter(total_population >= 1e6)
    } else{
      year_select()
    }
  })

  year_select <- reactive({
    tidy_fuels %>% filter(year == input$year)
  })

  country_select <- reactive({
    df <- highlight_key(hide_million(), ~country)
    if(is.null(input$countries)){
      ggplot(df,
             aes(x = gdp_per_capita, y = cooking, color = continent, size = total_population, text = tooltip)) +
        geom_point(alpha = .5) +
        scale_y_continuous(labels = scales::label_percent(scale = 1)) +
        scale_color_brewer(palette = "Set2") +
        labs(x = "GDP per capita",
             y = "Access to clean fuels and technologies for cooking",
             color = "",
             size = "",
             title = "Access to clean fuels and technologies for cooking") +
        theme_bw() +
        geom_blank()
    } else{
      ggplot(df, aes(x = gdp_per_capita, y = cooking, size = total_population, alpha = .5, text = tooltip)) +
        geom_point(alpha = .5, color = "lightgrey") +
        geom_point(data = hide_million() %>% filter(country %in% input$countries), aes(color = continent), alpha = .5) +
        scale_y_continuous(labels = scales::label_percent(scale = 1)) +
        scale_color_brewer(palette = "Set2") +
        labs(x = "GDP per capita",
             y = "Access to clean fuels and technologies for cooking",
             color = "",
             size = "",
             title = "Access to clean fuels and technologies for cooking") +
        theme_bw() +
        geom_blank()
    }
  })

  country_select_table <- reactive({
    if(is.null(input$countries)){
      tidy_fuels
    } else{
      tidy_fuels %>% filter(country %in% input$countries)
    }
  })

  # Define outputs here
  output$chart <- renderPlotly({

    ggplotly(lin_x(), tooltip = "text") %>%
      config(displaylogo = FALSE,
             modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d", "zoom2d", "pan2d")) %>%
      highlight(on = "plotly_hover")
  })

  output$table <- renderDataTable({
    datatable(country_select_table() %>%
      select(-c(code, tooltip)) %>%
      mutate(cooking = scales::percent(cooking, scale = 1, accuracy = 1),
             gdp_per_capita = scales::comma(gdp_per_capita, accuracy = 1),
             total_population = scales::comma(total_population, accuracy = 1)) %>%
      rename("Access to Cooking Fuel and Tools Access (%)" = cooking,
             "GDP Per Capita ($)" = gdp_per_capita,
             "Total Population" = total_population) %>%
      rename_with(str_to_title) %>%
      arrange(-Year),
      rownames = FALSE,
      caption = "Access to clean fuels and technologies for cooking",
      options = list(
        columnDefs = list(list(className = 'dt-right', targets = 3:5)),
        pageLength = 15
      ),
      filter = 'top'
    )

  })

  output$text <- renderText({
    tags$h1("This shiny app was created by Ibrahim Al-Hindi")
  })
}

runApp(shinyApp(ui, server))
