# =============================================================================
# Swedish Pharmacies 2024 - Shiny Application
# =============================================================================

library(shiny)
library(tidyverse)
library(leaflet)
library(sf)
library(shinyWidgets)
library(bslib)

# Load data --------------------------------------------------------------------
df_apotek <- readRDS("data/processed/df_apotek.rds")

# Load filter choices
filter_choices <- readRDS("data/processed/filter_choices.rds")
lan <- filter_choices$lan
kommungrupp <- filter_choices$kommungrupp
aktorer <- filter_choices$aktorer

# Load shape files for map polygons
lan_map <- st_read("data/processed/lan_map", quiet = TRUE)
kommun_map <- st_read("data/processed/kommun_map", quiet = TRUE)

# Colour palette for pharmacy operators (to be customised)
pharmacy_colours <- c("#ff0000", "#44ad2c", "#102d69", "lightblue", "#ff9e17")

# =============================================================================
# UI
# =============================================================================

ui <- navbarPage(
  title = "Swedish Pharmacies 2024",
  theme = bs_theme(
    version = 5,
    bg = "white",
    fg = "#003540",
    primary = "#005966",
    base_font = font_google("Inter"),
    heading_font = font_google("Overpass"),
    code_font = font_google("JetBrains Mono")
  ),

  # Map Tab ====================================================================
  tabPanel(
    "Map",
    sidebarLayout(
      # Sidebar ------------------------------------------------------------------
      sidebarPanel(
        width = 3,

        pickerInput(
          inputId = "select_indelning",
          label = tags$strong("Select county:"),
          choices = list(All = "Riket", County = lan),
          options = list(style = "background: white")
        ),

        pickerInput(
          "select_kommungrupp",
          tags$strong("Select municipality group:"),
          choices = kommungrupp,
          selected = "All",
          options = pickerOptions(
            dropdownAlignRight = TRUE,
            style = "background: white;"
          )
        ),

        pickerInput(
          inputId = "select_aktor",
          label = tags$strong("Select pharmacy operator:"),
          choices = aktorer,
          multiple = TRUE,
          selected = aktorer,
          options = pickerOptions(
            style = "background: white",
            `actions-box` = TRUE,
            selectAllText = "Select all",
            deselectAllText = "Deselect all"
          ),
          choicesOpt = list(
            style = paste0("background: ", pharmacy_colours, "; color: white;")
          )
        ),

        prettySwitch(
          inputId = "select_granser",
          label = tags$strong("National/county boundaries"),
          value = FALSE,
          fill = TRUE
        ),

        prettySwitch(
          inputId = "select_befolkningskarta",
          label = tags$strong("Municipality population"),
          value = FALSE,
          fill = TRUE
        ),

        numericRangeInput(
          "select_dist_apotek",
          label = tags$strong("Distance to nearest pharmacy (km):"),
          min = 0,
          max = ceiling(max(df_apotek$dist_km_narmaste_gln_korvag[is.finite(df_apotek$dist_km_narmaste_gln_korvag)], na.rm = TRUE)),
          separator = "to",
          value = c(0, ceiling(max(df_apotek$dist_km_narmaste_gln_korvag[is.finite(df_apotek$dist_km_narmaste_gln_korvag)], na.rm = TRUE)))
        ),

        sliderInput(
          "select_sarbarhet2",
          label = tags$strong("Vulnerability index:"),
          value = c(0, 100),
          min = 0,
          max = 100,
          sep = " ",
          ticks = FALSE
        ),

        downloadButton(
          outputId = "downloadKarta",
          label = "Download data",
          style = "width: 100%; margin-top: 10px;"
        ),

        tags$br()
      ),

      # Main Panel ---------------------------------------------------------------
      mainPanel(
        width = 9,
        leafletOutput("map", height = 650)
      )
    )
  ),

  # About Tab ==================================================================
  tabPanel(
    "About",
    fluidRow(
      column(
        width = 6,
        h3("Overview"),
        p("Interactive map visualizing Swedish pharmacy locations and accessibility metrics."),

        h4("Key Features:"),
        tags$ul(
          tags$li("Pharmacy locations across Sweden"),
          tags$li("Vulnerability index measuring closure impact"),
          tags$li("Population density by municipality"),
          tags$li("Filtering by county, operator, and distance")
        ),

        h4("Methodology:"),
        p("Vulnerability index = Population served × Increased travel distance if pharmacy closes"),
        p("Distances calculated using Vincenty ellipsoid (great-circle distance)."),

        h3("Data Sources"),

        h4("Pharmacy Locations"),
        tags$ul(
          tags$li(tags$a(href = "https://pipos.se/vara-tjanster/serviceanalys", "Pipos"), " (November 12, 2025)")
        ),

        h4("Population Data"),
        tags$ul(
          tags$li(
            tags$a(href = "https://www.scb.se/vara-tjanster/oppna-data/oppna-geodata/statistik-pa-rutor/",
                   "Statistics Sweden (SCB)"),
            " - 1km grid squares (2024)"
          )
        ),

        h4("Municipality Classifications"),
        tags$ul(
          tags$li("Statistics Sweden (SCB) (2023)")
        ),

        h4("Geographic Boundaries"),
        tags$ul(
          tags$li("swemaps2 R package")
        )
      ),

      column(
        width = 6,
        h3("Technical Details"),

        h4("Calculations:"),
        tags$ul(
          tags$li("Distance method: Vincenty ellipsoid formula"),
          tags$li("Map projection: WGS84 (EPSG:4326)"),
          tags$li("Analysis tools: R (sf, geosphere, leaflet)")
        ),

        h4("Data Processing:"),
        tags$ul(
          tags$li("Nearest neighbor analysis for each pharmacy"),
          tags$li("Population-weighted distance calculations"),
          tags$li("Vulnerability metrics based on closure impact")
        ),

        h3("About"),
        p("Portfolio project demonstrating geospatial analysis and interactive visualization."),

        h4("Technologies:"),
        tags$ul(
          tags$li("R Shiny"),
          tags$li("Leaflet.js mapping"),
          tags$li("Geospatial analysis (sf, geosphere)")
        ),

        tags$em("Last updated: November 2025")
      )
    )
  )
)

# =============================================================================
# Server
# =============================================================================

server <- function(input, output, session) {

  # Reactive: Filter pharmacies based on input selections =====================
  valda_apotek <- reactive({

    filtered_data <- df_apotek %>%
      filter(
        aktor %in% input$select_aktor,
        percentil_sarbarhet >= input$select_sarbarhet2[1] & percentil_sarbarhet <= input$select_sarbarhet2[2],
        dist_km_narmaste_gln_korvag >= input$select_dist_apotek[1] & dist_km_narmaste_gln_korvag <= input$select_dist_apotek[2]
      )

    if (input$select_indelning %in% lan) {
      filtered_data <- filtered_data %>% filter(lan == input$select_indelning)
    }

    if (input$select_kommungrupp != "All") {
      filtered_data <- filtered_data %>% filter(kommungrupp == input$select_kommungrupp)
    }

    return(filtered_data)
  })

  # Reactive: County and national boundaries based on region selection ========
  lanskarta <- reactive({
    if (input$select_indelning == "Riket") {
      map <- lan_map %>%
        summarise(geometry = st_union(geometry))
    } else if (input$select_indelning %in% lan) {
      map <- lan_map %>%
        filter(lan == input$select_indelning)
    }

    return(map)
  })

  # Reactive: Population map based on municipality population =================
  befolkningskarta <- reactive({
    if (input$select_indelning == "Riket") {
      map <- kommun_map
    } else if (input$select_indelning %in% lan) {
      map <- kommun_map %>%
        filter(lan == input$select_indelning)
    }

    return(map)
  })

  # Download handler: Selected pharmacies =====================================
  output$downloadKarta <- downloadHandler(
    filename = function() {
      str_glue("apotek_karta_{Sys.Date()}.xlsx")
    },
    content = function(file) {
      writexl::write_xlsx(valda_apotek() %>%
                            select(-c(starts_with("kvartil_"))), file)
    }
  )

  # Create palettes for pharmacies and population =============================
  pal_apotek <- colorFactor(palette = pharmacy_colours, levels = aktorer)
  bins <- c(0, 10e3, 20e3, 50e3, 100e3, 200e3, Inf)
  pal_pop <- colorBin("YlOrRd", domain = kommun_map$pop, bins = bins)

  # Render initial leaflet map ================================================
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addScaleBar(
        position = "bottomleft",
        options = scaleBarOptions(imperial = FALSE)
      ) %>%
      setView(lat = 62.17432013788275, lng = 14.946378696886965, zoom = 5)
  })

  # Update map: National/county boundaries ====================================
  observeEvent(
    eventExpr = {
      input$select_granser
      input$select_indelning
      input$select_kommungrupp
    },
    handlerExpr = {

      leafletProxy("map") %>%
        clearGroup("granser")

      if (input$select_granser == TRUE) {
        leafletProxy("map") %>%
          addPolygons(
            data = lanskarta(),
            fill = FALSE,
            color = "black",
            weight = 2,
            group = "granser"
          )
      }
    }
  )

  # Update map: Municipality population =======================================
  observeEvent(
    eventExpr = {
      input$select_befolkningskarta
      input$select_indelning
    },
    handlerExpr = {

      leafletProxy("map") %>%
        clearGroup("befolkningskarta")

      if (input$select_befolkningskarta == TRUE) {
        leafletProxy("map") %>%
          addPolygons(
            data = befolkningskarta(),
            fillColor = ~pal_pop(pop),
            popup = ~paste0(
              str_glue("<strong>{kommun}</strong>"), "<br>",
              str_glue("{format(pop, big.mark = ',')} inhabitants")
            ),
            color = "black",
            fillOpacity = 0.5,
            weight = 0.5,
            group = "befolkningskarta"
          )
      }
    }
  )

  # Update map: Pharmacy markers ==============================================
  observeEvent(
    eventExpr = {
      input$select_aktor
      input$select_indelning
      input$select_kommungrupp
      input$select_sarbarhet2
      input$select_dist_apotek
    },
    handlerExpr = {

      leafletProxy("map") %>%
        clearGroup("apotek") %>%
        removeControl("legendApotek")

      leafletProxy("map") %>%
        addCircles(
          data = valda_apotek(),
          radius = 10,
          color = ~pal_apotek(aktor),
          popup = ~paste0(
            str_glue("<strong>{namn}</strong>"), "<br>",
            str_glue("- <strong>Distance to nearest pharmacy:</strong> {round(dist_km_narmaste_gln_korvag, 1)} km"), "<br>",
            str_glue("- <strong>Vulnerability index:</strong> {percentil_sarbarhet}"), "<br>",
            str_glue("- <strong>Extra person-kilometres:</strong> {format(round(personkilometer_skillnad), big.mark = ',')}")
          ),
          opacity = 1,
          fill = "black",
          fillOpacity = 1,
          weight = 8,
          group = "apotek"
        ) %>%
        addLegend(
          data = valda_apotek(),
          pal = pal_apotek,
          values = ~aktor,
          title = "Pharmacy operator",
          opacity = 1,
          position = "bottomright",
          layerId = "legendApotek"
        )
    }
  )
}

# =============================================================================
# Run the application
# =============================================================================

shinyApp(ui = ui, server = server)
