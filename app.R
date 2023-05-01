#
# Dashboard for "Ciudad Verde - Corredores biológicos"
#


# PACKAGES
library(dplyr)
library(ggplot2)
library(plotly)
library(sf)
library(leaflet)
library(leaflet.extras)
library(leafem)
library(shiny)
library(shinythemes)
library(shinycssloaders)
library(colorspace)


# FUNCTIONS

# Get labels for quartiles
get_quartiles_labels <- function(x, unit) {
  # Returns a vector with the labels of the quartiles ranges
  
  r1 <- paste0(as.character(round(x[1], 1)), " - ", as.character(round(x[2], 1)), " ", unit)
  r2 <- paste0(as.character(round(x[2], 1)), " - ", as.character(round(x[3], 1)), " ", unit)
  r3 <- paste0(as.character(round(x[3], 1)), " - ", as.character(round(x[4], 1)), " ", unit)
  r4 <- paste0(as.character(round(x[4], 1)), " - ", as.character(round(x[5], 1)), " ", unit)
  
  return(c(r1, r2, r3, r4))
}

# Create map
create_map <-
  function(indicator_column, indicator_group, indicator_legend_title, indicator_legend_labels, indicator_fillColor, indicator_palette, indicator_unit) {
    leaflet() |>
      addProviderTiles(providers$OpenStreetMap.Mapnik, group = "Mapa de calles (OpenStreetMap)") |>
      addProviderTiles(providers$CartoDB.DarkMatter, group = "Mapa oscuro (CartoDB Dark Matter)") |>
      addProviderTiles(providers$Stamen.TonerLite, group = "Mapa claro (Stamen Toner Lite)") |>
      addProviderTiles(providers$Esri.WorldImagery, group = "Imágenes satelitales (ESRI World Imagery)") |>
      addPolygons(
        data = corridors,
        fillOpacity = ifelse(is.na(indicator_column), 0, 0.7),
        stroke = TRUE,
        color = "Black",
        fillColor = indicator_fillColor,
        weight = 1,
        popup = paste(
          paste("<strong>Corredor biológico:</strong>",  corridors[[COLUMN_CORRIDOR_NAME]]),
          paste(
            paste0("<strong>", indicator_group, ":</strong>"),
            paste0(round(indicator_column, 1), " ", indicator_unit)
          ),
          sep = '<br/>'
        ),
        label = paste(
          paste("Corredor biológico:",  corridors[[COLUMN_CORRIDOR_NAME]]),
          paste(paste0(indicator_group, ":"), round(indicator_column, 1), indicator_unit),
          sep = ' - '
        ),
        group = indicator_group
      ) |>
      addLegend(
        position = "bottomright",
        pal = indicator_palette,
        values = indicator_column,
        labFormat = function(type, cuts, p) {
          paste0(indicator_legend_labels)
        },
        group = indicator_group,
        title = indicator_legend_title
      ) |>
      addLayersControl(
        baseGroups = c(
          "Mapa de calles (OpenStreetMap)",
          "Mapa oscuro (CartoDB Dark Matter)",
          "Mapa claro (Stamen Toner Lite)",
          "Imágenes satelitales (ESRI World Imagery)"
        ),
        overlayGroups = c(indicator_group),
        options = layersControlOptions(collapsed = FALSE)
      ) |>
      addScaleBar(position = "bottomleft", options = scaleBarOptions(imperial = FALSE)) |>
      addMouseCoordinates() |>
      addSearchOSM() |>
      addResetMapButton() |>
      addFullscreenControl()
  }

# Create biological connectivity map
create_map_connectivity <-
  function() {
    leaflet() |>
      addProviderTiles(providers$OpenStreetMap.Mapnik, group = "Mapa de calles (OpenStreetMap)") |>
      addProviderTiles(providers$CartoDB.DarkMatter, group = "Mapa oscuro (CartoDB Dark Matter)") |>
      addProviderTiles(providers$Stamen.TonerLite, group = "Mapa claro (Stamen Toner Lite)") |>
      addProviderTiles(providers$Esri.WorldImagery, group = "Imágenes satelitales (ESRI World Imagery)") |>
      addWMSTiles(
        WMS_CATIE_URL,
        layers = WMS_PROBABILITY_BIOLOGICAL_CONNECTIVITY_LAYER,
        options = WMSTileOptions(
          format = "image/png",
          transparent = TRUE,
          crs = "EPSG:4326"
        ),
        group = INDICATOR_PROBABILITY_BIOLOGICAL_CONNECTIVITY
      ) |>
      addWMSLegend(
        layerId = "legend_probability_biological_connectivity",
        position = "topright",
        uri = paste0(
          WMS_CATIE_URL, "?",
          "REQUEST=GetLegendGraphic&VERSION=1.0.0", 
          "&FORMAT=image/png&WIDTH=20&HEIGHT=15&LAYER=probabilidad-conectividad-biologica"
        )
      ) |>
      addPolygons(
        data = corridors,
        layerId = ~nombre_cb,
        fillOpacity = 0.0,
        stroke = TRUE,
        color = "red",
        weight = 3,
        popup = paste(
          paste("<strong>Corredor biológico:</strong>",  corridors[[COLUMN_CORRIDOR_NAME]])
        ),
        label = paste(
          paste("Corredor biológico:",  corridors[[COLUMN_CORRIDOR_NAME]])
        ),
        group = "Corredores biológicos"
      ) |>      
      addLayersControl(
        baseGroups = c(
          "Mapa de calles (OpenStreetMap)",
          "Mapa oscuro (CartoDB Dark Matter)",
          "Mapa claro (Stamen Toner Lite)",
          "Imágenes satelitales (ESRI World Imagery)"
        ),
        overlayGroups = c(
          INDICATOR_PROBABILITY_BIOLOGICAL_CONNECTIVITY,
          "Corredores biológicos"
        ),
        position = "bottomleft",
        options = layersControlOptions(collapsed = FALSE)
      ) |>      
      addScaleBar(position = "bottomleft", options = scaleBarOptions(imperial = FALSE)) |>
      addMouseCoordinates() |>
      addSearchOSM() |>
      addResetMapButton() |>
      addFullscreenControl()
  }

# Create barplot
create_barplot <-
  function(indicator_column, indicator_geom_col_label, indicator_y_axis_label, indicator_geom_col_fill, indicator_unit) {
    # Ggplot2 plot
    barplot_recreation_ggplot2 <-
      corridors |>
      ggplot(aes(x = reorder(
        !!sym(COLUMN_CORRIDOR_NAME), -indicator_column
      ), y = indicator_column)) +
      geom_col(aes(
        text = paste0(
          "Corredor biológico: ",
          !!sym(COLUMN_CORRIDOR_NAME),
          "\n",
          indicator_geom_col_label,
          ": ",
          round(indicator_column, 1),
          " ",
          indicator_unit
        )
      ),
      fill = indicator_geom_col_fill) +
      xlab("Corredor biológico") +
      ylab(indicator_y_axis_label) +
      theme_classic() +
      theme(
        axis.text.x = element_text(
          angle = 50,
          vjust = 1,
          hjust = 1
        ),
        legend.position = "none"
      )
    
    # Plotly plot
    barplot_recreation_ggplot2 |>
      ggplotly(tooltip = "text") |>
      config(locale = 'es')    
  }


# DATASETS

# Data sources

# Corridors
DSN_CORRIDORS <- "data/metricas-corredores.geojson"

corridors <- st_read(dsn = DSN_CORRIDORS, quiet = TRUE)

WMS_CATIE_URL <- "https://catie.info/geoserver/atlasverde/wms"
WMS_PROBABILITY_BIOLOGICAL_CONNECTIVITY_LAYER <- "probabilidad-conectividad-biologica"

# Data cleaning


# Data columns
COLUMN_CORRIDOR_NAME <- "nombre_cb"

COLUMN_WILDPROTECTEDAREAS_PERCENTAGE <- corridors$PORC_ASP
# COLUMN_GREENBLUEINFRAESTRUCTURE_PERCENTAGE <- corridors$porc_tv
COLUMN_RIVERSIDEVEGETATION_PERCENTAGE <- corridors$propor_veg

COLUMN_CARBON_STOCK <- corridors$STOCK_C
COLUMN_CARBON_DENSITY <- corridors$DENS_C
COLUMN_SOILBIOTICINDEX <- corridors$IBS
COLUMN_LANDSURFACE_TEMPERATURE <- corridors$LST_MEAN

COLUMN_POPULATIONINTERNETACCESS_PERCENTAGE <- corridors$porc_inter

COLUMN_GREENAREA_AREA <- corridors$sup_hab
# COLUMN_TRAILS_LENGTH <- corridors$km_sendero

COLUMN_POPULATIONINFORMALSETTLEMENTS_PERCENTAGE <- corridors$porc_v_inf


# CONSTANTS

# Indicators
INDICATOR_WILDPROTECTEDAREAS_PERCENTAGE <- "Porcentaje de áreas de protección natural"
# INDICATOR_GREENBLUEINFRAESTRUCTURE_PERCENTAGE <- "Porcentaje de infraestructura verde y azul"
INDICATOR_RIVERSIDEVEGETATION_PERCENTAGE <- "Porcentaje de área ribereña cubierta por vegetación"
INDICATOR_PROBABILITY_BIOLOGICAL_CONNECTIVITY <- "Probabilidad de conectividad biológica"

INDICATOR_CARBON_STOCK <- "Almacenamiento y secuestro de carbono"
INDICATOR_CARBON_DENSITY <- "Densidad de almacenamiento de carbono"
INDICATOR_SOILBIOTICINDEX <- "Índice biótico del suelo"
INDICATOR_LANDSURFACE_TEMPERATURE <- "Temperatura superficial promedio de la tierra"

INDICATOR_POPULATIONINTERNETACCESS_PERCENTAGE <- "Porcentaje de población con conexión a Internet"

INDICATOR_GREENAREA_AREA <- "Superficie verde por habitante"
# INDICATOR_TRAILS_LENGTH <- "Longitud de senderos para caminar per cápita"

INDICATOR_POPULATIONINFORMALSETTLEMENTS_PERCENTAGE <- "Porcentaje de población que vive en asentamientos informales o viviendas inadecuadas"

# Units of measurement
UNIT_WILDPROTECTEDAREAS_PERCENTAGE <- "%"
# UNIT_GREENBLUEINFRAESTRUCTURE_PERCENTAGE <- "%"
UNIT_RIVERSIDEVEGETATION_PERCENTAGE <- "%"

UNIT_CARBON_STOCK <- "Mg"
UNIT_CARBON_DENSITY <- "Mg/ha"
UNIT_SOILBIOTICINDEX <- ""
UNIT_LANDSURFACE_TEMPERATURE <- "°C"

UNIT_POPULATIONINTERNETACCESS_PERCENTAGE <- "%"

UNIT_GREENAREA_AREA <- "m2/hab"
# UNIT_TRAILS_LENGTH <- "km/10000 hab"

UNIT_POPULATIONINFORMALSETTLEMENTS_PERCENTAGE <- "%"

# To avoid error: 'breaks' are not unique
QUANTILES_WILDPROTECTEDAREAS_PERCENTAGE_VALUES <- c(0, 13.3, 17.4)
QUANTILES_WILDPROTECTEDAREAS_PERCENTAGE_LABELS <- c("0 - 13.3 %", "13.3 - 17.4 %")

# QUANTILES_GREENBLUEINFRAESTRUCTURE_PERCENTAGE_VALUES <- fivenum(COLUMN_GREENBLUEINFRAESTRUCTURE_PERCENTAGE)
# QUANTILES_GREENBLUEINFRAESTRUCTURE_PERCENTAGE_LABELS <- get_quartiles_labels(QUANTILES_GREENBLUEINFRAESTRUCTURE_PERCENTAGE_VALUES, UNIT_GREENBLUEINFRAESTRUCTURE_PERCENTAGE)

QUANTILES_RIVERSIDEVEGETATION_PERCENTAGE_VALUES <- fivenum(COLUMN_RIVERSIDEVEGETATION_PERCENTAGE)
QUANTILES_RIVERSIDEVEGETATION_PERCENTAGE_LABELS <- get_quartiles_labels(QUANTILES_RIVERSIDEVEGETATION_PERCENTAGE_VALUES, UNIT_RIVERSIDEVEGETATION_PERCENTAGE)

QUANTILES_CARBON_STOCK_VALUES <- fivenum(COLUMN_CARBON_STOCK)
QUANTILES_CARBON_STOCK_LABELS <- get_quartiles_labels(QUANTILES_CARBON_STOCK_VALUES, UNIT_CARBON_STOCK)

QUANTILES_CARBON_DENSITY_VALUES <- fivenum(COLUMN_CARBON_DENSITY)
QUANTILES_CARBON_DENSITY_LABELS <- get_quartiles_labels(QUANTILES_CARBON_DENSITY_VALUES, UNIT_CARBON_DENSITY)

QUANTILES_SOILBIOTICINDEX_VALUES <- fivenum(COLUMN_SOILBIOTICINDEX)
QUANTILES_SOILBIOTICINDEX_LABELS <- get_quartiles_labels(QUANTILES_SOILBIOTICINDEX_VALUES, UNIT_SOILBIOTICINDEX)

QUANTILES_LANDSURFACE_TEMPERATURE_VALUES <- fivenum(COLUMN_LANDSURFACE_TEMPERATURE)
QUANTILES_LANDSURFACE_TEMPERATURE_LABELS <- get_quartiles_labels(QUANTILES_LANDSURFACE_TEMPERATURE_VALUES, UNIT_LANDSURFACE_TEMPERATURE)

QUANTILES_POPULATIONINTERNETACCESS_PERCENTAGE_VALUES <- fivenum(COLUMN_POPULATIONINTERNETACCESS_PERCENTAGE)
QUANTILES_POPULATIONINTERNETACCESS_PERCENTAGE_LABELS <- get_quartiles_labels(QUANTILES_POPULATIONINTERNETACCESS_PERCENTAGE_VALUES, UNIT_POPULATIONINTERNETACCESS_PERCENTAGE)

QUANTILES_GREENAREA_AREA_VALUES <- fivenum(COLUMN_GREENAREA_AREA)
QUANTILES_GREENAREA_AREA_LABELS <- get_quartiles_labels(QUANTILES_GREENAREA_AREA_VALUES, UNIT_GREENAREA_AREA)

# QUANTILES_TRAILS_LENGTH_VALUES <- fivenum(COLUMN_TRAILS_LENGTH)
# QUANTILES_TRAILS_LENGTH_LABELS <- get_quartiles_labels(QUANTILES_TRAILS_LENGTH_VALUES, UNIT_TRAILS_LENGTH)

# To avoid error: 'breaks' are not unique
QUANTILES_POPULATIONINFORMALSETTLEMENTS_PERCENTAGE_VALUES <- c(0, 0.37, 3.71, 20.02)
QUANTILES_POPULATIONINFORMALSETTLEMENTS_PERCENTAGE_LABELS <- c("0 - 0.37 %", "0.37 - 3.71 %", "3.71 - 20.02 %")

# Color palettes
PALETTE_WILDPROTECTEDAREAS_PERCENTAGE_COLOR <- rgb(60, 170, 0, maxColorValue = 255) # CORINE CR - Bosque secundario
PALETTE_WILDPROTECTEDAREAS_PERCENTAGE_START_COLOR <- lighten(PALETTE_WILDPROTECTEDAREAS_PERCENTAGE_COLOR, 0.4)
PALETTE_WILDPROTECTEDAREAS_PERCENTAGE_END_COLOR <- darken(PALETTE_WILDPROTECTEDAREAS_PERCENTAGE_COLOR, 0.4)
PALETTE_WILDPROTECTEDAREAS_PERCENTAGE <- colorBin(
  bins = QUANTILES_WILDPROTECTEDAREAS_PERCENTAGE_VALUES,
  palette = c(PALETTE_WILDPROTECTEDAREAS_PERCENTAGE_START_COLOR, PALETTE_WILDPROTECTEDAREAS_PERCENTAGE_END_COLOR),
  na.color = NA
)
# PALETTE_GREENBLUEINFRAESTRUCTURE_PERCENTAGE_COLOR <- rgb(0, 255, 255, maxColorValue = 255) # Cian
# PALETTE_GREENBLUEINFRAESTRUCTURE_PERCENTAGE_START_COLOR <- lighten(PALETTE_GREENBLUEINFRAESTRUCTURE_PERCENTAGE_COLOR, 0.4)
# PALETTE_GREENBLUEINFRAESTRUCTURE_PERCENTAGE_END_COLOR <- darken(PALETTE_GREENBLUEINFRAESTRUCTURE_PERCENTAGE_COLOR, 0.4)
# PALETTE_GREENBLUEINFRAESTRUCTURE_PERCENTAGE <- colorBin(
#   bins = QUANTILES_GREENBLUEINFRAESTRUCTURE_PERCENTAGE_VALUES,
#   palette = c(PALETTE_GREENBLUEINFRAESTRUCTURE_PERCENTAGE_START_COLOR, PALETTE_GREENBLUEINFRAESTRUCTURE_PERCENTAGE_END_COLOR),
#   na.color = NA
# )
PALETTE_RIVERSIDEVEGETATION_PERCENTAGE_COLOR <- rgb(167,167, 255, maxColorValue = 255) # CORINE CR - Bosque de galería
PALETTE_RIVERSIDEVEGETATION_PERCENTAGE_START_COLOR <- lighten(PALETTE_RIVERSIDEVEGETATION_PERCENTAGE_COLOR, 0.4)
PALETTE_RIVERSIDEVEGETATION_PERCENTAGE_END_COLOR <- darken(PALETTE_RIVERSIDEVEGETATION_PERCENTAGE_COLOR, 0.4)
PALETTE_RIVERSIDEVEGETATION_PERCENTAGE <- colorBin(
  bins = QUANTILES_RIVERSIDEVEGETATION_PERCENTAGE_VALUES,
  palette = c(PALETTE_RIVERSIDEVEGETATION_PERCENTAGE_START_COLOR, PALETTE_RIVERSIDEVEGETATION_PERCENTAGE_END_COLOR),
  na.color = NA
)

PALETTE_CARBON_STOCK_COLOR <- rgb(143, 0, 255, maxColorValue = 255) # Violet
PALETTE_CARBON_STOCK_START_COLOR <- lighten(PALETTE_CARBON_STOCK_COLOR, 0.4)
PALETTE_CARBON_STOCK_END_COLOR <- darken(PALETTE_CARBON_STOCK_COLOR, 0.4)
PALETTE_CARBON_STOCK <- colorBin(
  bins = QUANTILES_CARBON_STOCK_VALUES,
  palette = c(PALETTE_CARBON_STOCK_START_COLOR, PALETTE_CARBON_STOCK_END_COLOR),
  na.color = NA
)
PALETTE_CARBON_DENSITY_COLOR <- rgb(128, 0, 128, maxColorValue = 255) # Purple
PALETTE_CARBON_DENSITY_START_COLOR <- lighten(PALETTE_CARBON_DENSITY_COLOR, 0.4)
PALETTE_CARBON_DENSITY_END_COLOR <- darken(PALETTE_CARBON_DENSITY_COLOR, 0.4)
PALETTE_CARBON_DENSITY <- colorBin(
  bins = QUANTILES_CARBON_DENSITY_VALUES,
  palette = c(PALETTE_CARBON_DENSITY_START_COLOR, PALETTE_CARBON_DENSITY_END_COLOR),
  na.color = NA
)
PALETTE_SOILBIOTICINDEX_COLOR <- rgb(160, 82, 45, maxColorValue = 255) # Sienna
PALETTE_SOILBIOTICINDEX_START_COLOR <- lighten(PALETTE_SOILBIOTICINDEX_COLOR, 0.4)
PALETTE_SOILBIOTICINDEX_END_COLOR <- darken(PALETTE_SOILBIOTICINDEX_COLOR, 0.4)
PALETTE_SOILBIOTICINDEX <- colorBin(
  bins = QUANTILES_SOILBIOTICINDEX_VALUES,
  palette = c(PALETTE_SOILBIOTICINDEX_START_COLOR, PALETTE_SOILBIOTICINDEX_END_COLOR),
  na.color = NA
)
PALETTE_LANDSURFACE_TEMPERATURE_COLOR <- rgb(255, 69, 0, maxColorValue = 255) # Orange Red
PALETTE_LANDSURFACE_TEMPERATURE_START_COLOR <- lighten(PALETTE_LANDSURFACE_TEMPERATURE_COLOR, 0.4)
PALETTE_LANDSURFACE_TEMPERATURE_END_COLOR <- darken(PALETTE_LANDSURFACE_TEMPERATURE_COLOR, 0.4)
PALETTE_LANDSURFACE_TEMPERATURE <- colorBin(
  bins = QUANTILES_LANDSURFACE_TEMPERATURE_VALUES,
  palette = c(PALETTE_LANDSURFACE_TEMPERATURE_START_COLOR, PALETTE_LANDSURFACE_TEMPERATURE_END_COLOR),
  na.color = NA
)

PALETTE_POPULATIONINTERNETACCESS_PERCENTAGE_COLOR <- rgb(75, 0, 130, maxColorValue = 255) # Indigo
PALETTE_POPULATIONINTERNETACCESS_PERCENTAGE_START_COLOR <- lighten(PALETTE_POPULATIONINTERNETACCESS_PERCENTAGE_COLOR, 0.4)
PALETTE_POPULATIONINTERNETACCESS_PERCENTAGE_END_COLOR <- darken(PALETTE_POPULATIONINTERNETACCESS_PERCENTAGE_COLOR, 0.4)
PALETTE_POPULATIONINTERNETACCESS_PERCENTAGE <- colorBin(
  bins = QUANTILES_POPULATIONINTERNETACCESS_PERCENTAGE_VALUES,
  palette = c(PALETTE_POPULATIONINTERNETACCESS_PERCENTAGE_START_COLOR, PALETTE_POPULATIONINTERNETACCESS_PERCENTAGE_END_COLOR),
  na.color = NA
)

PALETTE_GREENAREA_AREA_COLOR <- rgb(175, 255, 175, maxColorValue = 255) # CORINE CR - Zonas verdes urbanas
PALETTE_GREENAREA_AREA_START_COLOR <- lighten(PALETTE_GREENAREA_AREA_COLOR, 0.4)
PALETTE_GREENAREA_AREA_END_COLOR <- darken(PALETTE_GREENAREA_AREA_COLOR, 0.4)
PALETTE_GREENAREA_AREA <- colorBin(
  bins = QUANTILES_GREENAREA_AREA_VALUES,
  palette = c(PALETTE_GREENAREA_AREA_START_COLOR, PALETTE_GREENAREA_AREA_END_COLOR),
  na.color = NA
)
# PALETTE_TRAILS_LENGTH_COLOR <- rgb(248, 0, 0, maxColorValue = 255) # CORINE CR - Red vial
# PALETTE_TRAILS_LENGTH_START_COLOR <- lighten(PALETTE_TRAILS_LENGTH_COLOR, 0.4)
# PALETTE_TRAILS_LENGTH_END_COLOR <- darken(PALETTE_TRAILS_LENGTH_COLOR, 0.4)
# PALETTE_TRAILS_LENGTH <- colorBin(
#   bins = QUANTILES_TRAILS_LENGTH_VALUES,
#   palette = c(
#     PALETTE_TRAILS_LENGTH_START_COLOR,
#     PALETTE_TRAILS_LENGTH_END_COLOR
#   ),
#   na.color = NA
# )

PALETTE_POPULATIONINFORMALSETTLEMENTS_PERCENTAGE_COLOR <- rgb(112, 128, 144, maxColorValue = 255) # Slate gray
PALETTE_POPULATIONINFORMALSETTLEMENTS_PERCENTAGE_START_COLOR <- lighten(PALETTE_POPULATIONINFORMALSETTLEMENTS_PERCENTAGE_COLOR, 0.4)
PALETTE_POPULATIONINFORMALSETTLEMENTS_PERCENTAGE_END_COLOR <- darken(PALETTE_POPULATIONINFORMALSETTLEMENTS_PERCENTAGE_COLOR, 0.4)
PALETTE_POPULATIONINFORMALSETTLEMENTS_PERCENTAGE <- colorBin(
  bins = QUANTILES_POPULATIONINFORMALSETTLEMENTS_PERCENTAGE_VALUES,
  palette = c(
    PALETTE_POPULATIONINFORMALSETTLEMENTS_PERCENTAGE_START_COLOR,
    PALETTE_POPULATIONINFORMALSETTLEMENTS_PERCENTAGE_END_COLOR
  ),
  na.color = NA
)


# USER INTERFACE
ui <- fluidPage(
  
  theme = "bootstrap",
  tags$head(
    tags$style(
      HTML(
        '/* Radio buttons size */
        #radiobuttons_indicators_urbanbiodiversity label {
          font-size: 18px;
        }
        #radiobuttons_indicators_environmentalquality label {
          font-size: 18px;
        }
        #radiobuttons_indicators_smartcity label {
          font-size: 18px;
        }        
        #radiobuttons_indicators_recreation label {
          font-size: 18px;
        }
        #radiobuttons_indicators_sustainableurbandevelopment label {
          font-size: 18px;
        }
        .texto_agradecimiento_logos_1 {
          text-align: center;
        }        
        .texto_agradecimiento_logos_2 {
          text-align: center;
        }'
      )
    )
  ),
  
  navbarPage(
    title = tags$span(
      tags$a(href = "https://atlasverde.net/", target = "_blank", "Atlas de servicios ecosistémicos de la GAM"),
      " - ",
      "Ciudad verde - Corredores biológicos"
    ),
    theme = shinytheme("lumen"),
    
    # Pilar Ciudad verde: Sostenibilidad
    navbarMenu("Sostenibilidad", 
      icon = icon("globe-americas"),
               
      # Dimensión: Biodiversidad urbana
      tabPanel("Biodiversidad urbana", fluid = TRUE, 
        icon = icon("globe-americas"),
        sidebarLayout(
          sidebarPanel(
            fluidRow(h1(strong("Pilar Ciudad Verde"), br(), "Sostenibilidad")),
            fluidRow(h2(strong("Dimensión"), br(), "Biodiversidad urbana")),
            fluidRow(h3(strong("Meta aspiracional"), br(), em("La ciudad aumenta y mejora la biodiversidad urbana."))),
            fluidRow(h3(strong("Indicadores"))),
            fluidRow(
              radioButtons("radiobuttons_indicators_urbanbiodiversity",
                label = "",
                choices = c(
                  INDICATOR_WILDPROTECTEDAREAS_PERCENTAGE,
                  # INDICATOR_GREENBLUEINFRAESTRUCTURE_PERCENTAGE,
                  INDICATOR_RIVERSIDEVEGETATION_PERCENTAGE,
                  INDICATOR_PROBABILITY_BIOLOGICAL_CONNECTIVITY
                ),
                selected = INDICATOR_WILDPROTECTEDAREAS_PERCENTAGE
              )
            )
          ),
          mainPanel(
            fluidRow(h3(strong(textOutput("header_urbanbiodiversity")))),
            fluidRow(withSpinner(leafletOutput("map_urbanbiodiversity"))),
            fluidRow(h1(column(width = 12))),
            fluidRow(withSpinner(plotlyOutput("barplot_urbanbiodiversity")))
          )              
        )
      ),
      
      # Dimensión: Calidad ambiental
      tabPanel("Calidad ambiental", fluid = TRUE, 
        icon = icon("globe-americas"),
        sidebarLayout(
          sidebarPanel(
            fluidRow(h1(strong("Pilar Ciudad Verde"), br(), "Sostenibilidad")),
            fluidRow(h2(strong("Dimensión"), br(), "Calidad ambiental")),
            fluidRow(h3(strong("Meta aspiracional"), br(), em("La ciudad gestiona un entorno con calidad ambiental."))),
            fluidRow(h3(strong("Indicadores"))),
            fluidRow(
              radioButtons("radiobuttons_indicators_environmentalquality",
                label = "",
                choices = c(
                  INDICATOR_CARBON_STOCK,
                  INDICATOR_CARBON_DENSITY,
                  INDICATOR_SOILBIOTICINDEX,
                  INDICATOR_LANDSURFACE_TEMPERATURE
                ),
                selected = INDICATOR_CARBON_STOCK
              )
            )
          ),
          mainPanel(
            fluidRow(h3(strong(textOutput("header_environmentalquality")))),
            fluidRow(withSpinner(leafletOutput("map_environmentalquality"))),
            fluidRow(h1(column(width = 12))),
            fluidRow(withSpinner(plotlyOutput("barplot_environmentalquality")))
          )              
        )
      ),
      
      # Dimensión: Ciudad inteligente
      tabPanel("Ciudad inteligente", fluid = TRUE, 
        icon = icon("globe-americas"),
        sidebarLayout(
          sidebarPanel(
            fluidRow(h1(strong("Pilar Ciudad Verde"), br(), "Sostenibilidad")),
            fluidRow(h2(strong("Dimensión"), br(), "Ciudad inteligente")),
            fluidRow(h3(strong("Meta aspiracional"), br(), em("La ciudad utiliza tecnologías de información y comunicación y el conocimiento técnico como una herramienta para mejorar la calidad de vida."))),
            fluidRow(h3(strong("Indicadores"))),
            fluidRow(
              radioButtons("radiobuttons_indicators_smartcity",
                label = "",
                choices = c(
                  INDICATOR_POPULATIONINTERNETACCESS_PERCENTAGE
                ),
                selected = INDICATOR_POPULATIONINTERNETACCESS_PERCENTAGE
              )
            )
          ),
          mainPanel(
            fluidRow(h3(strong(textOutput("header_smartcity")))),
            fluidRow(withSpinner(leafletOutput("map_smartcity"))),
            fluidRow(h1(column(width = 12))),
            fluidRow(withSpinner(plotlyOutput("barplot_smartcity")))
          )              
        )
      )      
      
    ),
    
    # Pilar Ciudad verde: Salud y bienestar
    navbarMenu("Salud y bienestar", 
      icon = icon("globe-americas"),    
      
      # Dimensión: Recreación
      tabPanel("Recreación", fluid = TRUE, 
        icon = icon("globe-americas"),
        sidebarLayout(
          sidebarPanel(
            fluidRow(h1(strong("Pilar Ciudad Verde"), br(), "Salud y bienestar")),
            fluidRow(h2(strong("Dimensión"), br(), "Recreación")),
            fluidRow(h3(strong("Meta aspiracional"), br(), em("La ciudad brinda a sus habitantes espacios verdes públicos y de calidad, para la recreación y la salud mental."))),
            fluidRow(h3(strong("Indicadores"))),
            fluidRow(
              radioButtons("radiobuttons_indicators_recreation",
                label = "",
                choices = c(
                  INDICATOR_GREENAREA_AREA
                  # INDICATOR_TRAILS_LENGTH
                ),
                selected = INDICATOR_GREENAREA_AREA
              )
            )            
          ),
          mainPanel(
            fluidRow(h3(strong(textOutput("header_recreation")))),
            fluidRow(withSpinner(leafletOutput("map_recreation"))),
            fluidRow(h1(column(width = 12))),
            fluidRow(withSpinner(plotlyOutput("barplot_recreation")))
          )          
        )
      )
    ),
    
    # Pilar Ciudad verde: Resiliencia
    navbarMenu("Resiliencia", 
      icon = icon("globe-americas"),    
      
      # Dimensión: Desarrollo urbano sostenible
      tabPanel("Desarrollo urbano sostenible", fluid = TRUE, 
        icon = icon("globe-americas"),
        sidebarLayout(
          sidebarPanel(
            fluidRow(h1(strong("Pilar Ciudad Verde"), br(), "Resiliencia")),
            fluidRow(h2(strong("Dimensión"), br(), "Desarrollo urbano sostenible")),
            fluidRow(h3(strong("Meta aspiracional"), br(), em("La ciudad se planifica y ordena territorialmente bajo un enfoque que toma en cuenta los servicios ecosistémicos."))),
            fluidRow(h3(strong("Indicadores"))),
            fluidRow(
              radioButtons("radiobuttons_indicators_sustainableurbandevelopment",
                label = "",
                choices = c(
                  INDICATOR_POPULATIONINFORMALSETTLEMENTS_PERCENTAGE
                ),
                selected = INDICATOR_POPULATIONINFORMALSETTLEMENTS_PERCENTAGE
              )
            )            
          ),
          mainPanel(
            fluidRow(h3(strong(textOutput("header_sustainableurbandevelopment")))),
            fluidRow(withSpinner(leafletOutput("map_sustainableurbandevelopment"))),
            fluidRow(h1(column(width = 12))),
            fluidRow(withSpinner(plotlyOutput("barplot_sustainableurbandevelopment")))
          )          
        )
      )
    )
  ),
  
  fluidRow(h1(column(width = 12))),
  fluidRow(h1(column(width = 12))),    
  h3(class = "texto_agradecimiento_logos_1", strong("Acerca del Atlas de Servicios Ecosistémicos de la GAM")),
  h3(class = "texto_agradecimiento_logos-2", "El Atlas de Servicios Ecosistémicos de la GAM es producto de la cooperación entre los Gobiernos de Alemania y Costa Rica en el marco del proyecto Biodiver_City – Establecimiento de Corredores Biológicos Interurbanos con el fin de promover el desarrollo urbano centrado en los beneficios de la naturaleza. El instrumento fue desarrollado por el CATIE, por encargo de la Cooperación alemana para el desarrollo GIZ, bajo una estrecha articulación con el MINAE, CENIGA, SINAC y con el apoyo técnico del Instituto de Estudios Ambientales Helmholtz, UFZ."),
  fluidRow(h1(column(width = 12))),
  fluidRow(
    column(width = 4, img(src = "logo-gcr20222026.png", height = 90)),
    column(width = 4, img(src = "logo-minae.png", height = 90)),
    column(width = 4, img(src = "logo-sinac.jpg", height = 90)),
    class = "text-center"
  ),
  fluidRow(h1(column(width = 12))),
  fluidRow(
    column(width = 4, img(src = "logo-catie.jpeg", height = 90)),
    column(width = 4, img(src = "logo-giz.png", height = 90)),
    column(
      width = 4,
      img(src = "logo-minambientealemania-iki.png", height = 90)
    ),
    class = "text-center"
  ),
  fluidRow(h1(column(width = 12))),
  fluidRow(h1(column(width = 12)))  
)


# SERVER LOGIC
server <- function(input, output) {
  # Urban biodiversity indicators header
  output$header_urbanbiodiversity <- renderText(if (input$radiobuttons_indicators_urbanbiodiversity == INDICATOR_WILDPROTECTEDAREAS_PERCENTAGE) {
    INDICATOR_WILDPROTECTEDAREAS_PERCENTAGE
#  } else if (input$radiobuttons_indicators_urbanbiodiversity == INDICATOR_GREENBLUEINFRAESTRUCTURE_PERCENTAGE) {
#    INDICATOR_GREENBLUEINFRAESTRUCTURE_PERCENTAGE
  } else if (input$radiobuttons_indicators_urbanbiodiversity == INDICATOR_RIVERSIDEVEGETATION_PERCENTAGE) {
    INDICATOR_RIVERSIDEVEGETATION_PERCENTAGE
  } else if (input$radiobuttons_indicators_urbanbiodiversity == INDICATOR_PROBABILITY_BIOLOGICAL_CONNECTIVITY) {
    INDICATOR_PROBABILITY_BIOLOGICAL_CONNECTIVITY
  })
  
  # Environmental quality indicators header
  output$header_environmentalquality <- renderText(if (input$radiobuttons_indicators_environmentalquality == INDICATOR_CARBON_STOCK) {
    INDICATOR_CARBON_STOCK
  } else if (input$radiobuttons_indicators_environmentalquality == INDICATOR_CARBON_DENSITY) {
    INDICATOR_CARBON_DENSITY
  } else if (input$radiobuttons_indicators_environmentalquality == INDICATOR_SOILBIOTICINDEX) {
    INDICATOR_SOILBIOTICINDEX
  } else if (input$radiobuttons_indicators_environmentalquality == INDICATOR_LANDSURFACE_TEMPERATURE) {
    INDICATOR_LANDSURFACE_TEMPERATURE
  })   
  
  # Smart city indicators header
  output$header_smartcity <- renderText(if (input$radiobuttons_indicators_smartcity == INDICATOR_POPULATIONINTERNETACCESS_PERCENTAGE) {
    INDICATOR_POPULATIONINTERNETACCESS_PERCENTAGE
  })
  
  # Recreation indicators header
  output$header_recreation <- renderText(if (input$radiobuttons_indicators_recreation == INDICATOR_GREENAREA_AREA) {
    INDICATOR_GREENAREA_AREA
#  } else if (input$radiobuttons_indicators_recreation == INDICATOR_TRAILS_LENGTH) {
#    INDICATOR_TRAILS_LENGTH
  })
  
  # Sustainable urban development indicators header
  output$header_sustainableurbandevelopment <- renderText(if (input$radiobuttons_indicators_sustainableurbandevelopment == INDICATOR_POPULATIONINFORMALSETTLEMENTS_PERCENTAGE) {
    INDICATOR_POPULATIONINFORMALSETTLEMENTS_PERCENTAGE
  })  
  
  
  # Urban biodiversity indicators map
  output$map_urbanbiodiversity <- renderLeaflet({
    if (input$radiobuttons_indicators_urbanbiodiversity == INDICATOR_WILDPROTECTEDAREAS_PERCENTAGE) {
      create_map(
        COLUMN_WILDPROTECTEDAREAS_PERCENTAGE,
        INDICATOR_WILDPROTECTEDAREAS_PERCENTAGE,
        INDICATOR_WILDPROTECTEDAREAS_PERCENTAGE,
        QUANTILES_WILDPROTECTEDAREAS_PERCENTAGE_LABELS,
        ~ PALETTE_WILDPROTECTEDAREAS_PERCENTAGE(COLUMN_WILDPROTECTEDAREAS_PERCENTAGE),
        PALETTE_WILDPROTECTEDAREAS_PERCENTAGE,
        UNIT_WILDPROTECTEDAREAS_PERCENTAGE
      )
#    } else if (input$radiobuttons_indicators_urbanbiodiversity == INDICATOR_GREENBLUEINFRAESTRUCTURE_PERCENTAGE) {
#      create_map(
#        COLUMN_GREENBLUEINFRAESTRUCTURE_PERCENTAGE,
#        INDICATOR_GREENBLUEINFRAESTRUCTURE_PERCENTAGE,
#        INDICATOR_GREENBLUEINFRAESTRUCTURE_PERCENTAGE,
#        QUANTILES_GREENBLUEINFRAESTRUCTURE_PERCENTAGE_LABELS,
#        ~ PALETTE_GREENBLUEINFRAESTRUCTURE_PERCENTAGE(porc_tv),
#        PALETTE_GREENBLUEINFRAESTRUCTURE_PERCENTAGE,
#        UNIT_GREENBLUEINFRAESTRUCTURE_PERCENTAGE
#      )      
    } else if (input$radiobuttons_indicators_urbanbiodiversity == INDICATOR_RIVERSIDEVEGETATION_PERCENTAGE) {
      create_map(
        COLUMN_RIVERSIDEVEGETATION_PERCENTAGE,
        INDICATOR_RIVERSIDEVEGETATION_PERCENTAGE,
        INDICATOR_RIVERSIDEVEGETATION_PERCENTAGE,
        QUANTILES_RIVERSIDEVEGETATION_PERCENTAGE_LABELS,
        ~ PALETTE_RIVERSIDEVEGETATION_PERCENTAGE(COLUMN_RIVERSIDEVEGETATION_PERCENTAGE),
        PALETTE_RIVERSIDEVEGETATION_PERCENTAGE,
        UNIT_RIVERSIDEVEGETATION_PERCENTAGE
      )            
    } else if (input$radiobuttons_indicators_urbanbiodiversity == INDICATOR_PROBABILITY_BIOLOGICAL_CONNECTIVITY) {
      create_map_connectivity()
    }
  })
  
# Environmental quality indicators map
  output$map_environmentalquality <- renderLeaflet({
    if (input$radiobuttons_indicators_environmentalquality == INDICATOR_CARBON_STOCK) {
      create_map(
        COLUMN_CARBON_STOCK,
        INDICATOR_CARBON_STOCK,
        INDICATOR_CARBON_STOCK,
        QUANTILES_CARBON_STOCK_LABELS,
        ~ PALETTE_CARBON_STOCK(COLUMN_CARBON_STOCK),
        PALETTE_CARBON_STOCK,
        UNIT_CARBON_STOCK
      )
    } else if (input$radiobuttons_indicators_environmentalquality == INDICATOR_CARBON_DENSITY) {
      create_map(
        COLUMN_CARBON_DENSITY,
        INDICATOR_CARBON_DENSITY,
        INDICATOR_CARBON_DENSITY,
        QUANTILES_CARBON_DENSITY_LABELS,
        ~ PALETTE_CARBON_DENSITY(COLUMN_CARBON_DENSITY),
        PALETTE_CARBON_DENSITY,
        UNIT_CARBON_DENSITY
      )      
    } else if (input$radiobuttons_indicators_environmentalquality == INDICATOR_SOILBIOTICINDEX) {
      create_map(
        COLUMN_SOILBIOTICINDEX,
        INDICATOR_SOILBIOTICINDEX,
        INDICATOR_SOILBIOTICINDEX,
        QUANTILES_SOILBIOTICINDEX_LABELS,
        ~ PALETTE_SOILBIOTICINDEX(COLUMN_SOILBIOTICINDEX),
        PALETTE_SOILBIOTICINDEX,
        UNIT_SOILBIOTICINDEX
      )            
    } else if (input$radiobuttons_indicators_environmentalquality == INDICATOR_LANDSURFACE_TEMPERATURE) {
      create_map(
        COLUMN_LANDSURFACE_TEMPERATURE,
        INDICATOR_LANDSURFACE_TEMPERATURE,
        INDICATOR_LANDSURFACE_TEMPERATURE,
        QUANTILES_LANDSURFACE_TEMPERATURE_LABELS,
        ~ PALETTE_LANDSURFACE_TEMPERATURE(COLUMN_LANDSURFACE_TEMPERATURE),
        PALETTE_LANDSURFACE_TEMPERATURE,
        UNIT_LANDSURFACE_TEMPERATURE
      )            
    }
  })
  
# Smart city indicators map
  output$map_smartcity <- renderLeaflet({
    if (input$radiobuttons_indicators_smartcity == INDICATOR_POPULATIONINTERNETACCESS_PERCENTAGE) {
      create_map(
        COLUMN_POPULATIONINTERNETACCESS_PERCENTAGE,
        INDICATOR_POPULATIONINTERNETACCESS_PERCENTAGE,
        INDICATOR_POPULATIONINTERNETACCESS_PERCENTAGE,
        QUANTILES_POPULATIONINTERNETACCESS_PERCENTAGE_LABELS,
        ~ PALETTE_POPULATIONINTERNETACCESS_PERCENTAGE(COLUMN_POPULATIONINTERNETACCESS_PERCENTAGE),
        PALETTE_POPULATIONINTERNETACCESS_PERCENTAGE,
        UNIT_POPULATIONINTERNETACCESS_PERCENTAGE
      )
    }
  })    
  
  # Recreation indicators map
  output$map_recreation <- renderLeaflet({
    if (input$radiobuttons_indicators_recreation == INDICATOR_GREENAREA_AREA) {
      create_map(
        COLUMN_GREENAREA_AREA,
        INDICATOR_GREENAREA_AREA,
        INDICATOR_GREENAREA_AREA,
        QUANTILES_GREENAREA_AREA_LABELS,
        ~ PALETTE_GREENAREA_AREA(COLUMN_GREENAREA_AREA),
        PALETTE_GREENAREA_AREA,
        UNIT_GREENAREA_AREA
      )
#    } else if (input$radiobuttons_indicators_recreation == INDICATOR_TRAILS_LENGTH) {
#      create_map(
#        COLUMN_TRAILS_LENGTH,
#        INDICATOR_TRAILS_LENGTH,
#        INDICATOR_TRAILS_LENGTH,
#        QUANTILES_TRAILS_LENGTH_LABELS,
#        ~ PALETTE_TRAILS_LENGTH(km_sendero),
#        PALETTE_TRAILS_LENGTH,
#        UNIT_TRAILS_LENGTH
#      )      
    }
  })
  
  # Sustainable urban development indicators map
  output$map_sustainableurbandevelopment  <- renderLeaflet({
    if (input$radiobuttons_indicators_sustainableurbandevelopment == INDICATOR_POPULATIONINFORMALSETTLEMENTS_PERCENTAGE) {
      create_map(
        COLUMN_POPULATIONINFORMALSETTLEMENTS_PERCENTAGE,
        INDICATOR_POPULATIONINFORMALSETTLEMENTS_PERCENTAGE,
        INDICATOR_POPULATIONINFORMALSETTLEMENTS_PERCENTAGE,
        QUANTILES_POPULATIONINFORMALSETTLEMENTS_PERCENTAGE_LABELS,
        ~ PALETTE_POPULATIONINFORMALSETTLEMENTS_PERCENTAGE(COLUMN_POPULATIONINFORMALSETTLEMENTS_PERCENTAGE),
        PALETTE_POPULATIONINFORMALSETTLEMENTS_PERCENTAGE,
        UNIT_POPULATIONINFORMALSETTLEMENTS_PERCENTAGE
      )
    }
  })  
  
  
  # Urban biodiversity indicators bar plot
  output$barplot_urbanbiodiversity <- renderPlotly({
    if (input$radiobuttons_indicators_urbanbiodiversity == INDICATOR_WILDPROTECTEDAREAS_PERCENTAGE) {
      create_barplot(
        COLUMN_WILDPROTECTEDAREAS_PERCENTAGE,
        INDICATOR_WILDPROTECTEDAREAS_PERCENTAGE,
        INDICATOR_WILDPROTECTEDAREAS_PERCENTAGE,
        PALETTE_WILDPROTECTEDAREAS_PERCENTAGE_END_COLOR,
        UNIT_WILDPROTECTEDAREAS_PERCENTAGE
      )      
#    } else if (input$radiobuttons_indicators_urbanbiodiversity == INDICATOR_GREENBLUEINFRAESTRUCTURE_PERCENTAGE) {
#      create_barplot(
#        COLUMN_GREENBLUEINFRAESTRUCTURE_PERCENTAGE,
#        INDICATOR_GREENBLUEINFRAESTRUCTURE_PERCENTAGE,
#        INDICATOR_GREENBLUEINFRAESTRUCTURE_PERCENTAGE,
#        PALETTE_GREENBLUEINFRAESTRUCTURE_PERCENTAGE_END_COLOR,
#        UNIT_GREENBLUEINFRAESTRUCTURE_PERCENTAGE
#      )
    } else if (input$radiobuttons_indicators_urbanbiodiversity == INDICATOR_RIVERSIDEVEGETATION_PERCENTAGE) {
      create_barplot(
        COLUMN_RIVERSIDEVEGETATION_PERCENTAGE,
        INDICATOR_RIVERSIDEVEGETATION_PERCENTAGE,
        INDICATOR_RIVERSIDEVEGETATION_PERCENTAGE,
        PALETTE_RIVERSIDEVEGETATION_PERCENTAGE_END_COLOR,
        UNIT_RIVERSIDEVEGETATION_PERCENTAGE
      )      
    }
  })
  
  # Environmental quality indicators bar plot
  output$barplot_environmentalquality <- renderPlotly({
    if (input$radiobuttons_indicators_environmentalquality == INDICATOR_CARBON_STOCK) {
      create_barplot(
        COLUMN_CARBON_STOCK,
        INDICATOR_CARBON_STOCK,
        INDICATOR_CARBON_STOCK,
        PALETTE_CARBON_STOCK_END_COLOR,
        UNIT_CARBON_STOCK
      )      
    } else if (input$radiobuttons_indicators_environmentalquality == INDICATOR_CARBON_DENSITY) {
      create_barplot(
        COLUMN_CARBON_DENSITY,
        INDICATOR_CARBON_DENSITY,
        INDICATOR_CARBON_DENSITY,
        PALETTE_CARBON_DENSITY_END_COLOR,
        UNIT_CARBON_DENSITY
      )
    } else if (input$radiobuttons_indicators_environmentalquality == INDICATOR_SOILBIOTICINDEX) {
      create_barplot(
        COLUMN_SOILBIOTICINDEX,
        INDICATOR_SOILBIOTICINDEX,
        INDICATOR_SOILBIOTICINDEX,
        PALETTE_SOILBIOTICINDEX_END_COLOR,
        UNIT_SOILBIOTICINDEX
        )
    } else if (input$radiobuttons_indicators_environmentalquality == INDICATOR_LANDSURFACE_TEMPERATURE) {
      create_barplot(
        COLUMN_LANDSURFACE_TEMPERATURE,
        INDICATOR_LANDSURFACE_TEMPERATURE,
        INDICATOR_LANDSURFACE_TEMPERATURE,
        PALETTE_LANDSURFACE_TEMPERATURE_END_COLOR,
        UNIT_LANDSURFACE_TEMPERATURE
      )
    }
  })  
  
  # Smart city indicators bar plot
  output$barplot_smartcity <- renderPlotly({
    if (input$radiobuttons_indicators_smartcity == INDICATOR_POPULATIONINTERNETACCESS_PERCENTAGE) {
      create_barplot(
        COLUMN_POPULATIONINTERNETACCESS_PERCENTAGE,
        INDICATOR_POPULATIONINTERNETACCESS_PERCENTAGE,
        INDICATOR_POPULATIONINTERNETACCESS_PERCENTAGE,
        PALETTE_POPULATIONINTERNETACCESS_PERCENTAGE_END_COLOR,
        UNIT_POPULATIONINTERNETACCESS_PERCENTAGE
      )      
    }
  })  

  # Recreation indicators bar plot
  output$barplot_recreation <- renderPlotly({
    if (input$radiobuttons_indicators_recreation == INDICATOR_GREENAREA_AREA) {
      create_barplot(
        COLUMN_GREENAREA_AREA,
        INDICATOR_GREENAREA_AREA,
        INDICATOR_GREENAREA_AREA,
        PALETTE_GREENAREA_AREA_END_COLOR,
        UNIT_GREENAREA_AREA
      )
#    } else if (input$radiobuttons_indicators_recreation == INDICATOR_TRAILS_LENGTH) {
#      create_barplot(
#        COLUMN_TRAILS_LENGTH,
#        INDICATOR_TRAILS_LENGTH,
#        INDICATOR_TRAILS_LENGTH,
#        PALETTE_TRAILS_LENGTH_END_COLOR,
#        UNIT_TRAILS_LENGTH
#      )      
    }        
  })
  
  # Sustainable urban development indicators bar plot
  output$barplot_sustainableurbandevelopment  <- renderPlotly({
    if (input$radiobuttons_indicators_sustainableurbandevelopment == INDICATOR_POPULATIONINFORMALSETTLEMENTS_PERCENTAGE) {
      create_barplot(
        COLUMN_POPULATIONINFORMALSETTLEMENTS_PERCENTAGE,
        INDICATOR_POPULATIONINFORMALSETTLEMENTS_PERCENTAGE,
        INDICATOR_POPULATIONINFORMALSETTLEMENTS_PERCENTAGE,
        PALETTE_POPULATIONINFORMALSETTLEMENTS_PERCENTAGE_END_COLOR,
        UNIT_POPULATIONINFORMALSETTLEMENTS_PERCENTAGE
      )
    }        
  })
  
  # # Initial "selected" canton
  # selected_canton <- reactiveVal("San José")
  # 
  # # Capture click event in corridors layer for zooming and changing styles
  # observeEvent(input$map_urbanbiodiversity_click, {
  #   print("a")
  #   click_data <- input$map_urbanbiodiversity_click
  # 
  #   if (!is.null(click_data)) {
  #     print("b")
  #     selected_canton(click_data$id)
  #     print(click_data$id)
  # 
  #     # Zoom to selected polygon
  #     selected_canton_polygon <- corridors |> filter(canton == click_data$id)
  #     leafletProxy("map_urbanbiodiversity") |>
  #       fitBounds(
  #         lng1 = min(st_bbox(selected_canton_polygon)[["xmin"]]),
  #         lat1 = min(st_bbox(selected_canton_polygon)[["ymin"]]),
  #         lng2 = max(st_bbox(selected_canton_polygon)[["xmax"]]),
  #         lat2 = max(st_bbox(selected_canton_polygon)[["ymax"]])
  #       )
  # 
  #   }
  # })
  
}


# RUN APPLICATION
shinyApp(ui = ui, server = server)