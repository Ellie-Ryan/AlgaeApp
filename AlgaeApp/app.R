## Snow Algae Mapping tool 
## Ellie Ryan, Western Washington University
## Created 5/13/2025
## Last modified 5/16/2025

## Includes code adapted from the following sources: 
# https://github.com/eparker12/nCoV_tracker


##### SETUP #####

# Load required packages 
library(shiny)
library(shinythemes)
library(tidyverse)
library(leaflet)
library(terra)
library(rsconnect)
library(shinyWidgets)


# Import data
df <- read.csv("ShinyApp_MergedAlgaeData_20250527.csv")


##### DATA PROCESSING #####

#Format date column 
df$CollectionDate <- as.Date(df$CollectionDate, format = "%m/%d/%Y")

# Create range of dates based on available data
date_choices <- df$CollectionDate

# Normalize SnowColor values
df$SnowColor <- tolower(trimws(df$SnowColor))

# Assign color palette to leaflet map points
snowColors <- c(
  "red" = "#BE5625",
  "algae-free" = "#87CBCD",
  "green" = "#62823B"
)

# Assign color palette to location 
locationColors <- c(
  "Lemon Creek Glacier" = "#e0b10d",
  "Camp Kiser" = "#1f3b77",
  "Antarctic Peninsula" = "#4c7575")

# Format plot theme
plot_theme <- theme(
  axis.title = element_text(size = 12, face = "bold"), # Axis labels: 12 pt, bold
  axis.text = element_text(size = 11), # Axis text: 12 pt
  axis.line = element_line(linewidth = 0.5),  # Thicker axis line
  legend.position = "top", # Position legend at the top
  legend.justification = c(0, 1), # Justify legend to top-left
  legend.text = element_text(size = 11), # Legend text: 12 pt
  legend.title = element_text(size = 12), 
  plot.title = element_text(size = 12, face = "bold"),  # Adjust plot title size
  plot.subtitle = element_text(size = 12, face = "italic"), 
  axis.text.x = element_text(angle = 45, hjust = 1), 
  plot.margin = margin(20, 30, 20, 40)
)



##### UI #####
ui <- bootstrapPage(
  tags$style(HTML("
  /*CSS code block*/
                body {
                  background-color: #25303c;
                }
                /* Set HTML controls for selection panel */
                #controls {
                  background-color: rgba(255, 255, 255, 0.7); /* White bg color with 70% opacity */
                  padding: 15px;
                  border-radius: 8px; /* Rounded borders */
                  box-shadow: 0 4px 8px rgba(0,0,0,0.1); /* add drop shadow */
                }
                
                /* Set HTML controls for pigment/location plot panel */
                #pigmentPanel {
                background-color: rgba(255, 255, 255, 0.7);
                  padding: 15px;
                  border-radius: 8px;
                  box-shadow: 0 4px 8px rgba(0,0,0,0.1);
                }
                
                /* Set HTML controls for pigment/color plot panel */
                #pigmentColorPanel {
                background-color: rgba(255, 255, 255, 0.7);
                  padding: 15px;
                  border-radius: 8px;
                  box-shadow: 0 4px 8px rgba(0,0,0,0.1);
                }
                
                /* Set HTML controls for cell plot panel */
                #cellPanel {
                background-color: rgba(255, 255, 255, 0.7);
                  padding: 15px;
                  border-radius: 8px;
                  box-shadow: 0 4px 8px rgba(0,0,0,0.1);
                }
                
                /* Set HTML controls so that h5 headings are bold */  
                #controls h5 {
                  font-weight: bold;
                }
                
                /* Set HTML controls so that overview text is white */
                #overview-text {
                  color: white;
                  padding: 20px;
                }
                
                /* Set project background text to white */
                #background {
                  color: white;
                  padding: 20px;
                }
                
                /* Set study overview text to white */
                 #overview-study {
                  color: white;
                  padding: 20px;
                }
                  ")),
  
  navbarPage(theme = shinytheme("flatly"), # Create navbar page layout
             collapsible = TRUE, # Collapsible navigation panel
             # Custom HTML element, adds white un-clickable text: "Khan Snow Algae"
             HTML('<a style="text-decoration:none;cursor:default;color:#FFFFFF;" class="active" href="#">Khan Lab Snow Algae</a>'), 
             id="nav",
             windowTitle = "Khan Lab Snow Algae",
             
             # Snow algae mapper tab in navbar
             tabPanel("Snow Algae Mapper",
                      # Format leaflet map to fill entire screen
                      leafletOutput("mymap", width = "100%", height = "100vh"),
                      
                      # Selection panel
                      absolutePanel(
                        id = "controls", # Call CSS controls
                        class = "panel panel-default",
                        fixed = TRUE, # Fixed size
                        draggable = TRUE, # Move within screen
                        style = "top: 10vh; left: 4vw; width: 20vw; height: auto;",
                        
                        # Site selector dropdown in selection panel
                        pickerInput(
                          inputId = "sitePicker",
                          label = h5("Select sites"),
                          choices = unique(df$Location), # Define dropdown options
                          selected = unique(df$Location), # Define default selection
                          multiple = TRUE, # allow multiple selections
                          options = list(`actions-box` = TRUE) # add select and deselect all options
                          ),
                        
                        # Snow color dropdown in selection panel
                        pickerInput(
                          inputId = "colorPicker",
                          label = h5("Select snow color"),
                          choices = unique(df$SnowColor),
                          selected = unique(df$SnowColor),
                          multiple = TRUE,
                          options = list(`actions-box` = TRUE)
                        ),
                        
                        # Date range window in selection panel
                        dateRangeInput(
                          inputId = "dateRange",
                          label = h5("Select mapping date range"),
                          start = min(df$CollectionDate), # Default start date
                          end = max(df$CollectionDate), # Default end date
                          min = min(as.Date(date_choices)), # Earliset selectable date
                          max = max(as.Date(date_choices)), # Latest selectable date
                          format = "yyyy-mm-dd",
                          separator = " to "
                        )
             ),
                    # Pigment bar plot by location (formatting)
                    absolutePanel(
                      id = "pigmentPanel",
                      class = "panel panel-default",
                      draggable = TRUE,
                      style = "top: 55vh; left: 4vw; width: 25vw; height: auto;",
                      h4(" Pigment Concentration by Site"),
                      plotOutput("pigmentPlot", height = "300px")
                      
              ),
                  # Pigment bar plot by color (formatting)
                   absolutePanel(
                     id = "pigmentColorPanel",
                     class = "panel panel-default",
                     draggable = TRUE,
                     style = "top: 10vh; right: 4vw; width: 25vw; height: auto;",
                     h4(" Pigment Concentration by Snow Color"),
                     plotOutput("colorPlot", height = "300px")
                     
               ),
                  # Cell density bar plot (formatting)
                   absolutePanel(
                     id = "cellPanel",
                     class = "panel panel-default",
                     style = "top: 55vh; right: 4vw; width: 25vw; height: auto;",
                     fixed = TRUE, 
                     draggable = TRUE,
                     h4(" Cell Density by Site"),
                     plotOutput("cellPlot", height = "300px")
                   
               ),
      ),
             
            # Project Overview tab in navbar
             tabPanel("Project Overview",
                      
                      # App overview text chunk
                      div(id = "overview-text",
                          h2("App Overview"),
                          "This app shows the locations of snow algae samples that have been collected (by myself and others) as a part of my thesis project as a master's candidate 
                          in Environmental Science at Western Washington University. It features data collected from three main field campaigns: Lemon Creek (Alaska), Camp Kiser 
                          (Mt. Baker, Washington), and various sites along the Western Antarctic Peninsula. The aim of this project is to compare observations of snow algal pigment 
                          concentrations and cell density between differing apparent snow colors (red, green, and algae-free) and geographic locations. It is intended for use within 
                          the Khan lab as a quick data visualizaiton tool to identify any patterns or trends in samples among the different locations and snow colors. The eventual goal 
                          is to create something similar to this that can be integrated into the lab webpage for public access. To use this app, toggle through the different 
                          sampling sites and snow colors to view data specific to each region or each snow color. Note that algae-free samples were visibly free of algae when sampled, 
                          but that this is a qualitative assessment. Pigment and cell density data often reveal small algal quantities in these clean samples."
                      ),
                      
                      # Project background text chunk
                      div(id = "background",
                          h2("What are snow algae and why are they significant?"),
                          # Fluid row for split image and text pane
                          fluidRow(
                            # Text pane in fluid row
                            column(
                              width = 6,
                              p("Snow algae are extremophilic primary producers that thrive in cold glacial and snowfield environments, 
          contributing significantly to snow albedo reduction (Hodson et al., 2008; Skiles et al., 2018b). 
          Green algae can reduce albedo by ~40%, and red algae by ~20% (Khan et al., 2021). Their pigments, 
          especially the carotenoid astaxanthin, absorb solar radiation and dissipate heat, promoting snowmelt 
          (Ganey et al., 2017; Bidigare et al., 1993). These adaptations help algae persist under high solar irradiance (Häder & Häder, 1989)."),
                              
                              p("Algae blooms are seasonal, occurring when temperatures rise above freezing and ending with the first snowfall or 
          complete melt (Hoham & Duval, 2001). Despite their brief activity, blooms often recur annually in the same locations 
          (Matsumoto et al., 2024), leading to sustained impacts on snow albedo."),
                              
                              p("In northwest North America, snow algae cover ~5% of glaciated terrain and up to 65% of individual glaciers (Engstrom & Quarmby, 2023). 
          Red-pigmented algae dominate the snow surface in mid-latitudes, while both red and green algae occur in polar regions (Hamilton & Havig, 2017; Ji et al., 2022)."),
                              
                              p("Algal blooms reduce snow albedo to as low as 45% (Huovinen et al., 2018) and can account for up to 17% of total snowmelt 
          (Ganey et al., 2017). Their radiative forcing is comparable to that of black carbon and other light-absorbing particles. 
          In the Antarctic, blooms are often linked to nutrient-rich zones near shorebird rookeries and rocky outcrops (Hoham & Remias, 2020; Lutz et al., 2015)."),
                              
                              p("The role of nutrients in algae growth is still debated. Nitrogen and phosphorus are essential, but studies are divided 
          on whether snow algae are nutrient-limited (Stibal et al., 2006; Lutz et al., 2016). Green algae are found in wetter, nutrient-rich snow, 
          while red algae tend to dominate in dry, nutrient-poor zones (Lutz et al., 2015). Direct field comparisons of pigment composition, 
          cell density, and nutrient availability—especially in the Antarctic Peninsula—are still lacking.")
                            ),
                            
                            # Image pane in fluid row
                            column(
                              width = 6,
                              tags$div(
                                style = "text-align: center;",
                                tags$img(src = "snow_algae.jpeg", 
                                         width = "70%", 
                                         height = "auto",
                                         style = "border-radius: 8px; box-shadow: 0 4px 8px rgba(0,0,0,0.2);"),
                                tags$figcaption("Red snow algae on the surface of a coastal snowfield in the Antarctic Peninsula. Photo: Ellie Ryan")
                              )
                            ))),
                      
                      # Study Overview text chunk
                      div(id = "overview-study",
                          h2("Study Overview & Field Campaigns"),
                          
                          # Lemon Creek Glacier row with image
                          fluidRow(
                            column(
                              width = 3,
                              h3("Lemon Creek Glacier (Alaska)"),
                              p("A spatially focused field campaign was conducted by the Khan Lab in August 2023 
         on the Lemon Creek Glacier, located at the southern edge of the Juneau Icefield. 
         Fifty-nine snow samples were collected, including twenty-nine from predetermined 
         random locations and the rest from visible snow algae patches.")
                            ),
                            column(
                              width = 3,
                              tags$img(src = "lemon_creek.jpg", style = "width:100%; border-radius: 8px; margin-bottom: 25px;")
                            )
                          ),
                          
                          # Camp Kiser row with image
                          fluidRow(
                            column(
                              width = 3,
                              h3("Camp Kiser (Mt. Baker, Washington)"),
                              p("In August 2024, a field campaign was conducted at Camp Kiser on the Sholes Glacier 
         boundary. Mount Baker, a glacier-covered stratovolcano in the North Cascades, feeds 
         the Nooksack Watershed. One hundred samples were collected from locations stratified 
         by algal presence and intensity, identified using satellite imagery.")
                            ),
                            column(
                              width = 3,
                              tags$img(src = "camp_kiser.jpeg", style = "width:100%; border-radius: 8px; margin-bottom: 25px;")
                            )
                          ),
                          
                          # Western Antarctic Peninsula row with image
                          fluidRow(
                            column(
                              width = 3,
                              h3("Western Antarctic Peninsula"),
                              p("During the 2025 austral summer (February–March), eighty-four snow samples were 
         collected from islands along the Antarctic Peninsula coastline. The samples included 
         red, green, and algae-free snow, collected opportunistically in this region where 
         snow algae significantly impact melt.")
                            ),
                            column(
                              width = 3,
                              tags$img(src = "antarctica.jpeg", style = "width:100%; border-radius: 8px; margin-bottom: 25px;")
                            )
                          )
                      ),
                      
                      # Citations section
                      fluidRow(
                        column(
                          width = 6,
                          h3("Citations", style = "color: white;"),
                          tags$div(
                            style = "max-height: 900px; overflow-y: auto; padding-right: 10px; color: white",
                            # Create an ordered list - li = list item
                            tags$ol(
                              tags$li("Bidigare, R. R., et al. (1993). Evidence a Photoprotective for Secondary Carotenoids of Snow Algae1. *Journal of Phycology, 29*(4), 427–434. https://doi.org/10.1111/j.1529-8817.1993.tb00143.x"),
                              tags$li("Engstrom, C. B., & Quarmby, L. M. (2023). Satellite mapping of red snow on North American glaciers. *Science Advances, 9*(47), eadi3268. https://doi.org/10.1126/sciadv.adi3268"),
                              tags$li("Ganey, G., et al. (2017). The role of microbes in snowmelt and radiative forcing on an Alaskan icefield. *Nature Geoscience, 10*. https://doi.org/10.1038/ngeo3027"),
                              tags$li("Häder, D.-P., & Häder, M. A. (1989). Effects of solar u.v.-B irradiation on photomovement and motility in photosynthetic and colorless flagellates. *Environmental and Experimental Botany, 29*(2), 273–282. https://doi.org/10.1016/0098-8472(89)90059-2"),
                              tags$li("Hamilton, T. L., & Havig, J. (2017). Primary productivity of snow algae communities on stratovolcanoes of the Pacific Northwest. *Geobiology, 15*(2), 280–295. https://doi.org/10.1111/gbi.12219"),
                              tags$li("Hodson, A., et al. (2008). Glacial Ecosystems. *Ecological Monographs, 78*(1), 41–67. https://doi.org/10.1890/07-0187.1"),
                              tags$li("Hoham, R. W., & Remias, D. (2020). Snow and Glacial Algae: A Review1. *Journal of Phycology, 56*(2), 264–282. https://doi.org/10.1111/jpy.12952"),
                              tags$li("Huovinen, P., et al. (2018). Remote sensing of albedo-reducing snow algae and impurities in the Maritime Antarctica. *ISPRS Journal of Photogrammetry and Remote Sensing, 146*, 507–517. https://doi.org/10.1016/j.isprsjprs.2018.10.015"),
                              tags$li("Ji, M., et al. (2022). Similar heterotrophic communities but distinct interactions supported by red and green‐snow algae in the Antarctic Peninsula. *New Phytologist, 233*(3), 1358–1368. https://doi.org/10.1111/nph.17764"),
                              tags$li("Khan, A. L., et al. (2021). Spectral characterization, radiative forcing and pigment content of coastal Antarctic snow algae... *The Cryosphere, 15*(1), 133–148. https://doi.org/10.5194/tc-15-133-2021"),
                              tags$li("Lutz, S., et al. (2015). Integrated ‘Omics’, Targeted Metabolite and Single-cell Analyses of Arctic Snow Algae Functionality and Adaptability. *Frontiers in Microbiology, 6*. https://doi.org/10.3389/fmicb.2015.01323"),
                              tags$li("Lutz, S., et al. (2016). The biogeography of red snow microbiomes and their role in melting arctic glaciers. *Nature Communications, 7*(1), 11968. https://doi.org/10.1038/ncomms11968"),
                              tags$li("Matsumoto, M., et al. (n.d.). Hypothesized life cycle of the snow algae Chlainomonas sp. *Journal of Phycology*. https://doi.org/10.1111/jpy.13454"),
                              tags$li("Hoham, R. W. & Duval, B. (2001). Microbial ecology of snow and freshwater ice with emphasis on snow algae."),
                              tags$li("Skiles, S. M., et al. (2018). Radiative forcing by light-absorbing particles in snow. *Nature Climate Change, 8*(11), 964–971. https://doi.org/10.1038/s41558-018-0296-5"),
                              tags$li("Stibal, M., et al. (2006). Microbial Communities on Glacier Surfaces in Svalbard... *Microbial Ecology, 52*(4), 644–654. https://doi.org/10.1007/s00248-006-9083-3")
                            )
                          )
                        )
                      )
                      
             )))





##### SERVER #####
server <- function(input, output, session) {
  
# Create a reactive variable that updates whenever user inputs change
filteredData <- reactive({
  df %>% 
    filter(Location %in% input$sitePicker,
           SnowColor %in% input$colorPicker,
           CollectionDate >= input$dateRange[1],
           CollectionDate <= input$dateRange[2])
})

# Leaflet map settings
output$mymap <- renderLeaflet({
  
  # store filtered data as a data frame, set map bounds
  filteredDF <- filteredData()
  if (nrow(filteredDF) == 0) {
    filteredDF <- df  # full dataset extent for map if no pts selected
  }
  
  # Base map without points, set initial bounds to entire df extent
  leaflet() %>% 
    addProviderTiles(providers$OpenStreetMap) %>%
    fitBounds(
      lng1 = min(filteredDF$Longitude), 
      lat1 = min(filteredDF$Latitude),
      lng2 = max(filteredDF$Longitude), 
      lat2 = max(filteredDF$Latitude)
    ) %>%
    # Add legend
    ## Used AI to generate this
    ## addLegend() did not fit legend to the map (cut off bottom half)
    addControl(
      html = HTML(
        "<div style='background:white;padding:10px;border-radius:5px;
         box-shadow:0 2px 6px rgba(0,0,0,0.3); 
         display:inline-block; margin-bottom:80px; max-width: 200px;'>
       <b>Snow Color</b><br>
       <div style='margin-top:5px;'>
         <div><span style='background:#BE5625;width:12px;height:12px;display:inline-block;margin-right:5px;'></span>Red</div>
         <div><span style='background:#62823B;width:12px;height:12px;display:inline-block;margin-right:5px;'></span>Green</div>
         <div><span style='background:#87CBCD;width:12px;height:12px;display:inline-block;margin-right:5px;'></span>Algae-Free</div>
       </div>
     </div>"
      ),
      position = "bottomright"
    )
})

# Observe filteredData and update markers accordingly
observe({
  data <- filteredData()
  
  # Assign hex colors for filtered data
  data$ColorHex <- snowColors[data$SnowColor]
  
  leafletProxy("mymap", data = data) %>%
    clearMarkers() %>%  # Remove any current markers
    addCircleMarkers( # Add circle markers using filteredData selections
      lng = ~Longitude,
      lat = ~Latitude,
      radius = 8,
      fillColor = ~ColorHex,
      stroke = FALSE,
      fillOpacity = 0.8,
      weight = 1,
      # Clickable popup for each point
      popup = ~paste(
        "Site:", Location,
        "<br>Date Collected:", CollectionDate,
        "<br>Sample ID:", SampleID,
        "<br>Color:", SnowColor,
        "<br>Cell Density (cells/mL):", CellDensity
      )
    )
})


# Pigment plot by location settings 
output$pigmentPlot <- renderPlot({
  data <- filteredData()
  
  # Pigment column names
  pigment_cols <- c("Astaxanthin", "Lutein", "ChlA", "ChlB", "BetaC", 
                    "Pheophorbide", "Pheophytin")
  
  # Filter and pivot data for bar plot
  pigment_long <- data %>%
    select(Location, all_of(pigment_cols)) %>%
    pivot_longer(cols = all_of(pigment_cols), names_to = "Pigment", values_to = "Concentration")
  
  # Plot
  ggplot(pigment_long, aes(x = Pigment, y = Concentration, fill = Location)) +
    geom_bar(stat = "summary", 
             fun = "mean", 
             position = position_dodge()) +
    scale_fill_manual(values = locationColors) +
    labs(x = NULL, 
         y = "Concentration (µg/L)", 
         fill = NULL) +
    theme_minimal() +
    plot_theme
})

# Pigment plot by color settings
output$colorPlot <- renderPlot({
  data <- filteredData()
  
  # Pigment column names
  pigment_cols <- c("Astaxanthin", "Lutein", "ChlA", "ChlB", "BetaC", 
                    "Pheophorbide", "Pheophytin")
  
  # Filter and pivot data for bar plot
  pigmentColor_long <- data %>%
    select(SnowColor, all_of(pigment_cols)) %>%
    pivot_longer(cols = all_of(pigment_cols), names_to = "Pigment", values_to = "Concentration")
  
  # Plot
  ggplot(pigmentColor_long, aes(x = Pigment, y = Concentration, fill = SnowColor)) +
    geom_bar(stat = "summary", 
             fun = "mean", 
             position = position_dodge()) +
    scale_fill_manual(values = snowColors) +
    labs(x = NULL, 
         y = "Concentration (µg/L)", 
         fill = NULL) +
    theme_minimal() +
    plot_theme
})


# Cell density plot
output$cellPlot <- renderPlot({
  
  # Plot
  ggplot(filteredData(), aes(x = Location, y = CellDensity, fill = SnowColor)) +
    geom_bar(stat = "summary", 
             fun = "mean", 
             position = position_dodge()) +
    scale_fill_manual(values = snowColors) +
    labs(x = NULL, 
         y = "Cell Density (cells/mL)", 
         fill = NULL) +
    theme_minimal() +
    plot_theme
})
}

shinyApp(ui, server)