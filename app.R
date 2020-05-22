## inits ----
library(shiny)
library(dplyr)
library(shinyWidgets)
library(highcharter)

## shiny stuff
button_labels <- c("Release Year", "Duration", "Popularity",
                   "Danceability", "Energy", "Valence", "Tempo")

## data
challenge <- readr::read_csv("data/challenge.csv") %>% 
  mutate(
    Thema  = forcats::as_factor(Thema),
    Person = forcats::as_factor(Person),
    them_n = as.numeric(Thema) - 1,
    # bugfixing: no emojis =(
    # rating = sample(LETTERS, nrow(.), replace = TRUE),
    x = as.numeric(Person) + runif(length(Person), -.3, .3)
  ) %>% 
  rowwise() %>% 
  mutate(
    tooltip = paste0('<a style="margin-right:',
                     max(max(nchar(as.character(Thema)),
                             nchar(artist) + nchar(track) + 5,
                             nchar(album) + 7) * 6, 55), 'px">',
                     '<img src=', img_1, ' style="float:left;margin-right:5px">',
                     '<i>', Thema, '</i><br>',
                     # '<b>', rating, " ", artist, ' - ', track, '</b><br>',
                     '<b>', artist, ' - ', track, '</b><br>',
                     'Album: ', album, '<br>',
                     'Released: ', released)
  )

project_colors <- c("#617a89", "#0b53c1", "#b3e93e", "#909495",
                    "#ffec1b", "#ff0055", "#23d0fc", "#fba29d")

days <- levels(challenge$Thema)

## highcharter stuff
slate_theme_hc <- hc_theme(
  colors = project_colors,
  chart = list(backgroundColor = "#1c1e22"),
  title = list(
    style = list(# fontFamily = "Njord Regular",
                 color = "#d9d9d9")
  ),
  subtitle = list(
    style = list(# fontFamily = "Klavika Regular",
                 color = "#8c8c8c")
  ),
  legend = list(
    itemStyle = list(# fontFamily = "Klavika Regular",
                     color = "#d9d9d9"),
    itemHoverStyle = list(color = "grey")
  ),
  yAxis = list(
    gridLineColor = "#4d4d4d",
    gridLineDashStyle = "Dot",
    labels = list(
      style = list(
        color = "#999999"
      ))
  ),
  tooltip = list(
    backgroundColor = "#43484f"
  )
)


## App layout ----
ui <- fluidPage(
  theme = shinythemes::shinytheme("slate"),
  # alternatives: cosmo, spacelab, slate (dark)
  
  titlePanel("30 Day Music Challenge Explorer"),
  
  ## Sidebar
  sidebarLayout(
    # Selection
    sidebarPanel(
      h3(textOutput("feature")),
      textOutput("feature_description"),
      radioGroupButtons(
        "feature", "", justified = TRUE,
        choices = button_labels,
        selected = "Release Year",
        direction = "vertical"
      )
    ),
    
    # Description
    # sidebarPanel(
    #   h3(textOutput("feature")),
    #   textOutput("feature_description")
    # ),
    
    ## Main Panel
    mainPanel(
      tabsetPanel(
        type = "pills",
        
        tabPanel(
          "nach Person",
          highchartOutput("plot_person", height = "550px")
        ),
        
        tabPanel(
          "nach Challenge",
          highchartOutput("plot_days", height = "800px")
        )
      )
    )
    
  )
)


## server logic ----
server <- function(input, output) {
  output$feature <- renderText({input$feature})
  
  output$feature_description <- renderText({
    switch(input$feature,
           "Release Year" = "Simpel: Wann ist der Track erschienen? Leider mitunter stark verfälscht, 
                             weil ich Suchergebnisse von Spotify nicht besonders kuratiere und 
                             daher zB das Erscheinungsjahr eines Best-Ofs o.ä. angezeigt wird.",
           "Duration"     = "Kommt von Haus aus in Millisekunden und wird auch so dargstellt,
                             weil Programmieren in/mit/über Zeiteinheiten die Hölle ist.
                             Dreieinhalbminuten sind zumindest 210.000ms.",
           "Popularity"   = "Je populärer desto 100. Basierend darauf, wie oft ein Track in letzter Zeit
                             gespielt wurde. Tracks, die vor kurzem oft gespielt wurden, sind populärer, 
                             auch wenn ein anderer Track zwar öfter gespielt wurde, aber eben vor längerer
                             Zeit. Im Gegensatz zu den anderen Variablen verändert sich die popularity also
                             auch im Laufe der Zeit.",
           "Danceability" = "Bildet ab was draufsteht: Danceability setzt sich zusammen aus Elementen wie
                             Tempo, Rhythmusstabilität doer wie dominant der Beat ist.",
           "Energy"       = "Eine relative Angabe über die Intensität und Regsamkeit eines Tracks.",
           "Valence"      = "Tracks mit einer hohen Valenz sind musikalisch eher positiv und transportieren
                             Eigenschaften wie Glück, Fröhlichkeit oder sogar Euphorie. Niedrige Werte 
                             dagegen weisen eher auf Traurigkeit, Melancholie oder Wut.",
           "Tempo"        = "Im Grunde nicht mehr als die mittleren Beats Per Minute (BPM) eines Tracks.")
  })
  
  output$plot_person <- renderHighchart({
    challenge$y <- switch(input$feature,
                          "Release Year" = challenge$released,
                          "Duration"     = challenge$dur_ms,
                          "Popularity"   = challenge$popularity,
                          "Danceability" = challenge$danceability,
                          "Energy"       = challenge$energy,
                          "Valence"      = challenge$valence,
                          "Tempo"        = challenge$tempo)
    
    labels_hc <- switch(
      input$feature,
      # [1] title;  [2] subtitle;  [3] yAxis title
      "Release Year" = c("Erscheinungsjahr", "mitunter verfälscht", "Jahr"),
      "Duration"     = c("Dauer der Tracks", "60.000ms = 1 Minute", "Dauer (in ms)"),
      "Popularity"   = c("Pop Pop Pop Sofa", "", "Popularity"),
      "Danceability" = c("Aka der Bootyshake.Index", "shake your tailfeather", "Danceability"),
      "Energy"       = c("Berzerk vs, Slowpoke", "", "Energy"),
      "Valence"      = c("Gloom or Gloria", "", "Valence"),
      "Tempo"        = c("Faster Pussy Cat", "(kill kill)", "BPM")
    )
    
    hc <- hchart(challenge, "scatter", hcaes(x = x, y = y, group = Person)) %>% 
      hc_tooltip(formatter = JS(paste0("function() {return this.point.tooltip;}")), useHTML = T) %>% 
      hc_xAxis(labels = list(enabled = F),
               title = list(text = "Person")) %>% 
      hc_yAxis(title = list(text = labels_hc[3])) %>% 
      hc_title(text = labels_hc[1]) %>% 
      hc_subtitle(text = labels_hc[2]) %>% 
      hc_add_theme(slate_theme_hc)
    
    hc
  })
  
  output$plot_days <- renderHighchart({
    challenge$y <- switch(input$feature,
                          "Release Year" = challenge$released,
                          "Duration"     = challenge$dur_ms,
                          "Popularity"   = challenge$popularity,
                          "Danceability" = challenge$danceability,
                          "Energy"       = challenge$energy,
                          "Valence"      = challenge$valence,
                          "Tempo"        = challenge$tempo)
    
    labels_hc <- switch(
      input$feature,
      # [1] title;  [2] subtitle;  [3] yAxis title
      "Release Year" = c("Erscheinungsjahr", "mitunter verfälscht", "Jahr"),
      "Duration"     = c("Dauer der Tracks", "60.000ms = 1 Minute", "Dauer (in ms)"),
      "Popularity"   = c("Pop Pop Pop Sofa", "", "Popularity"),
      "Danceability" = c("Aka der Bootyshake.Index", "shake your tailfeather", "Danceability"),
      "Energy"       = c("Berzerk vs, Slowpoke", "", "Energy"),
      "Valence"      = c("Gloom or Gloria", "", "Valence"),
      "Tempo"        = c("Faster Pussy Cat", "(kill kill)", "BPM")
    )
    
    hc <- hchart(challenge, "scatter", hcaes(x = y, y = them_n, group = Person)) %>% 
      hc_tooltip(formatter = JS(paste0("function() {return this.point.tooltip;}")), useHTML = T) %>%
      hc_xAxis(title = list(text = labels_hc[3])) %>%
      hc_yAxis(title = list(text = "Challenge"),
               categories = days) %>%
      hc_title(text = labels_hc[1]) %>%
      hc_subtitle(text = labels_hc[2]) %>%
      hc_add_theme(slate_theme_hc)
    
    hc
  })
}


## run App ----
shinyApp(ui = ui, server = server)