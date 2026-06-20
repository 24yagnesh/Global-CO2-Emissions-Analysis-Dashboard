#SHINY APP CODE
library(rvest)
library(dplyr)
library(ggplot2)
library(shiny)

data=read.csv("Cleaned_Data.csv")
theme_modern  <- function() {
  theme_minimal(base_family = "Poppins") +
    theme(
      plot.title = element_text(size = 18, face = "bold", color = "#222", hjust = 0.5),
      plot.subtitle = element_text(size = 11, color = "#555", hjust = 0.5),
      axis.title = element_text(size = 12, face = "bold", color = "#333"),
      axis.text = element_text(size = 10, color = "#222"),
      panel.grid.major = element_line(color = "#f0f0f0"),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "white", color = NA)
    )
}

#------------------------------------------------------
#4.SHINY UI
#------------------------------------------------------
ui  <- fluidPage(
  tags$head(
    tags$link(
      href = "https://fonts.googleapis.com/css2?family=Google Sans:wght@300;400;500;600;700&display=swap",
      rel = "stylesheet"
    ),
    tags$style("body { font-family: 'Google Sans', sans-serif; }")
  ),
  titlePanel("Global CO₂ Emissions in 2022 Dashboard (Worldometer)"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("top_n", h4("Top N countries to display:"), 5, 40, 10),
      sliderInput("pop_range", h4("Population range (millions):"), 0, 1500, c(0, 1500)),
      sliderInput("pc_range", h4("Per capita CO₂ (tons):"), 0, 40, c(0, 40)),
      sliderInput("em_range", h4("Total emissions (tons, billions):"), 0, 12000, c(0, 12000)),
      width = 3
    ),
    mainPanel(
      fluidRow(
        column(3, div(class="box", h4(tags$p("Total Emissions(tons)", style = "color:green;")), h2(textOutput("total_em")))),
        column(3, div(class="box", h4(tags$p("Mean per capita(tons)", style = "color:navy;")), h2(textOutput("mean_pc")))),
        column(3, div(class="box", h4(tags$p("Median Emissions(tons)", style = "color:green;")), h2(textOutput("median_em")))),
        column(3, div(class="box", h4(tags$p("Corr(Emisions,Popln.)", style = "color:navy;")), h2(textOutput("corr_em_pop"))))
      ),
      fluidRow(
        column(6, div(class="box", h4(tags$p("Highest Emitter", style = "color:orange;")), h2(textOutput("top_country")))),
        column(6, div(class="box", h4(tags$p("Lowest Emitter", style = "color:orange;")), h2(textOutput("low_country"))))
      ),
      tabsetPanel(
        tabPanel(h4("Top Emitters"), plotOutput("top_emitters", height = "420px")),
        tabPanel(h4("Scatter Plot"), plotOutput("scatter_plot", height = "420px")),
        tabPanel(h4("Density Plot"), plotOutput("density_plot", height = "420px")),
        tabPanel(h4("CO₂ Share Pie Chart"), plotOutput("pie_chart", height = "520px")),
        tabPanel(h4("Key Findings"), verbatimTextOutput("summary_text")),
        tabPanel(h4("About"),verbatimTextOutput("about"))
      )
    )
  )
)

#------------------------------------------------------
# 5. SERVER
#------------------------------------------------------
server  <- function(input, output, session) {
  filtered_data  <- reactive({
    data %>%
      filter(
        population_2022 / 1e6 >= input$pop_range[1],
        population_2022 / 1e6 <= input$pop_range[2],
        per_capita_tons >= input$pc_range[1],
        per_capita_tons <= input$pc_range[2],
        emissions_tons / 1e9 >= input$em_range[1],
        emissions_tons / 1e9 <= input$em_range[2]
      )
  })
  
  #------------------ METRICS ------------------
  output$total_em  <- renderText({
    format(sum(filtered_data()$emissions_tons, na.rm = TRUE), big.mark = ",")
  })
  output$mean_pc  <- renderText({
    round(mean(filtered_data()$per_capita_tons, na.rm = TRUE), 2)
  })
  output$median_em  <- renderText({
    format(round(median(filtered_data()$emissions_tons, na.rm = TRUE), 0), big.mark = ",")
  })
  output$corr_em_pop  <- renderText({
    round(cor(filtered_data()$emissions_tons, filtered_data()$population_2022, use = "complete.obs"), 3)
  })
  output$top_country  <- renderText({
    filtered_data() %>% arrange(desc(emissions_tons)) %>% slice(1) %>% pull(country)
  })
  output$low_country  <- renderText({
    filtered_data() %>% arrange(emissions_tons) %>% slice(1) %>% pull(country)
  })
  
  #------------------ PLOTS ------------------
  output$top_emitters  <- renderPlot({
    df  <- filtered_data() %>% arrange(desc(emissions_tons)) %>% head(input$top_n)
    ggplot(df, aes(x = reorder(country, emissions_tons), y = emissions_tons, fill = country)) +
      geom_col(show.legend = FALSE) +
      geom_text(aes(label = round(emissions_tons/1e9, 2)), hjust = -0.2, size = 4.3, color = "black") +
      coord_flip() +
      labs(
        title = expression("Top CO"[2] * " Emitting Countries"),
        subtitle = paste("Countries Displayed:", paste(df$country, collapse = ", ")),
        x = "Country",
        y = expression("Total CO"[2] * " Emissions (tons)")
      ) +
      theme_modern()+theme(axis.title.x = element_text(size = 7),axis.title.y = element_text(size = 7))
  })
  
  output$scatter_plot  <- renderPlot({
    df  <- filtered_data()
    ggplot(df, aes(x = population_2022 / 1e6, y = emissions_tons, color = per_capita_tons, label = country)) +
      geom_point(size = 3.2, alpha = 0.8) +
      geom_text(vjust = -1, size = 3, check_overlap = TRUE) +
      scale_color_gradient(low = "#2ecc71", high = "#c0392b") +
      labs(
        title = expression("CO"[2] * " Emissions vs Population"),
        x = "Population (millions)",
        y = expression("Total CO"[2] * " Emissions (tons)"),
        color = expression("Per Capita CO"[2] * " (tons)")
      ) +
      theme_modern()
  })
  
  output$density_plot  <- renderPlot({
    df  <- filtered_data()
    ggplot(df, aes(x = per_capita_tons)) +
      geom_density(fill = "skyblue", alpha = 0.7, color = "#333") +
      labs(
        title = expression("Distribution of Per Capita CO"[2] * " Emissions"),
        x = expression("Per Capita CO"[2] * " (tons/person)"),
        y = "Density"
      ) +
      theme_modern()
  })
  
  
  output$pie_chart  <- renderPlot({
    df  <- filtered_data() %>% arrange(desc(share_percent))
    top_df  <- df %>% slice_head(n = 10)
    others_share  <- sum(df$share_percent, na.rm = TRUE) - sum(top_df$share_percent, na.rm = TRUE)
    pie_df  <- bind_rows(top_df, data.frame(country = "Others", share_percent = others_share))
    pie_colors  <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd",
                     "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf", "#f2b447")
    ggplot(pie_df, aes(x = "", y = share_percent, fill = country)) +
      geom_bar(stat = "identity", width = 1, color = "white") +
      coord_polar(theta = "y") +
      geom_text(aes(label = paste0(round(share_percent, 1), "%")),
                position = position_stack(vjust = 0.5), size = 4, color = "white") +
      scale_fill_manual(values = pie_colors) +
      labs(
        title = expression("Percentage Share of Global CO"[2] * " Emissions "),
        subtitle = paste("Countries Displayed:", paste(top_df$country, collapse = ", ")),
        fill = "Country"
      ) +
      theme_void() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10),
        legend.position = "right",
        legend.text = element_text(size = 9)
      )
  })
  
  output$summary_text  <- renderPrint({
    df  <- filtered_data()
    cat("---- Summary of Key Metrics ----\n")
    cat("Countries considered:", nrow(df), "\n")
    cat("Average per capita emissions:", round(mean(df$per_capita_tons, na.rm = TRUE), 2), "\n")
    cat("Average global share (%):", round(mean(df$share_percent, na.rm = TRUE), 2), "\n")
    cat("Variance in emissions:", round(var(df$emissions_tons, na.rm = TRUE), 2), "\n")
    cat("Correlation (CO₂ vs Pop):", round(cor(df$emissions_tons, df$population_2022, use = 'complete.obs'), 3), "\n")
  })
  output$about  <- renderPrint({
    cat("The data used in this application is scraped from \n","Worldometers- CO2 Emissions by country \n")
    cat("which compiles the most recent and reliable CO₂ statistics from international databases such as: \n Global Carbon Project \n IEA (International Energy Agency) \n UN Statistics Division \n")
    cat("Each record contains: \n 1.Country name \n 2.Annual CO₂ emissions (in million tonnes) \n 3.CO₂ emissions per capita \n 4.Share of global emissions (%) \n 5.Population \n 6.Emission change (%) from the previous year \n")
    cat("
    A. Data Cleaning & Processing: \n
  After scraping, the dataset was cleaned and standardized: \n
      1.Removed commas, special characters, and non-numeric symbols using gsub(). \n
      2.Converted numeric fields into proper numeric formats. \n
      3.Checked for missing or inconsistent values. \n
      4.Renamed columns for readability and consistency. \n
      5.Suppressed unnecessary console warnings using suppressWarnings() to ensure smooth execution. \n

This cleaning ensures that all visualizations and summaries are based on consistent, reliable data. \n")
    cat("
    B. The methodology of data analysis focuses on: \n
      1.Understanding distribution patterns of CO₂ emissions globally.  \n
      2.Comparing total vs. per capita emissions. \n
      3.Relating emission levels to population categories. \n
      4.Highlighting top and bottom emitters interactively. \n")
    cat("
    C. Future Enhancements and planned extensions include: \n
      -Adding time-series data to track emission changes over years. \n
      -Integrating GDP data to study economic–environmental correlations. \n
      -Incorporating renewable energy metrics for deeper insights. \n")
    
  })
}

#------------------------------------------------------
# 6. RUN APP
#------------------------------------------------------
shinyApp(ui, server)