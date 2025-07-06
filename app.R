# ===================================================================
# MEMUAT LIBRARY YANG DIBUTUHKAN
# ===================================================================
if(!requireNamespace("shiny", quietly = TRUE)) install.packages("shiny")
if(!requireNamespace("DT", quietly = TRUE)) install.packages("DT")
if(!requireNamespace("plotly", quietly = TRUE)) install.packages("plotly")
if(!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
if(!requireNamespace("leaflet", quietly = TRUE)) install.packages("leaflet")
if(!requireNamespace("forecast", quietly = TRUE)) install.packages("forecast")
if(!requireNamespace("tseries", quietly = TRUE)) install.packages("tseries")
if(!requireNamespace("sf", quietly = TRUE)) install.packages("sf")
if(!requireNamespace("shinyjs", quietly = TRUE)) install.packages("shinyjs")
if(!requireNamespace("nortest", quietly = TRUE)) install.packages("nortest")
if(!requireNamespace("car", quietly = TRUE)) install.packages("car")
if(!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
if(!requireNamespace("corrplot", quietly = TRUE)) install.packages("corrplot")
if(!requireNamespace("GGally", quietly = TRUE)) install.packages("GGally")
if(!requireNamespace("cluster", quietly = TRUE)) install.packages("cluster")
if(!requireNamespace("factoextra", quietly = TRUE)) install.packages("factoextra")
if(!requireNamespace("jsonlite", quietly = TRUE)) install.packages("jsonlite")

library(shiny)
library(sf)
library(leaflet)
library(dplyr)
library(DT)
library(plotly)
library(shinyjs)
library(forecast)
library(tseries)
library(nortest)
library(car)
library(ggplot2)
library(corrplot)
library(GGally)
library(cluster)
library(factoextra)
library(jsonlite)

# ===================================================================
# DATA SAMPEL & FUNGSI BANTU
# ===================================================================
add_months <- function(date, n) {
  seq(date, by = "month", length.out = n + 1)[n + 1]
}

sample_peta_data <- data.frame(
  Kabupaten = c("Bandung", "Bekasi", "Bogor", "Cianjur", "Cirebon", "Garut", "Indramayu", 
                "Karawang", "Kuningan", "Majalengka", "Purwakarta", "Subang", "Sukabumi",
                "Sumedang", "Tasikmalaya", "Bandung Barat", "Pangandaran", "Ciamis", "Depok",
                "Cimahi", "Banjar"),
  Produksi_Padi = c(580000, 420000, 510000, 390000, 680000, 450000, 750000,
                    820000, 280000, 360000, 180000, 720000, 320000,
                    250000, 480000, 200000, 150000, 300000, 200, 
                    400, 30000)
)

# Data sampel untuk clustering (lebih komprehensif)
sample_clustering_data <- data.frame(
  Daerah = c("Bandung", "Bekasi", "Bogor", "Cianjur", "Cirebon", "Garut", "Indramayu", 
             "Karawang", "Kuningan", "Majalengka", "Purwakarta", "Subang", "Sukabumi",
             "Sumedang", "Tasikmalaya", "Bandung Barat", "Pangandaran", "Ciamis", "Depok",
             "Cimahi", "Banjar"),
  Produksi_Ton = c(580000, 420000, 510000, 390000, 680000, 450000, 750000,
                   820000, 280000, 360000, 180000, 720000, 320000,
                   250000, 480000, 200000, 150000, 300000, 200, 
                   400, 30000),
  Luas_Lahan_Ha = c(45000, 35000, 42000, 38000, 52000, 40000, 60000,
                    65000, 25000, 30000, 18000, 58000, 28000,
                    22000, 38000, 20000, 15000, 25000, 50,
                    80, 2500),
  Hasil_Per_Ha = c(12.9, 12.0, 12.1, 10.3, 13.1, 11.3, 12.5,
                   12.6, 11.2, 12.0, 10.0, 12.4, 11.4,
                   11.4, 12.6, 10.0, 10.0, 12.0, 4.0,
                   5.0, 12.0),
  Curah_Hujan_mm = c(2200, 1800, 2500, 2300, 1600, 2400, 1400,
                     1500, 2100, 1900, 2000, 1700, 2800,
                     2200, 2600, 2400, 2700, 2500, 1200,
                     1300, 2300),
  Suhu_Rata_C = c(24.5, 27.2, 25.1, 24.8, 28.1, 23.9, 28.5,
                  27.8, 24.2, 25.5, 25.8, 27.0, 23.5,
                  24.1, 24.0, 24.2, 26.5, 25.0, 28.0,
                  28.2, 24.8),
  Kelembapan_Persen = c(82, 78, 84, 83, 75, 85, 73,
                        76, 84, 80, 81, 77, 87,
                        83, 86, 84, 88, 85, 70,
                        72, 83)
)

set.seed(123)
sample_sarimax_data <- data.frame(
  No = 1:72,
  Bulan = rep(1:12, 6),
  Tahun = rep(2019:2024, each = 12),
  Produksi.Padi = rnorm(72, mean = 950000, sd = 20000),
  Curah.Hujan = rnorm(72, mean = 180, sd = 30),
  Suhu = rnorm(72, mean = 27, sd = 0.5),
  Kelembapan = rnorm(72, mean = 75, sd = 5),
  Sinar.Matahari = rnorm(72, mean = 7, sd = 1)
)

# ===================================================================
# USER INTERFACE (UI)
# ===================================================================
ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$link(rel = "shortcut icon", href = "jabar.png"),
    tags$style(HTML(paste(readLines("www/styles.css", warn = FALSE), collapse=" "))),
    tags$script(HTML("
      $(document).ready(function() {
        function makeDraggable() {
          $('.variable-item').attr('draggable', 'true');
          
          $('.variable-item').on('dragstart', function(e) {
            e.originalEvent.dataTransfer.setData('text/plain', $(this).data('variable'));
            $(this).addClass('dragging');
          });
          
          $('.variable-item').on('dragend', function(e) {
            $(this).removeClass('dragging');
          });
        }
        
        $('.drop-zone').on('dragover', function(e) {
          e.preventDefault();
          $(this).addClass('drag-over');
        });
        
        $('.drop-zone').on('dragleave', function(e) {
          $(this).removeClass('drag-over');
        });
        
        $('.drop-zone').on('drop', function(e) {
          e.preventDefault();
          $(this).removeClass('drag-over');
          
          var variableName = e.originalEvent.dataTransfer.getData('text/plain');
          var zoneType = $(this).data('zone');
          
          if (variableName && zoneType) {
            $(this).find('.dropped-variable').remove();
            $(this).append('<div class=\"dropped-variable\" data-variable=\"' + variableName + '\">' + 
                          '<span>' + variableName + '</span>' +
                          '<button class=\"remove-var\" onclick=\"removeVariable(this)\">×</button>' +
                          '</div>');
            Shiny.setInputValue('dropped_' + zoneType, variableName, {priority: 'event'});
          }
        });
        
        $(document).on('shiny:value', function(event) {
          if (event.target.id === 'variable_bank') {
            setTimeout(makeDraggable, 100);
          }
        });
        
        // Clear drop zones when data changes
        $(document).on('shiny:inputchanged', function(event) {
          if (event.name === 'explorationDataFile' || event.name === 'resetExplorationData') {
            $('.drop-zone .dropped-variable').remove();
          }
        });
      });
      
      function removeVariable(btn) {
        var dropZone = $(btn).closest('.drop-zone');
        var zoneType = dropZone.data('zone');
        $(btn).parent().remove();
        Shiny.setInputValue('dropped_' + zoneType, '', {priority: 'event'});
      }
    "))
  ),
  
  # --- Navigation Bar ---
  tags$div(class = "navbar",
           tags$div(class = "navbar-content",
                    tags$div(class = "logo",
                             style = "display: flex; align-items: center; cursor: pointer;",
                             onclick = "Shiny.setInputValue('current_page', 'welcome')",
                             
                             # Logo di sebelah kiri
                             tags$img(src = "jabar.png", style = "height: 30px; margin-right: 10px;"),
                             
                             # Teks di sebelah kanan
                             tags$span("Produksi Padi Jawa Barat", style = "font-weight: bold; font-size: 18px;")
                    ),
                    
                    tags$div(class = "nav-links",
                             tags$button("Beranda", class = "nav-link", id = "nav-welcome",
                                         onclick = "Shiny.setInputValue('current_page', 'welcome')"),
                             tags$button("Teori & User Guide", class = "nav-link", id = "nav-teori",
                                         onclick = "Shiny.setInputValue('current_page', 'teori')"),
                             tags$button("Visualisasi Peta", class = "nav-link", id = "nav-peta",
                                         onclick = "Shiny.setInputValue('current_page', 'peta')"),
                             tags$button("Eksplorasi Data", class = "nav-link", id = "nav-eksplorasi",
                                         onclick = "Shiny.setInputValue('current_page', 'eksplorasi')"),
                             tags$button("Clustering", class = "nav-link", id = "nav-clustering",
                                         onclick = "Shiny.setInputValue('current_page', 'clustering')"),
                             tags$button("Forecasting", class = "nav-link", id = "nav-sarimax",
                                         onclick = "Shiny.setInputValue('current_page', 'sarimax')")
                    )
           )
  ),
  
  # --- Main Content ---
  tags$div(class = "main-content",
           
           # --- Halaman Beranda (Welcome Page) ---
           conditionalPanel(
             condition = "input.current_page == 'welcome' || input.current_page == undefined || input.current_page == null",
             tags$div(class = "hero-section",
                      tags$div(class = "video-background",
                               tags$iframe(
                                 src = "https://www.youtube.com/embed/qH1jph2ldSg?autoplay=1&mute=1&loop=1&playlist=qH1jph2ldSg&controls=0",
                                 frameborder = "0",
                                 allow = "autoplay",
                                 allowfullscreen = TRUE
                               )
                      ),
                      
                      tags$div(class = "video-overlay"),
                      tags$div(class = "hero-content",
                               tags$h1("PRODUKSI PADI DI JAWA BARAT"),
                               tags$p("Analisis Spasial dan Peramalan Produksi", style="font-size:20px;"),
                               tags$div(
                                 style = "margin-top: 40px;",
                                 tags$button(
                                   "Mulai Eksplorasi",
                                   class = "btn btn-primary btn-lg",
                                   style = "padding: 12px 30px; font-size: 2rem;",
                                   onclick = "Shiny.setInputValue('current_page', 'peta')"
                                 )
                               )
                      )
             ),
             tags$div(class = "geographic-section",
                      tags$div(class = "content-section",
                               tags$h2("Kondisi Geografis Jawa Barat", class = "section-title"),
                               tags$div(class = "section-content",
                                        tags$p("Jawa Barat merupakan salah satu provinsi di Indonesia yang memiliki potensi besar dalam sektor pertanian, khususnya produksi padi. Provinsi ini terletak di bagian barat Pulau Jawa dengan luas wilayah sekitar 35.377,76 km² dan terdiri dari 18 kabupaten dan 9 kota.", style="font-size:20px;"),
                                        tags$p("Kondisi geografis Jawa Barat sangat mendukung untuk kegiatan pertanian padi. Provinsi ini memiliki dataran rendah yang luas, terutama di bagian utara dan tengah, dengan ketinggian antara 0-1.000 meter di atas permukaan laut. Iklim tropis dengan curah hujan yang cukup tinggi, berkisar antara 2.000-4.000 mm per tahun, menjadikan wilayah ini sangat cocok untuk budidaya padi.", style="font-size:20px;")
                               )
                      )
             ),
             
             # Bagian ini dihapus sesuai permintaan
             
             ### PERUBAHAN: Mengganti grafik di Beranda
             tags$div(class = "content-section",
                      tags$h2("Visualisasi Data Padi", class = "section-title"),
                      fluidRow(
                        column(12, # Mengubah lebar kolom menjadi 12
                               tags$div(class = "chart-container",
                                        h3("Rata-Rata Produksi Padi per Kabupaten/Kota (2018-2024)", style="text-align:center;"),
                                        # Mengubah ID output plotly
                                        plotlyOutput("beranda_avg_prod_chart", height = "500px") 
                               )
                        )
                      ),
                      tags$hr(),
                      tags$div(class = "video-container",
                               h2("Video Tutorial Penggunaan Aplikasi", class = "section-title"),
                               p("Video berikut memberikan gambaran singkat mengenai fitur dan cara penggunaan aplikasi ini.", style="text-align:center; font-size:20px;"),
                               tags$div(
                                 style = "display: flex; justify-content: center; margin-top: 20px;",
                                 tags$iframe(
                                   width = "896", 
                                   height = "504", 
                                   src = "https://www.youtube.com/embed/qH1jph2ldSg?si=8IbSuHPO6lDTAZlO", # URL Embed video YouTube
                                   title = "YouTube video player", 
                                   frameborder = "0", 
                                   allow = "accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture", 
                                   allowfullscreen = TRUE
                                 )
                               )
                      )
             )
           ),
           
           # --- Halaman Teori & User Guide ---
           ### PERUBAHAN 2: Mengganti konten halaman teori dengan tabsetPanel untuk PDF
           conditionalPanel(
             condition = "input.current_page == 'teori'",
             tags$div(class = "page-header",
                      tags$h1("Landasan Teori & Panduan Pengguna", class = "page-title"),
                      tags$p("Konsep, Metodologi, dan Cara Penggunaan Aplikasi", class = "page-subtitle ")
             ),
             tabsetPanel(
               type = "tabs",
               id = "teori_tabs",
               tabPanel("Landasan Teori",
                        tags$div(class = "pdf-container",
                                 tags$h3("Dokumen Teori", style="text-align:center; margin-top:20px;"),
                                 
                                 tags$iframe(style="height:800px; width:100%; border:none;", src="teori.pdf")
                        )
               ),
               tabPanel("User Guide",
                        tags$div(class = "pdf-container",
                                 tags$h3("Panduan Penggunaan Aplikasi", style="text-align:center; margin-top:20px;"),
                                 
                                 tags$iframe(style="height:800px; width:100%; border:none;", src="userguide.pdf")
                        )
               )
             ),
             
             # --- PENAMBAHAN FITUR DOWNLOAD TEMPLATE ---
             tags$div(class = "content-section",
                      tags$h2("Unduh Template Data", class = "section-title"),
                      tags$p("Gunakan template berikut untuk memastikan format data Anda sesuai dengan yang dibutuhkan oleh aplikasi pada setiap menu.", style="text-align:center; font-size: 20px !important;"),
                      br(),
                      fluidRow(
                        column(3, downloadButton("downloadVisualisasiTemplate", "Template Visualisasi", class="btn-analyze", style="width:100%; font-size: 14px;")),
                        column(3, downloadButton("downloadEksplorasiTemplate", "Template Eksplorasi", class="btn-analyze", style="width:100%; font-size: 14px;")),
                        column(3, downloadButton("downloadClusteringTemplate", "Template Clustering", class="btn-analyze", style="width:100%; font-size: 14px;")),
                        column(3, downloadButton("downloadForecastTemplate", "Template Forecast", class="btn-analyze", style="width:100%; font-size: 14px;"))
                      )
             )
             # --- AKHIR PENAMBAHAN FITUR ---
           ),
           
           # --- Halaman Visualisasi Peta ---
           conditionalPanel(
             condition = "input.current_page == 'peta'",
             
             tags$div(class = "page-header",
                      tags$h1("Visualisasi Spasial", class = "page-title"),
                      tags$p("Peta Interaktif Produksi Padi Jawa Barat", class="gede")
             ),
             
             tags$div(class = "controls-panel",
                      fluidRow(
                        column(12,
                               radioButtons("mapMode", "Pilih Mode Tampilan Peta:",
                                            choices = list("Nilai Absolut Produksi" = "absolute", 
                                                           "Perbandingan Tahunan (Kenaikan/Penurunan)" = "comparison"),
                                            selected = "absolute", inline = TRUE)
                        )
                      ),
                      hr(),
                      
                      tags$h4("Gunakan Data Anda Sendiri (Berdasarkan Tahun)", style="text-align:center; color: #2c3e50;"),
                      fluidRow(
                        column(4,
                               fileInput("mapDataFile", "Unggah Data Produksi (CSV):", 
                                         accept = ".csv", buttonLabel = "Pilih File", placeholder = "Tidak ada file dipilih")
                        ),
                        column(4,
                               uiOutput("yearSelectorUI")
                        ),
                        column(4, style="padding-top:25px;",
                               actionButton("applyUploadedData", "Terapkan Data Tahunan", class="btn-analyze", style="width: 48%;"),
                               actionButton("resetToSampleData", "Reset ke Data Contoh", style="width: 48%;")
                        )
                      )
             ),
             
             tags$div(class = "visualization-container",
                      tags$div(class = "viz-tabs",
                               tags$div("Peta Interaktif", class = "viz-tab active", id = "map-tab", onclick = "Shiny.setInputValue('viz_tab', 'map')"),
                               tags$div("Grafik Perbandingan", class = "viz-tab", id = "chart-tab", onclick = "Shiny.setInputValue('viz_tab', 'chart')"),
                               tags$div("Tabel Data", class = "viz-tab", id = "table-tab", onclick = "Shiny.setInputValue('viz_tab', 'table')")
                      ),
                      
                      tags$div(class = "viz-content",
                               conditionalPanel(
                                 condition = "input.viz_tab == 'map'",
                                 conditionalPanel(
                                   condition = "input.mapMode == 'absolute'",
                                   tags$div(class = "summary-stats",
                                            tags$div(class = "stat-box", tags$div(textOutput("stat_max"), class = "stat-value"), tags$div("Nilai Maksimum", class = "stat-label")),
                                            tags$div(class = "stat-box", tags$div(textOutput("stat_min"), class = "stat-value"), tags$div("Nilai Minimum", class = "stat-label")),
                                            tags$div(class = "stat-box", tags$div(textOutput("stat_mean"), class = "stat-value"), tags$div("Rata-rata", class = "stat-label")),
                                            tags$div(class = "stat-box", tags$div(textOutput("stat_total"), class = "stat-value"), tags$div("Total/Jumlah", class = "stat-label"))
                                   )
                                 ),
                                 leafletOutput("map", height = "500px")
                               ),
                               conditionalPanel(condition = "input.viz_tab == 'chart'", 
                                                # PERBAIKAN MASALAH 2: Menambahkan loading message dan optimize rendering
                                                tags$div(id = "chart-loading", style = "text-align: center; padding: 20px;", 
                                                         tags$p("Memuat grafik...", style = "color: #7f8c8d;")),
                                                plotOutput("comparison_chart_optimized", height = "500px")),
                               conditionalPanel(condition = "input.viz_tab == 'table'", DT::dataTableOutput("data_table"))
                      )
             )
           ),
           
           # --- Halaman Eksplorasi Data ---
           conditionalPanel(
             condition = "input.current_page == 'eksplorasi'",
             
             tags$div(class = "page-header",
                      tags$h1("Eksplorasi Data", class = "page-title"),
                      tags$p("Analisis Eksplorasi Data Interaktif dengan Drag & Drop", class = "page-subtitle")
             ),
             
             # Upload Data Section
             tags$div(class = "controls-panel",
                      tags$h4("Upload Data untuk Eksplorasi", style="text-align:center; color: #2c3e50;"),
                      fluidRow(
                        column(6,
                               fileInput("explorationDataFile", "Unggah File Data (CSV):", 
                                         accept = ".csv", buttonLabel = "Pilih File", placeholder = "Tidak ada file dipilih")
                        ),
                        column(6, style="padding-top:25px;",
                               actionButton("resetExplorationData", "Reset Data", class="btn-analyze", style="width: 100%;")
                        )
                      )
             ),
             
             # Drag and Drop Variable Selection
             conditionalPanel(
               condition = "output.explorationDataLoaded",
               tags$div(class = "controls-panel",
                        tags$h4("Drag & Drop Variabel untuk Analisis", style="text-align:center; color: #2c3e50;"),
                        
                        # Variable Bank
                        fluidRow(
                          column(12,
                                 tags$div(class = "variable-bank",
                                          tags$h5("Bank Variabel", style="color: #2c3e50; font-weight: bold; margin-bottom: 15px;"),
                                          tags$p("Drag variabel di bawah ini ke area yang sesuai:", style="color: #7f8c8d; margin-bottom: 10px;"),
                                          uiOutput("variable_bank")
                                 )
                          )
                        ),
                        
                        br(),
                        
                        # Drop Zones
                        fluidRow(
                          column(4,
                                 tags$div(class = "drop-zone", `data-zone` = "dependent",
                                          tags$h5("Variabel Dependen", style="color: #e74c3c; font-weight: bold; text-align: center;"),
                                          tags$p("Drop variabel target di sini", style="color: #95a5a6; text-align: center; font-style: italic;")
                                 )
                          ),
                          column(4,
                                 tags$div(class = "drop-zone", `data-zone` = "independent",
                                          tags$h5("Variabel Independen", style="color: #3498db; font-weight: bold; text-align: center;"),
                                          tags$p("Drop variabel analisis di sini", style="color: #95a5a6; text-align: center; font-style: italic;")
                                 )
                          ),
                          column(4,
                                 tags$div(class = "drop-zone", `data-zone` = "categorical",
                                          tags$h5("Variabel Kategorikal", style="color: #f39c12; font-weight: bold; text-align: center;"),
                                          tags$p("Drop variabel kategori di sini (opsional)", style="color: #95a5a6; text-align: center; font-style: italic;")
                                 )
                          )
                        )
               )
             ),
             
             # Analysis Tabs Section
             conditionalPanel(
               condition = "output.explorationDataLoaded && output.hasIndependentVar",
               tags$div(class = "visualization-container",
                        
                        # Tab Navigation for Analysis Types
                        # PERBAIKAN MASALAH 1: Mengubah "Eksplorasi Independen Saja" menjadi "Eksplorasi Dependen Saja"
                        tags$div(class = "exploration-tabs",
                                 tags$div("Eksplorasi Dependen Saja", class = "exploration-tab active", id = "dependent-only-tab", 
                                          onclick = "Shiny.setInputValue('exploration_tab', 'dependent_only')"),
                                 tags$div("Eksplorasi Independen + Dependen", class = "exploration-tab", id = "independent-dependent-tab", 
                                          onclick = "Shiny.setInputValue('exploration_tab', 'independent_dependent')"),
                                 tags$div("Eksplorasi Independen + Kategorik", class = "exploration-tab", id = "independent-categorical-tab", 
                                          onclick = "Shiny.setInputValue('exploration_tab', 'independent_categorical')")
                        ),
                        
                        # Tab 1: Eksplorasi Dependen Saja (DIUBAH DARI INDEPENDEN SAJA)
                        conditionalPanel(
                          condition = "input.exploration_tab == 'dependent_only' || input.exploration_tab == undefined || input.exploration_tab == null",
                          tags$div(class = "tab-content-exploration",
                                   tags$div(class = "controls-panel",
                                            tags$h4("Analisis Variabel Dependen Secara Individual", style="text-align:center; color: #2c3e50;"),
                                            fluidRow(
                                              column(12, style="text-align:center;",
                                                     actionButton("btnHistogramDep", "Histogram", class="btn-analyze"),
                                                     actionButton("btnBoxplotDepSingle", "Boxplot", class="btn-analyze"),
                                                     actionButton("btnDescriptiveDep", "Statistik Deskriptif", class="btn-analyze"),
                                                     actionButton("btnNormalityDep", "Uji Normalitas", class="btn-analyze")
                                              )
                                            )
                                   ),
                                   
                                   # Results for Dependent Only
                                   tags$div(class = "viz-content",
                                            conditionalPanel(condition = "output.showHistogramDep", tags$div(class = "analysis-result", tags$h3("Histogram Variabel Dependen"), plotlyOutput("histogramDepPlot", height = "400px"))),
                                            conditionalPanel(condition = "output.showBoxplotDepSingle", tags$div(class = "analysis-result", tags$h3("Boxplot Variabel Dependen"), plotlyOutput("boxplotDepSinglePlot", height = "400px"))),
                                            conditionalPanel(condition = "output.showDescriptiveDep", tags$div(class = "analysis-result", tags$h3("Statistik Deskriptif"), DT::dataTableOutput("descriptiveDepStats"))),
                                            conditionalPanel(condition = "output.showNormalityDep", tags$div(class = "analysis-result", tags$h3("Uji Normalitas"), verbatimTextOutput("normalityDepTest"), uiOutput("normalityDepInterpretation")))
                                   )
                          )
                        ),
                        
                        # Tab 2: Eksplorasi Independen + Dependen
                        conditionalPanel(
                          condition = "input.exploration_tab == 'independent_dependent'",
                          tags$div(class = "tab-content-exploration",
                                   conditionalPanel(
                                     condition = "output.hasDependentVar",
                                     tags$div(class = "controls-panel",
                                              tags$h4("Analisis Hubungan Independen dengan Dependen", style="text-align:center; color: #2c3e50;"),
                                              fluidRow(
                                                column(12, style="text-align:center;",
                                                       actionButton("btnScatterDep", "Scatterplot", class="btn-analyze"),
                                                       # actionButton("btnBoxplotDep", "Boxplot per Kategori Target", class="btn-analyze"),
                                                       actionButton("btnCorrelationDep", "Korelasi dengan Target", class="btn-analyze"),
                                                       actionButton("btnAnovaDep", "ANOVA/Uji T", class="btn-analyze")
                                                )
                                              )
                                     ),
                                     
                                     # Results for Independent + Dependent
                                     tags$div(class = "viz-content",
                                              conditionalPanel(condition = "output.showScatterDep", tags$div(class = "analysis-result", tags$h3("Scatterplot vs Target"), plotlyOutput("scatterDepPlot", height = "400px"))),
                                              conditionalPanel(condition = "output.showBoxplotDep", tags$div(class = "analysis-result", tags$h3("Boxplot per Kategori Target"), plotlyOutput("boxplotDepPlot", height = "400px"))),
                                              conditionalPanel(condition = "output.showCorrelationDep", tags$div(class = "analysis-result", tags$h3("Korelasi dengan Target"), DT::dataTableOutput("correlationDepTable"))),
                                              conditionalPanel(condition = "output.showAnovaDep", tags$div(class = "analysis-result", tags$h3("ANOVA/Uji T"), verbatimTextOutput("anovaDepTest"), uiOutput("anovaDepInterpretation")))
                                     )
                                   ),
                                   conditionalPanel(
                                     condition = "!output.hasDependentVar",
                                     tags$div(class = "notice", tags$p("Silakan pilih variabel dependen terlebih dahulu untuk analisis ini."))
                                   )
                          )
                        ),
                        
                        # Tab 3: Eksplorasi Independen + Kategorik
                        conditionalPanel(
                          condition = "input.exploration_tab == 'independent_categorical'",
                          tags$div(class = "tab-content-exploration",
                                   conditionalPanel(
                                     condition = "output.hasCategoricalVar",
                                     tags$div(class = "controls-panel",
                                              tags$h4("Analisis Independen dengan Variabel Kategorik", style="text-align:center; color: #2c3e50;"),
                                              fluidRow(
                                                column(12, style="text-align:center;",
                                                       actionButton("btnBoxplotCat", "Boxplot per Kategori", class="btn-analyze"),
                                                       actionButton("btnViolinCat", "Violin Plot", class="btn-analyze"),
                                                       actionButton("btnAnovaCat", "ANOVA/Kruskal-Wallis", class="btn-analyze"),
                                                       actionButton("btnSummaryCat", "Statistik per Kategori", class="btn-analyze")
                                                )
                                              )
                                     ),
                                     
                                     # Results for Independent + Categorical
                                     tags$div(class = "viz-content",
                                              conditionalPanel(condition = "output.showBoxplotCat", tags$div(class = "analysis-result", tags$h3("Boxplot per Kategori"), plotlyOutput("boxplotCatPlot", height = "400px"))),
                                              conditionalPanel(condition = "output.showViolinCat", tags$div(class = "analysis-result", tags$h3("Violin Plot per Kategori"), plotlyOutput("violinCatPlot", height = "400px"))),
                                              conditionalPanel(condition = "output.showAnovaCat", tags$div(class = "analysis-result", tags$h3("ANOVA/Kruskal-Wallis Test"), verbatimTextOutput("anovaCatTest"), uiOutput("anovaCatInterpretation"))),
                                              conditionalPanel(condition = "output.showSummaryCat", tags$div(class = "analysis-result", tags$h3("Statistik Ringkasan per Kategori"), DT::dataTableOutput("summaryCatTable")))
                                     )
                                   ),
                                   conditionalPanel(
                                     condition = "!output.hasCategoricalVar",
                                     tags$div(class = "notice", tags$p("Silakan pilih variabel kategorik terlebih dahulu untuk analisis ini."))
                                   )
                          )
                        )
               )
             )
           ),
           
           # --- Halaman Clustering ---
           conditionalPanel(
             condition = "input.current_page == 'clustering'",
             tags$div(class = "page-header",
                      tags$h1("Clustering Daerah", class = "page-title"),
                      tags$p("Analisis Clustering K-Means untuk Pengelompokan Daerah Berdasarkan Data Padi", class = "page-subtitle")
             ),
             
             # Upload Data and Variable Selection Section
             tags$div(class = "controls-panel",
                      tags$h4("Upload Data dan Pilih Variabel untuk Clustering", style="text-align:center; color: #2c3e50;"),
                      fluidRow(
                        column(4,
                               fileInput("clusteringDataFile", "Unggah File Data Clustering (CSV):", 
                                         accept = ".csv", buttonLabel = "Pilih File", placeholder = "Tidak ada file dipilih"),
                               actionButton("loadSampleClusteringData", "Gunakan Data Contoh", class="btn-analyze", style="width: 100%; margin-top: 10px;")
                        ),
                        column(4,
                               conditionalPanel(
                                 condition = "output.clusteringDataLoaded",
                                 tags$h5("Pilih Variabel untuk Clustering:", style="color: #2c3e50; margin-bottom: 15px;"),
                                 uiOutput("clusteringVariablesUI")
                               )
                        ),
                        column(4,
                               conditionalPanel(
                                 condition = "output.clusteringDataLoaded",
                                 tags$h5("Pilih Jumlah Cluster:", style="color: #2c3e50; margin-bottom: 15px;"),
                                 radioButtons("clusterMethod", "Metode Penentuan K:",
                                              choices = list("Otomatis (Elbow Method)" = "auto",
                                                             "Manual" = "manual"),
                                              selected = "auto"),
                                 conditionalPanel(
                                   condition = "input.clusterMethod == 'manual'",
                                   numericInput("manualK", "Jumlah Cluster (K):", 
                                                value = 3, min = 2, max = 10, step = 1)
                                 ),
                                 actionButton("runClustering", "Jalankan Clustering", class="btn-analyze", 
                                              style="width: 100%; margin-top: 15px; background-color: #27ae60;")
                               )
                        )
                      )
             ),
             
             # Clustering Results Tabs
             conditionalPanel(
               condition = "output.clusteringComplete",
               tags$div(class = "visualization-container",
                        tags$div(class = "viz-tabs",
                                 tags$div("Elbow Method", class = "viz-tab active", id = "elbow-tab", onclick = "Shiny.setInputValue('clustering_tab', 'elbow')"),
                                 tags$div("Scatter Plot", class = "viz-tab", id = "scatter-cluster-tab", onclick = "Shiny.setInputValue('clustering_tab', 'scatter')"),
                                 tags$div("Boxplot per Cluster", class = "viz-tab", id = "boxplot-cluster-tab", onclick = "Shiny.setInputValue('clustering_tab', 'boxplot')"),
                                 tags$div("Peta Clustering", class = "viz-tab", id = "map-cluster-tab", onclick = "Shiny.setInputValue('clustering_tab', 'map')")
                        ),
                        
                        tags$div(class = "viz-content",
                                 # Tab 1: Elbow Method
                                 conditionalPanel(
                                   condition = "input.clustering_tab == 'elbow' || input.clustering_tab == undefined || input.clustering_tab == null",
                                   tags$div(class = "analysis-result",
                                            tags$h3("Elbow Method untuk Menentukan Jumlah Cluster Optimal"),
                                            plotlyOutput("elbowPlot", height = "400px"),
                                            tags$div(class = "cluster-summary",
                                                     tags$h4("Ringkasan Clustering:"),
                                                     tags$p(textOutput("clusterSummaryText"))
                                            )
                                   )
                                 ),
                                 
                                 # Tab 2: Scatter Plot
                                 conditionalPanel(
                                   condition = "input.clustering_tab == 'scatter'",
                                   tags$div(class = "analysis-result",
                                            tags$h3("Scatter Plot Hasil Clustering (2 Variabel Teratas)"),
                                            plotlyOutput("scatterClusterPlot", height = "400px"),
                                            tags$div(class = "cluster-info",
                                                     tags$h4("Informasi Cluster:"),
                                                     DT::dataTableOutput("clusterCentroids")
                                            )
                                   )
                                 ),
                                 
                                 # Tab 3: Boxplot per Cluster
                                 conditionalPanel(
                                   condition = "input.clustering_tab == 'boxplot'",
                                   tags$div(class = "analysis-result",
                                            tags$h3("Boxplot Variabel per Cluster"),
                                            plotlyOutput("boxplotClusterPlot", height = "500px")
                                   )
                                 ),
                                 
                                 # Tab 4: Map Clustering
                                 conditionalPanel(
                                   condition = "input.clustering_tab == 'map'",
                                   tags$div(class = "analysis-result",
                                            tags$h3("Peta Interaktif Hasil Clustering Daerah"),
                                            tags$div(class = "summary-stats",
                                                     tags$div(class = "stat-box", tags$div(textOutput("cluster_count"), class = "stat-value"), tags$div("Jumlah Cluster", class = "stat-label")),
                                                     tags$div(class = "stat-box", tags$div(textOutput("silhouette_score"), class = "stat-value"), tags$div("Silhouette Score", class = "stat-label")),
                                                     tags$div(class = "stat-box", tags$div(textOutput("largest_cluster"), class = "stat-value"), tags$div("Cluster Terbesar", class = "stat-label")),
                                                     tags$div(class = "stat-box", tags$div(textOutput("variables_used"), class = "stat-value"), tags$div("Variabel Digunakan", class = "stat-label"))
                                            ),
                                            leafletOutput("clusteringMap", height = "500px")
                                   )
                                 )
                        )
               )
             )
           ),
           
           # --- Halaman Forecasting ---
           conditionalPanel(
             condition = "input.current_page == 'sarimax'",
             tags$div(class = "page-header",
                      tags$h1("SARIMAX Forecasting", class = "page-title"),
                      tags$p("Model Peramalan Produksi Padi dengan Variabel Eksogen")
             ),
             tags$div(class = "content-container",
                      tags$div(class = "tab-navigation",
                               tags$button("Input Data", class = "tab-btn", id = "input-btn", onclick = "Shiny.setInputValue('sarimax_tab', 'input')"),
                               tags$button("Statistik Deskriptif", class = "tab-btn", id = "statistics-btn", onclick = "Shiny.setInputValue('sarimax_tab', 'statistics')"),
                               tags$button("Matriks Korelasi", class = "tab-btn", id = "correlation-btn", onclick = "Shiny.setInputValue('sarimax_tab', 'correlation')"),
                               tags$button("Pengecekan Asumsi", class = "tab-btn", id = "assumptions-btn", onclick = "Shiny.setInputValue('sarimax_tab', 'assumptions')"),
                               tags$button("Forecasting & Tren", class = "tab-btn", id = "forecasting-btn", onclick = "Shiny.setInputValue('sarimax_tab', 'forecasting')")
                      ),
                      tags$div(class = "tab-content",
                               conditionalPanel(
                                 condition = "input.sarimax_tab == 'input' || input.sarimax_tab == undefined || input.sarimax_tab == null",
                                 tags$div(class = "input-section",
                                          tags$h4("Format Data yang Diperlukan"),
                                          tags$p("Data harus dalam format CSV dengan kolom: No, Bulan, Tahun, Produksi Padi, Curah Hujan, Suhu, Kelembapan, Sinar Matahari"),
                                          tags$div(class = "file-upload-area", fileInput("dataFile", label = NULL, accept = c(".csv"), buttonLabel = "Pilih File CSV", placeholder = "Belum ada file dipilih")),
                                          actionButton("loadSampleData", "Gunakan Data Contoh", class = "btn-analyze", style = "margin-top: 15px;")
                                 ),
                                 tags$div(class = "data-preview", conditionalPanel(condition = "output.dataLoaded", tags$h4("Preview Data"), DT::dataTableOutput("dataPreview")))
                               ),
                               conditionalPanel(
                                 condition = "input.sarimax_tab == 'statistics'",
                                 conditionalPanel(
                                   condition = "output.dataLoaded",
                                   tags$h3("Statistik Deskriptif Data Time Series"),
                                   tags$div(class = "statistics-grid",
                                            tags$div(class = "stat-box", tags$div(textOutput("totalObservations"), class = "stat-value"), tags$div("Total Observasi", class = "stat-label")),
                                            tags$div(class = "stat-box", tags$div(textOutput("meanProduction"), class = "stat-value"), tags$div("Rata-rata Produksi (ton)", class = "stat-label")),
                                            tags$div(class = "stat-box", tags$div(textOutput("maxProduction"), class = "stat-value"), tags$div("Produksi Maksimum (ton)", class = "stat-label")),
                                            tags$div(class = "stat-box", tags$div(textOutput("minProduction"), class = "stat-value"), tags$div("Produksi Minimum (ton)", class = "stat-label"))
                                   ),
                                   plotlyOutput("timeSeriesPlot", height = "400px")
                                 ),
                                 conditionalPanel(
                                   condition = "!output.dataLoaded",
                                   tags$div(class = "notice",
                                            tags$p("Silakan muat data terlebih dahulu pada tab 'Input Data' untuk melihat matriks korelasi.")
                                   )
                                 )
                               ),
                               conditionalPanel(
                                 condition = "input.sarimax_tab == 'correlation'",
                                 conditionalPanel(
                                   condition = "output.dataLoaded",
                                   tags$h3("Matriks Korelasi Variabel Eksternal", style = "text-align: center; margin-bottom: 25px;"),
                                   tags$div(
                                     style = "display: flex; justify-content: center;",
                                     plotlyOutput("externalCorrelationPlot", height = "500px", width = "600px")
                                   ),
                                   tags$div(class = "correlation-analysis", uiOutput("correlationInterpretation"))
                                 ),
                                 conditionalPanel(
                                   condition = "!output.dataLoaded",
                                   tags$div(class = "notice",
                                            tags$p("Silakan muat data terlebih dahulu pada tab 'Input Data' untuk melihat matriks korelasi.")
                                   )
                                 )
                               ),
                               conditionalPanel(
                                 condition = "input.sarimax_tab == 'assumptions'",
                                 conditionalPanel(
                                   condition = "output.dataLoaded",
                                   tags$h3("Hasil Pengecekan Asumsi (Model Lengkap)"),
                                   tags$div(class = "test-results",
                                            tags$div(class = "test-result",
                                                     tags$h4("1. Uji Stasioneritas (Augmented Dickey-Fuller)"),
                                                     verbatimTextOutput("adfTest"),
                                                     uiOutput("adfInterpretation")
                                            ),
                                            tags$div(class = "test-result",
                                                     tags$h4("2. Uji Normalitas Residual (Shapiro-Wilk)"),
                                                     verbatimTextOutput("normalityTest"),
                                                     uiOutput("normalityInterpretation")
                                            ),
                                            tags$div(class = "test-result",
                                                     tags$h4("3. Uji Autokorelasi Residual (Ljung-Box)"),
                                                     verbatimTextOutput("autocorrelationTest"),
                                                     uiOutput("autocorrelationInterpretation")
                                            ),
                                            plotlyOutput("residualPlots", height = "400px")
                                   )
                                 ),
                                 conditionalPanel(
                                   condition = "!output.dataLoaded",
                                   tags$div(class = "notice",
                                            tags$p("Silakan muat data terlebih dahulu pada tab 'Input Data' untuk melihat hasil pengecekan asumsi.")
                                   )
                                 )
                               ),
                               conditionalPanel(
                                 condition = "input.sarimax_tab == 'forecasting'",
                                 conditionalPanel(
                                   condition = "output.dataLoaded",
                                   tags$h3("Peramalan Produksi Padi dengan SARIMAX"),
                                   fluidRow(
                                     column(6, 
                                            tags$div(class = "model-params", 
                                                     tags$h4("Parameter Model SARIMAX"), 
                                                     tags$h5("Non-Seasonal Parameters:"), 
                                                     fluidRow(
                                                       column(4, numericInput("p", "p (AR):", value = 1, min = 0, max = 5)), 
                                                       column(4, numericInput("d", "d (I):", value = 1, min = 0, max = 2)), 
                                                       column(4, numericInput("q", "q (MA):", value = 1, min = 0, max = 5))
                                                     ), 
                                                     tags$h5("Seasonal Parameters:"), 
                                                     fluidRow(
                                                       column(4, numericInput("P", "P (SAR):", value = 1, min = 0, max = 3)), 
                                                       column(4, numericInput("D", "D (SI):", value = 1, min = 0, max = 2)), 
                                                       column(4, numericInput("Q", "Q (SMA):", value = 1, min = 0, max = 3))
                                                     ), 
                                                     numericInput("s", "s (Seasonal period):", value = 12, min = 4, max = 24),
                                                     numericInput("forecastPeriods", "Periode Forecast (bulan):", value = 12, min = 1, max = 24)
                                            )
                                     ),
                                     column(6, 
                                            tags$div(class = "model-params", 
                                                     tags$h4("Jalankan Model SARIMAX"), 
                                                     actionButton("runForecast", "Jalankan Forecasting", class = "btn-analyze", style = "width: 100%; margin-top: 20px; font-size: 1.2rem; padding: 15px;")
                                            )
                                     )
                                   ),
                                   conditionalPanel(
                                     condition = "output.forecastComplete",
                                     tags$div(class = "forecast-summary", 
                                              tags$h3("Ringkasan Hasil Forecasting"), 
                                              tags$div(tags$strong("Model: "), textOutput("selectedModel", inline = TRUE)), 
                                              tags$div(tags$strong("Tren Produksi 1 Tahun Ke Depan: "), tags$span(textOutput("trendDirection", inline = TRUE), class = "trend-indicator")), 
                                              tags$p(textOutput("trendAnalysis"))
                                     ),
                                     plotlyOutput("forecastPlot", height = "500px"),
                                     tags$h4("Tabel Hasil Forecasting SARIMAX"), 
                                     DT::dataTableOutput("forecastTable"),
                                     tags$div(class = "model-params", 
                                              tags$h4("Summary Model"), 
                                              verbatimTextOutput("modelSummary")
                                     )
                                   )
                                 ),
                                 conditionalPanel(
                                   condition = "!output.dataLoaded",
                                   tags$div(class = "notice",
                                            tags$p("Silakan muat data terlebih dahulu pada tab 'Input Data' untuk menjalankan peramalan.")
                                   )
                                 )
                               )
                      )
             )
           )
  )
)

# ===================================================================
# SERVER LOGIC
# ===================================================================
server <- function(input, output, session) {
  
  # --- Inisialisasi & Pengaturan Awal ---
  jabar_geojson <- tryCatch({
    sf::st_read("www/jawa-barat.geojson", quiet = TRUE)
  }, error = function(e) {
    showNotification("Gagal memuat file 'jawa-barat.geojson'. Pastikan file ada di folder 'www'.", type = "error", duration = NULL)
    return(NULL)
  })
  
  sample_peta_data_processed <- sample_peta_data %>%
    mutate(Kabupaten_Join = toupper(Kabupaten))
  
  map_data <- reactiveVal(sample_peta_data_processed)
  uploaded_data_raw <- reactiveVal(NULL)
  
  # --- Observer untuk Navigasi & Tampilan ---
  observe({
    req(input$current_page)
    shinyjs::removeClass(selector = ".nav-link", class = "active")
    shinyjs::addClass(selector = paste0("#nav-", input$current_page), class = "active")
  })
  
  observe({
    req(input$viz_tab)
    shinyjs::removeClass(selector = ".viz-tab", class = "active")
    shinyjs::addClass(selector = paste0("#", input$viz_tab, "-tab"), class = "active")
  })
  
  observe({
    req(input$sarimax_tab)
    shinyjs::removeClass(selector = ".tab-btn", class = "active")
    shinyjs::addClass(selector = paste0("#", input$sarimax_tab, "-btn"), class = "active")
  })
  
  # Observer untuk clustering tabs
  observe({
    if(!is.null(input$clustering_tab)) {
      shinyjs::removeClass(selector = ".viz-tab", class = "active")
      shinyjs::addClass(selector = paste0("#", gsub("_", "-", input$clustering_tab), "-tab"), class = "active")
    }
  })
  
  # Observer untuk exploration tabs
  observe({
    if(!is.null(input$exploration_tab)) {
      shinyjs::removeClass(selector = ".exploration-tab", class = "active")
      shinyjs::addClass(selector = paste0("#", gsub("_", "-", input$exploration_tab), "-tab"), class = "active")
    }
  })
  
  ### PERUBAHAN: Logika Server untuk Grafik Baru di Halaman Beranda
  # Memuat dan memproses Data_Visualisasi.csv
  tryCatch({
    # Membaca data, memastikan nama kolom tidak diubah (check.names = FALSE)
    data_vis <- read.csv("www/Data_Visualisasi.csv", stringsAsFactors = FALSE, check.names = FALSE)
    
    # Kolom tahun yang akan diproses
    year_cols <- as.character(2018:2024)
    
    # Membersihkan dan mengubah kolom tahun menjadi numerik
    for (col in year_cols) {
      if(col %in% names(data_vis)) {
        # Menghilangkan spasi, mengganti koma dengan titik, dan konversi ke numerik
        data_vis[[col]] <- as.numeric(gsub(",", ".", gsub("\\s", "", data_vis[[col]])))
      }
    }
    
    # Menghitung rata-rata produksi untuk setiap baris (daerah)
    data_vis$Rata_Rata_Produksi <- rowMeans(data_vis[, year_cols], na.rm = TRUE)
    
    # Menyiapkan data final untuk plot
    avg_prod_data <- data_vis %>%
      select(Nama, Rata_Rata_Produksi) %>%
      filter(!is.na(Rata_Rata_Produksi) & Rata_Rata_Produksi > 0)
    
  }, error = function(e) {
    # Jika file tidak ditemukan atau error, buat dataframe kosong
    showNotification("Gagal memuat 'Data_Visualisasi.csv'. Grafik Beranda tidak akan ditampilkan.", type = "error")
    avg_prod_data <- data.frame(Nama=character(), Rata_Rata_Produksi=numeric())
  })
  
  # Render plot baru
  output$beranda_avg_prod_chart <- renderPlotly({
    if (nrow(avg_prod_data) == 0) {
      return(plot_ly() %>% layout(title = "Data untuk visualisasi tidak tersedia."))
    }
    
    plot_ly(avg_prod_data, 
            x = ~reorder(Nama, -Rata_Rata_Produksi), 
            y = ~Rata_Rata_Produksi, 
            type = 'bar',
            marker = list(color = '#27ae60', line = list(color = '#229954', width = 1.5))) %>%
      layout(title = list(text = "Rata-Rata Produksi Padi Tahunan (2018-2024)", y = 0.95, x = 0.5, xanchor = 'center', yanchor = 'top'),
             xaxis = list(title = "Kabupaten/Kota", tickangle = -45),
             yaxis = list(title = "Rata-Rata Produksi (Ton)"))
  })
  
  # --- PENAMBAHAN LOGIKA SERVER UNTUK DOWNLOAD TEMPLATE ---
  output$downloadVisualisasiTemplate <- downloadHandler(
    filename = function() {
      "Template_Data_Visualisasi.csv"
    },
    content = function(file) {
      file.copy("www/Template_Data_Visualisasi.csv", file)
    },
    contentType = "text/csv"
  )
  
  output$downloadEksplorasiTemplate <- downloadHandler(
    filename = function() {
      "Template_Data_Eksplorasi.csv"
    },
    content = function(file) {
      file.copy("www/Template_Data_Eksplorasi.csv", file)
    },
    contentType = "text/csv"
  )
  
  output$downloadClusteringTemplate <- downloadHandler(
    filename = function() {
      "Template_Data_Clustering.csv"
    },
    content = function(file) {
      file.copy("www/Template_Data_Clustering.csv", file)
    },
    contentType = "text/csv"
  )
  
  output$downloadForecastTemplate <- downloadHandler(
    filename = function() {
      "Template_Data_Forecast.csv"
    },
    content = function(file) {
      file.copy("www/Template_Data_Forecast.csv", file)
    },
    contentType = "text/csv"
  )
  # --- AKHIR PENAMBAHAN LOGIKA SERVER ---
  
  
  # --- Logika untuk Halaman Visualisasi Peta (dengan perbaikan grafik) ---
  observeEvent(input$mapDataFile, {
    req(input$mapDataFile)
    tryCatch({
      df <- read.csv(input$mapDataFile$datapath, stringsAsFactors = FALSE, check.names = FALSE)
      
      if (!("Nama" %in% names(df) || "Kabupaten" %in% names(df))) {
        stop("File CSV harus memiliki kolom 'Nama' atau 'Kabupaten'.")
      }
      if ("Nama" %in% names(df)) {
        names(df)[names(df) == "Nama"] <- "Kabupaten"
      }
      
      year_cols <- grep("^[0-9]{4}$", names(df), value = TRUE)
      if (length(year_cols) == 0) {
        stop("Tidak ditemukan kolom tahun (contoh: 2020, 2021) di file Anda.")
      }
      
      for (col in year_cols) {
        df[[col]] <- as.numeric(gsub(",", ".", gsub("\\s", "", df[[col]])))
      }
      
      df <- df %>% filter(Kabupaten != "")
      
      uploaded_data_raw(df)
      updateRadioButtons(session, "mapMode", selected = "absolute")
      showNotification("File berhasil diunggah. Silakan pilih tahun dan klik 'Terapkan'.", type = "message")
    }, error = function(e) {
      showNotification(paste("Gagal memproses file:", e$message), type = "error", duration = 8)
    })
  })
  
  output$yearSelectorUI <- renderUI({
    df <- uploaded_data_raw()
    if (is.null(df)) {
      shinyjs::disable("applyUploadedData")
      return(tags$div(style="color: grey; padding-top: 30px;", "Unggah file CSV terlebih dahulu"))
    }
    
    year_cols <- grep("^[0-9]{4}$", names(df), value = TRUE)
    label_text <- if (input$mapMode == "comparison") "Pilih Tahun (untuk dibandingkan dengan tahun sebelumnya):" else "Pilih Tahun:"
    
    shinyjs::enable("applyUploadedData")
    selectInput("selectedYear", label_text, choices = year_cols, selected = tail(year_cols, 1))
  })
  
  observeEvent(input$applyUploadedData, {
    req(uploaded_data_raw(), input$selectedYear)
    df_raw <- uploaded_data_raw()
    
    if (input$mapMode == "absolute") {
      new_data <- data.frame(
        Kabupaten = df_raw$Kabupaten,
        Produksi_Padi = df_raw[[input$selectedYear]],
        stringsAsFactors = FALSE
      ) %>%
        mutate(Kabupaten_Join = toupper(Kabupaten))
      
      map_data(new_data)
      showNotification(paste("Menampilkan data produksi padi untuk tahun", input$selectedYear), type = "message")
      
    } else {
      year_cols <- grep("^[0-9]{4}$", names(df_raw), value = TRUE)
      selected_year_str <- input$selectedYear
      selected_year_num <- as.numeric(selected_year_str)
      previous_year_str <- as.character(selected_year_num - 1)
      
      if (!previous_year_str %in% year_cols) {
        showNotification(paste("Data untuk tahun sebelumnya (", previous_year_str, ") tidak ditemukan. Tidak dapat melakukan perbandingan."), type = "error", duration = 8)
        return()
      }
      
      new_data <- df_raw %>%
        select(Kabupaten, all_of(c(selected_year_str, previous_year_str))) %>%
        mutate(
          Perubahan = .data[[selected_year_str]] - .data[[previous_year_str]],
          Status = ifelse(Perubahan > 0, "Meningkat", "Menurun"),
          Kabupaten_Join = toupper(Kabupaten)
        )
      
      map_data(new_data)
      showNotification(paste("Menampilkan perbandingan produksi antara tahun", selected_year_str, "dan", previous_year_str), type = "message")
    }
  })
  
  observeEvent(input$resetToSampleData, {
    map_data(sample_peta_data_processed)
    uploaded_data_raw(NULL) 
    updateRadioButtons(session, "mapMode", selected = "absolute")
    showNotification("Peta direset ke data contoh.", type = "message")
  })
  
  filtered_data <- reactive({
    req(jabar_geojson, map_data())
    current_map_data <- map_data()
    
    jabar_geojson_clean <- jabar_geojson %>%
      mutate(Kabupaten_Join = toupper(gsub("KABUPATEN |KOTA ", "", KABKOT)))
    
    if (!"Kabupaten_Join" %in% names(current_map_data)) {
      current_map_data <- current_map_data %>% mutate(Kabupaten_Join = toupper(Kabupaten))
    }
    
    jabar_geojson_clean %>% left_join(current_map_data, by = "Kabupaten_Join")
  })
  
  output$stat_max <- renderText({
    req(input$mapMode == "absolute")
    data <- filtered_data(); req("Produksi_Padi" %in% names(data))
    val <- max(data[["Produksi_Padi"]], na.rm = TRUE)
    if(is.infinite(val)) "N/A" else paste(format(round(val, 1), big.mark = ","), "ton")
  })
  output$stat_min <- renderText({
    req(input$mapMode == "absolute")
    data <- filtered_data(); req("Produksi_Padi" %in% names(data))
    val <- min(data[["Produksi_Padi"]], na.rm = TRUE)
    if(is.infinite(val)) "N/A" else paste(format(round(val, 1), big.mark = ","), "ton")
  })
  output$stat_mean <- renderText({
    req(input$mapMode == "absolute")
    data <- filtered_data(); req("Produksi_Padi" %in% names(data))
    val <- mean(data[["Produksi_Padi"]], na.rm = TRUE)
    if(is.na(val)) "N/A" else paste(format(round(val, 1), big.mark = ","), "ton")
  })
  output$stat_total <- renderText({
    req(input$mapMode == "absolute")
    data <- filtered_data(); req("Produksi_Padi" %in% names(data))
    val <- sum(data[["Produksi_Padi"]], na.rm = TRUE)
    paste(format(round(val), big.mark = ","), "ton")
  })
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles(options = providerTileOptions(minZoom = 7, maxZoom = 12)) %>%
      setView(lng = 107.6, lat = -6.9, zoom = 8)
  })
  
  observe({
    req(filtered_data())
    data <- filtered_data()
    proxy <- leafletProxy("map", data = data) %>% clearShapes() %>% clearControls()
    
    if (nrow(data) == 0) {
      proxy %>% addControl("Tidak ada data untuk ditampilkan.", position = "topright")
      return()
    }
    
    if ("Produksi_Padi" %in% names(data) && input$mapMode == 'absolute') {
      if (all(is.na(data$Produksi_Padi))) {
        proxy %>% addControl("Tidak ada data produksi untuk ditampilkan.", position = "topright")
        return()
      }
      pal <- colorNumeric(palette = "YlOrRd", domain = data$Produksi_Padi, na.color = "transparent")
      labels <- sprintf("<strong>%s</strong><br/>Produksi Padi: %s ton", data$Kabupaten,
                        format(data$Produksi_Padi, big.mark = ",", nsmall = 0, na.last = "N/A")) %>% lapply(htmltools::HTML)
      
      proxy %>%
        addPolygons(fillColor = ~pal(Produksi_Padi), weight = 1.5, opacity = 1, color = "white",
                    dashArray = "3", fillOpacity = 0.8,
                    highlightOptions = highlightOptions(weight = 1.5, color = "white", dashArray = "3", fillOpacity = 0.8, bringToFront = FALSE),
                    label = labels,
                    labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "13px", direction = "auto")) %>%
        addLegend(pal = pal, values = ~Produksi_Padi, opacity = 0.7, title = "Produksi Padi (ton)", position = "bottomright")
      
    } else if ("Perubahan" %in% names(data) && input$mapMode == 'comparison') {
      if (all(is.na(data$Perubahan))) {
        proxy %>% addControl("Tidak ada data perubahan untuk ditampilkan.", position = "topright")
        return()
      }
      pal <- colorFactor(palette = c("Meningkat" = "green", "Menurun" = "red"), domain = data$Status, na.color = "transparent")
      labels <- sprintf("<strong>%s</strong><br/>Status: %s<br/>Perubahan: %s ton", data$Kabupaten, data$Status,
                        format(data$Perubahan, big.mark = ",", nsmall = 2, scientific = FALSE)) %>% lapply(htmltools::HTML)
      
      proxy %>%
        addPolygons(fillColor = ~pal(Status), weight = 1.5, opacity = 1, color = "white",
                    dashArray = "3", fillOpacity = 0.8,
                    highlightOptions = highlightOptions(weight = 1.5, color = "white", dashArray = "3", fillOpacity = 0.8, bringToFront = FALSE),
                    label = labels,
                    labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "13px", direction = "auto")) %>%
        addLegend(pal = pal, values = ~Status, opacity = 0.7, title = "Status Produksi", position = "bottomright")
    }
  })
  
  # PERBAIKAN MASALAH 2: Grafik perbandingan dioptimasi dan diganti dengan ggplot + base R rendering
  output$comparison_chart_optimized <- renderPlot({
    req(filtered_data())
    data <- filtered_data()
    
    # Hide loading message
    shinyjs::hide("chart-loading")
    
    if (nrow(data) == 0) {
      plot(1, type = "n", main = "Tidak ada data untuk ditampilkan.", xlab = "", ylab = "", axes = FALSE)
      return()
    }
    
    if ("Produksi_Padi" %in% names(data) && input$mapMode == 'absolute') {
      if (all(is.na(data$Produksi_Padi))) {
        plot(1, type = "n", main = "Tidak ada data produksi untuk ditampilkan.", xlab = "", ylab = "", axes = FALSE)
        return()
      }
      
      data_to_plot <- data %>% 
        filter(!is.na(Produksi_Padi)) %>%
        arrange(desc(Produksi_Padi)) %>%
        slice_head(n = 15)  # Hanya ambil 15 teratas untuk performa
      
      # Menggunakan ggplot2 sederhana tanpa plotly untuk performa lebih baik
      p <- ggplot(data_to_plot, aes(x = reorder(Kabupaten, Produksi_Padi), y = Produksi_Padi)) +
        geom_col(fill = '#3498db', alpha = 0.8) +
        coord_flip() +
        theme_minimal() +
        theme(
          axis.text = element_text(size = 11),
          plot.title = element_text(size = 14, hjust = 0.5),
          axis.title = element_text(size = 12)
        ) +
        labs(
          title = "Perbandingan Produksi Padi per Daerah (Top 15)",
          x = "Kabupaten/Kota", 
          y = "Produksi Padi (ton)"
        ) +
        scale_y_continuous(labels = scales::comma_format())
      
      print(p)
      
    } else if ("Perubahan" %in% names(data) && input$mapMode == 'comparison') {
      if (all(is.na(data$Perubahan))) {
        plot(1, type = "n", main = "Tidak ada data perubahan untuk ditampilkan.", xlab = "", ylab = "", axes = FALSE)
        return()
      }
      
      data_to_plot <- data %>% 
        filter(!is.na(Perubahan)) %>%
        arrange(desc(abs(Perubahan))) %>%
        slice_head(n = 15)  # Hanya ambil 15 dengan perubahan terbesar
      
      # Menggunakan ggplot2 sederhana untuk performa lebih baik
      p <- ggplot(data_to_plot, aes(x = reorder(Kabupaten, Perubahan), y = Perubahan, fill = Status)) +
        geom_col(alpha = 0.8) +
        coord_flip() +
        scale_fill_manual(values = c("Meningkat" = "green", "Menurun" = "red")) +
        theme_minimal() +
        theme(
          axis.text = element_text(size = 11),
          plot.title = element_text(size = 14, hjust = 0.5),
          axis.title = element_text(size = 12),
          legend.position = "bottom"
        ) +
        labs(
          title = "Perubahan Produksi Padi Dibandingkan Tahun Sebelumnya (Top 15)",
          x = "Kabupaten/Kota", 
          y = "Perubahan Produksi (ton)",
          fill = "Status"
        ) +
        scale_y_continuous(labels = scales::comma_format())
      
      print(p)
    }
  })
  
  # Hide loading message when chart tab is selected
  observe({
    if(!is.null(input$viz_tab) && input$viz_tab == "chart") {
      shinyjs::hide("chart-loading")
    }
  })
  
  output$data_table <- DT::renderDataTable({
    req(filtered_data())
    data <- filtered_data() %>% as.data.frame()
    
    if ("Produksi_Padi" %in% names(data) && input$mapMode == 'absolute') {
      display_data <- data %>%
        mutate(`Produksi Padi (ton)` = ifelse(is.na(Produksi_Padi), "N/A", format(Produksi_Padi, big.mark = ","))) %>%
        select(Kabupaten, `Produksi Padi (ton)`)
      
    } else if ("Perubahan" %in% names(data) && input$mapMode == 'comparison') {
      display_data <- data %>%
        mutate(
          `Perubahan (ton)` = ifelse(is.na(Perubahan), "N/A", format(round(Perubahan, 2), big.mark = ",")),
          `Tahun Dipilih` = format(data[[input$selectedYear]], big.mark = ","),
          `Tahun Sebelumnya` = format(data[[as.character(as.numeric(input$selectedYear) - 1)]], big.mark = ",")
        ) %>%
        select(Kabupaten, `Tahun Dipilih`, `Tahun Sebelumnya`, `Perubahan (ton)`, Status)
    } else {
      display_data <- data.frame(Message = "Pilih mode dan terapkan data untuk melihat tabel.")
    }
    
    DT::datatable(display_data, options = list(pageLength = 15, scrollX = TRUE), rownames = FALSE)
  })
  
  #=============================================================================
  # --- Halaman Eksplorasi Data (DENGAN PERBAIKAN TAB PERTAMA) ---
  #=============================================================================
  
  exploration_values <- reactiveValues(
    data = NULL,
    dependent_var = "",
    independent_vars = c(),
    categorical_var = "",
    # Dependent Only (BARU - untuk tab pertama yang sudah diubah)
    show_histogram_dep = FALSE,
    show_boxplot_dep_single = FALSE,
    show_descriptive_dep = FALSE,
    show_normality_dep = FALSE,
    # Independent + Dependent
    show_scatter_dep = FALSE,
    show_boxplot_dep = FALSE,
    show_correlation_dep = FALSE,
    show_anova_dep = FALSE,
    # Independent + Categorical
    show_boxplot_cat = FALSE,
    show_violin_cat = FALSE,
    show_anova_cat = FALSE,
    show_summary_cat = FALSE
  )
  
  # Upload data untuk eksplorasi
  observeEvent(input$explorationDataFile, {
    req(input$explorationDataFile)
    tryCatch({
      data <- read.csv(input$explorationDataFile$datapath, stringsAsFactors = FALSE)
      exploration_values$data <- data
      
      # Reset variabel yang dipilih
      exploration_values$dependent_var <- ""
      exploration_values$independent_vars <- c()
      exploration_values$categorical_var <- ""
      
      # Reset tampilan analisis
      for(var in names(exploration_values)) {
        if(startsWith(var, "show_")) exploration_values[[var]] <- FALSE
      }
      
      showNotification("Data berhasil dimuat untuk eksplorasi!", type = "message")
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
    })
  })
  
  # Reset data
  observeEvent(input$resetExplorationData, {
    exploration_values$data <- NULL
    exploration_values$dependent_var <- ""
    exploration_values$independent_vars <- c()
    exploration_values$categorical_var <- ""
    
    # Reset semua tampilan
    for(var in names(exploration_values)) {
      if(startsWith(var, "show_")) exploration_values[[var]] <- FALSE
    }
  })
  
  # Handle dropped variables - modification to handle multiple independent variables
  observeEvent(input$dropped_dependent, {
    exploration_values$dependent_var <- input$dropped_dependent
  })
  
  observeEvent(input$dropped_independent, {
    if(input$dropped_independent != "") {
      if(!input$dropped_independent %in% exploration_values$independent_vars) {
        exploration_values$independent_vars <- c(exploration_values$independent_vars, input$dropped_independent)
      }
    }
    # Reset tampilan analisis ketika variabel independen berubah
    for(var in names(exploration_values)) {
      if(startsWith(var, "show_")) exploration_values[[var]] <- FALSE
    }
  })
  
  observeEvent(input$dropped_categorical, {
    exploration_values$categorical_var <- input$dropped_categorical
    # Reset tampilan analisis ketika variabel kategori berubah
    for(var in names(exploration_values)) {
      if(startsWith(var, "show_")) exploration_values[[var]] <- FALSE
    }
  })
  
  # Render variable bank
  output$variable_bank <- renderUI({
    req(exploration_values$data)
    data <- exploration_values$data
    var_names <- names(data)
    
    variable_items <- lapply(var_names, function(var_name) {
      var_type <- if(is.numeric(data[[var_name]])) "Numerik" else "Kategorikal"
      var_class <- if(is.numeric(data[[var_name]])) "variable-numeric" else "variable-categorical"
      
      tags$div(
        class = paste("variable-item", var_class),
        `data-variable` = var_name,
        tags$span(var_name, class = "variable-name"),
        tags$span(paste("(", var_type, ")"), class = "variable-type")
      )
    })
    
    tags$div(class = "variable-container", variable_items)
  })
  
  # Output reactive untuk kondisi tampilan
  output$explorationDataLoaded <- reactive({ !is.null(exploration_values$data) })
  outputOptions(output, "explorationDataLoaded", suspendWhenHidden = FALSE)
  
  output$hasIndependentVar <- reactive({ 
    length(exploration_values$independent_vars) > 0 && 
      !is.null(exploration_values$data) && 
      all(exploration_values$independent_vars %in% names(exploration_values$data))
  })
  outputOptions(output, "hasIndependentVar", suspendWhenHidden = FALSE)
  
  output$hasDependentVar <- reactive({ 
    exploration_values$dependent_var != "" && 
      !is.null(exploration_values$data) && 
      exploration_values$dependent_var %in% names(exploration_values$data) 
  })
  outputOptions(output, "hasDependentVar", suspendWhenHidden = FALSE)
  
  output$hasCategoricalVar <- reactive({ 
    exploration_values$categorical_var != "" && 
      !is.null(exploration_values$data) && 
      exploration_values$categorical_var %in% names(exploration_values$data) 
  })
  outputOptions(output, "hasCategoricalVar", suspendWhenHidden = FALSE)
  
  # ===========================================================================
  # TAB 1: EKSPLORASI DEPENDEN SAJA (BARU - MENGGANTI INDEPENDEN SAJA)
  # ===========================================================================
  
  # Button observers untuk TAB 1 (DEPENDEN SAJA)
  observeEvent(input$btnHistogramDep, {
    exploration_values$show_histogram_dep <- TRUE
    for(var in names(exploration_values)) {
      if(startsWith(var, "show_") && var != "show_histogram_dep") exploration_values[[var]] <- FALSE
    }
  })
  
  observeEvent(input$btnBoxplotDepSingle, {
    exploration_values$show_boxplot_dep_single <- TRUE
    for(var in names(exploration_values)) {
      if(startsWith(var, "show_") && var != "show_boxplot_dep_single") exploration_values[[var]] <- FALSE
    }
  })
  
  observeEvent(input$btnDescriptiveDep, {
    exploration_values$show_descriptive_dep <- TRUE
    for(var in names(exploration_values)) {
      if(startsWith(var, "show_") && var != "show_descriptive_dep") exploration_values[[var]] <- FALSE
    }
  })
  
  observeEvent(input$btnNormalityDep, {
    exploration_values$show_normality_dep <- TRUE
    for(var in names(exploration_values)) {
      if(startsWith(var, "show_") && var != "show_normality_dep") exploration_values[[var]] <- FALSE
    }
  })
  
  # Output reactive untuk tab 1 (DEPENDEN SAJA)
  output$showHistogramDep <- reactive({ exploration_values$show_histogram_dep })
  outputOptions(output, "showHistogramDep", suspendWhenHidden = FALSE)
  
  output$showBoxplotDepSingle <- reactive({ exploration_values$show_boxplot_dep_single })
  outputOptions(output, "showBoxplotDepSingle", suspendWhenHidden = FALSE)
  
  output$showDescriptiveDep <- reactive({ exploration_values$show_descriptive_dep })
  outputOptions(output, "showDescriptiveDep", suspendWhenHidden = FALSE)
  
  output$showNormalityDep <- reactive({ exploration_values$show_normality_dep })
  outputOptions(output, "showNormalityDep", suspendWhenHidden = FALSE)
  
  # Analysis outputs untuk tab 1 (DEPENDEN SAJA)
  output$histogramDepPlot <- renderPlotly({
    req(exploration_values$data, exploration_values$dependent_var != "")
    
    data <- exploration_values$data
    dep_var <- exploration_values$dependent_var
    
    if(!is.numeric(data[[dep_var]])) return(plot_ly() %>% layout(title = "Variabel dependen harus numerik untuk histogram"))
    
    p <- ggplot(data, aes_string(x = dep_var)) +
      geom_histogram(bins = 30, fill = "#e74c3c", alpha = 0.7, color = "white") +
      theme_minimal() +
      labs(title = paste("Histogram", dep_var), x = dep_var, y = "Frekuensi")
    ggplotly(p)
  })
  
  output$boxplotDepSinglePlot <- renderPlotly({
    req(exploration_values$data, exploration_values$dependent_var != "")
    
    data <- exploration_values$data
    dep_var <- exploration_values$dependent_var
    
    if(!is.numeric(data[[dep_var]])) return(plot_ly() %>% layout(title = "Variabel dependen harus numerik untuk boxplot"))
    
    p <- ggplot(data, aes_string(y = dep_var)) +
      geom_boxplot(fill = "#e74c3c", alpha = 0.7) +
      theme_minimal() +
      labs(title = paste("Boxplot", dep_var), y = dep_var)
    ggplotly(p)
  })
  
  output$descriptiveDepStats <- DT::renderDataTable({
    req(exploration_values$data, exploration_values$dependent_var != "")
    
    data <- exploration_values$data
    dep_var <- exploration_values$dependent_var
    
    if(!is.numeric(data[[dep_var]])) return(NULL)
    
    result <- data %>%
      summarise(
        Variabel = dep_var,
        Count = n(),
        Mean = mean(.data[[dep_var]], na.rm = TRUE),
        Median = median(.data[[dep_var]], na.rm = TRUE),
        SD = sd(.data[[dep_var]], na.rm = TRUE),
        Min = min(.data[[dep_var]], na.rm = TRUE),
        Max = max(.data[[dep_var]], na.rm = TRUE)
      )
    
    DT::datatable(result, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  output$normalityDepTest <- renderPrint({
    req(exploration_values$data, exploration_values$dependent_var != "")
    
    data <- exploration_values$data
    dep_var <- exploration_values$dependent_var
    
    if(!is.numeric(data[[dep_var]])) return("Variabel dependen harus numerik untuk uji normalitas")
    
    var_data <- data[[dep_var]][!is.na(data[[dep_var]])]
    if(length(var_data) >= 3) {
      if(length(var_data) <= 5000) {
        shapiro.test(var_data)
      } else {
        lillie.test(var_data)
      }
    } else {
      "Data tidak cukup untuk uji normalitas"
    }
  })
  
  output$normalityDepInterpretation <- renderUI({
    req(exploration_values$data, exploration_values$dependent_var != "")
    
    data <- exploration_values$data
    dep_var <- exploration_values$dependent_var
    
    if(!is.numeric(data[[dep_var]])) return(NULL)
    
    var_data <- data[[dep_var]][!is.na(data[[dep_var]])]
    if(length(var_data) >= 3) {
      if(length(var_data) <= 5000) {
        test_result <- shapiro.test(var_data)
      } else {
        test_result <- lillie.test(var_data)
      }
      
      alpha <- 0.05
      if(test_result$p.value < alpha) {
        tags$p(HTML(sprintf("<strong>Kesimpulan:</strong> P-value (%.4f) < alpha (%.2f), data <strong>tidak berdistribusi normal</strong>.", 
                            test_result$p.value, alpha)))
      } else {
        tags$p(HTML(sprintf("<strong>Kesimpulan:</strong> P-value (%.4f) >= alpha (%.2f), data <strong>berdistribusi normal</strong>.", 
                            test_result$p.value, alpha)))
      }
    }
  })
  
  # ===========================================================================
  # TAB 2: EKSPLORASI INDEPENDEN + DEPENDEN (tidak berubah)
  # ===========================================================================
  
  # Button observers for tab 2
  observeEvent(input$btnScatterDep, {
    exploration_values$show_scatter_dep <- TRUE
    for(var in names(exploration_values)) {
      if(startsWith(var, "show_") && var != "show_scatter_dep") exploration_values[[var]] <- FALSE
    }
  })
  
  #observeEvent(input$btnBoxplotDep, {
  #  exploration_values$show_boxplot_dep <- TRUE
  #  for(var in names(exploration_values)) {
  #    if(startsWith(var, "show_") && var != "show_boxplot_dep") exploration_values[[var]] <- FALSE
  #  }
  #})
  
  observeEvent(input$btnCorrelationDep, {
    exploration_values$show_correlation_dep <- TRUE
    for(var in names(exploration_values)) {
      if(startsWith(var, "show_") && var != "show_correlation_dep") exploration_values[[var]] <- FALSE
    }
  })
  
  observeEvent(input$btnAnovaDep, {
    exploration_values$show_anova_dep <- TRUE
    for(var in names(exploration_values)) {
      if(startsWith(var, "show_") && var != "show_anova_dep") exploration_values[[var]] <- FALSE
    }
  })
  
  # Output reactive untuk tab 2
  output$showScatterDep <- reactive({ exploration_values$show_scatter_dep })
  outputOptions(output, "showScatterDep", suspendWhenHidden = FALSE)
  
  output$showBoxplotDep <- reactive({ exploration_values$show_boxplot_dep })
  outputOptions(output, "showBoxplotDep", suspendWhenHidden = FALSE)
  
  output$showCorrelationDep <- reactive({ exploration_values$show_correlation_dep })
  outputOptions(output, "showCorrelationDep", suspendWhenHidden = FALSE)
  
  output$showAnovaDep <- reactive({ exploration_values$show_anova_dep })
  outputOptions(output, "showAnovaDep", suspendWhenHidden = FALSE)
  
  # Analysis outputs for tab 2
  output$scatterDepPlot <- renderPlotly({
    req(exploration_values$data, exploration_values$dependent_var != "", length(exploration_values$independent_vars) > 0)
    
    data <- exploration_values$data
    dep_var <- exploration_values$dependent_var
    numeric_vars <- exploration_values$independent_vars[sapply(exploration_values$independent_vars, function(x) is.numeric(data[[x]]))]
    
    if(length(numeric_vars) == 0 || !is.numeric(data[[dep_var]])) {
      return(plot_ly() %>% layout(title = "Variabel dependen dan minimal 1 independen harus numerik"))
    }
    
    if(length(numeric_vars) == 1) {
      p <- ggplot(data, aes_string(x = numeric_vars[1], y = dep_var)) +
        geom_point(alpha = 0.6, color = "#3498db") +
        geom_smooth(method = "lm", se = TRUE, color = "#e74c3c") +
        theme_minimal() +
        labs(title = paste("Scatterplot:", numeric_vars[1], "vs", dep_var),
             x = numeric_vars[1], y = dep_var)
      ggplotly(p)
    } else {
      # Multiple scatterplots
      plot_list <- list()
      for(i in 1:min(4, length(numeric_vars))) {
        var_name <- numeric_vars[i]
        p <- ggplot(data, aes_string(x = var_name, y = dep_var)) +
          geom_point(alpha = 0.6, color = "#3498db") +
          geom_smooth(method = "lm", se = TRUE, color = "#e74c3c") +
          theme_minimal() +
          labs(title = paste(var_name, "vs", dep_var), x = var_name, y = dep_var)
        plot_list[[i]] <- ggplotly(p)
      }
      subplot(plot_list, nrows = 2, shareY = TRUE, titleX = TRUE)
    }
  })
  
  output$boxplotDepPlot <- renderPlotly({
    req(exploration_values$data, exploration_values$dependent_var != "", length(exploration_values$independent_vars) > 0)
    
    data <- exploration_values$data
    dep_var <- exploration_values$dependent_var
    
    # Create categories from dependent variable if it's numeric
    if(is.numeric(data[[dep_var]])) {
      # Create quartile-based categories
      quartiles <- quantile(data[[dep_var]], na.rm = TRUE)
      data$dep_category <- cut(data[[dep_var]], 
                               breaks = quartiles, 
                               labels = c("Q1", "Q2", "Q3", "Q4"),
                               include.lowest = TRUE)
    } else {
      data$dep_category <- data[[dep_var]]
    }
    
    numeric_vars <- exploration_values$independent_vars[sapply(exploration_values$independent_vars, function(x) is.numeric(data[[x]]))]
    
    if(length(numeric_vars) == 0) {
      return(plot_ly() %>% layout(title = "Minimal 1 variabel independen numerik diperlukan"))
    }
    
    if(length(numeric_vars) == 1) {
      p <- ggplot(data, aes_string(x = "dep_category", y = numeric_vars[1], fill = "dep_category")) +
        geom_boxplot(alpha = 0.7) +
        theme_minimal() +
        labs(title = paste("Boxplot", numeric_vars[1], "per Kategori", dep_var),
             x = paste("Kategori", dep_var), y = numeric_vars[1])
      ggplotly(p)
    } else {
      # First variable only for simplicity
      p <- ggplot(data, aes_string(x = "dep_category", y = numeric_vars[1], fill = "dep_category")) +
        geom_boxplot(alpha = 0.7) +
        theme_minimal() +
        labs(title = paste("Boxplot", numeric_vars[1], "per Kategori", dep_var),
             x = paste("Kategori", dep_var), y = numeric_vars[1])
      ggplotly(p)
    }
  })
  
  output$correlationDepTable <- DT::renderDataTable({
    req(exploration_values$data, exploration_values$dependent_var != "", length(exploration_values$independent_vars) > 0)
    
    data <- exploration_values$data
    dep_var <- exploration_values$dependent_var
    numeric_vars <- exploration_values$independent_vars[sapply(exploration_values$independent_vars, function(x) is.numeric(data[[x]]))]
    
    if(length(numeric_vars) == 0 || !is.numeric(data[[dep_var]])) {
      return(NULL)
    }
    
    correlations <- sapply(numeric_vars, function(var) {
      cor(data[[var]], data[[dep_var]], use = "complete.obs")
    })
    
    result <- data.frame(
      Variabel = numeric_vars,
      Korelasi_dengan_Target = round(correlations, 4),
      Korelasi_Absolut = round(abs(correlations), 4)
    ) %>%
      arrange(desc(Korelasi_Absolut))
    
    DT::datatable(result, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  output$anovaDepTest <- renderPrint({
    req(exploration_values$data, exploration_values$dependent_var != "", length(exploration_values$independent_vars) > 0)
    
    data <- exploration_values$data
    dep_var <- exploration_values$dependent_var
    numeric_vars <- exploration_values$independent_vars[sapply(exploration_values$independent_vars, function(x) is.numeric(data[[x]]))]
    
    if(length(numeric_vars) == 0 || !is.numeric(data[[dep_var]])) {
      return("Variabel dependen dan independen harus numerik untuk uji ini")
    }
    
    results <- list()
    for(var in numeric_vars) {
      # T-test for correlation significance
      test_result <- cor.test(data[[var]], data[[dep_var]])
      results[[var]] <- test_result
    }
    results
  })
  
  output$anovaDepInterpretation <- renderUI({
    req(exploration_values$data, exploration_values$dependent_var != "", length(exploration_values$independent_vars) > 0)
    
    data <- exploration_values$data
    dep_var <- exploration_values$dependent_var
    numeric_vars <- exploration_values$independent_vars[sapply(exploration_values$independent_vars, function(x) is.numeric(data[[x]]))]
    
    if(length(numeric_vars) == 0 || !is.numeric(data[[dep_var]])) return(NULL)
    
    interpretation_list <- list()
    alpha <- 0.05
    
    for(var in numeric_vars) {
      test_result <- cor.test(data[[var]], data[[dep_var]])
      
      if(test_result$p.value < alpha) {
        interpretation_list <- append(interpretation_list, list(
          tags$li(HTML(sprintf("<strong>%s:</strong> P-value (%.4f) < alpha (%.2f), terdapat <strong>korelasi signifikan</strong> dengan target.", 
                               var, test_result$p.value, alpha)))
        ))
      } else {
        interpretation_list <- append(interpretation_list, list(
          tags$li(HTML(sprintf("<strong>%s:</strong> P-value (%.4f) >= alpha (%.2f), <strong>tidak ada korelasi signifikan</strong> dengan target.", 
                               var, test_result$p.value, alpha)))
        ))
      }
    }
    
    if(length(interpretation_list) > 0) {
      tags$ul(class = "custom-list", interpretation_list)
    }
  })
  
  # ===========================================================================
  # TAB 3: EKSPLORASI INDEPENDEN + KATEGORIK (tidak berubah)
  # ===========================================================================
  
  # Button observers for tab 3
  observeEvent(input$btnBoxplotCat, {
    exploration_values$show_boxplot_cat <- TRUE
    for(var in names(exploration_values)) {
      if(startsWith(var, "show_") && var != "show_boxplot_cat") exploration_values[[var]] <- FALSE
    }
  })
  
  observeEvent(input$btnViolinCat, {
    exploration_values$show_violin_cat <- TRUE
    for(var in names(exploration_values)) {
      if(startsWith(var, "show_") && var != "show_violin_cat") exploration_values[[var]] <- FALSE
    }
  })
  
  observeEvent(input$btnAnovaCat, {
    exploration_values$show_anova_cat <- TRUE
    for(var in names(exploration_values)) {
      if(startsWith(var, "show_") && var != "show_anova_cat") exploration_values[[var]] <- FALSE
    }
  })
  
  observeEvent(input$btnSummaryCat, {
    exploration_values$show_summary_cat <- TRUE
    for(var in names(exploration_values)) {
      if(startsWith(var, "show_") && var != "show_summary_cat") exploration_values[[var]] <- FALSE
    }
  })
  
  # Output reactive untuk tab 3
  output$showBoxplotCat <- reactive({ exploration_values$show_boxplot_cat })
  outputOptions(output, "showBoxplotCat", suspendWhenHidden = FALSE)
  
  output$showViolinCat <- reactive({ exploration_values$show_violin_cat })
  outputOptions(output, "showViolinCat", suspendWhenHidden = FALSE)
  
  output$showAnovaCat <- reactive({ exploration_values$show_anova_cat })
  outputOptions(output, "showAnovaCat", suspendWhenHidden = FALSE)
  
  output$showSummaryCat <- reactive({ exploration_values$show_summary_cat })
  outputOptions(output, "showSummaryCat", suspendWhenHidden = FALSE)
  
  # Analysis outputs for tab 3
  output$boxplotCatPlot <- renderPlotly({
    req(exploration_values$data, exploration_values$categorical_var != "", length(exploration_values$independent_vars) > 0)
    
    data <- exploration_values$data
    cat_var <- exploration_values$categorical_var
    numeric_vars <- exploration_values$independent_vars[sapply(exploration_values$independent_vars, function(x) is.numeric(data[[x]]))]
    
    if(length(numeric_vars) == 0) {
      return(plot_ly() %>% layout(title = "Minimal 1 variabel independen numerik diperlukan"))
    }
    
    if(length(numeric_vars) == 1) {
      p <- ggplot(data, aes_string(x = cat_var, y = numeric_vars[1], fill = cat_var)) +
        geom_boxplot(alpha = 0.7) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        labs(title = paste("Boxplot", numeric_vars[1], "per", cat_var),
             x = cat_var, y = numeric_vars[1])
      ggplotly(p)
    } else {
      # First variable only
      p <- ggplot(data, aes_string(x = cat_var, y = numeric_vars[1], fill = cat_var)) +
        geom_boxplot(alpha = 0.7) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        labs(title = paste("Boxplot", numeric_vars[1], "per", cat_var),
             x = cat_var, y = numeric_vars[1])
      ggplotly(p)
    }
  })
  
  output$violinCatPlot <- renderPlotly({
    req(exploration_values$data, exploration_values$categorical_var != "", length(exploration_values$independent_vars) > 0)
    
    data <- exploration_values$data
    cat_var <- exploration_values$categorical_var
    numeric_vars <- exploration_values$independent_vars[sapply(exploration_values$independent_vars, function(x) is.numeric(data[[x]]))]
    
    if(length(numeric_vars) == 0) {
      return(plot_ly() %>% layout(title = "Minimal 1 variabel independen numerik diperlukan"))
    }
    
    p <- ggplot(data, aes_string(x = cat_var, y = numeric_vars[1], fill = cat_var)) +
      geom_violin(alpha = 0.7) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(title = paste("Violin Plot", numeric_vars[1], "per", cat_var),
           x = cat_var, y = numeric_vars[1])
    
    ggplotly(p)
  })
  
  output$anovaCatTest <- renderPrint({
    req(exploration_values$data, exploration_values$categorical_var != "", length(exploration_values$independent_vars) > 0)
    
    data <- exploration_values$data
    cat_var <- exploration_values$categorical_var
    numeric_vars <- exploration_values$independent_vars[sapply(exploration_values$independent_vars, function(x) is.numeric(data[[x]]))]
    
    if(length(numeric_vars) == 0) {
      return("Minimal 1 variabel independen numerik diperlukan")
    }
    
    results <- list()
    for(var in numeric_vars) {
      formula_str <- paste(var, "~", cat_var)
      tryCatch({
        aov_result <- aov(as.formula(formula_str), data = data)
        results[[var]] <- summary(aov_result)
      }, error = function(e) {
        results[[var]] <- paste("Error:", e$message)
      })
    }
    results
  })
  
  output$anovaCatInterpretation <- renderUI({
    req(exploration_values$data, exploration_values$categorical_var != "", length(exploration_values$independent_vars) > 0)
    
    data <- exploration_values$data
    cat_var <- exploration_values$categorical_var
    numeric_vars <- exploration_values$independent_vars[sapply(exploration_values$independent_vars, function(x) is.numeric(data[[x]]))]
    
    if(length(numeric_vars) == 0) return(NULL)
    
    interpretation_list <- list()
    alpha <- 0.05
    
    for(var in numeric_vars) {
      formula_str <- paste(var, "~", cat_var)
      tryCatch({
        aov_result <- aov(as.formula(formula_str), data = data)
        p_value <- summary(aov_result)[[1]][["Pr(>F)"]][1]
        
        if(p_value < alpha) {
          interpretation_list <- append(interpretation_list, list(
            tags$li(HTML(sprintf("<strong>%s:</strong> P-value (%.4f) < alpha (%.2f), terdapat <strong>perbedaan signifikan</strong> antar kategori.", 
                                 var, p_value, alpha)))
          ))
        } else {
          interpretation_list <- append(interpretation_list, list(
            tags$li(HTML(sprintf("<strong>%s:</strong> P-value (%.4f) >= alpha (%.2f), <strong>tidak ada perbedaan signifikan</strong> antar kategori.", 
                                 var, p_value, alpha)))
          ))
        }
      }, error = function(e) {
        interpretation_list <- append(interpretation_list, list(
          tags$li(HTML(sprintf("<strong>%s:</strong> Error dalam analisis", var)))
        ))
      })
    }
    
    if(length(interpretation_list) > 0) {
      tags$ul(class = "custom-list", interpretation_list)
    }
  })
  
  output$summaryCatTable <- DT::renderDataTable({
    req(exploration_values$data, exploration_values$categorical_var != "", length(exploration_values$independent_vars) > 0)
    
    data <- exploration_values$data
    cat_var <- exploration_values$categorical_var
    numeric_vars <- exploration_values$independent_vars[sapply(exploration_values$independent_vars, function(x) is.numeric(data[[x]]))]
    
    if(length(numeric_vars) == 0) return(NULL)
    
    # Summary for first numeric variable
    result <- data %>%
      group_by(!!sym(cat_var)) %>%
      summarise(
        Count = n(),
        Mean = mean(!!sym(numeric_vars[1]), na.rm = TRUE),
        Median = median(!!sym(numeric_vars[1]), na.rm = TRUE),
        SD = sd(!!sym(numeric_vars[1]), na.rm = TRUE),
        Min = min(!!sym(numeric_vars[1]), na.rm = TRUE),
        Max = max(!!sym(numeric_vars[1]), na.rm = TRUE),
        .groups = 'drop'
      )
    
    DT::datatable(result, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  #=============================================================================
  # --- Halaman Clustering (tidak berubah) ---
  #=============================================================================
  
  clustering_values <- reactiveValues(
    data = NULL,
    selected_vars = c(),
    clustering_result = NULL,
    optimal_k = NULL,
    selected_k = NULL,
    elbow_data = NULL,
    standardized_data = NULL,
    silhouette_score = NULL
  )
  
  # Load sample clustering data
  observeEvent(input$loadSampleClusteringData, {
    clustering_values$data <- sample_clustering_data
    showNotification("Data contoh clustering berhasil dimuat!", type = "message")
  })
  
  # Upload clustering data
  observeEvent(input$clusteringDataFile, {
    req(input$clusteringDataFile)
    tryCatch({
      data <- read.csv(input$clusteringDataFile$datapath, stringsAsFactors = FALSE)
      clustering_values$data <- data
      showNotification("Data clustering berhasil dimuat!", type = "message")
    }, error = function(e) {
      showNotification(paste("Error loading clustering data:", e$message), type = "error")
    })
  })
  
  # Output reactive untuk kondisi clustering data loaded
  output$clusteringDataLoaded <- reactive({ !is.null(clustering_values$data) })
  outputOptions(output, "clusteringDataLoaded", suspendWhenHidden = FALSE)
  
  # Render variable selection checkboxes
  output$clusteringVariablesUI <- renderUI({
    req(clustering_values$data)
    data <- clustering_values$data
    
    # Get numeric variables only (exclude area/region name columns)
    numeric_vars <- names(data)[sapply(data, is.numeric)]
    
    checkboxGroupInput("clusteringVariables", 
                       "Pilih variabel numerik:", 
                       choices = numeric_vars,
                       selected = numeric_vars[1:min(3, length(numeric_vars))],
                       inline = FALSE)
  })
  
  # Elbow method function
  calculate_elbow <- function(data, max_k = 10) {
    wss <- numeric(max_k)
    for (k in 1:max_k) {
      km_result <- kmeans(data, centers = k, nstart = 25, iter.max = 100)
      wss[k] <- km_result$tot.withinss
    }
    
    # Find optimal k using elbow method (simple approach)
    # Calculate the rate of change
    changes <- diff(wss)
    # Find the point where the rate of change starts to decrease significantly
    optimal_k <- which(diff(changes) > 0)[1] + 1
    if(is.na(optimal_k) || optimal_k > max_k) optimal_k <- 3  # default to 3
    
    return(list(wss = wss, optimal_k = optimal_k))
  }
  
  # Run clustering analysis
  observeEvent(input$runClustering, {
    req(clustering_values$data, input$clusteringVariables)
    
    if(length(input$clusteringVariables) < 2) {
      showNotification("Pilih minimal 2 variabel untuk clustering!", type = "error")
      return()
    }
    
    tryCatch({
      data <- clustering_values$data
      selected_vars <- input$clusteringVariables
      
      # Get the region/area column (usually first column that's not numeric)
      region_col <- names(data)[!sapply(data, is.numeric)][1]
      if(is.na(region_col)) region_col <- names(data)[1]
      
      # Prepare data for clustering
      clustering_data <- data[, selected_vars, drop = FALSE]
      
      # Remove rows with missing values
      complete_cases <- complete.cases(clustering_data)
      clustering_data <- clustering_data[complete_cases, ]
      data_filtered <- data[complete_cases, ]
      
      # Standardize the data
      standardized_data <- scale(clustering_data)
      
      # Calculate elbow method
      elbow_result <- calculate_elbow(standardized_data, max_k = min(10, nrow(standardized_data) - 1))
      
      # Determine K value based on user choice
      if(input$clusterMethod == "auto") {
        selected_k <- elbow_result$optimal_k
      } else {
        selected_k <- input$manualK
      }
      
      # Run K-means with selected k
      kmeans_result <- kmeans(standardized_data, centers = selected_k, nstart = 25, iter.max = 100)
      
      # Calculate silhouette score
      if(selected_k > 1 && nrow(standardized_data) > selected_k) {
        sil_score <- silhouette(kmeans_result$cluster, dist(standardized_data))
        avg_sil_score <- mean(sil_score[, 3])
      } else {
        avg_sil_score <- 0
      }
      
      # Prepare results
      clustering_result <- data_filtered
      clustering_result$Cluster <- as.factor(kmeans_result$cluster)
      
      # Store results in reactive values
      clustering_values$clustering_result <- clustering_result
      clustering_values$selected_vars <- selected_vars
      clustering_values$optimal_k <- elbow_result$optimal_k
      clustering_values$selected_k <- selected_k
      clustering_values$elbow_data <- data.frame(
        k = 1:length(elbow_result$wss),
        wss = elbow_result$wss
      )
      clustering_values$standardized_data <- standardized_data
      clustering_values$silhouette_score <- avg_sil_score
      clustering_values$region_col <- region_col
      clustering_values$centroids <- kmeans_result$centers
      
      showNotification(paste("Clustering berhasil dengan", selected_k, "cluster!"), type = "message")
      
    }, error = function(e) {
      showNotification(paste("Error in clustering:", e$message), type = "error")
    })
  })
  
  # Output reactive untuk kondisi clustering complete
  output$clusteringComplete <- reactive({ !is.null(clustering_values$clustering_result) })
  outputOptions(output, "clusteringComplete", suspendWhenHidden = FALSE)
  
  # Elbow plot
  output$elbowPlot <- renderPlotly({
    req(clustering_values$elbow_data)
    
    elbow_data <- clustering_values$elbow_data
    optimal_k <- clustering_values$optimal_k
    selected_k <- clustering_values$selected_k
    
    p <- ggplot(elbow_data, aes(x = k, y = wss)) +
      geom_line(color = "#3498db", size = 1) +
      geom_point(color = "#3498db", size = 3) +
      geom_point(data = elbow_data[optimal_k, ], aes(x = k, y = wss), 
                 color = "#e74c3c", size = 5) +
      geom_vline(xintercept = optimal_k, linetype = "dashed", color = "#e74c3c", alpha = 0.7) +
      theme_minimal() +
      labs(title = "Elbow Method untuk Menentukan Jumlah Cluster Optimal",
           x = "Jumlah Cluster (k)",
           y = "Within-Cluster Sum of Squares (WSS)",
           subtitle = paste("K optimal (Elbow Method):", optimal_k, "| K yang dipilih:", selected_k))
    
    # Add highlight for selected K if different from optimal
    if(selected_k != optimal_k) {
      p <- p + 
        geom_point(data = elbow_data[selected_k, ], aes(x = k, y = wss), 
                   color = "#27ae60", size = 5) +
        geom_vline(xintercept = selected_k, linetype = "dashed", color = "#27ae60", alpha = 0.7)
    }
    
    ggplotly(p)
  })
  
  # Cluster summary text
  output$clusterSummaryText <- renderText({
    req(clustering_values$clustering_result, clustering_values$selected_k)
    
    result <- clustering_values$clustering_result
    selected_k <- clustering_values$selected_k
    optimal_k <- clustering_values$optimal_k
    sil_score <- clustering_values$silhouette_score
    method_used <- if(input$clusterMethod == "auto") "Otomatis (Elbow Method)" else "Manual"
    
    cluster_counts <- table(result$Cluster)
    largest_cluster <- which.max(cluster_counts)
    
    paste("Analisis clustering menggunakan metode", method_used, "dengan", selected_k, "cluster.",
          "K optimal dari Elbow Method:", optimal_k, ".",
          "Cluster terbesar adalah Cluster", largest_cluster, "dengan", max(cluster_counts), "daerah.",
          "Silhouette Score:", round(sil_score, 3))
  })
  
  # Scatter plot clustering
  output$scatterClusterPlot <- renderPlotly({
    req(clustering_values$clustering_result, clustering_values$selected_vars)
    
    result <- clustering_values$clustering_result
    vars <- clustering_values$selected_vars
    region_col <- clustering_values$region_col
    
    # Use first two variables for scatter plot
    if(length(vars) >= 2) {
      x_var <- vars[1]
      y_var <- vars[2]
      
      p <- ggplot(result, aes_string(x = x_var, y = y_var, color = "Cluster", 
                                     text = paste0("paste('Daerah:', ", region_col, ")"))) +
        geom_point(size = 3, alpha = 0.7) +
        theme_minimal() +
        labs(title = paste("Scatter Plot Clustering:", x_var, "vs", y_var),
             x = x_var, y = y_var) +
        scale_color_discrete(name = "Cluster")
      
      ggplotly(p, tooltip = c("x", "y", "colour", "text"))
    } else {
      plot_ly() %>% layout(title = "Minimal 2 variabel diperlukan untuk scatter plot")
    }
  })
  
  # Cluster centroids table
  output$clusterCentroids <- DT::renderDataTable({
    req(clustering_values$centroids, clustering_values$selected_vars)
    
    centroids <- clustering_values$centroids
    vars <- clustering_values$selected_vars
    
    # Create centroids data frame
    centroids_df <- as.data.frame(centroids)
    centroids_df$Cluster <- paste("Cluster", 1:nrow(centroids_df))
    centroids_df <- centroids_df[, c("Cluster", vars)]
    
    # Round numeric values
    centroids_df[, vars] <- round(centroids_df[, vars], 3)
    
    DT::datatable(centroids_df, 
                  options = list(pageLength = 10, scrollX = TRUE), 
                  rownames = FALSE,
                  caption = "Centroid (Titik Pusat) Setiap Cluster")
  })
  
  # Boxplot per cluster
  output$boxplotClusterPlot <- renderPlotly({
    req(clustering_values$clustering_result, clustering_values$selected_vars)
    
    result <- clustering_values$clustering_result
    vars <- clustering_values$selected_vars
    
    # Prepare data for plotting (long format)
    plot_data <- result %>%
      select(all_of(c(vars, "Cluster"))) %>%
      tidyr::gather(key = "Variable", value = "Value", -Cluster)
    
    p <- ggplot(plot_data, aes(x = Cluster, y = Value, fill = Cluster)) +
      geom_boxplot(alpha = 0.7) +
      facet_wrap(~Variable, scales = "free_y", ncol = 2) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(title = "Boxplot Variabel per Cluster",
           x = "Cluster", y = "Nilai") +
      scale_fill_discrete(name = "Cluster")
    
    ggplotly(p)
  })
  
  # Clustering map statistics
  output$cluster_count <- renderText({ 
    req(clustering_values$selected_k)
    as.character(clustering_values$selected_k) 
  })
  
  output$silhouette_score <- renderText({ 
    req(clustering_values$silhouette_score)
    round(clustering_values$silhouette_score, 3) 
  })
  
  output$largest_cluster <- renderText({
    req(clustering_values$clustering_result)
    result <- clustering_values$clustering_result
    cluster_counts <- table(result$Cluster)
    largest_cluster <- which.max(cluster_counts)
    paste("Cluster", largest_cluster)
  })
  
  output$variables_used <- renderText({
    req(clustering_values$selected_vars)
    as.character(length(clustering_values$selected_vars))
  })
  
  # Clustering map
  output$clusteringMap <- renderLeaflet({
    leaflet() %>%
      addTiles(options = providerTileOptions(minZoom = 7, maxZoom = 12)) %>%
      setView(lng = 107.6, lat = -6.9, zoom = 8)
  })
  
  # Update clustering map
  observe({
    req(clustering_values$clustering_result, jabar_geojson)
    
    result <- clustering_values$clustering_result
    region_col <- clustering_values$region_col
    vars <- clustering_values$selected_vars
    
    # Prepare data for mapping
    if(region_col %in% names(result)) {
      # Clean region names for joining
      map_data_cluster <- result %>%
        mutate(Kabupaten_Join = toupper(gsub("KABUPATEN |KOTA ", "", get(region_col))))
      
      # Join with GeoJSON
      jabar_geojson_clean <- jabar_geojson %>%
        mutate(Kabupaten_Join = toupper(gsub("KABUPATEN |KOTA ", "", KABKOT)))
      
      map_data_final <- jabar_geojson_clean %>% 
        left_join(map_data_cluster, by = "Kabupaten_Join")
      
      proxy <- leafletProxy("clusteringMap", data = map_data_final) %>% 
        clearShapes() %>% clearControls()
      
      if(nrow(map_data_final) > 0 && "Cluster" %in% names(map_data_final)) {
        # Create color palette for clusters
        cluster_levels <- levels(as.factor(map_data_final$Cluster))
        colors <- rainbow(length(cluster_levels))
        pal <- colorFactor(palette = colors, domain = cluster_levels, na.color = "gray")
        
        # Create labels with cluster info
        label_text <- paste0(
          "<strong>", map_data_final[[region_col]], "</strong><br/>",
          "Cluster: ", map_data_final$Cluster, "<br/>"
        )
        
        # Add variable values to labels
        for(var in vars) {
          if(var %in% names(map_data_final)) {
            label_text <- paste0(label_text, var, ": ", 
                                 format(map_data_final[[var]], digits = 2, big.mark = ","), "<br/>")
          }
        }
        
        labels <- lapply(label_text, htmltools::HTML)
        
        proxy %>%
          addPolygons(
            fillColor = ~pal(Cluster), 
            weight = 1.5, 
            opacity = 1, 
            color = "white",
            dashArray = "3", 
            fillOpacity = 0.8,
            highlightOptions = highlightOptions(
              weight = 2, 
              color = "white", 
              dashArray = "3", 
              fillOpacity = 0.9, 
              bringToFront = TRUE
            ),
            label = labels,
            labelOptions = labelOptions(
              style = list("font-weight" = "normal", padding = "3px 8px"), 
              textsize = "13px", 
              direction = "auto"
            )
          ) %>%
          addLegend(
            pal = pal, 
            values = ~Cluster, 
            opacity = 0.7, 
            title = "Cluster", 
            position = "bottomright"
          )
      } else {
        proxy %>% addControl("Data clustering tidak tersedia untuk peta.", position = "topright")
      }
    }
  })
  
  #=============================================================================
  # --- Halaman Forecasting (tidak diubah) ---
  #=============================================================================
  
  values_fc <- reactiveValues(data = NULL, ts_data = NULL, model_full = NULL, model_selective = NULL, forecast_result = NULL, assumptions_tested = FALSE, comparison_complete = FALSE, external_vars = NULL, correlation_matrix = NULL, current_test_model = NULL, current_model_type = NULL, last_run_model = NULL)
  
  observeEvent(input$loadSampleData, {
    data <- sample_sarimax_data
    data$Date <- as.Date(paste(data$Tahun, data$Bulan, "01", sep = "-"), "%Y-%m-%d")
    values_fc$data <- data
    values_fc$ts_data <- ts(data$Produksi.Padi, start = c(2019, 1), frequency = 12)
    values_fc$external_vars <- list(
      full = data[, c("Curah.Hujan", "Suhu", "Kelembapan", "Sinar.Matahari")],
      selective = data[, c("Curah.Hujan", "Suhu")]
    )
    
    showNotification("Data contoh berhasil dimuat!", type = "message")
  })
  
  observeEvent(input$dataFile, {
    req(input$dataFile)
    tryCatch({
      data <- read.csv(input$dataFile$datapath)
      names(data) <- gsub(" ", ".", names(data))
      
      data$Date <- as.Date(paste(data$Tahun, data$Bulan, "01", sep = "-"), "%Y-%m-%d")
      data$Produksi.Padi <- as.numeric(data$Produksi.Padi)
      data$Curah.Hujan <- as.numeric(data$Curah.Hujan)
      data$Suhu <- as.numeric(data$Suhu)
      data$Kelembapan <- as.numeric(data$Kelembapan)
      data$Sinar.Matahari <- as.numeric(data$Sinar.Matahari)
      
      data_filtered <- subset(data, Tahun >= 2019 & Tahun <= 2024)
      
      values_fc$data <- data_filtered
      values_fc$ts_data <- ts(data_filtered$Produksi.Padi, start = c(min(data_filtered$Tahun), 1), frequency = 12)
      values_fc$external_vars <- list(
        full = data_filtered[, c("Curah.Hujan", "Suhu", "Kelembapan", "Sinar.Matahari")],
        selective = data_filtered[, c("Curah.Hujan", "Suhu")]
      )
      
      showNotification("Data berhasil dimuat!", type = "message")
    }, error = function(e) {
      showNotification(paste("Error loading file:", e$message), type = "error")
    })
  })
  
  output$dataLoaded <- reactive({ !is.null(values_fc$data) })
  outputOptions(output, "dataLoaded", suspendWhenHidden = FALSE)
  
  output$dataPreview <- DT::renderDataTable({
    req(values_fc$data)
    DT::datatable(values_fc$data, options = list(scrollX = TRUE, pageLength = 10), rownames = FALSE)
  })
  
  output$totalObservations <- renderText({ req(values_fc$data); as.character(nrow(values_fc$data)) })
  output$meanProduction <- renderText({ req(values_fc$data); format(round(mean(values_fc$data$Produksi.Padi, na.rm = TRUE)), big.mark = ",") })
  output$maxProduction <- renderText({ req(values_fc$data); format(max(values_fc$data$Produksi.Padi, na.rm = TRUE), big.mark = ",") })
  output$minProduction <- renderText({ req(values_fc$data); format(min(values_fc$data$Produksi.Padi, na.rm = TRUE), big.mark = ",") })
  
  output$timeSeriesPlot <- renderPlotly({
    req(values_fc$data)
    plot_ly(values_fc$data, x = ~Date, y = ~Produksi.Padi, type = 'scatter', mode = 'lines+markers') %>%
      layout(
        title = "Time Series Produksi Padi",
        xaxis = list(title = "Tanggal"),
        yaxis = list(title = "Produksi Padi (ton)")
      )
  })
  
  output$externalCorrelationPlot <- renderPlotly({
    req(values_fc$data)
    
    external_data <- values_fc$data[, c("Curah.Hujan", "Suhu", "Kelembapan", "Sinar.Matahari")]
    var_names <- c("Curah Hujan", "Suhu", "Kelembapan", "Sinar Matahari")
    names(external_data) <- var_names
    
    cor_matrix <- cor(external_data, use = "complete.obs")
    values_fc$correlation_matrix <- cor_matrix
    
    cor_text <- round(cor_matrix, 2)
    
    plot_ly(
      x = var_names,
      y = var_names,
      z = cor_matrix,
      type = "heatmap",
      colorscale = "RdBu",
      zmin = -1,
      zmax = 1,
      showscale = FALSE
    ) %>%
      add_annotations(
        x = rep(var_names, each = length(var_names)),
        y = rep(var_names, times = length(var_names)),
        text = as.vector(cor_text),
        showarrow = FALSE,
        font = list(color = "white")
      ) %>%
      layout(
        title = "",
        xaxis = list(title = "", side = "top"),
        yaxis = list(title = "", autorange = "reversed")
      )
  })
  
  output$correlationInterpretation <- renderUI({
    req(values_fc$correlation_matrix)
    
    cor_matrix <- values_fc$correlation_matrix
    var_names <- colnames(cor_matrix)
    
    interpretation_list <- list()
    
    get_interpretation_text <- function(val) {
      strength <- case_when(
        abs(val) >= 0.8 ~ "Sangat Kuat",
        abs(val) >= 0.6 ~ "Kuat",
        abs(val) >= 0.4 ~ "Moderat",
        TRUE ~ "Lemah"
      )
      direction <- if (val > 0) "Positif" else "Negatif"
      explanation <- if (val > 0) "berbanding lurus" else "berbanding terbalik"
      
      return(list(strength = strength, direction = direction, explanation = explanation))
    }
    
    for (i in 1:(nrow(cor_matrix) - 1)) {
      for (j in (i + 1):ncol(cor_matrix)) {
        val <- cor_matrix[i, j]
        
        if (abs(val) >= 0.4) {
          interp <- get_interpretation_text(val)
          
          item_text <- tags$li(
            HTML(sprintf(
              "Terdapat korelasi <strong>%s %s</strong> antara <strong>%s</strong> dan <strong>%s</strong> (nilai: %.2f). Ini menunjukkan hubungan yang %s.",
              interp$direction, interp$strength, var_names[i], var_names[j], val, interp$explanation
            ))
          )
          interpretation_list <- append(interpretation_list, list(item_text))
        }
      }
    }
    
    if (length(interpretation_list) > 0) {
      div(
        tags$h4("Interpretasi Korelasi:", style="margin-top: 20px;"),
        tags$ul(class = "custom-list", interpretation_list),
        tags$p(HTML("<strong>Rekomendasi:</strong> Jika terdapat korelasi dengan kategori 'Kuat' atau 'Sangat Kuat', pertimbangkan untuk tidak memasukkan kedua variabel tersebut secara bersamaan dalam model untuk menghindari masalah multikolinearitas."), style="margin-top:15px;")
      )
    } else {
      div(
        tags$h4("Interpretasi Korelasi:", style="margin-top: 20px;"),
        tags$p("Tidak ditemukan korelasi yang cukup signifikan (moderat atau lebih tinggi) antar variabel.")
      )
    }
  })
  
  assumption_model <- reactive({
    req(values_fc$ts_data, values_fc$external_vars)
    
    selected_vars <- values_fc$external_vars$full
    
    model <- tryCatch({
      Arima(values_fc$ts_data, order = c(1, 1, 1), seasonal = list(order = c(1, 1, 1), period = 12), xreg = as.matrix(selected_vars))
    }, error = function(e) {
      Arima(values_fc$ts_data, order = c(1, 1, 1), xreg = as.matrix(selected_vars))
    })
    
    return(model)
  })
  
  output$adfTest <- renderPrint({ req(assumption_model()); adf.test(residuals(assumption_model())) })
  output$normalityTest <- renderPrint({ req(assumption_model()); shapiro.test(residuals(assumption_model())) })
  output$autocorrelationTest <- renderPrint({ req(assumption_model()); Box.test(residuals(assumption_model()), lag = 10, type = "Ljung-Box") })
  output$residualPlots <- renderPlotly({
    req(assumption_model())
    plot_ly(x = fitted(assumption_model()), y = residuals(assumption_model()), type = 'scatter', mode = 'markers') %>%
      layout(
        title = "Residuals vs Fitted",
        xaxis = list(title = "Fitted values"),
        yaxis = list(title = "Residuals")
      )
  })
  
  output$adfInterpretation <- renderUI({
    req(assumption_model())
    adf_result <- adf.test(residuals(assumption_model()))
    alpha <- 0.05
    
    if (adf_result$p.value < alpha) {
      tags$p(HTML(sprintf("<strong>Kesimpulan:</strong> P-value (%.4f) lebih kecil dari alpha (%.2f), maka H0 ditolak. Data residual <strong>stasioner</strong>.", adf_result$p.value, alpha)))
    } else {
      tags$p(HTML(sprintf("<strong>Kesimpulan:</strong> P-value (%.4f) lebih besar dari alpha (%.2f), maka H0 gagal ditolak. Data residual <strong>tidak stasioner</strong>.", adf_result$p.value, alpha)))
    }
  })
  
  output$normalityInterpretation <- renderUI({
    req(assumption_model())
    norm_result <- shapiro.test(residuals(assumption_model()))
    alpha <- 0.05
    
    if (norm_result$p.value < alpha) {
      tags$p(HTML(sprintf("<strong>Kesimpulan:</strong> P-value (%.4f) lebih kecil dari alpha (%.2f), maka H0 ditolak. Data residual <strong>tidak terdistribusi normal</strong>.", norm_result$p.value, alpha)))
    } else {
      tags$p(HTML(sprintf("<strong>Kesimpulan:</strong> P-value (%.4f) lebih besar dari alpha (%.2f), maka H0 gagal ditolak. Data residual <strong>terdistribusi normal</strong>.", norm_result$p.value, alpha)))
    }
  })
  
  output$autocorrelationInterpretation <- renderUI({
    req(assumption_model())
    ljung_result <- Box.test(residuals(assumption_model()), lag = 10, type = "Ljung-Box")
    alpha <- 0.05
    
    if (ljung_result$p.value < alpha) {
      tags$p(HTML(sprintf("<strong>Kesimpulan:</strong> P-value (%.4f) lebih kecil dari alpha (%.2f), maka H0 ditolak. Terdapat <strong>autokorelasi</strong> pada data residual.", ljung_result$p.value, alpha)))
    } else {
      tags$p(HTML(sprintf("<strong>Kesimpulan:</strong> P-value (%.4f) lebih besar dari alpha (%.2f), maka H0 gagal ditolak. <strong>Tidak ada autokorelasi</strong> pada data residual.", ljung_result$p.value, alpha)))
    }
  })
  
  observeEvent(input$runForecast, {
    req(values_fc$ts_data)
    selected_vars <- values_fc$external_vars$full
    
    n_forecast <- input$forecastPeriods
    
    future_xreg <- as.data.frame(lapply(selected_vars, function(v) forecast(auto.arima(ts(v, frequency=12)), h=n_forecast)$mean))
    
    model <- Arima(values_fc$ts_data,
                   order = c(input$p, input$d, input$q),
                   seasonal = list(order = c(input$P, input$D, input$Q), period = input$s),
                   xreg = as.matrix(selected_vars))
    
    values_fc$last_run_model <- model
    values_fc$forecast_result <- forecast(model, h = n_forecast, xreg = as.matrix(future_xreg))
    
    values_fc$model_full <- model
  })
  
  output$forecastComplete <- reactive({ !is.null(values_fc$forecast_result) })
  outputOptions(output, "forecastComplete", suspendWhenHidden = FALSE)
  
  output$selectedModel <- renderText({ "Full Model" })
  output$trendDirection <- renderText({ req(values_fc$forecast_result); if(mean(values_fc$forecast_result$mean) > tail(values_fc$ts_data,1)) "Naik" else "Turun" })
  output$trendAnalysis <- renderText({ req(values_fc$forecast_result); "Analisis tren berdasarkan hasil peramalan." })
  
  output$forecastPlot <- renderPlotly({
    req(values_fc$forecast_result, values_fc$data)
    
    forecast_obj <- values_fc$forecast_result
    
    historical_data <- data.frame(
      Date = values_fc$data$Date,
      Value = as.numeric(forecast_obj$x)
    )
    
    last_historical_date <- tail(historical_data$Date, 1)
    forecast_dates <- seq(from = last_historical_date, by = "month", length.out = length(forecast_obj$mean) + 1)[-1]
    
    forecast_data <- data.frame(
      Date = forecast_dates,
      Point.Forecast = as.numeric(forecast_obj$mean),
      Lo.95 = as.numeric(forecast_obj$lower[, "95%"]),
      Hi.95 = as.numeric(forecast_obj$upper[, "95%"])
    )
    
    plot_ly() %>%
      add_ribbons(data = forecast_data,
                  x = ~Date,
                  ymin = ~Lo.95,
                  ymax = ~Hi.95,
                  line = list(color = 'rgba(0,100,80,0.1)'),
                  fillcolor = 'rgba(0,100,80,0.2)',
                  name = "Interval Kepercayaan 95%") %>%
      add_lines(data = historical_data, 
                x = ~Date, 
                y = ~Value,
                line = list(color = 'black'),
                name = "Data Historis") %>%
      add_lines(data = forecast_data, 
                x = ~Date, 
                y = ~Point.Forecast,
                line = list(color = 'blue', dash = 'dash'),
                name = "Forecast") %>%
      layout(
        title = "Hasil Peramalan Produksi Padi",
        xaxis = list(title = "Waktu"),
        yaxis = list(title = "Produksi Padi (ton)"),
        showlegend = TRUE
      )
  })
  
  output$forecastTable <- DT::renderDataTable({ req(values_fc$forecast_result); df <- as.data.frame(values_fc$forecast_result); DT::datatable(df, options=list(pageLength=12, scrollX=TRUE), rownames=TRUE) })
  output$modelSummary <- renderPrint({
    req(values_fc$last_run_model)
    summary(values_fc$last_run_model)
  })
}

# ===================================================================
# MENJALANKAN APLIKASI
# ===================================================================
shinyApp(ui = ui, server = server)