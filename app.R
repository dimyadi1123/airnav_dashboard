library(shiny)
library(shinydashboard)
library(RColorBrewer)
library(ggplot2)
library(scales)
library(dplyr)
library(wesanderson)
library(ggpubr)
library(DT)
library(fontawesome)
library(grid)
library(plotly)
library(tidyr)

# Fungsi css untuk melakukan customisasi page
css <- function(){
  tags$head(
    tags$style(HTML("
      .main-header .logo {
        font-size: 18px;
      }
      
      li {
        list-style-type: none;
        margin: 15px 20px;
        font-size: 18px;
      }
      
      .fa {
        margin-right: 3px;
        width: 22px;
      }
      
      .content-wrapper {
        background-color: white;
      }
      
      .upload-file {
        padding-left: 20px;
        padding-right: 20px;
      }
      
      
      /* Tambahkan CSS untuk mengaktifkan scroll horizontal */
      .dataTables_wrapper {
        overflow-x: scroll;
      }
    "))
  )
}

# UI
ui <- dashboardPage(
  dashboardHeader(title = tags$img(src = "https://www.airnavindonesia.co.id/wp-content/uploads/2024/09/LOGOAIRNAVINDONESIALandscapePutih-1.png", 
                                   height = '36', width = '120'),
                  tags$li(class="dropdown",tags$a(href="https://www.linkedin.com/in/dimyadi/" ,icon("linkedin"), "My Profile", target="_blank")),
                  tags$li(class="dropdown",tags$a(href="https://github.com/dimyadi1123", icon("github"), "Source Code", target="_blank"))),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Data", tabName = "Data", icon = icon("database"), selected = TRUE),
      menuItem("Transaksi", icon = icon("credit-card"),
               menuSubItem("Statistik Transaksi", tabName = "stat_transaksi", icon = icon("chart-line")),
               menuSubItem("Tabel Transaksi", tabName = "tabel_transaksi", icon = icon("table"))),
      menuItem("Maskapai", tabName = "maskapai", icon = icon("plane"))
    )
  ),
  dashboardBody(
    css(),
    tabItems(
      # Tab Data
      tabItem(tabName = "Data",
              fluidRow(
                box(
                  title = "Upload Data Invoice", status = "primary", solidHeader = TRUE, width = 12,
                  fileInput("upload", "Upload File CSV", accept = ".csv"),
                  dataTableOutput("data_table")
                )
              )),
      
      # Tab Statistik Transaksi
      tabItem(
        tabName = "stat_transaksi",
        h2("Statistik Transaksi"),
        fluidRow(
          valueBoxOutput("total_transaksi"),
          valueBoxOutput("akumulasi_idr"),
          valueBoxOutput("rata2_transaksi"),
          valueBoxOutput("rata2_idr")
        ),
        h2("Visualisasi Transaksi per Bulan"),
        plotlyOutput("visualisasi_transaksi", height = "400px")
      ),
      
      # Tab Tabel Transaksi
      tabItem(tabName = "tabel_transaksi",
              fluidRow(
                box(title = "Tabel Transaksi Maskapai per Status Pembayaran", status = "primary", solidHeader = TRUE, 
                    dataTableOutput("tabel_transaksi_per_status")),
                box(title = "Tabel Pembayaran Maskapai per Status Pembayaran", status = "primary", solidHeader = TRUE, 
                    dataTableOutput("tabel_pembayaran_per_status"))
              )),
      
      # Tab Maskapai
      tabItem(tabName = "maskapai",
              tabsetPanel(
                tabPanel("Statistik Maskapai",
                         fluidRow(
                           box(title = "Jumlah Maskapai", status = "info", solidHeader = TRUE, width = 12,
                               valueBoxOutput("total_maskapai")),
                           box(title = "Count Maskapai per Status Pembayaran", status = "primary", solidHeader = TRUE, width = 12,
                               dataTableOutput("maskapai_per_status")),
                           box(title = "Maskapai dengan Retensi Penggunaan Tertinggi", status = "success", solidHeader = TRUE, width = 12,
                               tableOutput("top_retensi_maskapai")),
                           box(title = "Maskapai dengan Open Terbanyak", status = "warning", solidHeader = TRUE, width = 12,
                               tableOutput("top_open_maskapai")),
                           box(title = "Maskapai dengan Waiting Payment Terbanyak", status = "danger", solidHeader = TRUE, width = 12,
                               tableOutput("top_waiting_payment_maskapai")),
                           box(title = "Maskapai dengan Completed Terbanyak", status = "success", solidHeader = TRUE, width = 12,
                               tableOutput("top_completed_maskapai"))
                         )),
                tabPanel("Filter & Visualisasi", 
                         fluidRow(
                           box(title = "Filter Maskapai", status = "primary", solidHeader = TRUE, width = 12,
                               textInput("search_maskapai", "Cari Maskapai:"),
                               selectInput("status_pay_filter", "Status Pembayaran:", 
                                           choices = c("All", "Open", "Waiting Payment", "Completed")),
                               sliderInput("month_filter", "Pilih Bulan:", min = 1, max = 12, value = c(1, 12))),
                           box(title = "Visualisasi", status = "primary", solidHeader = TRUE, width = 12,
                               plotOutput("filter_visual_transaksi"),
                               plotOutput("filter_visual_idr"))
                         ))
              ))
    )
  )
)

# Server
server <- function(input, output, session) {
  options(shiny.maxRequestSize = 50 * 1024^2)
  
  # Load Data dengan Preprocessing
  data <- reactive({
    req(input$upload)
    raw_data <- read.csv(input$upload$datapath)
    
    # Preprocessing
    preprocessed_data <- raw_data %>%
      mutate(
        # Konversi Total..IDR. ke numerik
        Total..IDR. = as.numeric(gsub("[^0-9.-]", "", Total..IDR.)),
        
        # Konversi Invoice.Date dari format m/d/Y ke format Date
        Invoice.Date = as.Date(Invoice.Date, format = "%m/%d/%Y"),
        
        # Pecah Invoice.Date menjadi year, month, day
        year = format(Invoice.Date, "%Y"),
        month = format(Invoice.Date, "%m"),
        day = format(Invoice.Date, "%d")
      )
    
    return(preprocessed_data)
  })
  
  output$data_table <- renderDataTable({
    req(data())
    datatable(data())
  })
  
  
  # Statistik Transaksi (1 Baris dengan 4 Output)
  output$total_transaksi <- renderValueBox({
    req(data())
    valueBox(
      nrow(data()), "Total Transaksi", icon = icon("list"), color = "blue"
    )
  })
  
  output$akumulasi_idr <- renderValueBox({
    req(data())
    valueBox(
      format(sum(data()$Total..IDR., na.rm = TRUE), big.mark = ",", scientific = FALSE),
      "Total IDR",
      icon = icon("dollar-sign"),
      color = "green"
    )
  })
  
  output$rata2_transaksi <- renderValueBox({
    req(data())
    rata2_transaksi <- data() %>%
      mutate(month = format(Invoice.Date, "%Y-%m")) %>%
      group_by(month) %>%
      summarise(transaksi_per_bulan = n()) %>%
      summarise(mean_transaksi = mean(transaksi_per_bulan, na.rm = TRUE)) %>%
      pull(mean_transaksi)
    
    valueBox(
      format(rata2_transaksi, big.mark = ",", scientific = FALSE),
      "Rata-Rata Transaksi per Bulan",
      icon = icon("chart-line"),
      color = "yellow"
    )
  })
  
  output$rata2_idr <- renderValueBox({
    req(data())
    
    rata2_idr <- data() %>%
      mutate(month = format(as.Date(Invoice.Date, "%Y-%m-%d"), "%Y-%m")) %>% # Ekstrak bulan dan tahun
      summarise(total_idr = sum(Total..IDR., na.rm = TRUE)) %>% # Total keseluruhan IDR
      pull(total_idr) / 16 # Dibagi 16 bulan (September 2023 - Desember 2024)
    
    valueBox(
      format(round(rata2_idr, 2), big.mark = ",", scientific = FALSE),
      "Rata-Rata Total IDR per Bulan",
      icon = icon("chart-bar"),
      color = "purple"
    )
  })
  
  
  # Visualisasi Transaksi Responsif
  output$visualisasi_transaksi <- renderPlotly({
    req(data())
    
    transaksi_per_bulan <- data() %>%
      mutate(month = format(Invoice.Date, "%Y-%m")) %>%
      group_by(month) %>%
      summarise(total_transaksi = n(), total_idr = sum(Total..IDR., na.rm = TRUE))
    
    # Plot Bar Responsif
    plot_ly(transaksi_per_bulan, 
            x = ~month, 
            y = ~total_transaksi, 
            type = 'bar', 
            text = ~paste("Total Transaksi: ", total_transaksi, 
                          "<br>Total IDR: ", scales::comma(total_idr)),
            hoverinfo = "text",
            marker = list(color = 'rgba(55, 128, 191, 0.7)', 
                          line = list(color = 'rgba(55, 128, 191, 1.0)', width = 1.5))
    ) %>%
      layout(
        title = "Visualisasi Transaksi per Bulan",
        xaxis = list(title = "Bulan"),
        yaxis = list(title = "Total Transaksi")
      )
  })
  
  # Tabel Transaksi Maskapai per Status Pembayaran
  output$tabel_transaksi_per_status <- renderDataTable({
    req(data())
    
    # Hitung total jumlah transaksi berdasarkan status pembayaran untuk setiap maskapai
    transaksi_per_status <- data() %>%
      group_by(Airname, Status.Pay) %>%
      summarise(jumlah_transaksi = n(), .groups = 'drop') %>%
      pivot_wider(names_from = Status.Pay, values_from = jumlah_transaksi, values_fill = list(jumlah_transaksi = 0)) %>%
      arrange(desc(rowSums(across(where(is.numeric))))) # Urutkan berdasarkan total jumlah transaksi
    
    # Tampilkan tabel dalam format datatable
    datatable(transaksi_per_status, options = list(pageLength = 5, scrollX = TRUE))
  })
  
  # Tabel Pembayaran Maskapai per Status Pembayaran
  output$tabel_pembayaran_per_status <- renderDataTable({
    req(data())
    
    # Hitung total akumulasi biaya berdasarkan status pembayaran untuk setiap maskapai
    pembayaran_per_status <- data() %>%
      group_by(Airname, Status.Pay) %>%
      summarise(total_idr = sum(Total..IDR., na.rm = TRUE), .groups = 'drop') %>%
      pivot_wider(names_from = Status.Pay, values_from = total_idr, values_fill = list(total_idr = 0)) %>%
      arrange(desc(rowSums(across(where(is.numeric))))) # Urutkan berdasarkan total IDR
    
    # Tampilkan tabel dalam format datatable
    datatable(pembayaran_per_status, options = list(pageLength = 5, scrollX = TRUE))
  })
  
  # Total jumlah maskapai
  output$total_maskapai <- renderValueBox({
    req(data())
    total_maskapai <- n_distinct(data()$Airname)
    valueBox(
      total_maskapai, "Jumlah Maskapai", icon = icon("plane"), color = "blue"
    )
  })
  
  # Count maskapai per status pembayaran
  output$maskapai_per_status <- renderDataTable({
    req(data())
    maskapai_per_status <- data() %>%
      group_by(Status.Pay) %>%
      summarise(jumlah_maskapai = n_distinct(Airname), .groups = 'drop') %>%
      arrange(desc(jumlah_maskapai))
    
    datatable(maskapai_per_status, options = list(pageLength = 5, scrollX = TRUE))
  })
  
  # Maskapai dengan retensi penggunaan tertinggi (5 atas dan bawah)
  output$top_retensi_maskapai <- renderTable({
    req(data())
    retensi <- data() %>%
      group_by(Airname) %>%
      summarise(total_transaksi = n(), .groups = 'drop') %>%
      arrange(desc(total_transaksi))
    
    top5 <- retensi %>% slice_head(n = 5)  # Top 5
    bottom5 <- retensi %>% slice_tail(n = 5)  # Bottom 5
    
    list(
      "Top 5 Retensi" = top5,
      "Bottom 5 Retensi" = bottom5
    )
  })
  
  # Maskapai dengan nilai open terbanyak
  output$top_open_maskapai <- renderTable({
    req(data())
    top_open <- data() %>%
      filter(Status.Pay == "Open") %>%
      group_by(Airname) %>%
      summarise(total_transaksi = n(), total_biaya = sum(Total..IDR., na.rm = TRUE), .groups = 'drop') %>%
      arrange(desc(total_transaksi)) %>%
      slice_head(n = 5)
    
    top_open
  })
  
  # Maskapai dengan nilai waiting payment terbanyak
  output$top_waiting_payment_maskapai <- renderTable({
    req(data())
    top_waiting_payment <- data() %>%
      filter(Status.Pay == "Waiting Payment") %>%
      group_by(Airname) %>%
      summarise(total_transaksi = n(), total_biaya = sum(Total..IDR., na.rm = TRUE), .groups = 'drop') %>%
      arrange(desc(total_transaksi)) %>%
      slice_head(n = 5)
    
    top_waiting_payment
  })
  
  # Maskapai dengan nilai completed terbanyak
  output$top_completed_maskapai <- renderTable({
    req(data())
    top_completed <- data() %>%
      filter(Status.Pay == "Completed") %>%
      group_by(Airname) %>%
      summarise(total_transaksi = n(), total_biaya = sum(Total..IDR., na.rm = TRUE), .groups = 'drop') %>%
      arrange(desc(total_transaksi)) %>%
      slice_head(n = 5)
    
    top_completed
  })
  
  
  # Tambahkan logika lainnya untuk fitur seperti visualisasi dan tabel sesuai kebutuhan
}

shinyApp(ui =ui, server = server)
