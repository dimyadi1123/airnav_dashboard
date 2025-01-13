library(shiny)
library(shinydashboard)
library(ggplot2)
library(scales)
library(dplyr)
library(ggpubr)
library(DT)
library(fontawesome)
library(grid)
library(plotly)
library(tidyr)

# Fungsi css untuk melakukan customisasi page
css <- function(){
  tags$head(
    tags$title("AirNav Indonesia"),  # Menambahkan nama website di tab browser
    tags$link(rel = "icon", href = "https://marketingplatform.digipop.ai/api/assets/uploads/landing_page/133/2024-04/1714038891.png", type = "image/png"),  # Menambahkan favicon
    tags$style(HTML("
      /* Header */
      .main-header {
        position: fixed; /* Menetapkan posisi tetap */
        top: 0; /* Menempelkan header di atas */
        width: 100%; /* Lebar penuh sesuai viewport */
        z-index: 1000; /* Prioritas di atas elemen lain */
        background-color: #007bff;
        box-shadow: 0px 2px 5px rgba(0, 0, 0, 0.1); /* Memberikan bayangan untuk efek elevasi */
      }
      
      .airnav-logo {
        width: 120px;   /* Lebar gambar */
        height: 36px;   /* Tinggi gambar */
      }
      
      .title-container {
        display: block;
        align-items: space-between;
      }

      .title-text {
        font-size: 24px;  /* Ukuran font untuk title */
        font-weight: bold;
        color: #333;
        margin-left: 10px;
      }
      
      .main-header .navbar-nav {
        margin-left: auto; /* Memindahkan item ke sebelah kanan */
        display: flex;
        align-items: center;
      }

      .main-header .navbar-nav .dropdown {
        margin-left: 15px; /* Memberikan jarak antar link */
      }
      
      /* Styling untuk navbar links */
      .main-header .navbar-nav .dropdown a {
        color: #fff;
        font-size: 16px;
        margin-top: 5px;
        padding: 10px 15px;
        transition: color 0.3s ease;
      }

      .main-header .navbar-nav .dropdown a:hover {
        color: #007bff;
      }
      
      /* Sidebar */
      .main-sidebar {
        background-color: #343a40;
        height: calc(100vh - 50px); /* Menyesuaikan tinggi sidebar dengan mengurangi tinggi header */
        padding-top: 60px;
        position: fixed;
        width: 250px;
        transition: all 0.3s;
        box-shadow: 2px 0 5px rgba(0, 0, 0, 0.2); /* Menambahkan shadow */
        top: 50px; /* Menyesuaikan posisi di bawah header */
      }
      
      .main-sidebar .sidebar {
        padding: 0;
      }
      
      .main-sidebar .sidebar-menu > li {
        padding: 15px;
        text-align: left;
        transition: background-color 0.3s ease; /* Efek transisi halus */
      }
      
      .main-sidebar .sidebar-menu > li.active,
      .main-sidebar .sidebar-menu > li:hover {
        background-color: #495057;
        color: #fff;
      }
      
      /* Mengubah tampilan link dalam sidebar */
      .main-sidebar .sidebar-menu > li > a {
        color: #adb5bd;
        font-size: 18px;
        text-decoration: none;
        transition: color 0.3s ease;
      }
      
      .main-sidebar .sidebar-menu > li > a:hover {
        color: #ffffff;
      }
      
      /* Konten utama */
      .content-wrapper {
        margin-left: 250px; /* Memberikan ruang untuk sidebar */
        margin-top: 50px; /* Memberikan ruang untuk header tetap */
        background-color: #f4f6f9;
        padding: 20px;
        overflow-x: hidden;
        transition: margin-left 0.3s ease; /* Efek transisi saat sidebar diubah */
      }
      
      /* Tampilan file upload */
      .upload-file {
        padding: 20px;
        background-color: #ffffff;
        border-radius: 5px;
        box-shadow: 0 4px 8px rgba(0, 0, 0, 0.1);
        margin-bottom: 20px;
      }
      
      /* Mengaktifkan scroll horizontal pada tabel */
      .dataTables_wrapper {
        overflow-x: auto;
        margin-top: 20px;
      }
      
      /* Menambahkan style untuk responsive layout */
      @media (max-width: 768px) {
        .main-header .logo {
          font-size: 20px;
        }
        
        .main-sidebar {
          width: 200px;
        }
        
        .content-wrapper {
          margin-left: 200px;
        }
        
        .main-sidebar .sidebar-menu > li {
          padding: 12px;
        }
        
        /* Sidebar menjadi collapsible */
        .main-sidebar {
          width: 100%;
          padding-top: 50px;
        }
        
        .content-wrapper {
          margin-left: 0;
        }
      }
    "))
  )
}



# UI
ui <- dashboardPage(
  dashboardHeader(title = tags$div(
    class = "title-container",
    tags$img(src="https://www.airnavindonesia.co.id/wp-content/uploads/2024/09/LOGOAIRNAVINDONESIALandscapePutih-1.png", 
             alt="AirNav Indonesia Logo", 
             class="airnav-logo"),
    tags$h1("AirNav Indonesia Dashboard", class="title-text")
  ),
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
          # ValueBox untuk Total Transaksi
          column(6, 
                 valueBoxOutput("total_transaksi", width = 12)
          ),
          # ValueBox untuk Akumulasi IDR
          column(6, 
                 valueBoxOutput("akumulasi_idr", width = 12)
          )
        ),
        
        fluidRow(
          # ValueBox untuk Rata-rata Transaksi
          column(6, 
                 valueBoxOutput("rata2_transaksi", width = 12)
          ),
          # ValueBox untuk Rata-rata IDR
          column(6, 
                 valueBoxOutput("rata2_idr", width = 12)
          )
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
                           box(title = "Statistik Maskapai", status = "info", solidHeader = TRUE, width = 12,
                               valueBoxOutput("total_maskapai"),
                               valueBoxOutput("maskapai_completed"),
                               valueBoxOutput("maskapai_open"),
                               valueBoxOutput("maskapai_waiting_reconcile"),
                               valueBoxOutput("maskapai_waiting_payment")
                           )
                         ),
                         fluidRow(
                           # Kolom pertama untuk Top Retensi dan Top Open
                           column(6,
                                  box(title = "Top Retensi Maskapai", status = "info", solidHeader = TRUE, width = 12,
                                      tableOutput("top_retensi_maskapai")
                                  ),
                                  box(title = "Top Open Maskapai", status = "info", solidHeader = TRUE, width = 12,
                                      tableOutput("top_open_maskapai")
                                  )
                           ),
                           # Kolom kedua untuk Top Waiting Payment dan Top Completed
                           column(6,
                                  box(title = "Top Waiting Payment Maskapai", status = "info", solidHeader = TRUE, width = 12,
                                      tableOutput("top_waiting_payment_maskapai")
                                  ),
                                  box(title = "Top Completed Maskapai", status = "info", solidHeader = TRUE, width = 12,
                                      tableOutput("top_completed_maskapai")
                                  )
                           )
                         )
                ),
                
                tabPanel("Filter & Visualisasi", 
                         fluidRow(
                           # Box untuk Filter (2x2 layout)
                           box(
                             title = "Filter Maskapai dan Transaksi", 
                             status = "primary", 
                             solidHeader = TRUE, 
                             width = 12,
                             fluidRow(
                               column(6, 
                                      selectInput("filter_maskapai", "Pilih Maskapai:", choices = NULL)
                               ),
                               column(6, 
                                      selectInput("filter_status", "Status Pembayaran:", 
                                                  choices = c("All", "Open", "Waiting Payment", "Waiting Reconciale", "Completed"))
                               )
                             ),
                             fluidRow(
                               column(6, 
                                      selectInput("filter_bulan", "Pilih Bulan:", choices = NULL, multiple = TRUE)
                               ),
                               column(6, 
                                      selectInput("filter_tahun", "Pilih Tahun:", choices = NULL, multiple = TRUE)
                               )
                             )
                           ),
                           
                           # Box untuk Visualisasi
                           box(
                             title = "Visualisasi", 
                             status = "primary", 
                             solidHeader = TRUE, 
                             width = 12,
                             plotlyOutput("filter_visual_transaksi", height = "400px"),
                             plotlyOutput("filter_visual_idr", height = "400px")
                           )
                         ))
              ))
    )
  )
)
