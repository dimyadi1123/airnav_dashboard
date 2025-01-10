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
