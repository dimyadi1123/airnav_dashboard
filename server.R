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
  
  # Mengelompokkan data berdasarkan bulan
  transaksi_per_bulan <- data() %>%
    mutate(month = format(Invoice.Date, "%Y-%m")) %>%
    group_by(month) %>%
    summarise(total_transaksi = n(), total_idr = sum(Total..IDR., na.rm = TRUE))
  
  # Membuat Line Plot Responsif
  plot_ly(transaksi_per_bulan, 
          x = ~month, 
          y = ~total_transaksi, 
          type = 'scatter', 
          mode = 'lines+markers', # Menampilkan garis dan titik
          line = list(color = 'rgba(55, 128, 191, 1.0)', width = 2), # Gaya garis
          marker = list(size = 8, color = 'rgba(255, 99, 71, 0.8)'), # Gaya titik
          hoverinfo = "x+y", # Menampilkan informasi saat hover
          hovertemplate = ~paste("Bulan: ", month, 
                                 "<br>Total Transaksi: ", total_transaksi, 
                                 "<br>Total IDR: ", scales::comma(total_idr), "<extra></extra>")
  ) %>%
    layout(
      title = "Visualisasi Transaksi per Bulan",
      xaxis = list(title = "Bulan", 
                   tickangle = -45, # Memiringkan label bulan
                   showgrid = FALSE), # Menghilangkan garis grid vertikal
      yaxis = list(title = "Total Transaksi"),
      hovermode = "x unified", # Menyatukan hover untuk semua elemen di titik x
      margin = list(t = 50, b = 80) # Mengatur margin agar lebih rapi
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
  
  # Statistik Maskapai per Status Pembayaran
  output$maskapai_completed <- renderValueBox({
    req(data())
    jumlah_completed <- data() %>%
      filter(Status.Pay == "completed") %>%
      summarise(jumlah_maskapai = n_distinct(Airname)) %>%
      pull(jumlah_maskapai)
    
    valueBox(
      jumlah_completed, "Maskapai (Completed)", icon = icon("check-circle"), color = "green"
    )
  })
  
  output$maskapai_open <- renderValueBox({
    req(data())
    jumlah_open <- data() %>%
      filter(Status.Pay == "Open") %>%
      summarise(jumlah_maskapai = n_distinct(Airname)) %>%
      pull(jumlah_maskapai)
    
    valueBox(
      jumlah_open, "Maskapai (Open)", icon = icon("exclamation-circle"), color = "yellow"
    )
  })
  
  output$maskapai_waiting_reconcile <- renderValueBox({
    req(data())
    jumlah_waiting_reconcile <- data() %>%
      filter(Status.Pay == "Waiting Reconciale") %>%
      summarise(jumlah_maskapai = n_distinct(Airname)) %>%
      pull(jumlah_maskapai)
    
    valueBox(
      jumlah_waiting_reconcile, "Maskapai (Waiting Reconcile)", icon = icon("sync-alt"), color = "blue"
    )
  })
  
  output$maskapai_waiting_payment <- renderValueBox({
    req(data())
    jumlah_waiting_payment <- data() %>%
      filter(Status.Pay == "Waiting Payment") %>%
      summarise(jumlah_maskapai = n_distinct(Airname)) %>%
      pull(jumlah_maskapai)
    
    valueBox(
      jumlah_waiting_payment, "Maskapai (Waiting Payment)", icon = icon("credit-card"), color = "red"
    )
  })
  
  output$top_retensi_maskapai <- renderTable({
    req(data())
    
    # Menghitung total transaksi per maskapai
    retensi <- data() %>%
      group_by(Airname) %>%
      summarise(retensi_transaksi = n(), .groups = 'drop') %>%
      arrange(desc(retensi_transaksi))
    
    # Menyusun Top 5 dan Bottom 5 maskapai berdasarkan total transaksi
    top5 <- retensi %>% slice_head(n = 5)  # Top 5
    bottom5 <- retensi %>% slice_tail(n = 5)  # Bottom 5
    
    # Membuat tabel hasil dengan header yang diinginkan
    top5_table <- top5 %>%
      rename(Airname = Airname, Retensi_transaksi = retensi_transaksi) %>%
      mutate(Header = "Top 5 Teratas") %>%
      select(Header, Airname, Retensi_transaksi)
    
    bottom5_table <- bottom5 %>%
      rename(Airname = Airname, Retensi_transaksi = retensi_transaksi) %>%
      mutate(Header = "Top 5 Terbawah") %>%
      select(Header, Airname, Retensi_transaksi)
    
    # Gabungkan tabel Top 5 dan Bottom 5
    final_table <- bind_rows(top5_table, bottom5_table)
    
    return(final_table)
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
      filter(Status.Pay == "completed") %>%
      group_by(Airname) %>%
      summarise(total_transaksi = n(), total_biaya = sum(Total..IDR., na.rm = TRUE), .groups = 'drop') %>%
      arrange(desc(total_transaksi)) %>%
      slice_head(n = 5)
    
    top_completed
  })
  
  # Dropdown dinamis untuk filter
  observe({
    req(data())
    
    # Dropdown Maskapai
    updateSelectInput(session, "filter_maskapai",
                      choices = c("All", unique(data()$Airname)),
                      selected = "All")
    
    # Dropdown Status Pembayaran
    updateSelectInput(session, "filter_status",
                      choices = c("All", "Open", "Waiting Payment", "Waiting Reconciale", "Completed"),
                      selected = "All")
    
    # Dropdown Bulan
    updateSelectInput(session, "filter_bulan",
                      choices = c("All", month.name),
                      selected = "All")
    
    # Dropdown Tahun
    available_years <- c("All", unique(format(data()$Invoice.Date, "%Y")))
    updateSelectInput(session, "filter_tahun",
                      choices = available_years,
                      selected = "All")
  })
  
  # Filter data berdasarkan input
  filtered_data <- reactive({
    req(data())
    df <- data()
    
    # Filter Maskapai
    if (input$filter_maskapai != "All") {
      df <- df %>% filter(Airname == input$filter_maskapai)
    }
    
    # Filter Status Pembayaran
    if (input$filter_status != "All") {
      df <- df %>% filter(Status.Pay == input$filter_status)
    }
    
    # Filter Bulan
    if (!is.null(input$filter_bulan) && !"All" %in% input$filter_bulan) {
      df <- df %>% filter(format(Invoice.Date, "%B") %in% input$filter_bulan)
    }
    
    # Filter Tahun
    if (!is.null(input$filter_tahun) && !"All" %in% input$filter_tahun) {
      df <- df %>% filter(format(Invoice.Date, "%Y") %in% input$filter_tahun)
    }
    
    return(df)
  })
  
  output$filter_visual_transaksi <- renderPlotly({
    req(filtered_data())
    
    transaksi_per_bulan <- filtered_data() %>%
      group_by(month = format(Invoice.Date, "%Y-%m")) %>%
      summarise(total_transaksi = n(), .groups = 'drop')
    
    plot_ly(
      data = transaksi_per_bulan,
      x = ~month,
      y = ~total_transaksi,
      type = 'bar',
      hoverinfo = "x+y",  # Menampilkan informasi hanya saat hover
      hovertemplate = ~paste(
        "Bulan: ", month, 
        "<br>Total Transaksi ", format(total_transaksi, scientific = FALSE),
        "<extra></extra>"
      ),
      marker = list(
        color = 'rgba(55, 128, 191, 0.7)', 
        line = list(color = 'rgba(55, 128, 191, 1.0)', width = 1.5)
      )
    ) %>%
      layout(
        title = "Visualisasi Transaksi per Bulan",
        xaxis = list(title = "Bulan"),
        yaxis = list(title = "Total Transaksi"),
        showlegend = FALSE
      )
    
  })
  
  
  
  output$filter_visual_idr <- renderPlotly({
    req(filtered_data())
    
    pembayaran_per_bulan <- filtered_data() %>%
      group_by(month = format(Invoice.Date, "%Y-%m")) %>%
      summarise(total_idr = sum(Total..IDR., na.rm = TRUE), .groups = 'drop')
    
    plot_ly(
      data = pembayaran_per_bulan,
      x = ~month,
      y = ~total_idr,
      type = "bar",
      hoverinfo = "x+y",  # Menampilkan informasi hanya saat hover
      hovertemplate = ~paste(
        "Bulan: ", month, 
        "<br>Total Pembayaran: Rp ", format(total_idr, big.mark = ".", scientific = FALSE),  # Format angka Rupiah
        "<extra></extra>"
      ),
      marker = list(
        color = 'rgba(50, 171, 96, 0.7)', 
        line = list(color = 'rgba(50, 171, 196, 0.7)', width = 1.5)
      )
    ) %>%
      layout(
        title = "Total Pembayaran per Bulan",
        xaxis = list(title = "Bulan"),
        yaxis = list(title = "Total Pembayaran (IDR)"),
        showlegend = FALSE  # Tidak menampilkan legenda tambahan
      )
  })
  
    
}
  
  
  # Tambahkan logika lainnya untuk fitur seperti visualisasi dan tabel sesuai kebutuhan