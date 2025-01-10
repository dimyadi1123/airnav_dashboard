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