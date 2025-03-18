library(shiny)
library(ggplot2)

# UI
ui <- fluidPage(
  titlePanel("🌍 Aplikasi Pengenalan Risiko Bencana Alam (Fuzzy Logic)"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("curah_hujan", "Curah Hujan (mm):", min = 0, max = 100, value = 0),
      sliderInput("kelembaban_tanah", "Kelembaban Tanah (%):", min = 0, max = 100, value = 0),
      sliderInput("kecepatan_angin", "Kecepatan Angin (km/jam):", min = 0, max = 100, value = 0),
      actionButton("cek_risiko", "🔍 Cek Risiko")
    ),
    
    mainPanel(
      h3("🔹 Hasil Analisis", style = "color:blue"),
      verbatimTextOutput("hasil_risiko"),
      
      h4("✅ Rekomendasi Tindakan"),
      verbatimTextOutput("rekomendasi_risiko"),
      
      h4("📊 Derajat Keanggotaan (Miu) untuk Setiap Risiko"),
      verbatimTextOutput("miu_risiko"),
      
      plotOutput("grafik_risiko")
    )
  )
)

# SERVER
server <- function(input, output) {
  observeEvent(input$cek_risiko, {
    
    # Normalisasi input (0-1)
    curah_hujan_n <- input$curah_hujan / 100
    kelembaban_tanah_n <- input$kelembaban_tanah / 100
    kecepatan_angin_n <- input$kecepatan_angin / 100
    
    # 🔹 Perhitungan Miu
    miu_banjir <- curah_hujan_n
    miu_longsor <- kelembaban_tanah_n
    miu_angin_kencang <- kecepatan_angin_n
    miu_bencana_ekstrem <- 0  # Default tidak ada bencana ekstrem
    
    # Jika semua parameter 100%, hanya "Bencana Ekstrem"
    if (input$curah_hujan == 100 && input$kelembaban_tanah == 100 && input$kecepatan_angin == 100) {
      miu_banjir <- 0
      miu_longsor <- 0
      miu_angin_kencang <- 0
      miu_bencana_ekstrem <- 1
    }
    
    # 🔹 Normalisasi agar total Miu = 1
    total_miu <- miu_banjir + miu_longsor + miu_angin_kencang + miu_bencana_ekstrem
    if (total_miu > 0) {
      miu_banjir <- miu_banjir / total_miu
      miu_longsor <- miu_longsor / total_miu
      miu_angin_kencang <- miu_angin_kencang / total_miu
      miu_bencana_ekstrem <- miu_bencana_ekstrem / total_miu
    }
    
    # 🔹 Menentukan bencana utama berdasarkan miu tertinggi
    bencana_potensial <- c(
      if (miu_banjir > 0) "Banjir",
      if (miu_longsor > 0) "Longsor",
      if (miu_angin_kencang > 0) "Angin Kencang",
      if (miu_bencana_ekstrem > 0) "Bencana Ekstrem (Banjir, Longsor, Angin Kencang)"
    )
    
    # 🔹 Rekomendasi Tindakan
    rekomendasi <- c()
    if (miu_banjir > 0) {
      rekomendasi <- c(rekomendasi, "🚧 Periksa kondisi saluran air dan siapkan peralatan darurat.")
    }
    if (miu_longsor > 0) {
      rekomendasi <- c(rekomendasi, "⛰️ Hindari daerah curam atau rawan longsor, pastikan jalur evakuasi tersedia.")
    }
    if (miu_angin_kencang > 0) {
      rekomendasi <- c(rekomendasi, "💨 Pastikan benda di luar rumah aman, hindari area terbuka saat angin kencang.")
    }
    if (miu_bencana_ekstrem > 0) {
      rekomendasi <- c(rekomendasi, "🔥 Segera evakuasi ke tempat aman dan ikuti arahan dari pihak berwenang.")
    }
    
    rekomendasi_text <- if (length(rekomendasi) > 0) {
      paste(rekomendasi, collapse = "\n")
    } else {
      "✅ Tidak ada tindakan khusus yang diperlukan."
    }
    
    # 🔹 Output Analisis
    output$hasil_risiko <- renderText({
      paste("Potensi Bencana:", paste(bencana_potensial, collapse = ", "))
    })
    
    output$rekomendasi_risiko <- renderText({
      rekomendasi_text
    })
    
    # 🔹 Output Derajat Keanggotaan
    output$miu_risiko <- renderText({
      paste(
        "🌊 Banjir:", round(miu_banjir, 3), "\n",
        "⛰️ Longsor:", round(miu_longsor, 3), "\n",
        "💨 Angin Kencang:", round(miu_angin_kencang, 3), "\n",
        "🔥 Bencana Ekstrem:", round(miu_bencana_ekstrem, 3)
      )
    })
    
    # 🔹 Grafik Risiko (Semua kategori selalu muncul)
    output$grafik_risiko <- renderPlot({
      data_risiko <- data.frame(
        Risiko = factor(c("Banjir", "Longsor", "Angin Kencang", "Bencana Ekstrem"), 
                        levels = c("Banjir", "Longsor", "Angin Kencang", "Bencana Ekstrem")),
        Miu = c(miu_banjir, miu_longsor, miu_angin_kencang, miu_bencana_ekstrem)
      )
      
      ggplot(data_risiko, aes(x = Risiko, y = Miu, fill = Risiko)) +
        geom_bar(stat = "identity") +
        labs(title = "Tingkat Risiko Bencana (Normalisasi)",
             x = "Kategori Risiko", y = "Derajat Keanggotaan (Miu)") +
        scale_fill_manual(values = c("blue", "brown", "orange", "red")) +
        theme_minimal()
    })
  })
}

# Jalankan Aplikasi
shinyApp(ui = ui, server = server)
