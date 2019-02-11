# rm(list = ls())
library(shiny)
library(argonR)
library(argonDash)
library(magrittr)
library(shinyjs)
library(plotly)
library(colourpicker)

modus <- function(v, na.rm = TRUE) {
  if(na.rm){
    v <- v[complete.cases(v)]
  }
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

def_col <- "#86B7CC"

kata_pengantar <- "Aplikasi ini disusun sebagai sarana mahasiswa untuk memperkuat pemahaman terhadap beberapa materi yang ada di perkuliahan STK211 Metode Statistika.<br/>
                      <br/>
                                   update terakhir: 25 November 2018<br/>
                                   <br/>
                                   Sampai saat ini terdapat 5 (lima) hal yang sudah disiapkan yaitu:<br/>
                                   1. Menghasilkan statistik deskriptif dari peubah numerik<br/>
                                   2. Menghitung peluang dari selang nilai tertentu dari peubah acak normal<br/>
                                   3. Melihat sebaran rata-rata contoh dari berbagai macam populasi<br/>
                                   4. Uji t untuk hipotesis nilai tengah satu populasi berdasarkan data contoh<br/>
                                   5. Selang kepercayaan nilai tengah populasi<br/>
                                   <br/>
                                   Aplikasi ini disusun oleh beberapa dosen di Departemen Statistika - Institut Pertanian Bogor, yang didanai oleh kegiatan Pengembangan Mandat Divisi Statistika Dasar tahun 2018.<br/>
                                   <br/>
                                   Ucapan terimakasih kami sampaikan kepada saudara Aep Hidayatuloh yang memberikan banyak saran dalam pembuatan aplikasi ini. Apresiasi juga disampaikan kepada pembuat aplikasi di www.artofstat.com yang memberikan banyak inspirasi bagi aplikasi ini.<br/>
                                   <br/>
                                   Masukan terhadap aplikasi ini dapat disampaikan melalui email Bagus Sartono dengan alamat bagusco@gmail.com.<br/>
                                   <br/>
                                   Selamat mencoba dan semoga bermanfaat..."

tabText1 <- "Raw denim you probably haven't heard of them jean shorts Austin. 
Nesciunt tofu stumptown aliqua, retro synth master cleanse. Mustache 
cliche tempor, williamsburg carles vegan helvetica. Reprehenderit 
butcher retro keffiyeh dreamcatcher synth. Raw denim you probably 
haven't heard of them jean shorts Austin. Nesciunt tofu stumptown 
aliqua, retro synth master cleanse"

tabText2 <- "Cosby sweater eu banh mi, qui irure terry richardson ex squid. 
Aliquip placeat salvia cillum iphone. Seitan aliquip quis cardigan 
american apparel, butcher voluptate nisi qui."

tabText3 <- "Raw denim you probably haven't heard of them jean shorts Austin. 
Nesciunt tofu stumptown aliqua, retro synth master cleanse. 
Mustache cliche tempor, williamsburg carles vegan helvetica. 
Reprehenderit butcher retro keffiyeh dreamcatcher synth"

  ui = argonDashPage(
    title = "Departemen Statistika - Institut Pertanian Bogor",
    author = "Aep",
    description = "Institut Pertanian Bogor",
    sidebar = argonDashSidebar(
      vertical = TRUE,
      skin = "light",
      background = "white",
      size = "lg",
      side = "left",
      id = "my_sidebar",
      # h1("Departemen Statistika"),
      brand_url = "https://www.ipb.ac.id",
      brand_logo = "images/logoipb.png",
      # argonDashHeader("Menu"),
      argonSidebarMenu(
        argonSidebarItem(
          tabName = "home",
          icon = "tv-2",
          icon_color = "primary",
          "Beranda"
        ),
        argonSidebarItem(
          tabName = "tabs",
          icon = "planet",
          icon_color = "warning",
          "Eksplorasi Peubah Numerik"
        ),
        argonSidebarItem(
          tabName = "alerts",
          icon = "bullet-list-67",
          icon_color = "danger",
          "Menghitung Peluang Normal"
        ),
        argonSidebarItem(
          tabName = "images",
          icon = "circle-08",
          icon_color = "success",
          "Simulasi Penarikan Sebaran Contoh"
        ),
        argonSidebarItem(
          tabName = "badges",
          icon = "ui-04",
          icon_color = "pink",
          "Uji Hipotesis"
        ),
        argonSidebarItem(
          tabName = "progress",
          icon = "pin-3",
          icon_color = "yellow",
          "Selang Kepercayaan"
        ),
        argonSidebarItem(
          tabName = "profile",
          icon = "spaceship",
          icon_color = "info",
          "Tim Penyusun"
        )#,
        # argonSidebarItem(
        #   tabName = "effects",
        #   icon = "atom",
        #   icon_color = "black",
        #   "CSS effects"
        # ),
        # argonSidebarItem(
        #   tabName = "sections",
        #   icon = "credit-card",
        #   icon_color = "grey",
        #   "Sections"
        # )
      ),
      argonSidebarDivider(),
      argonSidebarHeader(title = "Disusun Oleh:"),
      h5(HTML("Departemen Statistika<br/>Institut Pertanian Bogor"))
    ),
    navbar = argonDashNavbar(
    argonRow(
      # argonColumn(width = 12,
      img(src = "images/stk_icon_tr.png", width = "4.5%", height = "4.5%"),
      h1(HTML("Departemen Statistika"), style = "color:white;font-weight:bold;font-size:220%;")
      # h5(HTML("Institut Pertanian Bogor"), style = "color:white;font-weight:bold;font-size:140%;")
      # )
      # orientation = "right"#,
      # argonDropNavTitle(title = "Selamat Datang!"),
      #   argonDropNavItem(
      #     title = "Item 1", 
      #     src = "https://www.google.com", 
      #     icon = "single-02"
      #   ),
      #   argonDropNavItem(
      #     title = "Item 2", 
      #     src = NULL, 
      #     icon = "settings-gear-65"
      #   ),
      #   argonDropNavDivider(),
      #   argonDropNavItem(
      #     title = "Item 3", 
      #     src = "#", 
      #     icon = "calendar-grid-58"
      #   )
      )
    ),
    header = argonDashHeader(
      gradient = TRUE,
      color = "primary",
      separator = TRUE,
      separator_color = "secondary"
      # argonCard(
      #   title = "Argon Card",
      #   src = "http://www.google.com",
      #   hover_lift = TRUE,
      #   shadow = TRUE,
      #   shadow_size = NULL,
      #   hover_shadow = FALSE,
      #   border_level = 0,
      #   icon = "atom",
      #   status = "primary",
      #   background_color = NULL,
      #   gradient = FALSE, 
      #   floating = FALSE,
      #   "This is the content"
      # )
    ),
    body = argonDashBody(
      useShinyjs(),
      tags$head(tags$link(rel = "shortcut icon", href = "images/logoipb.png")),
      argonTabItems(
        argonTabItem(
          tabName = "home",
          argonRow(
            # argonCard(
            #   width = 12,
            #   src = NULL,
            #   icon = "ui-04",
            #   status = "success",
            #   shadow = TRUE,
            #   border_level = 2,
            #   hover_shadow = TRUE,
            #   title = "Kata Pengantar",
            #   argonRow(
            #     argonColumn(width = 12,
            #                 p(HTML(kata_pengantar), style = "font-weight:bold;")
            #                 )
            #   )
            #   )
            wellPanel(
              h1("Kata Pengantar", style = "font-weight:bold;"),
              p(HTML(kata_pengantar), style = "font-weight:bold;font-size:80%;")
            )
            )
        ),
        argonTabItem(
          tabName = "tabs",
          argonRow(
            h1("Eksplorasi Data Peubah Numerik")
            ),
          argonRow(
            argonColumn(width = 3, 
                   textAreaInput("data", h4("Isikan Data")),
                   p("pisahkan nilai data dengan spasi, gunakan '.' untuk tanda desimal", style = "color:#cccfd1;font-size:80%;"),
                   # br(),
                   checkboxGroupInput("out", h4("Output"), 
                                      choices = c("Statistik Deskriptif"="deskriptif", "Histogram"="histogram", "Boxplot"="boxplot"), 
                                      selected = c("deskriptif", "histogram", "boxplot")
                   ),
                   div(id = "opt_hist",
                       argonSidebarDivider(),
                       h4("Opsi Histogram"),
                       # checkboxInput("opt_hist_stat", "Nilai Kepekatan"),
                       sliderInput("bin", "Banyaknya Bin", min = 1, max = 15, value = 6),
                       colourpicker::colourInput("hcol", "Warna Batang", value = def_col)
                       ),
                   div(id = "opt_bp",
                       argonSidebarDivider(),
                       h4("Opsi Boxplot"),
                       colourpicker::colourInput("bxcol", "Warna Batang", value = def_col)
                       ),
                    argonSidebarDivider()
            ),
            argonColumn(width = 9,
                        div(
                          id = "section0",
                          h3("Statistik Deskriptif")
                          ),
                        div(
                          id = "section1",
                          tableOutput("desk")
                          ),
                        br(),
                        div(
                          id = "section2",
                          plotlyOutput("histogram")
                          ),
                        br(),
                        div(
                          id = "section3",
                          plotlyOutput("boxplot")
                          )
                        )
            )
        ),
        argonTabItem(
          tabName = "alerts",
          argonRow(
            h1("Menghitung Peluang Suatu Rentang Nilai Peubah Acak Normal")
            ),
          argonRow(
            argonColumn(width = 3,
                     h4("Parameter sebaran normal"),
                     div(style="display:inline-block;vertical-align:top;width:47%;", numericInput("mu", "mu", value = 0)),
                     div(style="display:inline-block;vertical-align:top;width:47%;", numericInput("sigma", "sigma", value = 1, min = 0.1)),
                     argonSidebarDivider(),
                     h4("Batas atas dan batas bawah peubah acak"),
                     h5("P(bb < X < ba)", style="font-weight:bold;color:black;font-size:200%;"),
                     div(style="display:inline-block;vertical-align:top;width:47%;", numericInput("nilai1", "Batas Bawah", value = -1.96)),
                     # div(style="display:inline-block;vertical-align:top;width:20%;", h2(" < X < ")),
                     div(style="display:inline-block;vertical-align:top;width:47%;", numericInput("nilai2", "Batas Atas", value = 1.96)),
                     argonSidebarDivider()
                     # div(style="display:inline-block;vertical-align:top;width:20%;", h5("Peluang", style="font-weight:bold;")),
                     # div(style="display:inline-block;vertical-align:top;width:1%;", h5(":", style="font-weight:bold;")),
                     # div(style="display:inline-block;vertical-align:top;width:28%;", textInput("peluang", NULL))
              ),
              argonColumn(width = 9,
                          br(),br(),
                     # plotOutput("plotlyPeluang")
                     plotlyOutput("plotlyPeluang")
              )
              
            )
        ),
        argonTabItem(
          tabName = "images",
          argonRow(
            h1("Simulasi Sebaran Rata-Rata Contoh", style = "font-weight:bold;"),
            argonColumn(width = 9,
                   plotlyOutput("distribusi", height = "80%"),
                   div(style = "display:inline-block;vertical-align:top;width:32%;", sliderInput("ukuran", "Ukuran Contoh", min = 1, max = 100, value = 1, step = 1)),
                   div(style = "display:inline-block;vertical-align:top;width:32%;", sliderInput("ulangan", "Ulangan", min = 100, max = 10000, value = 1000, step = 100)),
                   div(style = "display:inline-block;vertical-align:top;width:32%;", sliderInput("simBin", "Banyaknya Bin", min = 1, max = 20, value = 6))
            ),
            argonColumn(width = 3,
                   h4("Masukkan Kondisi Populasi", style = "font-weight:bold;"),
                   selectInput("sebaran", "Sebaran", 
                               choices = c("Normal", "Poisson", "Chi-Square", "Uniform", "Binomial")),
                   argonSidebarDivider(),
                   h4("Parameter"),
                   conditionalPanel(condition = "input.sebaran == 'Normal'",
                                    div(style = "display:inline-block;vertical-align:top;width:47%;", textInput("normMu", "mu", value = 0)),
                                    div(style = "display:inline-block;vertical-align:top;width:47%;", textInput("normSd", "sigma", value = 1))
                   ),
                   conditionalPanel(condition = "input.sebaran == 'Poisson'",
                                    textInput("poisLambda", "Lambda", value = 1)
                   ),
                   conditionalPanel(condition = "input.sebaran == 'Chi-Square'",
                                    textInput("csqDb", "Derajat Bebas", value = 1)
                   ),
                   conditionalPanel(condition = "input.sebaran == 'Uniform'",
                                    div(style="display:inline-block;vertical-align:top;width:45%;", textInput("unifMin", "Minimum", value = 0)),
                                    div(style="display:inline-block;vertical-align:top;width:45%;", textInput("unifMax", "Maksimum", value = 1))
                   ),
                   conditionalPanel(condition = "input.sebaran == 'Binomial'",
                                    div(style="display:inline-block;vertical-align:top;width:45%;", textInput("binomN", "Kejadian (n)", value = 1)),
                                    div(style="display:inline-block;vertical-align:top;width:45%;", textInput("binomP", "Proporsi (p)", value = 0.5))
                   ),
                   colourInput("simCol", "Warna Batang", value = def_col)
                   # selectInput("simCol", "Warna Batang", 
                   #             choices = c("Coral"="coral", "Biru"="blue", "Biru Muda"="lightblue", "Hijau"="green"))
            )
          )
        ),
        argonTabItem(
          tabName = "badges",
          argonRow(
            h1("Uji t untuk Pengujian Hipotesis Nilai Tengah Populasi", style = "font-weight:bold;"),
            argonColumn(width = 5, 
                   h4("Data", style = "font-weight:bold;"),
                   div(style = "width:100%;", textAreaInput("datauji", label = NULL, width = "100%")),
                   p("pisahkan nilai data dengan spasi, gunakan '.' untuk tanda desimal", style = "color:#cccfd1;font-size:80%;"),
                   div(style="display:inline-block;vertical-align:top;width:60%;", h4("Arah Hipotesis Tandingan", style="font-weight:bold;")),
                   div(style="display:inline-block;vertical-align:top;width:30%;", selectInput("alternatif", label = NULL, width = "100%", 
                                                                                               choices = c("<", ">", "!="))),
                   # div(style="display:inline-block;vertical-align:top;width:50%;", h5("Nilai Tengah Hipotesis", style="font-weight:bold;")),
                   # div(style="display:inline-block;vertical-align:top;width:1%;", h5(":", style="font-weight:bold;")),
                   div(style="display:inline-block;vertical-align:top;width:47%;", numericInput("nilai.mu0", "Nilai Tengah Hipotesis", value = 0)),
                   div(style="display:inline-block;vertical-align:top;width:47%;", numericInput("taraf.nyata", "Taraf Nyata", value = 0.95, min = 0, max = 1, step = 0.05)),
            #        ),
            # argonColumn(width = 7,
                   h4("Hasil Uji t", style = "font-weight:bold;"),
                   # div(style="display:inline-block;vertical-align:top;width:50%;", h5("Nilai p pengujian", style="font-weight:bold;")),
                   # div(style="display:inline-block;vertical-align:top;width:1%;", h5(":", style="font-weight:bold;")),
                   div(style="display:inline-block;vertical-align:top;width:47%;", textInput("nilai.p.pengujian", "Nilai p pengujian")),
                   # br(),
                   # div(style="display:inline-block;vertical-align:top;width:50%;", h5("Kesimpulan pengujian", style="font-weight:bold;")),
                   # div(style="display:inline-block;vertical-align:top;width:1%;", h5(":", style="font-weight:bold;")),
                   div(style="display:inline-block;vertical-align:top;width:47%;", textInput("kesimpulan.pengujian", "Kesimpulan pengujian")),
                   argonSidebarDivider()
            ),
            argonColumn(width = 7,
                        plotlyOutput("ujit.ilustrasi")
            )
          )
        ),
        argonTabItem(
          tabName = "progress",
          argonRow(
            h1("Selang Kepercayaan Nilai Tengah Populasi", style="font-weight:bold;")
            ),
          argonRow(
            argonColumn(width = 5, 
                   textAreaInput("dataSK", label = h4("Data", style="font-weight:bold;")),
                   p("pisahkan nilai data dengan spasi, gunakan '.' untuk tanda desimal", style = "color:#cccfd1;font-size:80%;"),
                   h4("Masukkan 2 (dua) Nilai Tingkat Kepercayaan"),
                   sliderInput("alpha1", "Tingkat Kepercayaan #1", min = 0.800, max = 0.995, value = 0.90),
                   sliderInput("alpha2", "Tingkat Kepercayaan #2", min = 0.800, max = 0.995, value = 0.95)
                   
            ),
            argonColumn(width = 7,
                   div(
                     id = "section3",
                     plotlyOutput("gambarSK")
                   )
            )
          )
          ),
        argonTabItem(
          tabName = "profile",
          argonRow(center = TRUE,
            argonUser(
              title = HTML("Departemen Statistika<br/>Institut Pertanian Bogor"),
              subtitle = NULL,
              src = "images/G01-STK-ICON.png"
            )
          ),
          br(), 
          p(HTML("Department of Statistics - Bogor Agricultural University (IPB) could be mentioned as the first institution in Indonesia which provides higher education in statistics. It has graduated qualified human resources which have acknowledged excellently in national level as well as in international levels. Many companies and state agencies agree that the graduates of Department Statistics are reliable and quickly adapted to answer their needs. Good learning process and high quality input of students has supported the students to be the best in their fields, and the graduates are suitable accross applied areas.<br/>
                  <br/>
                 In addition of having higher education in Bachelor Degree, the Department of Statistics organizes master and doctoral levels as well, which are also the frontier in Indonesia. We provide necessary information that guide the public to know the department better through this website.")),
          br(), br(),
          argonRow(center = TRUE,
                   argonColumn(
                     width = 3,
                     argonUser(
                       title = "Dr. Bagus Sartono",
                       subtitle = "Dosen",
                       src = "images/bagusco.png"
                     )
                   ),
                   argonColumn(
                     width = 3,
                     argonUser(
                       title = "Cici Suhaeni, M.Si",
                       subtitle = "Dosen",
                       src = "images/cici.jpg"
                     )
                   )
          )
              ),
        argonTabItem(
          tabName = "effects",
          argonRow(
            argonColumn(
              width = 6, 
              argonImage(
                src = "https://demos.creative-tim.com/argon-design-system/assets/img/ill/ill-2.svg",
                floating = TRUE,
                card_mode = TRUE
              ) %>% argonPersp(side = "left")
            )
          ) %>% argonPadding(orientation = "y", value = 5),
          argonRow(
            argonColumn(
              width = 6, 
              h1("Perspective effect"),
              h6("Disabled on small screens")
            ),
            argonColumn(
              width = 6, 
              argonImage(
                url = "https://www.google.com",
                src = "https://demos.creative-tim.com/argon-design-system/assets/img/theme/promo-1.png",
                floating = TRUE,
                card_mode = TRUE,
                hover_lift = FALSE
              ) %>% argonPersp(side = "right")
            )
          )
        ),
        argonTabItem(
          tabName = "sections",
          argonDashHeader(
            gradient = TRUE,
            color = "warning",
            separator = TRUE,
            separator_color = "info",
            top_padding = 8,
            bottom_padding = 8,
            argonCard(
              src = "https://www.google.com",
              status = "success",
              border_level = 0,
              hover_shadow = TRUE,
              title = "Card with Margins"
            ) %>% argonMargin(orientation = "t", value = -150)
          ),
          argonDashHeader(
            gradient = FALSE,
            color = "info",
            top_padding = 8,
            bottom_padding = 8,
            argonRow(
              argonColumn(
                width = 6,
                h1("Section Text"),
                h3("Some text here"),
                argonCard()
              ),
              argonColumn(
                width = 6, 
                argonCard() %>% argonMargin(orientation = "t", value = -200)
              )
            )
          ),
          argonDashHeader(
            gradient = FALSE,
            color = "secondary",
            top_padding = 8,
            bottom_padding = 8,
            mask = TRUE,
            background_img = "https://demos.creative-tim.com/argon-design-system/assets/img/theme/img-1-1200x1000.jpg",
            opacity = 6,
            argonH1("Header with mask", display = 1) %>% argonTextColor(color = "white"),
            argonLead("This is the content.") %>% argonTextColor(color = "white")
          )
        )
              )
          )
        )
  
  server <- function(input, output, session){
    
    # Menu Eksplorasi Peubah Numerik
    data <- reactive({
      x <- input$data
      if(x=="")
      {
        return(NULL)
      }
      x <- gsub("  ", " ", x)
      x <- strsplit(x, " ")
      x <- unlist(x)
      x <- as.numeric(x)
      x <- x[complete.cases(x)]
      x
    })
    
    output$desk <- renderTable({
      x <- data()
      if(is.null(x))
      {
        return(NULL)
      }
      data.frame(
        minimum = min(x, na.rm = T), 
        Q1 = quantile(x, 0.25, na.rm = T),
        `rata_rata` = mean(x, na.rm = T),
        median = median(x, na.rm = T),
        modus = modus(x, na.rm = T),
        `simpangan_baku` = sd(x, na.rm = T),
        Q3 = quantile(x, 0.75, na.rm = T),
        maksimum = max(x, na.rm = T)
      )
    })
    
    # rv <- reactiveValues(stat_view = NULL)
    output$histogram <- renderPlotly({
      x <- data()
      if(is.null(x) | length(x) == 1)
      {
        return(NULL)
      } else {
        x <- data.frame(x = data())
        
        compute_bins <- function(x, n) {
          list(
            start = min(x),
            end = max(x),
            size = (max(x) - min(x)) / n
          )
        }
        
        bins <- compute_bins(x$x, input$bin)
        # if(input$opt_hist_stat){
          plot_ly(x, 
                  x = ~x, 
                  type = "histogram", 
                  autobinx = FALSE,
                  xbins = bins, 
                  histnorm = "probability", 
                  marker = list(color = input$hcol),
                  hoverinfo = "text", text = "") %>% 
            layout(title = paste0('Histogram\nRata-rata: ', round(mean(x$x), 3)),
                   xaxis = list(title = 'Data'),
                   yaxis = list(title = 'Kepekatan'), 
                   bargap = 0.01,
                   paper_bgcolor = "rgba(0, 0, 0, 0)", 
                   plot_bgcolor = "rgba(0, 0, 0, 0)")
          
        # } else {
        #   plot_ly(x, 
        #           x = ~x, 
        #           type = "histogram", 
        #           nbinsx = input$bin,  
        #           marker = list(color = input$hcol),
        #           hoverinfo = "text", text = "") %>% 
        #     layout(title = paste0('Histogram\nRata-rata: ', round(mean(x$x), 3)),
        #            xaxis = list(title = 'Data'),
        #            yaxis = list(title = 'Jumlah'), 
        #            paper_bgcolor = "rgba(0, 0, 0, 0)", 
        #            plot_bgcolor = "rgba(0, 0, 0, 0)")
        #   
        # }
        # h <- hist(x$x, main = "Histogram", xlab = "", lwd = 3, 
        #      col = input$hcol, breaks = bins, freq = FALSE,
        #      ylim = c(0, 1), plot = FALSE)
        # hchart(h)
        
      }
    })
    
    output$boxplot <- renderPlotly({
      x <- data()
      if(is.null(x) | length(x) == 1)
      {
        return(NULL)
      } else {
        x <- data.frame(x = data())
        
        plot_ly(type = 'box') %>%
          add_boxplot(y = x$x, 
                      marker = list(color = input$bxcol),
                      line = list(color = input$bxcol),
                      name = "Data") %>% 
          layout(title = 'Boxplot',
                 # yaxis = list(title = 'Data'),
                 yaxis = list(title = 'Statistik'), 
                 paper_bgcolor = "rgba(0, 0, 0, 0)", 
                 plot_bgcolor = "rgba(0, 0, 0, 0)") %>% 
          hide_legend()
        # boxplot(x, main = "Boxplot", col = input$bxcol, horizontal = TRUE)
      }
      
    })
    
    observe({
      if(!("boxplot" %in% input$out))
      {
        hideElement(id = "section3")
        hideElement(id = "opt_bp")
      }
      else 
      {
        showElement(id = "section3")
        showElement(id = "opt_bp")
      }
      if(!("histogram" %in% input$out))
      {
        hideElement(id = "section2")
        hideElement(id = "opt_hist")
      }
      else 
      {
        showElement(id = "section2")
        showElement(id = "opt_hist")
      }
      if(!("deskriptif" %in% input$out) | input$data == "")
      {
        hideElement(id = "section1")
        hideElement(id = "section0")
      }
      else 
      {
        showElement(id = "section1")
        showElement(id = "section0")
      }
    })
    
    # Menu Simulasi Sebaran Penarikan Contoh
    output$distribusi <- renderPlotly({
      #ukuran contoh
      n = input$ukuran

      #banyaknya ulangan
      ulangan = input$ulangan

      # x <- matrix(NA, nrow = input$ulangan, ncol= input$ukuran)

      if(input$sebaran == "Normal")
      {
        if(input$normMu == "" | input$normSd == "")
        {
          return(NULL)
        }
        #menentukan parameter sebaran normal
        mu = as.numeric(input$normMu)
        sigma = as.numeric(input$normSd)

        #membangkitkan sampel (berukuran n sebanyak ulangan)
        sampel <- matrix(rnorm(n*ulangan, mu, sigma), nrow = n)

        #menghitung rata-rata setiap sampel
        rata.rata <- apply(sampel, MARGIN = 2, FUN = "mean")

        #menggambar histogram dari nilai rata.rata
        simBins <- seq(min(rata.rata), max(rata.rata), length.out = input$simBin + 1)
        plot_ly(x = ~rata.rata, 
                type = "histogram", 
                nbinsx = input$simBin,  
                marker = list(color = input$simCol),
                histnorm = "probability",
                hoverinfo = "text", text = "") %>% 
          layout(title = "Histogram Sebaran Rata-rata Contoh",
                 xaxis = list(title = 'Data'),
                 yaxis = list(title = 'Peluang'), 
                 bargap = 0.01,
                 paper_bgcolor = "rgba(0, 0, 0, 0)", 
                 plot_bgcolor = "rgba(0, 0, 0, 0)")
        # hist(rata.rata, breaks = simBins, col = input$simCol, main = "Histogram Sebaran Rata-rata Contoh", freq = FALSE, xlab = "")
        # curve(dnorm(x, mean = mu, sd = sigma), add = TRUE)
        #pemeriksaan kenormalan
        # ks.test(rata.rata, "pnorm", mean(rata.rata), sd(rata.rata))

        # sampel[i, ] <- rnorm(input$ukuran, mean = as.numeric(input$normMu), sd = as.numeric(input$normSd))
      } else if(input$sebaran == "Poisson")
      {
        if(input$poisLambda == "")
        {
          return(NULL)
        }
        #menentukan parameter sebaran normal
        lambda = as.numeric(input$poisLambda)

        #membangkitkan sampel (berukuran n sebanyak ulangan)
        sampel <- matrix(rpois(n*ulangan, lambda), nrow = n)

        #menghitung rata-rata setiap sampel
        rata.rata <- apply(sampel, MARGIN = 2, FUN = "mean")

        #menggambar histogram dari nilai rata.rata
        simBins <- seq(min(rata.rata), max(rata.rata), length.out = input$simBin + 1)
        plot_ly(x = ~rata.rata, 
                type = "histogram", 
                nbinsx = input$simBin,  
                marker = list(color = input$simCol),
                histnorm = "probability",
                hoverinfo = "text", text = "") %>% 
          layout(title = "Histogram Sebaran Rata-rata Contoh",
                 xaxis = list(title = 'Data'),
                 yaxis = list(title = 'Peluang'), 
                 bargap = 0.01,
                 paper_bgcolor = "rgba(0, 0, 0, 0)", 
                 plot_bgcolor = "rgba(0, 0, 0, 0)")
        # hist(rata.rata, breaks = simBins, col = input$simCol, main = "Histogram Sebaran Rata-rata Contoh", freq = FALSE, xlab = "")
        # x[i, ] <- rpois(input$ukuran, as.numeric(input$poisLambda))
      } else if(input$sebaran == "Chi-Square")
      {
        if(input$csqDb == "")
        {
          return(NULL)
        }
        #menentukan parameter sebaran normal
        csqdf = as.numeric(input$csqDb)

        #membangkitkan sampel (berukuran n sebanyak ulangan)
        sampel <- matrix(rchisq(n*ulangan, csqdf), nrow = n)

        #menghitung rata-rata setiap sampel
        rata.rata <- apply(sampel, MARGIN = 2, FUN = "mean")

        #menggambar histogram dari nilai rata.rata
        simBins <- seq(min(rata.rata), max(rata.rata), length.out = input$simBin + 1)
        plot_ly(x = ~rata.rata, 
                type = "histogram", 
                nbinsx = input$simBin,  
                marker = list(color = input$simCol),
                histnorm = "probability",
                hoverinfo = "text", text = "") %>% 
          layout(title = "Histogram Sebaran Rata-rata Contoh",
                 xaxis = list(title = 'Data'),
                 yaxis = list(title = 'Peluang'), 
                 bargap = 0.01,
                 paper_bgcolor = "rgba(0, 0, 0, 0)", 
                 plot_bgcolor = "rgba(0, 0, 0, 0)")
        # hist(rata.rata, breaks = simBins, col = input$simCol, main = "Histogram Sebaran Rata-rata Contoh", freq = FALSE, xlab = "")
        # x[i, ] <- rchisq(input$ukuran, as.numeric(input$chisqDb))
      } else if(input$sebaran == "Uniform")
      {
        if(input$unifMin == "" | input$unifMax == "")
        {
          return(NULL)
        }
        #menentukan parameter sebaran normal
        minimum = as.numeric(input$unifMin)
        maksimum = as.numeric(input$unifMax)

        #membangkitkan sampel (berukuran n sebanyak ulangan)
        sampel <- matrix(runif(n*ulangan, minimum, maksimum), nrow = n)

        #menghitung rata-rata setiap sampel
        rata.rata <- apply(sampel, MARGIN = 2, FUN = "mean")

        #menggambar histogram dari nilai rata.rata
        simBins <- seq(min(rata.rata), max(rata.rata), length.out = input$simBin + 1)
        plot_ly(x = ~rata.rata, 
                type = "histogram", 
                nbinsx = input$simBin,  
                marker = list(color = input$simCol),
                histnorm = "probability",
                hoverinfo = "text", text = "") %>% 
          layout(title = "Histogram Sebaran Rata-rata Contoh",
                 xaxis = list(title = 'Data'),
                 yaxis = list(title = 'Peluang'), 
                 bargap = 0.01,
                 paper_bgcolor = "rgba(0, 0, 0, 0)", 
                 plot_bgcolor = "rgba(0, 0, 0, 0)")
        # hist(rata.rata, breaks = simBins, col = input$simCol, main = "Histogram Sebaran Rata-rata Contoh", freq = FALSE, xlab = "")
        # x[i, ] <- runif(input$ukuran, min = as.numeric(input$unifMin), max = as.numeric(input$unifMax))
      } else if(input$sebaran == "Binomial")
      {
        if(input$binomN == "" | input$binomP == "")
        {
          return(NULL)
        }
        #menentukan parameter sebaran normal
        n = as.numeric(input$binomN)
        p = as.numeric(input$binomP)

        #membangkitkan sampel (berukuran n sebanyak ulangan)
        sampel <- matrix(rbinom(n*ulangan, n, p), nrow = n)

        #menghitung rata-rata setiap sampel
        rata.rata <- apply(sampel, MARGIN = 2, FUN = "mean")

        #menggambar histogram dari nilai rata.rata
        simBins <- seq(min(rata.rata), max(rata.rata), length.out = input$simBin + 1)
        plot_ly(x = ~rata.rata, 
                type = "histogram", 
                nbinsx = input$simBin,  
                marker = list(color = input$simCol),
                histnorm = "probability",
                hoverinfo = "text", text = "") %>% 
          layout(title = "Histogram Sebaran Rata-rata Contoh",
                 xaxis = list(title = 'Data'),
                 yaxis = list(title = 'Peluang'), 
                 bargap = 0.01,
                 paper_bgcolor = "rgba(0, 0, 0, 0)", 
                 plot_bgcolor = "rgba(0, 0, 0, 0)")
        # hist(rata.rata, breaks = simBins, col = input$simCol, main = "Histogram Sebaran Rata-rata Contoh", freq = FALSE, xlab = "")
        # x[i, ] <- rbinom(input$ukuran, as.numeric(input$binomN), as.numeric(input$binomP))
      }

    })

    # Menu Menghitung Peluang Normal

    output$plotlyPeluang <- renderPlotly({
      mu = as.numeric(input$mu)
      sigma = as.numeric(input$sigma)
      
      a = as.numeric(input$nilai1)
      b = as.numeric(input$nilai2)
      if(a > b | input$mu == "" | input$sigma == ""
         | input$nilai1 == "" | input$nilai2 == ""
         | input$nilai1 == "-" | input$nilai2 == "-"
         | input$mu == "-" | input$sigma == "-"
         | is.null(input$mu) | is.null(input$sigma))
      {
        return(NULL)
      }
      
      bawah.x = mu - 4.1*sigma
      atas.x = mu + 4.1*sigma
      # posisi = b + 0.7
      
      peluang1 = pnorm(a, mu, sigma)
      peluang2 = pnorm(0, mu, sigma) - peluang1
      peluang3 = pnorm(b, mu, sigma) - pnorm(0, mu, sigma)
      peluang4 = 1 - pnorm(b, mu, sigma)
      
      if(b <= bawah.x) {
        bawah.x <- b
        # posisi <- b * 1.1 
        # peluang = 1 - peluang
      }
      if(a >= atas.x) {
        atas.x <- a
        # posisi <- a * 1.1
        # peluang = 1 - peluang
      }
      
      
      df <- data.frame(poly.x = c(a, seq(a, b, 0.01), b),
                       poly.y = c(0, dnorm(seq(a, b, 0.01), mu, sigma), 0)
                       )
      df_norm <- data.frame(curve.x = c(bawah.x, seq(bawah.x, atas.x, 0.01), atas.x),
                            curve.y = c(0, dnorm(seq(bawah.x, atas.x, 0.01), mu, sigma), 0)
      )
      
      p9 <- ggplot(data = df_norm, aes(x = curve.x, y = curve.y)) +
        geom_area(fill = "lightblue", colour = "skyblue", size = 1.2) +
        geom_polygon(data = df, aes(x = poly.x, y = poly.y), fill = "#71A9CA") +
        geom_vline(xintercept = a, linetype = "dotted", colour = "#F24343") +
        geom_vline(xintercept = b, linetype = "dotted", colour = "#F24343") +
        geom_vline(xintercept = mu, linetype = "dotted", colour = "black") +
        # anotasi lower.tail
        annotate("text",
                 label = paste0(round(peluang1*100, 1), "%"),
                 x = a - 0.5, #posisi,
                 y = max(df_norm$curve.y) * 1.1,
                 size = 6, colour = "lightblue") +
        annotate("text",
                 label = paste0(round(peluang2*100, 1), "%"),
                 x = mu - 0.5, #posisi,
                 y = max(df_norm$curve.y) * 1.1,
                 size = 6, colour = "#71A9CA") +
        # anotasi upper.tail
        annotate("text",
                 label = paste0(round(peluang3*100, 1), "%"),
                 x = mu + 0.5, #posisi,
                 y = max(df_norm$curve.y) * 1.1,
                 size = 6, colour = "#71A9CA") +
        annotate("text",
                 label = paste0(round(peluang4*100, 1), "%"),
                 x = b + 0.5, #posisi,
                 y = max(df_norm$curve.y) * 1.1,
                 size = 6, colour = "lightblue") +
        annotate("text",
                 label = expression("\u03BC"),
                 x = mu,
                 y = max(df_norm$curve.y) * 1.1,
                 size = 3, colour = "black") +
        xlim(bawah.x, atas.x) +
        scale_y_continuous(expand = c(0, 0), limits = c(0, max(df_norm$curve.y) * 1.2)) +
        labs(title = paste0("Sebaran Normal dengan ", expression("\u03BC = "), mu, expression(" dan \u03C3 = "), sigma, 
                            "\nP(", a, " < X < ", b, ") = ", round((peluang2 + peluang3)*100, 1), "%\n\n\n")) +
        # stat_function(fun = funcShaded, args = list(mu, sigma, a, b), geom = "area", fill = "#84CA72", alpha = 0.2) +
        theme_classic() +
        theme(#axis.line.x = element_blank(),
              axis.line.y = element_blank(),
              axis.title = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              panel.background = element_blank(),
              panel.grid.minor = element_blank(),
              panel.grid.major = element_blank(),
              plot.background = element_blank()
              )
      # p9
      ggplotly(p9) %>%
      # plot_ly(xnorm, x = ~x, y = ~y,
      #         type = 'scatter', mode = 'lines', fill = 'tozeroy') %>%
      layout(#title = paste0("Sebaran Normal dengan $mu$ = ", mu, ", $sigma$ = ", sigma),
             paper_bgcolor = "rgba(0, 0, 0, 0)",
             plot_bgcolor = "rgba(0, 0, 0, 0)")

    })


    # Menu Uji Hipotesis
    data.uji <- reactive({
      x <- input$datauji
      if(x == "")
      {
        return(NULL)
      }
      x <- gsub("  ", " ", x)
      x <- strsplit(x, " ")
      x <- unlist(x)
      x <- as.numeric(x)
      x[complete.cases(x)]
    })

    observe({
      mu.0 = as.numeric(input$nilai.mu0)
      x <- data.uji()

      if(input$nilai.mu0 == "" 
         | (is.null(x) || is.na(x))
         | length(x) <= 1
         | is.null(mu.0) | is.na(mu.0)
         | as.numeric(input$taraf.nyata) > 1
         | as.numeric(input$taraf.nyata) < 0)
      {
        return(NULL)
      } else {
        if (input$alternatif == "!=")
        {
          p.value.uji <- t.test(x, mu = mu.0, alternative = "two.sided")$p.value
        } else if (input$alternatif == "<")
        {
          p.value.uji <- t.test(x, mu = mu.0, alternative = "less")$p.value
        } else
        {
          p.value.uji <- t.test(x, mu = mu.0, alternative = "greater")$p.value
        }
        updateTextInput(session, "nilai.p.pengujian", NULL, value = round(p.value.uji, 4))
        
        if (p.value.uji < as.numeric(input$taraf.nyata))  {
          updateTextInput(session, "kesimpulan.pengujian", NULL, value = "Tolak H0")
        } else
        {
          updateTextInput(session, "kesimpulan.pengujian", NULL, value = "Terima H0")
        }
        
      }


    })
    
    # Menu Selang Kepercayaan
    data.SK <- reactive({
      x <- input$dataSK
      if(x=="")
      {
        return(NULL)
      }
      x <- gsub("  ", " ", x)
      x <- strsplit(x, " ")
      x <- unlist(x)
      x <- as.numeric(x)
      x[complete.cases(x)]
    })

    # observe({
    #   x <- data.SK()
    # 
    #   if(is.null(x) | length(x) < 2)
    #   {
    #     return(NULL)
    #   }
    # 
    #   bawahSK1 <- t.test(x, conf.level = input$alpha1)$conf.int[1]
    #   atasSK1 <- t.test(x, conf.level = input$alpha1)$conf.int[2]
    # 
    #   bawahSK2 <- t.test(x, conf.level = input$alpha2)$conf.int[1]
    #   atasSK2 <- t.test(x, conf.level = input$alpha2)$conf.int[2]
    # })

    output$gambarSK <- renderPlotly({
      x <- data.SK()

      if(is.null(x) | length(x) < 2)
      {
        return(NULL)
      }

      x1 <- t.test(x, conf.level = input$alpha1)
      x2 <- t.test(x, conf.level = input$alpha2)
      # boxplot(c(bawahSK2, atasSK2, bawahSK1, atasSK1) ~
      #           c(paste0("SK ", input$alpha2*100,"%"), paste0("SK ", input$alpha2*100,"%"),
      #             paste0("SK ", input$alpha1*100,"%"), paste0("SK ", input$alpha1*100,"%")),
      #         col = c("coral", "cyan"), horizontal = TRUE,
      #         main = "selang kepercayaan dengan dua tingkat kepercayaan berbeda")
      plot_ly(x = x2$conf.int, type = "box", name = paste0("SK ",input$alpha2*100, "%"), 
              hoverinfo = "text", text = "name") %>% 
        add_boxplot(x = x1$conf.int, type = "box", name = paste0("SK ",input$alpha1*100, "%"), 
                    hoverinfo = "text", text = "name") %>% 
        layout(title = "Selang Kepercayaan",
          paper_bgcolor = "rgba(0, 0, 0, 0)",
          plot_bgcolor = "rgba(0, 0, 0, 0)") %>% 
        hide_legend()
      
    })
    
  }
  
shinyApp(ui, server)