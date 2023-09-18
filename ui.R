# Fungsi dashboardPage() diperuntuhkan untuk membuat ketiga bagian pada Shiny
dashboardPage(skin = "yellow",
  
  # Fungsi dashboardHeader() adalah bagian untuk membuat header
  dashboardHeader(title = "EGA: Evidence-based Goverment Actions (for Labor Force)",
                  titleWidth = 600),
  
  # Fungsi dashboardSidebar() adalah bagian untuk membuat sidebar
  dashboardSidebar(
    
    # sidebarMenu() untuk membuat dan mengatur menu
    sidebarMenu(
      menuItem('Beranda',
               tabName = 'menu_0',
               icon = icon('home')),
      # menuItem untuk menambahkan sebuah tab menu pada sidebar
      menuItem(
        # text untuk memeberikan tampilan nama pada tab
        text = "Partisipasi Angkatan Kerja",
        # tabName untuk memberikan identitas yang mewakili tab tersebut
        tabName = "menu_1",
        icon = icon("percent")
      ),
      menuItem(
        text = 'Pengangguran',
        tabName = 'menu_10',
        icon = icon("user-plus")
      ),
      menuItem(
        text = 'Persentase Penduduk Bekerja',
        tabName = 'menu_2',
        icon = icon('people-line')
      ),
      menuItem(
        text = 'Pekerja Formal vs. Informal',
        tabName = 'menu_3',
        icon = icon('person')
      ),
      menuItem(
        text = 'Lapangan Pekerjaan Utama',
        tabName = 'menu_4',
        icon = icon('store')
      ),
      menuItem(
        text = 'Jabatan Pekerjaan Utama',
        tabName = 'menu_5',
        icon = icon('user-tie')
      ),
      menuItem(
        text = 'Analisis Dinamis',
        tabName = 'menu_6',
        icon = icon('chart-line')
      )
    )
  ),
  
  # Fungsi dashboardBody() adalah bagian untuk membuat isi body
  dashboardBody(
    
    # tabItems() digunakan untuk mengumpulkan isi body dari setiap tab menu
    tabItems(
      tabItem(
        tabName = 'menu_0',
        fluidPage(
          title='Home',
          imageOutput('home_img')
          )
        ),
      # ------ Halaman pertama
      tabItem(
        # tabName adalah parameter untuk membuat koneksi antara body dengan tab menu berdasarkan identitasnya
        tabName = "menu_1",
        
        # fluidRow() adalah fungsi untuk membuat ruangan dengan layout orientasi baris
        fluidPage(
          h2(tags$b("Tingkat Partisipasi Angkatan Kerja (TPAK)")),
          br(),
          div(style = "text-align:justify", 
              p('Tingkat Partisipasi Angkatan Kerja (TPAK) merupakan indikator utama dari potensi pertumbuhan ekonomi. Semakin tinggi TPAK menunjukkan bahwa semakin
tinggi pula pasokan tenaga kerja yang tersedia untuk memproduksi barang dan jasa dalam suatu perekonomian.',
                "TPAK dapat fluktuatif dikarenakan adanya kebijakan, perubahan sosial budaya, iklim perekonomian, geografis, dan sebagainya (BPS, 2022)"),
              br()
          )
        ),
        # untuk infoBox
        fluidRow(
          
          # infoBox() adalah fungsi untuk membuat kotak berisi nilai
         uiOutput(outputId = 'minor_count'),
          uiOutput(outputId = 'minor_count2'),
          uiOutput(outputId = 'minor_count3'),
          uiOutput(outputId = 'minor_count4')
        ),
        fluidRow(
          box(
            width = 3, height = 530,
            selectInput(
              inputId = 'input_year',
              label = 'Pilih Tahun',
              choice = unique(svy_sak2022$variables$TAHUN),
              selected = 2022
              ),
            selectInput(
              inputId = 'input_wilayah',
              label = 'Pilih Wilayah',
              choice = c('Indonesia',
                         unique(svy_sak2022$variables$NAMA_PROV)),
              selected = 'Indonesia'
            ),
            selectInput(
              inputId = 'input_analisis',
              label = 'Pilih Analisis',
              choice = c('Perbandingan Daerah',
                         'Perbandingan Kelompok Umur',
                         'Perbandingan Pendidikan'),
              selected = 'Perbadingan Daerah'
              ),
            box(width = 20, height = 275,
                title = 'Story of The Labor',
                textOutput('the_story'))
            ),
            
            box(width = 9, height = 530,
                plotlyOutput(outputId = 'plot1')
                )
          )
        )
      ,
tabItem(
 tabName = 'menu_10',
 fluidPage(
   h2(tags$b("Tingkat Pengangguran Terbuka (TPT)")),
   br(),
   div(style = "text-align:justify", 
       p('Penganggur terbuka, terdiri dari: Mereka yang tak punya pekerjaan dan mencari pekerjaan, 
         Mereka yang tak punya pekerjaan dan mempersiapkan usaha,
         Mereka yang tak punya pekerjaan dan tidak mencari pekerjaan, karena merasa tidak mungkin mendapatkan pekerjaan,
         dan Mereka yang sudah punya pekerjaan, tetapi belum molai bekerja.'),
br()
   )
 ),
# untuk infoBox
fluidRow(
  # infoBox() adalah fungsi untuk membuat kotak berisi nilai
  uiOutput(outputId = 'minor_count_TPT'),
  uiOutput(outputId = 'minor_count_SMP'),
  uiOutput(outputId = 'minor_count_SMA'),
  uiOutput(outputId = 'minor_count_PT')
),
fluidRow(
  box(
    width = 3, height = 530,
    selectInput(
      inputId = 'input_year10',
      label = 'Pilih Tahun',
      choice = unique(svy_sak2022$variables$TAHUN),
      selected = 2022
    ),
    selectInput(
      inputId = 'input_wilayah10',
      label = 'Pilih Wilayah',
      choice = c('Indonesia',
                 unique(svy_sak2022$variables$NAMA_PROV)),
      selected = 'Indonesia'
    ),
    box(width = 20, height = 275,
        title = 'Story of The Labor',
        textOutput('the_story10'))
  ),
  
  box(width = 9, height = 530,
      plotlyOutput(outputId = 'plot10')
  )
)
),
      # ------ Halaman Kedua
tabItem(
tabName = "menu_2",

# fluidRow() adalah fungsi untuk membuat ruangan dengan layout orientasi baris
fluidPage(
  h2(tags$b("Persentase Penduduk Bekerja (Employment to Population Ratio)")),
  br(),
  div(style = "text-align:justify", 
      p('Employment to Population Ratio (EPR) menjadi informasi dalam
pengambilan keputusan penciptaan lapangan kerja (BPS, 2022). Semakin tinggi
nilai EPR, semakin banyak penyerapan tenaga kerja dalam pasar kerja. Sedangkan,
nilai EPR yang rendah mengindikasikan banyak tenaga kerja masih belum terserap,
sehingga perlu penciptaan lapangan kerja yang lebih banyak.' ),
br()
          )
        ),

fluidRow(
  
  # infoBox() adalah fungsi untuk membuat kotak berisi nilai
  uiOutput(outputId = 'minor_bekerja'),
  uiOutput(outputId = 'minor_pertanian'),
  uiOutput(outputId = 'minor_industri'),
  uiOutput(outputId = 'minor_jasa')
),
fluidRow(
  # Box for the first condition
  box(width = 3, height = 530+530,
        # Add content for the conditional box
        selectInput(
          inputId = 'input_year2',
          label = 'Pilih Tahun',
          choice = unique(svy_sak2022$variables$TAHUN),
          selected = 2022
        ),
        selectInput(
          inputId = 'input_wilayah2',
          label = 'Pilih Wilayah',
          choice = names(allOptions),
          selected = c('Indonesia')
        ),
        selectInput(
          inputId = 'input_sub_wilayah',
          label = 'Pilih Sub-Wilayah',
          choice = 'JAMBI'
        ),
      box(width = 20, height = 275,
          title = 'Story of The Labor',
          textOutput('the_story2'))
      ),
  # Box for the second condition
  # Add a unique ID for the conditionalPanel
  box(id = "conditionalBox",
        width = 9, height = 530,
      # Add content for the non-conditional box here
      plotlyOutput(outputId = 'plot2a')),
  box(width = 9, height = 530,
          plotlyOutput(outputId = 'plot2')
        )
      )
    ),
# ------ Halaman Ketiga
tabItem(
  tabName = "menu_3",
  
  # fluidRow() adalah fungsi untuk membuat ruangan dengan layout orientasi baris
  fluidPage(
    h2(tags$b("Perbandingan Pekerja Formal dan Informal")),
    br(),
    div(style = "text-align:justify", 
        p('Status pekerjaan penduduk dapat memberikan informasi mengenai
dinamika pasar tenaga kerja dan tingkat pembangunan di suatu negara. Status
pekerjaan dapat dibedakan menjadi dua kategori besar yakni pekerjaan formal: 
buruh/karyawan/pegawai, berusaha dibantu buruh tetap. Kategori berikutnya adalah pekerjaan informal: 
berusaha dibantu buruh tidak tetap, berusaha sendiri, pekerja bebas, 
dan pekerja keluarga.','Tingginya persentase penduduk yang bekerja di
sektor formal dapat memacu pertumbuhan ekonomi (ILO, 2016).'),
br()
    )
  ),
fluidRow(
  uiOutput(outputId = 'minor_formal'),
  uiOutput(outputId = 'minor_upah_formal'),
  uiOutput(outputId = 'minor_informal'),
  uiOutput(outputId = 'minor_upah_informal')
  ),
fluidRow(
  # Box for the first condition
  box(width = 3, height = 530,
      # Add content for the conditional box
      selectInput(
        inputId = 'input_year3',
        label = 'Pilih Tahun',
        choice = unique(svy_sak2022$variables$TAHUN),
        selected = 2022
      ),
      selectInput(
        inputId = 'input_wilayah3',
        label = 'Pilih Wilayah',
        choice = names(allOptions),
        selected = c('Indonesia')
      ),
      selectInput(
        inputId = 'input_sub_wilayah2',
        label = 'Pilih Sub-Wilayah',
        choice = NULL
        ),
      box(width = 20, height = 275,
          title = 'Story of The Labor',
          textOutput('the_story3'))
      ),
  box(width = 9, height = 530,
      plotOutput(outputId = 'plot3'))
    )
  ),

tabItem(
  tabName = "menu_4",
  
  # fluidRow() adalah fungsi untuk membuat ruangan dengan layout orientasi baris
  fluidPage(
    h2(tags$b("Lapangan Pekerjaan Utama")),
    br(),
    div(style = "text-align:justify", 
        p('Struktur lapangan pekerjaan memberikan informasi penting terkait tahap
pembangunan suatu negara. Pada umumnya, seiring dengan semakin
berkembangnya ekonomi suatu negara, sektor pekerjaan yang awalnya didominasi
sektor pertanian beralih ke sektor industri dan pada akhirnya bergerak ke sektor
jasa.'),
br()
    )
  ),
#fluidRow(
#  uiOutput(outputId = 'minor_lapus_max'),
#  uiOutput(outputId = 'minor_upah_max'),
#  uiOutput(outputId = 'minor_lapus_min'),
#  uiOutput(outputId = 'minor_upah_min')
#),
fluidRow(
  box(
    width = 3, height = 530,
    selectInput(
      inputId = 'input_year4',
      label = 'Pilih Tahun',
      choice = unique(svy_sak2022$variables$TAHUN),
      selected = 2022
    ),
    selectInput(
      inputId = 'input_wilayah4',
      label = 'Pilih Wilayah',
      choice = c('Indonesia',
                 unique(svy_sak2022$variables$NAMA_PROV)),
      selected = 'Indonesia'
    ),
    selectInput(
      inputId = 'input_analisis2',
      label = 'Pilih Analisis',
      choice = c('Jumlah Pekerja',
                 'Median Upah'),
      selected = 'Jumlah Pekerja'
    ),
    box(width = 20, height = 275,
        title = 'Story of The Labor',
        textOutput('the_story4'))
  ),
box(width = 9, height = 530,
    plotlyOutput(outputId = 'plot4')
    )
  )
),
tabItem(
  tabName = "menu_5",
  
  # fluidRow() adalah fungsi untuk membuat ruangan dengan layout orientasi baris
  fluidPage(
    h2(tags$b("Jabatan di Pekerjaan Utama")),
    br(),
    div(style = "text-align:justify", 
        p('Statistik jenis pekerjaan dapat digunakan untuk mengidentifikasi perubahan
tingkat keahlian tenaga kerja. Selain itu, proyeksi jenis pekerjaan seringkali
dijadikan sebagai dasar penyusunan kebijakan dalam rangka memenuhi kebutuhan
keahlian di masa mendatang (ILO, 2016).'),
br()
    )
  ),
fluidRow(
  box(
    width = 3, height = 530,
    selectInput(
      inputId = 'input_year5',
      label = 'Pilih Tahun',
      choice = unique(svy_sak2022$variables$TAHUN),
      selected = 2022
    ),
    selectInput(
      inputId = 'input_wilayah5',
      label = 'Pilih Wilayah',
      choice = c('Indonesia',
                 unique(svy_sak2022$variables$NAMA_PROV)),
      selected = 'Indonesia'
    ),
    selectInput(
      inputId = 'input_analisis3',
      label = 'Pilih Analisis',
      choice = c('Jumlah Pekerja',
                 'Median Upah'),
      selected = 'Jumlah Pekerja'
    ),
    box(width = 20, height = 275,
        title = 'Story of The Labor',
        textOutput('the_story5')
  )),
  box(width = 9, height = 530,
      plotlyOutput(outputId = 'plot5')
    )
  )
),

tabItem(
  tabName = "menu_6",
  
  # fluidRow() adalah fungsi untuk membuat ruangan dengan layout orientasi baris
  fluidPage(
    h2(tags$b("Government Action for Wage")),
    br(),
    div(style = "text-align:justify", 
        p('Pilih variabel sesuai dengan kebutuhan'),
        br()
    )
  ),
  fluidRow(
    box(width = 3, height=530,
        selectInput(
          inputId = 'input_year6',
          label = 'Pilih Tahun',
          choice = unique(svy_sak2022$variables$TAHUN),
          selected = c(2022)
        ),
        selectInput(
          inputId = 'input_Y',
          label = 'Pilih Variabel Y',
          choice = 'Upah',
          selected = 'Upah'
        ),
        selectInput(
          inputId = 'input_X',
          label = 'Pilih Kategorisasi',
          choice = c('Jenis Kelamin'),
          selected = 'Jenis Kelamin'
        ),
        box(width = 20, height = 275,
            title = 'Story of The Labor',
            textOutput('the_story6')
        )),
    box(width = 9, height=530,
        plotlyOutput(outputId = 'plot6'))
    )
)
        )
      )
    )
