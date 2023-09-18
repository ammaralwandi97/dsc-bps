shinyServer(function(input, output,session){
  # -- Below is area for page 1 plots --
  
  ## Output untuk Plot 1
  
  ## mempersiapkan output Id untuk dibaca ke bagian ui, 
  ## di dalamnya terdapat hasil render visualisasi dari renderPlotly()
  output$plot1 <- renderPlotly({
    if(input$input_wilayah != 'Indonesia'){
    if(input$input_analisis == 'Perbandingan Daerah'){
      case1 <- svy_sak2022 %>% 
        # menghubungkan filter dengan input dari checkboxGroupInput()
        filter(TAHUN %in% input$input_year) %>% 
        # menghubungkan filter dengan input dari selectInput()
        filter(NAMA_PROV %in% input$input_wilayah) %>% 
        group_by(NAMA_KAB, Jenis_Kegiatan3) %>% 
        summarise(Jumlah = survey_total()) %>% 
        filter(Jenis_Kegiatan3 != '3. Bukan Usia Kerja') %>% 
        mutate(Persen = round(Jumlah/sum(Jumlah)*100,2)) %>% 
        filter(Jenis_Kegiatan3 == '1. Angkatan Kerja') %>% 
        arrange(-Persen) %>% 
        mutate(text = glue("Wilayah: {NAMA_KAB}
                     Tingkat Partisipasi Angkatan Kerja: {Persen}%
                     Jumlah Angkatan Kerja: {format(Jumlah,big.mark='.')} (jiwa)"))
     
       plot1 <- case1 %>%
      # main layer
      ggplot(mapping = aes(
        x = Persen,
        y = reorder(NAMA_KAB, Persen),
        # menambah parameter text untuk info pada tooltip
        text = text
      )) +
      # ranking plot
      geom_col(mapping = aes(fill = Persen)) +
      # mengubah gradasi warna
      scale_fill_gradient(low = "orange", high = "firebrick") +
      # memberikan informasi mata uang dan mengubah formatnya
      #scale_x_continuous(labels = dollar_format(),
      #                   breaks = seq(0, 20000, 10000)) +
      # menambahkan judul dan label axis
      labs(title = paste0("Tingkat Partisipasi Angkatan Kerja (TPAK)\ndi Provinsi ",
                          input$input_wilayah,' Tahun ', input$input_year),
           y = "Wilayah",
           x = "Tingkat Partisipasi Angkatan Kerja (%)") +
      # ubah tema
      theme_classic() +
      # hilangkan legend
      theme(legend.position = "none") +
         xlim(c(0,100))} else 
        if(input$input_analisis == 'Perbandingan Kelompok Umur'){
        case1 <- svy_sak2022 %>% 
          # menghubungkan filter dengan input dari checkboxGroupInput()
          filter(TAHUN %in% input$input_year) %>% 
          # menghubungkan filter dengan input dari selectInput()
          filter(NAMA_PROV %in% input$input_wilayah) %>% 
          group_by(Jenis_Kegiatan3, kelompok_umur, K4) %>% 
          summarise(Jumlah = survey_total()) %>% 
          filter(Jenis_Kegiatan3 != '3. Bukan Usia Kerja') %>% 
          group_by(kelompok_umur) %>% 
          mutate(Persen = round(Jumlah/sum(Jumlah)*100,2)) %>% 
          filter(Jenis_Kegiatan3 == '1. Angkatan Kerja') %>% 
          mutate(label = glue("Kelompok Umur: {kelompok_umur}
                     Tingkat Partisipasi Angkatan Kerja: {Persen}%
                     Jumlah Angkatan Kerja: {format(Jumlah,big.mark='.')} (jiwa)
                     Jenis Kelamin: {K4}"))
        
        plot1 <- case1 %>%
          # main layer
          ggplot(mapping = aes(
            x = kelompok_umur,
            y = Persen,
            color = K4,
            group = K4,
          )) +
          geom_line()+
          geom_point(aes(text=label)) +
          labs(title = paste0("TPAK Menurut Kelompok Umur\ndi Provinsi ",
                              input$input_wilayah,' Tahun ', input$input_year),
               y = "Tingkat Partisipasi Angkatan Kerja (%)",
               x = "Kelompok Umur",
               color = 'Jenis Kelamin') +
          theme(legend.position = 'bottom') +
          scale_color_manual(values =  c("firebrick","orange")) +
          theme_minimal()
        } else 
          if(input$input_analisis == 'Perbandingan Pendidikan'){
          case1 <- svy_sak2022 %>% 
            filter(TAHUN %in% input$input_year) %>% 
            # menghubungkan filter dengan input dari selectInput()
            filter(NAMA_PROV %in% input$input_wilayah) %>% 
            group_by(PENDIDIKAN) %>% 
            group_by(PENDIDIKAN, Jenis_Kegiatan2) %>% 
            summarise(Jumlah = survey_total()) %>% #filter(Jenis_Kegiatan3 != '3. Bukan Usia Kerja') %>% 
            mutate(Persen = round(Jumlah/sum(Jumlah)*100,2)) %>%
            mutate(text = glue("Pendidikan: {PENDIDIKAN}
                     Persentase: {Persen}%
                     Jenis Kegiatan: {Jenis_Kegiatan2}
                     Jumlah: {format(Jumlah,big.mark='.')} (jiwa)"))
          
          plot1 <- case1 %>% 
            ggplot(mapping = aes(
              x=Persen,
              y=PENDIDIKAN,
              fill=Jenis_Kegiatan2,
              text = text
            )) +
            geom_col() +
            theme_classic() +
            scale_fill_manual(values=c('firebrick','orange','yellow')) +
            labs(title = 
                   paste0("Penduduk >= 15 Tahun Menurut Jenis Kegiatan\ndi Provinsi ",
                          input$input_wilayah,' Tahun ', input$input_year),
                 x = "Persentase (%)",
                 y = "Tingkat Pendidikan",
                 fill = 'Jenis Kegiatan')
        }
    
    
    layout(ggplotly(p = plot1, tooltip = "text"),height=500)
    } else {
      if(input$input_analisis == 'Perbandingan Daerah'){
        case1 <- svy_sak2022 %>% 
          # menghubungkan filter dengan input dari checkboxGroupInput()
          filter(TAHUN %in% input$input_year) %>% 
          # menghubungkan filter dengan input dari selectInput()
          group_by(NAMA_PROV, Jenis_Kegiatan3) %>% 
          summarise(Jumlah = survey_total()) %>% 
          filter(Jenis_Kegiatan3 != '3. Bukan Usia Kerja') %>% 
          mutate(Persen = round(Jumlah/sum(Jumlah)*100,2)) %>% 
          filter(Jenis_Kegiatan3 == '1. Angkatan Kerja') %>% 
          arrange(-Persen) %>% 
          mutate(text = glue("Wilayah: {NAMA_PROV}
                     Tingkat Partisipasi Angkatan Kerja: {Persen}%
                     Jumlah Angkatan Kerja: {format(Jumlah,big.mark='.')} (jiwa)"))
        
        plot1 <- case1 %>%
          # main layer
          ggplot(mapping = aes(
            x = Persen,
            y = reorder(NAMA_PROV, Persen),
            # menambah parameter text untuk info pada tooltip
            text = text
          )) +
          # ranking plot
          geom_col(mapping = aes(fill = Persen)) +
          # mengubah gradasi warna
          scale_fill_gradient(low = "orange", high = "firebrick") +
          # memberikan informasi mata uang dan mengubah formatnya
          #scale_x_continuous(labels = dollar_format(),
          #                   breaks = seq(0, 20000, 10000)) +
          # menambahkan judul dan label axis
          labs(title = paste0("Tingkat Partisipasi Angkatan Kerja (TPAK)\ndi ",
                              input$input_wilayah,' Tahun ', input$input_year),
               y = "Wilayah",
               x = "Tingkat Partisipasi Angkatan Kerja (%)") +
          # ubah tema
          theme_classic() +
          # hilangkan legend
          theme(legend.position = "none") +
          xlim(c(0,100))} else 
            if(input$input_analisis == 'Perbandingan Kelompok Umur'){
              case1 <- svy_sak2022 %>% 
                # menghubungkan filter dengan input dari checkboxGroupInput()
                filter(TAHUN %in% input$input_year) %>% 
                group_by(Jenis_Kegiatan3, kelompok_umur, K4) %>% 
                summarise(Jumlah = survey_total()) %>% 
                filter(Jenis_Kegiatan3 != '3. Bukan Usia Kerja') %>% 
                group_by(kelompok_umur) %>% 
                mutate(Persen = round(Jumlah/sum(Jumlah)*100,2)) %>% 
                filter(Jenis_Kegiatan3 == '1. Angkatan Kerja') %>% 
                mutate(label = glue("Kelompok Umur: {kelompok_umur}
                     Tingkat Partisipasi Angkatan Kerja: {Persen}%
                     Jumlah Angkatan Kerja: {format(Jumlah,big.mark='.')} (jiwa)
                     Jenis Kelamin: {K4}"))
              
              plot1 <- case1 %>%
                # main layer
                ggplot(mapping = aes(
                  x = kelompok_umur,
                  y = Persen,
                  color = K4,
                  group = K4,
                )) +
                geom_line()+
                geom_point(aes(text=label)) +
                labs(title = paste0("TPAK Menurut Kelompok Umur\ndi ",
                                    input$input_wilayah,' Tahun ', input$input_year),
                     y = "Tingkat Partisipasi Angkatan Kerja (%)",
                     x = "Kelompok Umur",
                     color = 'Jenis Kelamin') +
                theme(legend.position = 'bottom') +
                scale_color_manual(values =  c("firebrick","orange")) +
                theme_minimal()
            } else if(input$input_analisis == 'Perbandingan Pendidikan'){
              case1 <- svy_sak2022 %>% 
                filter(TAHUN %in% input$input_year) %>% 
                # menghubungkan filter dengan input dari selectInput()
                group_by(PENDIDIKAN) %>% 
                group_by(PENDIDIKAN, Jenis_Kegiatan2) %>% 
                summarise(Jumlah = survey_total()) %>% #filter(Jenis_Kegiatan3 != '3. Bukan Usia Kerja') %>% 
                mutate(Persen = round(Jumlah/sum(Jumlah)*100,2)) %>%
                mutate(text = glue("Pendidikan: {PENDIDIKAN}
                     Persentase: {Persen}%
                     Jenis Kegiatan: {Jenis_Kegiatan2}
                     Jumlah: {format(Jumlah,big.mark='.')} (jiwa)"))
              
              plot1 <- case1 %>% 
                ggplot(mapping = aes(
                  x=Persen,
                  y=PENDIDIKAN,
                  fill=Jenis_Kegiatan2,
                  text = text
                )) +
                geom_col() +
                theme_classic() +
                scale_fill_manual(values=c('firebrick','orange','yellow')) +
                labs(title = 
                       paste0("Penduduk >= 15 Tahun Menurut Jenis Kegiatan\ndi ",
                              input$input_wilayah,' Tahun ', input$input_year),
                     x = "Persentase (%)",
                     y = "Tingkat Pendidikan",
                     fill = 'Jenis Kegiatan')
            }
      
      
      layout(ggplotly(p = plot1, tooltip = "text"), height = 500)
  }
    })
  ## Interactive Infobox
  
  output$minor_count <- renderUI({
    if(input$input_wilayah != 'Indonesia'){
    infoBox(
      # parameter title memberi judul pada infoBox
      title = "Penduduk Bekerja",
      # parameter value memasukkan nilai pada infoBox
      value = paste0(format(temp %>% 
                              filter(TAHUN %in% input$input_year) %>% 
                              filter(NAMA_PROV %in% input$input_wilayah) %>% 
                              filter(Jenis_Kegiatan == '1. Bekerja') %>% 
                              .$Jumlah,big.mark = '.'),' (jiwa)'),
      # icon mengatur simbol yang ditampilkan pada infoBox
      icon = icon("briefcase"),
      color = "green",
      width = 3
    )
    } 
    else{
      infoBox(
        # parameter title memberi judul pada infoBox
        title = "Penduduk Bekerja",
        # parameter value memasukkan nilai pada infoBox
        value = paste0(format(temp2 %>%  
                                filter(TAHUN %in% input$input_year) %>% 
                                filter(Jenis_Kegiatan == '1. Bekerja') %>% 
                                .$Jumlah,big.mark = '.'),' (jiwa)'),
        # icon mengatur simbol yang ditampilkan pada infoBox
        icon = icon("briefcase"),
        color = "green",
        width = 3
      )
      
  }
    })
  
  output$minor_count2 <- renderUI({
    if(input$input_wilayah != 'Indonesia'){
    infoBox(
      # parameter title memberi judul pada infoBox
      title = "Tidak Bekerja",
      subtitle = 'Pengangguran Terbuka',
      # parameter value memasukkan nilai pada infoBox
      value = paste0(format(temp %>%
                              filter(TAHUN %in% input$input_year) %>% 
                              filter(NAMA_PROV %in% input$input_wilayah) %>%
                              filter(Jenis_Kegiatan == '2. Pengangguran') %>% 
                              .$Jumlah,big.mark = '.'),' (jiwa)'),
      # icon mengatur simbol yang ditampilkan pada infoBox
      icon = icon("face-frown"),
      color = "red",
      width = 3
    )
    } 
    else{
      infoBox(
        # parameter title memberi judul pada infoBox
        title = "Tidak Bekerja",
        subtitle = 'Pengangguran Terbuka',
        # parameter value memasukkan nilai pada infoBox
        value = paste0(format(temp2 %>%
                                filter(TAHUN %in% input$input_year) %>% 
                                filter(Jenis_Kegiatan == '2. Pengangguran') %>% 
                                .$Jumlah,big.mark = '.'),' (jiwa)'),
        # icon mengatur simbol yang ditampilkan pada infoBox
        icon = icon("face-frown"),
        color = "red",
        width = 3
      )
  }
    })
  
  output$minor_count3 <- renderUI({
    if(input$input_wilayah != 'Indonesia'){
    infoBox(
      # parameter title memberi judul pada infoBox
      title = "Angkatan Kerja",
      subtitle = 'Bekerja + Menganggur',
      # parameter value memasukkan nilai pada infoBox
      value = paste0(format(temp %>% 
                              filter(TAHUN %in% input$input_year) %>% 
                              filter(NAMA_PROV %in% input$input_wilayah) %>%
                              filter(Jenis_Kegiatan == '2. Pengangguran') %>% 
                              .$Jumlah +
                              temp %>% 
                              filter(TAHUN %in% input$input_year) %>% 
                              filter(NAMA_PROV %in% input$input_wilayah) %>%
                              filter(Jenis_Kegiatan == '1. Bekerja') %>% 
                              .$Jumlah,big.mark = '.'), ' (jiwa)'),
      # icon mengatur simbol yang ditampilkan pada infoBox
      icon = icon("people-group"),
      color = "yellow",
      width = 3
    )
    } 
    else{
      infoBox(
        # parameter title memberi judul pada infoBox
        title = "Angkatan Kerja",
        subtitle = 'Bekerja + Menganggur',
        # parameter value memasukkan nilai pada infoBox
        value = paste0(format(temp2 %>%
                                filter(TAHUN %in% input$input_year) %>% 
                                filter(Jenis_Kegiatan == '2. Pengangguran') %>% 
                                .$Jumlah +
                                temp2 %>%
                                filter(TAHUN %in% input$input_year) %>% 
                                filter(Jenis_Kegiatan == '1. Bekerja') %>% 
                                .$Jumlah,big.mark = '.'), ' (jiwa)'),
        # icon mengatur simbol yang ditampilkan pada infoBox
        icon = icon("people-group"),
        color = "yellow",
        width = 3
      )
      
  }
    })
  
  output$minor_count4 <- renderUI({
    if(input$input_wilayah !='Indonesia'){
    infoBox(
      # parameter title memberi judul pada infoBox
      title = "Non Angkatan Kerja",
      subtitle = 'Sekolah, Urus Rumah',
      # parameter value memasukkan nilai pada infoBox
      value = paste0(format(temp %>% 
                              filter(TAHUN %in% input$input_year) %>% 
                              filter(TAHUN %in% input$input_year) %>% 
                              filter(NAMA_PROV %in% input$input_wilayah) %>%
                              filter(Jenis_Kegiatan == '4. Sekolah') %>% 
                              .$Jumlah +
                              temp %>% 
                              filter(TAHUN %in% input$input_year) %>% 
                              filter(NAMA_PROV %in% input$input_wilayah) %>%
                              filter(Jenis_Kegiatan == '5. Mengurus Rumah Tangga') %>% 
                              .$Jumlah +
                              temp %>% 
                              filter(TAHUN %in% input$input_year) %>% 
                              filter(NAMA_PROV %in% input$input_wilayah) %>%
                              filter(Jenis_Kegiatan == '6. Lainnya') %>% 
                              .$Jumlah,big.mark = '.'),' (jiwa)'),
      # icon mengatur simbol yang ditampilkan pada infoBox
      icon = icon("chalkboard-user"),
      color = "blue",
      width = 3
    )
    } 
    else {
      infoBox(
        # parameter title memberi judul pada infoBox
        title = "Non Angkatan Kerja",
        subtitle = 'Sekolah, Urus Rumah',
        # parameter value memasukkan nilai pada infoBox
        value = paste0(format(temp2 %>% 
                                filter(TAHUN %in% input$input_year) %>% 
                                filter(Jenis_Kegiatan == '4. Sekolah') %>% 
                                .$Jumlah +
                                temp2 %>% 
                                filter(TAHUN %in% input$input_year) %>% 
                                filter(Jenis_Kegiatan == '5. Mengurus Rumah Tangga') %>% 
                                .$Jumlah +
                                temp2 %>%
                                filter(TAHUN %in% input$input_year) %>% 
                                filter(Jenis_Kegiatan == '6. Lainnya') %>% 
                                .$Jumlah,big.mark = '.'),' (jiwa)'),
        # icon mengatur simbol yang ditampilkan pada infoBox
        icon = icon("chalkboard-user"),
        color = "blue",
        width = 3
      )
  }
    })
  # Below is plots on page 2
  
  # Output untuk plot 3
  dependentOptions <- reactive({
    selectedOption <- input$input_wilayah2
    allOptions[[selectedOption]]
  })
  
  observeEvent(input$input_wilayah2, {
    updateSelectInput(session, inputId = 'input_sub_wilayah',
                      label = 'Pilih Sub-Wilayah', 
                      choices = dependentOptions())
  })
  
  output$minor_bekerja <- renderUI({
    if(input$input_wilayah2 != 'Indonesia'){
      infoBox(
        # parameter title memberi judul pada infoBox
        title = "Penduduk Bekerja",
        # parameter value memasukkan nilai pada infoBox
        value = paste0(format(temp3 %>% 
                                filter(TAHUN %in% input$input_year2) %>% 
                                filter(NAMA_PROV %in% input$input_wilayah2) %>% 
                                filter(NAMA_KAB %in% input$input_sub_wilayah) %>% 
                                group_by(Jenis_Kegiatan) %>% 
                                summarise(Jumlah = sum(Jumlah)) %>% 
                                .$Jumlah,big.mark = '.'),' (jiwa)'),
        # icon mengatur simbol yang ditampilkan pada infoBox
        icon = icon("briefcase"),
        color = "green",
        width = 3
      )
    } 
    else{
      infoBox(
        # parameter title memberi judul pada infoBox
        title = "Penduduk Bekerja",
        # parameter value memasukkan nilai pada infoBox
        value = paste0(format(temp %>% 
                                filter(TAHUN %in% input$input_year2) %>% 
                                filter(NAMA_PROV %in% input$input_sub_wilayah) %>% 
                                filter(Jenis_Kegiatan == '1. Bekerja') %>% 
                                .$Jumlah,big.mark = '.'),' (jiwa)'),
        # icon mengatur simbol yang ditampilkan pada infoBox
        icon = icon("briefcase"),
        color = "green",
        width = 3
      )
      
    }
  })
  
  output$minor_pertanian <- renderUI({
    if(input$input_wilayah2 != 'Indonesia'){
      infoBox(
        # parameter title memberi judul pada infoBox
        title = "Pekerja di Pertanian",
        subtitle = paste0(
          format(temp3 %>% 
                   filter(TAHUN %in% input$input_year2) %>% 
                   filter(NAMA_PROV %in% input$input_wilayah2) %>%
                   filter(NAMA_KAB %in% input$input_sub_wilayah) %>% 
                   mutate(Persen = round(Jumlah/sum(Jumlah)*100,2)) %>%
                   filter(KBLI3 == 'A. Pertanian') %>% 
                   .$Persen
          ), ' %'
        ),
        # parameter value memasukkan nilai pada infoBox
        value = paste0(format(temp3 %>%
                                filter(TAHUN %in% input$input_year2) %>% 
                                filter(NAMA_PROV %in% input$input_wilayah2) %>%
                                filter(NAMA_KAB %in% input$input_sub_wilayah) %>% 
                                filter(KBLI3 == 'A. Pertanian') %>% 
                                .$Jumlah,big.mark = '.'),' (jiwa)'),
        # icon mengatur simbol yang ditampilkan pada infoBox
        icon = icon("wheat-awn"),
        color = "red",
        width = 3
      )
    } 
    else{
      infoBox(
        # parameter title memberi judul pada infoBox
        title = "Pekerja di Pertanian",
        subtitle = paste0(
          format(temp3 %>% 
                   filter(TAHUN %in% input$input_year2) %>% 
                   filter(NAMA_PROV %in% input$input_sub_wilayah) %>%
                   group_by(KBLI3) %>%
                   summarise(Jumlah = sum(Jumlah)) %>% 
                   mutate(Persen = round(Jumlah/sum(Jumlah)*100,2)) %>%
                   filter(KBLI3 == 'A. Pertanian') %>% 
                   .$Persen
          ), ' %'
        ),
        # parameter value memasukkan nilai pada infoBox
        value = paste0(format(temp3 %>%
                                filter(TAHUN %in% input$input_year2) %>% 
                                filter(NAMA_PROV %in% input$input_sub_wilayah) %>%
                                group_by(KBLI3) %>% 
                                summarise(Jumlah = sum(Jumlah)) %>% 
                                filter(KBLI3 == 'A. Pertanian') %>% 
                                .$Jumlah,big.mark = '.'),' (jiwa)'),      # icon mengatur simbol yang ditampilkan pada infoBox
        icon = icon("wheat-awn"),
        color = "yellow",
        width = 3
      )
    }
  })
  
  output$minor_industri <- renderUI({
    if(input$input_wilayah2 != 'Indonesia'){
      infoBox(
        # parameter title memberi judul pada infoBox
        title = "Pekerja di Industri",
        subtitle = paste0(
          format(temp3 %>% 
                   filter(TAHUN %in% input$input_year2) %>% 
                   filter(NAMA_PROV %in% input$input_wilayah2) %>%
                   filter(NAMA_KAB %in% input$input_sub_wilayah) %>% 
                   mutate(Persen = round(Jumlah/sum(Jumlah)*100,2)) %>%
                   filter(KBLI3 == 'B. Industri') %>% 
                   .$Persen
          ), ' %'
        ),
        
        # parameter value memasukkan nilai pada infoBox
        value = paste0(format(temp3 %>%
                                filter(TAHUN %in% input$input_year2) %>% 
                                filter(NAMA_PROV %in% input$input_wilayah2) %>%
                                filter(NAMA_KAB %in% input$input_sub_wilayah) %>% 
                                filter(KBLI3 == 'B. Industri') %>% 
                                .$Jumlah,big.mark = '.'),' (jiwa)'),
        # icon mengatur simbol yang ditampilkan pada infoBox
        icon = icon("industry"),
        color = "yellow",
        width = 3
      )
    } 
    else{
      infoBox(
        # parameter title memberi judul pada infoBox
        title = "Pekerja di Industri",
        subtitle = paste0(
          format(temp3 %>% 
                   filter(TAHUN %in% input$input_year2) %>% 
                   filter(NAMA_PROV %in% input$input_sub_wilayah) %>%
                   group_by(KBLI3) %>%
                   summarise(Jumlah = sum(Jumlah)) %>% 
                   mutate(Persen = round(Jumlah/sum(Jumlah)*100,2)) %>%
                   filter(KBLI3 == 'B. Industri') %>% 
                   .$Persen
          ), ' %'
        ),
        
        # parameter value memasukkan nilai pada infoBox
        value = paste0(format(temp3 %>%
                                filter(TAHUN %in% input$input_year2) %>% 
                                filter(NAMA_PROV %in% input$input_sub_wilayah) %>%
                                group_by(KBLI3) %>% 
                                summarise(Jumlah = sum(Jumlah)) %>% 
                                filter(KBLI3 == 'B. Industri') %>% 
                                .$Jumlah,big.mark = '.'),' (jiwa)'),      # icon mengatur simbol yang ditampilkan pada infoBox
        icon = icon("industry"),
        color = "red",
        width = 3
      )
    }
  })
  
  output$minor_jasa <- renderUI({
    if(input$input_wilayah2 != 'Indonesia'){
      infoBox(
        # parameter title memberi judul pada infoBox
        title = "Pekerja di Jasa",
        subtitle = paste0(
          format(temp3 %>% 
                   filter(TAHUN %in% input$input_year2) %>% 
                   filter(NAMA_PROV %in% input$input_wilayah2) %>%
                   filter(NAMA_KAB %in% input$input_sub_wilayah) %>% 
                   mutate(Persen = round(Jumlah/sum(Jumlah)*100,2)) %>%
                   filter(KBLI3 == 'C. Jasa') %>% 
                   .$Persen
                 ), ' %'
        ),
        # parameter value memasukkan nilai pada infoBox
        value = paste0(format(temp3 %>%
                                filter(TAHUN %in% input$input_year2) %>% 
                                filter(NAMA_PROV %in% input$input_wilayah2) %>%
                                filter(NAMA_KAB %in% input$input_sub_wilayah) %>% 
                                filter(KBLI3 == 'C. Jasa') %>% 
                                .$Jumlah,big.mark = '.'),' (jiwa)'),
        # icon mengatur simbol yang ditampilkan pada infoBox
        icon = icon("money-bill-transfer"),
        color = "blue",
        width = 3
      )
    } 
    else{
      infoBox(
        # parameter title memberi judul pada infoBox
        title = "Pekerja di Jasa",
        subtitle = paste0(
          format(temp3 %>% 
                   filter(TAHUN %in% input$input_year2) %>% 
                   filter(NAMA_PROV %in% input$input_sub_wilayah) %>%
                   group_by(KBLI3) %>%
                   summarise(Jumlah = sum(Jumlah)) %>% 
                   mutate(Persen = round(Jumlah/sum(Jumlah)*100,2)) %>%
                   filter(KBLI3 == 'C. Jasa') %>% 
                   .$Persen
          ), ' %'
        ),
        # parameter value memasukkan nilai pada infoBox
        value = paste0(format(temp3 %>%
                                filter(TAHUN %in% input$input_year2) %>% 
                                filter(NAMA_PROV %in% input$input_sub_wilayah) %>%
                                group_by(KBLI3) %>% 
                                summarise(Jumlah = sum(Jumlah)) %>% 
                                filter(KBLI3 == 'C. Jasa') %>% 
                                .$Jumlah,big.mark = '.'),' (jiwa)'),      # icon mengatur simbol yang ditampilkan pada infoBox
        icon = icon("money-bill-transfer"),
        color = "blue",
        width = 3
      )
    }
  })
  
  output$plot2 <- renderPlotly({
    if(input$input_wilayah2 != 'Indonesia'){
      plot2 <- ggplot(temp4 %>% 
                        filter(NAMA_PROV %in% input$input_wilayah2) %>% 
                        filter(NAMA_KAB %in% input$input_sub_wilayah) %>% 
                        mutate(Persen = round(Jumlah/sum(Jumlah)*100,2)) %>% 
                        filter(Jenis_Kegiatan == '1. Bekerja') %>% 
                        mutate(text = glue(
                     "EPR: {Persen}%")),
                      aes(x = TAHUN,
                          y = Persen,
                          group = Jenis_Kegiatan,
                          text = text)) +
        geom_point(color='firebrick') +
        geom_line(color='firebrick') +
        labs(
          title = paste0('EPR ',input$input_sub_wilayah,
                         ' Tahun ', min(unique(temp4$TAHUN)),'-',
                         max(unique(temp4$TAHUN))),
          y = 'Persentase (%)',
          x = 'Tahun'
        ) +
        theme_bw() +
        ylim(c(50,80))
        
      layout(ggplotly(p = plot2, tooltip = "text"), height = 500)
      
    } else {
      plot2 <- ggplot(temp4 %>% 
                        filter(NAMA_PROV %in% input$input_sub_wilayah) %>% 
                        group_by(TAHUN, NAMA_PROV, Jenis_Kegiatan) %>% 
                        summarise(Jumlah = sum(Jumlah)) %>% 
                        mutate(Persen = round(Jumlah/sum(Jumlah)*100,2)) %>% 
                        filter(Jenis_Kegiatan == '1. Bekerja') %>% 
                        mutate(text = glue(
                          "EPR: {Persen}%")),
                      aes(x = TAHUN,
                          y = Persen,
                          group = Jenis_Kegiatan,
                          text = text)) +
        geom_point(color='firebrick') +
        geom_line(color='firebrick') +
        labs(
          title = paste0('EPR ',input$input_sub_wilayah,
                         ' Tahun ', min(unique(temp4$TAHUN)),'-',
                         max(unique(temp4$TAHUN))),
          y = 'Persentase (%)',
          x = 'Tahun'
        ) +
        theme_bw() +
        ylim(c(50,80))
      
      layout(ggplotly(p = plot2, tooltip = "text"), height = 500)
      
    }
  })
  
  output$plot2a <- renderPlotly({
    if(input$input_wilayah2 == 'Indonesia'){
    plot2 <- ggplot(temp4 %>% 
                      filter(TAHUN %in% input$input_year2) %>% 
                      filter(NAMA_PROV %in% input$input_sub_wilayah) %>%  
                      group_by(TAHUN, NAMA_KAB, Jenis_Kegiatan) %>%
                      summarise(Jumlah = sum(Jumlah)) %>% 
                      mutate(Persen = round(Jumlah/sum(Jumlah)*100,2)) %>%
                      filter(Jenis_Kegiatan == '1. Bekerja') %>% 
                      mutate(text = glue("EPR: {Persen}%")),
                    aes(x = Persen,
                        y = reorder(NAMA_KAB, Persen),
                        text = text,
                        fill = Persen)) +
      geom_col() +
      scale_fill_gradient(low = "orange", high = "firebrick") +
      theme_bw() +
      theme(legend.position = 'none') +
      labs(
        title = paste0('Employment to Population Ratio (EPR) di ',
                       input$input_sub_wilayah, ' Tahun ',
                       input$input_year2),
        x = 'Persentase (%)',
        y = 'Kabupaten/Kota'
      ) +
      xlim(c(0,100))} else {
        plot2 <- ggplot(temp4 %>% 
                          filter(TAHUN %in% input$input_year2) %>% 
                          filter(NAMA_PROV %in% input$input_wilayah2) %>%  
                          group_by(TAHUN, NAMA_KAB, Jenis_Kegiatan) %>%
                          summarise(Jumlah = sum(Jumlah)) %>% 
                          mutate(Persen = round(Jumlah/sum(Jumlah)*100,2)) %>%
                          filter(Jenis_Kegiatan == '1. Bekerja') %>% 
                          mutate(text = glue("EPR: {Persen}%")),
                        aes(x = Persen,
                            y = reorder(NAMA_KAB, Persen),
                            text = text,
                            fill = Persen)) +
          geom_col() +
          scale_fill_gradient(low = "orange", high = "firebrick") +
          theme_bw() +
          theme(legend.position = 'none') +
          labs(
            title = paste0('Employment to Population Ratio (EPR) di ',
                           input$input_wilayah2, ' Tahun ',
                           input$input_year2),
            x = 'Persentase (%)',
            y = 'Kabupaten/Kota'
          ) 
      }
    
    if (input$input_wilayah2 == 'Indonesia') {
      layout(ggplotly(plot2, tooltip = "text"), width = 800)
    } else {
      layout(ggplotly(plot2, tooltip = "text"), width = 800)  
      # Return an empty plotly object when the condition is not met
    }
  })
  
  observe({
    # Use shinyjs to show or hide the conditionalBox based on the input condition
    if (input$input_wilayah2 == 'Indonesia') {
      shinyjs::show("conditionalBox")
    } else {
      shinyjs::hide("conditionalBox")
    }
  })

  
  output$minor_formal <- renderUI({
    if(input$input_wilayah3 != 'Indonesia'){
      infoBox(
        # parameter title memberi judul pada infoBox
        title = "Pekerja Formal",
    #    subtitle = paste0(
     #     format(temp5 %>% 
      #             filter(TAHUN %in% input$input_year2) %>% 
       #            filter(NAMA_PROV %in% input$input_wilayah2) %>%
        #           filter(NAMA_KAB %in% input$input_sub_wilayah) %>% 
         #          mutate(Persen = round(Jumlah/sum(Jumlah)*100,2)) %>%
          #         filter(KBLI3 == 'C. Jasa') %>% 
           #        .$Persen
          #), ' %'
        #),
        # parameter value memasukkan nilai pada infoBox
        value = paste0(format(temp5 %>%
                                filter(TAHUN %in% input$input_year3) %>% 
                                filter(NAMA_PROV %in% input$input_wilayah3) %>%
                                filter(NAMA_KAB %in% input$input_sub_wilayah2) %>% 
                                filter(KBLI_Informal == 'Formal') %>% 
                                .$Jumlah,big.mark = '.'),' (jiwa)'),
        # icon mengatur simbol yang ditampilkan pada infoBox
        icon = icon("user-tie"),
        color = "blue",
        width = 3
      )
    } 
    else{
      infoBox(
        # parameter title memberi judul pada infoBox
        title = "Pekerja Formal",
        #subtitle = paste0(
         # format(temp3 %>% 
          #         filter(TAHUN %in% input$input_year) %>% 
           #        filter(NAMA_PROV %in% input$input_sub_wilayah) %>%
            #       group_by(KBLI3) %>%
             #      summarise(Jumlah = sum(Jumlah)) %>% 
              #     mutate(Persen = round(Jumlah/sum(Jumlah)*100,2)) %>%
               #    filter(KBLI3 == 'C. Jasa') %>% 
                #   .$Persen
          #), ' %'
        #),
        # parameter value memasukkan nilai pada infoBox
        value = paste0(format(temp5 %>%
                                filter(TAHUN %in% input$input_year3) %>% 
                                filter(NAMA_PROV %in% input$input_sub_wilayah2) %>%
                                group_by(KBLI_Informal) %>% 
                                summarise(Jumlah = sum(Jumlah)) %>% 
                                filter(KBLI_Informal == 'Formal') %>% 
                                .$Jumlah,big.mark = '.'),' (jiwa)'),  
        # icon mengatur simbol yang ditampilkan pada infoBox
        icon = icon("user-tie"),
        color = "blue",
        width = 3
      )
    }
  })
  output$minor_informal <- renderUI({
    if(input$input_wilayah3 != 'Indonesia'){
      infoBox(
        # parameter title memberi judul pada infoBox
        title = "Pekerja Informal",
        #    subtitle = paste0(
        #     format(temp5 %>% 
        #             filter(TAHUN %in% input$input_year2) %>% 
        #            filter(NAMA_PROV %in% input$input_wilayah2) %>%
        #           filter(NAMA_KAB %in% input$input_sub_wilayah) %>% 
        #          mutate(Persen = round(Jumlah/sum(Jumlah)*100,2)) %>%
        #         filter(KBLI3 == 'C. Jasa') %>% 
        #        .$Persen
        #), ' %'
        #),
        # parameter value memasukkan nilai pada infoBox
        value = paste0(format(temp5 %>%
                                filter(TAHUN %in% input$input_year3) %>% 
                                filter(NAMA_PROV %in% input$input_wilayah3) %>%
                                filter(NAMA_KAB %in% input$input_sub_wilayah2) %>% 
                                filter(KBLI_Informal == 'Informal') %>% 
                                .$Jumlah,big.mark = '.'),' (jiwa)'),
        # icon mengatur simbol yang ditampilkan pada infoBox
        icon = icon("person-digging"),
        color = "yellow",
        width = 3
      )
    } 
    else{
      infoBox(
        # parameter title memberi judul pada infoBox
        title = "Pekerja Informal",
        #subtitle = paste0(
        # format(temp3 %>% 
        #         filter(TAHUN %in% input$input_year) %>% 
        #        filter(NAMA_PROV %in% input$input_sub_wilayah) %>%
        #       group_by(KBLI3) %>%
        #      summarise(Jumlah = sum(Jumlah)) %>% 
        #     mutate(Persen = round(Jumlah/sum(Jumlah)*100,2)) %>%
        #    filter(KBLI3 == 'C. Jasa') %>% 
        #   .$Persen
        #), ' %'
        #),
        # parameter value memasukkan nilai pada infoBox
        value = paste0(format(temp5 %>%
                                filter(TAHUN %in% input$input_year3) %>% 
                                filter(NAMA_PROV %in% input$input_sub_wilayah2) %>%
                                group_by(KBLI_Informal) %>% 
                                summarise(Jumlah = sum(Jumlah)) %>% 
                                filter(KBLI_Informal == 'Informal') %>% 
                                .$Jumlah,big.mark = '.'),' (jiwa)'),      # icon mengatur simbol yang ditampilkan pada infoBox
        icon = icon("person-digging"),
        color = "yellow",
        width = 3
        )}
    })
  
 output$minor_upah_formal <- renderUI({
   if(input$input_wilayah3 != 'Indonesia'){
     infoBox(
       # parameter title memberi judul pada infoBox
       title = "Upah Formal",
       subtitle = 'Median Upah',
       # parameter value memasukkan nilai pada infoBox
       value = paste0('Rp. ',format(temp5 %>%
                               filter(TAHUN %in% input$input_year3) %>% 
                               filter(NAMA_PROV %in% input$input_wilayah3) %>%
                               filter(NAMA_KAB %in% input$input_sub_wilayah2) %>% 
                               filter(KBLI_Informal == 'Formal') %>% 
                               .$Upah,big.mark = '.')),
       # icon mengatur simbol yang ditampilkan pada infoBox
       icon = icon("money-bills"),
       color = "blue",
       width = 3
     )
   } 
   else{
     infoBox(
       # parameter title memberi judul pada infoBox
       title = "Upah Formal",
       subtitle = 'Median Upah',
       # parameter value memasukkan nilai pada infoBox
       value = paste0('Rp. ',format(temp5 %>%
                               filter(TAHUN %in% input$input_year3) %>% 
                               filter(NAMA_PROV %in% input$input_sub_wilayah2) %>%
                               group_by(KBLI_Informal) %>% 
                               summarise(Upah = median(Upah,na.rm=T)) %>% 
                               filter(KBLI_Informal == 'Formal') %>% 
                               .$Upah,big.mark = '.')),  
       # icon mengatur simbol yang ditampilkan pada infoBox
       icon = icon("money-bills"),
       color = "blue",
       width = 3
     )
   }
 })
 
 output$minor_upah_informal <- renderUI({
   if(input$input_wilayah3 != 'Indonesia'){
     infoBox(
       # parameter title memberi judul pada infoBox
       title = "Upah Informal",
       subtitle = 'Median Upah',
       # parameter value memasukkan nilai pada infoBox
       value = paste0('Rp. ',format(temp5 %>%
                               filter(TAHUN %in% input$input_year3) %>% 
                               filter(NAMA_PROV %in% input$input_wilayah3) %>%
                               filter(NAMA_KAB %in% input$input_sub_wilayah2) %>% 
                               filter(KBLI_Informal == 'Informal') %>% 
                               .$Upah,big.mark = '.')),
       # icon mengatur simbol yang ditampilkan pada infoBox
       icon = icon("money-bills"),
       color = "yellow",
       width = 3
     )
   } 
   else{
     infoBox(
       # parameter title memberi judul pada infoBox
       title = "Upah Informal",
       subtitle = 'Median Upah',
       # parameter value memasukkan nilai pada infoBox
       value = paste0('Rp. ',format(temp5 %>%
                                      filter(TAHUN %in% input$input_year3) %>% 
                                      filter(NAMA_PROV %in% input$input_sub_wilayah2) %>%
                                      group_by(NAMA_PROV, KBLI_Informal) %>% 
                                      summarise(Upah = mean(Upah)) %>% 
                                      filter(KBLI_Informal == 'Informal') %>% 
                                      .$Upah,big.mark = '.')),  
       # icon mengatur simbol yang ditampilkan pada infoBox
       icon = icon("money-bills"),
       color = "yellow",
       width = 3
     )
   }
 })
  
  dependentOptions2 <- reactive({
    selectedOption <- input$input_wilayah3
    allOptions[[selectedOption]]
  })
  observeEvent(input$input_wilayah3, {
    updateSelectInput(session, inputId = 'input_sub_wilayah2',
                      label = 'Pilih Sub-Wilayah', 
                      choices = dependentOptions2())
  })

  output$plot3 <- renderPlot({
    if(input$input_wilayah3 != 'Indonesia'){
    plot3 <- 
      ggplot(data=svy_sak2022$variables %>% 
               select(TAHUN, NAMA_PROV, NAMA_KAB, KBLI_Informal,
                      R15A_UANG, R15A_BRG) %>% 
               filter(KBLI_Informal != 'NULL') %>%
               mutate(Upah = R15A_UANG + R15A_BRG) %>% 
               filter(TAHUN %in% input$input_year3) %>% 
               filter(NAMA_PROV %in% input$input_wilayah3) %>% 
               filter(NAMA_KAB %in% input$input_sub_wilayah2),
             aes(y=NAMA_KAB, 
                 x=Upah,
                 fill = KBLI_Informal)) +
      geom_boxplot() +
      theme_bw() +
      scale_fill_manual(values=c('blue','orange')) +
      scale_x_continuous(breaks = c(1:10)*1000000)+
      xlim(c(0,10000000)) +
      theme(legend.position = 'bottom',
            text = element_text(size=16)) +
      labs(y='Kabupaten/Kota',
           fill = 'Status Pekerjaan')
   (plot3) } else {
     plot3 <- ggplot(data=svy_sak2022$variables %>% 
              select(TAHUN, NAMA_PROV, NAMA_KAB, KBLI_Informal,
                     R15A_UANG, R15A_BRG) %>% 
              filter(KBLI_Informal != 'NULL') %>%
              mutate(Upah = R15A_UANG + R15A_BRG) %>% 
              filter(TAHUN %in% input$input_year3) %>% 
              filter(NAMA_PROV %in% input$input_sub_wilayah2),
            aes(y=NAMA_PROV, 
                x=Upah,
                fill = KBLI_Informal)) +
       geom_boxplot() +
       theme_bw() +
       scale_fill_manual(values=c('blue','orange')) +
       scale_x_continuous(breaks = c(1:10)*1000000)+
       xlim(c(0,10000000)) +
       theme(legend.position = 'bottom',
             text = element_text(size=16)) +
       labs(y='Provinsi',
            fill = 'Status Pekerjaan')
     (plot3)
   }
  })
  
  
#  output$minor_lapus_max <- renderUI({})
 # output$minor_upah_max <- renderUI({})
  #output$minor_lapus_min <- renderUI({})
  #output$minor_upah_min <- renderUI({})
  
  output$plot4 <- renderPlotly({
    if(input$input_wilayah4 != 'Indonesia'){
    if(input$input_analisis2 == 'Jumlah Pekerja'){
      plot5 <- ggplot(temp6 %>% 
                      filter(TAHUN %in% input$input_year4) %>% 
                      filter(NAMA_PROV %in% input$input_wilayah4) %>% 
                     mutate(text = glue("Lapangan Usaha: {Lapangan.Usaha}
                     Jumlah Pekerja: {format(Jumlah,big.mark='.')} (jiwa)")),
                      aes(x=Jumlah,
                          y=reorder(Lapangan.Usaha,Jumlah),
                          fill=Jumlah,
                          text=text)) +
        geom_col() +
        theme_classic() +
        scale_fill_gradient(low='orange', high='firebrick') +
        theme(legend.position = 'none') +
        labs(x='Jumlah Pekerja',
             y='Lapangan Usaha')
      layout(ggplotly(p = plot5, tooltip = "text"),height=500)
    } else {
      plot5 <- ggplot(temp6 %>% 
                        filter(TAHUN %in% input$input_year4) %>% 
                        filter(NAMA_PROV %in% input$input_wilayah4) %>% 
                        mutate(text = glue("Lapangan Usaha: {Lapangan.Usaha}
                     Median Upah: Rp. {format(Upah,big.mark='.')}")),
                     aes(x=Upah,
                         y=reorder(Lapangan.Usaha,Upah),
                         fill=Upah,
                         text=text)) +
        geom_col() +
        theme_classic() +
        scale_fill_gradient(low='orange', high='firebrick') +
        theme(legend.position = 'none') +
        labs(x='Upah Pekerja',
             y='Lapangan Usaha')
      layout(ggplotly(p = plot5, tooltip = "text"),height=500)
    }} else {
      if(input$input_analisis2 == 'Jumlah Pekerja'){
        plot5 <- ggplot(temp6 %>% 
                          filter(TAHUN %in% input$input_year4) %>%
                          group_by(TAHUN, Lapangan.Usaha) %>% 
                          summarise(Jumlah = sum(Jumlah)) %>% 
                          mutate(text = glue("Lapangan Usaha: {Lapangan.Usaha}
                     Jumlah Pekerja: {format(Jumlah,big.mark='.')} (jiwa)")),
                     aes(x=Jumlah,
                         y=reorder(Lapangan.Usaha,Jumlah),
                         fill=Jumlah,
                         text=text)) +
          geom_col() +
          theme_classic() +
          scale_fill_gradient(low='orange', high='firebrick') +
          theme(legend.position = 'none') +
          labs(x='Jumlah Pekerja',
               y='Lapangan Usaha')
        layout(ggplotly(p = plot5, tooltip = "text"),height=500)
      } else {
        plot5 <- ggplot(temp6 %>% 
                          filter(TAHUN %in% input$input_year5) %>% 
                          group_by(TAHUN, Lapangan.Usaha) %>% 
                          summarise(Upah = median(Upah,na.rm=T)) %>% 
                          mutate(text = glue("Lapangan Usaha: {Lapangan.Usaha}
                     Median Upah: Rp. {format(Upah,big.mark='.')}")),
                     aes(x=Upah,
                         y=reorder(Lapangan.Usaha,Upah),
                         fill=Upah,
                         text=text)) +
          geom_col() +
          theme_classic() +
          scale_fill_gradient(low='orange', high='firebrick') +
          theme(legend.position = 'none') +
          labs(x='Upah Pekerja',
               y='Lapangan Usaha')
        layout(ggplotly(p = plot5, tooltip = "text"),height=500)
      }
    }
  })
  
  output$plot5 <- renderPlotly({
    if(input$input_wilayah5 != 'Indonesia'){
      if(input$input_analisis3 == 'Jumlah Pekerja'){
        plot5 <- ggplot(temp7 %>% 
                          filter(TAHUN %in% input$input_year5) %>% 
                          filter(NAMA_PROV %in% input$input_wilayah5) %>% 
                          mutate(text = glue("Jabatan: {Jabatan}
                     Jumlah Pekerja: {format(Jumlah,big.mark='.')} (jiwa)")),
                     aes(x=Jumlah,
                         y=reorder(Jabatan,Jumlah),
                         fill=Jumlah,
                         text=text)) +
          geom_col() +
          theme_classic() +
          scale_fill_gradient(low='orange', high='firebrick') +
          theme(legend.position = 'none') +
          labs(x='Jumlah Pekerja',
               y='Jabatan')
        layout(ggplotly(p = plot5, tooltip = "text"),height=500)
      } 
      else {
        plot5 <- ggplot(temp7 %>% 
                          filter(TAHUN %in% input$input_year5) %>% 
                          filter(NAMA_PROV %in% input$input_wilayah5) %>% 
                          mutate(text = glue("Jabatan: {Jabatan}
                     Median Upah: Rp. {format(Upah,big.mark='.')}")),
                     aes(x=Upah,
                         y=reorder(Jabatan,Upah),
                         fill=Upah,
                         text=text)) +
          geom_col() +
          theme_classic() +
          scale_fill_gradient(low='orange', high='firebrick') +
          theme(legend.position = 'none') +
          labs(x='Upah Pekerja',
               y='Jabatan')
        layout(ggplotly(p = plot5, tooltip = "text"),height=500)
      }
      } else {
        if(input$input_analisis3 == 'Jumlah Pekerja'){
          plot5 <- ggplot(temp7 %>% 
                            filter(TAHUN %in% input$input_year5) %>%
                            group_by(TAHUN, Jabatan) %>% 
                            summarise(Jumlah = sum(Jumlah)) %>% 
                            mutate(text = glue("Jabatan: {Jabatan}
                     Jumlah Pekerja: {format(Jumlah,big.mark='.')} (jiwa)")),
                     aes(x=Jumlah,
                         y=reorder(Jabatan,Jumlah),
                         fill=Jumlah,
                         text=text)) +
            geom_col() +
            theme_classic() +
            scale_fill_gradient(low='orange', high='firebrick') +
            theme(legend.position = 'none') +
            labs(x='Jumlah Pekerja',
                 y='Jabatan')
          layout(ggplotly(p = plot5, tooltip = "text"),height=500)
        } 
        else {
          plot5 <- ggplot(temp7 %>% 
                            filter(TAHUN %in% input$input_year5) %>% 
                            group_by(TAHUN, Jabatan) %>% 
                            summarise(Upah = median(Upah,na.rm=T)) %>% 
                            mutate(text = glue("Jabatan: {Jabatan}
                     Median Upah: Rp. {format(Upah,big.mark='.')}")),
                     aes(x=Upah,
                         y=reorder(Jabatan,Upah),
                         fill=Upah,
                         text=text)) +
            geom_col() +
            theme_classic() +
            scale_fill_gradient(low='orange', high='firebrick') +
            theme(legend.position = 'none') +
            labs(x='Upah Pekerja',
                 y='Jabatan')
          layout(ggplotly(p = plot5, tooltip = "text"),height=500)
        }
      }
  })
  
  output$plot6 <- renderPlotly({
    plot6 <- ggplot(temp8 %>% filter(TAHUN %in% input$input_year6) %>% 
                      mutate(text = 
                               glue("Provinsi: {NAMA_PROV}
                     Median Upah: Rp. {format(Upah,big.mark='.')}
                     EPR: {round(Persen,2)} %")), 
                    aes(x=Persen, 
                        y=Upah,
                        text = text)) +
      geom_point(aes(color=Jenis.Kelamin)) + 
      geom_smooth(aes(color=Jenis.Kelamin),method='lm',se=F) + 
      facet_wrap(~PENDIDIKAN) + 
    #  scale_color_manual(breaks=c('firebrick','orange')) +
      theme_bw() + 
      labs(x='Employment to Population Ratio',
           y= 'Median Upah',
           color = 'Jenis Kelamin')
    
    layout(ggplotly(p = plot6, tooltip = "text"),height=500)
  })



output$minor_count_TPT <- renderUI({
  if(input$input_wilayah10 != 'Indonesia'){
    infoBox(
      # parameter title memberi judul pada infoBox
      title = "Pengangguran",
      subtitle = 'Pengangguran Terbuka',
      # parameter value memasukkan nilai pada infoBox
      value = paste0(format(temp %>% 
                              filter(TAHUN %in% input$input_year10) %>% 
                              filter(NAMA_PROV %in% input$input_wilayah10) %>% 
                              filter(Jenis_Kegiatan == '2. Pengangguran') %>% 
                              .$Jumlah,big.mark = '.'),' (jiwa)'),
      # icon mengatur simbol yang ditampilkan pada infoBox
      icon = icon("face-frown"),
      color = "red",
      width = 3
    )
  } 
  else{
    infoBox(
      # parameter title memberi judul pada infoBox
      title = "Pengangguran Terbuka",
      subtitle = 'Pengangguran Terbuka',
      # parameter value memasukkan nilai pada infoBox
      value = paste0(format(temp2 %>%  
                              filter(TAHUN %in% input$input_year10) %>% 
                              filter(Jenis_Kegiatan == '2. Pengangguran') %>% 
                              .$Jumlah,big.mark = '.'),' (jiwa)'),
      # icon mengatur simbol yang ditampilkan pada infoBox
      icon = icon("face-frown"),
      color = "red",
      width = 3
    )
    
  }
})

output$minor_count_SMP <- renderUI({
  if(input$input_wilayah10 != 'Indonesia'){
    infoBox(
      # parameter title memberi judul pada infoBox
      title = "SMP Kebawah",
      subtitle = 'Pengangguran Terbuka',
      # parameter value memasukkan nilai pada infoBox
      value = paste0(format(temp10 %>%
                              filter(TAHUN %in% input$input_year10) %>% 
                              filter(NAMA_PROV %in% input$input_wilayah10) %>%
                              filter(Jenis_Kegiatan == '2. Pengangguran') %>% 
                              filter(PENDIDIKAN_TPT == '<= SMP') %>% 
                              .$Jumlah,big.mark = '.'),' (jiwa)'),
      # icon mengatur simbol yang ditampilkan pada infoBox
      icon = icon("graduation-cap"),
      color = "blue",
      width = 3
    )
  } 
  else{
    infoBox(
      # parameter title memberi judul pada infoBox
      title = "SMP Kebawah",
      subtitle = 'Pengangguran Terbuka',
      # parameter value memasukkan nilai pada infoBox
      value = paste0(format(temp210 %>%
                              filter(TAHUN %in% input$input_year10) %>% 
                              filter(Jenis_Kegiatan == '2. Pengangguran') %>%
                              filter(PENDIDIKAN_TPT == '<= SMP') %>% 
                              .$Jumlah,big.mark = '.'),' (jiwa)'),
      # icon mengatur simbol yang ditampilkan pada infoBox
      icon = icon("graduation-cap"),
      color = "blue",
      width = 3
    )
  }
})

output$minor_count_SMA <- renderUI({
  if(input$input_wilayah10 != 'Indonesia'){
    infoBox(
      # parameter title memberi judul pada infoBox
      title = "SMA/SMK",
      subtitle = 'Pengangguran Terbuka',
      # parameter value memasukkan nilai pada infoBox
      value = paste0(format(temp10 %>% 
                              filter(TAHUN %in% input$input_year10) %>% 
                              filter(NAMA_PROV %in% input$input_wilayah10) %>%
                              filter(Jenis_Kegiatan == '2. Pengangguran') %>% 
                              filter(PENDIDIKAN_TPT == 'SMA') %>% 
                              .$Jumlah ,big.mark = '.'), ' (jiwa)'),
      # icon mengatur simbol yang ditampilkan pada infoBox
      icon = icon("graduation-cap"),
      color = "light-blue",
      width = 3
    )
  } 
  else{
    infoBox(
      # parameter title memberi judul pada infoBox
      title = "SMA/SMK",
      subtitle = 'Pengangguran Terbuka',
      # parameter value memasukkan nilai pada infoBox
      value = paste0(format(temp210 %>%
                              filter(TAHUN %in% input$input_year10) %>% 
                              filter(Jenis_Kegiatan == '2. Pengangguran') %>%
                              filter(PENDIDIKAN_TPT == 'SMA') %>% 
                              .$Jumlah, big.mark = '.'), ' (jiwa)'),
      # icon mengatur simbol yang ditampilkan pada infoBox
      icon = icon("graduation-cap"),
      color = "light-blue",
      width = 3
    )
    
  }
})

output$minor_count_PT <- renderUI({
  if(input$input_wilayah10 !='Indonesia'){
    infoBox(
      # parameter title memberi judul pada infoBox
      title = "Perguruan Tinggi",
      subtitle = 'Pengangguran Terbuka',
      # parameter value memasukkan nilai pada infoBox
      value = paste0(format(temp10 %>%
                              filter(TAHUN %in% input$input_year10) %>% 
                              filter(NAMA_PROV %in% input$input_wilayah10) %>%
                              filter(Jenis_Kegiatan == '2. Pengangguran') %>%
                              filter(PENDIDIKAN_TPT == 'PT') %>% 
                              .$Jumlah ,big.mark = '.'),' (jiwa)'),
      # icon mengatur simbol yang ditampilkan pada infoBox
      icon = icon("graduation-cap"),
      color = "black",
      width = 3
    )
  } 
  else {
    infoBox(
      # parameter title memberi judul pada infoBox
      title = "Perguruan Tinggi",
      subtitle = 'Pengangguran Terbuka',
      # parameter value memasukkan nilai pada infoBox
      value = paste0(format(temp210 %>% 
                              filter(TAHUN %in% input$input_year10) %>% 
                              filter(Jenis_Kegiatan == '2. Pengangguran') %>%
                              filter(PENDIDIKAN_TPT == 'PT') %>% 
                              .$Jumlah,big.mark = '.'),' (jiwa)'),
      # icon mengatur simbol yang ditampilkan pada infoBox
      icon = icon("graduation-cap"),
      color = "black",
      width = 3
    )
  }
})

output$plot10 <- renderPlotly({
  if(input$input_wilayah10 != 'Indonesia'){
      case10 <- svy_sak2022 %>% 
        # menghubungkan filter dengan input dari checkboxGroupInput()
        filter(TAHUN %in% input$input_year10) %>% 
        # menghubungkan filter dengan input dari selectInput()
        filter(NAMA_PROV %in% input$input_wilayah10) %>% 
        group_by(NAMA_KAB, Jenis_Kegiatan2) %>% 
        summarise(Jumlah = survey_total()) %>% 
        filter(Jenis_Kegiatan2 != '3. Bukan Angkatan Kerja') %>% 
        mutate(Persen = round(Jumlah/sum(Jumlah)*100,2)) %>% 
        filter(Jenis_Kegiatan2 == '2. Pengangguran') %>% 
        arrange(-Persen) %>% 
        mutate(text = glue("Wilayah: {NAMA_KAB}
                     Tingkat Pengangguran Terbuka: {Persen}%
                     Jumlah Pengangguran Terbuka: {format(Jumlah,big.mark='.')} (jiwa)"))
      
      plot10 <- case10 %>%
        # main layer
        ggplot(mapping = aes(
          x = Persen,
          y = reorder(NAMA_KAB, Persen),
          # menambah parameter text untuk info pada tooltip
          text = text
        )) +
        # ranking plot
        geom_col(mapping = aes(fill = Persen)) +
        # mengubah gradasi warna
        scale_fill_gradient(low = "orange", high = "firebrick") +
        # memberikan informasi mata uang dan mengubah formatnya
        #scale_x_continuous(labels = dollar_format(),
        #                   breaks = seq(0, 20000, 10000)) +
        # menambahkan judul dan label axis
        labs(title = paste0("Tingkat Pengangguran Terbuka (TPT)\ndi Provinsi ",
                            input$input_wilayah10,' Tahun ', input$input_year10),
             y = "Wilayah",
             x = "Tingkat Pengangguran Terbuka (%)") +
        # ubah tema
        theme_classic() +
        # hilangkan legend
        theme(legend.position = "none") +
        xlim(c(0,10))
    
    layout(ggplotly(p = plot10, tooltip = "text"),height=500)
  } else {
      case10 <- svy_sak2022 %>% 
        # menghubungkan filter dengan input dari checkboxGroupInput()
        filter(TAHUN %in% input$input_year10) %>% 
        # menghubungkan filter dengan input dari selectInput()
        group_by(NAMA_PROV, Jenis_Kegiatan2) %>% 
        summarise(Jumlah = survey_total()) %>% 
        filter(Jenis_Kegiatan2 != '3. Bukan Angkatan Kerja') %>% 
        mutate(Persen = round(Jumlah/sum(Jumlah)*100,2)) %>% 
        filter(Jenis_Kegiatan2 == '2. Pengangguran') %>% 
        arrange(-Persen) %>% 
        mutate(text = glue("Wilayah: {NAMA_PROV}
                     Tingkat Pengangguran Terbuka: {Persen}%
                     Jumlah Pengangguran Terbuka: {format(Jumlah,big.mark='.')} (jiwa)"))
      
      plot10 <- case10 %>%
        # main layer
        ggplot(mapping = aes(
          x = Persen,
          y = reorder(NAMA_PROV, Persen),
          # menambah parameter text untuk info pada tooltip
          text = text
        )) +
        # ranking plot
        geom_col(mapping = aes(fill = Persen)) +
        # mengubah gradasi warna
        scale_fill_gradient(low = "orange", high = "firebrick") +
        # memberikan informasi mata uang dan mengubah formatnya
        #scale_x_continuous(labels = dollar_format(),
        #                   breaks = seq(0, 20000, 10000)) +
        # menambahkan judul dan label axis
        labs(title = paste0("Tingkat Pengangguran Terbuka \ndi ",
                            input$input_wilayah10,' Tahun ', input$input_year10),
             y = "Wilayah",
             x = "Tingkat Pengangguran Terbuka (%)") +
        # ubah tema
        theme_classic() +
        # hilangkan legend
        theme(legend.position = "none") +
        xlim(c(0,10))
          
    
    
    layout(ggplotly(p = plot10, tooltip = "text"), height = 500)
  }
})

output$the_story <- renderText(
  if(input$input_analisis == 'Perbandingan Daerah'){
    'Semakin besar TPAK artinya semakin banyak penduduk yang bersedia untuk bekerja.
     TPAK yang tinggi merupakan potensi untuk memproduksi lebih banyak barang dan jasa.'
  } else if(input$input_analisis =='Perbandingan Kelompok Umur'){
    'Perempuan Cenderung lebih rendah dalam memasuki pasar tenaga kerja dibandingkan Laki-laki. 
    Perlu kebijakan untuk lebih melibatkan Perempuan untuk memasuki pasar tenaga kerja.'} 
  else {'Penduduk lulusan DIV/S1 cenderung lebih banyak masuk kedalam pasar tenaga kerja.'}
  )

output$the_story10 <- renderText(
  'Jumlah pengangguran terdidik masih tergolong besar. 
   Diperlukan program Link and Match antara dunia pendidikan tinggi dengan industri/perusahaan.'
)

output$the_story2 <- renderText(
  'Makin banyak penduduk bekerja menandakan ada geliat ekonomi di daerah tersebut.'
)


output$the_story3 <- renderText(
  'Pekerja formal memiliki tingkat upah lebih baik dibandingkan dengan pekerja informal.'
)

output$the_story4 <- renderText(
  'Pekerja di Pertanian merupakan yang paling banyak jumlahnya, akan tetapi upah yang didapatkan masih terpaut jauh dengan sektor 
  jasa-jasa.'
)

output$the_story5 <- renderText(
  'Tenaga Penjualan, Pekerja Pertanian, dan Pekerja Kasar merupakan jabatan mayoritas yang diduduki oleh pekerja Indonesia. 
  Kestabilan kondisi ekonomi sangat menentukan nasib pekerja-pekerja tersebut.'
)

output$the_story6 <- renderText(
  'Tidak terdapat perbedaan rata-rata upah yang signifikan antara laki-laki dan perempuan yang bekerja.
  Selain itu, perempuan yang berpendidikan SMP atau SMA cenderung tidak banyak terlibat dalam pasar tenaga kerja.
  Sementara perempuan yang berpendidikan tinggi akan cenderung masuk dalam pasar tenaga kerja.'
)

output$home_img <- renderImage({
  
  list(src = "myImage.png",
       width = "100%",
       height = 550)
}, deleteFile = F)

})