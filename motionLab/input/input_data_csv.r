  observeEvent(input$arquivo_csv, {
    req(input$arquivo_csv)
    
    tryCatch({
      df <- read.csv(input$arquivo_csv$datapath)
      
      # Verificar se tem as colunas necessárias
      if(!all(c("t", "S") %in% names(df))) {
        # Tentar com a primeira e segunda coluna
        if(ncol(df) >= 2) {
          names(df)[1:2] <- c("t", "S")
          df <- df[, c("t", "S")]
        } else {
          showNotification("O arquivo deve conter pelo menos 2 colunas!", 
                           type = "error")
          return()
        }
      }
      
      valores$dados_csv <- df[, c("t", "S")]
      
    }, error = function(e) {
      showNotification("Erro ao ler arquivo CSV!", type = "error")
    })
  })