observeEvent(input$analisar, {
    
    if(input$metodo_entrada == "manual") {
      n <- input$n_pontos
      
      # Coletar dados
      s_values <- sapply(1:n, function(i) input[[paste0("s_", i)]])
      t_values <- sapply(1:n, function(i) input[[paste0("t_", i)]])
      
      # Verificar se há dados válidos
      if(any(is.na(s_values)) || any(is.na(t_values))) {
        showNotification("Por favor, preencha todos os valores!", type = "error")
        return()
      }
      
      # Criar dataframe
      valores$dados <- data.frame(
        t = t_values,
        S = s_values
      )
    } else {
      # Usar dados do CSV
      if(is.null(valores$dados_csv)) {
        showNotification("Por favor, faça upload de um arquivo CSV!", type = "error")
        return()
      }
      valores$dados <- valores$dados_csv
    }
    
    # Ordenar por tempo
    valores$dados <- valores$dados[order(valores$dados$t), ]
    
    # Remover NAs
    valores$dados <- na.omit(valores$dados)
    
    if(nrow(valores$dados) < 3) {
      showNotification("São necessários pelo menos 3 pontos válidos!", type = "error")
      return()
    }
    
    # Determinar tipo de movimento
    if(input$tipo_analise == "auto") {
      # Ajustar modelos
      modelo_linear <- lm(S ~ t, data = valores$dados)
      modelo_quad <- lm(S ~ t + I(t^2), data = valores$dados)
      
      # Comparar R²
      r2_linear <- summary(modelo_linear)$r.squared
      r2_quad <- summary(modelo_quad)$r.squared
      
      # Teste F para comparar modelos
      anova_test <- anova(modelo_linear, modelo_quad)
      p_value <- anova_test$`Pr(>F)`[2]
      
      # Decidir baseado em R² e significância
      if(r2_quad - r2_linear > 0.02 && !is.na(p_value) && p_value < 0.05) {
        valores$tipo <- "mruv"
        valores$modelo <- modelo_quad
      } else {
        valores$tipo <- "mru"
        valores$modelo <- modelo_linear
      }
    } else {
      valores$tipo <- input$tipo_analise
      if(valores$tipo == "mru") {
        valores$modelo <- lm(S ~ t, data = valores$dados)
      } else {
        valores$modelo <- lm(S ~ t + I(t^2), data = valores$dados)
      }
    }
    
    showNotification("Análise concluída com sucesso!", type = "message")
    valores$analise_realizada <- TRUE
    print("valores$analise_realizada")
    print(valores$analise_realizada)
    print("valores$modelo")
    print(valores$modelo)
  })