output$download_relatorio <- downloadHandler(
    filename = function() {
      paste0("relatorio_movimento_", Sys.Date(), ".txt")
    },
    content = function(file) {
      if(is.null(valores$modelo)) return()
      
      # Criar relatório
      sink(file)
      cat("RELATÓRIO DE ANÁLISE DE MOVIMENTO\n")
      cat("==================================\n\n")
      cat("Data:", format(Sys.Date(), "%d/%m/%Y"), "\n")
      cat("Hora:", format(Sys.time(), "%H:%M:%S"), "\n\n")
      
      cat("DADOS EXPERIMENTAIS:\n")
      cat("-------------------\n")
      cat("Número de pontos:", nrow(valores$dados), "\n\n")
      print(valores$dados)
      cat("\n")
      
      cat("TIPO DE MOVIMENTO:", 
          ifelse(valores$tipo == "mru", "MRU", "MRUV"), "\n\n")
      
      if(valores$tipo == "mru") {
        v <- coef(valores$modelo)[2]
        s0 <- coef(valores$modelo)[1]
        
        cat("EQUAÇÃO HORÁRIA:\n")
        cat(sprintf("S(t) = %.3f + %.3f·t\n\n", s0, v))
        
        cat("PARÂMETROS:\n")
        cat(sprintf("- Posição inicial (S₀): %.3f m\n", s0))
        cat(sprintf("- Velocidade (v): %.3f m/s\n", v))
        cat("- Aceleração (a): 0 m/s²\n")
      } else {
        s0 <- coef(valores$modelo)[1]
        v0 <- coef(valores$modelo)[2]
        a <- 2 * coef(valores$modelo)[3]
        
        cat("EQUAÇÃO HORÁRIA:\n")
        cat(sprintf("S(t) = %.3f + %.3f·t + %.3f·t²\n\n", s0, v0, a/2))
        
        cat("PARÂMETROS:\n")
        cat(sprintf("- Posição inicial (S₀): %.3f m\n", s0))
        cat(sprintf("- Velocidade inicial (v₀): %.3f m/s\n", v0))
        cat(sprintf("- Aceleração (a): %.3f m/s²\n", a))
      }
      
      cat("\nQUALIDADE DO AJUSTE:\n")
      cat("-------------------\n")
      cat(sprintf("R² = %.4f\n", summary(valores$modelo)$r.squared))
      cat(sprintf("Erro padrão = %.4f m\n", summary(valores$modelo)$sigma))
      
      cat("\nRESÍDUOS:\n")
      cat("---------\n")
      residuos <- residuals(valores$modelo)
      cat(sprintf("Média: %.6f\n", mean(residuos)))
      cat(sprintf("Desvio padrão: %.4f\n", sd(residuos)))
      cat(sprintf("Máximo absoluto: %.4f\n", max(abs(residuos))))
      
      sink()
    }
  )