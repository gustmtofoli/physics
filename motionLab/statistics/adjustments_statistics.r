  output$estatisticas <- renderUI({
    if(is.null(valores$modelo)) return(NULL)
    
    # Verificar se o modelo é confiável
    sumario <- tryCatch({
      summary(valores$modelo)
    }, warning = function(w) {
      # Retornar sumário mesmo com warning
      suppressWarnings(summary(valores$modelo))
    })
    
    residuos <- residuals(valores$modelo)
    n <- length(residuos)
    
    # Cálculos para explicações
    SST <- sum((valores$dados$S - mean(valores$dados$S))^2)
    SSR <- sum((predict(valores$modelo) - mean(valores$dados$S))^2)
    SSE <- sum(residuos^2)
    
    # Graus de liberdade
    if(valores$tipo == "mru") {
      p <- 2  # 2 parâmetros: S0 e v
    } else {
      p <- 3  # 3 parâmetros: S0, v0 e a/2
    }
    df <- n - p
    
    tagList(
      h5("Qualidade do Ajuste"),
      tags$hr(),
      
      # R-squared
      p(strong("Coeficiente de Determinação (R²):"), 
        sprintf(" %.4f", sumario$r.squared)),
      p(em("Indica que ", sprintf("%.1f%%", sumario$r.squared * 100), 
           " da variação em S é explicada pelo modelo")),
      tags$div(
        style = "background-color: #f8f9fa; padding: 10px; margin: 10px 0; border-radius: 5px;",
        tags$small(
          tags$strong("Como é calculado:"), tags$br(),
          tags$code("R² = SSR/SST = 1 - SSE/SST"), tags$br(),
          sprintf("SST (Soma Total dos Quadrados) = %.4f", SST), tags$br(),
          sprintf("SSR (Soma dos Quadrados da Regressão) = %.4f", SSR), tags$br(),
          sprintf("SSE (Soma dos Quadrados dos Erros) = %.4f", SSE), tags$br(),
          tags$code(sprintf("R² = %.4f / %.4f = %.4f", SSR, SST, sumario$r.squared))
        )
      ),
      
      # Aviso se ajuste perfeito
      if(sumario$r.squared > 0.9999) {
        tags$div(
          class = "alert alert-info",
          icon("info-circle"),
          " Ajuste praticamente perfeito - dados podem ser teóricos ou muito precisos"
        )
      },
      
      tags$hr(),
      
      # Erro padrão
      p(strong("Erro padrão residual:"), 
        sprintf(" %.4f m", sumario$sigma)),
      tags$div(
        style = "background-color: #f8f9fa; padding: 10px; margin: 10px 0; border-radius: 5px;",
        tags$small(
          tags$strong("Como é calculado:"), tags$br(),
          tags$code("σ = √(SSE / (n - p))"), tags$br(),
          sprintf("n = %d (número de observações)", n), tags$br(),
          sprintf("p = %d (número de parâmetros)", p), tags$br(),
          sprintf("Graus de liberdade = %d", df), tags$br(),
          tags$code(sprintf("σ = √(%.4f / %d) = %.4f m", SSE, df, sumario$sigma))
        )
      ),
      
      tags$hr(),
      
      # Análise de resíduos
      p(strong("Análise de resíduos:")),
      tags$ul(
        tags$li(sprintf("Média: %.4f", mean(residuos))),
        tags$li(sprintf("Desvio padrão: %.4f", sd(residuos))),
        tags$li(sprintf("Máximo: %.4f", max(abs(residuos))))
      ),
      
      tags$div(
        style = "background-color: #f8f9fa; padding: 10px; margin: 10px 0; border-radius: 5px;",
        tags$small(
          tags$strong("Desvio padrão dos resíduos:"), tags$br(),
          tags$code("s = √(Σ(resíduo - média)² / (n-1))"), tags$br(),
          "Este é o desvio padrão amostral dos resíduos,", tags$br(),
          "diferente do erro padrão residual que usa n-p no denominador."
        )
      ),
      
      tags$hr(),
      
      # Teste de normalidade
      if(length(residuos) > 3 && sd(residuos) > 1e-10) {
        tryCatch({
          shapiro_test <- shapiro.test(residuos)
          tagList(
            p(strong("Teste de normalidade (Shapiro-Wilk):"),
              sprintf(" p-valor = %.4f", shapiro_test$p.value),
              if(shapiro_test$p.value > 0.05) {
                span(" (resíduos normais)", style = "color: green;")
              } else {
                span(" (resíduos não normais)", style = "color: orange;")
              }),
            
            tags$div(
              style = "background-color: #f8f9fa; padding: 10px; margin: 10px 0; border-radius: 5px;",
              tags$small(
                tags$strong("Como funciona o teste:"), tags$br(),
                "O teste de Shapiro-Wilk verifica se os resíduos seguem", tags$br(),
                "uma distribuição normal. A estatística W é calculada como:", tags$br(),
                tags$code("W = (Σaᵢx₍ᵢ₎)² / Σ(xᵢ - x̄)²"), tags$br(),
                "onde x₍ᵢ₎ são os resíduos ordenados e aᵢ são coeficientes tabulados.", tags$br(),
                tags$br(),
                tags$strong("Interpretação:"), tags$br(),
                "H₀: Os resíduos seguem distribuição normal", tags$br(),
                "H₁: Os resíduos não seguem distribuição normal", tags$br(),
                sprintf("Como p-valor = %.4f", shapiro_test$p.value),
                if(shapiro_test$p.value > 0.05) {
                  ", não rejeitamos H₀ (α = 0.05)"
                } else {
                  ", rejeitamos H₀ (α = 0.05)"
                }
              )
            )
          )
        }, error = function(e) {
          p(em("Teste de normalidade não aplicável (resíduos muito pequenos)"))
        })
      } else {
        p(em("Teste de normalidade não aplicável"),
          if(length(residuos) <= 3) {
            " (poucos dados - mínimo 4 observações)"
          } else {
            " (variância dos resíduos muito pequena)"
          })
      }
    )
  })