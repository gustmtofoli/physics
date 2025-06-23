  output$grafico_principal <- renderPlotly({
    if(is.null(valores$dados)) return(NULL)
    
    # Criar predições
    t_pred <- seq(min(valores$dados$t), max(valores$dados$t), length.out = 100)
    S_pred <- predict(valores$modelo, newdata = data.frame(t = t_pred))
    
    # Equação
    coef <- coef(valores$modelo)
    if(valores$tipo == "mru") {
      eq <- sprintf("S = %.3f + %.3f·t", coef[1], coef[2])
    } else {
      eq <- sprintf("S = %.3f + %.3f·t + %.3f·t²", 
                    coef[1], coef[2], coef[3])
    }
    
    # Calcular erro para barras de erro
    residuos <- valores$dados$S - predict(valores$modelo)
    erro_padrao <- sd(residuos)
    
    # Configurar cores baseado no tema
    is_dark <- !is.null(input$current_theme) && input$current_theme == "dark"
    
    plot_config <- list(
      paper_bgcolor = if(is_dark) '#2d2d2d' else '#ffffff',
      plot_bgcolor = if(is_dark) '#2d2d2d' else '#fafafa',
      font = list(color = if(is_dark) '#ffffff' else '#000000'),
      gridcolor = if(is_dark) '#404040' else '#f0f0f0'
    )
    
    # Criar gráfico
    p <- plot_ly() %>%
      add_trace(data = valores$dados,
                x = ~t, y = ~S,
                type = 'scatter',
                mode = 'markers',
                name = 'Dados experimentais',
                marker = list(
                  size = 8,
                  color = if(is_dark) '#4fc3f7' else '#1976d2'
                ),
                error_y = if(erro_padrao > 1e-10) {
                  list(
                    type = 'constant',
                    value = erro_padrao,
                    visible = TRUE,
                    color = if(is_dark) '#4fc3f7' else '#1976d2'
                  )
                } else {
                  list(visible = FALSE)
                }) %>%
      add_trace(x = t_pred, y = S_pred,
                type = 'scatter',
                mode = 'lines',
                name = 'Ajuste',
                line = list(
                  width = 3,
                  color = if(is_dark) '#ff6b6b' else '#d32f2f'
                )) %>%
      layout(
        xaxis = list(
          title = "Tempo t (s)", 
          gridcolor = plot_config$gridcolor,
          zerolinecolor = plot_config$gridcolor
        ),
        yaxis = list(
          title = "Posição S (m)", 
          gridcolor = plot_config$gridcolor,
          zerolinecolor = plot_config$gridcolor
        ),
        title = list(
          text = paste("Gráfico S × t -", eq),
          font = list(color = plot_config$font$color)
        ),
        paper_bgcolor = plot_config$paper_bgcolor,
        plot_bgcolor = plot_config$plot_bgcolor,
        font = plot_config$font,
        hovermode = 'closest'
      )
    
    p
  })