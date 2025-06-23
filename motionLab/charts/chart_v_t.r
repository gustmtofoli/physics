  output$grafico_velocidade <- renderPlotly({
    if(is.null(valores$modelo)) return(NULL)
    
    t_range <- seq(min(valores$dados$t), max(valores$dados$t), length.out = 100)
    
    # Configurar cores baseado no tema
    is_dark <- !is.null(input$current_theme) && input$current_theme == "dark"
    
    plot_config <- list(
      paper_bgcolor = if(is_dark) '#2d2d2d' else '#ffffff',
      plot_bgcolor = if(is_dark) '#2d2d2d' else '#fafafa',
      font = list(color = if(is_dark) '#ffffff' else '#000000'),
      gridcolor = if(is_dark) '#404040' else '#f0f0f0',
      linecolor = if(is_dark) '#ff6b6b' else '#e74c3c'
    )
    
    if(valores$tipo == "mru") {
      v <- rep(coef(valores$modelo)[2], length(t_range))
      titulo <- "Velocidade × Tempo (MRU)"
    } else {
      v0 <- coef(valores$modelo)[2]
      a <- 2 * coef(valores$modelo)[3]
      v <- v0 + a * t_range
      titulo <- "Velocidade × Tempo (MRUV)"
    }
    
    plot_ly(x = t_range, y = v,
            type = 'scatter',
            mode = 'lines',
            line = list(color = plot_config$linecolor, width = 3)) %>%
      layout(
        xaxis = list(
          title = "Tempo t (s)", 
          gridcolor = plot_config$gridcolor,
          zerolinecolor = plot_config$gridcolor
        ),
        yaxis = list(
          title = "Velocidade v (m/s)", 
          gridcolor = plot_config$gridcolor,
          zerolinecolor = plot_config$gridcolor
        ),
        title = list(
          text = titulo,
          font = list(color = plot_config$font$color)
        ),
        paper_bgcolor = plot_config$paper_bgcolor,
        plot_bgcolor = plot_config$plot_bgcolor,
        font = plot_config$font
      )
  })