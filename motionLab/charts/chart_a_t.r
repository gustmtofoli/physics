  output$grafico_aceleracao <- renderPlotly({
    if(is.null(valores$modelo)) return(NULL)
    
    t_range <- seq(min(valores$dados$t), max(valores$dados$t), length.out = 100)
    
    # Configurar cores baseado no tema
    is_dark <- !is.null(input$current_theme) && input$current_theme == "dark"
    
    plot_config <- list(
      paper_bgcolor = if(is_dark) '#2d2d2d' else '#ffffff',
      plot_bgcolor = if(is_dark) '#2d2d2d' else '#fafafa',
      font = list(color = if(is_dark) '#ffffff' else '#000000'),
      gridcolor = if(is_dark) '#404040' else '#f0f0f0',
      linecolor = if(is_dark) '#4fc3f7' else '#3498db'
    )
    
    if(valores$tipo == "mru") {
      a <- rep(0, length(t_range))
      titulo <- "Aceleração × Tempo (MRU)"
    } else {
      a_value <- 2 * coef(valores$modelo)[3]
      a <- rep(a_value, length(t_range))
      titulo <- "Aceleração × Tempo (MRUV)"
    }
    
    plot_ly(x = t_range, y = a,
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
          title = "Aceleração a (m/s²)", 
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