output$analise_movimento <- renderUI({
    if(is.null(valores$modelo)) return(NULL)
    
    if(valores$tipo == "mru") {
      v <- coef(valores$modelo)[2]
      s0 <- coef(valores$modelo)[1]
      
      tagList(
        h5("Movimento Retilíneo Uniforme (MRU)"),
        tags$hr(),
        p(strong("Equação horária:")),
        p(sprintf("S(t) = %.3f + %.3f·t", s0, v), 
          style = "font-size: 18px; color:rgb(127, 186, 245);"),
        tags$hr(),
        p(strong("Características:")),
        tags$ul(
          tags$li("Velocidade constante"),
          tags$li("Aceleração nula"),
          tags$li(sprintf("R² = %.4f", summary(valores$modelo)$r.squared))
        ),
        if(v > 0) {
          p("Movimento progressivo (velocidade positiva)", 
            style = "color: green;")
        } else if(v < 0) {
          p("Movimento retrógrado (velocidade negativa)", 
            style = "color: red;")
        }
      )
    } else {
      s0 <- coef(valores$modelo)[1]
      v0 <- coef(valores$modelo)[2]
      a <- 2 * coef(valores$modelo)[3]
      
      tagList(
        h5("Movimento Retilíneo Uniformemente Variado (MRUV)"),
        tags$hr(),
        p(strong("Equação horária:")),
        p(sprintf("S(t) = %.3f + %.3f·t + %.3f·t²", s0, v0, a/2), 
          style = "font-size: 18px; color: rgb(127, 186, 245);"),
        tags$hr(),
        p(strong("Características:")),
        tags$ul(
          tags$li("Velocidade variável"),
          tags$li("Aceleração constante"),
          tags$li(sprintf("R² = %.4f", summary(valores$modelo)$r.squared))
        ),
        if(a > 0) {
          p("Movimento acelerado", style = "color: green;")
        } else if(a < 0) {
          p("Movimento retardado", style = "color: red;")
        }
      )
    }
  })