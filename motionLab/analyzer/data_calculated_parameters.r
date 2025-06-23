output$parametros <- renderUI({
    if(is.null(valores$modelo)) return(NULL)
    
    if(valores$tipo == "mru") {
      v <- coef(valores$modelo)[2]
      s0 <- coef(valores$modelo)[1]
      
      # Calcular distância percorrida
      t_inicial <- min(valores$dados$t)
      t_final <- max(valores$dados$t)
      delta_t <- t_final - t_inicial
      dist <- v * delta_t
      
      tagList(
        tags$div(class = "parameter-box",
                 icon("location-dot"),
                 strong(" Posição inicial (S₀):"),
                 sprintf(" %.3f m", s0),
                 tags$br(),
                 tags$small(tags$em("Valor de S quando t = 0"))
        ),
        tags$hr(),
        
        tags$div(class = "parameter-box",
                 icon("gauge-high"),
                 strong(" Velocidade (v):"),
                 sprintf(" %.3f m/s", v),
                 tags$br(),
                 tags$small(tags$em("Calculada como a inclinação da reta S × t")),
                 tags$br(),
                 tags$small(tags$code(sprintf("v = ΔS/Δt = %.3f m/s", v)))
        ),
        tags$hr(),
        
        tags$div(class = "parameter-box",
                 icon("forward"),
                 strong(" Aceleração (a):"),
                 " 0 m/s²",
                 tags$br(),
                 tags$small(tags$em("No MRU a velocidade é constante, portanto a = 0"))
        ),
        tags$hr(),
        
        tags$div(class = "parameter-box",
                 icon("route"),
                 strong(" Deslocamento (ΔS):"),
                 sprintf(" %.3f m", dist),
                 tags$br(),
                 tags$small(tags$em("Calculado por:")),
                 tags$br(),
                 tags$small(tags$code(sprintf("ΔS = v × Δt = %.3f × %.3f = %.3f m", 
                                              v, delta_t, dist)))
        ),
        tags$hr(),
        
        tags$div(class = "parameter-box",
                 icon("clock"),
                 strong(" Período analisado (Δt):"),
                 sprintf(" %.1f s", delta_t),
                 tags$br(),
                 tags$small(tags$code(sprintf("Δt = %.1f - %.1f = %.1f s", 
                                              t_final, t_inicial, delta_t)))
        )
      )
    } else {
      s0 <- coef(valores$modelo)[1]
      v0 <- coef(valores$modelo)[2]
      a <- 2 * coef(valores$modelo)[3]
      
      # Tempo para v = 0 (se aplicável)
      t_v0 <- if(a != 0 && sign(v0) != sign(a)) -v0/a else NA
      
      # Velocidade final
      t_final <- max(valores$dados$t)
      v_final <- v0 + a * t_final
      
      # Deslocamento total
      delta_s <- s0 + v0*t_final + 0.5*a*t_final^2 - s0
      
      tagList(
        tags$div(class = "parameter-box",
                 icon("location-dot"),
                 strong(" Posição inicial (S₀):"),
                 sprintf(" %.3f m", s0),
                 tags$br(),
                 tags$small(tags$em("Valor de S quando t = 0")),
                 tags$br(),
                 tags$small(tags$code("Da equação: S = S₀ + v₀t + at²/2"))
        ),
        tags$hr(),
        
        tags$div(class = "parameter-box",
                 icon("gauge"),
                 strong(" Velocidade inicial (v₀):"),
                 sprintf(" %.3f m/s", v0),
                 tags$br(),
                 tags$small(tags$em("Velocidade no instante t = 0")),
                 tags$br(),
                 tags$small(tags$code("Coeficiente linear do termo 't'"))
        ),
        tags$hr(),
        
        tags$div(class = "parameter-box",
                 icon("forward"),
                 strong(" Aceleração (a):"),
                 sprintf(" %.3f m/s²", a),
                 tags$br(),
                 tags$small(tags$em("Taxa de variação da velocidade")),
                 tags$br(),
                 tags$small(tags$code(sprintf("a = 2 × coef(t²) = 2 × %.3f = %.3f m/s²", 
                                              a/2, a)))
        ),
        tags$hr(),
        
        tags$div(class = "parameter-box",
                 icon("gauge-high"),
                 strong(" Velocidade final:"),
                 sprintf(" %.3f m/s", v_final),
                 tags$br(),
                 tags$small(tags$em("Calculada por:")),
                 tags$br(),
                 tags$small(tags$code(sprintf("v = v₀ + at = %.3f + %.3f × %.3f = %.3f m/s", 
                                              v0, a, t_final, v_final)))
        ),
        tags$hr(),
        
        tags$div(class = "parameter-box",
                 icon("route"),
                 strong(" Deslocamento total:"),
                 sprintf(" %.3f m", delta_s),
                 tags$br(),
                 tags$small(tags$em("Variação de posição:")),
                 tags$br(),
                 tags$small(tags$code(sprintf("ΔS = v₀t + at²/2 = %.3f × %.1f + %.3f × %.1f²/2", 
                                              v0, t_final, a, t_final)))
        ),
        
        if(!is.na(t_v0) && t_v0 > 0 && t_v0 < max(valores$dados$t)) {
          tags$div(
            tags$hr(),
            tags$div(class = "parameter-box",
                     icon("stop"),
                     strong(" Tempo para v = 0:"),
                     sprintf(" %.3f s", t_v0),
                     tags$br(),
                     tags$small(tags$em("Momento em que o móvel para")),
                     tags$br(),
                     tags$small(tags$code(sprintf("t = -v₀/a = -(%.3f)/(%.3f) = %.3f s", 
                                                  v0, a, t_v0)))
            )
          )
        }
      )
    }
  })