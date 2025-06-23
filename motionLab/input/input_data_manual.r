output$inputs_tabela <- renderUI({
    n <- input$n_pontos
    
    tags$table(
      class = "table table-striped",
      tags$thead(
        tags$tr(
          tags$th("Medida"),
          tags$th("S (m)"),
          tags$th("t (s)")
        )
      ),
      tags$tbody(
        lapply(1:n, function(i) {
          tags$tr(
            tags$td(i),
            tags$td(
              numericInput(paste0("s_", i), 
                           label = NULL,
                           value = i * 2,
                           width = "100px")
            ),
            tags$td(
              numericInput(paste0("t_", i), 
                           label = NULL,
                           value = i,
                           width = "100px")
            )
          )
        })
      )
    )
  })