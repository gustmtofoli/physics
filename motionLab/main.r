library(shiny)
library(ggplot2)
library(plotly)
library(DT)
library(bslib)
library(thematic)

# Ativar thematic para adaptar gráficos ao tema
thematic_shiny()

# UI
source("ui.r", local=TRUE)

# Server
server <- function(input, output, session) {
  
  # Valores reativos
  valores <- reactiveValues(
    dados = NULL,
    modelo = NULL,
    tipo = NULL,
    dados_csv = NULL,
    analise_realizada = FALSE  # Flag para controlar visualização
  )
  
  # =========================================================================
  # CONTROLE DE VISIBILIDADE E NAVEGAÇÃO
  # =========================================================================
  
  # Output para controlar visibilidade dos resultados
  output$tem_analise <- reactive({
    valores$analise_realizada
  })
  outputOptions(output, "tem_analise", suspendWhenHidden = FALSE)
  
  # Resumo na sidebar da seção Análise
  output$resumo_sidebar <- renderUI({
    if(!valores$analise_realizada || is.null(valores$modelo)) return(NULL)
    
    tagList(
      tags$div(
        class = "card mb-3",
        tags$div(
          class = "card-body",
          tags$p(
            tags$strong("Tipo de Movimento: "),
            tags$span(
              class = "badge bg-primary",
              if(valores$tipo == "mru") "MRU" else "MRUV"
            )
          ),
          tags$p(
            tags$strong("Número de Pontos: "),
            nrow(valores$dados)
          ),
          tags$p(
            tags$strong("R² do Ajuste: "),
            sprintf("%.4f", summary(valores$modelo)$r.squared)
          ),
          
          # Informações específicas do tipo de movimento
          if(valores$tipo == "mru") {
            tags$div(
              tags$p(
                tags$strong("Velocidade: "),
                sprintf("%.3f m/s", coef(valores$modelo)[2])
              )
            )
          } else {
            tags$div(
              tags$p(
                tags$strong("Velocidade Inicial: "),
                sprintf("%.3f m/s", coef(valores$modelo)[2])
              ),
              tags$p(
                tags$strong("Aceleração: "),
                sprintf("%.3f m/s²", 2 * coef(valores$modelo)[3])
              )
            )
          },
          
          # Período analisado
          tags$p(
            tags$strong("Intervalo de Tempo: "),
            sprintf("%.1f s a %.1f s", 
                    min(valores$dados$t), 
                    max(valores$dados$t))
          )
        )
      )
    )
  })
  
  # =========================================================================
  # ENTRADA DE DADOS
  # =========================================================================
  
  # Gerar inputs em formato tabela
  source("input/input_data_manual.r", local=TRUE)
  
  # Processar upload CSV
  source("input/input_data_csv.r", local=TRUE)
  
  # Preview do CSV
  output$preview_csv <- renderPrint({
    if(!is.null(valores$dados_csv)) {
      cat("Primeiras 5 linhas do arquivo:\n")
      print(head(valores$dados_csv, 5))
      cat("\nTotal de pontos:", nrow(valores$dados_csv))
    }
  })
  
  # =========================================================================
  # ANÁLISE E PROCESSAMENTO
  # =========================================================================
  
  # Análise quando botão é clicado
  observeEvent(input$analisar, {
    # Carregar o script de análise
    source("analyzer/data_analysis.r", local=TRUE)

    print(">>> valores$analise_realizada: ")
    print(valores$analise_realizada)
    
    print(">>> valores$modelo")
    print(valores$modelo)

    # Se a análise foi bem-sucedida
    if(!is.null(valores$analise_realizada)) {
      # valores$analise_realizada <- TRUE
      
      # Solução: Usar um reactiveTimer para garantir sincronização
      # Criar um flag temporário
      valores$mudando_secao <- TRUE
      
      # Observer temporário para mudar de seção
      observe({
        if(valores$mudando_secao && valores$analise_realizada) {
          # Verificar se o output já foi atualizado
          isolate({
            # Mudar para seção de Análise
            session$sendCustomMessage(type = "changeSection", message = "analise")
            
            # Resetar flag
            valores$mudando_secao <- FALSE
          })
        }
      })
    }
  })
  
  # =========================================================================
  # VISUALIZAÇÕES E OUTPUTS
  # =========================================================================
  
  # Tabela de dados
  output$tabela_dados <- renderDT({
    if(is.null(valores$dados)) return(NULL)
    
    # Adicionar coluna de resíduos se modelo existir
    df_display <- valores$dados
    if(!is.null(valores$modelo)) {
      df_display$Previsto <- predict(valores$modelo)
      df_display$Residuo <- df_display$S - df_display$Previsto
    }
    
    DT::datatable(df_display,
                  options = list(
                    pageLength = 15,
                    scrollY = "350px",
                    scrollCollapse = TRUE,
                    dom = 'Bfrtip',
                    buttons = c('copy', 'csv')
                  ),
                  extensions = 'Buttons',
                  rownames = FALSE) %>%
      formatRound(columns = names(df_display), digits = 3)
  })
  
  # Gráfico principal
  source("charts/chart_s_t.r", local=TRUE)
  
  # Análise do movimento
  source("analyzer/movement_analysis.r", local=TRUE)
  
  # Parâmetros calculados
  source("analyzer/data_calculated_parameters.r", local=TRUE)
  
  # Gráfico de velocidade
  source("charts/chart_v_t.r", local=TRUE)
  
  # Gráfico de aceleração
  source("charts/chart_a_t.r", local=TRUE)
  
  # Estatísticas do ajuste
  source("statistics/adjustments_statistics.r", local=TRUE)
  
  # =========================================================================
  # DOWNLOADS
  # =========================================================================
  
  # Download do relatório
  source("reports/txt/generate_report.r", local=TRUE)
  
  # Download template CSV
  output$download_template <- downloadHandler(
    filename = "template_dados_movimento.csv",
    content = function(file) {
      # Criar dados de exemplo
      template <- data.frame(
        S = c(0, 2.1, 4.0, 6.1, 7.9, 10.0),
        t = c(0, 1, 2, 3, 4, 5)
      )
      write.csv(template, file, row.names = FALSE)
    }
  )
  
  # =========================================================================
  # HANDLERS DE NAVEGAÇÃO
  # =========================================================================
  
  # Observar mudanças na seção ativa (vinda do JavaScript)
  observeEvent(input$active_section, {
    # Pode ser usado para logging ou outras ações quando mudar de seção
    if(!is.null(input$active_section)) {
      cat("Seção ativa:", input$active_section, "\n")
    }
  })
  
  # Resetar análise quando voltar para entrada de dados
  observeEvent(input$active_section, {
    if(input$active_section == "entrada" && valores$analise_realizada) {
      # Opcional: perguntar se deseja manter os dados atuais
      # ou começar uma nova análise
    }
  })
}

# JavaScript customizado para mudança de seção programática
tags$script(HTML("
  Shiny.addCustomMessageHandler('changeSection', function(section) {
    showSection(section);
  });
"))

# Executar app
shinyApp(ui = ui, server = server)