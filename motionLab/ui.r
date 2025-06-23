ui <- page_sidebar(
  theme = bs_theme(bootswatch = "flatly"),
  # title = "Analisador de Movimento - MRU e MRUV",
  fillable_mobile = FALSE,
  fillable = FALSE,  # Permite scroll da página
  
  # CSS customizado para dark mode
  tags$head(
    tags$style(HTML("
      /* Transições suaves */
      * {
        transition: background-color 0.3s ease, color 0.3s ease, border-color 0.3s ease;
      }
      
      /* Dark mode styles */
      [data-bs-theme='dark'] {
        --bs-body-bg: #1a1a1a;
        --bs-body-color: #ffffff;
        --bs-card-bg: #2d2d2d;
        --bs-border-color: #404040;
      }
      
      [data-bs-theme='dark'] .form-control,
      [data-bs-theme='dark'] .form-select {
        background-color: #404040;
        color: #ffffff;
        border-color: #505050;
      }
      
      [data-bs-theme='dark'] .form-control:focus {
        background-color: #4a4a4a;
        border-color: #0d6efd;
        color: #ffffff;
      }
      
      [data-bs-theme='dark'] .table {
        --bs-table-bg: #2d2d2d;
        --bs-table-color: #ffffff;
        --bs-table-border-color: #404040;
        --bs-table-striped-bg: #363636;
        --bs-table-hover-bg: #404040;
      }
      
      [data-bs-theme='dark'] .btn-primary {
        background-color: #0d6efd;
        border-color: #0d6efd;
      }
      
      [data-bs-theme='dark'] .btn-primary:hover {
        background-color: #0b5ed7;
        border-color: #0a58ca;
      }
      
      [data-bs-theme='dark'] .alert-info {
        background-color: #084298;
        border-color: #084298;
        color: #cfe2ff;
      }
      
      /* Plotly dark mode */
      [data-bs-theme='dark'] .plot-container {
        background-color: #2d2d2d !important;
      }
      
      /* Toggle button styles */
      .theme-toggle {
        position: fixed;
        top: 10px;
        right: 10px;
        z-index: 1000;
        background: transparent;
        border: none;
        font-size: 1.5rem;
        cursor: pointer;
        padding: 0.5rem;
        border-radius: 50%;
        width: 50px;
        height: 50px;
        display: flex;
        align-items: center;
        justify-content: center;
        transition: transform 0.3s ease;
      }
      
      .theme-toggle:hover {
        transform: scale(1.1);
        background-color: rgba(0,0,0,0.1);
      }
      
      [data-bs-theme='dark'] .theme-toggle:hover {
        background-color: rgba(255,255,255,0.1);
      }
      
      /* Botões da DataTable com melhor contraste */
      .dt-buttons .btn {
        background-color: #0d6efd;
        border-color: #0d6efd;
        color: white;
      }
      
      .dt-buttons .btn:hover {
        background-color: #0b5ed7;
        border-color: #0a58ca;
      }
      
      [data-bs-theme='dark'] .dt-buttons .btn {
        background-color: #0d6efd;
        border-color: #0d6efd;
        color: white;
      }
      
      [data-bs-theme='dark'] .dt-buttons .btn:hover {
        background-color: #0b5ed7;
        border-color: #0a58ca;
      }
      
      [data-bs-theme='dark'] .dataTables_filter input,
      [data-bs-theme='dark'] .dataTables_length select {
        background-color: #404040;
        color: #ffffff;
        border-color: #505050;
      }
      
      [data-bs-theme='dark'] .dataTables_paginate .paginate_button {
        color: #ffffff !important;
      }
      
      [data-bs-theme='dark'] .dataTables_info {
        color: #ffffff;
      }
      
      /* Caixas de explicação no dark mode */
      [data-bs-theme='dark'] div[style*=\"background-color: #f8f9fa\"] {
        background-color: #2a2a2a !important;
        color: #e0e0e0 !important;
        border: 1px solid #404040 !important;
      }
      
      /* Forçar cor do texto em elementos small e code dentro das caixas */
      [data-bs-theme='dark'] div[style*=\"background-color: #f8f9fa\"] small,
      [data-bs-theme='dark'] div[style*=\"background-color: #f8f9fa\"] code,
      [data-bs-theme='dark'] div[style*=\"background-color: #f8f9fa\"] strong {
        color: #e0e0e0 !important;
      }
      
      /* Estilos para o menu sidebar */
      .nav-underline .nav-link {
        font-size: 1.1rem;
        padding: 0.75rem 1rem;
        margin-bottom: 0.5rem;
        border-radius: 0.375rem;
        transition: all 0.3s ease;
      }
      
      .nav-underline .nav-link:hover {
        background-color: rgba(13, 110, 253, 0.1);
      }
      
      .nav-underline .nav-link.active {
        background-color: #0d6efd;
        color: white !important;
        border-bottom: none;
      }
      
      [data-bs-theme='dark'] .nav-underline .nav-link {
        color: #ffffff;
      }
      
      [data-bs-theme='dark'] .nav-underline .nav-link:hover {
        background-color: rgba(255, 255, 255, 0.1);
      }
      
      [data-bs-theme='dark'] .nav-underline .nav-link.active {
        background-color: #0d6efd;
        color: white !important;
      }
      
      /* Área de conteúdo visível/oculta */
      .content-area {
        display: none;
      }
      
      .content-area.active {
        display: block;
      }
    "))
  ),
  
  # Botão de toggle do tema
  tags$button(
    id = "theme_toggle",
    class = "theme-toggle",
    onclick = "toggleTheme()",
    "🌙"
  ),
  
  # JavaScript para toggle e navegação
  tags$script(HTML("
    function toggleTheme() {
      const html = document.documentElement;
      const currentTheme = html.getAttribute('data-bs-theme');
      const newTheme = currentTheme === 'dark' ? 'light' : 'dark';
      const icon = newTheme === 'dark' ? '🌙' : '☀️';
      
      html.setAttribute('data-bs-theme', newTheme);
      document.getElementById('theme_toggle').textContent = icon;
      
      // Salvar preferência
      localStorage.setItem('theme', newTheme);
      
      // Notificar Shiny
      Shiny.setInputValue('current_theme', newTheme);
    }
    
    // Carregar tema salvo ao iniciar
    document.addEventListener('DOMContentLoaded', function() {
      const savedTheme = localStorage.getItem('theme') || 'light';
      const icon = savedTheme === 'dark' ? '🌙' : '☀️';
      
      document.documentElement.setAttribute('data-bs-theme', savedTheme);
      document.getElementById('theme_toggle').textContent = icon;
      
      // Notificar Shiny do tema inicial
      setTimeout(function() {
        Shiny.setInputValue('current_theme', savedTheme);
      }, 100);
    });
    
    // Função para alternar entre seções
    function showSection(section) {
      // Atualizar estado dos links do menu
      document.querySelectorAll('.nav-link').forEach(link => {
        link.classList.remove('active');
      });
      document.querySelector(`[data-section='${section}']`).classList.add('active');
      
      // Notificar Shiny da seção ativa - IMPORTANTE: usar priority event
      Shiny.setInputValue('active_section', section, {priority: 'event'});
    }
    
    // Handler para mudança programática de seção
    Shiny.addCustomMessageHandler('changeSection', function(section) {
      showSection(section);
    });
    
    // Inicializar com a seção de entrada ativa
    $(document).ready(function() {
      // Pequeno delay para garantir que Shiny está pronto
      setTimeout(function() {
        showSection('entrada');
      }, 100);
    });
  ")),
  
  sidebar = sidebar(
    width = 300,
    
    # Menu de navegação
    tags$nav(
      class = "nav nav-pills nav-fill flex-column",
      
      tags$a(
        class = "nav-link active",
        href = "#",
        `data-section` = "entrada",
        onclick = "showSection('entrada'); return false;",
        icon("edit"),
        " Entrada de Dados"
      ),
      
      tags$a(
        class = "nav-link",
        href = "#",
        `data-section` = "analise",
        onclick = "showSection('analise'); return false;",
        icon("chart-line"),
        " Análise"
      )
    ),
    
    tags$hr(),
    
    # Conteúdo da sidebar para Análise
    conditionalPanel(
      condition = "input.active_section == 'analise'",
      
      tags$div(
        h4("Resultados da Análise"),
        
        conditionalPanel(
          condition = "output.tem_analise",
          
          # Download do relatório
          downloadButton("download_relatorio", 
                         "Baixar Relatório Completo",
                         width = "100%",
                         class = "btn-primary"),
          
          hr(),
          
          # Resumo dos resultados
          h5("Resumo"),
          uiOutput("resumo_sidebar"),
          
          hr(),
          
          # Navegação rápida
          h5("Navegação Rápida"),
          tags$div(
            class = "d-grid gap-2",
            actionButton("scroll_dados", "📊 Dados", 
                         class = "btn-sm btn-outline-primary",
                         onclick = "document.getElementById('card-dados').scrollIntoView({behavior: 'smooth'});"),
            actionButton("scroll_grafico", "📈 Gráfico Principal", 
                         class = "btn-sm btn-outline-primary",
                         onclick = "document.getElementById('card-grafico').scrollIntoView({behavior: 'smooth'});"),
            actionButton("scroll_analise", "🔍 Análise", 
                         class = "btn-sm btn-outline-primary",
                         onclick = "document.getElementById('card-analise').scrollIntoView({behavior: 'smooth'});"),
            actionButton("scroll_graficos", "📉 Gráficos Adicionais", 
                         class = "btn-sm btn-outline-primary",
                         onclick = "document.getElementById('card-graficos').scrollIntoView({behavior: 'smooth'});"),
            actionButton("scroll_stats", "📋 Estatísticas", 
                         class = "btn-sm btn-outline-primary",
                         onclick = "document.getElementById('card-stats').scrollIntoView({behavior: 'smooth'});")
          )
        ),
        
        conditionalPanel(
          condition = "!output.tem_analise",
          tags$div(
            class = "alert alert-info",
            icon("info-circle"),
            " Nenhuma análise disponível.",
            tags$br(),
            "Insira dados e clique em 'Analisar Dados'."
          )
        )
      )
    )
  ),
  
  # Área principal
  # Seção de Entrada de Dados
  conditionalPanel(
    condition = "input.active_section == 'entrada' || input.active_section == null",
    
    tags$div(
      class = "container-fluid mt-3",
      
      tags$div(
        class = "row",
        tags$div(
          class = "col-12",
          card(
            card_header("Configuração da Análise"),
            card_body(
              # Escolher método de entrada
              radioButtons("metodo_entrada",
                           "Método de entrada:",
                           choices = c("Manual" = "manual",
                                       "Upload CSV" = "csv"),
                           selected = "manual",
                           inline = TRUE),
              
              # Tipo de análise
              radioButtons("tipo_analise",
                           "Tipo de análise:",
                           choices = c("Automática" = "auto",
                                       "MRU" = "mru", 
                                       "MRUV" = "mruv"),
                           selected = "auto",
                           inline = TRUE)
            )
          )
        )
      ),
      
      tags$div(
        class = "row mt-3",
        tags$div(
          class = "col-md-8",
          card(
            card_header("Entrada de Dados"),
            card_body(
              conditionalPanel(
                condition = "input.metodo_entrada == 'manual'",
                
                # Número de medidas
                numericInput("n_pontos", 
                             "Número de medidas:", 
                             value = 5, 
                             min = 3, 
                             max = 20,
                             width = "200px"),
                
                # Inputs dinâmicos em formato de tabela
                div(style = "max-height: 500px; overflow-y: auto;",
                    uiOutput("inputs_tabela")
                )
              ),
              
              conditionalPanel(
                condition = "input.metodo_entrada == 'csv'",
                
                fileInput("arquivo_csv", 
                          "Escolha o arquivo CSV:",
                          accept = c(".csv", ".txt"),
                          width = "100%"),
                
                helpText("O arquivo deve conter duas colunas: S (posição) e t (tempo)"),
                
                # Preview do CSV
                tags$div(
                  class = "mt-3",
                  verbatimTextOutput("preview_csv")
                )
              )
            )
          )
        ),
        
        tags$div(
          class = "col-md-4",
          card(
            card_header("Ações"),
            card_body(
              # Botão de análise
              actionButton("analisar", 
                           "Analisar Dados", 
                           class = "btn-primary btn-lg",
                           width = "100%"),
              
              br(), br(),
              
              downloadButton("download_template", 
                             "Baixar Template CSV",
                             width = "100%"),
              
              br(), br(),
              
              tags$div(
                class = "alert alert-info",
                tags$h6(icon("info-circle"), " Instruções:"),
                tags$ul(
                  tags$li("Insira pelo menos 3 pontos"),
                  tags$li("Use ponto (.) como separador decimal"),
                  tags$li("Tempo deve ser crescente")
                )
              )
            )
          )
        )
      )
    )
  ),
  
  # Seção de Análise - Resultados
  conditionalPanel(
    condition = "input.active_section == 'analise' && output.tem_analise",
    
    layout_columns(
      col_widths = c(12),
      
      card(
        id = "card-dados",
        card_header("Dados do Experimento"),
        card_body(
          DTOutput("tabela_dados")
        ),
        height = "500px"
      ),
      
      card(
        id = "card-grafico",
        card_header("Gráfico S(m) × t(s)"),
        card_body(
          plotlyOutput("grafico_principal", height = "600px")
        )
      ),
      
      layout_columns(
        col_widths = c(6, 6),
        
        card(
          id = "card-analise",
          card_header("Análise do Movimento"),
          card_body(
            uiOutput("analise_movimento")
          ),
          height = "450px"
        ),
        
        card(
          id = "card-parametros",
          card_header("Parâmetros Calculados"),
          card_body(
            style = "font-size: 14px;",
            uiOutput("parametros")
          ),
          height = "600px"
        )
      ),
      
      card(
        id = "card-graficos",
        card_header("Gráficos Adicionais"),
        card_body(
          layout_columns(
            col_widths = c(6, 6),
            plotlyOutput("grafico_velocidade", height = "400px"),
            plotlyOutput("grafico_aceleracao", height = "400px")
          )
        )
      ),
      
      card(
        id = "card-stats",
        card_header("Estatísticas do Ajuste"),
        card_body(
          uiOutput("estatisticas")
        ),
        height = "600px"
      )
    )
  ),
  
  # Mensagem quando está em análise mas não há dados
  conditionalPanel(
    condition = "input.active_section == 'analise' && !output.tem_analise",
    tags$div(
      class = "container mt-5",
      tags$div(
        class = "alert alert-info text-center",
        tags$h4(icon("info-circle"), " Nenhuma análise disponível"),
        tags$p("Para ver os resultados, primeiro você precisa:"),
        tags$ol(
          class = "text-start",
          style = "max-width: 500px; margin: 0 auto;",
          tags$li("Ir para a seção 'Entrada de Dados'"),
          tags$li("Inserir os dados do experimento"),
          tags$li("Clicar em 'Analisar Dados'")
        )
      )
    )
  )
)