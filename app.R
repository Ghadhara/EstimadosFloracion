# ============================================================
# SHINY APP - PLANIFICACIÓN DE COSECHA DE CRISANTEMOS
# ============================================================
# Requiere: shiny, shinydashboard, DT, readxl, dplyr, lubridate, 
#           ggplot2, plotly, writexl, shinyWidgets, jsonlite
# Instalar con:
# install.packages(c("shiny","shinydashboard","DT","readxl","dplyr",
#                    "lubridate","ggplot2","plotly","writexl",
#                    "shinyWidgets","jsonlite","tidyr"))
# ============================================================

library(shiny)
library(shinydashboard)
library(DT)
library(readxl)
library(dplyr)
library(lubridate)
library(ggplot2)
library(plotly)
library(writexl)
library(shinyWidgets)
library(jsonlite)
library(tidyr)

# ── Ruta del archivo de datos de variedades ──────────────────────────────────
XLSX_PATH <- "ciclos_noches_luz_curvas.xlsx"

# ── Ruta de persistencia del registro de siembras ───────────────────────────
REGISTRO_PATH <- "registro_siembras.rds"

# ── Cargar catálogo de variedades ────────────────────────────────────────────
cargar_catalogo <- function() {
  df <- read_excel(XLSX_PATH)
  df <- df %>%
    rename(
      variedad       = Variedad,
      noches_luz     = `Noches Luz`,
      ciclo          = `Ciclo floracion`,
      producto       = producto,
      dia1 = DIA1, dia2 = DIA2, dia3 = DIA3,
      dia4 = DIA4, dia5 = DIA5, dia6 = DIA6, dia7 = DIA7
    )
  df[is.na(df)] <- 0
  df
}

catalogo <- cargar_catalogo()

# ── Helpers ──────────────────────────────────────────────────────────────────

# Lunes de la semana que contiene una fecha dada
lunes_semana <- function(fecha) {
  fecha - (wday(fecha, week_start = 1) - 1)
}

# Número de semana ISO (lunes = inicio)
semana_iso <- function(fecha) {
  isoweek(fecha)
}

# Año ISO
anio_iso <- function(fecha) {
  isoyear(fecha)
}

# Etiqueta "Semana WW/AAAA (dd/mm – dd/mm)"
etiqueta_semana <- function(fecha) {
  lun <- lunes_semana(fecha)
  dom <- lun + 6
  sprintf("Sem %02d/%d  (%s – %s)",
          semana_iso(lun), anio_iso(lun),
          format(lun, "%d/%m"), format(dom, "%d/%m"))
}

# Calcular proyección de corte para una siembra
proyectar_siembra <- function(row_siembra, cat) {
  variedad_sel <- row_siembra$variedad
  producto_sel <- row_siembra$producto
  fecha_s      <- as.Date(row_siembra$fecha_siembra)
  esquejes     <- as.numeric(row_siembra$esquejes)
  
  # Buscar variedad + producto en catálogo
  info <- cat %>%
    filter(variedad == variedad_sel, producto == producto_sel) %>%
    slice(1)
  
  if (nrow(info) == 0) return(NULL)
  
  ciclo <- info$ciclo
  
  # DIA4 = día de floración máxima = fecha_siembra + ciclo días
  fecha_dia4 <- fecha_s + ciclo
  
  dias_info <- data.frame(
    dia_num  = 1:7,
    pct      = as.numeric(c(info$dia1, info$dia2, info$dia3,
                            info$dia4, info$dia5, info$dia6, info$dia7))
  ) %>% filter(pct > 0)
  
  # Offset relativo a DIA4
  dias_info <- dias_info %>%
    mutate(
      offset      = dia_num - 4,
      fecha_corte = fecha_dia4 + offset,
      tallos      = round(esquejes * pct),
      semana_lun  = lunes_semana(fecha_corte),
      semana_num  = semana_iso(fecha_corte),
      anio        = anio_iso(fecha_corte),
      etiqueta_sw = etiqueta_semana(fecha_corte)
    ) %>%
    mutate(
      bloque   = row_siembra$bloque,
      cama     = row_siembra$cama,
      variedad = variedad_sel,
      producto = producto_sel,
      id_siembra = row_siembra$id
    )
  
  dias_info
}

# ── Cargar / guardar registro ────────────────────────────────────────────────
cargar_registro <- function() {
  if (file.exists(REGISTRO_PATH)) {
    readRDS(REGISTRO_PATH)
  } else {
    data.frame(
      id            = character(),
      bloque        = character(),
      cama          = character(),
      producto      = character(),
      variedad      = character(),
      fecha_siembra = as.Date(character()),
      esquejes      = numeric(),
      stringsAsFactors = FALSE
    )
  }
}

guardar_registro <- function(df) {
  saveRDS(df, REGISTRO_PATH)
}

# ── UI ───────────────────────────────────────────────────────────────────────
ui <- dashboardPage(
  skin = "green",
  
  dashboardHeader(title = "🌸 Planificador de Crisantemos"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("➕ Registrar Siembra",   tabName = "registro",  icon = icon("seedling")),
      menuItem("📋 Mis Registros",        tabName = "registros", icon = icon("table")),
      menuItem("📆 Resumen por Semana",   tabName = "resumen",   icon = icon("calendar-week")),
      menuItem("🔮 Estimación de Cosecha",tabName = "estimacion",icon = icon("chart-bar"))
    )
  ),
  
  dashboardBody(
    tags$head(tags$style(HTML("
      .skin-green .main-header .logo { background-color: #2e7d32; }
      .skin-green .main-header .navbar { background-color: #388e3c; }
      .skin-green .main-sidebar { background-color: #1b5e20; }
      .skin-green .sidebar-menu > li.active > a { background-color: #2e7d32 !important; }
      .box.box-success { border-top-color: #2e7d32; }
      .info-box-icon { font-size: 28px; }
      .table thead th { background-color: #2e7d32; color: white; }
      .week-badge { background:#2e7d32; color:white; padding:3px 8px;
                    border-radius:4px; font-size:12px; }
    "))),
    
    tabItems(
      
      # ── TAB 1: Registrar Siembra ──────────────────────────────────────────
      tabItem(tabName = "registro",
              fluidRow(
                box(title = "Nueva Siembra", status = "success", solidHeader = TRUE,
                    width = 12, icon = icon("plus-circle"),
                    fluidRow(
                      column(3,
                             textInput("bloque", "Bloque", placeholder = "Ej: A"),
                             textInput("cama",   "Cama",   placeholder = "Ej: 01")
                      ),
                      column(3,
                             selectInput("producto_reg", "Producto",
                                         choices = sort(unique(catalogo$producto))),
                             uiOutput("ui_variedad")
                      ),
                      column(3,
                             dateInput("fecha_siembra", "Fecha de Siembra",
                                       value = Sys.Date(), language = "es",
                                       format = "dd/mm/yyyy"),
                             numericInput("esquejes", "Cantidad de Esquejes",
                                          value = 1000, min = 1, step = 1)
                      ),
                      column(3,
                             br(), br(),
                             actionButton("btn_agregar", "➕ Agregar Siembra",
                                          class = "btn-success btn-lg", width = "100%")
                      )
                    ),
                    hr(),
                    fluidRow(
                      column(12,
                             h4("Vista previa de floración"),
                             DTOutput("preview_floracion")
                      )
                    )
                )
              )
      ),
      
      # ── TAB 2: Mis Registros ──────────────────────────────────────────────
      tabItem(tabName = "registros",
              fluidRow(
                box(title = "Registro de Siembras", status = "success",
                    solidHeader = TRUE, width = 12,
                    fluidRow(
                      column(3,
                             downloadButton("btn_descargar", "⬇ Descargar Excel",
                                            class = "btn-success")
                      ),
                      column(3,
                             actionButton("btn_limpiar_todo", "🗑 Limpiar todo",
                                          class = "btn-danger")
                      )
                    ),
                    br(),
                    DTOutput("tabla_registro"),
                    br(),
                    uiOutput("ui_borrar_fila")
                )
              )
      ),
      
      # ── TAB 3: Resumen por Semana ─────────────────────────────────────────
      tabItem(tabName = "resumen",
              fluidRow(
                box(title = "Resumen de Floración por Semana", status = "success",
                    solidHeader = TRUE, width = 12,
                    p("Se muestra el total de tallos proyectados por semana, variedad y producto."),
                    DTOutput("tabla_resumen")
                )
              ),
              fluidRow(
                box(title = "Distribución de Tallos por Semana", status = "success",
                    solidHeader = TRUE, width = 12,
                    plotlyOutput("grafico_semanas", height = "400px")
                )
              )
      ),
      
      # ── TAB 4: Estimación de Cosecha ─────────────────────────────────────
      tabItem(tabName = "estimacion",
              fluidRow(
                box(title = "Selecciona las semanas a estimar", status = "success",
                    solidHeader = TRUE, width = 4,
                    uiOutput("ui_semanas_disponibles"),
                    br(),
                    actionButton("btn_estimar", "🔮 Calcular Estimación",
                                 class = "btn-success btn-lg", width = "100%")
                ),
                box(title = "Resultado de la Estimación", status = "success",
                    solidHeader = TRUE, width = 8,
                    DTOutput("tabla_estimacion")
                )
              ),
              fluidRow(
                box(title = "Gráfico de Estimación", status = "success",
                    solidHeader = TRUE, width = 12,
                    plotlyOutput("grafico_estimacion", height = "450px")
                )
              )
      )
    )
  )
)

# ── SERVER ───────────────────────────────────────────────────────────────────
server <- function(input, output, session) {
  
  # ── Estado reactivo ────────────────────────────────────────────────────────
  rv <- reactiveValues(
    registro = cargar_registro()
  )
  
  # ── UI dinámica: variedad según producto ───────────────────────────────────
  output$ui_variedad <- renderUI({
    req(input$producto_reg)
    vars <- catalogo %>%
      filter(producto == input$producto_reg) %>%
      pull(variedad) %>% unique() %>% sort()
    selectInput("variedad_reg", "Variedad", choices = vars)
  })
  
  # ── Proyección de la siembra actual (preview) ──────────────────────────────
  proyeccion_preview <- reactive({
    req(input$variedad_reg, input$producto_reg,
        input$fecha_siembra, input$esquejes,
        input$bloque, input$cama)
    
    row_temp <- data.frame(
      id            = "preview",
      bloque        = input$bloque,
      cama          = input$cama,
      variedad      = input$variedad_reg,
      producto      = input$producto_reg,
      fecha_siembra = input$fecha_siembra,
      esquejes      = input$esquejes,
      stringsAsFactors = FALSE
    )
    proyectar_siembra(row_temp, catalogo)
  })
  
  output$preview_floracion <- renderDT({
    df <- proyeccion_preview()
    req(!is.null(df))
    
    df_show <- df %>%
      select(dia_num, fecha_corte, etiqueta_sw, pct, tallos) %>%
      rename(
        `Día` = dia_num,
        `Fecha de Corte` = fecha_corte,
        `Semana` = etiqueta_sw,
        `% Corte` = pct,
        `Tallos Estimados` = tallos
      ) %>%
      mutate(`% Corte` = scales::percent(`% Corte`, accuracy = 1))
    
    datatable(df_show, options = list(dom = 't', pageLength = 7),
              rownames = FALSE) %>%
      formatStyle("Día", fontWeight = "bold") %>%
      formatStyle("Tallos Estimados",
                  background = styleColorBar(range(df$tallos), "#a5d6a7"),
                  backgroundSize = "90% 70%",
                  backgroundRepeat = "no-repeat",
                  backgroundPosition = "center")
  })
  
  # ── Agregar siembra al registro ────────────────────────────────────────────
  observeEvent(input$btn_agregar, {
    req(input$bloque, input$cama, input$variedad_reg,
        input$producto_reg, input$fecha_siembra, input$esquejes)
    
    nueva_fila <- data.frame(
      id            = format(Sys.time(), "%Y%m%d%H%M%S"),
      bloque        = toupper(trimws(input$bloque)),
      cama          = toupper(trimws(input$cama)),
      producto      = input$producto_reg,
      variedad      = input$variedad_reg,
      fecha_siembra = as.Date(input$fecha_siembra),
      esquejes      = as.numeric(input$esquejes),
      stringsAsFactors = FALSE
    )
    
    rv$registro <- bind_rows(rv$registro, nueva_fila)
    guardar_registro(rv$registro)
    
    showNotification(
      paste0("✅ Siembra agregada: Bloque ", nueva_fila$bloque,
             " / Cama ", nueva_fila$cama),
      type = "message", duration = 3
    )
  })
  
  # ── Tabla de registros ─────────────────────────────────────────────────────
  output$tabla_registro <- renderDT({
    df <- rv$registro
    if (nrow(df) == 0) {
      df_show <- data.frame(Mensaje = "No hay siembras registradas aún.")
    } else {
      df_show <- df %>%
        select(-id) %>%
        rename(
          Bloque = bloque, Cama = cama,
          Producto = producto, Variedad = variedad,
          `Fecha Siembra` = fecha_siembra,
          Esquejes = esquejes
        ) %>%
        mutate(`Fecha Siembra` = format(`Fecha Siembra`, "%d/%m/%Y"))
    }
    datatable(df_show, selection = "single", rownames = TRUE,
              options = list(pageLength = 15, scrollX = TRUE))
  })
  
  # ── Botón borrar fila seleccionada ─────────────────────────────────────────
  output$ui_borrar_fila <- renderUI({
    req(nrow(rv$registro) > 0)
    actionButton("btn_borrar_fila", "🗑 Borrar fila seleccionada",
                 class = "btn-warning")
  })
  
  observeEvent(input$btn_borrar_fila, {
    req(input$tabla_registro_rows_selected)
    idx <- input$tabla_registro_rows_selected
    rv$registro <- rv$registro[-idx, ]
    guardar_registro(rv$registro)
    showNotification("🗑 Siembra eliminada.", type = "warning", duration = 3)
  })
  
  observeEvent(input$btn_limpiar_todo, {
    showModal(modalDialog(
      title = "¿Confirmar eliminación?",
      "Esto borrará TODOS los registros de siembra permanentemente.",
      footer = tagList(
        modalButton("Cancelar"),
        actionButton("confirmar_limpiar", "Sí, eliminar todo",
                     class = "btn-danger")
      )
    ))
  })
  
  observeEvent(input$confirmar_limpiar, {
    rv$registro <- rv$registro[0, ]
    guardar_registro(rv$registro)
    removeModal()
    showNotification("🗑 Todos los registros eliminados.", type = "error", duration = 3)
  })
  
  # ── Descargar registros en Excel ───────────────────────────────────────────
  output$btn_descargar <- downloadHandler(
    filename = function() paste0("registro_siembras_", Sys.Date(), ".xlsx"),
    content  = function(file) {
      write_xlsx(rv$registro %>%
                   rename(Bloque=bloque, Cama=cama, Producto=producto,
                          Variedad=variedad, `Fecha Siembra`=fecha_siembra,
                          Esquejes=esquejes) %>% select(-id),
                 file)
    }
  )
  
  # ── Proyecciones de todos los registros ───────────────────────────────────
  todas_proyecciones <- reactive({
    df <- rv$registro
    if (nrow(df) == 0) return(NULL)
    
    resultado <- purrr::map_dfr(seq_len(nrow(df)), function(i) {
      proyectar_siembra(df[i, ], catalogo)
    })
    resultado
  })
  
  # ── Tabla resumen por semana ───────────────────────────────────────────────
  resumen_semanas <- reactive({
    df <- todas_proyecciones()
    req(!is.null(df))
    
    df %>%
      group_by(anio, semana_num, etiqueta_sw, variedad, producto) %>%
      summarise(tallos_total = sum(tallos, na.rm = TRUE), .groups = "drop") %>%
      arrange(anio, semana_num)
  })
  
  output$tabla_resumen <- renderDT({
    df <- resumen_semanas()
    req(!is.null(df), nrow(df) > 0)
    
    df_show <- df %>%
      rename(Año = anio, `N° Semana` = semana_num,
             Semana = etiqueta_sw, Variedad = variedad,
             Producto = producto, `Tallos Totales` = tallos_total)
    
    datatable(df_show, rownames = FALSE,
              options = list(pageLength = 20, scrollX = TRUE,
                             order = list(list(0, "asc"), list(1, "asc")))) %>%
      formatStyle("Tallos Totales",
                  background = styleColorBar(range(df$tallos_total), "#a5d6a7"),
                  backgroundSize = "90% 70%",
                  backgroundRepeat = "no-repeat",
                  backgroundPosition = "center")
  })
  
  output$grafico_semanas <- renderPlotly({
    df <- resumen_semanas()
    req(!is.null(df), nrow(df) > 0)
    
    df_agg <- df %>%
      group_by(etiqueta_sw, semana_num, anio, producto) %>%
      summarise(tallos = sum(tallos_total), .groups = "drop") %>%
      arrange(anio, semana_num)
    
    df_agg$etiqueta_sw <- factor(df_agg$etiqueta_sw,
                                 levels = unique(df_agg$etiqueta_sw))
    
    p <- ggplot(df_agg, aes(x = etiqueta_sw, y = tallos, fill = producto,
                            text = paste0("Semana: ", etiqueta_sw,
                                          "<br>Producto: ", producto,
                                          "<br>Tallos: ", format(tallos, big.mark = ",")))) +
      geom_col(position = "stack") +
      scale_fill_brewer(palette = "Set2") +
      labs(x = "Semana", y = "Tallos estimados", fill = "Producto") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))
    
    ggplotly(p, tooltip = "text")
  })
  
  # ── Selector de semanas para estimación ───────────────────────────────────
  output$ui_semanas_disponibles <- renderUI({
    df <- resumen_semanas()
    if (is.null(df) || nrow(df) == 0) {
      return(p("No hay proyecciones aún. Agrega siembras primero."))
    }
    
    semanas <- df %>%
      arrange(anio, semana_num) %>%
      pull(etiqueta_sw) %>% unique()
    
    checkboxGroupInput("semanas_sel", "Semanas disponibles:",
                       choices  = semanas,
                       selected = semanas[1:min(4, length(semanas))])
  })
  
  # ── Tabla de estimación ────────────────────────────────────────────────────
  estimacion_resultado <- eventReactive(input$btn_estimar, {
    req(input$semanas_sel)
    df <- resumen_semanas()
    req(!is.null(df))
    
    df %>%
      filter(etiqueta_sw %in% input$semanas_sel) %>%
      arrange(anio, semana_num, producto, variedad)
  })
  
  output$tabla_estimacion <- renderDT({
    df <- estimacion_resultado()
    req(nrow(df) > 0)
    
    df_show <- df %>%
      rename(Semana = etiqueta_sw, Variedad = variedad,
             Producto = producto, `Tallos Estimados` = tallos_total) %>%
      select(Semana, Producto, Variedad, `Tallos Estimados`)
    
    datatable(df_show, rownames = FALSE,
              options = list(pageLength = 25, scrollX = TRUE)) %>%
      formatStyle("Tallos Estimados",
                  background = styleColorBar(range(df$tallos_total), "#81c784"),
                  backgroundSize = "90% 70%",
                  backgroundRepeat = "no-repeat",
                  backgroundPosition = "center") %>%
      formatRound("Tallos Estimados", digits = 0)
  })
  
  output$grafico_estimacion <- renderPlotly({
    df <- estimacion_resultado()
    req(nrow(df) > 0)
    
    df_plot <- df %>%
      group_by(etiqueta_sw, semana_num, anio, producto) %>%
      summarise(tallos = sum(tallos_total), .groups = "drop") %>%
      arrange(anio, semana_num)
    
    df_plot$etiqueta_sw <- factor(df_plot$etiqueta_sw,
                                  levels = unique(df_plot$etiqueta_sw))
    
    p <- ggplot(df_plot,
                aes(x = etiqueta_sw, y = tallos, fill = producto,
                    text = paste0(etiqueta_sw, "\n",
                                  producto, ": ",
                                  format(tallos, big.mark = ","), " tallos"))) +
      geom_col(position = "stack", width = 0.6) +
      geom_text(aes(label = format(tallos, big.mark = ",")),
                position = position_stack(vjust = 0.5),
                size = 3, color = "white", fontface = "bold") +
      scale_fill_brewer(palette = "Set2") +
      labs(title = "Estimación de Tallos por Semana",
           x = "Semana", y = "Tallos", fill = "Producto") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 30, hjust = 1))
    
    ggplotly(p, tooltip = "text")
  })
}

# ── Run ──────────────────────────────────────────────────────────────────────
shinyApp(ui, server)