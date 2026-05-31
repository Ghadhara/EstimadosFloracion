# ============================================================
# SHINY APP - PLANIFICACIÓN DE COSECHA DE CRISANTEMOS
# ============================================================
# Instalar paquetes (solo la primera vez):
# install.packages(c("shiny","shinydashboard","DT","readxl","dplyr",
#                    "lubridate","ggplot2","plotly","writexl",
#                    "shinyWidgets","jsonlite","tidyr","purrr","scales"))
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
library(purrr)
library(scales)

XLSX_PATH     <- "ciclos_noches_luz_curvas.xlsx"
REGISTRO_PATH <- "registro_siembras.rds"

# ── Catálogo de variedades ────────────────────────────────────────────────────
cargar_catalogo <- function() {
  df <- read_excel(XLSX_PATH) %>%
    rename(
      variedad   = Variedad,
      noches_luz = `Noches Luz`,
      ciclo      = `Ciclo floracion`,
      dia1 = DIA1, dia2 = DIA2, dia3 = DIA3,
      dia4 = DIA4, dia5 = DIA5, dia6 = DIA6, dia7 = DIA7
    )
  df[is.na(df)] <- 0
  df
}

catalogo <- cargar_catalogo()

# ── Helpers de fechas ─────────────────────────────────────────────────────────
lunes_semana    <- function(f) f - (wday(f, week_start = 1) - 1)
semana_iso      <- function(f) isoweek(f)
anio_iso        <- function(f) isoyear(f)
etiqueta_semana <- function(f) {
  lun <- lunes_semana(f); dom <- lun + 6
  sprintf("Sem %02d/%d  (%s – %s)",
          semana_iso(lun), anio_iso(lun),
          format(lun, "%d/%m"), format(dom, "%d/%m"))
}

# ── Proyectar días de corte de una siembra ────────────────────────────────────
proyectar_siembra <- function(row_s, cat) {
  info <- cat %>%
    filter(variedad == row_s$variedad, producto == row_s$producto) %>%
    slice(1)
  if (nrow(info) == 0) return(NULL)
  
  fecha_s    <- as.Date(row_s$fecha_siembra)
  esquejes   <- as.numeric(row_s$esquejes)
  fecha_dia4 <- fecha_s + info$ciclo
  
  data.frame(
    dia_num = 1:7,
    pct = as.numeric(c(info$dia1, info$dia2, info$dia3,
                       info$dia4, info$dia5, info$dia6, info$dia7))
  ) %>%
    filter(pct > 0) %>%
    mutate(
      offset      = dia_num - 4,
      fecha_corte = fecha_dia4 + offset,
      tallos      = round(esquejes * pct),
      semana_lun  = lunes_semana(fecha_corte),
      semana_num  = semana_iso(fecha_corte),
      anio        = anio_iso(fecha_corte),
      etiqueta_sw = etiqueta_semana(fecha_corte),
      bloque      = row_s$bloque,
      cama        = row_s$cama,
      variedad    = row_s$variedad,
      producto    = row_s$producto,
      id_siembra  = row_s$id
    )
}

# ── Persistencia ──────────────────────────────────────────────────────────────
registro_vacio <- function() {
  data.frame(id=character(), bloque=character(), cama=character(),
             producto=character(), variedad=character(),
             fecha_siembra=as.Date(character()), esquejes=numeric(),
             stringsAsFactors=FALSE)
}
cargar_registro  <- function() {
  if (file.exists(REGISTRO_PATH)) readRDS(REGISTRO_PATH) else registro_vacio()
}
guardar_registro <- function(df) saveRDS(df, REGISTRO_PATH)

cols_plantilla <- c("bloque","cama","producto","variedad","fecha_siembra","esquejes")

# ═══════════════════════════════════════════════════════════════════════════════
# UI
# ═══════════════════════════════════════════════════════════════════════════════
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = tags$span(
    tags$img(src="https://img.icons8.com/emoji/28/blossom-emoji.png",
             style="vertical-align:middle; margin-right:6px;"),
    "Planificador de Crisantemos"
  )),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("🏠 Inicio",                 tabName = "inicio",     icon = icon("house")),
      menuItem("➕ Registrar Siembra",    tabName = "registro",   icon = icon("seedling")),
      menuItem("📋 Mis Registros",         tabName = "registros",  icon = icon("table")),
      menuItem("📆 Resumen de Siembras",   tabName = "resumen",    icon = icon("calendar-week")),
      menuItem("🔮 Estimación de Cosecha", tabName = "estimacion", icon = icon("chart-bar")),
      menuItem("✂ Camas Cortadas",         tabName = "cortadas",   icon = icon("scissors"))
    )
  ),
  
  dashboardBody(
    tags$head(tags$style(HTML("
      /* ── Paleta principal #2f8d96 ── */
      .skin-blue .main-header .logo              { background-color:#2f8d96 !important; }
      .skin-blue .main-header .logo:hover        { background-color:#267880 !important; }
      .skin-blue .main-header .navbar            { background-color:#267880 !important; }
      .skin-blue .main-sidebar                   { background-color:#1a5c63 !important; }
      .skin-blue .sidebar-menu > li > a          { color:#cce8ea !important; }
      .skin-blue .sidebar-menu > li.active > a,
      .skin-blue .sidebar-menu > li > a:hover    { background-color:#2f8d96 !important;
                                                   color:#ffffff !important; }
      .skin-blue .main-header .navbar .sidebar-toggle { color:#fff !important; }

      /* Boxes */
      .box.box-success  { border-top-color:#2f8d96 !important; }
      .box.box-primary  { border-top-color:#2f8d96 !important; }
      .box.box-info     { border-top-color:#4ab8c1 !important; }
      .box.box-warning  { border-top-color:#e07b28 !important; }
      .box-header.with-border { border-bottom-color:#b2dde0 !important; }

      /* Botones success → teal */
      .btn-success, .btn-success:focus {
        background-color:#2f8d96 !important;
        border-color:#267880 !important; color:#fff !important; }
      .btn-success:hover {
        background-color:#267880 !important;
        border-color:#1e6169 !important; }

      /* Info boxes */
      .info-box-icon { font-size:26px; line-height:60px; }
      .info-box       { min-height:60px; }
      .info-box-content { padding:8px 10px; }
      .info-box-number  { font-size:22px; font-weight:700; }

      /* Tablas */
      .table thead th { background-color:#2f8d96 !important; color:white !important; }

      /* Misc */
      .alerta-roja { color:#c62828; font-weight:bold; }
      hr.sep { border-top:2px dashed #b2dde0; margin:18px 0; }

      /* Página inicio */
      .hero-card { background:linear-gradient(135deg,#2f8d96 0%,#1a5c63 100%);
                   color:white; border-radius:12px; padding:32px 36px;
                   margin-bottom:20px; }
      .hero-card h2 { color:#fff; margin-top:0; font-size:26px; }
      .hero-card p  { color:#d4f0f2; font-size:15px; }
      .feature-card { border-left:4px solid #2f8d96; padding:12px 16px;
                      background:#f0fafb; border-radius:0 8px 8px 0;
                      margin-bottom:12px; }
      .feature-card h4 { margin:0 0 4px 0; color:#1a5c63; }
      .feature-card p  { margin:0; color:#444; font-size:13px; }
      .badge-teal { background:#2f8d96; color:white; border-radius:12px;
                    padding:2px 10px; font-size:12px; font-weight:600; }
      .firma-box { background:#f0fafb; border:1px solid #b2dde0;
                   border-radius:10px; padding:16px 20px; margin-top:16px;
                   text-align:center; }
    "))),
    
    tabItems(
      
      # ── TAB 0: INICIO ─────────────────────────────────────────────────────
      tabItem(tabName = "inicio",
              fluidRow(
                column(12,
                       div(class="hero-card",
                           fluidRow(
                             column(8,
                                    h2("🌸 Planificador de Cosecha de Crisantemos"),
                                    p("Sistema de gestión y proyección de siembras para optimizar
                    la planificación de floraciones y estimación de tallos a cortar.")
                             ),
                             column(4, align="right",
                                    br(),
                                    tags$img(
                                      src="https://img.icons8.com/emoji/96/blossom-emoji.png",
                                      style="opacity:0.7"
                                    )
                             )
                           )
                       )
                )
              ),
              fluidRow(
                column(6,
                       h3(style="color:#1a5c63;", "📖 ¿Qué puedes hacer con esta app?"),
                       div(class="feature-card",
                           h4("➕ Registrar Siembras"),
                           p("Ingresa siembras una a una o carga un Excel con cientos de registros.
                 La app valida cada fila y te permite corregir errores directamente.")
                       ),
                       div(class="feature-card",
                           h4("📋 Mis Registros"),
                           p("Consulta, filtra, descarga y elimina el historial completo de siembras guardadas.
                 Los datos persisten entre sesiones automáticamente.")
                       ),
                       div(class="feature-card",
                           h4("📆 Resumen de Siembras"),
                           p("Visualiza cuántos esquejes se sembraron semana a semana, agrupados por
                 producto y variedad, con totales por producto.")
                       ),
                       div(class="feature-card",
                           h4("🔮 Estimación de Cosecha"),
                           p("Selecciona las semanas que te interesan y obtén la proyección de tallos
                 a cortar por día, usando los ciclos y porcentajes de cada variedad.")
                       ),
                       div(class="feature-card",
                           h4("✂ Camas Cortadas"),
                           p("Identifica automáticamente las camas cuyo ciclo de corte ya terminó,
                 archívalas en respaldo y límpialas del registro activo.")
                       )
                ),
                column(6,
                       h3(style="color:#1a5c63; margin-bottom:12px;", "⚙ ¿Cómo funciona el cálculo?"),
                       div(
                         style = paste0(
                           "background:#f0fafb; border:1px solid #b2dde0; border-radius:8px;",
                           "padding:16px 20px; margin-bottom:20px;"
                         ),
                         tags$ul(style="font-size:14px; line-height:2.0; margin:0; padding-left:20px;",
                                 tags$li(tags$b("DIA 4"), " = Fecha de floración máxima = Fecha siembra + Ciclo de la variedad"),
                                 tags$li(tags$b("DIA 1–3"), " ocurren antes del DIA 4 (offset negativo)"),
                                 tags$li(tags$b("DIA 5–7"), " ocurren después del DIA 4 (offset positivo)"),
                                 tags$li(tags$b("Tallos"), " = Esquejes × % de corte del día"),
                                 tags$li(tags$b("Semanas"), " comienzan en lunes y terminan en domingo (ISO)"),
                                 tags$li(tags$b("Noches Luz"), " está registrada por variedad para referencia del cultivo")
                         )
                       ),
                       div(class="firma-box",
                           h4(style="color:#1a5c63; margin-top:0;", "👨‍💻 Desarrollado por"),
                           p(style="font-size:15px; margin:6px 0;",
                             tags$b("Giovanny Reales Rodríguez"), " · Ingeniero Agropecuario y Analista de Producción"),
                           p(style="font-size:15px; margin:6px 0;",
                             tags$b("Claude"), " · Asistente de IA de Anthropic 🤖"),
                           br(),
                           tags$span(class="badge-teal", "v2.2"),
                           tags$span(style="margin-left:8px; color:#666; font-size:12px;",
                                     paste0("Última actualización: ", format(Sys.Date(), "%d/%m/%Y")))
                       )
                )
              )
      ),
      
      # ── TAB 1: REGISTRAR SIEMBRA ─────────────────────────────────────────
      tabItem(tabName = "registro",
              
              fluidRow(
                box(title = "Nueva Siembra (individual)", status = "success",
                    solidHeader = TRUE, width = 12,
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
                      column(3, br(), br(),
                             actionButton("btn_agregar", "➕ Agregar Siembra",
                                          class = "btn-success btn-lg", width = "100%")
                      )
                    ),
                    hr(class = "sep"),
                    fluidRow(
                      column(12,
                             h4("Vista previa de floración"),
                             DTOutput("preview_floracion")
                      )
                    )
                )
              ),
              
              # ── Carga masiva ──────────────────────────────────────────────────
              fluidRow(
                box(title = "📥 Carga Masiva desde Excel", status = "success",
                    solidHeader = TRUE, width = 12,
                    fluidRow(
                      column(6,
                             h4("Instrucciones"),
                             p("El Excel debe tener exactamente estas columnas (minúsculas, sin tildes):"),
                             tags$ul(
                               tags$li(tags$b("bloque"),         " — texto, ej: A"),
                               tags$li(tags$b("cama"),            " — texto, ej: 01"),
                               tags$li(tags$b("producto"),        " — debe existir en el catálogo"),
                               tags$li(tags$b("variedad"),        " — debe existir en el catálogo"),
                               tags$li(tags$b("fecha_siembra"),   " — formato DD/MM/AAAA o AAAA-MM-DD"),
                               tags$li(tags$b("esquejes"),        " — número entero positivo")
                             ),
                             downloadButton("btn_plantilla", "⬇ Descargar plantilla de ejemplo",
                                            class = "btn-info")
                      ),
                      column(6,
                             fileInput("archivo_masivo", "Selecciona el archivo Excel:",
                                       accept = c(".xlsx", ".xls"),
                                       buttonLabel = "Buscar...",
                                       placeholder = "Ningún archivo seleccionado"),
                             uiOutput("ui_btn_confirmar_masivo")
                      )
                    ),
                    uiOutput("ui_tabla_masivo")
                )
              )
      ),
      
      # ── TAB 2: MIS REGISTROS ─────────────────────────────────────────────
      tabItem(tabName = "registros",
              fluidRow(
                box(title = "Registro de Siembras", status = "success",
                    solidHeader = TRUE, width = 12,
                    fluidRow(
                      column(3, downloadButton("btn_descargar", "⬇ Descargar Excel",
                                               class = "btn-success")),
                      column(3, actionButton("btn_limpiar_todo", "🗑 Limpiar todo",
                                             class = "btn-danger"))
                    ),
                    br(),
                    DTOutput("tabla_registro"),
                    br(),
                    uiOutput("ui_borrar_fila")
                )
              )
      ),
      
      # ── TAB 3: RESUMEN DE SIEMBRAS (semana de siembra) ───────────────────
      tabItem(tabName = "resumen",
              fluidRow(uiOutput("ui_infoboxes_resumen")),
              fluidRow(
                box(title = "Resumen de Siembras por Semana", status = "success",
                    solidHeader = TRUE, width = 12,
                    p("Muestra cuántos esquejes se sembraron en cada semana, por producto y variedad. No es la proyección de floración."),
                    DTOutput("tabla_resumen_siembra")
                )
              ),
              fluidRow(
                box(title = "🔍 Detalle por semana seleccionada", status = "success",
                    solidHeader = TRUE, width = 12,
                    fluidRow(
                      column(5, uiOutput("ui_selector_semana_resumen")),
                      column(7, uiOutput("ui_infoboxes_semana_sel"))
                    ),
                    br(),
                    DTOutput("tabla_detalle_semana")
                )
              ),
              fluidRow(
                box(title = "Esquejes sembrados por semana", status = "success",
                    solidHeader = TRUE, width = 12,
                    plotlyOutput("grafico_resumen_siembra", height = "400px")
                )
              )
      ),
      
      # ── TAB 4: ESTIMACIÓN DE COSECHA ─────────────────────────────────────
      tabItem(tabName = "estimacion",
              fluidRow(uiOutput("ui_infoboxes_estimacion")),
              fluidRow(
                box(title = "Selecciona las semanas a estimar", status = "success",
                    solidHeader = TRUE, width = 4,
                    uiOutput("ui_semanas_disponibles"),
                    br(),
                    actionButton("btn_estimar", "🔮 Calcular Estimación",
                                 class = "btn-success btn-lg", width = "100%")
                ),
                box(title = "Resultado de la Estimación de Floración", status = "success",
                    solidHeader = TRUE, width = 8,
                    DTOutput("tabla_estimacion")
                )
              ),
              fluidRow(
                box(title = "Gráfico de Estimación", status = "success",
                    solidHeader = TRUE, width = 12,
                    plotlyOutput("grafico_estimacion", height = "450px")
                )
              ),
              
              # ── Detalle diario por variedad ───────────────────────────────────
              fluidRow(
                box(title = "📅 Detalle diario por variedad — lunes a domingo",
                    status = "success", solidHeader = TRUE, width = 12,
                    p("Filtra por semana, producto y variedad (puedes seleccionar varios) para ver
               los tallos proyectados día a día con los bloques y camas de origen."),
                    fluidRow(
                      column(3, uiOutput("ui_semana_detalle_dia")),
                      column(3, uiOutput("ui_producto_detalle_dia")),
                      column(3, uiOutput("ui_variedad_detalle_dia")),
                      column(3, br(),
                             actionButton("btn_ver_detalle_dia", "📅 Ver detalle diario",
                                          class = "btn-success btn-lg", width = "100%")
                      )
                    ),
                    br(),
                    uiOutput("ui_cards_dias"),
                    br(),
                    DTOutput("tabla_detalle_dia")
                )
              )
      ),
      
      # ── TAB 5: CAMAS CORTADAS ─────────────────────────────────────────────
      tabItem(tabName = "cortadas",
              fluidRow(
                box(title = "✂ Gestión de Camas Cortadas", status = "warning",
                    solidHeader = TRUE, width = 12,
                    p("Se muestran las siembras cuyo último día de corte proyectado ya pasó. Puedes archivarlas (respaldo) y eliminarlas del registro activo."),
                    br(),
                    fluidRow(
                      column(4,
                             dateInput("fecha_ref_cortadas", "Fecha de referencia:",
                                       value = Sys.Date(), language = "es",
                                       format = "dd/mm/yyyy"),
                             p(em("Camas con último día de corte ANTES de esta fecha se consideran cortadas."))
                      ),
                      column(4, br(),
                             actionButton("btn_archivar_cortadas",
                                          "📦 Archivar camas cortadas",
                                          class = "btn-warning btn-lg", width = "100%"),
                             br(), br(),
                             p(em("Guarda un respaldo en archivo .rds separado antes de eliminar."))
                      ),
                      column(4, br(),
                             actionButton("btn_eliminar_cortadas",
                                          "🗑 Eliminar camas cortadas",
                                          class = "btn-danger btn-lg", width = "100%"),
                             br(), br(),
                             downloadButton("btn_descargar_cortadas",
                                            "⬇ Descargar listado", class = "btn-info")
                      )
                    ),
                    hr(class = "sep"),
                    h4(uiOutput("titulo_tabla_cortadas")),
                    DTOutput("tabla_cortadas")
                )
              )
      )
    )
  )
)

# ═══════════════════════════════════════════════════════════════════════════════
# SERVER
# ═══════════════════════════════════════════════════════════════════════════════
server <- function(input, output, session) {
  
  rv <- reactiveValues(
    registro       = cargar_registro(),
    df_masivo      = NULL,   # dataframe completo con columna .error
    fila_editar    = NULL    # índice de fila que está en edición
  )
  
  # ── Variedad dinámica según producto ──────────────────────────────────────
  output$ui_variedad <- renderUI({
    req(input$producto_reg)
    vars <- catalogo %>% filter(producto == input$producto_reg) %>%
      pull(variedad) %>% unique() %>% sort()
    selectInput("variedad_reg", "Variedad", choices = vars)
  })
  
  # ── Preview floración individual ──────────────────────────────────────────
  proyeccion_preview <- reactive({
    req(input$variedad_reg, input$producto_reg,
        input$fecha_siembra, input$esquejes, input$bloque, input$cama)
    row_tmp <- data.frame(id="preview", bloque=input$bloque, cama=input$cama,
                          variedad=input$variedad_reg, producto=input$producto_reg,
                          fecha_siembra=input$fecha_siembra,
                          esquejes=input$esquejes, stringsAsFactors=FALSE)
    proyectar_siembra(row_tmp, catalogo)
  })
  
  output$preview_floracion <- renderDT({
    df <- proyeccion_preview(); req(!is.null(df))
    df %>%
      select(dia_num, fecha_corte, etiqueta_sw, pct, tallos) %>%
      rename(`Día`=dia_num, `Fecha de Corte`=fecha_corte,
             Semana=etiqueta_sw, `% Corte`=pct, `Tallos Estimados`=tallos) %>%
      mutate(`% Corte` = percent(`% Corte`, accuracy=1)) %>%
      datatable(options=list(dom='t', pageLength=10), rownames=FALSE) %>%
      formatStyle("Tallos Estimados",
                  background=styleColorBar(range(df$tallos), "#a5d6a7"),
                  backgroundSize="90% 70%", backgroundRepeat="no-repeat",
                  backgroundPosition="center")
  })
  
  # ── Agregar siembra individual ─────────────────────────────────────────────
  observeEvent(input$btn_agregar, {
    req(input$bloque, input$cama, input$variedad_reg,
        input$producto_reg, input$fecha_siembra, input$esquejes)
    nueva <- data.frame(
      id            = format(Sys.time(), "%Y%m%d%H%M%S%OS3"),
      bloque        = toupper(trimws(input$bloque)),
      cama          = toupper(trimws(input$cama)),
      producto      = input$producto_reg,
      variedad      = input$variedad_reg,
      fecha_siembra = as.Date(input$fecha_siembra),
      esquejes      = as.numeric(input$esquejes),
      stringsAsFactors = FALSE
    )
    rv$registro <- bind_rows(rv$registro, nueva)
    guardar_registro(rv$registro)
    showNotification(paste0("✅ Siembra agregada: Bloque ", nueva$bloque,
                            " / Cama ", nueva$cama),
                     type="message", duration=3)
  })
  
  # ══ CARGA MASIVA ══════════════════════════════════════════════════════════
  
  # Helper: calcula errores fila a fila y devuelve vector de strings (vacío = OK)
  errores_fila <- function(row) {
    errs <- c()
    if (!row$producto %in% catalogo$producto)
      errs <- c(errs, paste0("Producto '", row$producto, "' no existe en catálogo"))
    if (!row$variedad %in% catalogo$variedad)
      errs <- c(errs, paste0("Variedad '", row$variedad, "' no existe en catálogo"))
    # Validar combinación producto+variedad
    if (row$producto %in% catalogo$producto && row$variedad %in% catalogo$variedad) {
      combo_ok <- any(catalogo$producto == row$producto & catalogo$variedad == row$variedad)
      if (!combo_ok)
        errs <- c(errs, paste0("La variedad '", row$variedad,
                               "' no corresponde al producto '", row$producto, "'"))
    }
    if (is.na(row$fecha_siembra))
      errs <- c(errs, "Fecha inválida (usa DD/MM/AAAA)")
    if (is.na(row$esquejes) || row$esquejes <= 0)
      errs <- c(errs, "Esquejes debe ser un número positivo")
    errs
  }
  
  # Parsear fecha desde varios formatos
  parsear_fecha <- function(x) {
    s <- as.character(x)
    if (grepl("^\\d{2}/\\d{2}/\\d{4}$", s)) return(as.Date(s, "%d/%m/%Y"))
    if (grepl("^\\d{4}-\\d{2}-\\d{2}$", s)) return(as.Date(s, "%Y-%m-%d"))
    n <- suppressWarnings(as.numeric(s))
    if (!is.na(n))  return(as.Date(n, origin="1899-12-30"))
    return(as.Date(NA))
  }
  
  # Plantilla descargable
  output$btn_plantilla <- downloadHandler(
    filename = "plantilla_siembras.xlsx",
    content  = function(file) {
      write_xlsx(data.frame(
        bloque        = c("A","A","B"),
        cama          = c("01","02","01"),
        producto      = c("PREMIUM","P.AMERICANO","SANTINI"),
        variedad      = c("ALMA","ALBA","APRILIA"),
        fecha_siembra = c("15/01/2025","20/01/2025","01/02/2025"),
        esquejes      = c(1200L, 800L, 1500L),
        stringsAsFactors = FALSE
      ), file)
    }
  )
  
  # Leer y validar archivo masivo — guarda TODO con columna .error
  observeEvent(input$archivo_masivo, {
    rv$df_masivo   <- NULL
    rv$fila_editar <- NULL
    path <- input$archivo_masivo$datapath
    
    tryCatch({
      df_raw <- read_excel(path)
      names(df_raw) <- tolower(trimws(gsub(" ", "_", names(df_raw))))
      
      faltantes <- setdiff(cols_plantilla, names(df_raw))
      if (length(faltantes) > 0) {
        showNotification(paste("Faltan columnas:", paste(faltantes, collapse=", ")),
                         type="error", duration=8)
        return()
      }
      
      df <- df_raw %>% select(all_of(cols_plantilla)) %>%
        mutate(
          bloque   = toupper(trimws(as.character(bloque))),
          cama     = toupper(trimws(as.character(cama))),
          producto = trimws(as.character(producto)),
          variedad = trimws(as.character(variedad)),
          esquejes = suppressWarnings(as.numeric(esquejes)),
          fecha_siembra = as.Date(sapply(fecha_siembra, parsear_fecha,
                                         USE.NAMES=FALSE),
                                  origin="1970-01-01")
        )
      
      # Calcular error por fila
      df$.error <- sapply(seq_len(nrow(df)), function(i) {
        errs <- errores_fila(df[i,])
        if (length(errs) == 0) "" else paste(errs, collapse=" | ")
      })
      
      rv$df_masivo <- df
      
    }, error = function(e) {
      showNotification(paste("Error leyendo el archivo:", e$message),
                       type="error", duration=8)
    })
  })
  
  # Botón confirmar: solo aparece cuando no hay errores
  output$ui_btn_confirmar_masivo <- renderUI({
    df <- rv$df_masivo; req(!is.null(df))
    n_err <- sum(df$.error != "")
    n_ok  <- sum(df$.error == "")
    if (n_ok == 0) return(NULL)
    lbl <- if (n_err > 0)
      paste0("✅ Importar ", n_ok, " fila(s) válidas (omitir ", n_err, " con errores)")
    else
      paste0("✅ Importar todas (", n_ok, " siembras)")
    actionButton("btn_confirmar_masivo", lbl,
                 class="btn-success btn-lg", width="100%")
  })
  
  # Tabla de revisión con errores marcados en rojo + botones por fila
  output$ui_tabla_masivo <- renderUI({
    df <- rv$df_masivo; if (is.null(df)) return(NULL)
    n_err <- sum(df$.error != "")
    n_ok  <- sum(df$.error == "")
    
    tagList(
      hr(class="sep"),
      fluidRow(
        column(12,
               if (n_err > 0)
                 div(class="alerta-roja",
                     icon("triangle-exclamation"),
                     paste0(" ", n_err, " fila(s) con errores — corrígelas o bórralas antes de importar."),
                     br(), em("Las filas en rojo tienen problemas. Haz clic en ✏ para editar o 🗑 para borrar."))
               else
                 div(style="color:#2e7d32; font-weight:bold;",
                     icon("circle-check"),
                     paste0(" Todas las filas son válidas (", n_ok, " siembras listas).")),
               br(),
               DTOutput("tabla_preview_masivo")
        )
      )
    )
  })
  
  output$tabla_preview_masivo <- renderDT({
    df <- rv$df_masivo; req(!is.null(df))
    
    # Columna de acciones HTML
    acciones <- sapply(seq_len(nrow(df)), function(i) {
      paste0(
        '<button class="btn btn-xs btn-warning" ',
        'onclick="Shiny.setInputValue(\'editar_fila_masivo\', ', i,
        ', {priority: \'event\'})">✏ Editar</button> ',
        '<button class="btn btn-xs btn-danger" ',
        'onclick="Shiny.setInputValue(\'borrar_fila_masivo\', ', i,
        ', {priority: \'event\'})">🗑 Borrar</button>'
      )
    })
    
    df_show <- df %>%
      mutate(
        Acciones      = acciones,
        fecha_siembra = ifelse(is.na(fecha_siembra), "❌ INVÁLIDA",
                               format(as.Date(fecha_siembra), "%d/%m/%Y")),
        Estado        = ifelse(.error == "", "✅ OK",
                               paste0("⚠ ", .error))
      ) %>%
      select(Acciones, bloque, cama, producto, variedad,
             fecha_siembra, esquejes, Estado) %>%
      rename(Bloque=bloque, Cama=cama, Producto=producto, Variedad=variedad,
             `Fecha Siembra`=fecha_siembra, Esquejes=esquejes)
    
    es_error <- df$.error != ""
    
    datatable(
      df_show,
      escape   = FALSE,
      rownames = FALSE,
      selection = "none",
      options  = list(
        dom        = 'tip',
        pageLength = 10,
        scrollX    = TRUE,
        columnDefs = list(list(orderable=FALSE, targets=0))
      )
    ) %>%
      formatStyle(
        "Estado",
        backgroundColor = styleEqual(
          c("✅ OK"), c("transparent")
        )
      ) %>%
      formatStyle(
        columns    = names(df_show),
        valueColumns = "Estado",
        backgroundColor = JS(
          "function(value, type, row) {
            if (type === 'display') {
              var estado = row[row.length - 1];
              if (typeof estado === 'string' && estado.indexOf('⚠') >= 0)
                return '#FFEBEE';
            }
            return '';
          }"
        )
      )
  }, server = FALSE)
  
  # Borrar fila directamente desde la tabla
  observeEvent(input$borrar_fila_masivo, {
    idx <- as.integer(input$borrar_fila_masivo)
    req(!is.null(rv$df_masivo), idx >= 1, idx <= nrow(rv$df_masivo))
    rv$df_masivo <- rv$df_masivo[-idx, ]
    showNotification(paste0("🗑 Fila ", idx, " eliminada."), type="warning", duration=2)
  })
  
  # Abrir modal de edición
  observeEvent(input$editar_fila_masivo, {
    idx <- as.integer(input$editar_fila_masivo)
    req(!is.null(rv$df_masivo), idx >= 1, idx <= nrow(rv$df_masivo))
    rv$fila_editar <- idx
    fila <- rv$df_masivo[idx, ]
    
    # Variedades válidas para el producto actual
    vars_prod <- catalogo %>%
      filter(producto == fila$producto) %>%
      pull(variedad) %>% unique() %>% sort()
    
    showModal(modalDialog(
      title = paste0("✏ Editar fila ", idx),
      size  = "m",
      fluidRow(
        column(6, textInput("edit_bloque",   "Bloque",   value=fila$bloque)),
        column(6, textInput("edit_cama",     "Cama",     value=fila$cama))
      ),
      fluidRow(
        column(6,
               selectInput("edit_producto", "Producto",
                           choices  = sort(unique(catalogo$producto)),
                           selected = fila$producto)
        ),
        column(6, uiOutput("ui_edit_variedad"))
      ),
      fluidRow(
        column(6,
               textInput("edit_fecha", "Fecha Siembra (DD/MM/AAAA)",
                         value = ifelse(is.na(fila$fecha_siembra), "",
                                        format(as.Date(fila$fecha_siembra), "%d/%m/%Y")))
        ),
        column(6,
               numericInput("edit_esquejes", "Esquejes",
                            value=fila$esquejes, min=1, step=1)
        )
      ),
      footer = tagList(
        modalButton("Cancelar"),
        actionButton("btn_guardar_edicion", "💾 Guardar cambios", class="btn-success")
      )
    ))
  })
  
  # Variedad dinámica dentro del modal de edición
  output$ui_edit_variedad <- renderUI({
    req(input$edit_producto)
    vars <- catalogo %>% filter(producto == input$edit_producto) %>%
      pull(variedad) %>% unique() %>% sort()
    fila <- if (!is.null(rv$fila_editar)) rv$df_masivo[rv$fila_editar, ] else NULL
    sel  <- if (!is.null(fila) && fila$variedad %in% vars) fila$variedad else vars[1]
    selectInput("edit_variedad", "Variedad", choices=vars, selected=sel)
  })
  
  # Guardar edición y revalidar fila
  observeEvent(input$btn_guardar_edicion, {
    idx <- rv$fila_editar; req(!is.null(idx))
    
    fecha_p <- parsear_fecha(input$edit_fecha)
    
    rv$df_masivo[idx, "bloque"]        <- toupper(trimws(input$edit_bloque))
    rv$df_masivo[idx, "cama"]          <- toupper(trimws(input$edit_cama))
    rv$df_masivo[idx, "producto"]      <- input$edit_producto
    rv$df_masivo[idx, "variedad"]      <- input$edit_variedad
    rv$df_masivo[idx, "fecha_siembra"] <- fecha_p
    rv$df_masivo[idx, "esquejes"]      <- as.numeric(input$edit_esquejes)
    
    # Revalidar
    errs <- errores_fila(rv$df_masivo[idx, ])
    rv$df_masivo[idx, ".error"] <- if (length(errs)==0) "" else paste(errs, collapse=" | ")
    
    removeModal()
    rv$fila_editar <- NULL
    showNotification("💾 Fila actualizada.", type="message", duration=2)
  })
  
  # Confirmar importación (solo filas sin error)
  observeEvent(input$btn_confirmar_masivo, {
    df <- rv$df_masivo; req(!is.null(df))
    df_ok <- df %>% filter(.error == "") %>% select(-".error")
    req(nrow(df_ok) > 0)
    
    df_new <- df_ok %>%
      mutate(id = paste0("M", format(Sys.time(), "%Y%m%d%H%M%S"),
                         "_", seq_len(n())),
             fecha_siembra = as.Date(fecha_siembra)) %>%
      select(id, bloque, cama, producto, variedad, fecha_siembra, esquejes)
    
    rv$registro  <- bind_rows(rv$registro, df_new)
    guardar_registro(rv$registro)
    rv$df_masivo <- NULL
    showNotification(paste0("✅ ", nrow(df_new), " siembras importadas correctamente."),
                     type="message", duration=4)
  })
  
  # ── Mis Registros ─────────────────────────────────────────────────────────
  output$tabla_registro <- renderDT({
    df <- rv$registro
    if (nrow(df) == 0)
      return(datatable(data.frame(Mensaje="No hay siembras registradas aún."),
                       rownames=FALSE))
    df %>% select(-id) %>%
      rename(Bloque=bloque, Cama=cama, Producto=producto, Variedad=variedad,
             `Fecha Siembra`=fecha_siembra, Esquejes=esquejes) %>%
      mutate(`Fecha Siembra`=format(`Fecha Siembra`, "%d/%m/%Y")) %>%
      datatable(selection="single", rownames=TRUE,
                options=list(pageLength=10, scrollX=TRUE,
                             lengthMenu=list(c(10,25,50,-1),c("10","25","50","Todos")),
                             order=list(list(4,"desc"))))
  })
  
  output$ui_borrar_fila <- renderUI({
    req(nrow(rv$registro) > 0)
    actionButton("btn_borrar_fila", "🗑 Borrar fila seleccionada", class="btn-warning")
  })
  
  observeEvent(input$btn_borrar_fila, {
    req(input$tabla_registro_rows_selected)
    rv$registro <- rv$registro[-input$tabla_registro_rows_selected, ]
    guardar_registro(rv$registro)
    showNotification("🗑 Siembra eliminada.", type="warning", duration=3)
  })
  
  observeEvent(input$btn_limpiar_todo, {
    showModal(modalDialog(
      title = "¿Confirmar eliminación?",
      "Esto borrará TODOS los registros permanentemente.",
      footer = tagList(
        modalButton("Cancelar"),
        actionButton("confirmar_limpiar", "Sí, eliminar todo", class="btn-danger")
      )
    ))
  })
  observeEvent(input$confirmar_limpiar, {
    rv$registro <- registro_vacio()
    guardar_registro(rv$registro)
    removeModal()
    showNotification("🗑 Todos los registros eliminados.", type="error", duration=3)
  })
  
  output$btn_descargar <- downloadHandler(
    filename = function() paste0("registro_siembras_", Sys.Date(), ".xlsx"),
    content  = function(file) {
      write_xlsx(rv$registro %>%
                   rename(Bloque=bloque, Cama=cama, Producto=producto,
                          Variedad=variedad, `Fecha Siembra`=fecha_siembra,
                          Esquejes=esquejes) %>% select(-id), file)
    }
  )
  
  # ── Proyecciones (base para estimación y camas cortadas) ──────────────────
  todas_proyecciones <- reactive({
    df <- rv$registro
    if (nrow(df) == 0) return(NULL)
    map_dfr(seq_len(nrow(df)), function(i) proyectar_siembra(df[i,], catalogo))
  })
  
  # ══ TAB 3: RESUMEN DE SIEMBRAS ════════════════════════════════════════════
  # (por semana en que SE SEMBRÓ, no de floración)
  resumen_siembras <- reactive({
    df <- rv$registro
    if (nrow(df) == 0) return(NULL)
    df %>%
      mutate(
        fs          = as.Date(fecha_siembra),
        semana_num  = semana_iso(fs),
        anio        = anio_iso(fs),
        etiqueta_sw = etiqueta_semana(fs)
      ) %>%
      group_by(anio, semana_num, etiqueta_sw, producto, variedad) %>%
      summarise(esquejes_total = sum(esquejes, na.rm=TRUE),
                n_camas        = n(),
                .groups = "drop") %>%
      arrange(anio, semana_num)
  })
  
  output$tabla_resumen_siembra <- renderDT({
    df <- resumen_siembras(); req(!is.null(df), nrow(df)>0)
    df %>%
      rename(Año=anio, `N° Semana`=semana_num, `Semana de Siembra`=etiqueta_sw,
             Producto=producto, Variedad=variedad,
             `Esquejes Sembrados`=esquejes_total, `Camas`=n_camas) %>%
      datatable(rownames=FALSE,
                options=list(pageLength=10, scrollX=TRUE,
                             lengthMenu=list(c(10,25,50,-1),c("10","25","50","Todos")),
                             order=list(list(5,"desc")))) %>%
      formatStyle("Esquejes Sembrados",
                  background=styleColorBar(range(df$esquejes_total), "#a5d6a7"),
                  backgroundSize="90% 70%", backgroundRepeat="no-repeat",
                  backgroundPosition="center")
  })
  
  output$ui_selector_semana_resumen <- renderUI({
    df <- resumen_siembras(); req(!is.null(df), nrow(df)>0)
    semanas <- df %>% arrange(anio, semana_num) %>% pull(etiqueta_sw) %>% unique()
    selectInput("semana_resumen_sel", "Selecciona una semana:",
                choices = semanas, selected = semanas[1], width = "100%")
  })
  
  detalle_semana_sel <- reactive({
    req(input$semana_resumen_sel)
    df <- resumen_siembras(); req(!is.null(df))
    df %>% filter(etiqueta_sw == input$semana_resumen_sel)
  })
  
  output$ui_infoboxes_semana_sel <- renderUI({
    df <- detalle_semana_sel(); req(!is.null(df), nrow(df)>0)
    
    totales_prod <- df %>%
      group_by(producto) %>%
      summarise(esq=sum(esquejes_total), cam=sum(n_camas), .groups="drop") %>%
      arrange(desc(esq))
    
    gran_total <- sum(df$esquejes_total)
    total_camas <- sum(df$n_camas)
    
    tagList(
      fluidRow(
        lapply(seq_len(nrow(totales_prod)), function(i) {
          column(3,
                 div(style="background:#2f8d96;color:white;border-radius:8px;
                        padding:10px 14px;text-align:center;margin-bottom:8px;
                        box-shadow:0 2px 5px rgba(0,0,0,0.15);",
                     div(style="font-size:11px;opacity:.85;text-transform:uppercase;
                          letter-spacing:.5px;", totales_prod$producto[i]),
                     div(style="font-size:22px;font-weight:700;margin:3px 0;",
                         format(totales_prod$esq[i], big.mark=",")),
                     div(style="font-size:11px;opacity:.75;",
                         paste0(totales_prod$cam[i], " cama(s)"))
                 )
          )
        })
      ),
      fluidRow(
        column(4,
               div(style="background:#1a5c63;color:white;border-radius:8px;
                      padding:10px 14px;text-align:center;margin-bottom:8px;
                      box-shadow:0 2px 5px rgba(0,0,0,0.2);",
                   div(style="font-size:11px;opacity:.85;text-transform:uppercase;
                        letter-spacing:.5px;", "TOTAL SEMANA"),
                   div(style="font-size:22px;font-weight:700;margin:3px 0;",
                       format(gran_total, big.mark=",")),
                   div(style="font-size:11px;opacity:.75;",
                       paste0(total_camas, " camas en total"))
               )
        )
      )
    )
  })
  
  output$tabla_detalle_semana <- renderDT({
    df <- detalle_semana_sel(); req(!is.null(df), nrow(df)>0)
    df %>%
      select(producto, variedad, n_camas, esquejes_total) %>%
      rename(Producto=producto, Variedad=variedad,
             Camas=n_camas, `Esquejes Sembrados`=esquejes_total) %>%
      arrange(Producto, Variedad) %>%
      datatable(rownames=FALSE,
                options=list(dom='tip', pageLength=10, scrollX=TRUE,
                             lengthMenu=list(c(10,25,50,-1),c("10","25","50","Todos")),
                             order=list(list(3,"desc")))) %>%
      formatStyle("Esquejes Sembrados",
                  background=styleColorBar(range(df$esquejes_total), "#9dd8dc"),
                  backgroundSize="90% 70%", backgroundRepeat="no-repeat",
                  backgroundPosition="center") %>%
      formatStyle("Producto", fontWeight="bold")
  })
  
  output$grafico_resumen_siembra <- renderPlotly({
    df <- resumen_siembras(); req(!is.null(df), nrow(df)>0)
    df_agg <- df %>%
      group_by(etiqueta_sw, semana_num, anio, producto) %>%
      summarise(esquejes=sum(esquejes_total), .groups="drop") %>%
      arrange(anio, semana_num)
    df_agg$etiqueta_sw <- factor(df_agg$etiqueta_sw, levels=unique(df_agg$etiqueta_sw))
    
    p <- ggplot(df_agg, aes(x=etiqueta_sw, y=esquejes, fill=producto,
                            text=paste0("Semana siembra: ", etiqueta_sw,
                                        "<br>Producto: ", producto,
                                        "<br>Esquejes: ", format(esquejes, big.mark=",")))) +
      geom_col(position="stack") +
      scale_fill_brewer(palette="Set2") +
      labs(x="Semana de siembra", y="Esquejes sembrados", fill="Producto") +
      theme_minimal() +
      theme(axis.text.x=element_text(angle=45, hjust=1, size=8))
    ggplotly(p, tooltip="text")
  })
  
  # ══ TAB 4: ESTIMACIÓN DE COSECHA (proyección de floración) ════════════════
  resumen_floracion <- reactive({
    df <- todas_proyecciones(); req(!is.null(df))
    df %>%
      group_by(anio, semana_num, etiqueta_sw, variedad, producto) %>%
      summarise(tallos_total=sum(tallos, na.rm=TRUE), .groups="drop") %>%
      arrange(anio, semana_num)
  })
  
  output$ui_semanas_disponibles <- renderUI({
    df <- resumen_floracion()
    if (is.null(df) || nrow(df)==0)
      return(p("No hay proyecciones. Agrega siembras primero."))
    semanas <- df %>% arrange(anio, semana_num) %>% pull(etiqueta_sw) %>% unique()
    checkboxGroupInput("semanas_sel", "Semanas de floración:",
                       choices=semanas,
                       selected=semanas[1:min(4, length(semanas))])
  })
  
  estimacion_resultado <- eventReactive(input$btn_estimar, {
    req(input$semanas_sel)
    df <- resumen_floracion(); req(!is.null(df))
    df %>% filter(etiqueta_sw %in% input$semanas_sel) %>%
      arrange(anio, semana_num, producto, variedad)
  })
  
  output$tabla_estimacion <- renderDT({
    df <- estimacion_resultado(); req(nrow(df)>0)
    df %>%
      rename(Semana=etiqueta_sw, Variedad=variedad, Producto=producto,
             `Tallos Estimados`=tallos_total) %>%
      select(Semana, Producto, Variedad, `Tallos Estimados`) %>%
      datatable(rownames=FALSE,
                options=list(pageLength=10, scrollX=TRUE,
                             lengthMenu=list(c(10,25,50,-1),c("10","25","50","Todos")),
                             order=list(list(3,"desc")))) %>%
      formatStyle("Tallos Estimados",
                  background=styleColorBar(range(df$tallos_total), "#81c784"),
                  backgroundSize="90% 70%", backgroundRepeat="no-repeat",
                  backgroundPosition="center")
  })
  
  output$grafico_estimacion <- renderPlotly({
    df <- estimacion_resultado(); req(nrow(df)>0)
    df_plot <- df %>%
      group_by(etiqueta_sw, semana_num, anio, producto) %>%
      summarise(tallos=sum(tallos_total), .groups="drop") %>%
      arrange(anio, semana_num)
    df_plot$etiqueta_sw <- factor(df_plot$etiqueta_sw, levels=unique(df_plot$etiqueta_sw))
    
    p <- ggplot(df_plot,
                aes(x=etiqueta_sw, y=tallos, fill=producto,
                    text=paste0(etiqueta_sw, "\n",
                                producto, ": ", format(tallos, big.mark=","), " tallos"))) +
      geom_col(position="stack", width=0.6) +
      geom_text(aes(label=format(tallos, big.mark=",")),
                position=position_stack(vjust=0.5),
                size=3, color="white", fontface="bold") +
      scale_fill_brewer(palette="Set2") +
      labs(title="Estimación de Tallos por Semana de Floración",
           x="Semana de floración", y="Tallos", fill="Producto") +
      theme_minimal() +
      theme(axis.text.x=element_text(angle=30, hjust=1))
    ggplotly(p, tooltip="text")
  })
  
  # ── Detalle diario por variedad ───────────────────────────────────────────
  
  # Selector de semana: todas las semanas con proyecciones
  output$ui_semana_detalle_dia <- renderUI({
    df <- resumen_floracion()
    if (is.null(df) || nrow(df) == 0) return(p("Sin datos aún."))
    semanas <- df %>% arrange(anio, semana_num) %>% pull(etiqueta_sw) %>% unique()
    selectInput("semana_dia_sel", "Semana:", choices = semanas, width = "100%")
  })
  
  # Selector de producto: filtrado por semana
  output$ui_producto_detalle_dia <- renderUI({
    req(input$semana_dia_sel)
    df <- todas_proyecciones(); req(!is.null(df))
    prods <- df %>%
      filter(etiqueta_sw == input$semana_dia_sel) %>%
      pull(producto) %>% unique() %>% sort()
    pickerInput(
      "producto_dia_sel", "Producto:",
      choices  = prods,
      selected = prods,
      multiple = TRUE,
      options  = list(
        `actions-box`         = TRUE,
        `select-all-text`     = "Todos",
        `deselect-all-text`   = "Ninguno",
        `selected-text-format`= "count > 1",
        `count-selected-text` = "{0} productos",
        size = 8
      ),
      width = "100%"
    )
  })
  
  # Selector de variedad: filtrado por semana + productos seleccionados, multiselect
  output$ui_variedad_detalle_dia <- renderUI({
    req(input$semana_dia_sel, input$producto_dia_sel)
    df <- todas_proyecciones(); req(!is.null(df))
    vars <- df %>%
      filter(etiqueta_sw == input$semana_dia_sel,
             producto    %in% input$producto_dia_sel) %>%
      pull(variedad) %>% unique() %>% sort()
    pickerInput(
      "variedad_dia_sel", "Variedad:",
      choices  = vars,
      selected = vars,
      multiple = TRUE,
      options  = list(
        `actions-box`         = TRUE,
        `select-all-text`     = "Todas",
        `deselect-all-text`   = "Ninguna",
        `selected-text-format`= "count > 1",
        `count-selected-text` = "{0} variedades",
        `live-search`         = TRUE,
        size = 10
      ),
      width = "100%"
    )
  })
  
  # Datos filtrados (1 fila = 1 cama × 1 día)
  detalle_dia_data <- eventReactive(input$btn_ver_detalle_dia, {
    req(input$semana_dia_sel,
        length(input$producto_dia_sel) > 0,
        length(input$variedad_dia_sel) > 0)
    df <- todas_proyecciones(); req(!is.null(df))
    df %>%
      filter(etiqueta_sw == input$semana_dia_sel,
             producto    %in% input$producto_dia_sel,
             variedad    %in% input$variedad_dia_sel) %>%
      arrange(fecha_corte, bloque, cama)
  })
  
  # Tarjetas lun–dom con total de tallos
  output$ui_cards_dias <- renderUI({
    df <- detalle_dia_data()
    req(!is.null(df), nrow(df) > 0)
    
    dias_es <- c("Lunes","Martes","Miércoles","Jueves","Viernes","Sábado","Domingo")
    lun     <- lunes_semana(min(df$fecha_corte))
    
    tarjetas <- lapply(0:6, function(i) {
      f     <- lun + i
      sub   <- df %>% filter(fecha_corte == f)
      total   <- sum(sub$tallos, na.rm = TRUE)
      n_camas <- nrow(sub)
      bg    <- if (total > 0) "#2f8d96" else "#b0bec5"
      bg_dk <- if (total > 0) "#1a5c63" else "#90a4ae"
      
      div(
        style = paste0(
          "background:", bg, ";color:white;border-radius:10px;",
          "padding:12px 8px;text-align:center;margin:0 5px 10px 5px;",
          "flex:1;min-width:120px;max-width:160px;",
          "box-shadow:0 2px 6px rgba(0,0,0,0.18);"
        ),
        div(style = "font-size:12px;font-weight:700;text-transform:uppercase;
                     letter-spacing:.4px;opacity:.9;", dias_es[i + 1]),
        div(style = "font-size:11px;opacity:.8;margin:2px 0;", format(f, "%d/%m/%Y")),
        div(style = paste0("font-size:24px;font-weight:800;margin:6px 0;",
                           "background:", bg_dk, ";border-radius:6px;padding:4px;"),
            if (total > 0) format(total, big.mark = ",") else "—"),
        div(style = "font-size:11px;opacity:.8;",
            if (n_camas > 0) paste0(n_camas, " cama(s)") else "sin corte")
      )
    })
    
    div(style = "display:flex; flex-wrap:wrap; gap:6px; padding:4px 0;",
        tagList(tarjetas))
  })
  
  # Tabla detallada incluyendo variedad y producto cuando hay selección múltiple
  output$tabla_detalle_dia <- renderDT({
    df <- detalle_dia_data()
    req(!is.null(df), nrow(df) > 0)
    
    dias_es  <- c("Lunes","Martes","Miércoles","Jueves","Viernes","Sábado","Domingo")
    multi_var  <- length(unique(df$variedad))  > 1
    multi_prod <- length(unique(df$producto)) > 1
    
    df_show <- df %>%
      mutate(
        dia_semana = dias_es[wday(fecha_corte, week_start = 1)],
        fecha_fmt  = format(fecha_corte, "%d/%m/%Y"),
        pct_fmt    = scales::percent(pct, accuracy = 1)
      )
    
    # Columnas base; insertar producto y/o variedad si hay múltiples
    cols_sel <- c("dia_semana","fecha_fmt")
    if (multi_prod) cols_sel <- c(cols_sel, "producto")
    if (multi_var)  cols_sel <- c(cols_sel, "variedad")
    cols_sel <- c(cols_sel, "bloque","cama","pct_fmt","tallos")
    
    df_show <- df_show %>% select(all_of(cols_sel))
    
    rename_map <- c(
      dia_semana = "Día",    fecha_fmt  = "Fecha",
      producto   = "Producto", variedad = "Variedad",
      bloque     = "Bloque", cama       = "Cama",
      pct_fmt    = "% Corte", tallos    = "Tallos"
    )
    names(df_show) <- rename_map[names(df_show)]
    
    col_tallos <- which(names(df_show) == "Tallos") - 1  # 0-indexed para DT
    
    datatable(
      df_show,
      rownames = FALSE,
      options  = list(
        pageLength = 10,
        scrollX    = TRUE,
        lengthMenu = list(c(10,25,50,-1), c("10","25","50","Todos")),
        order      = list(list(1,"asc"), list(col_tallos,"desc")),
        columnDefs = list(list(className="dt-center", targets="_all"))
      )
    ) %>%
      formatStyle("Día",
                  fontWeight = "bold", color = "white",
                  backgroundColor = styleEqual(
                    c("Lunes","Martes","Miércoles","Jueves","Viernes","Sábado","Domingo"),
                    rep("#2f8d96", 7)
                  )) %>%
      formatStyle("Tallos",
                  background         = styleColorBar(range(df$tallos), "#9dd8dc"),
                  backgroundSize     = "90% 70%",
                  backgroundRepeat   = "no-repeat",
                  backgroundPosition = "center")
  })
  
  ultimo_corte_por_siembra <- reactive({
    df <- todas_proyecciones()
    if (is.null(df)) return(NULL)
    df %>%
      group_by(id_siembra) %>%
      summarise(ultimo_corte=max(fecha_corte), .groups="drop")
  })
  
  siembras_cortadas <- reactive({
    ult <- ultimo_corte_por_siembra()
    req(!is.null(ult))
    fecha_ref  <- as.Date(input$fecha_ref_cortadas)
    ids_cortadas <- ult %>% filter(ultimo_corte < fecha_ref) %>% pull(id_siembra)
    rv$registro %>%
      filter(id %in% ids_cortadas) %>%
      left_join(ult, by=c("id"="id_siembra")) %>%
      arrange(ultimo_corte)
  })
  
  output$titulo_tabla_cortadas <- renderUI({
    df <- siembras_cortadas()
    n  <- if (is.null(df)) 0 else nrow(df)
    if (n == 0)
      span("✅ No hay camas cortadas antes de la fecha de referencia.")
    else
      span(class="alerta-roja",
           paste0("⚠ ", n, " cama(s) ya cortadas — pendientes de archivar/eliminar"))
  })
  
  output$tabla_cortadas <- renderDT({
    df <- siembras_cortadas()
    if (is.null(df) || nrow(df)==0)
      return(datatable(data.frame(Mensaje="Sin camas cortadas para la fecha indicada."),
                       rownames=FALSE))
    df %>% select(-id) %>%
      rename(Bloque=bloque, Cama=cama, Producto=producto, Variedad=variedad,
             `Fecha Siembra`=fecha_siembra, Esquejes=esquejes,
             `Último Corte`=ultimo_corte) %>%
      mutate(`Fecha Siembra`=format(`Fecha Siembra`, "%d/%m/%Y"),
             `Último Corte` =format(`Último Corte`,  "%d/%m/%Y")) %>%
      datatable(rownames=FALSE,
                options=list(pageLength=10, scrollX=TRUE,
                             lengthMenu=list(c(10,25,50,-1),c("10","25","50","Todos")),
                             order=list(list(6,"asc")))) %>%
      formatStyle("Último Corte", color="#c62828", fontWeight="bold")
  })
  
  output$btn_descargar_cortadas <- downloadHandler(
    filename = function() paste0("camas_cortadas_", Sys.Date(), ".xlsx"),
    content  = function(file) {
      df <- siembras_cortadas(); req(!is.null(df), nrow(df)>0)
      write_xlsx(df %>% select(-id) %>%
                   rename(Bloque=bloque, Cama=cama, Producto=producto,
                          Variedad=variedad, `Fecha Siembra`=fecha_siembra,
                          Esquejes=esquejes, `Último Corte`=ultimo_corte), file)
    }
  )
  
  observeEvent(input$btn_archivar_cortadas, {
    df <- siembras_cortadas(); req(!is.null(df), nrow(df)>0)
    arch <- paste0("archivo_cortadas_", format(Sys.Date(), "%Y%m%d"), ".rds")
    prev <- if (file.exists(arch)) readRDS(arch) else registro_vacio()
    saveRDS(bind_rows(prev, df %>% select(-ultimo_corte)), arch)
    showNotification(paste0("📦 ", nrow(df), " camas archivadas en '", arch, "'"),
                     type="message", duration=5)
  })
  
  observeEvent(input$btn_eliminar_cortadas, {
    df <- siembras_cortadas(); req(!is.null(df), nrow(df)>0)
    showModal(modalDialog(
      title = "¿Eliminar camas cortadas del registro activo?",
      paste0("Se eliminarán ", nrow(df), " siembras. ",
             "Usa 'Archivar' primero si quieres conservar un respaldo."),
      footer = tagList(
        modalButton("Cancelar"),
        actionButton("confirmar_eliminar_cortadas",
                     "Sí, eliminar", class="btn-danger")
      )
    ))
  })
  
  observeEvent(input$confirmar_eliminar_cortadas, {
    ids_borrar <- siembras_cortadas()$id
    rv$registro <- rv$registro %>% filter(!id %in% ids_borrar)
    guardar_registro(rv$registro)
    removeModal()
    showNotification(paste0("🗑 ", length(ids_borrar),
                            " camas eliminadas del registro activo."),
                     type="warning", duration=4)
  })
  # ── Infoboxes resumen de siembras (totales por producto) ─────────────────
  output$ui_infoboxes_resumen <- renderUI({
    df <- rv$registro
    if (is.null(df) || nrow(df) == 0) return(NULL)
    
    totales <- df %>%
      group_by(producto) %>%
      summarise(total = sum(esquejes, na.rm=TRUE), .groups="drop") %>%
      arrange(desc(total))
    
    iconos <- c("P.AMERICANO"="leaf", "PREMIUM"="star", "SANTINI"="seedling",
                "DESBOTONADO"="scissors", "BUQUETERA"="spa", "MUESTREO"="flask")
    colores <- c("aqua","light-blue","teal","olive","purple","maroon","navy","orange")
    
    boxes <- lapply(seq_len(nrow(totales)), function(i) {
      prod  <- totales$producto[i]
      total <- format(totales$total[i], big.mark=",")
      icono <- if (!is.null(iconos[[prod]])) iconos[[prod]] else "box"
      color <- colores[((i-1) %% length(colores)) + 1]
      column(2,
             div(style=paste0(
               "background:#2f8d96; color:white; border-radius:8px; ",
               "padding:14px 16px; margin-bottom:12px; text-align:center;",
               "box-shadow:0 2px 6px rgba(0,0,0,0.15);"
             ),
             div(style="font-size:12px; opacity:0.85; text-transform:uppercase;
                     letter-spacing:0.5px;", prod),
             div(style="font-size:26px; font-weight:700; margin:4px 0;", total),
             div(style="font-size:11px; opacity:0.75;", "esquejes sembrados")
             )
      )
    })
    
    # Tarjeta de gran total
    gran_total <- format(sum(df$esquejes, na.rm=TRUE), big.mark=",")
    boxes_con_total <- c(boxes, list(
      column(2,
             div(style=paste0(
               "background:#1a5c63; color:white; border-radius:8px; ",
               "padding:14px 16px; margin-bottom:12px; text-align:center;",
               "box-shadow:0 2px 8px rgba(0,0,0,0.2);"
             ),
             div(style="font-size:12px; opacity:0.85; text-transform:uppercase;
                     letter-spacing:0.5px;", "TOTAL GENERAL"),
             div(style="font-size:26px; font-weight:700; margin:4px 0;", gran_total),
             div(style="font-size:11px; opacity:0.75;", "todos los productos")
             )
      )
    ))
    
    do.call(fluidRow, boxes_con_total)
  })
  
  # ── Infoboxes estimación de cosecha (totales por producto) ───────────────
  output$ui_infoboxes_estimacion <- renderUI({
    df <- estimacion_resultado()
    if (is.null(df) || nrow(df) == 0) return(NULL)
    
    totales <- df %>%
      group_by(producto) %>%
      summarise(total = sum(tallos_total, na.rm=TRUE), .groups="drop") %>%
      arrange(desc(total))
    
    boxes <- lapply(seq_len(nrow(totales)), function(i) {
      prod  <- totales$producto[i]
      total <- format(totales$total[i], big.mark=",")
      column(2,
             div(style=paste0(
               "background:#4ab8c1; color:white; border-radius:8px; ",
               "padding:14px 16px; margin-bottom:12px; text-align:center;",
               "box-shadow:0 2px 6px rgba(0,0,0,0.15);"
             ),
             div(style="font-size:12px; opacity:0.9; text-transform:uppercase;
                     letter-spacing:0.5px;", prod),
             div(style="font-size:26px; font-weight:700; margin:4px 0;", total),
             div(style="font-size:11px; opacity:0.8;", "tallos estimados")
             )
      )
    })
    
    gran_total <- format(sum(df$tallos_total, na.rm=TRUE), big.mark=",")
    boxes_con_total <- c(boxes, list(
      column(2,
             div(style=paste0(
               "background:#267880; color:white; border-radius:8px; ",
               "padding:14px 16px; margin-bottom:12px; text-align:center;",
               "box-shadow:0 2px 8px rgba(0,0,0,0.2);"
             ),
             div(style="font-size:12px; opacity:0.85; text-transform:uppercase;
                     letter-spacing:0.5px;", "TOTAL GENERAL"),
             div(style="font-size:26px; font-weight:700; margin:4px 0;", gran_total),
             div(style="font-size:11px; opacity:0.75;", "todos los productos")
             )
      )
    ))
    
    do.call(fluidRow, boxes_con_total)
  })
  
}

shinyApp(ui, server)