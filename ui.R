# ============================================================
# ui.R  –  Interface Shiny – Système de suivi patients
# ============================================================

ui <- dashboardPage(
  skin = "blue",

  # ── En-tête ─────────────────────────────────────────────
  dashboardHeader(
    title = tags$span(
      tags$img(src = "logo.png", height = "30px", style = "margin-right:8px;"),
      "MediTrack"
    ),
    titleWidth = 220
  ),

  # ── Sidebar ──────────────────────────────────────────────
  dashboardSidebar(
    width = 220,
    sidebarMenu(
      id = "tabs",
      menuItem("🏠 Accueil",         tabName = "accueil",    icon = icon("home")),
      menuItem("👤 Ajouter patient", tabName = "add_patient",icon = icon("user-plus")),
      menuItem("🏥 Consultation",    tabName = "consult",    icon = icon("stethoscope")),
      menuItem("📋 Liste patients",  tabName = "liste",      icon = icon("list")),
      menuItem("📊 Statistiques",    tabName = "stats",      icon = icon("chart-bar"))
    )
  ),

  # ── Corps ────────────────────────────────────────────────
  dashboardBody(
    useShinyjs(),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
    ),

    tabItems(

      # ===== ACCUEIL =========================================
      tabItem("accueil",
        fluidRow(
          valueBoxOutput("box_patients",  width = 3),
          valueBoxOutput("box_consults",  width = 3),
          valueBoxOutput("box_medecins",  width = 3),
          valueBoxOutput("box_medics",    width = 3)
        ),
        fluidRow(
          box(title = "📈 Consultations par mois", width = 8,
              status = "primary", solidHeader = TRUE,
              plotlyOutput("plot_mois", height = "300px")),
          box(title = "🦠 Top maladies", width = 4,
              status = "warning", solidHeader = TRUE,
              plotlyOutput("plot_top_diag", height = "300px"))
        ),
        fluidRow(
          box(title = "🕐 Dernières consultations", width = 12,
              status = "info", solidHeader = TRUE,
              DTOutput("tbl_recent"))
        )
      ),

      # ===== AJOUTER PATIENT ================================
      tabItem("add_patient",
        fluidRow(
          box(title = "➕ Nouveau patient", width = 6,
              status = "success", solidHeader = TRUE,
              textInput("p_nom",     "Nom *"),
              textInput("p_prenom",  "Prénom *"),
              dateInput( "p_dob",    "Date de naissance",
                         value = Sys.Date() - 365*30,
                         format = "dd/mm/yyyy"),
              selectInput("p_sexe",  "Sexe",
                          choices = c("Masculin" = "M",
                                      "Féminin"  = "F",
                                      "Autre"    = "Autre")),
              textInput("p_tel",     "Téléphone"),
              textInput("p_adresse", "Adresse"),
              selectInput("p_sang",  "Groupe sanguin",
                          choices = c("","A+","A-","B+","B-","O+","O-","AB+","AB-")),
              actionButton("btn_add_patient", "💾 Enregistrer",
                           class = "btn-success btn-lg"),
              br(), br(),
              verbatimTextOutput("msg_patient")
          ),
          box(title = "📄 Patients récents", width = 6,
              status = "info", solidHeader = TRUE,
              DTOutput("tbl_patients_recent"))
        )
      ),

      # ===== CONSULTATION ====================================
      tabItem("consult",
        fluidRow(
          box(title = "🏥 Nouvelle consultation", width = 7,
              status = "primary", solidHeader = TRUE,
              selectInput("c_patient",   "Patient *",
                          choices = NULL),
              selectInput("c_medecin",   "Médecin *",
                          choices = NULL),
              dateInput(  "c_date",      "Date",
                          value = Sys.Date(), format = "dd/mm/yyyy"),
              textInput(  "c_diag",      "Diagnostic"),
              textAreaInput("c_obs",     "Observations", rows = 3),
              fluidRow(
                column(4, textInput("c_tension", "Tension (ex: 12/8)")),
                column(4, numericInput("c_temp", "Temp. (°C)",
                                       value = 37, min = 35, max = 42, step = 0.1)),
                column(4, numericInput("c_poids","Poids (kg)",
                                       value = 70, min = 1,  max = 200))
              ),
              hr(),
              h4("💊 Prescription (optionnel)"),
              selectInput("c_medic",    "Médicament",
                          choices = NULL),
              fluidRow(
                column(6, textInput("c_posologie", "Posologie")),
                column(6, numericInput("c_duree",  "Durée (jours)",
                                       value = 7, min = 1, max = 365))
              ),
              actionButton("btn_add_consult", "💾 Enregistrer la consultation",
                           class = "btn-primary btn-lg"),
              br(), br(),
              verbatimTextOutput("msg_consult")
          ),
          box(title = "🔍 Historique patient", width = 5,
              status = "warning", solidHeader = TRUE,
              selectInput("hist_patient", "Choisir un patient",
                          choices = NULL),
              DTOutput("tbl_hist")
          )
        )
      ),

      # ===== LISTE PATIENTS ==================================
      tabItem("liste",
        fluidRow(
          box(title = "👥 Tous les patients", width = 12,
              status = "info", solidHeader = TRUE,
              fluidRow(
                column(4, textInput("search_patient", "🔍 Recherche",
                                    placeholder = "Nom, prénom…")),
                column(2, br(),
                       actionButton("btn_search", "Rechercher",
                                    class = "btn-info"))
              ),
              DTOutput("tbl_all_patients")
          )
        )
      ),

      # ===== STATISTIQUES ====================================
      tabItem("stats",
        fluidRow(
          box(title = "🦠 Cas par maladie", width = 6,
              status = "danger", solidHeader = TRUE,
              plotlyOutput("plot_diag", height = "350px")),
          box(title = "⚖️ Répartition par sexe", width = 6,
              status = "success", solidHeader = TRUE,
              plotlyOutput("plot_sexe", height = "350px"))
        ),
        fluidRow(
          box(title = "🌡️ Température moyenne par diagnostic", width = 6,
              status = "warning", solidHeader = TRUE,
              plotlyOutput("plot_temp", height = "350px")),
          box(title = "📅 Consultations par mois", width = 6,
              status = "primary", solidHeader = TRUE,
              plotlyOutput("plot_mois2", height = "350px"))
        ),
        fluidRow(
          box(title = "💊 Médicaments les plus prescrits", width = 12,
              status = "info", solidHeader = TRUE,
              plotlyOutput("plot_medic", height = "300px"))
        )
      )

    ) # fin tabItems
  ) # fin dashboardBody
)
