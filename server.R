# ============================================================
# server.R  –  Logique serveur – Système de suivi patients
# ============================================================

server <- function(input, output, session) {

  # ── Initialiser les listes déroulantes ───────────────────
  observe({
    updateSelectInput(session, "c_patient",    choices = get_patients_choices())
    updateSelectInput(session, "c_medecin",    choices = get_medecins_choices())
    updateSelectInput(session, "c_medic",      choices = c("Aucun" = 0,
                                                           get_medicaments_choices()))
    updateSelectInput(session, "hist_patient", choices = get_patients_choices())
  })

  # ── Valeurs réactives globales ────────────────────────────
  refresh <- reactiveVal(0)   # incrément pour forcer le rechargement

  # ============================================================
  # ACCUEIL – Boîtes de comptage
  # ============================================================
  output$box_patients <- renderValueBox({
    refresh()
    n <- db_query("SELECT COUNT(*) AS n FROM patients")$n
    valueBox(n, "Patients enregistrés", icon = icon("user"), color = "blue")
  })
  output$box_consults <- renderValueBox({
    refresh()
    n <- db_query("SELECT COUNT(*) AS n FROM consultations")$n
    valueBox(n, "Consultations", icon = icon("stethoscope"), color = "green")
  })
  output$box_medecins <- renderValueBox({
    refresh()
    n <- db_query("SELECT COUNT(*) AS n FROM medecins")$n
    valueBox(n, "Médecins", icon = icon("user-md"), color = "orange")
  })
  output$box_medics <- renderValueBox({
    refresh()
    n <- db_query("SELECT COUNT(*) AS n FROM medicaments")$n
    valueBox(n, "Médicaments", icon = icon("pills"), color = "purple")
  })

  # Graphique consultations par mois
  output$plot_mois <- renderPlotly({
    refresh()
    data <- db_query("
      SELECT strftime('%Y-%m', date_consult) AS mois,
             COUNT(*) AS nb
      FROM consultations
      GROUP BY mois
      ORDER BY mois")
    plot_ly(data, x = ~mois, y = ~nb, type = "bar",
            marker = list(color = "#3c8dbc")) |>
      layout(xaxis = list(title = "Mois"),
             yaxis = list(title = "Consultations"),
             plot_bgcolor  = "transparent",
             paper_bgcolor = "transparent")
  })

  # Top diagnostics (accueil)
  output$plot_top_diag <- renderPlotly({
    refresh()
    data <- db_query("
      SELECT diagnostic, COUNT(*) AS nb
      FROM consultations
      WHERE diagnostic IS NOT NULL
      GROUP BY diagnostic
      ORDER BY nb DESC
      LIMIT 5")
    plot_ly(data, labels = ~diagnostic, values = ~nb, type = "pie") |>
      layout(showlegend = TRUE,
             plot_bgcolor  = "transparent",
             paper_bgcolor = "transparent")
  })

  # Dernières consultations
  output$tbl_recent <- renderDT({
    refresh()
    db_query("
      SELECT c.date_consult AS Date,
             p.nom || ' ' || p.prenom AS Patient,
             m.nom || ' ' || m.prenom AS Médecin,
             c.diagnostic             AS Diagnostic
      FROM consultations c
      JOIN patients p ON p.id_patient = c.id_patient
      JOIN medecins m ON m.id_medecin = c.id_medecin
      ORDER BY c.date_consult DESC
      LIMIT 10") |>
      datatable(options = list(dom = "t", paging = FALSE),
                rownames = FALSE)
  })

  # ============================================================
  # AJOUTER PATIENT
  # ============================================================
  output$tbl_patients_recent <- renderDT({
    refresh()
    db_query("SELECT nom, prenom, sexe, telephone, created_at AS 'Ajouté le'
              FROM patients ORDER BY id_patient DESC LIMIT 8") |>
      datatable(options = list(dom = "t", paging = FALSE), rownames = FALSE)
  })

  observeEvent(input$btn_add_patient, {
    req(input$p_nom, input$p_prenom)
    tryCatch({
      db_execute(
        "INSERT INTO patients (nom, prenom, date_nais, sexe, telephone, adresse, groupe_sang)
         VALUES (?, ?, ?, ?, ?, ?, ?)",
        params = list(
          toupper(trimws(input$p_nom)),
          trimws(input$p_prenom),
          as.character(input$p_dob),
          input$p_sexe,
          input$p_tel,
          input$p_adresse,
          input$p_sang
        )
      )
      output$msg_patient <- renderText("✅ Patient enregistré avec succès !")
      refresh(refresh() + 1)
      updateSelectInput(session, "c_patient",    choices = get_patients_choices())
      updateSelectInput(session, "hist_patient", choices = get_patients_choices())
      # Vider les champs
      updateTextInput(session, "p_nom",     value = "")
      updateTextInput(session, "p_prenom",  value = "")
      updateTextInput(session, "p_tel",     value = "")
      updateTextInput(session, "p_adresse", value = "")
    }, error = function(e) {
      output$msg_patient <- renderText(paste("❌ Erreur :", e$message))
    })
  })

  # ============================================================
  # CONSULTATION
  # ============================================================
  observeEvent(input$btn_add_consult, {
    req(input$c_patient, input$c_medecin, input$c_diag)
    tryCatch({
      # Insérer la consultation
      db_execute(
        "INSERT INTO consultations
           (id_patient, id_medecin, date_consult, diagnostic,
            observations, tension, temperature, poids)
         VALUES (?, ?, ?, ?, ?, ?, ?, ?)",
        params = list(
          as.integer(input$c_patient),
          as.integer(input$c_medecin),
          as.character(input$c_date),
          input$c_diag,
          input$c_obs,
          input$c_tension,
          input$c_temp,
          input$c_poids
        )
      )
      # Récupérer l'id de la consultation créée
      last_id <- db_query("SELECT MAX(id_consult) AS id FROM consultations")$id

      # Prescription si médicament sélectionné
      if (!is.null(input$c_medic) && as.integer(input$c_medic) > 0 &&
          nchar(trimws(input$c_posologie)) > 0) {
        db_execute(
          "INSERT INTO prescriptions (id_consult, id_medicament, posologie, duree_jours)
           VALUES (?, ?, ?, ?)",
          params = list(
            last_id,
            as.integer(input$c_medic),
            input$c_posologie,
            as.integer(input$c_duree)
          )
        )
      }
      output$msg_consult <- renderText("✅ Consultation enregistrée !")
      refresh(refresh() + 1)
    }, error = function(e) {
      output$msg_consult <- renderText(paste("❌ Erreur :", e$message))
    })
  })

  # Historique d'un patient
  output$tbl_hist <- renderDT({
    req(input$hist_patient)
    refresh()
    db_query("
      SELECT c.date_consult            AS Date,
             c.diagnostic              AS Diagnostic,
             c.temperature             AS 'T°',
             c.tension                 AS Tension,
             m.nom || ' ' || m.prenom  AS Médecin,
             GROUP_CONCAT(med.nom, ', ') AS Médicaments
      FROM consultations c
      JOIN medecins m ON m.id_medecin = c.id_medecin
      LEFT JOIN prescriptions pr ON pr.id_consult = c.id_consult
      LEFT JOIN medicaments med ON med.id_medicament = pr.id_medicament
      WHERE c.id_patient = ?
      GROUP BY c.id_consult
      ORDER BY c.date_consult DESC",
      params = list(as.integer(input$hist_patient))
    ) |> datatable(options = list(pageLength = 5), rownames = FALSE)
  })

  # ============================================================
  # LISTE PATIENTS
  # ============================================================
  output$tbl_all_patients <- renderDT({
    input$btn_search
    refresh()
    q <- isolate(input$search_patient)
    if (is.null(q) || nchar(trimws(q)) == 0) {
      sql <- "SELECT id_patient AS ID, nom AS Nom, prenom AS Prénom,
                     date_nais AS 'Naissance', sexe AS Sexe,
                     telephone AS Téléphone, groupe_sang AS 'Groupe sg.'
              FROM patients ORDER BY nom"
      db_query(sql)
    } else {
      db_query("
        SELECT id_patient AS ID, nom AS Nom, prenom AS Prénom,
               date_nais AS 'Naissance', sexe AS Sexe,
               telephone AS Téléphone, groupe_sang AS 'Groupe sg.'
        FROM patients
        WHERE nom LIKE ? OR prenom LIKE ?
        ORDER BY nom",
        params = list(paste0("%", q, "%"), paste0("%", q, "%"))
      )
    }
  }, options = list(pageLength = 10), rownames = FALSE)

  # ============================================================
  # STATISTIQUES
  # ============================================================

  # Cas par maladie (barres horizontales)
  output$plot_diag <- renderPlotly({
    refresh()
    data <- db_query("
      SELECT diagnostic, COUNT(*) AS nb
      FROM consultations
      WHERE diagnostic IS NOT NULL AND diagnostic != ''
      GROUP BY diagnostic
      ORDER BY nb DESC")
    plot_ly(data,
            x = ~nb, y = ~reorder(diagnostic, nb),
            type = "bar", orientation = "h",
            marker = list(color = "#e74c3c")) |>
      layout(yaxis = list(title = ""),
             xaxis = list(title = "Nombre de cas"),
             plot_bgcolor  = "transparent",
             paper_bgcolor = "transparent")
  })

  # Répartition par sexe
  output$plot_sexe <- renderPlotly({
    refresh()
    data <- db_query("
      SELECT CASE sexe WHEN 'M' THEN 'Masculin'
                       WHEN 'F' THEN 'Féminin'
                       ELSE sexe END AS Sexe,
             COUNT(*) AS nb
      FROM patients GROUP BY sexe")
    plot_ly(data, labels = ~Sexe, values = ~nb, type = "pie",
            marker = list(colors = c("#3498db", "#e91e63", "#9c27b0"))) |>
      layout(plot_bgcolor = "transparent", paper_bgcolor = "transparent")
  })

  # Température moyenne par diagnostic
  output$plot_temp <- renderPlotly({
    refresh()
    data <- db_query("
      SELECT diagnostic,
             ROUND(AVG(temperature), 1) AS temp_moy,
             COUNT(*)                   AS nb
      FROM consultations
      WHERE temperature IS NOT NULL AND diagnostic IS NOT NULL
      GROUP BY diagnostic
      ORDER BY temp_moy DESC")
    plot_ly(data, x = ~diagnostic, y = ~temp_moy,
            type = "scatter", mode = "markers+lines",
            marker = list(size = ~nb * 3, color = "#f39c12")) |>
      layout(xaxis = list(title = "", tickangle = -30),
             yaxis = list(title = "T° moy. (°C)", range = c(36, 40)),
             plot_bgcolor  = "transparent",
             paper_bgcolor = "transparent")
  })

  # Consultations par mois (stats)
  output$plot_mois2 <- renderPlotly({
    refresh()
    data <- db_query("
      SELECT strftime('%Y-%m', date_consult) AS mois,
             COUNT(*) AS nb
      FROM consultations
      GROUP BY mois ORDER BY mois")
    plot_ly(data, x = ~mois, y = ~nb, type = "scatter",
            mode = "lines+markers",
            line = list(color = "#27ae60", width = 2)) |>
      layout(xaxis = list(title = "Mois"),
             yaxis = list(title = "Consultations"),
             plot_bgcolor  = "transparent",
             paper_bgcolor = "transparent")
  })

  # Médicaments les plus prescrits
  output$plot_medic <- renderPlotly({
    refresh()
    data <- db_query("
      SELECT m.nom AS medicament, COUNT(*) AS nb
      FROM prescriptions pr
      JOIN medicaments m ON m.id_medicament = pr.id_medicament
      GROUP BY m.id_medicament
      ORDER BY nb DESC")
    plot_ly(data, x = ~reorder(medicament, nb), y = ~nb,
            type = "bar",
            marker = list(color = "#8e44ad")) |>
      layout(xaxis = list(title = ""),
             yaxis = list(title = "Prescriptions"),
             plot_bgcolor  = "transparent",
             paper_bgcolor = "transparent")
  })

}
