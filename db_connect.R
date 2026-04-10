# ============================================================
# db_connect.R  –  Fonctions utilitaires de connexion SQLite
# ============================================================

library(DBI)
library(RSQLite)

# L'appli Shiny tourne dans shiny/ → la base est dans ../sql/
DB_PATH <- file.path("..", "sql", "sante.db")

get_con <- function() {
  if (!file.exists(DB_PATH)) {
    stop(paste0("Base introuvable : ", DB_PATH,
                "\n→ Lancez sql/init_db.R depuis la racine du projet."))
  }
  dbConnect(RSQLite::SQLite(), DB_PATH)
}

db_query <- function(sql, params = NULL) {
  con <- get_con(); on.exit(dbDisconnect(con))
  if (is.null(params)) dbGetQuery(con, sql)
  else dbGetQuery(con, sql, params = params)
}

db_execute <- function(sql, params = NULL) {
  con <- get_con(); on.exit(dbDisconnect(con))
  if (is.null(params)) dbExecute(con, sql)
  else dbExecute(con, sql, params = params)
}

get_medecins_choices <- function() {
  df <- db_query("SELECT id_medecin, nom || ' ' || prenom AS label FROM medecins ORDER BY nom")
  setNames(df$id_medecin, df$label)
}

get_medicaments_choices <- function() {
  df <- db_query("SELECT id_medicament, nom || ' (' || dosage || ')' AS label FROM medicaments ORDER BY nom")
  setNames(df$id_medicament, df$label)
}

get_patients_choices <- function() {
  df <- db_query("SELECT id_patient, nom || ' ' || prenom AS label FROM patients ORDER BY nom")
  setNames(df$id_patient, df$label)
}
