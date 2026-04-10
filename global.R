# ============================================================
# global.R  –  Chargement des packages et helpers communs
# ============================================================

# Packages – installation automatique si absents
pkgs <- c("shiny", "shinydashboard", "DBI", "RSQLite",
          "DT", "ggplot2", "dplyr", "shinyjs", "plotly")

for (p in pkgs) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
  library(p, character.only = TRUE)
}

# Source du module de connexion SQL
source("db_connect.R")
