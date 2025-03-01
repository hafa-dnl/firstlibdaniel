# Projet ----

#' Génère un rapport en HTML à partir d'un fichier Quarto
#'
#' Cette fonction génère un rapport en HTML à partir d'un fichier Quarto spécifié en utilisant les paramètres
#' de la commune et du département. Le rapport est rendu en HTML et enregistré à l'emplacement spécifié par
#' l'utilisateur.
#'
#' @param commune Code de la commune. Doit être un nombre ou une chaîne de caractères représentant le code
#'               de la commune.
#' @param departement Code du département. Doit être un nombre ou une chaîne de caractères représentant
#'                    le code du département.
#' @param output Chemin complet où le fichier HTML généré sera sauvegardé.
#'
#' @return Aucun retour, mais un fichier HTML est généré et sauvegardé à l'emplacement spécifié.
#'
#' @import quarto
#' @export
#'
#' @examples
#' # Génère un rapport pour la commune avec code 44109 et le département avec code 32 avec un chemin de sortie personnalisé
#' generer_rapport(77056, 77, "output/mon_rapport.html")
generer_rapport <- function(commune, departement, output) {
  quarto::quarto_render(
    input = "inst/rapport.qmd",  # Cherche le fichier rapport.qmd
    output_format = "html",
    output_file = output,  # Spécifie le chemin de sortie
    execute_params = list(
      "code_commune" = commune,
      "code_departement" = departement
    )
  )
}

library(firstlibdaniel)
generer_rapport(75056, 77, "rapport.html")
