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

  # Localisation du fichier rapport.qmd
  qmd_file <- system.file("rapport.qmd", package = "firstlibdaniel")

  # Si le package n'est pas installe (en developpement), utiliser le chemin local
  if (qmd_file == "") {
    qmd_file <- file.path("inst", "rapport.qmd")
  }

  # Vérification de l'existence du fichier
  if (!file.exists(qmd_file)) {
    stop("Le fichier rapport.qmd n'a pas ete trouve.")
  }

  # Déterminer le format de sortie en fonction de l'extension
  ext <- tools::file_ext(output)
  if (ext == "pdf") {
    output_format <- "pdf"
  } else if (ext == "html") {
    output_format <- "html"
  } else {
    stop("Format de sortie non supporte. Utiliser '.pdf' ou '.html'.")
  }

  # Compilation du rapport avec Quarto
  quarto::quarto_render(
    input = qmd_file,
    output_format = output_format,
    output_file = output,
    execute_params = list(
      code_commune = commune,
      code_departement = departement
    )
  )

  # Récupérer le chemin exact du fichier généré
  generated_file <- list.files(path = ".", pattern = output, full.names = TRUE, recursive = TRUE)

  # Vérifier si le fichier a bien été généré
  if (length(generated_file) == 0 || !file.exists(generated_file[1])) {
    stop("Le fichier genere n'a pas ete trouve.")
  }

  # Déplacer le fichier généré dans le dossier "output/"
  output_dir <- "output"
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  destination_file <- file.path(output_dir, output)
  file.rename(generated_file[1], destination_file)

  # Message de confirmation
  message("Rapport genere avec succes : ", destination_file)
  return(destination_file)
}
