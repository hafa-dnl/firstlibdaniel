#' Generer un rapport
#'
#' Cette fonction genere un rapport Quarto au format PDF ou HTML incluant :
#'
#' @param commune La commune pour laquelle generer le rapport (code INSEE).
#' @param departement Le departement concerne (code numerique).
#' @param output Le nom du fichier de sortie (ex: "rapport.pdf" ou "rapport.html").
#'
#' @return Le chemin du fichier genere.
#' @import ggplot2
#' @export
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
