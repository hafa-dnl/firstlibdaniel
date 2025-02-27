# --------------- Fonction 1 : Compter le nombre d'adjoints ---------------
#' Compter le nombre d'adjoints
#'
#' Cette fonction permet de compter le nombre d'adjoints dans un jeu de donnees en fonction
#' de la colonne `Libelle_fonction`. Elle identifie les lignes contenant le terme "adjoint"
#' et retourne le nombre total d'adjoints dans le data.frame.
#'
#' @param df Un data.frame contenant les donnees des elus, incluant la colonne `Libelle_fonction`
#'           qui specifie la fonction des elus.
#' @return Un entier representant le nombre d'adjoints dans le data.frame.
#' @examples
#' df <- data.frame(Libelle_fonction = c("Maire", "Adjoint au Maire", "Adjoint", "Conseiller"))
#' compter_nombre_d_adjoints(df)
#'
compter_nombre_d_adjoints <- function(df) {

  adjoints <- grepl("adjoint", df$Libelle_fonction, ignore.case = TRUE)

  nombre_adjoints <- sum(adjoints)

  return(nombre_adjoints)
}




