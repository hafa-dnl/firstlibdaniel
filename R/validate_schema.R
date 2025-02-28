#' Validation du schema du dataframe
#'
#' Cette fonction verifie que le dataframe contient les colonnes necessaires
#' pour etre considere comme un objet de type "commune" ou "departement".
#'
#' @param df Un dataframe a valider.
#'
#' @return Aucun retour si la validation est reussie. Stoppe avec une erreur sinon.
#'
#' @details
#' La fonction verifie la presence des colonnes essentielles :
#' - Pour un objet "commune" : `Code.de.la.commune`, `Libelle.de.la.commune`
#' - Pour un objet "departement" : `Code.du.departement`, `Libelle.du.departement`
#'
#' @examples
#' \dontrun{
#' data <- data.frame(
#'   Code.de.la.commune = "75056",
#'   Libelle.de.la.commune = "Paris"
#' )
#' validate_schema(data)
#' }
validate_schema <- function(df) {
 # Verifier que df est un dataframe
 if (!is.data.frame(df)) {
  stop("Erreur : L'objet fourni n'est pas un dataframe.")
 }

 # Colonnes requises pour la classe "commune"
 colonnes_commune <- c("Code.de.la.commune", "Libelle.de.la.commune")
 colonnes_departement <- c("Code.du.departement", "Libelle.du.departement")

 # Verification pour "commune"
 if (inherits(df, "commune")) {
  if (!all(colonnes_commune %in% colnames(df))) {
   stop("Erreur : Le dataframe de type 'commune' doit contenir les colonnes : ",
        paste(colonnes_commune, collapse = ", "))
  }
 }

 # Verification pour "departement"
 if (inherits(df, "departement")) {
  if (!all(colonnes_departement %in% colnames(df))) {
   stop("Erreur : Le dataframe de type 'departement' doit contenir les colonnes : ",
        paste(colonnes_departement, collapse = ", "))
  }
 }
}
