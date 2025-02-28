# --------------- Fonction 1 : Compter le nombre d'adjoints ---------------
#' Compter le nombre d'adjoints
#'
#' Compter le nombre d'elus uniques
#'
#' Cette fonction compte le nombre total d'elus presents dans un dataframe contenant leurs informations.
#'
#' @param data Un dataframe contenant au moins les colonnes `Nom.de.l.elu`, `Prenom.de.l.elu` et `Date.de.naissance`.
#'
#' @return Un entier representant le nombre total d'elus dans le dataframe.
#'
#' @details
#' - La fonction verifie la presence des colonnes obligatoires dans `data`.
#' - Elle compte le nombre total de lignes du dataframe, supposant que chaque ligne represente un elu unique.
#'
compter_nombre_d_elus <- function(data) {
  if (!all(c("Nom.de.l.elu", "Prenom.de.l.elu", "Date.de.naissance") %in% colnames(data))) {
    stop("Les colonnes de nom, prenom et date de naissance doivent etre presentes dans le dataframe.")
  }

  nombre_unique <- nrow(data)

  return(nombre_unique)
}

# --------------- Fonction 2 : Trouver l'elu le plus age ---------------
#' Trouver l'elu le plus age
#'
#' Cette fonction identifie l'elu(e) le plus age(e) dans un dataframe contenant des informations sur les elus.
#'
#' @param data Un dataframe contenant au moins les colonnes suivantes :
#'   - `Nom.de.l.elu` (character) : Le nom de l'elu(e).
#'   - `Prenom.de.l.elu` (character) : Le prenom de l'elu(e).
#'   - `Date.de.naissance` (character) : La date de naissance sous format `jour/mois/annee` (ex: "12/05/1950").
#'
#' @return Un dataframe avec les informations de l'elu(e) le plus age(e), contenant les colonnes :
#'   - `Nom.de.l.elu`
#'   - `Prenom.de.l.elu`
#'   - `Date.de.naissance` (au format Date).
#'
#' @details
#' - La fonction verifie la presence des colonnes requises.
#' - Elle convertit la colonne `Date.de.naissance` en format Date avec `lubridate::dmy()`.
#' - Elle selectionne l'elu(e) avec la date de naissance la plus ancienne.
#'
#'
#' @importFrom dplyr mutate slice select
#' @importFrom lubridate dmy
#'
trouver_l_elus_le_plus_age <- function(data) {
  if (!all(c("Nom.de.l.elu", "Prenom.de.l.elu", "Date.de.naissance") %in% colnames(data))) {
    stop("Les colonnes 'Nom.de.l.elu', 'Prenom.de.l.elu' et 'Date.de.naissance' doivent etre presentes dans le dataframe.")
  }

  data |>
    dplyr::mutate(Date.de.naissance = lubridate::dmy(Date.de.naissance)) |>
    dplyr::slice(which.min(Date.de.naissance)) |>
    dplyr::select(Nom.de.l.elu, Prenom.de.l.elu, Date.de.naissance)
}





# --------------- Fonction 3 : calcul_distribution_age ---------------
#' Calculer la distribution des ages
#'
#' Cette fonction calcule la distribution des ages a partir d'une colonne `Date.de.naissance` en format **Date**.
#'
#' @param data Un dataframe contenant au moins la colonne suivante :
#'   - `Date.de.naissance` (Date) : La date de naissance des elus, qui doit etre deja convertie en format `Date`.
#'
#' @return Un vecteur numerique representant les ages des elus en annees.
#'
#' @details
#' - La fonction verifie la presence de la colonne `Date.de.naissance`.
#' - Elle verifie que la colonne est bien de type **Date**, sinon elle renvoie une erreur.
#' - Elle calcule l'age en annees en utilisant la difference entre la date du jour (`Sys.Date()`) et la `Date.de.naissance`.
#'
#'
#'
calcul_distribution_age <- function(data) {
  if (!"Date.de.naissance" %in% colnames(data)) {
    stop("La colonne 'Date.de.naissance' doit etre presente dans le dataframe.")
  }

  # Verification que la colonne "Date.de.naissance" est de type Date
  if (!inherits(data$Date.de.naissance, "Date")) {
    stop("La colonne 'Date.de.naissance' doit etre au format Date. Utilisez as.Date() pour la convertir.")
  }

  # Calcul des ages en annees
  ages <- as.numeric(difftime(Sys.Date(), data$Date.de.naissance, units = "days")) %/% 365

  return(ages)
}




# --------------- Fonction 4 : plot_code_professions ---------------
#' Visualiser la répartition des catégories socio-professionnelles des maires
#' Visualiser la repartition des elus par code professionnel
#'
#' Cette fonction genere un graphique en barres horizontales representant le nombre d'elus par code professionnel.
#'
#' @description
#' La fonction compte le nombre d'elus pour chaque code professionnel (`Code.de.la.categorie.socio.professionnelle`),
#' filtre les codes n'ayant aucun elu, puis produit un graphique en barres classe par ordre decroissant.
#'
#' @param data Un dataframe contenant au moins la colonne suivante :
#'   - `Code.de.la.categorie.socio.professionnelle` (character) : Code professionnel des elus.
#'
#' @return Un graphique `ggplot2` representant la repartition des elus par code professionnel.
#'
#' @details
#' - Les valeurs a `n = 0` sont filtrees avant l'affichage du graphique.
#' - Le graphique est trie par nombre d'elus de maniere decroissante.
#' - Les barres sont colorees en bleu fonce pour une meilleure lisibilite.
#'
#' @importFrom dplyr count filter arrange
#' @importFrom ggplot2 ggplot aes geom_bar labs theme_minimal
#'
plot_code_professions <- function(data) {
  if (!"Code.de.la.categorie.socio.professionnelle" %in% colnames(data)) {
    stop("La colonne 'Code.de.la.categorie.socio.professionnelle' doit etre presente dans le dataframe.")
  }

  # Compter le nombre d'elus par code professionnel et filtrer ceux a 0
  count_professions <- data |>
    dplyr::count(Code.de.la.categorie.socio.professionnelle) |>
    dplyr::filter(n > 0) |>
    dplyr::arrange(desc(n))

  # Generer le bar chart horizontal
  ggplot2::ggplot(count_professions, ggplot2::aes(x = n, y = reorder(Code.de.la.categorie.socio.professionnelle, n))) +
    ggplot2::geom_bar(stat = "identity", fill = "darkblue") +
    ggplot2::labs(title = "Nombre d'elus par code professionnel",
                  x = "Nombre d'elus",
                  y = "Code professionnel") +
    ggplot2::theme_minimal()
}


# --------------- Fonction 5 : summary.commune ---------------
#' Resume des donnees d'une commune
#'
#' Cette fonction genere un resume pour les objets de classe `commune`.
#' Elle affiche le nom de la commune, le nombre total d'elus et la repartition professionnelle.
#'
#' @param obj Un objet de classe `commune`.
#' @param ... Arguments supplementaires (non utilises).
#'
#' @return Un resume des informations de la commune sous forme de liste.
#' @export
summary_commune <- function(obj, ...) {
  # Verification de la classe
  if (!inherits(obj, "commune")) {
    stop("L'objet n'est pas de classe 'commune'")
  }

  # Creation du resume
  result <- list(
    nom_commune = unique(obj$Libelle.de.la.commune),
    nombre_elus = nrow(obj),
    repartition_professionnelle = obj %>%
      dplyr::count(Code.de.la.categorie.socio.professionnelle, name = "n")
  )

  # Attribution de la classe S3 pour l'affichage
  class(result) <- "summary.commune"

  return(result)
}

#' Methode S3 pour summary.commune
#'
#' Cette fonction redirige automatiquement `summary()` vers `summary_commune()`
#' pour les objets de classe `commune`.
#'
#' @param object Un objet de classe `commune`.
#' @param ... Arguments supplementaires (non utilises).
#'
#' @export
summary.commune <- function(object, ...) {
  summary_commune(object, ...)
}

#' Affichage du resume pour summary.commune
#'
#' Cette fonction definit l'affichage personnalise pour les objets de classe `summary.commune`.
#'
#' @param x Un objet de classe `summary.commune`.
#' @param ... Arguments supplementaires (non utilises).
#'
#' @return NULL (resume affiche dans la console)
#' @export
print.summary.commune <- function(x, ...) {
  cat("Resume de la commune :\n")
  cat("Nom de la commune :", x$nom_commune, "\n")
  cat("Nombre total d'elus :", x$nombre_elus, "\n")
  cat("\nRepartition professionnelle :\n")
  print(x$repartition_professionnelle)
}


# --------------- Fonction 6 : summary.departement ---------------
#' Summary des Departements
#'
#' Cette fonction genere un resume pour les objets de classe `departement`.
#' Elle affiche le nom du departement, le nombre total de communes, et le nombre total d'elus.
#'
#' @param object Un objet de classe `departement`.
#' @param ... Arguments supplementaires (non utilises).
#'
#' @return Un resume des informations du departement sous forme de liste.
#' @export
summary_departement <- function(object, ...) {

  # Verification de la classe de l'objet
  if (!inherits(object, "departement")) {
    stop("L'objet n'est pas de classe 'departement'")
  }

  # Calcul du nom du departement
  nom_departement <- unique(object$Libelle.du.departement)

  # Calcul du nombre total de communes
  nb_communes <- object |>
    dplyr::distinct(Code.de.la.commune) |>
    nrow()

  # Calcul du nombre total d'elus
  nb_elus <- nrow(object)

  # Creation du resume sous forme de liste
  result <- list(
    nom_departement = nom_departement,
    nombre_communes = nb_communes,
    nombre_elus = nb_elus
  )

  # Attribution de la classe "summary.departement" a la liste
  class(result) <- "summary.departement"

  return(result)
}

#' Methode S3 pour summary.departement
#'
#' Cette fonction redirige automatiquement `summary()` vers `summary_departement()`
#' pour les objets de classe `departement`.
#'
#' @param object Un objet de classe `departement`.
#' @param ... Arguments supplementaires (non utilises).
#'
#' @export
summary.departement <- function(object, ...) {
  summary_departement(object, ...)
}

#' Affichage du resume pour summary.departement
#'
#' Cette fonction definit l'affichage personnalise pour les objets de classe `summary.departement`.
#'
#' @param x Un objet de classe `summary.departement`.
#' @param ... Arguments supplementaires (non utilises).
#'
#' @return NULL (resume affiche dans la console)
#' @export
print.summary.departement <- function(x, ...) {
  cat("Resume du departement :\n")
  cat("Nom du departement :", x$nom_departement, "\n")
  cat("Nombre total de communes :", x$nombre_communes, "\n")
  cat("Nombre total d'elus :", x$nombre_elus, "\n")
}


# --------------- Fonction 7 : plot.commune ---------------
#' Visualiser les catégories socio-professionnelles des Maires d'une commune
#'
#' Cette fonction genere un graphique en barres representant le nombre d'elus par code professionnel
#' pour une commune donnee.
#'
#' @description
#' La fonction verifie que l'objet fourni est bien de classe `"commune"`, valide sa structure avec `validate_schema()`,
#' puis produit un graphique en barres horizontales representant la repartition des elus par code professionnel.
#' Le titre du graphique est compose du nom de la commune et de son departement.
#'
#' @param x Un objet de classe "commune".
#' @param ... Arguments supplementaires passes a `ggplot2::ggplot()`.
#'
#' @return Un graphique `ggplot2` representant la repartition des elus par code professionnel dans la commune.
#'
#' @details
#' - La fonction utilise `validate_schema(x)` pour s'assurer que la structure des donnees est correcte.
#' - Elle verifie que `x` est bien un objet de classe `"commune"`, sinon elle renvoie une erreur.
#' - Elle compte le nombre d'elus par code professionnel et filtre ceux ayant `n = 0`.
#' - Un graphique en barres est genere, trie en ordre decroissant et affichant les valeurs sur les barres.
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' library(dplyr)
#'
#' # Exemple de dataframe representant une commune
#' data <- data.frame(
#'   Libelle.de.la.commune = rep("Paris", 6),
#'   Libelle.du.departement = rep("Paris", 6),
#'   Code.de.la.categorie.socio.professionnelle = c("A1", "B2", "A1", "C3", "B2", "A1")
#' )
#'
#' # Ajouter la classe "commune"
#' class(data) <- c("commune", class(data))
#'
#' # Generer le graphique
#' plot_commune(data)
#' }
#'
#' @importFrom dplyr count filter arrange desc
#' @importFrom ggplot2 ggplot aes geom_bar geom_text labs theme_minimal
#' @importFrom stats reorder
#' @export
plot_commune <- function(x, ...) {
  df <- x  # Assigne `x` a `df` pour garder le reste du code inchange

  # Verifier que le DataFrame respecte le schema attendu
  validate_schema(df)

  # Verifier que l'objet df est bien de la classe "commune"
  if (!inherits(df, "commune")) {
    stop("L'objet doit etre de classe 'commune'. Utilisez `creer_commune()` pour le transformer.")
  }

  # Extraire le nom de la commune et du departement
  nom_commune <- unique(df$Libelle.de.la.commune)
  nom_departement <- unique(df$Libelle.du.departement)

  # Compter le nombre d'elus par code professionnel et filtrer ceux a 0
  count_professions <- df |>
    dplyr::count(Code.de.la.categorie.socio.professionnelle) |>
    dplyr::filter(n > 0) |>
    dplyr::arrange(dplyr::desc(n))

  # Construire le titre et l'axe des abscisses
  titre_graphique <- paste(nom_commune, "-", nom_departement)
  axe_x <- paste("Libelles des codes professionnels pour les", sum(count_professions$n), "elus")

  # Generer le graphique en barres horizontal
  ggplot2::ggplot(count_professions, ggplot2::aes(x = n, y = stats::reorder(Code.de.la.categorie.socio.professionnelle, n))) +
    ggplot2::geom_bar(stat = "identity", fill = "darkblue") +
    ggplot2::geom_text(ggplot2::aes(label = n), hjust = -0.2, color = "black", size = 2) +  # Ajoute les etiquettes des valeurs
    ggplot2::labs(
      title = titre_graphique,
      x = axe_x,
      y = "Code professionnel"
    ) +
    ggplot2::theme_minimal()
}



# --------------- Fonction 8 : plot_departement ---------------
#' Visualiser la repartition des elus par code professionnel dans un departement
#'
#' Cette fonction genere un graphique en barres representant le nombre d'elus par code professionnel
#' pour un departement donne.
#'
#' @description
#' La fonction verifie que l'objet fourni est bien de classe `"departement"`, valide sa structure avec `validate_schema()`,
#' puis produit un graphique en barres horizontales representant la repartition des elus par code professionnel.
#' Le graphique affiche les **10 codes professionnels les plus representes** dans le departement.
#' Le titre du graphique inclut le nom du departement et le nombre total de communes qu'il contient.
#'
#' @param x Un objet de classe "departement".
#' @param ... Arguments supplementaires passes a `ggplot2::ggplot()`.
#'
#' @return Un graphique `ggplot2` representant la repartition des elus par code professionnel dans le departement.
#'
#' @details
#' - La fonction utilise `validate_schema(x)` pour s'assurer que la structure des donnees est correcte.
#' - Elle verifie que `x` est bien un objet de classe `"departement"`, sinon elle renvoie une erreur.
#' - Elle compte le nombre total de communes distinctes presentes dans le departement.
#' - Elle filtre et selectionne les **10 codes professionnels les plus representes**.
#' - Un graphique en barres est genere, trie en ordre decroissant et affichant les valeurs sur les barres.
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' library(dplyr)
#'
#' # Exemple de dataframe representant un departement
#' data <- data.frame(
#'   Libelle.du.departement = rep("Paris", 15),
#'   Libelle.de.la.commune = c(rep("Paris", 10), rep("Boulogne-Billancourt", 5)),
#'   Code.de.la.categorie.socio.professionnelle = sample(LETTERS[1:5], 15, replace = TRUE)
#' )
#'
#' # Ajouter la classe "departement"
#' class(data) <- c("departement", class(data))
#'
#' # Generer le graphique
#' plot_departement(data)
#' }
#'
#' @importFrom dplyr count filter arrange distinct slice_head desc
#' @importFrom ggplot2 ggplot aes geom_bar geom_text labs theme_minimal
#' @importFrom stats reorder
#' @export
plot_departement <- function(x, ...) {
  df <- x  # Assigne `x` a `df` pour garder le reste du code inchange

  # Verifier que le DataFrame respecte la structure minimale
  validate_schema(df)

  # Verifier que l'objet df est bien de la classe "departement"
  if (!inherits(df, "departement")) {
    stop("L'objet doit etre de classe 'departement'. Utilisez `creer_departement()` pour le transformer.")
  }

  # Extraire le nom du departement
  nom_departement <- unique(df$Libelle.du.departement)

  # Calculer le nombre de communes distinctes dans le departement
  nb_communes <- df |>
    dplyr::distinct(Libelle.de.la.commune) |>
    nrow()

  # Compter le nombre d'elus par code professionnel et filtrer les 10 plus frequents
  count_professions <- df |>
    dplyr::count(Code.de.la.categorie.socio.professionnelle) |>
    dplyr::filter(n > 0) |>
    dplyr::arrange(dplyr::desc(n)) |>
    dplyr::slice_head(n = 10) # Selectionner les 10 codes professionnels les plus representes

  # Construire le titre et l'axe des abscisses
  titre_graphique <- paste(nom_departement, "-", nb_communes, "communes")
  axe_x <- paste("Libelles des 10 codes professionnels les plus representes pour", nom_departement)

  # Generer le graphique en barres horizontal
  ggplot2::ggplot(count_professions, ggplot2::aes(x = n, y = stats::reorder(Code.de.la.categorie.socio.professionnelle, n))) +
    ggplot2::geom_bar(stat = "identity", fill = "darkblue") +
    ggplot2::geom_text(ggplot2::aes(label = n), hjust = -0.2, color = "black", size = 2) +  # Ajoute les etiquettes des valeurs
    ggplot2::labs(
      title = titre_graphique,
      x = axe_x,
      y = "Code professionnel"
    ) +
    ggplot2::theme_minimal()
}


# --------------- Fonction 9 : creer_commune ---------------
#' Cette fonction transforme un dataframe representant une seule commune en un objet de classe "commune".
#'
#' @description
#' La fonction s'assure que le dataframe contient une seule commune distincte,
#' et lui attribue la classe `"commune"` s'il ne l'a pas deja.
#'
#' @param df Un dataframe contenant au moins la colonne suivante :
#'   - `Code.de.la.commune` (character ou numeric) : Le code identifiant la commune.
#'
#' @return Un dataframe de classe `"commune"`, pret a etre utilise dans d'autres fonctions specifiques aux communes.
#'
#' @details
#' - La fonction verifie que la colonne `Code.de.la.commune` est presente dans `df`.
#' - Elle s'assure qu'il n'y a **qu'une seule commune distincte**, sinon elle genere une erreur.
#' - Si la classe `"commune"` n'est pas deja presente, elle est ajoutee a l'objet.
#'
creer_commune <- function(df) {
  # Verifier si la colonne 'Code.de.la.commune' existe dans le dataframe
  if (!"Code.de.la.commune" %in% colnames(df)) {
    stop("Le dataframe doit contenir la colonne 'Code.de.la.commune'.")
  }

  # Verifier qu'il y a une seule commune unique
  unique_communes <- unique(df$Code.de.la.commune)
  if (length(unique_communes) > 1) {
    stop("Le dataframe contient plusieurs communes. Fournissez les donnees d'une seule commune.")
  }

  # Ajouter la classe "commune" si elle n'est pas deja presente
  if (!inherits(df, "commune")) {
    class(df) <- c("commune", class(df))
  }

  return(df)
}


# --------------- Fonction 10 : creer_departement ---------------
#' Creer un Departement
#'
#' Cette fonction transforme un dataframe representant un departement en un objet de classe "departement".
#'
#' @param df Un dataframe contenant au moins la colonne `Code.du.departement`.
#'
#' @return Un dataframe de classe `"departement"`, qui peut etre utilise avec des fonctions specifiques aux departements.
#'
#' @details
#' - La fonction verifie que la colonne `Code.du.departement` est presente dans `df`.
#' - Elle s'assure que le dataframe represente un seul departement, sinon elle genere une erreur.
#' - La classe `"departement"` est ajoutee a l'objet pour permettre un traitement specifique.
#'
#'
#' @export
creer_departement <- function(df) {
  # Verifier la presence de la colonne cle
  if (!"Code.du.departement" %in% colnames(df)) {
    stop("Le dataframe doit contenir la colonne 'Code.du.departement'.")
  }

  # Verifier s'il y a plusieurs departements
  unique_departements <- unique(df$Code.du.departement)
  if (length(unique_departements) > 1) {
    stop("Le dataframe contient plusieurs departements. Fournissez les donnees d'un seul departement.")
  }

  # Ajouter la classe "departement"
  if (!inherits(df, "departement")) {
    class(df) <- c("departement", class(df))
  }

  return(df)
}

