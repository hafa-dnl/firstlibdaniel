library(testthat)

test_that("creer_departement crée un département avec les bons attributs", {
  # Créer un dataframe de test avec une seule valeur pour Code.du.departement
  df_test <- data.frame(
    Code.du.departement = rep("IDF", 5),
    Nom_commun = c("Paris", "Lyon", "Marseille", "Toulouse", "Nice")
  )

  # Appliquer la fonction
  result <- creer_departement(df_test)

  # Vérifier que la colonne 'Code.du.departement' est bien présente
  expect_true("Code.du.departement" %in% colnames(result))

  # Vérifier que la classe 'departement' a bien été ajoutée
  expect_true(inherits(result, "departement"))

  # Vérifier qu'il n'y a qu'un seul département unique
  expect_equal(length(unique(result$Code.du.departement)), 1)
})

test_that("creer_departement retourne une erreur si le dataframe contient plusieurs départements", {
  # Créer un dataframe de test avec plusieurs valeurs pour Code.du.departement
  df_test <- data.frame(
    Code.du.departement = c("IDF", "PAC", "IDF"),
    Nom_commun = c("Paris", "Marseille", "Lyon")
  )

  # Vérifier que la fonction génère une erreur
  expect_error(creer_departement(df_test), "Le dataframe contient plusieurs departements")
})

test_that("creer_departement retourne une erreur si la colonne 'Code.du.departement' est absente", {
  # Créer un dataframe de test sans la colonne 'Code.du.departement'
  df_test <- data.frame(
    Nom_commun = c("Paris", "Marseille", "Lyon")
  )

  # Vérifier que la fonction génère une erreur
  expect_error(creer_departement(df_test), "Le dataframe doit contenir la colonne 'Code.du.departement'")
})
