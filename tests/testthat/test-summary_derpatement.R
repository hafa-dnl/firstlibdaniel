test_that("summary_departement crée un résumé correct pour un département", {

  # Création d'un dataframe de test avec la classe 'departement'
  df_test <- data.frame(
    Libelle.du.departement = c("Île-de-France", "Île-de-France", "Île-de-France"),
    Code.de.la.commune = c("75056", "69123", "13055"),
    Nom.de.l.elu = c("Dupont", "Martin", "Durand"),
    Prenom.de.l.elu = c("Jean", "Sophie", "Paul"),
    stringsAsFactors = FALSE
  )

  # Ajout de la classe "departement"
  class(df_test) <- c("departement", class(df_test))

  # Application de la fonction summary_departement
  result <- summary_departement(df_test)

  # Vérification du contenu du résumé
  expect_equal(result$nom_departement, "Île-de-France")
  expect_equal(result$nombre_communes, 3)  # Trois communes distinctes
  expect_equal(result$nombre_elus, 3)      # Trois élus

  # Vérification de la classe de l'objet retourné
  expect_true(inherits(result, "summary.departement"))
})

test_that("summary_departement retourne une erreur si l'objet n'est pas un département", {
  df_test_invalid <- data.frame(
    Libelle_du_departement = "Île-de-France",
    Code_de_la_commune = "75056",
    Nom_de_l_elu = "Dupont",
    Prenom_de_l_elu = "Jean"
  )

  expect_error(summary_departement(df_test_invalid), "L'objet n'est pas de classe 'departement'")
})
