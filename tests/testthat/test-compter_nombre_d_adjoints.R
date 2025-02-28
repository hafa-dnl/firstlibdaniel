test_that("compter_nombre_d_adjoints fonctionne correctement", {
  # Création d'un dataframe de test
  df_test <- data.frame(
    Nom.de.l.elu = c("Dupont", "Martin", "Durand"),
    Prenom.de.l.elu = c("Jean", "Sophie", "Paul"),
    Date.de.naissance = as.Date(c("1960-05-14", "1980-08-25", "1975-12-10"))
  )

  # Appeler la fonction et vérifier le résultat
  result <- compter_nombre_d_adjoints(df_test)
  expect_equal(result, 3)  # Il y a 3 lignes dans le dataframe

  # Test avec un dataframe vide
  df_empty <- data.frame(
    Nom.de.l.elu = character(0),
    Prenom.de.l.elu = character(0),
    Date.de.naissance = as.Date(character(0))
  )
  result_empty <- compter_nombre_d_adjoints(df_empty)
  expect_equal(result_empty, 0)  # Le dataframe est vide, donc le résultat attendu est 0
})

