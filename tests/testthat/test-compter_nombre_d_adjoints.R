test_that("compter_nombre_d_adjoints fonctionne correctement", {
  # Test 1 : Vérifier avec un jeu de données exemple
  df_test <- data.frame(Libelle_fonction = c("Adjoint", "Maire", "Adjoint"))
  expect_equal(compter_nombre_d_adjoints(df_test), 2)

  # Test 2 : Vérifier avec un jeu de données vide
  df_empty <- data.frame(Libelle_fonction = character(0))
  expect_equal(compter_nombre_d_adjoints(df_empty), 0)
})
