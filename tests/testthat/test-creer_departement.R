test_that("creer_departement crée un département avec les bons attributs", {
  result <- creer_departement("Île-de-France", c("Paris", "Lyon"))
  expect_true("Libelle_departement" %in% names(result))
  expect_true("Communes" %in% names(result))
  expect_equal(result$Libelle_departement, "Île-de-France")
  expect_equal(result$Communes[[1]], c("Paris", "Lyon"))
})

test_that("creer_departement retourne une erreur si les entrées sont incorrectes", {
  expect_error(creer_departement(123, c("Paris", "Lyon")))
})
