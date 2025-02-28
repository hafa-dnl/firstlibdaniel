test_that("trouver_l_elu_le_plus_age retourne l'élu le plus âgé", {
  df_test <- data.frame(
    Nom_elu = c("Dupont", "Martin", "Durand"),
    Prenom_elu = c("Jean", "Sophie", "Paul"),
    Date_de_naissance = as.Date(c("1960-05-14", "1980-08-25", "1975-12-10"))
  )

  result <- trouver_l_elu_le_plus_age(df_test)

  expect_equal(result$Nom_elu, "Dupont")
  expect_equal(result$Prenom_elu, "Jean")
  expect_equal(result$Date_de_naissance, as.Date("1960-05-14"))
})



test_that("trouver_l_elu_le_plus_age retourne une erreur si le dataframe est vide", {
  df_test <- data.frame(Nom_elu = character(), Date_de_naissance = as.Date(character()))
  expect_error(trouver_l_elu_le_plus_age(df_test))
})



