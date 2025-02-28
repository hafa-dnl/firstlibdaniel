library(testthat)
library(dplyr)
library(magrittr)  # Pour utiliser le pipe %>%
library(firstlibsabiron)

# Vérifie que les données sont bien chargées
skip_if_not_installed("firstlibsabiron")

# Charger les donnees du package
data("elus_conseillers_municipaux_cm", package = "firstlibsabiron")

#### 1️⃣ TESTS POUR creer_commune() ####
test_that("creer_commune fonctionne avec une seule commune", {
 df_test <- elus_conseillers_municipaux_cm %>%
  dplyr::filter(Libelle.de.la.commune == "Paris")
 result <- creer_commune(df_test)
 expect_true(inherits(result, "commune"))
})

test_that("creer_commune renvoie une erreur si plusieurs communes", {
 expect_error(creer_commune(elus_conseillers_municipaux_cm),
              "Le dataframe contient plusieurs communes. Fournissez les donnees d'une seule commune.")
})

#### 2️⃣ TESTS POUR summary_commune() ####
test_that("summary_commune fonctionne correctement", {
 df_test <- elus_conseillers_municipaux_cm %>%
  dplyr::filter(Libelle.de.la.commune == "Paris") %>%
  creer_commune()

 result <- summary_commune(df_test)  # Utilisation explicite de summary_commune()
 expect_true(is.list(result))
 expect_s3_class(result, "summary.commune")
})

#### 5️⃣ TESTS POUR plot_commune() ####
test_that("plot_commune genere un ggplot valide", {
 df_test <- elus_conseillers_municipaux_cm %>%
  dplyr::filter(Libelle.de.la.commune == "Paris") %>%
  creer_commune()

 # Utilisation explicite de plot_commune()
 p <- plot_commune(df_test)
 expect_s3_class(p, "ggplot")
})

#### 6️⃣ TESTS POUR plot_departement() ####
test_that("plot_departement genere un ggplot valide", {
 df_test <- elus_conseillers_municipaux_cm %>%
  dplyr::filter(Code.du.departement == "75") %>%
  creer_departement()

 # Utilisation explicite de plot_departement()
 p <- plot_departement(df_test)
 expect_s3_class(p, "ggplot")
})

#### 7️⃣ TESTS POUR summary_commune() ####
test_that("summary_commune fonctionne correctement", {
 df_test <- elus_conseillers_municipaux_cm %>%
  dplyr::filter(Libelle.de.la.commune == "Paris") %>%
  creer_commune()

 result <- summary_commune(df_test)  # Utilisation explicite de summary_commune()
 expect_true(is.list(result))
 expect_s3_class(result, "summary.commune")
})

#### 8️⃣ TESTS POUR summary_departement() ####
test_that("summary_departement fonctionne correctement", {
 df_test <- elus_conseillers_municipaux_cm %>%
  dplyr::filter(Code.du.departement == "75") %>%
  creer_departement()

 result <- summary_departement(df_test)  # Utilisation explicite de summary_departement()
 expect_true(is.list(result))
 expect_s3_class(result, "summary.departement")
})



