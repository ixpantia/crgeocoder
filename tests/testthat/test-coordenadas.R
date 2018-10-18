context("Coordenadas")

library(dplyr)

test_that("tenemos datos para los cantones", {
  cantones <- crgeo_canton()
  expect_equal(nrow(cantones), 81)
})

test_that("tenemos datos para los distritos", {
  distritos <- crgeo_distrito()
  expect_equal(nrow(distritos), 471)
})

test_that("podemos leer coordenadas wsg84 para cantones", {
  sanjose <- canton_wsg84("San José")
  expect_equal(round(sanjose$lat, 4), round(9.934138, 4))
  expect_equal(round(sanjose$lng, 4), round(-84.07306, 4))
})

test_that("podemos leer coordenadas wsg84 para distritos", {
  huacas <- distrito_wsg84(canton = "Cartago",
                           unidad_territorial = "Carmen")
  expect_equal(round(huacas$lat, 4), round(9.87219, 4))
  expect_equal(round(huacas$lng, 4), round(-83.9208, 4))
})

test_that("nos encargamos de datos faltantes distritos", {
  huacas <- distrito_wsg84(canton = "escritorio",
                           unidad_territorial = "Carmen")
  expect_equal(round(huacas$lat, 4), round(9.87219, 4))
  expect_equal(round(huacas$lng, 4), round(-83.9208, 4))
})
