test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})
test_that("dtriang works correctly and catches errors", {
  # Pruebas de errores[cite: 1]
  expect_error(dtriang(1, min = 2, max = 1, mode = 1.5))
  expect_error(dtriang(1, min = 0, max = 2, mode = 3))

  # Pruebas de cálculos matemáticos vectorizados
  expect_equal(dtriang(-1, 0, 2, 1), 0)   # Fuera del límite inferior
  expect_equal(dtriang(3, 0, 2, 1), 0)    # Fuera del límite superior
  expect_equal(dtriang(0.5, 0, 2, 1), 0.5)  # En la subida
  expect_equal(dtriang(1, 0, 2, 1), 1)    # En la moda exacta
  expect_equal(dtriang(1.5, 0, 2, 1), 0.5)  # En la bajada
})

test_that("ptriang works correctly and catches errors", {
  # Pruebas de errores[cite: 1]
  expect_error(ptriang(1, min = 2, max = 1, mode = 1.5))
  expect_error(ptriang(1, min = 0, max = 2, mode = 3))

  # Pruebas de cálculos de probabilidad acumulada
  expect_equal(ptriang(-1, 0, 2, 1), 0)
  expect_equal(ptriang(3, 0, 2, 1), 1)
  expect_equal(ptriang(0.5, 0, 2, 1), 0.125)
  expect_equal(ptriang(1.5, 0, 2, 1), 0.875)
  expect_equal(ptriang(1, 0, 2, 1), 0.5)
})

test_that("qtriang works correctly and catches errors", {
  # Pruebas de errores[cite: 1]
  expect_error(qtriang(0.5, min = 2, max = 1, mode = 1.5))
  expect_error(qtriang(0.5, min = 0, max = 2, mode = 3))
  expect_error(qtriang(-0.1, 0, 2, 1)) # Probabilidad menor a 0[cite: 1]
  expect_error(qtriang(1.1, 0, 2, 1))  # Probabilidad mayor a 1[cite: 1]

  # Pruebas de cálculo de cuantiles
  expect_equal(qtriang(0, 0, 2, 1), 0)
  expect_equal(qtriang(1, 0, 2, 1), 2)
  expect_equal(qtriang(0.5, 0, 2, 1), 1)
})

test_that("rtriang works correctly and catches errors", {
  # Pruebas de errores[cite: 1]
  expect_error(rtriang(10, min = 2, max = 1, mode = 1.5))
  expect_error(rtriang(10, min = 0, max = 2, mode = 3))

  # Prueba de generación y longitud del vector
  set.seed(42)
  res1 <- rtriang(5, 0, 2, 1)
  expect_length(res1, 5)
  expect_true(all(res1 >= 0 & res1 <= 2))

  # Prueba de reciclaje (si length(n) > 1)[cite: 1]
  res2 <- rtriang(c(1, 2, 3), 0, 2, 1)
  expect_length(res2, 3)
})
