data("square_data")
data("hexagonal_data")

test_that(desc = "inappropriate x",
          code = {
            expect_error(object = LandComp(x = TRUE, aggregation_steps = 1))
            expect_error(object = LandComp(x = "foo", aggregation_step = 2))

            hexagonal_data_char = hexagonal_data[1:10, 1:2]
            hexagonal_data_char$VT1 <- as.character(hexagonal_data_char$VT1)
            hexagonal_data_char$VT2 <- as.character(hexagonal_data_char$VT2)
            expect_warning(object = LandComp(x = hexagonal_data_char, aggregation_steps = 0))

            hexagonal_data_logi = hexagonal_data[1:10, 1:2]
            hexagonal_data_logi$VT1 <- as.logical(hexagonal_data_logi$VT1)
            hexagonal_data_logi$VT2 <- as.logical(hexagonal_data_logi$VT2)
            expect_warning(object = LandComp(x = hexagonal_data_logi, aggregation_steps = 0))
          }
)

test_that(desc = "strange or extreme x",
          code = {
            expect_error(object = LandComp(x = sf::st_geometry(square_data), aggregation_steps = 1)) # column0
            rowmask = 1:50
            column1 = LandComp(x = square_data[rowmask,1], aggregation_steps = 0:1)
            expect_equal(object = column1[,"SpatialUnit_Size"],
                         expected = c(1,9))
            expect_equal(object = column1[,"SpatialUnit_Count"],
                         expected = c(50, 8))
            expect_equal(object = column1[,"UniqueCombination_Count"],
                         expected = c(2, 2))
            expect_equal(object = round(column1[,"CD_bit"], digits = 2),
                         expected = c(0.47, 0.95))
            expect_equal(object = round(column1[,"AS_bit"], digits = 2),
                         expected = c(0, 0))

            row1 = LandComp(x = square_data[1,], aggregation_steps = 0:1)
            expect_equal(object = row1[,"SpatialUnit_Size"],
                         expected = c(1,9))
            expect_equal(object = row1[,"SpatialUnit_Count"],
                         expected = c(1, 0))
            expect_equal(object = row1[,"UniqueCombination_Count"],
                         expected = c(1, 0))
            expect_equal(object = round(row1[,"CD_bit"], digits = 2),
                         expected = c(0, 0))
            expect_equal(object = round(row1[,"AS_bit"], digits = 2),
                         expected = c(0, 0))

            other_extreme_data = data.frame(VT000 = rep(0, times = 300),
                                            VT111 = rep(1, times = 300))
            sf::st_geometry(other_extreme_data) <- sf::st_geometry(square_data)
            rowmask = 1:10
            column000 <- LandComp(x = other_extreme_data[rowmask,"VT000"], aggregation_steps = 0)
            expect_equal(object = column000[,"SpatialUnit_Size"],
                         expected = 1)
            expect_equal(object = column000[,"SpatialUnit_Count"],
                         expected = 10)
            expect_equal(object = column000[,"UniqueCombination_Count"],
                         expected = 1)
            expect_equal(object = round(column000[,"CD_bit"], digits = 2),
                         expected = 0)
            expect_equal(object = round(column000[,"AS_bit"], digits = 2),
                         expected = 0)

            column111 <- LandComp(x = other_extreme_data[rowmask,"VT000"], aggregation_steps = 0)
            expect_equal(object = column111[,"SpatialUnit_Size"],
                         expected = 1)
            expect_equal(object = column111[,"SpatialUnit_Count"],
                         expected = 10)
            expect_equal(object = column111[,"UniqueCombination_Count"],
                         expected = 1)
            expect_equal(object = round(column111[,"CD_bit"], digits = 2),
                         expected = 0)
            expect_equal(object = round(column111[,"AS_bit"], digits = 2),
                         expected = 0)
          }
)


test_that(desc = "inappropriate aggregation_step",
          code = {
            expect_error(object = LandComp(x = square_data, aggregation_steps = TRUE))
            expect_error(object = LandComp(x = square_data, aggregation_steps = "foo"))
            expect_warning(expect_error(object = LandComp(x = square_data, aggregation_steps = NA)))
            expect_warning(expect_error(object = LandComp(x = square_data, aggregation_steps = Inf)))

            expect_warning(expect_error(object = LandComp(x = square_data, aggregation_steps = -1)))
            expect_warning(expect_error(object = LandComp(x = hexagonal_data, aggregation_steps = -10)))

          }
)


test_that(desc = "aggregation step in the interval ]0;1[ when using hexagonal grid",
          code = {
            expect_warning(expect_error(object = LandComp(x = hexagonal_data, aggregation_step = 0.5)))
          }
)


test_that(desc = "inappropriate parallelrun",
          code = {
            expect_error(object = LandComp(x = square_data, aggregation_steps = 1,parallelrun = 100))
            expect_error(object = LandComp(x = square_data, aggregation_steps = 1, parallelrun = "bla"))
            expect_error(object = LandComp(x = square_data, aggregation_steps = 1, parallelrun = NA))
            rowmask = 1:10
            expect_warning(object = LandComp(x = square_data[rowmask,], aggregation_step = 3, parallelrun = c(TRUE, FALSE)))
          }
)


test_that(desc = "inappropriate savememory",
          code = {
            expect_error(object = LandComp(x = square_data, aggregation_steps = 1, savememory = 100))
            expect_error(object = LandComp(x = square_data, aggregation_steps = 1, savememory = "bla"))
            expect_error(object = LandComp(x = square_data, aggregation_steps = 1, savememory = NA))
            rowmask = 1:10
            expect_warning(object = LandComp(x = square_data[rowmask,], aggregation_step = 3, savememory = c(TRUE, FALSE)))
          }
)


test_that(desc = "inappropriate precision",
          code = {
            expect_error(object = LandComp(x = square_data, aggregation_steps = 1, precision = TRUE))
            expect_error(object = LandComp(x = square_data, aggregation_steps = 1, precision = "foo"))
            expect_error(object = LandComp(x = square_data, aggregation_steps = 1, precision = NA))
            rowmask = 1:10
            expect_warning(object = LandComp(x = square_data[rowmask,], aggregation_step = 3,precision = c(4, 5)))
          }
)


test_that(desc = "appropriate setting - square grid",
          code = {
            square_result <- LandComp(x = square_data, aggregation_steps = seq(from = 0, to = 1.5, by = 0.5))
            expect_equal(object = square_result[,"AggregationStep"],
                         expected = seq(from = 0, to = 1.5, by = 0.5))
            expect_equal(object = square_result[,"SpatialUnit_Count"],
                         expected = c(300, 234, 234, 176))
            expect_equal(object = square_result[,"SpatialUnit_Size"],
                         expected = c(1, 5, 9, 21))
            expect_equal(object = square_result[,"UniqueCombination_Count"],
                         expected = c(13, 18, 18, 11))
            expect_equal(object = round(square_result[,"CD_bit"], digits = 2),
                         expected = c(2.76, 3.57, 3.18, 2.17))
            expect_equal(object = round(square_result[,"AS_bit"], digits = 2),
                         expected = c(0.17, 0.75, 1.09, 1.25))
          }
)


test_that(desc = "appropriate setting - hexagonal grid",
          code = {
            hexagonal_result <- LandComp(x = hexagonal_data, aggregation_steps = c(0, 1, 1.5, 2))
            expect_equal(object = hexagonal_result[,"AggregationStep"],
                         expected = c(0, 1, 1.5, 2))
            expect_equal(object = hexagonal_result[,"SpatialUnit_Count"],
                         expected = c(300, 234, 187, 176))
            expect_equal(object = hexagonal_result[,"SpatialUnit_Size"],
                         expected = c(1, 7, 13, 19))
            expect_equal(object = hexagonal_result[,"UniqueCombination_Count"],
                         expected = c(12, 16, 21, 21))
            expect_equal(object = round(hexagonal_result[,"CD_bit"], digits = 2),
                         expected = c(1.97, 3.42, 3.49, 2.85))
            expect_equal(object = round(hexagonal_result[,"AS_bit"], digits = 2),
                         expected = c(0.13, 0.54, 0.55, 0.44))
          }
)
