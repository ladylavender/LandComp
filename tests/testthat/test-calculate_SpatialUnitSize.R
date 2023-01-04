data("square_data")
data("hexagonal_data")

test_that(desc = "inappropriate aggregation_step",
          code = {
            expect_error(object = calculate_SpatialUnitSize(aggregation_step = TRUE, square = TRUE))
            expect_warning(expect_error(object = calculate_SpatialUnitSize(aggregation_step = "foo", square = TRUE)))
            expect_warning(expect_error(object = calculate_SpatialUnitSize(aggregation_step = NA, square = TRUE)))
            expect_warning(expect_error(object = calculate_SpatialUnitSize(aggregation_step = Inf, square = TRUE)))

            expect_error(object = calculate_SpatialUnitSize(aggregation_step = -1, square = TRUE))
            expect_error(object = calculate_SpatialUnitSize(aggregation_step = -10, square = FALSE))

            expect_warning(object = calculate_SpatialUnitSize(aggregation_step = 0:2, square = TRUE))
            expect_warning(expect_error(object = calculate_SpatialUnitSize(aggregation_step = -2:10, square = FALSE)))
          }
)


test_that(desc = "inappropriate square",
          code = {
            expect_error(object = calculate_SpatialUnitSize(aggregation_step = 1, square = "foo"))
            expect_error(object = calculate_SpatialUnitSize(aggregation_step = 2, square =  100))

            expect_warning(object = calculate_SpatialUnitSize(aggregation_step = 3, square = c(TRUE, FALSE)))
            expect_error(object = calculate_SpatialUnitSize(aggregation_step = 4, square = NA))
          }
)


test_that(desc = "aggregation step in the interval ]0;1[ when using hexagonal grid",
          code = {
            expect_error(object = calculate_SpatialUnitSize(aggregation_step = 0.5, square = FALSE))
          }
)


test_that(desc = "appropriate setting",
          code = {
            expect_equal(object = calculate_SpatialUnitSize(aggregation_step = 0, square = TRUE), 1)
            expect_equal(object = calculate_SpatialUnitSize(aggregation_step = 0.5, square = TRUE), 5)
            expect_equal(object = calculate_SpatialUnitSize(aggregation_step = 1, square = TRUE), 9)
            expect_equal(object = calculate_SpatialUnitSize(aggregation_step = 1.5, square = TRUE), 21)
            expect_equal(object = calculate_SpatialUnitSize(aggregation_step = 2, square = TRUE), 25)

            expect_equal(object = calculate_SpatialUnitSize(aggregation_step = 0, square = FALSE), 1)
            expect_equal(object = calculate_SpatialUnitSize(aggregation_step = 1, square = FALSE), 7)
            expect_equal(object = calculate_SpatialUnitSize(aggregation_step = 1.5, square = FALSE), 13)
            expect_equal(object = calculate_SpatialUnitSize(aggregation_step = 2, square = FALSE), 19)
          }
)
