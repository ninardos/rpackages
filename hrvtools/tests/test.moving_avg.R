library("testthat")
context("Moving average")

test_that
(
    "Moving Average is NA up to filter-1", 
	{
        expect_that(c(rep(NA,6),4), is_equivalent_to(as.numeric(moving_avg(1:7,7))))
    }
)