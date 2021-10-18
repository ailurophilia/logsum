test_that("Log(sum(exp(1:10)) )= sum_of_logs(1:10)", {
  expect_equal(sum_of_logs(1:10), log(sum(exp(1:10))))
})

test_that("Log(sum(exp(-1:10)) )= sum_of_logs(-1:10)", {
  expect_equal(sum_of_logs(-1:10), log(sum(exp(-1:10))))
})

test_that("Log(exp(1:10) + exp(10:1))= sum_of_logs(1:10,10:1)", {
  expect_equal(sum_of_logs(1:10,10:1), log(exp(10:1) + exp(1:10)))
})


test_that("Log(exp(1:10) + exp(10:1))= sum_of_logs(1:10,10:1)", {
  expect_equal(sum_of_logs(1:10,10:1), log(exp(10:1) + exp(1:10)))
})

test_that("Log(exp(1:10) + exp(10:1))= sum_of_logs(1:10,10:1)", {
  expect_equal(diff_of_logs(1:10,0:9), log(exp(1:10) - exp(0:9)))
})

test_that("signed_log_sum(1:10,rep(c(-1,1),5))[1] gives same answer as direct evaluation.",{
  expect_equal(signed_log_sum(1:10,rep(c(-1,1),5))[1],
               log(sum(exp(seq(2,10,2))) - sum(exp(seq(1,9,2)))))
      })
