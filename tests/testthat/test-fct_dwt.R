mra_obj=wavelets::mra(as.matrix(data_waveleT[[2]]),n.levels=9,filter="la8", method="modwt")
mra_data=wav_mra_data(data_waveleT,mra_obj)
mra_sum <- wav_mra_sum(mra_data, "la8",c("D8","D9","S9"))

test_that("wav_mra_data() returns data.frame", {
  class_mra_data=class(mra_data)
  expect_true("data.frame" %in% class_mra_data)
})
test_that("wav_mra_sum() returns list with correct elements", {
  expect_true(is.list(mra_sum))
  expect_true(all(c("y","lab","n.boundary") %in% names(mra_sum)))
})
test_that("wav_mra_sum() returns correct length of y", {
  expect_equal(length(mra_sum$y), nrow(mra_data))
})
test_that("wav_mra_sum() returns Error when level asked is not in mra_data", {
  expect_error( wav_mra_sum(mra_data, "la8",c("D8","D9","S9","S12")))
})
