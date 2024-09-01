library(digest)
library(testthat)
library(dplyr)

load('answers.rda')

args <- commandArgs(trailingOnly = TRUE)
question =args[1]


if (question == 'Q1') {
  print('Testing answer for Q1')

  test_that("Q1 - Is Answer Correct?", {
    expect_true(is.data.frame(artists))
    expect_equal(digest(ncol(artists), 'sha1'), "20864973f3a271ea8b27e2c75face8663d1a900c")
    expect_equal(digest(nrow(artists), 'sha1'), "d0e9195a496b8e9a38d71dd627a6bc8304043f18")
  }
  )
} else if (question == 'Q2') {
  print('Testing answer for Q2')

  test_that("Q2 - Is Answer Correct?", {
    expect_true(is.data.frame(artists_by_state))
    expect_equal(digest(artists_by_state %>% .[1,2] %>% as.vector(mode = 'numeric'), 'sha1'), "06b7dbd391ef2ef35d067409a18403dac66ccd19")
  }
  )
} else if (question == 'Q3') {
  print('Testing answer for Q3')

  test_that("Q3 - Is Answer Correct?", {
    expect_true(is.data.frame(avg_artists_share))
    expect_equal(digest(avg_artists_share %>%  .[1,2] %>% as.vector(mode = 'numeric'), 'sha1'), "34f7095d51f396ca6ee3da3c0aaaf19149b5e0fe")
    
  }
  )
} else if (question == 'Q4') {
  print('Testing answer for Q4')

  test_that("Q4 - Is Answer Correct?", {
    expect_true(is.data.frame(artists_state_race))
    expect_equal(digest(artists_state_race %>%  .[3,3] %>% as.vector(mode = 'numeric'), 'sha1'), "a85f2868b0339a40fcef70dd443129a95c856f68")
  }
  )
} else if (question == 'Q5') {
  print('Testing answer for Q5')

  test_that("Q5 - Is Answer Correct?", {
    expect_true(is.data.frame(non_designers))
    expect_equal(digest(non_designers %>%  .[1,2] %>% as.vector(mode = 'numeric'), 'sha1'), "9dc3d0ec39a2d689a5398bef09ebce02d28ae960")
  }
  )
} else {
  print('Not a valid argument')
}