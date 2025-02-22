tmp <- res <- restore_fixture()

test_that("prefixed episodes can be created", {

  initial_episode <- fs::dir_ls(fs::path(tmp, "episodes"), glob = "*Rmd") %>%
    expect_length(1L) %>%
    expect_match("01-introduction.Rmd")

  second_episode <- create_episode_md("First Markdown", path = tmp) %>%
    expect_match("02-first-markdown.md", fixed = TRUE)

  ep1 <- readLines(initial_episode)
  ep2 <- readLines(second_episode)

  expect_equal(ep1[[2]], "title: 'introduction'")
  expect_true(any(grepl("^```[{]r pyramid", ep1))) # first episode will have R Markdown
  
  expect_equal(ep2[[2]], "title: 'First Markdown'")
  expect_no_match(ep2, "^```[{]r pyramid") # second episode will not have R Markdown
  expect_no_match(ep2, "^Or you") # second episode will not have R Markdown

})

test_that("un-prefixed episodes can be created", {

  skip_on_os("windows") # y'all ain't ready for this
  title <- "\uC548\uB155 :joy_cat: \U0001F62D KITTY"
  third_episode <- create_episode_rmd(title, make_prefix = FALSE, path = tmp) %>%
    expect_match("\uC548\uB155-\U0001F62D-kitty.Rmd", fixed = TRUE)

  expect_equal(readLines(third_episode, n = 2)[[2]], 
    paste0("title: '", title, "'"))
})
