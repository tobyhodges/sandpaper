# Tests for the post-processing applied to Quarto-rendered markdown.
# These test the regex transformations without needing Quarto/jupyter.

test_that("fenced div {.class} syntax is stripped", {
  lines <- c(
    "::: {.questions}",
    "- A question?",
    ":::",
    "",
    "::::: {.challenge}",
    "## Challenge",
    ":::::"
  )
  result <- gsub("^(:{3,})\\s*\\{\\.([-a-zA-Z0-9]+)\\}\\s*$", "\\1 \\2", lines)
  expect_equal(result[1], "::: questions")
  expect_equal(result[5], "::::: challenge")
  # Closing divs unchanged
  expect_equal(result[3], ":::")
})

test_that("escaped reference links are restored", {
  lines <- c(
    "See the \\[documentation\\]\\[docs\\] for details.",
    "Also \\[this link\\]\\[other\\].",
    "Normal [link](https://example.com) is unchanged."
  )
  result <- gsub("\\\\\\[(.+?)\\\\\\]\\\\\\[(.+?)\\\\\\]", "[\\1][\\2]", lines)
  expect_equal(result[1], "See the [documentation][docs] for details.")
  expect_equal(result[2], "Also [this link][other].")
  expect_equal(result[3], "Normal [link](https://example.com) is unchanged.")
})

test_that("GFM alert blockquotes are converted to Carpentries divs", {
  lines <- c(
    "Some text.",
    "",
    "> [!NOTE]",
    ">",
    "> ### My Note",
    ">",
    "> This is a note.",
    "",
    "> [!WARNING]",
    ">",
    "> A warning without a title.",
    "",
    "More text."
  )

  callout_map <- c(
    NOTE = "callout", TIP = "callout",
    WARNING = "caution", CAUTION = "caution", IMPORTANT = "caution"
  )
  out <- character(0)
  in_alert <- FALSE
  alert_class <- ""
  for (line in lines) {
    alert_match <- regmatches(line, regexec("^>\\s*\\[!(\\w+)\\]", line))[[1]]
    if (length(alert_match) == 2 && !in_alert) {
      alert_type <- alert_match[2]
      alert_class <- callout_map[alert_type]
      if (!is.na(alert_class)) {
        in_alert <- TRUE
        out <- c(out, paste(":::", alert_class))
        next
      }
    }
    if (in_alert) {
      if (!grepl("^>", line) && nzchar(trimws(line))) {
        in_alert <- FALSE
        out <- c(out, ":::", "", line)
      } else if (!grepl("^>", line) && !nzchar(trimws(line))) {
        in_alert <- FALSE
        out <- c(out, ":::", "")
      } else {
        out <- c(out, sub("^>\\s?", "", line))
      }
    } else {
      out <- c(out, line)
    }
  }
  if (in_alert) out <- c(out, ":::")

  # NOTE becomes callout
  expect_true(any(grepl("^::: callout$", out)))
  # WARNING becomes caution
  expect_true(any(grepl("^::: caution$", out)))
  # No GFM alert syntax remaining
  expect_false(any(grepl("\\[!NOTE\\]", out)))
  expect_false(any(grepl("\\[!WARNING\\]", out)))
  # Content is preserved
  expect_true(any(grepl("My Note", out)))
  expect_true(any(grepl("A warning without a title", out)))
  # Surrounding text preserved
  expect_equal(out[1], "Some text.")
  expect_true(any(grepl("More text.", out)))
})
