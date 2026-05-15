# Tests for post-processing applied to Quarto-rendered markdown.
# These exercise the pure helpers in R/utils-quarto-postprocess.R so a
# drift in the production logic is caught, rather than re-implementing
# the transforms inside the test body.

# ---- strip_fenced_div_attrs() ----------------------------------------------

test_that("strip_fenced_div_attrs() rewrites {.class} to bare class", {
  lines <- c(
    "::: {.questions}",
    "- A question?",
    ":::",
    "",
    "::::: {.challenge}",
    "## Challenge",
    ":::::"
  )
  result <- sandpaper:::strip_fenced_div_attrs(lines)
  expect_equal(result[1], "::: questions")
  expect_equal(result[5], "::::: challenge")
  # Closing fences unchanged
  expect_equal(result[3], ":::")
  expect_equal(result[7], ":::::")
})

test_that("strip_fenced_div_attrs() leaves non-div lines alone", {
  lines <- c("Some prose.", "::: {.unrelated}", "> [!NOTE]")
  result <- sandpaper:::strip_fenced_div_attrs(lines)
  expect_equal(result[1], "Some prose.")
  expect_equal(result[2], "::: unrelated")
  expect_equal(result[3], "> [!NOTE]")
})

# ---- unescape_reference_links() --------------------------------------------

test_that("unescape_reference_links() restores escaped reference links", {
  lines <- c(
    "See the \\[documentation\\]\\[docs\\] for details.",
    "Also \\[this link\\]\\[other\\].",
    "Normal [link](https://example.com) is unchanged."
  )
  result <- sandpaper:::unescape_reference_links(lines)
  expect_equal(result[1], "See the [documentation][docs] for details.")
  expect_equal(result[2], "Also [this link][other].")
  expect_equal(result[3], "Normal [link](https://example.com) is unchanged.")
})

test_that("unescape_reference_links() handles a realistic gfm fixture", {
  # Canary against a realistic snippet produced by running Quarto's
  # gfm+fenced_divs writer over a .qmd episode that contained a
  # reference-style link definition and inline usage. If Quarto ever
  # changes its escaping, this fixture catches the drift.
  fixture <- c(
    "## Introduction",
    "",
    "This lesson was created via The Carpentries Workbench\\[^1\\]. For",
    "full documentation refer to the \\[Introduction to The Carpentries",
    "Workbench\\]\\[carpentries-workbench\\].",
    "",
    "See also the \\[pandoc\\]\\[pandoc\\] and \\[r-markdown\\]\\[r-markdown\\]",
    "references.",
    "",
    "  \\[carpentries-workbench\\]: https://carpentries.github.io/workbench/",
    "  \\[pandoc\\]: https://pandoc.org/MANUAL.html",
    "  \\[r-markdown\\]: https://rmarkdown.rstudio.com/"
  )
  result <- sandpaper:::unescape_reference_links(fixture)
  # Inline references are rewritten
  expect_true(any(grepl(
    "[Introduction to The Carpentries",
    result, fixed = TRUE
  )))
  expect_true(any(grepl("[pandoc][pandoc]", result, fixed = TRUE)))
  expect_true(any(grepl("[r-markdown][r-markdown]", result, fixed = TRUE)))
  # No escaped pairs remain in inline usage
  expect_false(any(grepl("\\\\\\[.+?\\\\\\]\\\\\\[.+?\\\\\\]", result)))
})

# ---- convert_gfm_alerts_to_callouts() --------------------------------------

test_that("convert_gfm_alerts_to_callouts() maps NOTE and WARNING", {
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
  out <- sandpaper:::convert_gfm_alerts_to_callouts(lines)

  expect_true(any(grepl("^::: callout$", out)))
  expect_true(any(grepl("^::: caution$", out)))
  expect_false(any(grepl("\\[!NOTE\\]", out)))
  expect_false(any(grepl("\\[!WARNING\\]", out)))
  expect_true(any(grepl("My Note", out)))
  expect_true(any(grepl("A warning without a title", out)))
  # Surrounding prose preserved
  expect_equal(out[1], "Some text.")
  expect_true(any(grepl("More text.", out)))
})

test_that("convert_gfm_alerts_to_callouts() closes an unterminated alert", {
  lines <- c(
    "> [!TIP]",
    ">",
    "> trailing alert at end of file"
  )
  out <- sandpaper:::convert_gfm_alerts_to_callouts(lines)
  expect_equal(out[1], "::: callout")
  expect_equal(out[length(out)], ":::")
})

test_that("convert_gfm_alerts_to_callouts() leaves unknown alert types alone", {
  lines <- c("> [!WEIRD]", ">", "> something odd")
  out <- sandpaper:::convert_gfm_alerts_to_callouts(lines)
  # Unknown type is not rewritten to a callout fence
  expect_false(any(grepl("^::: ", out)))
  expect_true(any(grepl("\\[!WEIRD\\]", out)))
})

test_that("convert_gfm_alerts_to_callouts() unwraps a nested blockquote once", {
  # A nested blockquote inside an alert is a rare pattern. The
  # transform strips one level of `>` so the inner blockquote survives
  # as a regular blockquote inside the callout. Deeper nesting would
  # need a real parser; this test pins the documented single-level
  # behaviour so regressions are visible.
  lines <- c(
    "> [!NOTE]",
    ">",
    "> A note containing a quotation:",
    ">",
    "> > quoted text",
    ">",
    "> resuming the note."
  )
  out <- sandpaper:::convert_gfm_alerts_to_callouts(lines)
  expect_equal(out[1], "::: callout")
  # Single-level unwrap: `> > quoted text` becomes `> quoted text`
  expect_true(any(grepl("^> quoted text$", out)))
  # Alert boundary is closed
  expect_equal(out[length(out)], ":::")
  # Prose around the nested quote is preserved
  expect_true(any(grepl("A note containing a quotation", out)))
  expect_true(any(grepl("resuming the note", out)))
})

# ---- read_qmd_frontmatter() ------------------------------------------------

test_that("read_qmd_frontmatter() returns YAML body lines without delimiters", {
  src <- withr::local_tempfile(fileext = ".qmd")
  writeLines(c(
    "---",
    "title: 'Introduction'",
    "teaching: 10",
    "exercises: 2",
    "---",
    "",
    "Body content here."
  ), src)
  yaml <- sandpaper:::read_qmd_frontmatter(src)
  expect_equal(yaml, c(
    "title: 'Introduction'",
    "teaching: 10",
    "exercises: 2"
  ))
})

test_that("read_qmd_frontmatter() returns character(0) when no frontmatter", {
  src <- withr::local_tempfile(fileext = ".qmd")
  writeLines(c("# Just an H1", "", "No YAML here."), src)
  expect_equal(sandpaper:::read_qmd_frontmatter(src), character(0))
})

test_that("read_qmd_frontmatter() returns character(0) on unclosed delimiter", {
  src <- withr::local_tempfile(fileext = ".qmd")
  writeLines(c("---", "title: 'unfinished'", "body without close"), src)
  expect_equal(sandpaper:::read_qmd_frontmatter(src), character(0))
})

test_that("read_qmd_frontmatter() returns character(0) for empty frontmatter", {
  src <- withr::local_tempfile(fileext = ".qmd")
  writeLines(c("---", "---", "body"), src)
  expect_equal(sandpaper:::read_qmd_frontmatter(src), character(0))
})

# ---- strip_leading_h1() ----------------------------------------------------

test_that("strip_leading_h1() removes the first H1 and its trailing blank", {
  lines <- c(
    "# Quarto features stress test",
    "",
    "::: questions",
    "- A question?",
    ":::"
  )
  result <- sandpaper:::strip_leading_h1(lines)
  expect_equal(result, c(
    "::: questions",
    "- A question?",
    ":::"
  ))
})

test_that("strip_leading_h1() handles a leading blank line before the H1", {
  lines <- c("", "# Title", "", "Body.")
  result <- sandpaper:::strip_leading_h1(lines)
  expect_equal(result, c("", "Body."))
})

test_that("strip_leading_h1() leaves H2+ headings alone", {
  lines <- c("## Section", "", "Body.")
  result <- sandpaper:::strip_leading_h1(lines)
  expect_equal(result, c("## Section", "", "Body."))
})

test_that("strip_leading_h1() is a no-op when there is no leading H1", {
  lines <- c("Just prose.", "", "More prose.")
  expect_equal(sandpaper:::strip_leading_h1(lines), lines)
})

# ---- postprocess_quarto_md() -----------------------------------------------

test_that("postprocess_quarto_md() composes all three transforms", {
  lines <- c(
    "::: {.questions}",
    "- See \\[docs\\]\\[docs\\].",
    ":::",
    "",
    "> [!NOTE]",
    ">",
    "> Heads up."
  )
  result <- sandpaper:::postprocess_quarto_md(lines)
  expect_equal(result[1], "::: questions")
  expect_true(any(grepl("[docs][docs]", result, fixed = TRUE)))
  expect_true(any(grepl("^::: callout$", result)))
  expect_false(any(grepl("\\[!NOTE\\]", result)))
})

test_that("postprocess_quarto_md() prepends source YAML and strips the title H1", {
  # Quarto's gfm writer drops the source frontmatter and converts the
  # `title:` field into a leading H1. The downstream sandpaper/pkgdown
  # phase reads `title`, `teaching`, `exercises` etc. from a YAML
  # header, so we reconstruct the header from the source .qmd and
  # remove the redundant H1.
  lines <- c(
    "# Introduction",
    "",
    "::: {.questions}",
    "- A question?",
    ":::"
  )
  yaml <- c("title: 'Introduction'", "teaching: 10", "exercises: 2")
  result <- sandpaper:::postprocess_quarto_md(lines, source_yaml = yaml)
  expect_equal(result[1], "---")
  expect_equal(result[2], "title: 'Introduction'")
  expect_equal(result[3], "teaching: 10")
  expect_equal(result[4], "exercises: 2")
  expect_equal(result[5], "---")
  expect_false(any(grepl("^# Introduction$", result)))
  expect_true(any(grepl("^::: questions$", result)))
})

test_that("postprocess_quarto_md() with NULL source_yaml does not modify the head", {
  # Backwards-compatible: when no YAML is supplied (legacy callers or
  # the existing inline tests), the H1 stays and no frontmatter is
  # added.
  lines <- c("# Introduction", "", "Body.")
  result <- sandpaper:::postprocess_quarto_md(lines, source_yaml = NULL)
  expect_equal(result[1], "# Introduction")
  expect_false(any(grepl("^---$", result)))
})
