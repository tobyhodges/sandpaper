# Post-processing for markdown rendered by Quarto (gfm+fenced_divs).
#
# Quarto's gfm+fenced_divs writer is close to what sandpaper/pegboard
# expect, but it differs in four ways that need to be normalised before
# the file flows through the rest of the build pipeline:
#
#   1. Fenced divs are emitted with attribute syntax (`::: {.questions}`)
#      rather than the bare class form (`::: questions`) that pegboard
#      parses.
#   2. Reference links are escaped as `\[text\]\[ref\]` in the writer's
#      output; pegboard expects unescaped `[text][ref]`.
#   3. Quarto callouts round-trip through gfm as GitHub alert
#      blockquotes (`> [!NOTE]`), which sandpaper does not recognise;
#      these are remapped to Carpentries `::: callout` / `::: caution`
#      fenced divs.
#   4. The source YAML frontmatter is dropped and the `title:` field is
#      emitted as a leading H1. sandpaper's downstream pkgdown phase
#      reads `title`, `teaching`, `exercises` etc. from a YAML header,
#      so we reconstruct it from the source `.qmd` and strip the
#      redundant H1. See `postprocess_quarto_md(source_yaml = ...)`.
#
# Each transform is a pure function over a character vector so the
# logic can be exercised in tests without invoking Quarto.

# Map from GitHub alert types to Carpentries callout classes. Anything
# not in this map is left as a plain alert blockquote.
gfm_alert_callout_map <- c(
  NOTE      = "callout",
  TIP       = "callout",
  WARNING   = "caution",
  CAUTION   = "caution",
  IMPORTANT = "callout"
)

postprocess_quarto_md <- function(lines, source_yaml = NULL) {
  lines <- strip_fenced_div_attrs(lines)
  lines <- unescape_reference_links(lines)
  lines <- convert_gfm_alerts_to_callouts(lines)
  if (!is.null(source_yaml) && length(source_yaml) > 0) {
    lines <- strip_leading_h1(lines)
    lines <- c("---", source_yaml, "---", "", lines)
  }
  lines
}

# Read the YAML frontmatter from a .qmd source file as a character
# vector of body lines (excluding the `---` delimiters). Returns
# `character(0)` when the file has no frontmatter, an unclosed
# delimiter, or an empty frontmatter block.
read_qmd_frontmatter <- function(path) {
  lines <- readLines(path, encoding = "UTF-8", warn = FALSE)
  if (length(lines) < 2L || !identical(trimws(lines[1]), "---")) {
    return(character(0))
  }
  close_idx <- which(trimws(lines[-1]) == "---")
  if (length(close_idx) == 0L) {
    return(character(0))
  }
  end <- close_idx[1] + 1L
  if (end <= 2L) {
    return(character(0))
  }
  lines[2:(end - 1L)]
}

# Strip the first H1 (and a single trailing blank line, if present)
# from a rendered markdown body. Leading blank lines before the H1
# are preserved. H2+ headings are not touched.
strip_leading_h1 <- function(lines) {
  i <- 1L
  while (i <= length(lines) && !nzchar(trimws(lines[i]))) {
    i <- i + 1L
  }
  if (i > length(lines) || !grepl("^#\\s", lines[i])) {
    return(lines)
  }
  out <- lines[-i]
  # Also strip the blank line directly following the H1, if any
  if (i <= length(out) && !nzchar(trimws(out[i]))) {
    out <- out[-i]
  }
  out
}

# `::: {.questions}` -> `::: questions`
strip_fenced_div_attrs <- function(lines) {
  gsub("^(:{3,})\\s*\\{\\.([-a-zA-Z0-9]+)\\}\\s*$", "\\1 \\2", lines)
}

# `\[text\]\[ref\]` -> `[text][ref]`
#
# Quarto's gfm writer escapes the square brackets in reference-style
# links. If Quarto ever stops escaping, this regex silently becomes a
# no-op, so test-qmd-postprocess.R exercises it against a realistic
# rendered fixture to act as a canary.
unescape_reference_links <- function(lines) {
  gsub("\\\\\\[(.+?)\\\\\\]\\\\\\[(.+?)\\\\\\]", "[\\1][\\2]", lines)
}

# Convert GitHub alert blockquotes to Carpentries fenced divs.
#
# An alert starts with a `> [!TYPE]` line and continues until the first
# line that is not part of the blockquote. Inside the block, the leading
# `>` is stripped so the content lands inside the fenced div.
#
# Limitation: a nested blockquote inside an alert (`> > quoted`) has its
# outer `>` stripped to `> quoted`, which is the correct unwrap for one
# level of nesting but is not round-trip safe for deeper nesting. Nested
# blockquotes inside Carpentries callouts are rare enough that we accept
# the single-level behaviour rather than carry a full parser.
convert_gfm_alerts_to_callouts <- function(lines) {
  out <- character(0)
  in_alert <- FALSE
  for (line in lines) {
    alert_match <- regmatches(line, regexec("^>\\s*\\[!(\\w+)\\]", line))[[1]]
    if (length(alert_match) == 2 && !in_alert) {
      alert_class <- gfm_alert_callout_map[alert_match[2]]
      if (!is.na(alert_class)) {
        in_alert <- TRUE
        out <- c(out, paste(":::", alert_class))
        next
      }
    }
    if (in_alert) {
      if (!grepl("^>", line) && nzchar(trimws(line))) {
        # Non-blockquote, non-empty line: alert ends, line belongs after
        in_alert <- FALSE
        out <- c(out, ":::", "", line)
      } else if (!grepl("^>", line) && !nzchar(trimws(line))) {
        # Blank line: alert ends
        in_alert <- FALSE
        out <- c(out, ":::", "")
      } else {
        # Strip one level of `>` and optional following space
        out <- c(out, sub("^>\\s?", "", line))
      }
    } else {
      out <- c(out, line)
    }
  }
  if (in_alert) {
    out <- c(out, ":::")
  }
  out
}
