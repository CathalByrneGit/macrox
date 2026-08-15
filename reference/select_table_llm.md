# Extract a table using an LLM

Renders the PDF page (optionally cropped to `area`), sends it to an LLM
with a structured schema, and returns a clean data frame. Good for
multi-level headers, tables embedded in mixed-content pages, and any
situation where positional extraction fails.

## Usage

``` r
select_table_llm(
  sess,
  label,
  page,
  area = NULL,
  chat = NULL,
  provider = getOption("macrox.llm.provider", "anthropic"),
  model = getOption("macrox.llm.model", NULL),
  base_url = NULL,
  schema = NULL,
  prompt = NULL,
  dpi = 150L,
  header_rows = 1L,
  mode = c("structured", "page"),
  table_index = 1L
)
```

## Arguments

- sess:

  A `macrox_session` object.

- label:

  Character label for the extracted table.

- page:

  Page number (integer).

- area:

  Named numeric vector `c(top, left, bottom, right)` in PDF points. When
  supplied the image sent to the LLM is cropped to that region, which
  improves accuracy on pages with multiple tables.

- chat:

  An existing `ellmer` Chat object, e.g.
  [`ellmer::chat_anthropic()`](https://ellmer.tidyverse.org/reference/chat_anthropic.html),
  [`ellmer::chat_openrouter()`](https://ellmer.tidyverse.org/reference/chat_openrouter.html),
  or
  `ellmer::chat_openai_compatible(base_url = "http://localhost:11434/v1", model = "llama2")`.
  The chat is cloned before use (its history is left untouched). When
  supplied, `provider`, `model`, and `base_url` are ignored for the call
  but recorded (derived from the chat object) so the step can be
  replayed.

- provider:

  Name of an `ellmer` chat constructor, without the `chat_` prefix, e.g.
  `"anthropic"` (default), `"openai"`, `"google_gemini"`,
  `"openrouter"`, `"groq"`, `"ollama"`, or `"openai_compatible"`. Any
  provider for which `ellmer` exports `chat_<provider>()` is supported.
  Ignored when `chat` is supplied.

- model:

  Model name. `NULL` uses a sensible default for `"anthropic"`,
  `"openai"`, and `"google_gemini"`; other providers require `model` to
  be set explicitly. Ignored when `chat` is supplied.

- schema:

  Named character vector mapping column names to R types:
  `c(Month = "character", Male = "integer", Total = "integer")`.
  Supported types: `"character"`, `"integer"`, `"numeric"`. `NULL`
  (default) asks the LLM to auto-detect the columns.

- prompt:

  Optional extra instructions appended to the base prompt, e.g.
  `"Ignore the footnote row at the bottom."`.

- dpi:

  Render resolution for the page image (default 150).

- header_rows:

  Number of header rows. When \> 1 the prompt asks the LLM to flatten
  them with `_` separators. Ignored in `"page"` mode.

- mode:

  Extraction mode:

  - `"structured"` (default) — renders the area (or full page if no
    `area`) and asks the LLM to return a typed JSON object. Best when
    you know the table's location and can supply a schema.

  - `"page"` — renders the full page, asks the LLM to convert it to
    markdown, and parses the resulting markdown tables. No schema or
    area is required; good for quick extraction when the layout is
    unknown. `area` is ignored. Use `table_index` to pick among multiple
    tables.

- table_index:

  When `mode = "page"` and multiple tables are detected on the page,
  which one to return (default 1).

## Value

`sess` invisibly (step is recorded).

## Details

Requires the `ellmer` package (`install.packages("ellmer")`) and an API
key for the chosen provider set as an environment variable
(`ANTHROPIC_API_KEY`, `OPENAI_API_KEY`, `GEMINI_API_KEY`).
