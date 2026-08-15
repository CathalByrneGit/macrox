# Configure a session-level default LLM

Sets a single LLM configuration for the session so that
[`select_table_llm()`](https://cathalbyrnegit.github.io/macrox/reference/select_table_llm.md)
and
[`select_item()`](https://cathalbyrnegit.github.io/macrox/reference/select_item.md)
can be called without specifying `provider`, `model`, or `chat` on every
call. The configured chat object is cloned for each extraction so its
conversation history is never shared between calls.

## Usage

``` r
mx_configure_llm(
  sess,
  chat = NULL,
  provider = getOption("macrox.llm.provider", "anthropic"),
  model = getOption("macrox.llm.model", NULL),
  base_url = NULL,
  system = NULL
)
```

## Arguments

- sess:

  A `macrox_session` object.

- chat:

  An existing `ellmer` Chat object, e.g.
  [`ellmer::chat_anthropic()`](https://ellmer.tidyverse.org/reference/chat_anthropic.html).
  When supplied, `provider`, `model`, `base_url`, and `system` are
  ignored.

- provider:

  Provider name (default `"anthropic"`). Any provider for which `ellmer`
  exports `chat_<provider>()` is supported. Ignored when `chat` is
  supplied.

- model:

  Model name. `NULL` uses a sensible default for the provider. Ignored
  when `chat` is supplied.

- base_url:

  Base URL for `"openai_compatible"` providers.

- system:

  System prompt passed to the provider constructor. `NULL` (default)
  uses a built-in extraction-focused prompt for the `"anthropic"`
  provider; other providers receive no default. Ignored when `chat` is
  supplied.

## Value

`sess` invisibly.

## Examples

``` r
if (FALSE) { # \dontrun{
sess <- mx_session("report.pdf")

# Use Anthropic with a specific model — all subsequent LLM calls use it
sess |> mx_configure_llm(provider = "anthropic", model = "claude-opus-4-8")
sess |> select_table_llm("my_table", page = 5)
sess |> select_item("date", prompt = "Publication date")

# Or pass a fully configured chat object for maximum control
chat <- ellmer::chat_anthropic(
  model  = "claude-opus-4-8",
  system = "Extract data exactly as shown."
)
sess |> mx_configure_llm(chat = chat)
} # }
```
