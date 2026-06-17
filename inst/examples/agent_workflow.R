# Agent workflow example — macrox + LLM integration
#
# This file shows how to use macrox's agent API functions with an LLM to
# automatically generate and validate extraction macros.

library(macrox)

# ── Step 1: Profile the PDF ──────────────────────────────────────────────────

profile <- mx_profile("my_report.pdf", pages = 1:20)
print(profile)

# ── Step 2: Pass profile to LLM ──────────────────────────────────────────────
#
# Use .agent_system_prompt() to get a system prompt, then send the profile
# as the user message.

# agent_prompt <- .agent_system_prompt()
# response     <- your_llm_call(system = agent_prompt,
#                                user   = jsonlite::toJSON(profile, auto_unbox = TRUE))
# macro_yaml   <- parse_yaml_from_response(response)

# ── Step 3: Validate the generated macro ─────────────────────────────────────

# result <- validate_macro("my_report.pdf", macro_yaml)
# print(result)
#
# if (!result$valid) {
#   # Feed errors back to LLM and regenerate
#   error_msg <- paste(result$errors, collapse = "\n")
#   response2 <- your_llm_call(system = agent_prompt,
#                               user   = paste("Fix these errors:\n", error_msg,
#                                              "\nOriginal macro:\n", macro_yaml))
# }

# ── Step 4: Replay ───────────────────────────────────────────────────────────

# tables <- mx_replay("my_report.pdf", macro_yaml)


# ── System prompt for the LLM ────────────────────────────────────────────────

.agent_system_prompt <- function() {
  paste0(
    "You are an expert at reading structured data from PDF reports and writing ",
    "macrox YAML extraction macros.\n\n",
    "## macrox macro YAML schema\n\n",
    "A macro is a YAML file with two top-level keys: `macro` (metadata) and `steps` (list).\n\n",
    "### Step types\n\n",
    "**select_table** — extract a table from the PDF\n",
    "  Required: step (\"select_table\"), label (string)\n",
    "  One of: page (integer) OR label_match (string for fuzzy search)\n",
    "  Optional: table_index (default 1), area (list: top/left/bottom/right in pts),\n",
    "            method (\"lattice\" or \"stream\", default \"lattice\"),\n",
    "            header_rows (integer, default 1), max_dist (default 0.2)\n\n",
    "**rename_columns** — rename raw PDF headers to clean names\n",
    "  Required: step (\"rename_columns\"), table (label), mapping (OldName: new_name)\n\n",
    "**cast_types** — parse columns to correct R types\n",
    "  Required: step (\"cast_types\"), table (label),\n",
    "            types (col_name: type where type is numeric/integer/character/date:<fmt>)\n\n",
    "**filter_rows** — remove rows matching an R expression\n",
    "  Required: step (\"filter_rows\"), table (label),\n",
    "            exclude_where (R expression string, e.g. \"month == 'Total'\")\n\n",
    "### Rules\n",
    "1. select_table must appear before any transforms on the same label.\n",
    "2. Use method: stream for whitespace-separated tables (no visible grid lines).\n",
    "3. Use header_rows: 2 when a table has merged spanning headers.\n",
    "4. Always add a filter_rows step to remove 'Total' or summary rows.\n",
    "5. Cast all numeric columns to integer or numeric for analysis.\n",
    "6. Use label_match only when page numbers may shift between report editions.\n\n",
    "### Example macro\n\n",
    "macro:\n",
    "  name: example_macro\n",
    "  created: '2026-01-01 00:00'\n",
    "  source: report.pdf\n",
    "  n_steps: 4\n",
    "steps:\n",
    "  - step: select_table\n",
    "    label: monthly_data\n",
    "    page: 5\n",
    "    table_index: 1\n",
    "    method: lattice\n",
    "  - step: rename_columns\n",
    "    table: monthly_data\n",
    "    mapping:\n",
    "      Month: month\n",
    "      Value: value\n",
    "  - step: cast_types\n",
    "    table: monthly_data\n",
    "    types:\n",
    "      value: integer\n",
    "  - step: filter_rows\n",
    "    table: monthly_data\n",
    "    exclude_where: \"month == 'Total'\"\n"
  )
}
