# macrox MCP Agent Interface

## Tools

- `initialize_session(file_path)` → `mx_profile(file_path)`
- `test_parameters(file, page, area, method, header_rows)` →
  `test_extraction(...)`
- `lock_macro(sess, name, path)` → `save_macro(...)`

## Agent feedback loop

[`test_extraction()`](https://cathalbyrnegit.github.io/macrox/reference/test_extraction.md)
returns `status`, `metrics`, `guidance`, `preview`. Loop until
`status == "success"` then
[`save_macro()`](https://cathalbyrnegit.github.io/macrox/reference/save_macro.md).
