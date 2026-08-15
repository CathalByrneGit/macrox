# Launch the macrox standalone app

Opens a full Shiny application for interactive PDF table extraction,
cleaning, and export. The app is self-contained — no existing session or
macro is required.

## Usage

``` r
mx_app(viewer = c("browser", "dialog", "pane"))
```

## Arguments

- viewer:

  Where to display the app:

  - `"browser"` (default) — system web browser

  - `"dialog"` — floating RStudio dialog (1 200 × 900)

  - `"pane"` — RStudio Viewer pane

## Value

Called for its side-effect. Returns invisibly when the app is closed.
