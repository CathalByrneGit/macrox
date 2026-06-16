"""GLiNER2 helpers for pdfmacro — local NLP-based field extraction.

The model is loaded once and reused across calls.  Use gliner_setup() to
load (or swap) the model and gliner_clear() to release it.
"""

_gliner_model = None
_gliner_model_name = None


def gliner_setup(model_name="fastino/gliner2-base-v1"):
    """Load a GLiNER2 model into memory."""
    global _gliner_model, _gliner_model_name
    try:
        from gliner2 import GLiNER2
    except ImportError:
        raise ImportError(
            "gliner2 is not installed. "
            "Run pdfmacro::setup_gliner() to install it."
        )
    _gliner_model = GLiNER2.from_pretrained(model_name)
    _gliner_model_name = model_name


def gliner_loaded_model():
    """Return the name of the currently loaded model, or None."""
    return _gliner_model_name


def gliner_extract_item(text, label, description):
    """Extract a single field value from text.

    Tries schema-based extraction (extract_json) first, then falls back to
    entity extraction.  Returns the first match as a string, or None.

    Parameters
    ----------
    text        : str  — document or page text
    label       : str  — field name (e.g. "invoice_number")
    description : str  — natural-language description / prompt
    """
    if _gliner_model is None:
        raise RuntimeError(
            "GLiNER2 model not loaded. Call setup_gliner() first."
        )

    text = str(text)[:20000]  # cap to avoid OOM on very long docs

    # ── Attempt 1: schema-based extraction ────────────────────────────────────
    try:
        schema = {"document": [f"{label}::str::{description}"]}
        result = _gliner_model.extract_json(text, schema)
        doc = result.get("document") if isinstance(result, dict) else None
        if doc is not None:
            if isinstance(doc, dict):
                val = doc.get(label)
            elif isinstance(doc, list) and doc:
                first = doc[0]
                val = first.get(label) if isinstance(first, dict) else None
            else:
                val = None
            if val is not None and str(val).strip():
                return str(val).strip()
    except Exception:
        pass

    # ── Attempt 2: entity extraction using label as entity type ───────────────
    try:
        result = _gliner_model.extract_entities(text, [label])
        entities = result.get("entities", {}) if isinstance(result, dict) else {}
        matches = entities.get(label, [])
        if matches:
            return str(matches[0]).strip()
    except Exception:
        pass

    return None


def gliner_extract_entities(text, entity_types):
    """Extract multiple entity types in one pass.

    Returns a dict of {entity_type: [value, ...]}.
    """
    if _gliner_model is None:
        raise RuntimeError(
            "GLiNER2 model not loaded. Call setup_gliner() first."
        )
    text = str(text)[:20000]
    result = _gliner_model.extract_entities(text, list(entity_types))
    return result.get("entities", {}) if isinstance(result, dict) else {}


def gliner_clear():
    """Release the loaded GLiNER2 model."""
    global _gliner_model, _gliner_model_name
    _gliner_model = None
    _gliner_model_name = None
