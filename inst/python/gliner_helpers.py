"""GLiNER2 helpers for macrox — local NLP-based field extraction.

Single-page calls use extract_entities / extract_json (one text, one pass).
Multi-page calls use batch_extract_entities / batch_extract_json (list of
texts, one forward pass across all pages).  The R side passes a Python list
when multiple pages are requested and a plain string for a single page.
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
            "Run macrox::setup_gliner() to install it."
        )
    _gliner_model = GLiNER2.from_pretrained(model_name)
    _gliner_model_name = model_name


def gliner_loaded_model():
    """Return the name of the currently loaded model, or None."""
    return _gliner_model_name


def _cap(t):
    return str(t)[:20000]


def _entities_from_result(result, label):
    entities = result.get("entities", {}) if isinstance(result, dict) else {}
    return [str(m).strip() for m in entities.get(label, []) if str(m).strip()]


def gliner_extract_item(text, label, description, all_matches=False):
    """Extract a field from one page (str) or multiple pages (list of str).

    Single page  → extract_entities()        (one inference pass)
    Multi-page   → batch_extract_entities()  (one forward pass, N texts)

    Matches from all pages are pooled.  all_matches=False returns the first
    match found; all_matches=True returns every match across all pages.
    """
    if _gliner_model is None:
        raise RuntimeError("GLiNER2 model not loaded. Call setup_gliner() first.")

    label_map = {label: description}

    if isinstance(text, list):
        texts   = [_cap(t) for t in text]
        results = _gliner_model.batch_extract_entities(texts, label_map)
        matches = []
        for r in results:
            matches.extend(_entities_from_result(r, label))
    else:
        result  = _gliner_model.extract_entities(_cap(text), label_map)
        matches = _entities_from_result(result, label)

    if all_matches:
        return matches
    return matches[0] if matches else None


def gliner_batch_extract(text, items, all_matches=False):
    """Extract multiple named fields from one page (str) or multiple (list).

    Single page  → extract_entities()        (one pass, all fields)
    Multi-page   → batch_extract_entities()  (one forward pass, N texts)

    Returns {label: first_match_or_None} or {label: [all_matches]}.
    """
    if _gliner_model is None:
        raise RuntimeError("GLiNER2 model not loaded. Call setup_gliner() first.")
    if not items:
        return {}

    label_map = dict(items)

    if isinstance(text, list):
        texts   = [_cap(t) for t in text]
        results = _gliner_model.batch_extract_entities(texts, label_map)
        out = {lbl: [] for lbl in label_map}
        for r in results:
            for lbl in label_map:
                out[lbl].extend(_entities_from_result(r, lbl))
        return {
            lbl: (ms if all_matches else (ms[0] if ms else None))
            for lbl, ms in out.items()
        }
    else:
        result   = _gliner_model.extract_entities(_cap(text), label_map)
        entities = result.get("entities", {}) if isinstance(result, dict) else {}
        return {
            lbl: (
                [str(m).strip() for m in entities.get(lbl, []) if str(m).strip()]
                if all_matches else
                next((str(m).strip() for m in entities.get(lbl, []) if str(m).strip()), None)
            )
            for lbl in label_map
        }


def gliner_extract_struct(text, entity, field_specs):
    """Extract structured records from one page (str) or multiple (list).

    Single page  → extract_json()        (one pass)
    Multi-page   → batch_extract_json()  (one forward pass, N texts)

    Always requests per-field confidence scores. Records from all pages are
    combined into a single flat list. Each record contains data fields plus a
    '_confidence' dict of {field_name: score} when the model provides it.

    Parameters
    ----------
    text        : str or list of str
    entity      : str  — entity type name in the schema
    field_specs : list of str  — pre-formatted GLiNER2 schema strings, e.g.:
                    "name::str::Full product name"
                    "tier::[basic|premium]::str::Subscription level"
                    "features::list::Key features"
    """
    if _gliner_model is None:
        raise RuntimeError("GLiNER2 model not loaded. Call setup_gliner() first.")

    schema = {entity: list(field_specs)}

    if isinstance(text, list):
        texts   = [_cap(t) for t in text]
        results = _gliner_model.batch_extract_json(
            texts, schema, include_confidence=True
        )
        records = []
        for r in results:
            if isinstance(r, dict):
                records.extend(r.get(entity, []))
    else:
        result = _gliner_model.extract_json(
            _cap(text), schema, include_confidence=True
        )
        records = result.get(entity, []) if isinstance(result, dict) else []

    return records


def gliner_clear():
    """Release the loaded GLiNER2 model."""
    global _gliner_model, _gliner_model_name
    _gliner_model = None
    _gliner_model_name = None
