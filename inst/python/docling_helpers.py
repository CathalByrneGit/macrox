"""Docling helpers for macrox — table extraction from PDF pages.

Documents are cached after conversion so repeated calls for different
tables on the same page do not re-run the full pipeline.
"""

_doc_cache = {}  # keyed by (str(pdf_path), int(page_no))


def docling_extract_tables(pdf_path, page_no):
    """Extract all tables from one PDF page using Docling.

    Parameters
    ----------
    pdf_path : str
        Absolute path to the PDF file.
    page_no : int
        1-based page number to extract tables from.

    Returns
    -------
    dict with keys:
        n_tables : int
        tables   : list of {"headers": [...], "data": [[...],...]}
    """
    try:
        from docling.document_converter import DocumentConverter
    except ImportError:
        raise ImportError(
            "docling is not installed. "
            "Run macrox::setup_docling() to install it."
        )

    cache_key = (str(pdf_path), int(page_no))
    if cache_key not in _doc_cache:
        converter = DocumentConverter()
        result = converter.convert(
            str(pdf_path),
            page_range=(int(page_no), int(page_no)),
        )
        _doc_cache[cache_key] = result.document

    doc = _doc_cache[cache_key]

    page_tables = [
        t for t in doc.tables
        if t.prov and t.prov[0].page_no == int(page_no)
    ]

    tables = [_tbl_to_dict(t, doc) for t in page_tables]
    return {"n_tables": len(tables), "tables": tables}


def docling_detect_range(pdf_path, pages):
    """Convert a page range in one pass and populate the per-page cache.

    Converts all pages in *pages* with a single DocumentConverter call, then
    writes a cache entry for every requested page so that subsequent
    docling_extract_tables() calls are served from cache without re-converting.

    Parameters
    ----------
    pdf_path : str
    pages    : list[int]  1-based page numbers to detect

    Returns
    -------
    list of dicts, one per page:
        {"page": int, "n_tables": int, "tables": [{"headers": [...], "data": [[...]]}]}
    """
    try:
        from docling.document_converter import DocumentConverter
    except ImportError:
        raise ImportError(
            "docling is not installed. "
            "Run macrox::setup_docling() to install it."
        )

    pages = [int(p) for p in pages]
    start, end = min(pages), max(pages)

    converter = DocumentConverter()
    result = converter.convert(str(pdf_path), page_range=(start, end))
    doc = result.document

    # Populate per-page cache so extract calls are free
    for pg in pages:
        cache_key = (str(pdf_path), pg)
        if cache_key not in _doc_cache:
            _doc_cache[cache_key] = doc

    out = []
    for pg in sorted(pages):
        page_tables = [
            t for t in doc.tables
            if t.prov and t.prov[0].page_no == pg
        ]
        tables = []
        for tbl in page_tables:
            tables.append(_tbl_to_dict(tbl, doc))
        out.append({"page": pg, "n_tables": len(tables), "tables": tables})

    return out


def _tbl_to_dict(tbl, doc):
    """Convert a Docling TableItem to {"headers": [...], "data": [[...]]}."""
    df = tbl.export_to_dataframe(doc=doc)

    if df.columns.nlevels > 1:
        cols = [
            "_".join(str(x) for x in col if str(x)).strip("_")
            for col in df.columns
        ]
    else:
        cols = [str(c) for c in df.columns]

    data = [
        [str(cell) if cell is not None else "" for cell in row]
        for row in df.values.tolist()
    ]
    return {"headers": cols, "data": data}


def docling_clear_cache():
    """Release all cached Docling documents."""
    _doc_cache.clear()
