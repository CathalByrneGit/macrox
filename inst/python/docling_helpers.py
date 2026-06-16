"""Docling helpers for pdfmacro — table extraction from PDF pages.

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
            "Run pdfmacro::setup_docling() to install it."
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

    tables = []
    for tbl in page_tables:
        df = tbl.export_to_dataframe(doc=doc)

        # Flatten MultiIndex columns (spanned headers)
        if df.columns.nlevels > 1:
            cols = [
                "_".join(str(x) for x in col if str(x)).strip("_")
                for col in df.columns
            ]
        else:
            cols = [str(c) for c in df.columns]

        # Convert cell values to plain strings
        data = [
            [str(cell) if cell is not None else "" for cell in row]
            for row in df.values.tolist()
        ]

        tables.append({"headers": cols, "data": data})

    return {"n_tables": len(tables), "tables": tables}


def docling_clear_cache():
    """Release all cached Docling documents."""
    _doc_cache.clear()
