"""
pdfmacro PaddleOCR helpers
--------------------------
Bundled Python code loaded via reticulate::source_python().

2-tier extraction strategy
  Tier 1 - ONNX Runtime (backend="onnxruntime" or "auto"):
    PaddleX table_recognition pipeline fed directly with the pre-cropped
    table image, bypassing PP-DocBlockLayout layout detection entirely.
    Lightweight, no paddlepaddle required.

  Tier 2 - PaddlePaddle (backend="paddle" or "auto" fallback):
    Full PPStructureV3 pipeline with all preprocessing models.
    Handles rotated/warped/complex scans.  Requires paddlepaddle.

backend="auto"         tries Tier 1; escalates to Tier 2 if empty and paddle available.
backend="onnxruntime"  forces Tier 1 only.
backend="paddle"       forces Tier 2 only; errors if paddlepaddle absent.

pdfmacro_paddle_extract() returns a dict:
  {"tier": "onnxruntime" | "paddle" | "none", "tables": [<pred_html str>, ...]}
so callers can confirm which backend actually produced the result.
"""

import glob
import json
import os
import shutil
import tempfile

# PaddlePaddle 3.x: disable PIR executor and OneDNN to avoid
# NotImplementedError: ConvertPirAttribute2RuntimeAttribute not support
# pir::ArrayAttribute<pir::DoubleAttribute> in onednn_instruction.cc
os.environ["FLAGS_use_mkldnn"]                   = "0"
os.environ["PADDLE_DISABLE_ONEDNN"]              = "1"
os.environ["FLAGS_enable_pir_in_executor"]       = "0"
os.environ["FLAGS_enable_pir_with_pt_frontend"]  = "0"
os.environ["FLAGS_new_executor_use_local_scope"] = "0"
# PaddleX selects its own "mkldnn" run_mode per-model regardless of the
# FLAGS_* settings above, which re-triggers the same PIR/oneDNN bug. This
# is the actual switch that disables it.
os.environ["PADDLE_PDX_ENABLE_MKLDNN_BYDEFAULT"] = "False"

# Default text-detection image size limit.
# PP-StructureV3 scales the input so its longest edge equals this value before
# running the text detector. The PaddleOCR default is 960, which scales a
# full A4 page at 200 DPI (~2340 px tall) down to ~410×960 — too small for
# dense numeric tables. 1920 keeps 2× more detail while still being
# manageable on CPU.
_TEXT_DET_LIMIT = 1920


# --------------------------------------------------------------------------- #
#  Main entry point                                                             #
# --------------------------------------------------------------------------- #

def pdfmacro_paddle_extract(img_path, use_gpu=False, show_log=False,
                             backend="auto", debug=False):
    """Extract table HTML strings from a pre-cropped table image."""

    os.environ.setdefault("PADDLE_PDX_DISABLE_MODEL_SOURCE_CHECK", "True")

    device = "gpu" if use_gpu else "cpu"

    if debug:
        import shutil as _sh
        debug_path = img_path + "_pdfmacro_debug.png"
        _sh.copy2(img_path, debug_path)
        print("[pdfmacro debug] image saved to: " + debug_path, flush=True)

    # Mild PIL enhancement: slightly boosts contrast and sharpens edges.
    # Helps the text detector pick up small digits that would otherwise fall
    # below the pixel-probability threshold after the image is downscaled.
    #_enhance_image(img_path)

    if backend in ("auto", "onnxruntime"):
        result = _try_onnxruntime(img_path, device, debug=debug)
        if result is not None:
            if result or backend == "onnxruntime":
                return {"tier": "onnxruntime", "tables": result}

    if backend in ("auto", "paddle"):
        tables = _try_paddle(img_path, device, required=(backend == "paddle"), debug=debug)
        return {"tier": "paddle", "tables": tables}

    return {"tier": "none", "tables": []}


# --------------------------------------------------------------------------- #
#  Image preprocessing                                                         #
# --------------------------------------------------------------------------- #

def _enhance_image(img_path):
    """Mild PIL contrast + sharpening pass before OCR.

    Safe on clean PDF renders (no clipping, no ringing).  Falls back silently
    if Pillow is unavailable.
    """
    try:
        from PIL import Image, ImageEnhance, ImageFilter
        img = Image.open(img_path)
        if img.mode != "RGB":
            img = img.convert("RGB")
        # 1.2× contrast lift — recovers detail that gets lost in downscaling
        img = ImageEnhance.Contrast(img).enhance(1.2)
        # Unsharp mask: radius=1 px, 120% strength, threshold=3 levels
        # Sharpens digit edges without introducing ringing on clean renders
        img = img.filter(ImageFilter.UnsharpMask(radius=1, percent=120, threshold=3))
        img.save(img_path, "PNG", optimize=False)
    except Exception:
        pass


# --------------------------------------------------------------------------- #
#  Tier implementations                                                        #
# --------------------------------------------------------------------------- #

def _try_onnxruntime(img_path, device, debug=False):
    """Tier 1: feed pre-cropped image directly into table_recognition pipeline."""
    os.environ["PADDLE_INFERENCE_BACKEND"]     = "onnxruntime"
    os.environ["PADDLE_PDX_INFERENCE_BACKEND"] = "onnxruntime"

    # Strategy A: PPStructureV3 with doc preprocessors disabled (clean PDF renders
    # don't need orientation correction or unwarping) and text detection scaled to
    # _TEXT_DET_LIMIT on the longest side. The web demo uses text_det_limit_type="min"
    # with side_len=64, which effectively disables downscaling — but DBNet was trained
    # at ~960 px scale, so passing unresized images at full DPI merges nearby cells.
    # Using type="max" keeps the detector in its trained scale range.
    try:
        from paddleocr import PPStructureV3
        try:
            pipe = PPStructureV3(
                device                       = device,
                use_doc_orientation_classify = False,
                use_doc_unwarping            = False,
                use_textline_orientation     = False,
                text_det_limit_type          = "max",
                text_det_limit_side_len      = _TEXT_DET_LIMIT,
                use_wired_table_rec          = True,
                use_wireless_table_rec       = False,
            )
        except TypeError:
            pipe = PPStructureV3(device=device)
        result = _run_pipeline(pipe, img_path, debug=debug)
        if result is not None:
            return result
    except Exception as e:
        if debug:
            import traceback
            print("[pdfmacro debug] Strategy A (PPStructureV3) failed: " + str(e), flush=True)
            traceback.print_exc()

    # Strategy B: PaddleX table_recognition pipeline fallback.
    try:
        from paddlex import create_pipeline
        try:
            pipe = create_pipeline(
                pipeline             = "table_recognition",
                device               = device,
                use_doc_preprocessor = False,
                use_wired_table_rec         = False,
                use_wireless_table_rec      = False,
            )
        except TypeError:
            try:
                pipe = create_pipeline(
                    pipeline             = "table_recognition",
                    device               = device,
                    use_doc_preprocessor = False,
                )
            except TypeError:
                pipe = create_pipeline(pipeline="table_recognition", device=device)
        return _run_pipeline_paddlex(pipe, img_path, debug=debug)
    except Exception as e:
        if debug:
            import traceback
            print("[pdfmacro debug] Strategy B (table_recognition) failed: " + str(e), flush=True)
            traceback.print_exc()
    return None


def _try_paddle(img_path, device, required=False, debug=False):
    """Tier 2: PPStructureV3 with table-optimised settings."""
    os.environ.pop("PADDLE_INFERENCE_BACKEND",     None)
    os.environ.pop("PADDLE_PDX_INFERENCE_BACKEND", None)

    try:
        import paddle.inference  # noqa: F401
    except (ImportError, AttributeError) as err:
        if required:
            msg = (
                "paddlepaddle is not available: " + str(err) + "\n"
                "pdfmacro should have called py_require('paddlepaddle') automatically.\n"
                "If this persists, restart R and try again — reticulate will "
                "provision paddlepaddle on next use."
            )
            raise RuntimeError(msg)
        return []

    import paddle as _paddle
    try:
        _paddle.set_flags({"FLAGS_use_mkldnn": False})
    except Exception:
        pass

    from paddleocr import PPStructureV3
    try:
        pipe = PPStructureV3(
            device                  = device,
            use_table_recognition = True,
            use_formula_recognition = False,
            use_seal_recognition    = False,
            text_det_limit_side_len = _TEXT_DET_LIMIT,
        )
    except TypeError:
        pipe = PPStructureV3(device=device)
    return _run_pipeline(pipe, img_path, debug=debug)


# --------------------------------------------------------------------------- #
#  Pipeline runners                                                            #
# --------------------------------------------------------------------------- #

def _run_pipeline_paddlex(pipe, img_path, debug=False):
    """Run a PaddleX pipeline and collect pred_html strings."""
    tmp_dir  = tempfile.mkdtemp()
    html_out = []
    if debug:
        print("[pdfmacro debug] paddlex tmp dir: " + tmp_dir, flush=True)
    try:
        for res in pipe.predict(img_path):
            res.save_to_json(save_path=tmp_dir)

        for jf in glob.glob(os.path.join(tmp_dir, "**", "*.json"), recursive=True):
            with open(jf, encoding="utf-8") as fh:
                data = json.load(fh)

            if debug:
                print("[pdfmacro debug] JSON keys in " + jf + ": " + str(list(data.keys())), flush=True)
                if "model_settings" in data:
                    print("[pdfmacro debug] model_settings: " + json.dumps(data["model_settings"]), flush=True)

            if "pred_html" in data:
                html = data["pred_html"]
                if debug:
                    print("[pdfmacro debug] pred_html (top-level): " + html, flush=True)
                if html:
                    html_out.append(html)
                continue

            for i, tbl in enumerate(data.get("table_res_list", [])):
                html = tbl.get("pred_html", "")
                if debug:
                    print("[pdfmacro debug] table_res_list[" + str(i) + "] pred_html: " + html, flush=True)
                if html:
                    html_out.append(html)

        return html_out

    except Exception:
        return None
    finally:
        try:
            pipe.close()
        except Exception:
            pass
        if not debug:
            shutil.rmtree(tmp_dir, ignore_errors=True)


def _run_pipeline(pipe, img_path, debug=False):
    """Run a PPStructureV3 pipeline and collect pred_html strings."""
    tmp_dir  = tempfile.mkdtemp()
    html_out = []
    if debug:
        print("[pdfmacro debug] pipeline tmp dir: " + tmp_dir, flush=True)
    try:
        for res in pipe.predict(img_path):
            res.save_to_json(save_path=tmp_dir)

        for jf in glob.glob(os.path.join(tmp_dir, "**", "*.json"), recursive=True):
            with open(jf, encoding="utf-8") as fh:
                data = json.load(fh)

            if debug:
                print("[pdfmacro debug] JSON keys in " + jf + ": " + str(list(data.keys())), flush=True)
                if "model_settings" in data:
                    print("[pdfmacro debug] model_settings: " + json.dumps(data["model_settings"]), flush=True)

            for i, tbl in enumerate(data.get("table_res_list", [])):
                html = tbl.get("pred_html", "")
                if debug:
                    print("[pdfmacro debug] table_res_list[" + str(i) + "] pred_html: " + html, flush=True)
                if html:
                    html_out.append(html)

        return html_out

    finally:
        try:
            pipe.close()
        except Exception:
            pass
        if not debug:
            shutil.rmtree(tmp_dir, ignore_errors=True)
