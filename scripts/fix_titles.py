# Fix the HTML title for the overview report
# Weirdest Quarto bug: it never generates the overview HTML title correctly
# Auto-run while rendering the site with Quarto 
# from inside _quarto.yml — post-render: python scripts/fix_titles.py

from pathlib import Path
import re

html_file = Path("docs/reports/overview.html")

if html_file.exists():
    html = html_file.read_text(encoding="utf-8")

    html, count = re.subn(
        r"<title>.*?</title>",
        "<title>The World Bank in Costa Rica – Tico-nomics</title>",
        html,
        count=1
    )

    if count:
        html_file.write_text(html, encoding="utf-8")
        print("Fixed overview HTML title.")
    else:
        raise RuntimeError("Could not find HTML title tag.")
else:
    raise FileNotFoundError(html_file)