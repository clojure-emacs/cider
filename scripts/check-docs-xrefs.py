#!/usr/bin/env python3
"""Check that the manual's cross-references and images point at things that exist.

Walks doc/modules/ROOT (pages and nav.adoc) and verifies:

- every xref: target page exists under pages/ (an error);
- every xref: fragment resolves to an anchor in the target page, either an
  explicit [#id] / [[id]] or a section title's auto-generated id (a warning,
  since Antora's id generation has corner cases this script doesn't model);
- every image:/image:: target exists under assets/images (an error).

Exits non-zero only on errors, so it can gate CI without being pedantic.
"""

import os
import re
import sys

ROOT = os.path.join(os.path.dirname(os.path.abspath(__file__)), "..", "doc", "modules", "ROOT")
PAGES = os.path.join(ROOT, "pages")
IMAGES = os.path.join(ROOT, "assets", "images")

XREF_RE = re.compile(r"xref:([^\[\s]+)\[")
IMAGE_RE = re.compile(r"image::?([^\[\s]+)\[")
EXPLICIT_ANCHOR_RE = re.compile(r"^\[#([^\],]+)|\[\[([^\]]+)\]\]", re.MULTILINE)
SECTION_RE = re.compile(r"^=+\s+(.+?)\s*$", re.MULTILINE)


def section_id(title):
    """Approximate the auto id for TITLE the way the site builds it.

    The playbook sets idprefix to the empty string and idseparator to `-',
    so a section titled "Up and Running" gets the id `up-and-running'.
    """
    title = re.sub(r"`|\*|_(?=\w)|(?<=\w)_", "", title)  # strip inline formatting marks
    title = re.sub(r"kbd:\[([^\]]*)\]", r"\1", title)
    return re.sub(r"[^a-z0-9]+", "-", title.lower()).strip("-")


def anchors_in(path):
    with open(path, encoding="utf-8") as f:
        text = f.read()
    ids = set()
    for m in EXPLICIT_ANCHOR_RE.finditer(text):
        ids.add(m.group(1) or m.group(2))
    for m in SECTION_RE.finditer(text):
        ids.add(section_id(m.group(1)))
    return ids


def adoc_files():
    for dirpath, _, names in os.walk(PAGES):
        for name in sorted(names):
            if name.endswith(".adoc"):
                yield os.path.join(dirpath, name)
    yield os.path.join(ROOT, "nav.adoc")


def main():
    errors, warnings = [], []
    anchor_cache = {}
    for path in adoc_files():
        rel = os.path.relpath(path, ROOT)
        with open(path, encoding="utf-8") as f:
            lines = f.read().splitlines()
        for lineno, line in enumerate(lines, 1):
            for target in XREF_RE.findall(line):
                page, _, anchor = target.partition("#")
                if page:
                    if not page.endswith(".adoc"):
                        continue  # a resource or module-qualified xref; not modelled
                    target_path = os.path.normpath(os.path.join(PAGES, page))
                else:
                    target_path = path
                if not os.path.isfile(target_path):
                    errors.append(f"{rel}:{lineno}: xref to missing page {page}")
                    continue
                if anchor:
                    if target_path not in anchor_cache:
                        anchor_cache[target_path] = anchors_in(target_path)
                    if anchor not in anchor_cache[target_path]:
                        warnings.append(f"{rel}:{lineno}: xref to unknown anchor #{anchor} in {page or rel}")
            for target in IMAGE_RE.findall(line):
                if "://" in target:
                    continue
                if not os.path.isfile(os.path.join(IMAGES, target)):
                    errors.append(f"{rel}:{lineno}: image {target} not found under assets/images")
    for w in warnings:
        print(f"warning: {w}")
    for e in errors:
        print(f"error: {e}")
    print(f"{len(errors)} error(s), {len(warnings)} warning(s)")
    return 1 if errors else 0


if __name__ == "__main__":
    sys.exit(main())
