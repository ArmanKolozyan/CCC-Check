#!/usr/bin/env python3
"""
circom-loc.py – count *reachable* logical lines of code in Circom projects.

Usage:
    python circom-loc.py <top-level .circom>
"""

from pathlib import Path
import re, sys, collections

# ───── regexes
RX_INCLUDE   = re.compile(r'^\s*include\s+"([^"]+)"\s*;', re.I)
RX_TPL_START = re.compile(r'^\s*template\s+(\w+)\s*\(')
RX_MAIN      = re.compile(r'^\s*component\s+main\s*=\s*([A-Za-z_]\w*)')
# three invocation styles
RX_CALLS     = re.compile(
    r'''
        (?:component\s+\w+\s*=\s*)?      # optional `component tmp =`
        (?P<name>[A-Z][A-Za-z0-9_]*)     # Template name (capitalised)
        \s*
        (?:\([^()]*\))?                  # an optional “(n)” generic block
        \s*
        \(\s*                            # the “()” that starts the arg list
        ''', re.VERBOSE,
)

# ───── comment stripper (/* … */ & // …)
def strip_comments(lines):
    out, in_block = [], False
    for ln in lines:
        if in_block:
            if '*/' in ln:
                ln = ln.split('*/', 1)[1]
                in_block = False
            else:
                continue
        if '/*' in ln:
            pre, post = ln.split('/*', 1)
            ln = pre
            if '*/' not in post:
                in_block = True
            else:
                ln += post.split('*/', 1)[1]
        ln = re.sub(r'//.*', '', ln)
        out.append(ln.rstrip('\n'))
    return out

# ───── small structs
TplInfo  = collections.namedtuple('TplInfo',  'file start end')
FileIdx  = collections.namedtuple('FileIdx', 'templates includes lines')

# ───── indexing
cache = {}
def index_file(path: Path):
    path = path.resolve()
    if path in cache:
        return
    try:
        raw = path.read_text('utf-8').splitlines()
    except FileNotFoundError:
        cache[path] = FileIdx({}, [], [])
        return

    lines     = strip_comments(raw)
    includes  = []
    templates = {}

    for ln in lines:
        m = RX_INCLUDE.match(ln)
        if m:
            includes.append((path.parent / m.group(1)).resolve())

    i, n = 0, len(lines)
    while i < n:
        m = RX_TPL_START.match(lines[i])
        if m:
            name, depth, start = m.group(1), 0, i
            while i < n:
                depth += lines[i].count('{') - lines[i].count('}')
                if depth == 0:
                    templates[name] = TplInfo(path, start, i)
                    break
                i += 1
        i += 1

    cache[path] = FileIdx(templates, includes, lines)
    for inc in includes:
        index_file(inc)

# ───── reachability
def reachable_templates(root: Path):
    idx_root = cache[root]
    # find the template used for `component main`
    main_tpl = next(
        (RX_MAIN.match(ln).group(1) for ln in idx_root.lines if RX_MAIN.match(ln)),
        None
    )
    if not main_tpl:
        sys.exit("❌  No `component main = …` found.")

    # breadth-first walk
    global_tpls = {t for idx in cache.values() for t in idx.templates}
    reach, queue = set(), collections.deque([main_tpl])

    while queue:
        tpl = queue.popleft()
        if tpl in reach:
            continue
        reach.add(tpl)

        # locate definition
        info = next((idx.templates[tpl] for idx in cache.values()
                     if tpl in idx.templates), None)
        if not info:
            continue                    # built-ins, external, or missing

        body = cache[info.file].lines[info.start : info.end + 1]
        for ln in body:
            for m in RX_CALLS.finditer(ln):
                cand = m.group('name')
                if cand in global_tpls and cand not in reach:
                    queue.append(cand)
    return reach

# ───── LOC
def reachable_loc(reach):
    loc = 0
    for idx in cache.values():
        for name, info in idx.templates.items():
            if name not in reach:
                continue
            # body, minus header and closing brace
            for ln in idx.lines[info.start + 1 : info.end]:
                if ln.strip():
                    loc += 1
    return loc

# ───── CLI
if __name__ == "__main__":
    if len(sys.argv) != 2:
        sys.exit("usage: python circom-loc.py <root.circom>")

    root = Path(sys.argv[1]).resolve()
    if root.suffix.lower() != ".circom":
        sys.exit("input must be a .circom file")

    index_file(root)
    reachable = reachable_templates(root)
    print(f"Reachable LOC: {reachable_loc(reachable)}")
