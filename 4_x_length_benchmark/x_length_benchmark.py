"""Reproduce the X-post length benchmark reported for the stimuli.

The analysis uses a deterministic stratified 10% sample of the original
22-million-post release accompanying Balasubramanian et al. (2024). Two of
the 20 files in each of part_1 through part_22 were selected with Python's
``random.Random(12345).sample``. The selected paths and SHA-256 checksums are
stored in ``x_length_sample_manifest.csv`` next to this script.

The primary benchmark (``english_nonreply_content``, flagged in the output)
uses English-language posts, rows for which the dataset's ``lang`` field
equals ``en``. Replies are excluded because the experimental stimuli are
standalone posts. A row is classified as a reply when either
``in_reply_to_status_id_str`` or ``in_reply_to_screen_name`` is non-empty.
Length is the number of whitespace-separated tokens in ``rawContent`` after
tokens beginning with ``@``, ``http://``, ``https://``, or ``www.`` are
removed. This makes the benchmark comparable to stimuli that contain neither
mentions nor URLs. Posts that contain no remaining tokens are retained with a
length of zero. Quartiles use the linear, type-7 definition used by pandas and
NumPy.

The output also reports three sensitivity specifications: all English posts
with content-only tokens, all English posts with URLs and mentions retained,
and English non-reply posts with URLs and mentions retained. The original
all-post sample is retained as a descriptive row.

Run from the repository root:

    python 4_x_length_benchmark/x_length_benchmark.py --download

The source files (about 830 MB) are downloaded from the pinned dataset commit
to ``data/x-24-us-election`` next to this script when they are not already
present; use ``--data-dir`` to keep them elsewhere. The script writes the
summary to ``x_length_benchmark.csv`` next to this script.
"""

from __future__ import annotations

import argparse
import csv
import gzip
import hashlib
import math
import random
import shutil
import sys
import urllib.request
from collections import Counter
from concurrent.futures import ThreadPoolExecutor, as_completed
from dataclasses import dataclass
from pathlib import Path


DATASET_COMMIT = "829172118f35d901de008a94cec89d3eea04410f"
SAMPLE_SEED = 12345
TEXT_FIELD = "rawContent"
LANGUAGE_FIELD = "lang"
ENGLISH_CODE = "en"
REPLY_FIELDS = ("in_reply_to_status_id_str", "in_reply_to_screen_name")
FILES_PER_PART = 2
N_PARTS = 22

PRIMARY_SPECIFICATION = "english_nonreply_content"
EXPECTED_RESULTS = {
    "all_posts_raw": {"n": 2_200_000, "q1_words": 10.0, "median_words": 19.0, "q3_words": 35.0},
    "english_all_raw": {"n": 1_917_125, "q1_words": 12.0, "median_words": 21.0, "q3_words": 36.0},
    "english_all_content": {"n": 1_917_125, "q1_words": 10.0, "median_words": 19.0, "q3_words": 35.0},
    "english_nonreply_raw": {"n": 528_628, "q1_words": 13.0, "median_words": 21.0, "q3_words": 35.0},
    "english_nonreply_content": {"n": 528_628, "q1_words": 12.0, "median_words": 20.0, "q3_words": 34.0},
}

SCRIPT_DIR = Path(__file__).resolve().parent
DEFAULT_MANIFEST = SCRIPT_DIR / "x_length_sample_manifest.csv"
DEFAULT_DATA_DIR = SCRIPT_DIR / "data" / "x-24-us-election"
DEFAULT_OUTPUT = SCRIPT_DIR / "x_length_benchmark.csv"


@dataclass(frozen=True)
class SourceFile:
    part: int
    chunk: int
    relative_path: Path
    sha256: str
    size_bytes: int


def set_csv_field_size_limit() -> None:
    """Use the largest CSV field size supported by the current platform."""
    limit = sys.maxsize
    while True:
        try:
            csv.field_size_limit(limit)
            return
        except OverflowError:
            limit //= 10


def load_manifest(path: Path) -> list[SourceFile]:
    with path.open(newline="", encoding="utf-8") as source:
        rows = [
            SourceFile(
                part=int(row["part"]),
                chunk=int(row["chunk"]),
                relative_path=Path(row["path"]),
                sha256=row["sha256"].strip().lower(),
                size_bytes=int(row["bytes"]),
            )
            for row in csv.DictReader(source)
        ]

    counts = Counter(row.part for row in rows)
    expected_parts = set(range(1, N_PARTS + 1))
    if set(counts) != expected_parts or any(counts[part] != FILES_PER_PART for part in expected_parts):
        raise ValueError(
            f"Manifest must contain {FILES_PER_PART} files for each of part_1 through part_{N_PARTS}."
        )
    if len({row.relative_path for row in rows}) != len(rows):
        raise ValueError("Manifest contains duplicate file paths.")
    for row in rows:
        expected_path = Path(f"part_{row.part}/may_july_chunk_{row.chunk}.csv.gz")
        if row.relative_path != expected_path:
            raise ValueError(f"Manifest path does not match its part and chunk: {row.relative_path}")

    generator = random.Random(SAMPLE_SEED)
    expected_chunks = {
        chunk
        for part in range(1, N_PARTS + 1)
        for chunk in generator.sample(range(20 * (part - 1) + 1, 20 * part + 1), FILES_PER_PART)
    }
    if {row.chunk for row in rows} != expected_chunks:
        raise ValueError(f"Manifest does not match the documented sampling seed {SAMPLE_SEED}.")

    return rows


def sha256_file(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as source:
        for block in iter(lambda: source.read(1024 * 1024), b""):
            digest.update(block)
    return digest.hexdigest()


def source_url(row: SourceFile) -> str:
    return (
        "https://raw.githubusercontent.com/sinking8/x-24-us-election/"
        f"{DATASET_COMMIT}/{row.relative_path.as_posix()}"
    )


def download_one(row: SourceFile, data_dir: Path) -> Path:
    destination = data_dir / row.relative_path
    destination.parent.mkdir(parents=True, exist_ok=True)

    if destination.exists() and destination.stat().st_size == row.size_bytes:
        if sha256_file(destination) == row.sha256:
            return destination

    temporary = destination.with_suffix(destination.suffix + ".part")
    if temporary.exists():
        temporary.unlink()

    request = urllib.request.Request(source_url(row), headers={"User-Agent": "Viewpoints-JESP-reproduction"})
    with urllib.request.urlopen(request, timeout=120) as response, temporary.open("wb") as target:
        shutil.copyfileobj(response, target, length=1024 * 1024)

    if temporary.stat().st_size != row.size_bytes:
        temporary.unlink(missing_ok=True)
        raise ValueError(f"Downloaded size does not match the manifest for {row.relative_path}.")
    if sha256_file(temporary) != row.sha256:
        temporary.unlink(missing_ok=True)
        raise ValueError(f"Downloaded checksum does not match the manifest for {row.relative_path}.")

    temporary.replace(destination)
    return destination


def download_sources(rows: list[SourceFile], data_dir: Path, workers: int) -> None:
    with ThreadPoolExecutor(max_workers=workers) as pool:
        jobs = {pool.submit(download_one, row, data_dir): row for row in rows}
        for index, job in enumerate(as_completed(jobs), start=1):
            row = jobs[job]
            job.result()
            print(f"Downloaded or verified {index}/{len(rows)}: {row.relative_path}")


def verify_sources(rows: list[SourceFile], data_dir: Path) -> list[Path]:
    missing = [row.relative_path for row in rows if not (data_dir / row.relative_path).exists()]
    if missing:
        preview = "\n".join(f"  {path}" for path in missing[:5])
        raise FileNotFoundError(
            f"{len(missing)} source files are missing from {data_dir}.\n{preview}\n"
            "Run this script with --download to retrieve the pinned sample."
        )

    paths = []
    for index, row in enumerate(rows, start=1):
        path = data_dir / row.relative_path
        if path.stat().st_size != row.size_bytes or sha256_file(path) != row.sha256:
            raise ValueError(f"Source file does not match the manifest: {path}")
        paths.append(path)
        print(f"Verified {index}/{len(rows)}: {row.relative_path}")

    return paths


def is_reply(row: dict[str, str]) -> bool:
    """Return True when either dataset reply field is non-empty."""
    return any((row.get(field) or "").strip() for field in REPLY_FIELDS)


def content_word_count(text: str) -> int:
    """Count whitespace tokens after removing mention and URL tokens."""
    excluded_prefixes = ("http://", "https://", "www.")
    count = 0
    for token in text.split():
        lower = token.lower()
        if token.startswith("@") or lower.startswith(excluded_prefixes):
            continue
        count += 1
    return count


def count_words(paths: list[Path]) -> dict[str, Counter[int]]:
    set_csv_field_size_limit()
    counts = {specification: Counter() for specification in EXPECTED_RESULTS}

    for index, path in enumerate(paths, start=1):
        with gzip.open(path, "rt", encoding="utf-8", errors="replace", newline="") as source:
            reader = csv.DictReader(source)
            required = {TEXT_FIELD, LANGUAGE_FIELD, *REPLY_FIELDS}
            missing = required.difference(reader.fieldnames or [])
            if missing:
                raise ValueError(f"{path} is missing required column(s): {', '.join(sorted(missing))}")

            for row in reader:
                text = row[TEXT_FIELD] or ""
                raw_words = len(text.split())
                counts["all_posts_raw"][raw_words] += 1

                if (row[LANGUAGE_FIELD] or "").strip().lower() != ENGLISH_CODE:
                    continue

                content_words = content_word_count(text)
                counts["english_all_raw"][raw_words] += 1
                counts["english_all_content"][content_words] += 1

                if not is_reply(row):
                    counts["english_nonreply_raw"][raw_words] += 1
                    counts["english_nonreply_content"][content_words] += 1

        print(f"Analysed {index}/{len(paths)}: {path.name}")

    return counts


def order_statistic(frequencies: Counter[int], zero_based_index: int) -> int:
    cumulative = 0
    for value in sorted(frequencies):
        cumulative += frequencies[value]
        if cumulative > zero_based_index:
            return value
    raise IndexError("Order-statistic index exceeds the number of observations.")


def linear_quantile(frequencies: Counter[int], probability: float) -> float:
    """Return the linear/type-7 quantile used by pandas and NumPy."""
    n = sum(frequencies.values())
    if n == 0:
        raise ValueError("Cannot calculate a quantile for an empty population.")
    position = (n - 1) * probability
    lower_index = math.floor(position)
    upper_index = math.ceil(position)
    lower = order_statistic(frequencies, lower_index)
    upper = order_statistic(frequencies, upper_index)
    return lower + (position - lower_index) * (upper - lower)


def summarise(
    specification: str,
    label: str,
    frequencies: Counter[int],
    language_filter: str,
    reply_filter: str,
    tokenisation: str,
) -> dict[str, object]:
    n = sum(frequencies.values())
    return {
        "dataset_commit": DATASET_COMMIT,
        "sample_seed": SAMPLE_SEED,
        "specification": specification,
        "primary_benchmark": specification == PRIMARY_SPECIFICATION,
        "population": label,
        "n": n,
        "mean_words": sum(words * count for words, count in frequencies.items()) / n,
        "q1_words": linear_quantile(frequencies, 0.25),
        "median_words": linear_quantile(frequencies, 0.50),
        "q3_words": linear_quantile(frequencies, 0.75),
        "text_field": TEXT_FIELD,
        "language_filter": language_filter,
        "reply_filter": reply_filter,
        "tokenisation": tokenisation,
    }


def validate_results(rows: list[dict[str, object]]) -> None:
    by_specification = {str(row["specification"]): row for row in rows}
    for specification, expected_values in EXPECTED_RESULTS.items():
        summary = by_specification[specification]
        for key, expected in expected_values.items():
            if summary[key] != expected:
                raise AssertionError(
                    f"{specification} {key} was {summary[key]!r}; expected {expected!r}."
                )


def write_output(rows: list[dict[str, object]], path: Path) -> None:
    columns = [
        "dataset_commit",
        "sample_seed",
        "specification",
        "primary_benchmark",
        "population",
        "n",
        "mean_words",
        "q1_words",
        "median_words",
        "q3_words",
        "text_field",
        "language_filter",
        "reply_filter",
        "tokenisation",
    ]
    with path.open("w", newline="", encoding="utf-8") as target:
        writer = csv.DictWriter(target, fieldnames=columns, lineterminator="\n")
        writer.writeheader()
        for row in rows:
            output_row = dict(row)
            output_row["mean_words"] = f'{float(row["mean_words"]):.6f}'
            for key in ("q1_words", "median_words", "q3_words"):
                output_row[key] = f'{float(row[key]):g}'
            writer.writerow(output_row)


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--manifest", type=Path, default=DEFAULT_MANIFEST)
    parser.add_argument("--data-dir", type=Path, default=DEFAULT_DATA_DIR)
    parser.add_argument("--output", type=Path, default=DEFAULT_OUTPUT)
    parser.add_argument("--download", action="store_true", help="Download missing files from the pinned dataset commit.")
    parser.add_argument("--workers", type=int, default=4, help="Concurrent download workers used with --download.")
    return parser.parse_args()


def main() -> None:
    args = parse_args()
    manifest = load_manifest(args.manifest)

    if args.download:
        download_sources(manifest, args.data_dir, max(1, args.workers))

    paths = verify_sources(manifest, args.data_dir)
    counts = count_words(paths)
    rows = [
        summarise(
            "all_posts_raw",
            "All sampled posts",
            counts["all_posts_raw"],
            "none",
            "none",
            "Python str.split; URLs and mentions retained",
        ),
        summarise(
            "english_all_raw",
            "English-language sampled posts",
            counts["english_all_raw"],
            f"{LANGUAGE_FIELD} == {ENGLISH_CODE}",
            "none",
            "Python str.split; URLs and mentions retained",
        ),
        summarise(
            "english_all_content",
            "English-language sampled posts",
            counts["english_all_content"],
            f"{LANGUAGE_FIELD} == {ENGLISH_CODE}",
            "none",
            "Python str.split after removing tokens beginning with @, http://, https://, or www.",
        ),
        summarise(
            "english_nonreply_raw",
            "English-language non-reply sampled posts",
            counts["english_nonreply_raw"],
            f"{LANGUAGE_FIELD} == {ENGLISH_CODE}",
            "both reply fields empty",
            "Python str.split; URLs and mentions retained",
        ),
        summarise(
            "english_nonreply_content",
            "English-language non-reply sampled posts",
            counts["english_nonreply_content"],
            f"{LANGUAGE_FIELD} == {ENGLISH_CODE}",
            "both reply fields empty",
            "Python str.split after removing tokens beginning with @, http://, https://, or www.",
        ),
    ]
    validate_results(rows)
    write_output(rows, args.output)

    primary = next(row for row in rows if row["specification"] == PRIMARY_SPECIFICATION)
    print(
        "Primary benchmark, English-language non-reply X posts "
        "after removing URLs and mentions: "
        f'n = {primary["n"]:,}, '
        f'Mdn = {primary["median_words"]:g} words, '
        f'IQR = {primary["q1_words"]:g}-{primary["q3_words"]:g} words.'
    )
    print(f"Summary written to {args.output}")


if __name__ == "__main__":
    main()
