#!/usr/bin/env python3
import os
import pathlib
import sys


def has_correct_message(msg: str) -> bool:
    print(f"Commit message:\n{msg}")
    title = msg.splitlines()[0]

    # allow a bang for commit squashing and fixup
    if title == "!" or title.startswith("fixup!"):
        return True

    if title.count(":") != 1:
        print("only one `:` should be present in the commit title.")
        return False

    scope, sentence = title.split(":")
    # Allow `[a-z]`, `-` and `/` characters
    if not all((c.isalpha() and c.islower()) or c in '-/' for c in scope):
        print("scope must be a single lowercase word.")
        return False

    if not sentence[0].startswith(" "):
        print("A space must be present following the scope separator.")
        return False

    verb, *words = sentence.split()
    possible_reports = {
        len(title) >= 72: "be shorter than 72 character",
        len(words) < 2: "include at least 3 words",
        "  " in title: "not contain double spaces",
        verb[0].isupper(): "not contain a capitalized verb",
        title.startswith(" "): "not start with a leading space",
        title.endswith(" "): "not end with a trailling space",
        any(map(verb.endswith, {"ed", "ing", "s"})): "use imperative tense",
        title.endswith("."): "not end with a period",
    }.get(True)

    if possible_reports is None:
        return True

    print("The title must", possible_reports)
    return False


def main():
    if len(sys.argv) != 2:
        return os.EX_USAGE

    message = pathlib.Path(sys.argv[1]).read_text()
    return not has_correct_message(message)


if __name__ == "__main__":
    sys.exit(main())
