"""

lexicon.py
----------

Author: Michael Dickens <mdickens93@gmail.com>
Created: 2020-12-23

Lexicon taken from:
https://docs.google.com/spreadsheets/d/1JdaG1PaSQJRE2LpILvdzthbzz1k_a0VT86XSXouwGy8/

"""


def roots_to_emacs_lisp():
    '''
    '''
    with open("db/roots.tsv") as lexicon_file:
        print("(defconst ithkuil-roots")
        print("  '(", end="")
        is_header = True
        is_first = True
        for line in lexicon_file:
            if is_header:
                is_header = False
                continue
            entries = line.strip().replace('"', '\\"').split("\t")
            if is_first:
                prefix = ""
                is_first = False
            else:
                prefix = "    "
            print(prefix + "(" + " ".join(['"{}"'.format(x) for x in entries]) + ")")
        print("  )")
        print("  \"A list of roots, where each root is specified by a list containing:")
        print("  - root, in Ithkuil")
        print("  - Stem 0 / Description")
        print("  - Stem 1")
        print("  - Stem 2")
        print("  - Stem 3")
        print("  - Schema")
        print("  - (rest) Parameters")
        print("  All items after the first are optional.")
        print("")
        print("  Last updated 2020-12-23.")
        print("  \")")


def roots_all_chars():
    with open("db/roots.tsv") as lexicon_file:
        chars = []
        for line in lexicon_file:
            root = line.strip().split("\t")[0]
            chars.extend(list(root))
        print("".join(sorted(list(set(chars)))))


def affixes_to_emacs_lisp():


affixes_to_emacs_lisp()
