;; The share of non-blank lines the default style re-indents in each corpus,
;; as (NAME DECISION RAW) in percent, measured with a prototype of the style
;; on the same files. RAW counts every line whose indentation changes;
;; DECISION counts a line only when it moves by a different amount than the
;; line holding its list's opener. The prototype follows the style as
;; written: a comment line never decides a list's layout, a symbol is read
;; through its \x...; escapes, a first argument line neither aligned nor at
;; +2 goes to +2, a line starting with a closer goes where the next element
;; would, a bracketed list's elements go under its head, and a top-level
;; datum goes to column 0. A file the formatter refuses, because it does
;; not read as complete data, is left out, as the check leaves it out: 11
;; fragments in csug and 8 in tspl.
(chez-s 5.9 32.0)
(chez-mats 14.0 24.6)
(tspl 2.1 2.5)
(csug 3.9 6.8)
(nanopass 2.3 16.7)
(swish 3.6 11.2)
(industria 1.9 8.3)
(akku 0.3 1.3)
(thunderchez 10.5 31.2)
(scheme-langserver 54.8 70.8)
(chez-srfi 2.9 9.8)
