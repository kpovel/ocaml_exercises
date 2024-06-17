stats:
	ocamlopt textstat.mli textstat.ml stats.ml -o stats

reverse_lines:
	ocamlopt reverse_lines.ml -o reverse_lines
