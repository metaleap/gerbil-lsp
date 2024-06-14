;;;	-*-	Gerbil	-*-
(import	:std/error
								:std/sugar
								:std/cli/getopt
								./lib)
(export	main)

(include	"../manifest.ss")

(def	(main	.	args)
 (def	foo
				(command	'foo	help:	"foo	this	bar	baz"))
		(call-with-getopt	lsp-main	args
				program:	"lsp"
				help:	"A	one	line	description	of	your	program"
				foo-cmd
				))
