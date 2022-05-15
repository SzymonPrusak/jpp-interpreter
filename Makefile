interpreter: src/gram.cf src/Common.hs src/Interpreter.hs src/Main.hs src/TypeChecker.hs src/TypeHelper.hs
	cd src/Gram && happy --array --info --ghc --coerce Par.y
	cd src/Gram && alex --ghc Lex.x
	cd src && ghc Main.hs -o ../interpreter
