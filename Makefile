interpreter: src/gram.cf src/Common.hs src/Interpreter.hs src/Main.hs src/TypeChecker.hs src/TypeHelper.hs
	cd src/Gram && happy
	cd src && ghc Main.hs -o ../interpreter
