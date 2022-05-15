interpreter: src/Gram/*.hs src/Common.hs src/Interpreter.hs src/Main.hs src/TypeChecker.hs src/TypeHelper.hs
	cd src && bnfc -d -m gram.cf --functor && "$(MAKE)"
	cd src && ghc Main.hs -o ../interpreter
