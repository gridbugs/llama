# Build and run the playground binary
playground:
	dune exec ./bin/playground.exe

# Run the playground binary directly so the dune watch server can keep running
dev-playground:
	_build/default/bin/playground.exe

.PHONY: dev-playground
