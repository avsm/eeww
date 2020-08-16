README.md: README.cpp.md $(wildcard ./example/*.ml)
	@cppo -n $< -o $@
