BUILD_FOLDER=build

.PHONY: clean
clean:
	rm -Rf "$(BUILD_FOLDER)" || true

.PHONY: solve
solve: $(BUILD_FOLDER)/main
	@"$(BUILD_FOLDER)/main" "input"

$(BUILD_FOLDER)/main: Main.hs
	mkdir -p "$(BUILD_FOLDER)"
	ghc \
		-dynamic \
		-hidir "$(BUILD_FOLDER)" \
		-odir "$(BUILD_FOLDER)" \
		-o "$(BUILD_FOLDER)/main" \
		"Main.hs"
	chmod +x "$(BUILD_FOLDER)/main"
