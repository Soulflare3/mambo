all:
	@go build -o bin/title priv/title.go
	@echo Compiled priv/title.go
	@mix compile

clean:
	@echo cleaning
	@rm -rf bin/
	@mix clean

run:
	@iex -S mix

.PHONY: all clean run
