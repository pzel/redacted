.PHONY: run

run:
	-pkill -f SimpleHTTPServer; sleep 1;
	python -m SimpleHTTPServer 9999 &
	while inotifywait -q -e modify *.elm index.html; do \
	 elm-make Main.elm --output=main.js; \
	done

