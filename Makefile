build:
	(cd bin/spa && runghc Main.hs ../../app/dict.txt ../../app/index.spa)

watch:
	(cd bin/spa && watch 'runghc Main.hs ../../app/dict.txt ../../app/index.spa > ../../deploy/index.html')

server:
	(cd deploy; python -m SimpleHTTPServer 8888)

help:
	@echo "make test" # show under developing

test:
	clear
	(cd bin/spa && runghc test/ParseTest.hs)
	(cd bin/spa && runghc test/BuildTest.hs)

shell:
	clear
	(cd bin/spa && ghci Parse.hs)
