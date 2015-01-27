build:
	(cd bin/spa && runghc Main.hs ../../deploy/ ../../app/index.spa)

help:
	@echo "make test" # show under developing

test:
	clear
	(cd bin/spa && runghc test/ParseTest.hs)
	(cd bin/spa && runghc test/BuildTest.hs)

shell:
	clear
	(cd bin/spa && ghci Parse.hs)
