help:
	@echo "make test" # show under developing

test:
	clear
	(cd bin/spa && runghc test/ParseTest.hs)
	(cd bin/spa && runghc test/BuildTest.hs)
