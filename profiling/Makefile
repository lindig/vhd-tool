# make coverage 
# prepares for building with coverage analysis
#
# make uncover
# reverses the setup from "make coverage"

coverage: _tags _tags.coverage 
	test ! -f _tags.orig && mv _tags _tags.orig || true
	cat _tags.coverage _tags.orig > _tags

uncover: _tags.orig
	mv _tags.orig _tags

.PHONY: coverage uncover
	
