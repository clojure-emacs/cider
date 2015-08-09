.PHONY: all clean

all: | clean manual.pdf

.PHONY: manual-html
html-manual: logo.png fdl.texi
	makeinfo --html cider.texi

cider.html: logo.png fdl.texi
	makeinfo --html --no-split cider.texi

cider.info: fdl.texi
	makeinfo cider.texi

cider.pdf: logo.png fdl.texi
	$(if $(shell which pdftex),pdftex cider.texi && pdftex cider.texi,$(info pdfTeX unavailable))	
# which pdftex && pdftex cider.texi && pdftex cider.texi	
# which pdftex || echo "pdfTeX is unavailable"


clean:
	rm -f logo.{png,gif,pdf}
	rm -f cider.{aux,cp,fn,ky,log,pg,toc,tp,vr}

logo.png:
	convert -background none ../logo/cider-logo.svg logo.png

fdl.texi:
	wget --quiet "http://www.gnu.org/licenses/fdl-1.3.texi" -O "fdl.texi"
