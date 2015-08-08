.PHONY: all clean

all: | clean manual.pdf

.PHONY: manual-html
html-manual: logo.png gratipay.png paypal.png fdl.texi
	makeinfo --html manual.texi

manual.html: logo.png gratipay.png paypal.png fdl.texi
	makeinfo --html --no-split manual.texi

manual.info: fdl.texi
	makeinfo manual.texi

manual.pdf: logo.png gratipay.png paypal.png fdl.texi
	pdftex manual.texi
	pdftex manual.texi

clean:
	rm -f {logo,gratipay,paypal}.{png,gif,pdf}
	rm -f manual.{aux,cp,fn,ky,log,pg,toc,tp,vr}

logo.png:
	convert -background none ../logo/cider-logo.svg logo.png

gratipay.png:
	wget --quiet "http://cdn.rawgit.com/gratipay/gratipay-badge/2.1.3/dist/gratipay.png" -O "gratipay.png"

paypal.png:
	wget --quiet "http://www.paypalobjects.com/en_US/i/btn/btn_donate_SM.gif" -O "paypal.gif"
	convert -background none paypal.gif paypal.png

fdl.texi:
	wget --quiet "http://www.gnu.org/licenses/fdl-1.3.texi" -O "fdl.texi"
