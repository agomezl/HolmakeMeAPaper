.PHONY : clean all

all :
	Holmake

clean :
	Holmake cleanAll
	rm -f heap munge.exe
	latexmk -pdf -C paper
	rm -f paper.tex
