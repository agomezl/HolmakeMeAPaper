.PHONY : clean all

all :
	Holmake --fast pdf

clean :
	Holmake cleanAll
	rm -f heap munge.exe
	latexmk -pdf -C
	rm -f paper.tex
