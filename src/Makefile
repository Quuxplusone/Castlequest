ifneq (, $(shell which f77))
FORTRAN:=f77
else ifneq (, $(shell which gfortran))
FORTRAN:=gfortran -std=legacy
else
$(error I couldn't find any way to compile Fortran on your system!)
endif

cquest: adscor.o des.o fread.o ggnome.o help.o init.o input.o invent.o isig.o main.o move.o obj.o rstart.o savres.o wwolf.o yorn.o
	$(FORTRAN) $^ -o cquest

%.o: %.f
	$(FORTRAN) -c $< -o $@

clean:
	rm -f cquest *.o
