#Compile the program and put the executable in ~/bin

#FORT = lf95
FORT = ifort

#To develop code  (don't mix -check bounds with -O>0!):
#CFLAGS = -free -O0  -vec-report0 -warn alignments -warn declarations -warn uncalled -warn usage -check bounds -traceback #-check format -check output_conversion
CFLAGS = -free -O0  -vec-report0 -warn all -check all

#To debug code:
#CFLAGS = -free -O0  -vec-report0 -g

#To optimise code by hand (memory):
#CFLAGS = -free -O0  -vec-report0 -warn alignments -warn declarations -warn uncalled -warn usage -check bounds -check format -check output_conversion -traceback -warn unused
#CFLAGS = -free -O0  -vec-report0 -warn alignments -warn declarations -warn uncalled -warn usage -check bounds -check format -traceback -warn unused

#To use optimum run speed (~70s compile time):
#CFLAGS = -free -O3  -vec-report0 -traceback


#LFLAGS = -lm -lifcore -limf -lirc -lsvml -lifport -Xlinker 
LFLAGS = -lpgplot_ifort -lX11 -lm -lifcore -limf -lirc -lsvml -lifport -Xlinker 

STDOBJ = functions.o
PLTOBJ = plotfunctions.o

.f.o: 
	$(FORT) $(CFLAGS) -c $?

exit:

all: findplt listmdl listmod listplt makerun mergeplt plotmdl plotmdln plotmod plotplt plotpltn plt2ce plt2obs selplt

findplt: $(STDOBJ) findplt.o
	$(FORT) $(LFLAGS) -o ~/bin/findplt $(STDOBJ) findplt.o

getgrid: $(STDOBJ) getgrid.o
	$(FORT) $(LFLAGS) -o ~/bin/getgrid $(STDOBJ) getgrid.o

grid: $(STDOBJ) grid.o
	$(FORT) $(LFLAGS) -o ~/bin/grid $(STDOBJ) grid.o

listmdl: $(STDOBJ) listmdl.o
	$(FORT) $(LFLAGS) -o ~/bin/listmdl $(STDOBJ) listmdl.o

listmod: $(STDOBJ) listmod.o
	$(FORT) $(LFLAGS) -o ~/bin/listmod $(STDOBJ) listmod.o

listplt: $(STDOBJ) listplt.o
	$(FORT) $(LFLAGS) -o ~/bin/listplt $(STDOBJ) listplt.o

makerun: $(STDOBJ) makerun.o
	$(FORT) $(LFLAGS) -o ~/bin/makerun $(STDOBJ) makerun.o

mergeplt: $(STDOBJ) mergeplt.o
	$(FORT) $(LFLAGS) -o ~/bin/mergeplt $(STDOBJ) mergeplt.o



plotmdl: $(STDOBJ) $(PLTOBJ) plotmdl.o
	$(FORT) $(LFLAGS) -o ~/bin/plotmdl $(STDOBJ) $(PLTOBJ) plotmdl.o

plotmdln: $(STDOBJ) $(PLTOBJ) plotmdln.o
	$(FORT) $(LFLAGS) -o ~/bin/plotmdln $(STDOBJ) $(PLTOBJ) plotmdln.o

plotmod: $(STDOBJ) $(PLTOBJ) plotmod.o
	$(FORT) $(LFLAGS) -o ~/bin/plotmod $(STDOBJ) $(PLTOBJ) plotmod.o

plotplt: $(STDOBJ) $(PLTOBJ) plotplt.o
	$(FORT) $(LFLAGS) -o ~/bin/plotplt $(STDOBJ) $(PLTOBJ) plotplt.o

plotpltn: $(STDOBJ) $(PLTOBJ) plotpltn.o
	$(FORT) $(LFLAGS) -o ~/bin/plotpltn $(STDOBJ) $(PLTOBJ) plotpltn.o



plt2ce: $(STDOBJ) plt2ce.o
	$(FORT) $(LFLAGS) -o ~/bin/plt2ce $(STDOBJ) plt2ce.o

plt2obs: $(STDOBJ) plt2obs.o
	$(FORT) $(LFLAGS) -o ~/bin/plt2obs $(STDOBJ) plt2obs.o

selplt: $(STDOBJ) selplt.o
	$(FORT) $(LFLAGS) -o ~/bin/selplt $(STDOBJ) selplt.o




clean:
	rm -f *.o *.mod
