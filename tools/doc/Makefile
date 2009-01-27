#Compile the program and put the executable in ~/bin

#COMP = lf95
COMP = ifort

#To develop code  (don't mix -check bounds with -O>0!):
CFLAGS = -free -O0  -vec-report0 -warn alignments -warn declarations -warn uncalled -warn usage -check bounds -traceback #-check format -check output_conversion
CFLAGS = -free -O0  -vec-report0 -traceback -warn all -check all 


#To debug code:
#CFLAGS = -free -O0  -vec-report0 -g

#To optimise code by hand (memory):
#CFLAGS = -free -O0  -vec-report0 -warn alignments -warn declarations -warn uncalled -warn usage -check bounds -check format -check output_conversion -traceback -warn unused
#CFLAGS = -free -O0  -vec-report0 -warn alignments -warn declarations -warn uncalled -warn usage -check bounds -check format -traceback -warn unused

#To use optimum run speed
#CFLAGS = -free -O3 -vec-report0 -traceback
#CFLAGS = -free -O3 -arch SSE2 -vec-report0 -traceback
#CFLAGS = -free -O3 -arch SSE2 -ipo -vec-report0 -traceback


#LFLAGS = -lm -lifcore -limf -lirc -lsvml -lifport -Xlinker 
LFLAGS = -lpgplot_ifort -lX11 -lm -lifcore -limf -lirc -lsvml -lifport -Xlinker 
#LFLAGS = -ipo -vec-report0 -lpgplot_ifort -lX11 -lm -lifcore -limf -lirc -lsvml -lifport -Xlinker 



#gfortran: similar in speed to ifort, but file reading is ~2.5x slower!
#COMP = gfortran
#CFLAGS = -O0 -ffree-form -ffree-line-length-none
#CFLAGS = -O0 -ffree-form -ffree-line-length-none   -Wall 
#CFLAGS = -O -ffree-form -ffree-line-length-none
#CFLAGS = -O3 -ffree-form -ffree-line-length-none
#LFLAGS = -lpgplot_ifort -L/usr/X11R6/lib -lX11 -lm -L/opt/intel/fc/10.0.026/lib -lifcore -limf -lirc -lsvml -lifport -Xlinker 





STDOBJ = functions.o
PLTOBJ = plotfunctions.o

.f.o: 
	$(COMP) $(CFLAGS) -c $?

exit:

all: findplt listmdl listmod listplt makerun mergeplt plt2ce plt2obs selplt

findplt: $(STDOBJ) findplt.o
	$(COMP) $(LFLAGS) -o ~/bin/findplt $(STDOBJ) findplt.o

getgrid: $(STDOBJ) getgrid.o
	$(COMP) $(LFLAGS) -o ~/bin/getgrid $(STDOBJ) getgrid.o

grid: $(STDOBJ) grid.o
	$(COMP) $(LFLAGS) -o ~/bin/grid $(STDOBJ) grid.o

listmdl: $(STDOBJ) listmdl.o
	$(COMP) $(LFLAGS) -o ~/bin/listmdl $(STDOBJ) listmdl.o

listmod: $(STDOBJ) listmod.o
	$(COMP) $(LFLAGS) -o ~/bin/listmod $(STDOBJ) listmod.o

listplt: $(STDOBJ) listplt.o
	$(COMP) $(LFLAGS) -o ~/bin/listplt $(STDOBJ) listplt.o

makerun: $(STDOBJ) makerun.o
	$(COMP) $(LFLAGS) -o ~/bin/makerun $(STDOBJ) makerun.o

mergeplt: $(STDOBJ) mergeplt.o
	$(COMP) $(LFLAGS) -o ~/bin/mergeplt $(STDOBJ) mergeplt.o



plt2ce: $(STDOBJ) plt2ce.o
	$(COMP) $(LFLAGS) -o ~/bin/plt2ce $(STDOBJ) plt2ce.o

plt2obs: $(STDOBJ) plt2obs.o
	$(COMP) $(LFLAGS) -o ~/bin/plt2obs $(STDOBJ) plt2obs.o

selplt: $(STDOBJ) selplt.o
	$(COMP) $(LFLAGS) -o ~/bin/selplt $(STDOBJ) selplt.o




clean:
	rm -f *.o *.mod
