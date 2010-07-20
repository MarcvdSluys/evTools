#Compile the program and put the executable in ~/usr/bin

#No text/space after statements below!
FORT = gfortran#  Use GNU fortran compiler
FORT = ifort#     Use Intel fortran compiler


#ifort:
ifeq ($(FORT),ifort)
    #To develop code  (don't mix -check bounds with -O>0!):
    CFLAGS = -O0  -vec-report0 -traceback -check all -check noarg_temp_created

    #To debug code:
    #CFLAGS = -O0  -vec-report0 -g

    #To use optimum run speed
    CFLAGS = -O2 -vec-report0 -traceback
    #CFLAGS = -O3 -vec-report0 -traceback
    #CFLAGS = -O3 -arch SSE4.1 -vec-report0 -traceback
    #CFLAGS = -O3 -arch SSE4.1 -ipo -vec-report0 -traceback

    CFLAGS += -warn all
    CFLAGS += -nogen-interfaces  #For ifort v.11

    LFLAGS = 
endif


#gfortran: similar in speed to ifort, but file reading is ~2.5x slower!
#ifeq ($(FORT),gfortran)
ifneq (,$(filter $(FORT),gfortran gfortran-4.4.2))
    CFLAGS = -O0
    #CFLAGS = -O2 
    CFLAGS += -Wall
    LFLAGS = 
endif





STDOBJ = functions.o plt_functions.o


%.o: %.f90 Makefile
	$(FORT) $(CFLAGS) -c $< -o $@


all: dat2plt findplt getgrid grid listmdl listmod listplt makerun mergeplt plt2ce plt2dat plt2obs selplt

exit:




dat2plt: $(STDOBJ) dat2plt.o
	$(FORT) $(LFLAGS) -o ~/usr/bin/dat2plt $(STDOBJ) dat2plt.o

convert_initdat: $(STDOBJ) convert_initdat.o
	$(FORT) $(LFLAGS) -o ~/usr/bin/convert_initdat $(STDOBJ) convert_initdat.o

findplt: $(STDOBJ) findplt.o
	$(FORT) $(LFLAGS) -o ~/usr/bin/findplt $(STDOBJ) findplt.o

getgrid: $(STDOBJ) getgrid.o
	$(FORT) $(LFLAGS) -o ~/usr/bin/getgrid $(STDOBJ) getgrid.o

grid: $(STDOBJ) grid.o
	$(FORT) $(LFLAGS) -o ~/usr/bin/grid $(STDOBJ) grid.o

listmdl: $(STDOBJ) listmdl.o
	$(FORT) $(LFLAGS) -o ~/usr/bin/listmdl $(STDOBJ) listmdl.o

listmod: $(STDOBJ) listmod.o
	$(FORT) $(LFLAGS) -o ~/usr/bin/listmod $(STDOBJ) listmod.o

listplt: $(STDOBJ) listplt.o
	$(FORT) $(LFLAGS) -o ~/usr/bin/listplt $(STDOBJ) listplt.o

makerun: $(STDOBJ) makerun.o
	$(FORT) $(LFLAGS) -o ~/usr/bin/makerun $(STDOBJ) makerun.o

mergeplt: $(STDOBJ) mergeplt.o
	$(FORT) $(LFLAGS) -o ~/usr/bin/mergeplt $(STDOBJ) mergeplt.o



plt2ce: $(STDOBJ) plt2ce.o
	$(FORT) $(LFLAGS) -o ~/usr/bin/plt2ce $(STDOBJ) plt2ce.o

plt2dat: $(STDOBJ) plt2dat.o
	$(FORT) $(LFLAGS) -o ~/usr/bin/plt2dat $(STDOBJ) plt2dat.o

plt2obs: $(STDOBJ) plt2obs.o
	$(FORT) $(LFLAGS) -o ~/usr/bin/plt2obs $(STDOBJ) plt2obs.o

selplt: $(STDOBJ) selplt.o
	$(FORT) $(LFLAGS) -o ~/usr/bin/selplt $(STDOBJ) selplt.o




clean:
	rm -f *.o *.mod

cleanwarnings:
	rm -f *__genmod.f90 *__genmod.mod

