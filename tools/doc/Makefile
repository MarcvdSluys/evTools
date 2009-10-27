#Compile the program and put the executable in ~/usr/bin

#No text/space after statements below!
COMP = gfortran#  Use GNU fortran compiler
COMP = ifort#     Use Intel fortran compiler


#ifort:
ifeq ($(COMP),ifort)
    #To develop code  (don't mix -check bounds with -O>0!):
    CFLAGS = -free -O0  -vec-report0 -traceback -warn all -check all -check noarg_temp_created

    #To debug code:
    #CFLAGS = -free -O0  -vec-report0 -g

    #To use optimum run speed
    #CFLAGS = -free -O3 -vec-report0 -traceback
    #CFLAGS = -free -O3 -arch SSE4.1 -vec-report0 -traceback
    #CFLAGS = -free -O3 -arch SSE4.1 -ipo -vec-report0 -traceback

    CFLAGS += -nogen-interfaces  #For ifort v.11

    LFLAGS = 
endif


#gfortran: similar in speed to ifort, but file reading is ~2.5x slower!
ifeq ($(COMP),gfortran)
    #CFLAGS = -O0 -ffree-form -ffree-line-length-none
    CFLAGS = -O0 -ffree-form -ffree-line-length-none   -Wall 
    #CFLAGS = -O3 -ffree-form -ffree-line-length-none
    LFLAGS = 
endif





STDOBJ = functions.o


.f.o: 
	$(COMP) $(CFLAGS) -c $?

all: findplt listmdl listmod listplt makerun mergeplt plt2ce plt2obs selplt

exit:




dat2plt: $(STDOBJ) dat2plt.o
	$(COMP) $(LFLAGS) -o ~/usr/bin/dat2plt $(STDOBJ) dat2plt.o

findplt: $(STDOBJ) findplt.o
	$(COMP) $(LFLAGS) -o ~/usr/bin/findplt $(STDOBJ) findplt.o

getgrid: $(STDOBJ) getgrid.o
	$(COMP) $(LFLAGS) -o ~/usr/bin/getgrid $(STDOBJ) getgrid.o

grid: $(STDOBJ) grid.o
	$(COMP) $(LFLAGS) -o ~/usr/bin/grid $(STDOBJ) grid.o

listmdl: $(STDOBJ) listmdl.o
	$(COMP) $(LFLAGS) -o ~/usr/bin/listmdl $(STDOBJ) listmdl.o

listmod: $(STDOBJ) listmod.o
	$(COMP) $(LFLAGS) -o ~/usr/bin/listmod $(STDOBJ) listmod.o

listplt: $(STDOBJ) listplt.o
	$(COMP) $(LFLAGS) -o ~/usr/bin/listplt $(STDOBJ) listplt.o

makerun: $(STDOBJ) makerun.o
	$(COMP) $(LFLAGS) -o ~/usr/bin/makerun $(STDOBJ) makerun.o

mergeplt: $(STDOBJ) mergeplt.o
	$(COMP) $(LFLAGS) -o ~/usr/bin/mergeplt $(STDOBJ) mergeplt.o



plt2ce: $(STDOBJ) plt2ce.o
	$(COMP) $(LFLAGS) -o ~/usr/bin/plt2ce $(STDOBJ) plt2ce.o

plt2obs: $(STDOBJ) plt2obs.o
	$(COMP) $(LFLAGS) -o ~/usr/bin/plt2obs $(STDOBJ) plt2obs.o

selplt: $(STDOBJ) selplt.o
	$(COMP) $(LFLAGS) -o ~/usr/bin/selplt $(STDOBJ) selplt.o




clean:
	rm -f *.o *.mod

cleanwarnings:
	rm -f *__genmod.f90 *__genmod.mod

