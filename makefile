FORTRAN_COMPILER = gfortran
CXX = g++

FCFLAGS = -g -c  -fbacktrace -fno-align-commons
# link flags-fdefault-real-8
FLFLAGS = -g -fbacktrace
#c++flags
CXXFLAG= -Wall -Wextra -pedantic  -std=c++17 -lstdc++ -lc++ -lgfortran
#LDFLAGS =

OBJ= main_test.o

.SUFFIXES: .f08 .cpp
.f08.0 : ; $(FORTRAN_COMPILER) $(FCFLAGS) $*.f08  -o $.o
.cpp.o : ; $(CXX) $(CXXFLAG) $*.cpp -o $.o

%.o: %.mod
#
main_testout.out ${OBJ}  $(FLFLAGS)

depend: .depend
	all: $(PROGRAM)

	$(PROGRAM): $(SRCS)
	    $(FC) $(FLFLAGS) -o $@ $<

	%.o: %.F
	    $(FC) $(FCFLAGS) -o $@ $<

clean:
    rm -f *.o *.a

dist-clean: clean
   rm -f *~ .depend

include .depend
