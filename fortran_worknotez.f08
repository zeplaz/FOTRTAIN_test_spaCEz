

  touch foo.f90; gfortran -cpp -E -dM foo.f90
-dU will output all. macors and more dealz

-isystem dir
-CC comment handling
more printouts for the
include depth -H
    Search dir for header files, after all directories specified by -I but before the standard system directories. Mark it as a system directory, so that it gets the same special treatment as is applied to the standard system directories. If dir begins with =, then the = will be replaced by the sysroot prefix; see --sysroot and -isysroot.
-fdump-fortran-optimized

-fno-backtrace

will show all the predefined macros.

 In summary then, a Fortran argument list of the form (A,B,C) must always be
  declared in C and C++ in the form (typename *a, typename *b, typename *c),
   and used in the form (&a, &b, &c).
   function C_to_F_string(c_string_pointer) result(f_string)
   use, intrinsic :: iso_c_binding, only: c_ptr,c_f_pointer,c_char,c_null_char
   type(c_ptr), intent(in) :: c_string_pointer
   character(len=:), allocatable :: f_string
   character(kind=c_char), dimension(:), pointer :: char_array_pointer => null()
   character(len=255) :: aux_string
   integer :: i,length
   call c_f_pointer(c_string_pointer,char_array_pointer,[255])
   if (.not.associated(char_array_pointer)) then
     allocate(character(len=4)::f_string); f_string="NULL"; return
   end if
   aux_string=" "
   do i=1,255
     if (char_array_pointer(i)==c_null_char) then
       length=i-1; exit
     end if
     aux_string(i:i)=char_array_pointer(i)
   end do
   allocate(character(len=length)::f_string)
   f_string=aux_string(1:length)
   end function C_to_F_string
use iso_c_binding

integer(C_INT) function add(a,b) bind(C,name="add")
    use,intrinsic::ISO_C_BINDING
    implicit none
    integer(C_INT),value::a,b
end function add


   Name	C definition	Value
   C_NULL_CHAR	null character	'\0'
   C_ALERT	alert	'\a'
   C_BACKSPACE	backspace	'\b'
   C_FORM_FEED	form feed	'\f'
   C_NEW_LINE	new line	'\n'
   C_CARRIAGE_RETURN	carriage return	'\r'
   C_HORIZONTAL_TAB	horizontal tab	'\t'
   C_VERTICAL_TAB	vertical tab	'\v'

   Moreover, the following two named constants are defined:
   Name	Type
   C_NULL_PTR	C_PTR
   C_NULL_FUNPTR	C_FUNPTR

assumed-type assumed-rank dummy arguments (TYPE(*), DIMENSION(..)) allow
for both scalars and arrays, but require special code on the
 callee side to handle the array descriptor.
   MODULE m
  IMPLICIT NONE

  ! Define interface of call-back routine.
  ABSTRACT INTERFACE
    SUBROUTINE callback (x)
      USE, INTRINSIC :: ISO_C_BINDING
      REAL(KIND=C_DOUBLE), INTENT(IN), VALUE :: x
    END SUBROUTINE callback
  END INTERFACE

CONTAINS

  ! Define C-bound procedure.
  SUBROUTINE get_values (cproc) BIND(C)
    USE, INTRINSIC :: ISO_C_BINDING
    TYPE(C_FUNPTR), INTENT(IN), VALUE :: cproc

    PROCEDURE(callback), POINTER :: proc

    ! Convert C to Fortran procedure pointer.
    CALL C_F_PROCPOINTER (cproc, proc)

    ! Call it.
    CALL proc (1.0_C_DOUBLE)
    CALL proc (-42.0_C_DOUBLE)
    CALL proc (18.12_C_DOUBLE)
  END SUBROUTINE get_values

END MODULE m

MODULE m
  USE, INTRINSIC :: ISO_C_BINDING
  IMPLICIT NONE

  ! Define interface of C function.
  INTERFACE
    INTEGER(KIND=C_INT) FUNCTION call_it (func, arg) BIND(C)
      USE, INTRINSIC :: ISO_C_BINDING
      TYPE(C_FUNPTR), INTENT(IN), VALUE :: func
      INTEGER(KIND=C_INT), INTENT(IN), VALUE :: arg
    END FUNCTION call_it
  END INTERFACE

CONTAINS

!int
call_it (int (*func)(int), int arg)
{
  return func (arg);
}


  ! Define procedure passed to C function.
  ! It must be interoperable!
  INTEGER(KIND=C_INT) FUNCTION double_it (arg) BIND(C)
    INTEGER(KIND=C_INT), INTENT(IN), VALUE :: arg
    double_it = arg + arg
  END FUNCTION double_it

  ! Call C function.
  SUBROUTINE foobar ()
    TYPE(C_FUNPTR) :: cproc
    INTEGER(KIND=C_INT) :: i

    ! Get C procedure pointer.
    cproc = C_FUNLOC (double_it)

    ! Use it.
    DO i = 1_C_INT, 10_C_INT
      PRINT *, call_it (cproc, i)
    END DO
  END SUBROUTINE foobar

END MODULE m
