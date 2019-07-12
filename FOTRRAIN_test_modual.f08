 MODULE f_test_mod_c
    IMPLICIT NONE

    !abstrac call back interface

    ABSTRACT INTERFACE
      SUBROUTINE sub_call_back_ABST(vala)
        USE, INTRINSIC :: IOS_C_BINDING
        REAL(KIND=c_long_double),INTENT(IN), VALUE :: vala
      END SUBROUTINE sub_call_back_ABST
    END INTERFACE

    CONTAINS

    SUBROUTINE fortain_func_do_x(in_c_func_ptr) BIND(C)
      USE INTRINSIC :: IOS_C_BINDING
      TYPE(C_FUNPTR), INTENT(IN), VALUE :: in_c_func_ptr

      PROCEDURE(sub_call_back_ABST),POINTER :: ft_ptr_for_func
      CALL C_F_PROCPOINTER (in_c_func_ptr,ft_ptr_for_func)
    END SUBROUTINE fortain_func_do_x

    SUBROUTINE some_fortan_processsub(the_in_valuz)

    PRINT *,
    
    END SUBROUTINE some_fortan_processsub







    END MODULE f_test_mod_c
