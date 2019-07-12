
interface

use iso_c_binding
implicit none

implicit real(c_long_double)()
end interface
! interfance is kinda setupz the bits which will be used in
interface
  function create_c_obj_test_fr_func(in_dvar_ld,in_dvar_d,out_dumvar,inout) bind(C, name= "func_crete_called_in_c")
    use iso_c_binding
    implicit none
    type(*):: c_ptr_via_type
    type(c_ptr) :: create_c_obj_test_fr_func
    integer,

    real(c_long_double) :: create_c_obj_test_fr_func
    real(c_long_double) :: array1

    real, target::array(),scalar_utilb
    real,pointer ::for_port(:), ptr_nm

    real(c_long_double),intent(out) :: out_dumvar
    real(c_long_double),intent(inout) :: out_dumvar

    !not think intent for in can often be assumed?
    real(c_long_double), value :: in_dvar_ld
    real(c_double), value:: in_dvar_d

  end function create_c_obj_test_fr_func
end interface

type ptr_1D
  real, pointer :: array_a(:)
end type ptr_1D


//
  character (len=1, kind=c_char), dimension(:), pointer :: filchar=>null()

subroutine get_c_data_frm_ptr(ptr,name,type,counf,data) bind (c, name='get_c_data_frm_ptr')
  use iso_c_binding
  implicit none
  type(c_ptr), value, intent(in) :: data

  end subroutine

  type(c_ptr), value, intent(in) :: data
  subroutine destroy_objk_testfr(test_obj) bind (C, "destz_test_obj_incall")
      use iso_c_binding
      implicit none
        end subroutine



program test_gauss_sparse
    implicit none

!   explicit interface to the gauss_sparse function
    interface
        function gauss_sparse(num_iter, tol, b, A, x, actual_iter) result(tol_max)
           real ::  tol_max
           integer, intent(in) :: num_iter
           real, intent(in) :: tol
           real, intent(in), dimension(:) :: b, A(:,:)
           real, intent(inout) :: x(:)
           integer, optional, intent(out) :: actual_iter
        end function
    end interface

!   declare variables
    integer :: i, N = 3, actual_iter
    real :: residue
    real, allocatable :: A(:,:), x(:), b(:)

!   allocate arrays
    allocate (A(N, N), b(N), x(N))

!   Initialize matrix
    A = reshape([(real(i), i = 1, size(A))], shape(A))

!   Make matrix diagonally dominant
    do i = 1, size(A, 1)
        A(i,i) = sum(A(i,:)) + 1
    enddo

!   Initialize b
    b = [(i, i = 1, size(b))]

!   Initial (guess) solution
    x = b

!   invoke the gauss_sparse function
    residue = gauss_sparse(num_iter = 100, &
                           tol = 1E-5, &
                           b = b, &
                           A = a, &
                           x = x, &
                           actual_iter = actual_iter)

!   Output
    print '(/ "A = ")'
    do i = 1, size(A, 1)
        print '(100f6.1)', A(i,:)
    enddo

    print '(/ "b = " / (f6.1))', b

    print '(/ "residue = ", g10.3 / "iterations = ", i0 / "solution = "/ (11x, g10.3))', &
        residue, actual_iter, x

end program test_gauss_sparse


function gauss_sparse(num_iter, tol, b, A, x, actual_iter) result(tol_max)

!  This function solves a system of equations (Ax = b) by using the Gauss-Seidel Method

   implicit none

   real ::  tol_max

!  Input: its value cannot be modified from within the function
   integer, intent(in) :: num_iter
   real, intent(in) :: tol
   real, intent(in), dimension(:) :: b, A(:,:)

!  Input/Output: its input value is used within the function, and can be modified
   real, intent(inout) :: x(:)

!  Output: its value is modified from within the function, only if the argument is required
   integer, optional, intent(out) :: actual_iter

!  Locals
   integer :: i, n, iter
   real :: xk

!  Initialize values
   n = size(b)  ! Size of array, obtained using size intrinsic function
   tol_max = 2. * tol
   iter = 0

!  Compute solution until convergence
   convergence_loop: do while (tol_max >= tol .and. iter < num_iter); iter = iter + 1

      tol_max = -1.  ! Reset the tolerance value

!     Compute solution for the k-th iteration
      iteration_loop: do i = 1, n

!        Compute the current x-value
         xk = (b(i) - dot_product(A(i,:i-1),x(:i-1)) - dot_product(A(i,i+1:n),x(i+1:n))) / A(i, i)

!        Compute the error of the solution
!        dot_product(a,v)=a'b
         tol_max = max((abs(x(i) - xk)/(1. + abs(xk))) ** 2, abs(A(i, i) * (x(i) - xk)), tol_max)
         x(i) = xk
      enddo iteration_loop
   enddo convergence_loop

   if (present(actual_iter)) actual_iter = iter

end function gauss_sparse
