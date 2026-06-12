module vector_mod
    use iso_fortran_env, only: int32, int64
    implicit none

    ! 1. The Parameterized Derived Type (Pure Data)
    type :: Vector(k)
        integer, kind :: k
        integer(kind=k), allocatable :: arr(:)
        integer :: count = 0
    end type Vector

    ! 2. The Generic Interface
    ! This cleanly routes standard procedure calls based on the variable's kind
    interface push
        module procedure push_32
        module procedure push_64
    end interface

contains

    ! 3. The Preprocessor Trick
    ! We stamp out the 32-bit implementation
    #define KIND_VAL int32
    #define PROC_NAME push_32
    #include "vector_template.inc"
    #undef KIND_VAL
    #undef PROC_NAME

    ! We stamp out the 64-bit implementation
    #define KIND_VAL int64
    #define PROC_NAME push_64
    #include "vector_template.inc"
    #undef KIND_VAL
    #undef PROC_NAME

end module vector_mod

program test_pdt
    use iso_fortran_env, only: int32, int64
    use vector_mod
    implicit none

    ! Instantiate our Parameterized Types
    type(Vector(int32)) :: vec32
    type(Vector(int64)) :: vec64

    ! The generic interface figures out which implementation to call
    call push(vec32, 10_int32)
    call push(vec64, 9999999999_int64)

    print *, "Vec32 count: ", vec32%count, " | Array(1): ", vec32%arr(1)
    print *, "Vec64 count: ", vec64%count, " | Array(1): ", vec64%arr(1)
end program test_pdt
