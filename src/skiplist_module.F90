! File   : skiplist_module.F90
! Author : Sandeep Koranne (C) 2026.
! Purpose: Master module that uses CPP macros to generate explicitly typed
!          SkipList structures and functions for both Int32 and Int64.

module SkipListModule
    use iso_fortran_env, only: int32, int64
    implicit none
    private

    ! Make the Types and Generic Interfaces Public
    public :: SkipListInt32, SkipListInt64
    public :: InitSkipList, DestroySkipList, InsertNode, FindNode
    public :: SkipListNodeInt32, SkipListNodeInt64
    public :: SkipListPtrInt32, SkipListPtrInt64
    public :: find_first_node, get_next_key, get_prev_key
    ! ==============================================================================
    ! TYPE GENERATION BLOCK
    ! ==============================================================================
    #define SKIPLIST_GENERATE_TYPES
    ! --- Generate 32-Bit Types ---
    #define SL_TYPE SkipListInt32
    #define SL_NODE SkipListNodeInt32
    #define SL_PTR  SkipListPtrInt32
    #define SL_PAYLOAD integer(kind=int32)
    #include "skiplist_template.inc"    
    #undef SL_TYPE
    #undef SL_NODE
    #undef SL_PTR
    #undef SL_PAYLOAD

    ! --- Generate 64-Bit Types ---
    #define SL_TYPE SkipListInt64
    #define SL_NODE SkipListNodeInt64
    #define SL_PTR  SkipListPtrInt64
    #define SL_PAYLOAD integer(kind=int64)
    #include "skiplist_template.inc"
    #undef SL_TYPE
    #undef SL_NODE
    #undef SL_PTR
    #undef SL_PAYLOAD

    #undef SKIPLIST_GENERATE_TYPES


    ! ==============================================================================
    ! GENERIC INTERFACES
    ! Allows the user to call `InsertNode(MyList, Val)` regardless of the kind.
    ! ==============================================================================
    interface InitSkipList
        module procedure SkipListInt32_Init
        module procedure SkipListInt64_Init
    end interface

    interface DestroySkipList
        module procedure SkipListInt32_Destroy
        module procedure SkipListInt64_Destroy
    end interface

    interface InsertNode
        module procedure SkipListInt32_Insert
        module procedure SkipListInt64_Insert
    end interface

    interface FindNode
        module procedure SkipListInt32_Find
        module procedure SkipListInt64_Find
    end interface

    interface find_first_node
        module procedure SkipListInt32_FindFirst
        module procedure SkipListInt64_FindFirst
    end interface

    interface get_next_key
        module procedure SkipListInt32_GetNext
        module procedure SkipListInt64_GetNext
    end interface

    interface get_prev_key
        module procedure SkipListInt32_GetPrev
        module procedure SkipListInt64_GetPrev
    end interface
contains

    ! ==============================================================================
    ! PROCEDURE GENERATION BLOCK
    ! ==============================================================================
    #define SKIPLIST_GENERATE_PROCEDURES
    ! --- Generate 32-Bit Procedures ---
    #define SL_TYPE SkipListInt32
    #define SL_NODE SkipListNodeInt32
    #define SL_PTR  SkipListPtrInt32
    #define SL_PAYLOAD integer(kind=int32)    
    #define SL_INIT SkipListInt32_Init
    #define SL_DESTROY SkipListInt32_Destroy
    #define SL_GET_FREE SkipListInt32_GetFreeNode
    #define SL_RELEASE SkipListInt32_ReleaseNode
    #define SL_RAND_LVL SkipListInt32_RandomLevel
    #define SL_INSERT SkipListInt32_Insert
    #define SL_FIND SkipListInt32_Find
    #define SL_FIND_FIRST SkipListInt32_FindFirst
    #define SL_GET_NEXT   SkipListInt32_GetNext
    #define SL_GET_PREV   SkipListInt32_GetPrev
    #include "skiplist_template.inc"
    #undef SL_FIND_FIRST
    #undef SL_GET_NEXT
    #undef SL_GET_PREV
    #undef SL_TYPE
    #undef SL_NODE
    #undef SL_PTR
    #undef SL_PAYLOAD
    #undef SL_INIT
    #undef SL_DESTROY
    #undef SL_GET_FREE
    #undef SL_RELEASE
    #undef SL_RAND_LVL
    #undef SL_INSERT
    #undef SL_FIND

    ! --- Generate 64-Bit Procedures ---
    #define SL_TYPE SkipListInt64
    #define SL_NODE SkipListNodeInt64
    #define SL_PTR  SkipListPtrInt64
    #define SL_PAYLOAD integer(kind=int64)    
    #define SL_INIT SkipListInt64_Init
    #define SL_DESTROY SkipListInt64_Destroy
    #define SL_GET_FREE SkipListInt64_GetFreeNode
    #define SL_RELEASE SkipListInt64_ReleaseNode
    #define SL_RAND_LVL SkipListInt64_RandomLevel
    #define SL_INSERT SkipListInt64_Insert
    #define SL_FIND SkipListInt64_Find
    #define SL_FIND_FIRST SkipListInt64_FindFirst
    #define SL_GET_NEXT   SkipListInt64_GetNext
    #define SL_GET_PREV   SkipListInt64_GetPrev
    #include "skiplist_template.inc"
    #undef SL_FIND_FIRST
    #undef SL_GET_NEXT
    #undef SL_GET_PREV
    #undef SL_TYPE
    #undef SL_NODE
    #undef SL_PTR
    #undef SL_PAYLOAD
    #undef SL_INIT
    #undef SL_DESTROY
    #undef SL_GET_FREE
    #undef SL_RELEASE
    #undef SL_RAND_LVL
    #undef SL_INSERT
    #undef SL_FIND

    

    #undef SKIPLIST_GENERATE_PROCEDURES
end module SkipListModule
