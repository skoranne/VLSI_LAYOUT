!=====================================================================
!  skiplist_module.F90
!  ------------------
!  Public module that supplies a kind‑parameterised skip‑list.
!  The heavy lifting lives in skiplist_template.inc, which is included
!  twice (int32 and int64) via the C‑pre‑processor.
!=====================================================================
module SkipListModule2
  use iso_fortran_env, only: int32, int64, real64
  implicit none
  private

  !=================================================================
  !  Public container type – parameterised by integer kind K.
  !=================================================================
  type, public :: SkipList(k)
     integer,          public :: maxLevel = 0               ! maximum height
     real,             public :: prob     = 0.0              ! promotion probability
     type(SkipNode(k)), pointer :: head => null()          ! sentinel node
     type(SkipNode(k)), pointer :: arena(:) => null()      ! pre‑allocated nodes
     type(SkipNode(k)), pointer :: freePtr => null()       ! LIFO free‑list
  end type SkipList

  !-----------------------------------------------------------------
  !  Node type – the same definition is used for both 32‑ and 64‑bit.
  !-----------------------------------------------------------------
  type, public :: SkipNode(k)
     integer(k)                     :: key   = 0_k
     integer(k)                     :: value = 0_k
     integer                        :: level = 0
     type(SkipNode(k)), pointer :: forward(:) => null()
  end type SkipNode

  !=================================================================
  !  Generic interfaces – the user calls the generic name; the
  !  pre‑processor supplies the kind‑specific implementations.
  !=================================================================
  interface init_skiplist
     module procedure push_32_init_skiplist, push_64_init_skiplist
  end interface init_skiplist

  interface insert_node
     module procedure push_32_insert_node, push_64_insert_node
  end interface insert_node

  interface find_candidate
     module procedure push_32_find_candidate, push_64_find_candidate
  end interface find_candidate

  interface find_first_node
     module procedure push_32_find_first_node, push_64_find_first_node
  end interface find_first_node

  interface next_key
     module procedure push_32_next_key, push_64_next_key
  end interface next_key

  interface prev_key
     module procedure push_32_prev_key, push_64_prev_key
  end interface prev_key

  interface destroy_skiplist
     module procedure push_32_destroy_skiplist, push_64_destroy_skiplist
  end interface destroy_skiplist

  interface get_free_node
     module procedure push_32_get_free_node, push_64_get_free_node
  end interface get_free_node

  interface release_node
     module procedure push_32_release_node, push_64_release_node
  end interface release_node

  interface random_level
     module procedure push_32_random_level, push_64_random_level
  end interface random_level

  public :: &
       init_skiplist, insert_node, find_candidate, find_first_node, &
       next_key, prev_key, destroy_skiplist, &
       get_free_node, release_node, random_level, &
       SkipList, SkipNode

contains

  !=================================================================
  !  Include the template twice – once for int32 and once for int64.
  !=================================================================
  !--- 32‑bit version -------------------------------------------------
#define SKIPLIST_GENERATE_TYPES
#define KIND_VAL int32
#define PROC_NAME push_32
#include "./include/skiplist.inc"
#undef KIND_VAL
#undef PROC_NAME

  !--- 64‑bit version -------------------------------------------------
#define KIND_VAL int64
#define PROC_NAME push_64
#include "./include/skiplist.inc"
#undef KIND_VAL
#undef PROC_NAME
#undef SKIPLIST_GENERATE_TYPES
end module SkipListModule2
