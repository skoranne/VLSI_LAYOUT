dnl File   : op_template.m4
dnl Author : Sandeep Koranne (C) 2026
dnl Purpose: Easy to use macro facility
define(`OP1', `exec run $1:$2_1 = $3:$2 GROW nothing $4 $4 $4 $4
exec run $1:$2_2 = $3:$2 SHRINK $1:nothing $4 $4 $4 $4
exec run $1:$2_outer_ring = $1:$2_1 - $3:$2
exec run $1:$2_inner_ring = $3:$2 - $1:$2_2
exec run $1:$2_ring_and1  = $1:$2_outer_ring * $3:$2
exec run $1:$2_ring_intersection  = $1:$2_outer_ring * $1:$2_inner_ring
exec run nothing = $1:$2_ring_and1 ASSERT_ZERO $1:nothing
exec run nothing = $1:$2_ring_intersection ASSERT_ZERO $1:nothing')dnl
