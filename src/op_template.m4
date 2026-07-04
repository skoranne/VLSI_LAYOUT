dnl File   : op_template.m4
dnl Author : Sandeep Koranne (C) 2026
dnl Purpose: Easy to use macro facility
define(`__USER__', esyscmd(`printf "%s" "$USER"'))dnl
define(`__HOME__', esyscmd(`printf "%s" "$HOME"'))dnl
define(`__PWD__', esyscmd(`printf "%s" "$PWD"'))dnl
dnl define(`__D1_INPUT__', esyscmd(`printf "%s" "$D1_INPUT"'))dnl
dnl define(`mkTempFolder', esyscmd(`printf "%s" "mktemp -d MTL_TEMP_XXX"'))dnl
define(`mkTempFolder', esyscmd(`printf "%s" "$(mktemp -d MTL_TEMP_XXX)"'))dnl
define(`RUN_LOOP', `ifelse(`$4', `', `',
`$1(`$2', `$3', `$4', `$5')
RUN_LOOP(`$1', `$2', `$3', shift(shift(shift(shift(shift($@))))))')')dnl
dnl t1:poly_op = d1:poly GROW t1:nothing 0.1 0.1 0.1 0.1
define(`OP1', `# Generating $@
exec run $1:$3_1 = $2:$3 GROW $1:nothing $4 $4 $4 $4
exec run $1:$3_2 = $2:$3 SIZE $1:nothing -$4
exec run $1:$3_outer_ring = $1:$3_1 ~ $2:$3
exec run $1:$3_inner_ring = $2:$3 ~ $1:$3_2
exec run $1:$3_ring_and1  = $1:$3_outer_ring * $2:$3
exec run $1:$3_ring_intersection  = $1:$3_outer_ring * $1:$3_inner_ring
exec run $1:nothing = $1:$3_ring_and1 ASSERT_ZERO $1:nothing
exec run $1:nothing = $1:$3_ring_intersection ASSERT_ZERO $1:nothing')dnl


