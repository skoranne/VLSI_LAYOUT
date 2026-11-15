! Fortran ISO_C_BINDING interface to src/db layoutdb (GDSII/OASIS reader)
module layoutdb_f_api
  use iso_c_binding
  implicit none

  type, bind(C) :: ldb_extent_c
     integer(c_int64_t) :: xmin, ymin, xmax, ymax
     integer(c_int)     :: valid
  end type ldb_extent_c

  interface
     function ldb_open_c(path, errbuf, errbuf_sz) bind(C, name="ldb_open")
       import :: c_ptr, c_char, c_size_t
       type(c_ptr) :: ldb_open_c
       character(kind=c_char), dimension(*), intent(in) :: path
       character(kind=c_char), dimension(*), intent(inout) :: errbuf
       integer(c_size_t), value :: errbuf_sz
     end function ldb_open_c

     subroutine ldb_close_c(f) bind(C, name="ldb_close")
       import :: c_ptr
       type(c_ptr), value :: f
     end subroutine ldb_close_c

     function ldb_format_c(f) bind(C, name="ldb_format")
       import :: c_ptr, c_int
       integer(c_int) :: ldb_format_c
       type(c_ptr), value :: f
     end function ldb_format_c

     function ldb_iterate_boxes_c(f, cb, ud) bind(C, name="ldb_iterate_boxes")
       import :: c_ptr, c_funptr, c_int
       integer(c_int) :: ldb_iterate_boxes_c
       type(c_ptr), value :: f
       type(c_funptr), value :: cb
       type(c_ptr), value :: ud
     end function ldb_iterate_boxes_c

     function ldb_print_hierarchy_c(f, out_fp) bind(C, name="ldb_print_hierarchy")
       import :: c_ptr, c_int
       integer(c_int) :: ldb_print_hierarchy_c
       type(c_ptr), value :: f
       type(c_ptr), value :: out_fp
     end function ldb_print_hierarchy_c

     function ldb_iterate_boxes_hier_c(f, top_cell, depth, cb, ud) bind(C, name="ldb_iterate_boxes_hier")
       import :: c_ptr, c_funptr, c_int, c_char
       integer(c_int) :: ldb_iterate_boxes_hier_c
       type(c_ptr), value :: f
       character(kind=c_char), dimension(*), intent(in) :: top_cell
       integer(c_int), value :: depth
       type(c_funptr), value :: cb
       type(c_ptr), value :: ud
     end function ldb_iterate_boxes_hier_c

     function ldb_extent_whole_hier_c(f, top_cell, out_ext) bind(C, name="ldb_extent_whole_hier")
       import :: c_ptr, c_int, c_char, ldb_extent_c
       integer(c_int) :: ldb_extent_whole_hier_c
       type(c_ptr), value :: f
       character(kind=c_char), dimension(*), intent(in) :: top_cell
       type(ldb_extent_c), intent(out) :: out_ext
     end function ldb_extent_whole_hier_c

     function ldb_top_cell_count_c(f) bind(C, name="ldb_top_cell_count")
       import :: c_ptr, c_int
       integer(c_int) :: ldb_top_cell_count_c
       type(c_ptr), value :: f
     end function ldb_top_cell_count_c

     function ldb_top_cell_name_c(f, i) bind(C, name="ldb_top_cell_name")
       import :: c_ptr, c_int
       type(c_ptr) :: ldb_top_cell_name_c
       type(c_ptr), value :: f
       integer(c_int), value :: i
     end function ldb_top_cell_name_c
  end interface

end module layoutdb_f_api
