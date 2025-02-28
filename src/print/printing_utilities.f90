module pic_message_printer
   use pic_string_utils
   implicit none
   public :: print_message
contains

   ! Subroutine to print a message
   subroutine print_message(message)
      character(len=*), intent(in) :: message
      print *, trim(message)
   end subroutine print_message

end module pic_message_printer

module pic_matrix_printer
   use pic_types
   use pic_string_utils
   implicit none
   !public :: print_array, print_array_with_bounds
   !private  ! Make all procedures private by default

   interface print_array
      module procedure print_vector
      module procedure print_matrix
   end interface print_array

   interface print_array_with_bounds
      module procedure print_vector_n
      module procedure print_matrix_m_n
   end interface print_array_with_bounds

   character(len=*), parameter :: fmt_edge = '(F14.10)'
   character(len=*), parameter :: fmt_in = '(F14.10, ", ")'

contains

   subroutine print_vector_n(vec, n_elements, format_type)
      implicit none
      double precision, intent(in) :: vec(:)
      character(len=*), intent(in), optional :: format_type
      character(len=20) :: format_selected
      integer(kind=default_int), intent(in) :: n_elements
      ! Determine the format: default to "PLAIN" if not specified
      if (present(format_type)) then
         format_selected = trim(adjustl(format_type))
      else
         format_selected = 'PLAIN'
      end if
      ! Handle plain format separately or delegate to print routine based on the format
      if (format_selected == 'PLAIN') then
         call print_plain_vector(vec, n_elements)
      else
         call print_vector_in_format(vec, format_selected, n_elements)
      end if
   end subroutine print_vector_n
   ! Subroutine to print a vector (1D array)
   subroutine print_vector(vec, format_type)
      implicit none
      real(kind=dp), intent(in) :: vec(:)  ! 1D array
      character(len=*), intent(in), optional :: format_type
      character(len=20) :: format_selected

      ! Determine the format: default to "PLAIN" if not specified
      if (present(format_type)) then
         format_selected = trim(adjustl(format_type))
      else
         format_selected = 'PLAIN'
      end if
      ! Handle plain format separately or delegate to print routine based on the format
      if (format_selected == 'PLAIN') then
         call print_plain_vector(vec)
      else
         call print_vector_in_format(vec, format_selected)
      end if
   end subroutine print_vector

   ! Subroutine to print a matrix (2D array)
   subroutine print_matrix_m_n(mat, n_cols, n_rows, format_type)
      implicit none
      real(kind=dp), intent(in) :: mat(:, :)  ! 2D array
      integer(kind=default_int), intent(in) :: n_cols, n_rows
      character(len=*), intent(in), optional :: format_type
      character(len=20) :: format_selected
      ! Determine the format: default to "PLAIN" if not specified
      if (present(format_type)) then
         format_selected = trim(adjustl(format_type))
      else
         format_selected = 'PLAIN'
      end if
      ! Handle plain format separately or delegate to print routine based on the format
      if (format_selected == 'PLAIN') then
         call print_plain_matrix(mat, n_cols, n_rows)
      else
         call print_matrix_in_format(mat, format_selected, n_cols, n_rows)
      end if
   end subroutine print_matrix_m_n

   ! Subroutine to print a matrix (2D array)
   subroutine print_matrix(mat, format_type)
      implicit none
      real(kind=dp), intent(in) :: mat(:, :)  ! 2D array
      character(len=*), intent(in), optional :: format_type
      character(len=20) :: format_selected
      ! Determine the format: default to "PLAIN" if not specified
      if (present(format_type)) then
         format_selected = trim(adjustl(format_type))
      else
         format_selected = 'PLAIN'
      end if
      ! Handle plain format separately or delegate to print routine based on the format
      if (format_selected == 'PLAIN') then
         call print_plain_matrix(mat)
      else
         call print_matrix_in_format(mat, format_selected)
      end if
   end subroutine print_matrix

   ! Private subroutine to print plain format for vectors
   subroutine print_plain_vector(vec, n_elements)
      implicit none
      real(kind=dp), intent(in) :: vec(:)
      integer(kind=default_int), intent(in), optional :: n_elements
      integer(kind=default_int) :: i, loop_bound
      if (present(n_elements)) then
         loop_bound = n_elements
      else
         loop_bound = size(vec)
      end if
      print *, "Vector (Plain format):"
      do i = 1, loop_bound
         write (*, fmt_edge) vec(i)
      end do
   end subroutine print_plain_vector

   ! Private subroutine to print plain format for matrices
   subroutine print_plain_matrix(mat, n_cols, n_rows)
      implicit none
      real(kind=dp), intent(in) :: mat(:, :)
      integer(kind=default_int), intent(in), optional :: n_cols, n_rows
      integer(kind=default_int) :: i, j, loop_bound_i, loop_bound_j
      if (present(n_cols) .and. present(n_rows)) then
         loop_bound_i = n_cols
         loop_bound_j = n_rows
      else
         loop_bound_i = size(mat, 1)
         loop_bound_j = size(mat, 2)
      end if
      print *, "Matrix (Plain format):"
      do i = 1, loop_bound_i
         do j = 1, loop_bound_j
            if (j == loop_bound_j) then
               write (*, fmt_edge, advance="yes") mat(i, j)  ! Last element in the row, new line
            else
               write (*, fmt_in, advance="no") mat(i, j)  ! In-between elements
            end if
         end do
      end do
   end subroutine print_plain_matrix

   ! Subroutine to print vector in a specific format (NumPy/Mathematica)
   subroutine print_vector_in_format(vec, format_type, n_elements)
      implicit none
      real(kind=dp), intent(in) :: vec(:)
      character(len=*), intent(in) :: format_type
      integer(kind=default_int), intent(in), optional :: n_elements
      character(len=1) :: open_bracket, close_bracket
      integer(kind=default_int) :: i, loop_bound_i

      if (present(n_elements)) then
         loop_bound_i = n_elements
      else
         loop_bound_i = size(vec)
      end if
      ! Select brackets based on format type
      if (format_type == 'NUMPY') then
         open_bracket = '['
         close_bracket = ']'
      else if (format_type == 'MATHEMATICA') then
         open_bracket = '{'
         close_bracket = '}'
      else
         print *, "Error: Unsupported format type. Defaulting to NumPy format."
         open_bracket = '['
         close_bracket = ']'
      end if
      ! Print the vector in the selected format
      print *, "Vector (", trim(format_type), " format):"
      print *, open_bracket
      do i = 1, loop_bound_i
         if (i == loop_bound_i) then  ! Last element in the vector
            write (*, fmt_edge, advance="no") vec(i)
         else  ! Elements in between
            write (*, fmt_in, advance="no") vec(i)
         end if
      end do
      print *, close_bracket
   end subroutine print_vector_in_format

   ! Subroutine to print matrix in a specific format (NumPy/Mathematica)
   subroutine print_matrix_in_format(mat, format_type, n_cols, n_rows)
      implicit none
      real(kind=dp), intent(in) :: mat(:, :)
      character(len=*), intent(in) :: format_type
      character(len=1) :: open_bracket, close_bracket
      integer(kind=default_int), intent(in), optional :: n_cols, n_rows
      integer(kind=default_int) :: i, j, loop_bound_i, loop_bound_j
      if (present(n_cols) .and. present(n_rows)) then
         loop_bound_i = n_cols
         loop_bound_j = n_rows
      else
         loop_bound_i = size(mat, 1)
         loop_bound_j = size(mat, 2)
      end if

      ! Select brackets based on format type
      if (format_type == 'NUMPY') then
         open_bracket = '['
         close_bracket = ']'
      else if (format_type == 'MATHEMATICA') then
         open_bracket = '{'
         close_bracket = '}'
      else
         print *, "Error: Unsupported format type. Defaulting to NumPy format."
         open_bracket = '['
         close_bracket = ']'
      end if

      ! Print the matrix in the selected format
      print *, "Matrix (", trim(format_type), " format):"
      print *, open_bracket
      do i = 1, loop_bound_i
         write (*, '(A)', advance="no") open_bracket  ! Start of a row
         do j = 1, loop_bound_j
            if (j == loop_bound_j) then  ! Last element in the row
               write (*, fmt_edge, advance="no") mat(i, j)
            else  ! Elements in between
               write (*, fmt_in, advance="no") mat(i, j)
            end if
         end do
         if (i == loop_bound_i) then
            print *, close_bracket  ! Close bracket without a comma for the last row
         else
            print *, close_bracket, ","  ! Close bracket with a comma for all other rows
         end if
      end do
      print *, close_bracket
   end subroutine print_matrix_in_format

end module pic_matrix_printer
