module pic_string_utils
   use pic_types
   implicit none
   ! Generic interface for to_string to handle different types
   interface to_string
      module procedure to_string_int32
      module procedure to_string_int64
      module procedure to_string_dp
      module procedure to_string_char
      module procedure to_string_logical
   end interface

   interface operator(+)
      module procedure concatenate_strings
   end interface

contains

   function concatenate_strings(str1, str2) result(concatenated)
      character(len=*), intent(in) :: str1, str2
      character(len=len(str1) + len(str2)) :: concatenated
      concatenated = str1//str2
      concatenated = trim(concatenated)
   end function concatenate_strings

   ! Overloaded to_string function for integer
   function to_string_int32(i) result(trimmed_str)
      integer(kind=int32), intent(in) :: i
      character(len=50) :: str
      character(len=:), allocatable :: trimmed_str
      write (str, '(I0)') i  ! Convert integer to string without leading spaces
      trimmed_str = trim(str)
   end function to_string_int32

   function to_string_int64(i) result(trimmed_str)
      integer(kind=int64), intent(in) :: i
      character(len=50) :: str
      character(len=:), allocatable :: trimmed_str
      write (str, '(I0)') i  ! Convert integer to string without leading spaces
      trimmed_str = trim(str)
   end function to_string_int64

   ! Overloaded to_string function for real
   function to_string_dp(r) result(trimmed_str)
      double precision, intent(in) :: r
      character(len=50) :: str
      character(len=:), allocatable :: trimmed_str
      write (str, '(F0.12)') r  ! Convert real to string with 3 decimal places
      trimmed_str = trim(str)
   end function to_string_dp

   ! Overloaded to_string function for character
   function to_string_char(c) result(trimmed_str)
      character(len=*), intent(in) :: c
      character(len=500) :: str
      character(len=:), allocatable :: trimmed_str
      str = c
      trimmed_str = trim(str)
   end function to_string_char

   function to_string_logical(l) result(trimmed_str)
      logical, intent(in) :: l
      character(len=5) :: str
      character(len=:), allocatable :: trimmed_str
      if (l) then
         str = 'TRUE'
      else
         str = 'FALSE'
      end if
      trimmed_str = trim(str)
   end function to_string_logical

end module pic_string_utils
