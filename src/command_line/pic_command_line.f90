module pic_command_line
  !! command line interaction module
   implicit none(type, external)

   public

contains

   function get_first_argument_from_command_line() result(filename)
      character(len=255) :: filename
      character(len=255) :: arg
      integer :: num_args

      num_args = command_argument_count()

      if (num_args < 1) then
         write (*, '(A)') 'Usage: ./my_executable <filename>'
         stop 1
      end if

      call get_command_argument(1, arg)

      filename = trim(adjustl(arg))

   end function get_first_argument_from_command_line

end module pic_command_line
