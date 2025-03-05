module pic_message_printer
   use pic_string_utils, only: to_string
   implicit none (type, external)
   public :: print_message
   private
contains

   ! Subroutine to print a message
   subroutine print_message(message)
      character(len=*), intent(in) :: message
      print *, trim(message)
   end subroutine print_message

end module pic_message_printer
