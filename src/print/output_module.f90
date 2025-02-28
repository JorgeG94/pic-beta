module pic_output
   use pic_types
   use pic_output_helpers
   implicit none
   private

   type :: Logger
    !! A simple logger class for outputting messages at different verbosity levels
      integer(kind=default_int) :: level = 4
   contains
      procedure :: set_verbosity
      procedure :: debug
      procedure :: verbose
      procedure :: standard
      procedure :: info
      procedure :: minimal
      procedure :: warning
      procedure :: fatal_error
   end type Logger

   ! Define verbosity levels as public constants
   integer(kind=default_int), parameter :: VERBOSITY_DEBUG = 6
   integer(kind=default_int), parameter :: VERBOSITY_VERBOSE = 5
   integer(kind=default_int), parameter :: VERBOSITY_STANDARD = 4
   integer(kind=default_int), parameter :: VERBOSITY_INFO = 3
   integer(kind=default_int), parameter :: VERBOSITY_MINIMAL = 2
   integer(kind=default_int), parameter :: VERBOSITY_WARNING = 1
   integer(kind=default_int), parameter :: VERBOSITY_ERROR = 0
   integer(kind=default_int), parameter :: asterisk_level = 15

   public :: Logger

contains

   ! Subroutine to set verbosity level from string
   subroutine set_verbosity(this, level_str)
      class(Logger), intent(inout) :: this
      character(len=*), intent(in) :: level_str

      select case (trim(adjustl(level_str)))
      case ('DEBUG')
         this%level = VERBOSITY_DEBUG
      case ('VERBOSE')
         this%level = VERBOSITY_VERBOSE
      case ('STANDARD')
         this%level = VERBOSITY_STANDARD
      case ('INFO')
         this%level = VERBOSITY_INFO
      case ('MINIMAL')
         this%level = VERBOSITY_MINIMAL
      case ('WARNING')
         this%level = VERBOSITY_WARNING
      case ('ERROR')
         this%level = VERBOSITY_ERROR
      case default
         print *, "Warning: Unrecognized verbosity level. Defaulting to MINIMAL."
         this%level = VERBOSITY_STANDARD
      end select
   end subroutine set_verbosity

   subroutine debug(this, msg)
      class(Logger), intent(in) :: this
      character(len=*), intent(in) :: msg
      if (this%level >= VERBOSITY_DEBUG) then
         print *, "[DEBUG]: ", trim(msg)
      end if
   end subroutine debug

   subroutine verbose(this, msg)
      class(Logger), intent(in) :: this
      character(len=*), intent(in) :: msg
      if (this%level >= VERBOSITY_VERBOSE) then
         print *, "[VERBOSE]: ", trim(msg)
      end if
   end subroutine verbose

   subroutine standard(this, msg)
      class(Logger), intent(in) :: this
      character(len=*), intent(in) :: msg
      if (this%level >= VERBOSITY_STANDARD) then
         print *, "[STANDARD]: ", trim(msg)
      end if
   end subroutine standard

   subroutine info(this, msg)
      class(Logger), intent(in) :: this
      character(len=*), intent(in) :: msg
      if (this%level >= VERBOSITY_INFO) then
         print *, "[INFO]: ", trim(msg)
      end if
   end subroutine info

   subroutine minimal(this, msg)
      class(Logger), intent(in) :: this
      character(len=*), intent(in) :: msg
      if (this%level >= VERBOSITY_MINIMAL) then
         print *, "[MINIMAL]: ", trim(msg)
      end if
   end subroutine minimal

   subroutine warning(this, msg)
      class(Logger), intent(in) :: this
      character(len=*), intent(in) :: msg
      if (this%level >= VERBOSITY_WARNING) then
         call print_asterisk_row(asterisk_level)
         print *, "[WARNING]: ", trim(msg)
         call print_asterisk_row(asterisk_level)
      end if
   end subroutine warning

   subroutine fatal_error(this, msg)
      class(Logger), intent(in) :: this
      character(len=*), intent(in) :: msg
      if (this%level >= VERBOSITY_ERROR) then
         call print_asterisk_row(asterisk_level)
         print *, "[FATAL_ERROR]: ", trim(msg)
         call print_asterisk_row(asterisk_level)
      end if
      stop
   end subroutine fatal_error

end module pic_output
