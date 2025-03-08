module pic_logger
  !! console specific logger, will control printing at different levels
  !! intended to be used to selectively print out information depending
  !! on a log level requested
   use pic_types, only: default_int

   implicit none(type, external)
   private
   public :: global_logger, logger_type

   character(*), parameter :: name = 'pic_logger'
     !! experimental variable information, this will tell the logger what module and procedure a print is called from
   integer, parameter, public :: debug_level = 10
     !! highest logger level, will show _everything_
   integer, parameter, public :: verbose_level = 9
     !! detailed log level, will not show program state such as "calling blas library"
   integer, parameter, public :: info_level = 8
     !! default level, will show the most important information for program execution
   integer, parameter, public :: performance_level = 7
     !! will show performance related information such as timings for subroutines, FLOP rates for BLAS calls
   integer, parameter, public :: warning_level = 6
     !! will only show warnings, such as "matrix is nearly singular!"
   integer, parameter, public :: error_level = 5
     !! will only show catastrophic error that will crash the program

   type :: logger_type

      private

      integer(default_int), public :: log_level = info_level

   contains

      procedure, public, pass(self) :: configuration
      procedure, public, pass(self) :: configure
      procedure, public, pass(self) :: log
      procedure, public, pass(self) :: debug
      procedure, public, pass(self) :: verbose
      procedure, public, pass(self) :: info
      procedure, public, pass(self) :: performance
      procedure, public, pass(self) :: warning
      procedure, public, pass(self) :: error

   end type logger_type

   type(logger_type) :: global_logger

contains

   pure subroutine configuration(self, level)
     !! gets the configuration for the logger, i.e. what level it is set to
      class(logger_type), intent(in) :: self
      integer(default_int), intent(out), optional :: level
      if (present(level)) level = self%log_level
   end subroutine configuration

   pure subroutine configure(self, level)
     !! sets the logger to a default print level
      class(logger_type), intent(inout) :: self
      integer(default_int), intent(in), optional :: level
      if (present(level)) self%log_level = level
   end subroutine configure

   subroutine debug(self, message, module, procedure)
     !! wrapper for debug level printing
      class(logger_type), intent(in) :: self
      character(*), intent(in) :: message
      character(*), intent(in), optional :: module, procedure
      call self%log("DEBUG", message, module, procedure)
   end subroutine debug

   subroutine verbose(self, message, module, procedure)
     !! wrapper for verbose level printer
      class(logger_type), intent(in) :: self
      character(*), intent(in) :: message
      character(*), intent(in), optional :: module, procedure
      call self%log("VERBOSE", message, module, procedure)
   end subroutine verbose

   subroutine info(self, message, module, procedure)
     !! wrapper for info level printer
      class(logger_type), intent(in) :: self
      character(*), intent(in) :: message
      character(*), intent(in), optional :: module, procedure
      call self%log("INFO", message, module, procedure)
   end subroutine info

   subroutine warning(self, message, module, procedure)
     !! wrapper for warning level printer
      class(logger_type), intent(in) :: self
      character(*), intent(in) :: message
      character(*), intent(in), optional :: module, procedure
      call self%log("WARNING", message, module, procedure)
   end subroutine warning

   subroutine performance(self, message, module, procedure)
     !! wrapper for performance level printer
      class(logger_type), intent(in) :: self
      character(*), intent(in) :: message
      character(*), intent(in), optional :: module, procedure
      call self%log("PERFORMANCE", message, module, procedure)
   end subroutine performance

   subroutine error(self, message, module, procedure)
     !! wrapper for error level printer
      class(logger_type), intent(in) :: self
      character(*), intent(in) :: message
      character(*), intent(in), optional :: module, procedure
      call self%log("ERROR", message, module, procedure)
   end subroutine error

   subroutine log(self, level, message, module, procedure)
     !! main routine for printing at the desired log level
      class(logger_type), intent(in) :: self
      character(*), intent(in) :: level
      character(*), intent(in) :: message
      character(*), intent(in), optional :: module, procedure

      integer :: log_level_value

      select case (trim(level))
      case ('DEBUG')
         log_level_value = debug_level
      case ('VERBOSE')
         log_level_value = verbose_level
      case ('INFO')
         log_level_value = info_level
      case ('WARNING')
         log_level_value = warning_level
      case ('PERFORMANCE')
         log_level_value = performance_level
      case ('ERROR')
         log_level_value = error_level
      case default
         write (*, *) 'ERROR: Invalid log level "', trim(level), '"'
         return
      end select

      if (self%log_level >= log_level_value) then
         if (present(module) .and. present(procedure)) then
            write (*, *) trim(level), ': ', module, procedure, ': ', message
         else if (present(module)) then
            write (*, *) trim(level), ': ', module, ': ', message
         else
            write (*, *) trim(level), ': ', message
         end if
      end if
   end subroutine log

end module pic_logger
