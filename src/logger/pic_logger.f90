module pic_logger
   use pic_types, only: default_int

   implicit none(type, external)
   private
   public :: global_logger, logger_type

   character(*), parameter :: name = 'pic_logger'
   integer, parameter, public :: &
      debug_level = 10, &
      verbose_level = 9, &
      info_level = 8, &
      warning_level = 7, &
      performance_level = 6, &
      error_level = 5

   type :: logger_type

      private

      integer(default_int), public :: log_level = info_level

   contains

      procedure, public, pass(self) :: configuration
      procedure, public, pass(self) :: configure
      procedure, public, pass(self) :: debug
      procedure, public, pass(self) :: verbose
      procedure, public, pass(self) :: info
      procedure, public, pass(self) :: warning
      procedure, public, pass(self) :: performance
      procedure, public, pass(self) :: error

   end type logger_type

   type(logger_type) :: global_logger

contains

   pure subroutine configuration(self, level)
      class(logger_type), intent(in) :: self
      integer(default_int), intent(out), optional :: level
      if (present(level)) level = self%log_level
   end subroutine configuration

   pure subroutine configure(self, level)
      class(logger_type), intent(inout) :: self
      integer(default_int), intent(in), optional :: level
      if (present(level)) self%log_level = level
   end subroutine configure

   subroutine debug(self, message, module, procedure)
      class(logger_type), intent(in) :: self
      character(*), intent(in) :: message
      character(*), intent(in), optional :: module, procedure

      if (self%log_level >= debug_level) then
         if (present(module) .and. present(procedure)) then
            write (*, *) 'DEBUG: ', module, procedure, ': ', message
         else if (present(module)) then
            write (*, *) 'DEBUG: ', module, ': ', message
         else
            write (*, *) 'DEBUG: ', message
         end if
      end if
   end subroutine debug

   subroutine verbose(self, message, module, procedure)
      class(logger_type), intent(in) :: self
      character(*), intent(in) :: message
      character(*), intent(in), optional :: module, procedure

      if (self%log_level >= verbose_level) then
         if (present(module) .and. present(procedure)) then
            write (*, *) 'VERBOSE: ', module, procedure, ': ', message
         else if (present(module)) then
            write (*, *) 'VERBOSE: ', module, ': ', message
         else
            write (*, *) 'VERBOSE: ', message
         end if
      end if
   end subroutine verbose

   subroutine info(self, message, module, procedure)
      class(logger_type), intent(in) :: self
      character(*), intent(in) :: message
      character(*), intent(in), optional :: module, procedure

      if (self%log_level >= info_level) then
         if (present(module) .and. present(procedure)) then
            write (*, *) 'INFO: ', module, procedure, ': ', message
         else if (present(module)) then
            write (*, *) 'INFO: ', module, ': ', message
         else
            write (*, *) 'INFO: ', message
         end if
      end if
   end subroutine info

   subroutine warning(self, message, module, procedure)
      class(logger_type), intent(in) :: self
      character(*), intent(in) :: message
      character(*), intent(in), optional :: module, procedure

      if (self%log_level >= warning_level) then
         if (present(module) .and. present(procedure)) then
            write (*, *) 'WARNING: ', module, procedure, ': ', message
         else if (present(module)) then
            write (*, *) 'WARNING: ', module, ': ', message
         else
            write (*, *) 'WARNING: ', message
         end if
      end if
   end subroutine warning

   subroutine performance(self, message, module, procedure)
      class(logger_type), intent(in) :: self
      character(*), intent(in) :: message
      character(*), intent(in), optional :: module, procedure

      if (self%log_level >= performance_level) then
         if (present(module) .and. present(procedure)) then
            write (*, *) 'PERFORMANCE: ', module, procedure, ': ', message
         else if (present(module)) then
            write (*, *) 'PERFORMANCE: ', module, ': ', message
         else
            write (*, *) 'PERFORMANCE: ', message
         end if
      end if
   end subroutine performance

   subroutine error(self, message, module, procedure)
      class(logger_type), intent(in) :: self
      character(*), intent(in) :: message
      character(*), intent(in), optional :: module, procedure

      if (self%log_level >= error_level) then
         if (present(module) .and. present(procedure)) then
            write (*, *) 'ERROR: ', module, procedure, ': ', message
         else if (present(module)) then
            write (*, *) 'ERROR: ', module, ': ', message
         else
            write (*, *) 'ERROR: ', message
         end if
      end if
   end subroutine error

end module pic_logger
