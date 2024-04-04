!---------------------------------------------------------------------------------------------------
!> @file   mod_error_handling.f90
!> @author Asdrubal Lozada-Blanco
!> @brief  This module contains procedures to error handling
!> This is a Standard Fortran 2008 compliance code
!> @date - Nov 2019
!> - module created 
!> @date - Mar 2024
!> - module incorporated into code
!---------------------------------------------------------------------------------------------------

module mod_error_handling
  use iso_fortran_env , only : output_unit, error_unit

  implicit none
  private

  type, public :: error
    integer                       :: code
    character                     :: type
    character(len=:), allocatable :: message, check, tip
  contains
    procedure                     :: error       => raise_error
    procedure                     :: termination => normal_termination
  end type error

  public :: Normal_termination, Raise_error

contains

  subroutine Raise_error(self, type, code, message, check, tip)
    class(error), intent(inout)                 :: self
    character, intent(in)                       :: type        
    integer, optional, intent(in)               :: code        
    character( len = * ), optional, intent(in)  :: message, check, tip

    self % type = type

    if (present( message )) self % message = message
    if (present( check ))   self % check   = check
    if (present( tip ))     self % tip     = tip
    if (present( code ))    self % code    = code

    select case(type)
      case('w')
        if (present( message )) write(output_unit,'(/,T5,"Warning: ",a)') self % message
        if (present( check ))   write(output_unit,'(/,T5,"Warning: ",a)') self % check
        if (present( tip ))     write(output_unit,'(/,T5,"Warning: ",a)') self % tip
        if (present( code ))    write(output_unit,'("Code: ",i0)')        self % code
      case('e')
        if (present( message )) write(output_unit,'(/,T5,"Error: abnormal condition ",a)') self % message
        if (present( check ))   write(output_unit,'(/,T5,"Please check ",a)')              self % check
        if (present( tip ))     write(output_unit,'(/,T5,"TIP: ",a)')                      self % tip
        if (present( code ))    write(output_unit,'("Code: ",i0)')                         self % code
    end select

    if ( allocated( self % message ) ) deallocate( self % message )
    if ( allocated( self % check ) ) deallocate( self % check )
    if ( allocated( self % tip ) ) deallocate( self % tip )

  end subroutine Raise_error   

  subroutine Normal_termination(self,iostat,status)
    class(error), intent(INOUT)         :: self
    integer, intent(IN)                 :: iostat
    character( len = * ), intent(IN)    :: status
    
    if(iostat == 0) then
      select case(status)
        case('f')
          write(output_unit,'(T5,"Sucessfull termination.")')
          stop
        case('i')
          write(output_unit,'(T5,"Normal termination.")')        
      end select
    end if

  end subroutine Normal_termination

end module mod_error_handling
