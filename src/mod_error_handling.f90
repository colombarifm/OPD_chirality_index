!---------------------------------------------------------------------------------------------------
! opd: a program to calculate the scaled Osipov-Pickup-Dunmur chirality index                                                  
!---------------------------------------------------------------------------------------------------
!
!   Free software, licensed under GNU GPL v3
!
!   Copyright (c) 2018 - 2024 Felippe M. Colombari
!
!---------------------------------------------------------------------------------------------------
!
!   This is a free software: you can redistribute it and/or modify it under the terms of the GNU 
!   General Public License as published by the Free Software Foundation, either version 3 of the 
!   License, or (at your option) any later version.
!
!   This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; 
!   without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See 
!   the GNU General Public License for more details.
!
!   You should have received a copy of the GNU General Public License along with this program. If 
!   not, see <https://www.gnu.org/licenses/>.
!
!---------------------------------------------------------------------------------------------------
!> @file   mod_error_handling.f90
!> @author Asdrubal Lozada-Blanco, Felippe M. Colombari
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
    integer                       :: err_code
    character                     :: err_type
    character(len=:), allocatable :: err_message, err_check, err_tip
  contains
    procedure                     :: error       => raise_error
    procedure                     :: termination => normal_termination
  end type error

  public :: Normal_termination, Raise_error

contains

  subroutine Raise_error(self, err_type, err_code, err_message, err_check, err_tip)
    class(error), intent(inout)                 :: self
    character, intent(in)                       :: err_type        
    integer, optional, intent(in)               :: err_code        
    character( len = * ), optional, intent(in)  :: err_message, err_check, err_tip

    self % err_type = err_type

    if (present( err_message )) self % err_message = err_message
    if (present( err_check ))   self % err_check   = err_check
    if (present( err_tip ))     self % err_tip     = err_tip
    if (present( err_code ))    self % err_code    = err_code

    select case(err_type)
      case('w')
        if (present( err_message )) write(output_unit,'(/,T5,"Warning: ",a)') self % err_message
        if (present( err_check ))   write(output_unit,'(/,T5,"Warning: ",a)') self % err_check
        if (present( err_tip ))     write(output_unit,'(/,T5,"Warning: ",a)') self % err_tip
        if (present( err_code ))    write(output_unit,'("Code: ",i0)')        self % err_code
      case('e')
        if (present( err_message )) write(output_unit,'(/,T5,"Error: abnormal condition ",a)') self % err_message
        if (present( err_check ))   write(output_unit,'(/,T5,"Please check ",a)')              self % err_check
        if (present( err_tip ))     write(output_unit,'(/,T5,"TIP: ",a)')                      self % err_tip
        if (present( err_code ))    write(output_unit,'("Code: ",i0)')                         self % err_code
    end select

    if ( allocated( self % err_message ) ) deallocate( self % err_message )
    if ( allocated( self % err_check ) )   deallocate( self % err_check )
    if ( allocated( self % err_tip ) )     deallocate( self % err_tip )

  end subroutine Raise_error   

  subroutine Normal_termination(self, iostat, status)
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
