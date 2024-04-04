!---------------------------------------------------------------------------------------------------
!> @file   mod_cmd_line.f90
!> @author Felippe M. Colombari
!> @brief  Get command line arguments         
!> @date - Mar, 2024                                                           
!> - independent module created                                                
!---------------------------------------------------------------------------------------------------

module mod_cmd_line
  use iso_fortran_env    , only : stdout => output_unit
  use mod_error_handling
  use mod_constants      , only : DP, int_alphabet, float_alphabet, char_alphabet, dashline

  implicit none

  private
  public Parse_Arguments, filename, arg

  character( len = 64 )                            :: filename = char(0)
  character( len = 20 ), allocatable, dimension(:) :: arg         

  integer                                          :: ierr
  type(error)                                      :: err

contains
  
  !---------------------------------------------------------------------------
  !> @brief Parses the command line arguments
  !> @author Felippe M. Colombari
  !---------------------------------------------------------------------------	
  subroutine Parse_arguments

    implicit none

    integer                :: i
    integer                :: ios            = 0
    integer                :: narg           = 0
    integer                :: nochar         = 0
    character( len = 256 ) :: cmd_line       = char(0)
      
    narg = command_argument_count()
    call get_command(cmd_line)

    write(stdout,'(/,T3, A)') dashline
    write(stdout,'(/,T5, A, A)') "COMMAND LINE READ: ", trim(cmd_line)
    write(stdout,'(/,T3, A)') dashline

    if ( narg > 0 ) then
    
      ! to avoid allocation errors if one forget the argument "rad"

      allocate( arg(narg+1), stat=ierr )
      if(ierr/=0) call err % error('e',message="abnormal memory allocation")

      arg = char(0)

      do i = 1, narg

        call get_command_argument( i, arg(i) )

        if ( arg(i)(1:2) == '--' ) then

          SELECT CASE( arg(i) )

            CASE( '--help' )

              call Display_help

            CASE( '--license' )

              call Display_license

            CASE( '--version')  

              call display_version

            CASE( '--input' )

              call Get_command_argument( i+1, arg(i+1) )

              nochar = verify( trim( arg(i+1) ), char_alphabet )

              if ( nochar > 0 ) then

                call err % error('e',message="while reading command line.")
                call err % error('e',check="molecule coordinate file.") 
                call err % error('e',tip="Should be a valid .xyz file.")
               
                stop
                
              else

                read(arg(i+1),*,iostat=ios) filename

                if ( ios > 0 ) then

                  call err % error('e',message="while reading command line.")
                  call err % error('e',check="molecule coordinate file.") 
                  call err % error('e',tip="Should be a valid .xyz file.")
               
                  stop

                endif

              endif

            CASE DEFAULT

              call err % error('e',message="while reading command line.")
              call err % error('e',check="invalid command line flag '"//trim(adjustl(arg(i)))//"'.")
               
              stop

          end SELECT

        else 

          if ( arg(1)(1:2) /= '--' ) then

            call err % error('e',message="while reading command line.")
            call err % error('e',check="'"//trim(adjustl(arg(i+1)))//"' argument of '"//trim(adjustl(arg(i)))//"' flag.")

            stop

          endif

          if ( ( i > 1 ) .and. ( arg(i-1)(1:2) ) /= '--' ) then

            call err % error('e',message="while reading command line.")
            call err % error('e',check="'"//trim(adjustl(arg(i+1)))//"' argument of '"//trim(adjustl(arg(i)))//"' flag.")

            stop

          endif

        endif

      enddo

      if ( allocated(arg) ) deallocate(arg)

    else if ( narg == 0 ) then 

      call err % error('e',message="while reading command line.")
      call err % error('e',tip="Command line arguments are missing.")

      stop

    endif

    if ( filename == char(0) ) then

      call err % error('e',message="while reading command line.")
      call err % error('e',check="molecule coordinate file.") 
      call err % error('e',tip="Should be a valid .xyz file.")

      stop

    endif

    if ( allocated(arg) ) deallocate(arg)

  end subroutine Parse_arguments

  !---------------------------------------------------------------------------
  !> @brief Displays command line options
  !> @author Felippe M. Colombari
  !---------------------------------------------------------------------------	
  subroutine Display_help
            
    implicit none

    write(stdout,'(/,T20, A)')'Usage:  opd --input [FILE] '
    write(stdout,'(/,T3, A)') dashline
    write(stdout,'(/,T25, A)')'[FILE]   is a .xyz coordinate file.'
    write(stdout,'(/,T3, A)') dashline
    
    if ( allocated(arg) ) deallocate(arg)

    call err % termination(0,'f')

  end subroutine Display_help

  !---------------------------------------------------------------------------
  !> @brief Displays the license
  !> @author Felippe M. Colombari
  !---------------------------------------------------------------------------	
  subroutine Display_license

    implicit none

    write(stdout,'(T36, A)')'Copyright 2024 Felippe M. Colombari'
    write(stdout,'(/,T33, A)')'License GPLv3+: GNU GPL version 3 or later' 
    write(stdout,'(/,T6, A)')' This program is free software: you can redistribute it and/or modify it &
                      &under the terms of the'
    write(stdout,'(T5, A)')'GNU General Public License as published by the Free Software Foundation, &
                      &either version 3 of the'
    write(stdout,'(T30, A)')'License, or (at your option) any later version.'
    write(stdout,'(/,T5, A)')'This program is distributed in the hope that it will be useful, but &
                      &WITHOUT ANY WARRANTY; without'
    write(stdout,'(T12, A)')'even the implied warranty of MERCHANTABILITY or FITNESS FOR A &
                      &PARTICULAR PURPOSE.'
    write(stdout,'(T26, A)')'See the GNU General Public License for more details.'
    write(stdout,'(/,T4, A)')'You should have received a copy of the GNU General Public License along & 
                      &with this program. If not,'
    write(stdout,'(T34, A)')'see <https://www.gnu.org/licenses/>.'
    write(stdout,'(/,T36, A)')'E-mail: colombarifm@hotmail.com'
    write(stdout,'(/,T3, A,/)') dashline

    if ( allocated(arg) ) deallocate(arg)

    call err % termination(0,'f')

  end subroutine Display_license

  !---------------------------------------------------------------------------
  !> @brief Displays the version
  !> @author Felippe M. Colombari
  !---------------------------------------------------------------------------	
  subroutine Display_version()

    implicit none
    
    character(len=:), allocatable :: version
    
    ! TODO link to version control 

    version = '1.0.0-beta'

    write(stdout,'(/,T3, A,/)') dashline
    write(stdout,'(/, T5, "OPD_chirality_index ", A)') version
    write(stdout,'(/,T3, A,/)') dashline
     
    if ( allocated(arg) ) deallocate(arg)
    
    call err % termination(0,'f')

  end subroutine Display_version

end module mod_cmd_line
