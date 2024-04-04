!---------------------------------------------------------------------------------------------------
!> @file   mod_inquire.f90
!> @author Felippe M. Colombari
!> @brief This module performs inquire checks for all files that will be read
!> @date - Mar, 2024                                                           
!> - independent module created                                                
!---------------------------------------------------------------------------------------------------

module mod_inquire
  use mod_error_handling

  implicit none

contains

  subroutine Inquire_file(file_unit,file_name,file_format,file_access)
      
    implicit none

    logical                          :: lexists
    integer                          :: ios
    integer, intent(IN)              :: file_unit             
    character( len = * ), intent(IN) :: file_format, file_access, file_name
    type(error)                      :: err

    ios = 0

    inquire ( file = trim(file_name), exist = lexists, iostat = ios )
      
    if ( ios == 0 ) then 
        
      if ( lexists ) then 

        open( unit = file_unit, file = trim(file_name), status = 'old', &
              form = trim(file_format), access = trim(file_access) ) 

        rewind(file_unit)

      else

        call err % error('e',message="while reading file: "//trim(file_name))
        call err % error('e',check="if file exists in this directory.")

        stop

      endif

    else

      call err % error('e',message="while reading file: "//trim(file_name))
      
      stop

    endif

    return
  end subroutine Inquire_file

end module mod_inquire
