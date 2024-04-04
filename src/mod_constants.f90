!---------------------------------------------------------------------------------------------------
!> @file   mod_constants.f90
!> @author Felippe M. Colombari
!> @brief  Defines a set constants.
!> @date - Mar, 2024                                                           
!> - independent module created                                                
!---------------------------------------------------------------------------------------------------

module mod_constants

  implicit none

  integer, public, parameter           :: DP = selected_real_kind(15, 307) !  double precision constant for portability
  
  character( len = 11 ), public, parameter   :: INT_ALPHABET   = '1234567890'    !       allowed character for integers
  character( len = 12 ), public, parameter   :: FLOAT_ALPHABET = '.-1234567890'  !         allowed character for floats
  character( len = 66 ), public, parameter   :: CHAR_ALPHABET  = &
    'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ._-1234567890 '   !              allowed character for strings
  character( len = 100 ), public, parameter  :: DASHLINE = repeat('-',100)      !                       just a dashline

end module mod_constants
