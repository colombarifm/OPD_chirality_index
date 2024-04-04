!---------------------------------------------------------------------------------------------------
! OPD_chirality_index: a program to calculate the Osipov-Pickup-Dunmur chirality index                                                  
!---------------------------------------------------------------------------------------------------
!
!   This file was written by Felippe M. Colombari 
!
!---------------------------------------------------------------------------------------------------
!> @file   osipov_omp.f90
!> @author Felippe M. Colombari
!> @brief  This file contains modules and functions to perform all calculations
!> @note   The scaled OPD chirality index is calculated according to http://dx.doi.org/10.1063/1.1476321
!> @date - May, 2018                                                           
!> - first version was written                                                
!> @date - Nov 2022
!> - OpenMP parallelization was implemented
!---------------------------------------------------------------------------------------------------


module mod_products

  use mod_constants, only : DP
  implicit none

  contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    function cross( a , b )

      real(kind=dp), dimension(3)             :: cross
      real(kind=dp), dimension(3), intent(IN) :: a, b

      cross(1) = a(2) * b(3) - a(3) * b(2)
      cross(2) = a(3) * b(1) - a(1) * b(3)
      cross(3) = a(1) * b(2) - a(2) * b(1)

    end function cross

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    function dot( a , b )

      real(kind=dp)                           :: dot
      real(kind=dp), dimension(3), intent(IN) :: a, b

      dot = a(1) * b(1) + a(2) * b(2) + a(3) * b(3)

    end function dot

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module mod_products

