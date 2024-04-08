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
!> @file   mod_products.f90
!> @author Felippe M. Colombari
!> @brief  This file contains modules and functions to perform all calculations
!> @note   The scaled OPD chirality index is calculated according to http://dx.doi.org/10.1063/1.1476321
!> @date - May, 2018                                                           
!> - first version was written                                                
!> @date - Mar, 2024
!> - dot_product function dropped; using fortran intrinsic instead
!---------------------------------------------------------------------------------------------------

module mod_products

  use mod_constants, only : DP
  implicit none

  contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    function cross_product( vector_a , vector_b ) result( cross )

      real(kind=dp), dimension(3)             :: cross
      real(kind=dp), dimension(3), intent(IN) :: vector_a, vector_b

      cross(1) = vector_a(2) * vector_b(3) - vector_a(3) * vector_b(2)
      cross(2) = vector_a(3) * vector_b(1) - vector_a(1) * vector_b(3)
      cross(3) = vector_a(1) * vector_b(2) - vector_a(2) * vector_b(1)

    end function cross_product

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module mod_products
