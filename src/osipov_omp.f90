
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
!> @file   osipov_omp.f90
!> @author Felippe M. Colombari
!> @brief  This file contains modules and functions to perform all calculations
!> @note   The scaled OPD chirality index is calculated according to http://dx.doi.org/10.1063/1.1476321
!> @note   This is a very simple implementation! 
!> @date - May, 2018                                                           
!> - first version was written                                                
!> @date - Nov 2022
!> - OpenMP parallelization was implemented
!> @date - Mar 2024
!> - Modularization and documentation
!> - Use fortran intrinsics norm2 and dot_product
!---------------------------------------------------------------------------------------------------

program osipov_omp

  use iso_fortran_env       , only : stdout => output_unit
  use mod_error_handling
  use mod_constants         , only : dashline, DP
  use mod_cmd_line          , only : Parse_arguments, filename
  use mod_read_molecule     , only : mol 
  use mod_products
  use omp_lib


  implicit none

  real(kind=dp)                             :: denominator, G0, G0_scaled
  real(kind=dp)                             :: rij, rjk, rkl, ril
  real(kind=dp)                             :: dot_01, dot_02, dot_03
  real(kind=dp), dimension(3)               :: vij, vjk, vkl, vil, vcross

  integer                                   :: i, j, k, l
  type(error)                               :: err

  call Parse_arguments
  call mol % Read_molecule( filename )

  vij       = 0.0_dp
  vjk       = 0.0_dp
  vkl       = 0.0_dp
  vil       = 0.0_dp
  G0        = 0.0_dp
  G0_scaled = 0.0_dp

  !$omp parallel do private(i,j,k,l,vij,rij,vjk,rjk,vkl,rkl,vil,ril,dot_01,dot_02,dot_03,vcross,denominator),&
  !$omp & reduction(+:G0)
  do i = 1, mol % num_atoms
    do j = 1, mol % num_atoms
      if ( j == i ) then
        cycle
      else
      vij = mol % atoms(i) % xyz(:) - mol % atoms(j) % xyz(:)
      rij = norm2( vij(:) )
      do k = 1, mol % num_atoms
        if ( ( k == j ) .or. ( k == i ) ) then
          cycle
        else
        vjk = mol % atoms(j) % xyz(:) - mol % atoms(k) % xyz(:)
        rjk = norm2( vjk(:) )
        do l = 1, mol % num_atoms
          if ( ( l == k ) .or. ( l == j ) .or. ( l == i ) ) then
            cycle
          else
          vkl = mol % atoms(k) % xyz(:) - mol % atoms(l) % xyz(:)
          rkl = norm2( vkl(:) )
          vil = mol % atoms(i) % xyz(:) - mol % atoms(l) % xyz(:)
          ril = norm2( vil(:) )

          !!!

          vcross = cross_product( vij , vkl )
          dot_01 = dot_product( vcross , vil )
          dot_02 = dot_product( vij , vjk )
          dot_03 = dot_product( vjk , vkl )

          !!!

          denominator = ril * ( rij * rjk * rkl ) ** 2

          !!!

          G0 = G0 + ( dot_01 * dot_02 * dot_03 ) / denominator

        endif

        enddo !l

      endif

      enddo !k

    endif

    enddo !j

  enddo !i
  !$omp end parallel do

  !from paper: G0s = [ 4! / ( 3 * N^4 ) ] * G0 
  G0_scaled = 24 * G0 / ( 3 * mol % num_atoms ** 4 ) 

  write(stdout,'(/,T3, A)') dashline
  write(stdout,'(/,T5, "Scaled Osipov index; G0s = ", f20.8)') G0_scaled
  write(stdout,'(/,T3, A)') dashline

  deallocate(mol % atoms)
  
  call err % termination(0,'f')

end program osipov_omp
