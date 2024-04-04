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
!> @date - Mar 2024
!> - Modularization and documentation
!---------------------------------------------------------------------------------------------------

program osipov

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
  real(kind=dp)                             :: dot_one, dot_two, dot_three
  real(kind=dp), dimension(3)               :: vij, vjk, vkl, vil, vcross

  integer                                   :: i, j, k, l
  integer                                   :: ierr
  type(error)                               :: err

  call Parse_arguments
  call mol % Read_molecule( filename )

  vij       = 0.0_dp
  vjk       = 0.0_dp
  vkl       = 0.0_dp
  vil       = 0.0_dp
  G0        = 0.0_dp
  G0_scaled = 0.0_dp

  !$omp parallel do private(i,j,k,l,vij,rij,vjk,rjk,vkl,rkl,vil,ril,dot_one,dot_two,dot_three,vcross,denominator),&
  !$omp & reduction(+:G0)
  do i = 1, mol % num_atoms

    do j = 1, mol % num_atoms

      if ( j == i ) then

        cycle

      else

      vij = mol % atoms(i) % xyz(:) - mol % atoms(j) % xyz(:)

      rij = dsqrt( sum( vij(:) * vij(:) ) )

      do k = 1, mol % num_atoms

        if ( ( k == j ) .or. ( k == i ) ) then

          cycle

        else

        vjk = mol % atoms(j) % xyz(:) - mol % atoms(k) % xyz(:)

        rjk = dsqrt( sum( vjk(:) * vjk(:) ) )

        do l = 1, mol % num_atoms

          if ( ( l == k ) .or. ( l == j ) .or. ( l == i ) ) then

            cycle

          else

          vkl = mol % atoms(k) % xyz(:) - mol % atoms(l) % xyz(:)

          rkl = dsqrt( sum( vkl(:) * vkl(:) ) )
          
          vil = mol % atoms(i) % xyz(:) - mol % atoms(l) % xyz(:)

          ril = dsqrt( sum( vil(:) * vil(:)) )

          !!!

          vcross = cross( vij , vkl )

          dot_one = dot( vcross , vil )

          dot_two = dot( vij , vjk )

          dot_three = dot( vjk , vkl )

          !!!

          ! from paper
          denominator = 3 * ril * ( rij * rjk * rkl ) ** 2

          !!!

          G0 = G0 + ( dot_one * dot_two * dot_three ) / denominator

        endif

        enddo !l

      endif

      enddo !k

    endif

    enddo !j

  enddo !i
  !$omp end parallel do

  !from paper
  G0_scaled = 24 * G0 / ( mol % num_atoms ** 4 ) 

  write(stdout,'(/,T3, A)') dashline
  write(stdout,'(/,T5, "Osipov index;        G0  = ", f20.8)') G0
  write(stdout,'(/,T5, "Scaled Osipov index; G0s = ", f20.8)') G0_scaled
  write(stdout,'(/,T3, A)') dashline

  deallocate(mol % atoms)
  
  call err % termination(0,'f')

end program
