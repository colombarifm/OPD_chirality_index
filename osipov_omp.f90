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
!> @date - May, 2018                                                           
!> - first version was written                                                
!> @date - Nov 2022
!> - OpenMP parallelization was implemented
!---------------------------------------------------------------------------------------------------


module products

  implicit none

  contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    function cross( a , b )

      integer, parameter                      :: dp = selected_real_kind(15, 307)
      real(kind=dp), dimension(3)             :: cross
      real(kind=dp), dimension(3), intent(IN) :: a, b

      cross(1) = a(2) * b(3) - a(3) * b(2)
      cross(2) = a(3) * b(1) - a(1) * b(3)
      cross(3) = a(1) * b(2) - a(2) * b(1)

    end function cross

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    function dot( a , b )

      integer, parameter                      :: dp = selected_real_kind(15, 307)
      real(kind=dp)                           :: dot
      real(kind=dp), dimension(3), intent(IN) :: a, b

      dot = a(1) * b(1) + a(2) * b(2) + a(3) * b(3)

    end function dot

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module products

program osipov

  USE products
  USE OMP_LIB

  implicit none

  integer, parameter                        :: dp = selected_real_kind(15, 307)

  real(kind=dp)                             :: denominator, G0, G
  real(kind=dp)                             :: rij, rjk, rkl, rik
  real(kind=dp)                             :: dot_one, dot_two, dot_three
  real(kind=dp), dimension(3)               :: vij, vjk, vkl, vik, vcross
  real(kind=dp), allocatable, dimension(:)  :: x, y, z


  character(50)                             :: infile
  character(3), allocatable, dimension(:)   :: atom
  

  integer                                   :: numat
  integer                                   :: i, j, k, l

  call get_command_argument( 1, infile )

  open(unit=10, file=trim(infile), status="old")

  read(10,*) numat
  read(10,*)

  allocate( atom(numat) )
  allocate( x(numat) )
  allocate( y(numat) )
  allocate( z(numat) )

  do i = 1, numat

    read(10,*) atom(i), x(i), y(i), z(i)

  enddo

  close(10)

  vij = 0.0_dp
  vjk = 0.0_dp
  vkl = 0.0_dp
  vik = 0.0_dp
  G0  = 0.0_dp
  G   = 0.0_dp

  !$omp parallel do private(i,j,k,l,vij,rij,vjk,rjk,vkl,rkl,vik,rik,dot_one,dot_two,dot_three,vcross,denominator),&
  !$omp & shared(numat), reduction(+:G)
  do i = 1, numat

    do j = 1, numat

      if ( j == i ) then

        cycle

      else

      vij = [ x(i) - x(j) , y(i) - y(j) , z(i) - z(j) ]

      rij = dsqrt( vij(1)*vij(1) + vij(2)*vij(2) + vij(3)*vij(3) )

      do k = 1, numat

        if ( ( k == j ) .or. ( k == i ) ) then

          cycle

        else

        vjk = [ x(j) - x(k) , y(j) - y(k) , z(j) - z(k) ]

        rjk = dsqrt( vjk(1)*vjk(1) + vjk(2)*vjk(2) + vjk(3)*vjk(3) )

        do l = 1, numat

          if ( ( l == k ) .or. ( l == j ) .or. ( l == i ) ) then

            cycle

          else

          vkl = [ x(k) - x(l) , y(k) - y(l) , z(k) - z(l) ]

          rkl = dsqrt( vkl(1)*vkl(1) + vkl(2)*vkl(2) + vkl(3)*vkl(3) )

          vik = [ x(i) - x(k) , y(i) - y(k) , z(i) - z(k) ]

          rik = dsqrt( vik(1)*vik(1) + vik(2)*vik(2) + vik(3)*vik(3) )

          !!!

          vcross = cross( vij , vkl )

          dot_one = dot( vcross , vik )

          dot_two = dot( vij , vjk )

          dot_three = dot( vjk , vkl )

          !!!

          denominator = 3 * rik * ( rij * rjk * rkl ) ** 2

          !!!

          G = G + ( dot_one * dot_two * dot_three ) / denominator

          !write(*,'(4i3,6f12.6)') i, j, k, l, rij, rjk, rkl, rik, denominator, G0

        endif

        enddo !l

      endif

      enddo !k

    endif

    enddo !j

  enddo !i
  !$omp end parallel do

  deallocate( atom )
  deallocate( x )
  deallocate( y )
  deallocate( z )

  G0 = 24 * G / ( numat ** 4 )

  open( unit = 666, file = "results.log", status = "unknown" )

  write(666,'("------------------------------------------------------")')
  write(666,*)
  write(666,'("Osipov index; G = ", f20.8 )') G
  write(666,'("Reduced Osipov index; G0 = ", f20.8)') G0
  write(666,*)
  write(666,'("------------------------------------------------------")')
 
  close(666)

  end program
