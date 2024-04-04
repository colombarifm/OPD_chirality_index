!---------------------------------------------------------------------------------------------------
!> @file   mod_read_molecule.f90
!> @author Felippe M. Colombari
!> @brief  This module reads coordinates for a given .xyz file and sets the vdW radii for each atom
!> @date - Mar, 2024                                                           
!> - independent module created                                                
!---------------------------------------------------------------------------------------------------

module mod_read_molecule
  use iso_fortran_env    , only : stdout => output_unit
  use mod_constants

  implicit none
  
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  type atom 
    real( kind = dp )                       :: xyz(3)
    character( len = 2 )                    :: label
  end type atom

  type molecule
    type( atom ), allocatable,dimension(:) :: atoms
    integer                                :: num_atoms
  contains
    procedure, pass                        :: Read_molecule
  end type molecule

  type( molecule )                         :: mol

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  type atom_entry
    character( len = 2 )                   :: symbol 
  end type atom_entry

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

contains

  !---------------------------------------------------------------------------
  !> @brief This routine reads coordinates for xyz_files.
  !> @author Felippe M. Colombari
  !---------------------------------------------------------------------------	
  subroutine Read_molecule ( this, molecule_filename )
    use mod_inquire, only: Inquire_file
    use mod_error_handling

    implicit none

    class( molecule ), intent(inout) :: this
    character( len = * ), intent(in) :: molecule_filename
    integer                          :: i
    integer                          :: ios         = 0
    integer                          :: file_unit   = 10        
    character( len = 15 )            :: file_format = "formatted"
    character( len = 15 )            :: file_access = "sequential"

    integer                          :: ierr
    type(error)                      :: err

    call Inquire_file( file_unit, molecule_filename, file_format, file_access )

    read(file_unit,*,iostat=ios) this % num_atoms
    read(file_unit,*)

    if ( allocated ( this % atoms ) ) deallocate ( this % atoms )
    allocate( this % atoms( this % num_atoms ), stat=ierr )
    if(ierr/=0) call err % error('e',message="abnormal memory allocation")

    do i = 1, this % num_atoms

      read(file_unit,*,iostat=ios) this % atoms(i) % label, this % atoms(i) % xyz(:)

    enddo

    close(file_unit)

    write(stdout,'(/,T5, A, i0, A)') "Structure has ", this % num_atoms, " atoms"

    return
  end subroutine Read_molecule

end module mod_read_molecule
