!> @author Haart
!>
!> a facade module for integration a complex function by custom built path
!> with explicit description of path parts
!> for not standart complex integration
!>
!> with all premade tools avaible
module integration
use integrator, only: &
  integrator_obj, integrator_builder_obj, &
  integrator_builder_constructor



use integration_template, only: integration_template_obj

use integration_template_recalculation, only: &
  integration_template_recalculation_obj, integration_template_recalculation_constructor



use integration_path_part, only: &
  integration_path_part_obj, &
  integrated_function_type, projection_function_type, normilized_delta_type

use integration_path_part_parallel_Im, only: &
  integration_path_part_parallel_Im_obj, integration_path_part_parallel_Im_constructor
use integration_path_part_parallel_Re, only: &
  integration_path_part_parallel_Re_obj, integration_path_part_parallel_Re_constructor



use integration_element, only: integration_element_obj

use integration_element_gauss3, only: &
  integration_element_gauss3_obj, integration_element_gauss3_constructor
use integration_element_gauss4, only: &
  integration_element_gauss4_obj, integration_element_gauss4_constructor
use integration_element_gauss5, only: &
  integration_element_gauss5_obj, integration_element_gauss5_constructor
use integration_element_gauss6, only: &
  integration_element_gauss6_obj, integration_element_gauss6_constructor
use integration_element_gauss7, only: &
  integration_element_gauss7_obj, integration_element_gauss7_constructor
use integration_element_gauss8, only: &
  integration_element_gauss8_obj, integration_element_gauss8_constructor
implicit none (type, external)
private
  character(len = *), parameter :: module_name = "integral_generator___integration"

  public :: integrator_obj, integrator_builder_obj, &
    integrator_builder_constructor



  public :: integration_template_recalculation_constructor



  public :: integrated_function_type

  public :: integration_path_part_parallel_Im_constructor
  public :: integration_path_part_parallel_Re_constructor



  public :: integration_element_gauss3_constructor
  public :: integration_element_gauss4_constructor
  public :: integration_element_gauss5_constructor
  public :: integration_element_gauss6_constructor
  public :: integration_element_gauss7_constructor
  public :: integration_element_gauss8_constructor
end module integration
