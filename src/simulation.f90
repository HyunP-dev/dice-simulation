module simulation
  implicit none
  private

  public :: X
contains
  subroutine random_dice(dice)
    integer, intent(out) :: dice
    real :: r
    call random_number(r)
    dice = int(r * 6) + 1
  end subroutine random_dice

  function one_shot(a) result(retval)
    integer, intent(in) :: a
    integer :: retval
    integer :: dice
    real, allocatable :: randoms(:)

    call random_dice(dice)
    if (dice <= a) then
        allocate(randoms(5))
        call random_number(randoms)
        retval = count(randoms < 0.5)
    else
        allocate(randoms(3))
        call random_number(randoms)
        retval = count(randoms < 0.5)
    end if
    
    deallocate(randoms)
  end function one_shot

  function X(a) result(retval)
    integer, intent(in) :: a
    integer :: retval
    integer :: result
    integer :: index

    retval = 0

    !$omp parallel do reduction(+:retval) private(result)
    do index = 1, 19200
      result = one_shot(a)
      if ( result == 3 ) then
        retval = retval + 1
      end if
    end do
    !$omp end parallel do
  end function X
end module simulation
