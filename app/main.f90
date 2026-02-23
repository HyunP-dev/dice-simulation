program main
  use simulation, only: X
  implicit none

  integer :: a
  integer :: index
  
  integer :: t = 100
  integer :: samples(100)
  integer :: avgs(6)
  integer :: diffs(6)

  integer :: sample
  real :: p
  
  do a = 1, 6
    !$omp parallel do
    do index = 1, t
      samples(index) = X(a)
    end do
    !$omp end parallel do
    avgs(a) = sum(samples) / t

    diffs(a) = abs(avgs(a) - 4800)  ! E(X) = 4800
    write (*, '(A, I1, A, I4)') "a=", a, ", E(X)=", avgs(a)
  end do

  a = minloc(diffs, 1)
  write (*, '(A, I1)') "Found: a=", a

  p = 0.0
  !$omp parallel do reduction(+:p) private(sample) schedule(dynamic)
  do index = 1, 100000
    sample = X(a)
    if (sample <= 4800 + 30 * a) then
      p = p + 1.0
    end if
  end do
  !$omp end parallel do
  p = p / 100000
  write (*, '(A, F5.3)') "  P(X .le. 4800 + 30a)=", p

end program main
