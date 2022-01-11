program first_test_case
use matrix
integer :: rows = 1000, cols = 1000
real, dimension(1000,1000) :: first, identity
real, allocatable, dimension(:,:) :: output
real, allocatable, dimension(:,:,:) :: outputs
integer :: i,j, err
real :: start, end, bstart, bend

allocate(output(rows,cols))

!real, dimension(1,5) :: tvec
!real, dimension(5,1) :: svec
!do i = 1, 5
!tvec(1,i) = i
!end do 

call cpu_time(bstart)
do i = 1, rows
do j = 1, cols 

first(i,j) = i + j
identity(i,j) = i + j

end do
end do
call cpu_time(bend)

print*, "construct time: ", bend - start

call cpu_time(start)

do i = 1, 10
call dot_product(first,identity, output)
end do

call cpu_time(end)

deallocate(output)

Print*, "time it took: ", end - start

! Print*, "original matrix: "
! call print_matrix(first)

! print*, "matrix squared: "
! call print_matrix(output)

end program
