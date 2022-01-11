module matrix

contains
subroutine dot_product(first, second, output)
real, dimension(:,:) :: first, second
real, allocatable, dimension(:,:) :: output
real, dimension(2) :: fdims, sdims
integer :: i,j,k,l
real :: jsum

!allocate(output(fdims(1), sdims(2)))

! should handle inner dims not matching

fdims = shape(first) 
sdims = shape(second)

! for each row and column of the output matrix
do i = 1, fdims(1)
do j = 1, sdims(2)

jsum = 0
do k = 1, fdims(2)
jsum = jsum + (first(i, k) * second(j,k))
end do 
output(i,j) = jsum


end do 
end do
end subroutine dot_product

subroutine print_matrix(matrix)
real, dimension(:,:) :: matrix
integer, dimension(2) :: dims
integer :: i, j

dims = shape(matrix)
do i = 1, dims(1)

Print*, "[", matrix(i, :), "]"

end do

end subroutine print_matrix

subroutine transpose(matrix, output, err)
integer :: err, i, j
real, dimension(:,:) :: matrix, output
integer, dimension(2) :: mdims, odims
mdims = shape(matrix)
odims = shape(output)
if(mdims(1) /= odims(2) .or. mdims(2) /= odims(1)) then
err = -1
Print*, "mismatched dimensions in matrix transpose"

else

do i = 1, mdims(1)
do j = 1, mdims(2)

output(j,i) = matrix(i,j)

end do
end do
err = 0
end if

end subroutine transpose

! determinant using cofactor expansion
!subroutine determinant(matrix, output, err)
!integer, dimension(2) :: dims
!real, dimension(:,:) :: matrix
!integer :: i, j, err, len, l, output
!dims = shape(matrix)
!if(dims(1) /= dims(2)) then
!err = -1
!else
!len = dims(1)
!do l = 1, len



!end do
!err = 0
!end if

!end subroutine

function identmat(size)
integer :: size, i, j
real, dimension(size, size) :: identmat

do i = 1, size
do j = 1, size

if(i .eq. j) then
identmat(i,j) = 1
else
identmat(i,j) = 0
end if

end do
end do
end function identmat

end module matrix
