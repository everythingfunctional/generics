module semigroup_m
    !! A semigroup is a type with a sensible operation for combining two objects
    !! of that type to produce another object of the same type.
    !! A sensible operation has the associative property (i.e. (a + b) + c == a + (b + c))
    !! Given this property, it also makes sense to combine a list of objects of
    !! that type into a single object, or to repeatedly combine an object with
    !! itself. These operations can be derived in terms of combine.
    !! Examples include integer (i.e. +), and character (i.e. //)
    implicit none
    private
    public :: semigroup, extended_semigroup, derive_extended_semigroup

    requirement semigroup(T, combine)
        type, deferred :: T
        interface
            elemental function combine(x, y) result(combined)
                type(T), intent(in) :: x, y
                type(T) :: combined
            end function
        end interface
    end requirement

    requirement extended_semigroup(T, combine, sconcat, stimes)
        require :: semigroup(T, combine)
        interface
            pure function sconcat(list) result(combined)
                type(T), intent(in) :: list(:) !! Must contain at least one element
                type(T) :: combined
            end function
            elemental function stimes(n, a) result(repeated)
                integer, intent(in) :: n
                type(T), intent(in) :: a
                type(T) :: repeated
            end function
        end interface
    end requirement

    template derive_extended_semigroup(T, combine)
        require :: semigroup(T, combine)

        private
        public :: sconcat, stimes

        interface sconcat
            procedure sconcat_
        end interface
        interface stimes
            procedure stimes_
        end interface
    contains
        pure function sconcat_(list) result(combined)
            type(T), intent(in) :: list(:)
            type(T) :: combined

            integer :: i

            if (size(list) > 0) then
                combined = list(1)
                do i = 2, size(list)
                    combined = combine(combined, list(i))
                end do
            else
                error stop "Attempted to sconcat empty list"
            end if
        end function

        elemental function stimes_(n, a) result(repeated)
            integer, intent(in) :: n
            type(T), intent(in) :: a
            type(T) :: repeated

            integer :: i

            if (n < 1) error stop "n must be > 0"
            repeated = a
            do i = 2, n
                repeated = combine(repeated, a)
            end do
        end function
    end template
end module

module monoid_m
    !! A monoid is a semigroup with a sensible "empty" or "zero" value.
    !! For sensible implementations combine(empty(), a) == combine(a, empty()) == a.
    use semigroup_m, only: semigroup, extended_semigroup, derive_extended_semigroup

    implicit none
    private
    public :: monoid, extended_monoid, derive_extended_monoid

    requirement monoid(T, combine, empty)
        require :: semigroup(T, combine)
        interface
            pure function empty()
                type(T) :: empty
            end function
        end interface
    end requirement

    requirement extended_monoid(T, combine, sconcat, stimes, empty, mconcat)
        require :: extended_semigroup(T, combine, sconcat, stimes)
        require :: monoid(T, combine, empty)
        interface
            pure function mconcat(list) result(combined)
                type(T), intent(in) :: list(:)
                type(T) :: combined
            end function
        end interface
    end requirement

    template derive_extended_monoid(T, combine, empty)
        require :: monoid(T, combine, empty)

        private
        public :: stimes, mconcat

        instantiate derive_extended_semigroup(T, combine), only: stimes

        interface mconcat
            procedure mconcat_
        end interface
    contains
        pure function mconcat_(list) result(combined)
            type(T), intent(in) :: list(:)
            type(T) :: combined

            integer :: i

            if (size(list) > 0) then
                combined = list(1)
                do i = 2, size(list)
                    combined = combine(combined, list(i))
                end do
            else
                combined = empty()
            end if
        end function
    end template
end module

module semiring_m
    !! A semiring is a type that is a monoid with two separate operations and zero values
    !! For example integer is a monoid with + and 0, or * and 1.
    use monoid_m, only: monoid

    implicit none
    private
    public :: semiring

    requirement semiring(T, plus, zero, mult, one)
        require :: monoid(T, plus, zero)
        require :: monoid(T, mult, one)
    end requirement
end module

module unit_ring_m
    !! A unit ring is a type that is a semiring with negation or minus operations.
    use semiring_m, only: semiring

    implicit none
    private
    public :: &
            unit_ring_only_minus, &
            unit_ring_only_negate, &
            unit_ring, &
            derive_unit_ring_from_minus, &
            derive_unit_ring_from_negate

    requirement unit_ring_only_minus(T, plus, zero, mult, one, minus)
        require :: semiring(T, plus, zero, mult, one)
        interface
            elemental function minus(x, y) result(difference)
                type(T), intent(in) :: x, y
                type(T) :: difference
            end function
        end interface
    end requirement

    requirement unit_ring_only_negate(T, plus, zero, mult, one, negate)
        require :: semiring(T, plus, zero, mult, one)
        interface
            elemental function negate(x) result(negated)
                type(T), intent(in) :: x
                type(T) :: negated
            end function
        end interface
    end requirement

    requirement unit_ring(T, plus, zero, mult, one, minus, negate)
        require :: unit_ring_only_minus(T, plus, zero, mult, one, minus)
        require :: unit_ring_only_negate(T, plus, zero, mult, one, negate)
    end requirement

    template derive_unit_ring_from_minus(T, plus, zero, mult, one, minus)
        require :: unit_ring_only_minus(T, plus, zero, mult, one, minus)

        private
        public :: negate

        interface negate
            procedure negate_
        end interface
    contains
        elemental function negate_(x) result(negated)
            type(T), intent(in) :: x
            type(T) :: negated

            negated = minus(zero(), x)
        end function
    end template

    template derive_unit_ring_from_negate(T, plus, zero, mult, one, negate)
        require :: unit_ring_only_negate(T, plus, zero, mult, one, negate)

        private
        public :: minus

        interface minus
            procedure minus_
        end interface
    contains
        elemental function minus_(x, y) result(difference)
            type(T), intent(in) :: x, y
            type(T) :: difference

            difference = plus(x, negate(y))
        end function
    end template
end module

module field_m
    !! field is a unit_ring that also has a division or inverse operation
    use unit_ring_m, only: unit_ring

    implicit none
    private
    public :: &
            field_only_division, &
            field_only_inverse, &
            field, &
            derive_field_from_division, &
            derive_field_from_inverse

    requirement field_only_division(T, plus, zero, mult, one, minus, negate, divide)
        require :: unit_ring(T, plus, zero, mult, one, minus, negate)
        interface
            elemental function divide(x, y) result(quotient)
                type(T), intent(in) :: x, y
                type(T) :: quotient
            end function
        end interface
    end requirement

    requirement field_only_inverse(T, plus, zero, mult, one, minus, negate, invert)
        require :: unit_ring(T, plus, zero, mult, one, minus, negate)
        interface
            elemental function invert(x) result(inverse)
                type(T), intent(in) :: x
                type(T) :: inverse
            end function
        end interface
    end requirement

    requirement field(T, plus, zero, mult, one, minus, negate, divide, invert)
        require :: field_only_division(T, plus, zero, mult, one, minus, negate, divide)
        require :: field_only_inverse(T, plus, zero, mult, one, minus, negate, invert)
    end requirement

    template derive_field_from_division(T, plus, zero, mult, one, minus, negate, divide)
        require :: field_only_division(T, plus, zero, mult, one, minus, negate, divide)

        private
        public :: invert

        interface invert
            procedure invert_
        end interface
    contains
        elemental function invert_(x) result(inverse)
            type(T), intent(in) :: x
            type(T) :: inverse

            inverse = divide(one(), x)
        end function
    end template

    template derive_field_from_inverse(T, plus, zero, mult, one, minus, negate, invert)
        require :: field_only_inverse(T, plus, zero, mult, one, minus, negate, invert)

        private
        public :: divide

        interface divide
            procedure divide_
        end interface
    contains
        elemental function divide_(x, y) result(quotient)
            type(T), intent(in) :: x, y
            type(T) :: quotient

            quotient = mult(x, invert(y))
        end function
    end template
end module

module matrix_m
    use monoid_m, only: derive_monoid
    use semiring_m, only: semiring
    use unit_ring_m, only: unit_ring_only_minus, derive_unit_ring_from_minus

    implicit none
    private
    public :: &
            use_real_matrix, &
            use_complex_matrix, &
            use_integer_matrix, &
            use_tropical_semiring_matrix, &
            use_block_matrix

    template matrix_tmpl(T, plus_t, zero_t, times_t, one_t)
        require :: semiring(T, plus_t, zero_t, times_t, one_t)

        private
        public :: &
                matrix, &
                operator(+), &
                operator(*), &
                zero, &
                one

        type :: matrix
            type(T) :: elements(5, 5)
        end type

        interface operator(+)
            procedure plus_matrix
        end interface
        interface operator(*)
            procedure times_matrix
        end interface
    contains
        elemental function plus_matrix(x, y) result(combined)
            type(matrix), intent(in) :: x, y
            type(matrix) :: combined

            do j = 1, size(combined%elements, dim=2)
                do i = 1, size(combined%elements, dim=1)
                    combined%elements = plus_t(x%elements(i,j), y%elements(i, j))
                end do
            end do
        end function

        pure function zero()
            type(matrix) :: zero

            zero%elements = zero_t()
        end function

        elemental function times_matrix(x, y) result(combined)
            type(matrix), intent(in) :: x, y
            type(matrix) :: combined

            instantiate derive_extended_monoid(T, plus_t, zero_t), only: sum => mconcat
            integer :: i, j

            do j = 1, size(combined%elements, dim=2)
                do i = 1, size(combined%elements, dim=1)
                    combined%elements(i, j) = sum(times(x%elements(i,:), y%elements(:,j)))
                end do
            end do
        end function

        pure function one()
            type(matrix) :: one

            integer :: i

            one%elements = zero_t()
            do i = 1, size(one%elements, dim=1)
                one%elements(i, i) = one_t()
            end do
        end function
    end template

    template matrix_with_subtraction_tmpl(T, plus_t, zero_t, times_t, one_t, minus_t)
        require :: unit_ring_only_minus(T, plus_t, zero_t, times_t, one_t, minus_t)
        instantiate matrix_tmpl(T, plus_t, zero_t, times_t, one_t), only: &
                matrix, &
                operator(+), &
                operator(*), &
                zero, &
                one

        private
        public :: &
                matrix, &
                operator(+), &
                operator(*), &
                zero, &
                one, &
                operator(-)

        interface operator(-)
            procedure minus_matrix
        end interface
    contains
        elemental function minus_matrix(x, y) result(difference)
            type(matrix), intent(in) :: x, y
            type(matrix) :: difference

            difference%elements = minus_t(x%elements, y%elements)
        end function
    end template

    template matrix_with_division_tmpl(T, plus_t, zero_t, times_t, one_t, minus_t, div_t)
        instantiate derive_unit_ring_from_minus(T, plus_t, zero_t, times_t, one_t, minus_t), only: negate
        require :: field_only_division(T, plus_t, zero_t, times_t, one_t, minus_t, negate, div_t)
        instantiate matrix_with_subtraction_tmpl(T, plus_t, zero_t, times_t, one_t, minus_t), only: &
                matrix, &
                operator(+), &
                operator(*), &
                zero, &
                one, &
                operator(-)

        private
        public :: &
                matrix, &
                operator(+), &
                operator(*), &
                zero, &
                one, &
                operator(-), &
                operator(/)

        interface operator(/)
            procedure div_matrix
        end interface
    contains
        elemental function div_matrix(x, y) result(quotient)
            type(matrix), intent(in) :: x, y
            type(matrix) :: quotient

            quotient = back_substitute(row_eschelon(x), y)
        end function

        pure function row_eschelon(x) result(reduced)
            type(matrix), intent(in) :: x
            type(matrix) :: reduced

            integer :: i, ii, j
            type(T) :: r

            reduced = x

            do i = 1, n
                ! Assume pivot m(i,i) is not zero
                do ii = i+1, n
                    r = div_t(reduced%elements(i,i), reduced%elements(ii,i))
                    reduced%elements(ii, i) = zero_t()
                    do j = i+1, n
                        reduced%elements(ii, j) = minus_t(reduced%elements(ii, j), times_t(reduced%elements(i, j), r))
                    end do
                end do
            end do
        end function

        pure function back_substitute(x, y) result(solved)
            type(matrix), intent(in) :: x, y
            type(matrix) :: solved

            integer :: i, j
            type(T) :: tmp(n)

            solved = y
            do i = n, 1, -1
                tmp = zero_t
                do j = i+1, n
                    tmp = plus(tmp, times(x%elements(i,j), solved%elements(:,j)))
                end do
                solved%elements(:,i) = div_t(minus(solved%elements(:, i), tmp), x%elements(i,i))
            end do
        end function
    end template
contains
    pure function real_zero()
        real :: real_zero

        real_zero = 0.
    end function

    pure function real_one()
        real :: real_one

        real_one = 1.
    end function

    pure function complex_zero()
        complex :: complex_zero

        complex_zero = 0.
    end function

    pure function complex_one()
        complex :: complex_one

        complex_one = 1.
    end function

    pure function integer_zero()
        integer :: integer_zero

        integer_zero = 0
    end function

    pure function integer_one()
        integer :: integer_one

        integer_one = 1
    end function

    pure function real_inf()
        use ieee_arithmetic, only: ieee_value, ieee_positive_inf

        real :: real_inf

        real_inf = ieee_value(real_inf, ieee_positive_inf)
    end function

    subroutine use_real_matrix
        instantiate matrix_with_division_tmpl( &
                real, operator(+), real_zero, operator(*), real_one, operator(-), operator(/)), only: &
                matrix, operator(+), zero, operator(*), one, operator(-), operator(/)

        type(matrix) :: m
        integer :: i, j

        do j = 1, size(m%elements, dim=2)
            do i = 1, size(m%elements, dim=1)
                m%elements(i, j) = (j-1) * size(m%elements, dim=1) + i
            end do
        end do
    end subroutine

    subroutine use_complex_matrix
        instantiate matrix_with_division_tmpl( &
                complex, operator(+), complex_zero, operator(*), complex_one, operator(-), operator(/)), only: &
                matrix, operator(+), zero, operator(*), one, operator(-), operator(/)

        type(matrix) :: m
        integer :: i, j

        do j = 1, size(m%elements, dim=2)
            do i = 1, size(m%elements, dim=1)
                m%elements(i, j)%re = j
                m%elements(i, j)%im = i
            end do
        end do
    end subroutine

    subroutine use_integer_matrix
        ! Note, gaussian_solver wouldn't work correctly for integer matrix
        instantiate matrix_with_subtraction_tmpl( &
                integer, operator(+), integer_zero, operator(*), integer_one, operator(-)), only: &
                matrix, operator(+), zero, operator(*), one, operator(-)

        type(matrix) :: m
        integer :: i, j

        do j = 1, size(m%elements, dim=2)
            do i = 1, size(m%elements, dim=1)
                m%elements(i, j) = (j-1) * size(m%elements, dim=1) + i
            end do
        end do
    end subroutine

    subroutine use_tropical_semiring_matrix
        ! This is a useful representation for graphs for certain algorithms.
        instantiate matrix_tmpl(real, min, real_inf, operator(+), real_zero), only: &
                matrix, operator(+), zero, operator(*)

        type(matrix) :: m
        integer :: i, j

        do j = 1, size(m%elements, dim=2)
            do i = 1, size(m%elements, dim=1)
                m%elements(i, j) = (j-1) * size(m%elements, dim=1) + i
            end do
        end do
    end subroutine

    subroutine use_block_matrix
        instantiate matrix_with_division_tmpl( &
                real, operator(+), real_zero, operator(*), real_one, operator(-), operator(/)), only: &
                real_matrix => matrix, operator(+), real_matrix_zero => zero, operator(*), real_matrix_one => one, operator(-), operator(/)
        ! We can make a block matrix with no extra code
        instantiate matrix_with_division_tmpl( &
                real_matrix, operator(+), real_matrix_zero, operator(*), real_matrix_one, operator(-), operator(/)), only: &
                matrix, operator(+), zero, operator(*), one, operator(-), operator(/)

        type(matrix) :: m
        integer :: i, ii, j, jj

        do j = 1, size(m%elements, dim=2)
            do i = 1, size(m%elements, dim=1)
                do jj = 1, size(m%elements(i, j)%elements, dim=2)
                    do ii = 1, size(m%elements(i,j)%elements, dim=1)
                        m%elements(i,j)%elements(ii,jj) = &
                                (j-1) * size(m%elements, dim=1) * size(m%elements(i,j)%elements) & ! whole blocks above this row
                                + (jj-1) * size(m%elements, dim=1) * size(m%elements(i,j)%elements, dim=1) & ! rows above this row
                                + (i-1) * size(m%elements(i,j)%elements, dim=1) & ! elements to the left in other blocks
                                + ii ! elements to the left in this block
                    end do
                end do
            end do
        end do
    end subroutine
end module

program use_it
    use block_matrix_m, only: &
            use_real_matrix, &
            use_complex_matrix, &
            use_integer_matrix, &
            use_tropical_semiring_matrix, &
            use_block_matrix
    implicit none

    call use_real_matrix
    call use_complex_matrix
    call use_integer_matrix
    call use_tropical_semiring_matrix
    call use_block_matrix
end program
