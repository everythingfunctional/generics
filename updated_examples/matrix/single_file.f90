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
        requires semigroup(T, combine)
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
        requires semigroup(T, combine)

        private
        public :: sconcat, stimes

        generic :: sconcat => sconcat_
        generic :: stimes => stimes_
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
        requires semigroup(T, combine)
        interface
            pure function empty()
                type(T) :: empty
            end function
        end interface
    end requirement

    requirement extended_monoid(T, combine, sconcat, stimes, empty, mconcat)
        requires extended_semigroup(T, combine, sconcat, stimes)
        requires monoid(T, combine, empty)
        interface
            pure function mconcat(list) result(combined)
                type(T), intent(in) :: list(:)
                type(T) :: combined
            end function
        end interface
    end requirement

    template derive_extended_monoid(T, combine, empty)
        requires monoid(T, combine, empty)

        private
        public :: stimes, mconcat

        instantiate derive_extended_semigroup(T, combine), only: stimes

        generic :: mconcat => mconcat_
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
        requires monoid(T, plus, zero)
        requires monoid(T, mult, one)
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
        requires semiring(T, plus, zero, mult, one)
        interface
            elemental function minus(x, y) result(difference)
                type(T), intent(in) :: x, y
                type(T) :: difference
            end function
        end interface
    end requirement

    requirement unit_ring_only_negate(T, plus, zero, mult, one, negate)
        requires semiring(T, plus, zero, mult, one)
        interface
            elemental function negate(x) result(negated)
                type(T), intent(in) :: x
                type(T) :: negated
            end function
        end interface
    end requirement

    requirement unit_ring(T, plus, zero, mult, one, minus, negate)
        requires unit_ring_only_minus(T, plus, zero, mult, one, minus)
        requires unit_ring_only_negate(T, plus, zero, mult, one, negate)
    end requirement

    template derive_unit_ring_from_minus(T, plus, zero, mult, one, minus)
        requires unit_ring_only_minus(T, plus, zero, mult, one, minus)

        private
        public :: negate

        generic :: negate => negate_
    contains
        elemental function negate_(x) result(negated)
            type(T), intent(in) :: x
            type(T) :: negated

            negated = minus(zero(), x)
        end function
    end template

    template derive_unit_ring_from_negate(T, plus, zero, mult, one, negate)
        requires unit_ring_only_negate(T, plus, zero, mult, one, negate)

        private
        public :: minus

        generic :: minus => minus_
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
        requires unit_ring(T, plus, zero, mult, one, minus, negate)
        interface
            elemental function divide(x, y) result(quotient)
                type(T), intent(in) :: x, y
                type(T) :: quotient
            end function
        end interface
    end requirement

    requirement field_only_inverse(T, plus, zero, mult, one, minus, negate, invert)
        requires unit_ring(T, plus, zero, mult, one, minus, negate)
        interface
            elemental function invert(x) result(inverse)
                type(T), intent(in) :: x
                type(T) :: inverse
            end function
        end interface
    end requirement

    requirement field(T, plus, zero, mult, one, minus, negate, divide, invert)
        requires field_only_division(T, plus, zero, mult, one, minus, negate, divide)
        requires field_only_inverse(T, plus, zero, mult, one, minus, negate, invert)
    end requirement

    template derive_field_from_division(T, plus, zero, mult, one, minus, negate, divide)
        requires field_only_division(T, plus, zero, mult, one, minus, negate, divide)

        private
        public :: invert

        generic :: invert => invert_
    contains
        elemental function invert_(x) result(inverse)
            type(T), intent(in) :: x
            type(T) :: inverse

            inverse = divide(one(), x)
        end function
    end template

    template derive_field_from_inverse(T, plus, zero, mult, one, minus, negate, invert)
        requires field_only_inverse(T, plus, zero, mult, one, minus, negate, invert)

        private
        public :: divide

        generic :: divide => divide_
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
    public :: matrix_tmpl

    template matrix_tmpl(T, plus_t, zero_t, times_t, one_t, n)
        requires semiring(T, plus_t, zero_t, times_t, one_t)

        integer, constant :: n

        private
        public :: &
                matrix, &
                operator(+), &
                operator(*), &
                zero, &
                one, &
                matrix_subtraction_tmpl

        type :: matrix
            type(T) :: elements(n, n)
        end type

        generic :: operator(+) => plus_matrix
        generic :: operator(*) => times_matrix

        template matrix_subtraction_tmpl(minus_t)
            requires unit_ring_only_minus(T, plus_t, zero_t, times_t, one_t, minus_t)

            private
            public :: operator(-), gaussian_solver_tmpl

            generic :: operator(-) => minus_matrix

            template gaussian_solver_tmpl(div_t)
                instantiate derive_unit_ring_from_minus(T, plus_t, zero_t, times_t, one_t, minus_t), only: negate
                requires field_only_division(T, plus_t, zero_t, times_t, one_t, minus_t, negate, div_t)

                private
                public :: operator(/)

                generic :: operator(/) => div_matrix
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
            elemental function minus_matrix(x, y) result(difference)
                type(matrix), intent(in) :: x, y
                type(matrix) :: difference

                difference%elements = minus_t(x%elements, y%elements)
            end function
        end template
    contains
        elemental function plus_matrix(x, y) result(combined)
            type(matrix), intent(in) :: x, y
            type(matrix) :: combined

            combined%elements = plus_t(x%elements, y%elements)
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

            do concurrent (i = 1:n, j = 1:n)
                combined%elements(i, j) = sum(times(x%elements(i,:), y%elements(:,j)))
            end do
        end function

        pure function one()
            type(matrix) :: one

            integer :: i

            one%elements = zero_t()
            do concurrent (i = 1:n)
                one%elements(i, i) = one_t()
            end do
        end function
    end template
end module

module real_matrix_m
    use matrix_m, only: matrix_tmpl

    implicit none

    integer, parameter :: n = 10

    instantiate matrix_tmpl(real, operator(+), real_zero, operator(*), real_one, n), only: &
            matrix, operator(+), zero, operator(*), one, matrix_subtraction_tmpl
    instantiate matrix_subtraction_tmpl(operator(-)), only: operator(-), gaussian_solver_tmpl
    instantiate gaussian_solver_tmpl(operator(/)), only: operator(/)
contains
    pure function real_zero()
        real :: real_zero

        real_zero = 0.
    end function

    pure function real_one()
        real :: real_one

        real_one = 1.
    end function
end module

module complex_matrix_m
    use matrix_m, only: matrix_tmpl

    implicit none

    integer, parameter :: n = 10

    instantiate matrix_tmpl(complex, operator(+), complex_zero, operator(*), complex_one, n), only: &
            matrix, operator(+), zero, operator(*), one, matrix_subtraction_tmpl
    instantiate matrix_subtraction_tmpl(operator(-)), only: operator(-), gaussian_solver_tmpl
    instantiate gaussian_solver_tmpl(operator(/)), only: operator(/)
contains
    pure function complex_zero()
        complex :: complex_zero

        complex_zero = 0.
    end function

    pure function complex_one()
        complex :: complex_one

        complex_one = 1.
    end function
end module

module integer_matrix_m
    use matrix_m, only: matrix_tmpl

    implicit none

    integer, parameter :: n = 10

    instantiate matrix_tmpl(integer, operator(+), integer_zero, operator(*), integer_one, n), only: &
            matrix, operator(+), zero, operator(*), one, matrix_subtraction_tmpl
    instantiate matrix_subtraction_tmpl(operator(-)), only: operator(-)
    ! Note, gaussian_solver wouldn't work correctly for integer matrix
contains
    pure function integer_zero()
        integer :: integer_zero

        integer_zero = 0
    end function

    pure function integer_one()
        integer :: integer_one

        integer_one = 1
    end function
end module

module tropical_semiring_m
    !! This is a useful representation for graphs for certain algorithms.
    use matrix_m, only: matrix_tmpl

    implicit none

    integer, parameter :: n = 10

    instantiate matrix_tmpl(real, min, real_inf, operator(+), real_zero, n), only: &
            matrix, operator(+), zero, operator(*)
contains
    pure function real_inf()
        use ieee_arithmetic, only: ieee_value, ieee_positive_inf

        real :: real_inf

        real_inf = ieee_value(real_inf, ieee_positive_inf)
    end function

    pure function real_zero()
        real :: real_zero

        real_zero = 0.
    end function
end module

module block_matrix_m
    use matrix_m, only: matrix_tmpl
    use real_matrix_m, only: &
            block_ => matrix, &
            operator(+), &
            zero_b => zero, &
            operator(*), &
            one_b => one, &
            operator(-), &
            operator(/)

    implicit none

    instantiate matrix_tmpl(block_, operator(+), zero_b, operator(*), one_b, 5), only: &
            matrix, operator(+), zero, operator(*), one, matrix_subtraction_tmpl
    instantiate matrix_subtraction_tmpl(operator(-)), only: operator(-), gaussian_solver_tmpl
    instantiate gaussian_solver_tmpl(operator(/)), only: operator(/)
end module

program use_it
    use block_matrix_m
    implicit none

    type(matrix) :: m
    integer :: i, j

    do j = 1, size(m%elements, dim=2)
        do i = 1, size(m%elements, dim=1)
            call random_number(m%elements(i,j)%elements)
        end do
    end do
    print *, m
end program

