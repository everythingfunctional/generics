module matrix_example
    implicit none
    private
    public :: run_it

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
    contains
        pure function sconcat(list) result(combined)
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

        elemental function stimes(n, a) result(repeated)
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

        instantiate derive_extended_semigroup(T, combine), only: stimes => stimes
    contains
        pure function mconcat(list) result(combined)
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

    requirement semiring(T, plus, zero, mult, one)
        require :: monoid(T, plus, zero)
        require :: monoid(T, mult, one)
    end requirement

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
    contains
        elemental function negate(x) result(negated)
            type(T), intent(in) :: x
            type(T) :: negated

            negated = minus(zero(), x)
        end function
    end template

    template derive_unit_ring_from_negate(T, plus, zero, mult, one, negate)
        require :: unit_ring_only_negate(T, plus, zero, mult, one, negate)

        private
        public :: minus
    contains
        elemental function minus(x, y) result(difference)
            type(T), intent(in) :: x, y
            type(T) :: difference

            difference = plus(x, negate(y))
        end function
    end template

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
    contains
        elemental function invert(x) result(inverse)
            type(T), intent(in) :: x
            type(T) :: inverse

            inverse = divide(one(), x)
        end function
    end template

    template derive_field_from_inverse(T, plus, zero, mult, one, minus, negate, invert)
        require :: field_only_inverse(T, plus, zero, mult, one, minus, negate, invert)

        private
        public :: divide
    contains
        elemental function divide(x, y) result(quotient)
            type(T), intent(in) :: x, y
            type(T) :: quotient

            quotient = mult(x, invert(y))
        end function
    end template

    template matrix_tmpl(T, plus_t, zero_t, times_t, one_t)
        require :: semiring(T, plus_t, zero_t, times_t, one_t)
        instantiate derive_extended_monoid(T, plus_t, zero_t), only: sum => mconcat

        type :: matrix
            type(T) :: elements(5, 5)
        end type
    contains
        elemental function matrix_plus(x, y) result(combined)
            type(matrix), intent(in) :: x, y
            type(matrix) :: combined

            integer :: i, j

            do j = 1, size(combined%elements, dim=2)
                do i = 1, size(combined%elements, dim=1)
                    combined%elements(i,j) = plus_t(x%elements(i,j), y%elements(i, j))
                end do
            end do
        end function

        pure function matrix_zero()
            type(matrix) :: matrix_zero

            integer :: i, j

            do j = 1, size(matrix_zero%elements, dim=2)
                do i = 1, size(matrix_zero%elements, dim=1)
                    matrix_zero%elements(i, j) = zero_t()
                end do
            end do
        end function

        elemental function matrix_times(x, y) result(combined)
            type(matrix), intent(in) :: x, y
            type(matrix) :: combined

            integer :: i, j, k
            type(T) :: tmp(size(combined%elements, dim=2))

            do j = 1, size(combined%elements, dim=2)
                do i = 1, size(combined%elements, dim=1)
                    do k = 1, size(combined%elements, dim=2)
                        tmp(k) = times_t(x%elements(i,k), y%elements(k,j))
                    end do
                    combined%elements(i, j) = sum(tmp)
                end do
            end do
        end function

        pure function matrix_one()
            type(matrix) :: matrix_one

            integer :: i, j

            do j = 1, size(matrix_one%elements, dim=2)
                do i = 1, size(matrix_one%elements, dim=1)
                    if (i == j) then
                        matrix_one%elements(i, j) = one_t()
                    else
                        matrix_one%elements(i, j) = zero_t()
                    end if
                end do
            end do
        end function
    end template

    template matrix_with_subtraction_tmpl(T, plus_t, zero_t, times_t, one_t, minus_t)
        require :: unit_ring_only_minus(T, plus_t, zero_t, times_t, one_t, minus_t)
        instantiate matrix_tmpl(T, plus_t, zero_t, times_t, one_t), only: &
            matrix => matrix, &
            matrix_plus => matrix_plus, &
            matrix_zero => matrix_zero, &
            matrix_times => matrix_times, &
            matrix_one => matrix_one
    contains
        elemental function matrix_minus(x, y) result(difference)
            type(matrix), intent(in) :: x, y
            type(matrix) :: difference

            integer :: i, j

            do j = 1, size(difference%elements, dim=2)
                do i = 1, size(difference%elements, dim=1)
                    difference%elements(i,j) = minus_t(x%elements(i,j), y%elements(i, j))
                end do
            end do
        end function
    end template
contains
    pure function real_zero()
        real :: real_zero
        real_zero = 0.0
    end function

    pure function real_one()
        real :: real_one
        real_one = 1.0
    end function

    subroutine run_it
        instantiate matrix_with_subtraction_tmpl( &
                real, operator(+), real_zero, operator(*), real_one, operator(-)), only: &
                matrix => matrix, &
                matrix_zero => matrix_zero, &
                matrix_one => matrix_one, &
                matrix_plus => matrix_plus, &
                matrix_times => matrix_times, &
                matrix_minus => matrix_minus
        type(matrix) :: m1, m2, ans
        integer :: i, j
        do j = 1, size(m1%elements, dim=2)
            do i = 1, size(m1%elements, dim=1)
                m1%elements(i,j) = (j-1)*size(m1%elements, dim=1) + i
            end do
        end do
        do j = 1, size(m2%elements, dim=2)
            do i = 1, size(m2%elements, dim=1)
                m2%elements(i,j) = (j-1)*size(m2%elements, dim=1) + i + 25
            end do
        end do
        do i = 1, size(m1%elements, dim=1)
            print *, (m1%elements(i,j), j = 1, size(m1%elements, dim=2))
        end do
        do i = 1, size(m2%elements, dim=1)
            print *, (m2%elements(i,j), j = 1, size(m2%elements, dim=2))
        end do
        ans = matrix_zero()
        do i = 1, size(ans%elements, dim=1)
            print *, (ans%elements(i,j), j = 1, size(ans%elements, dim=2))
        end do
        ans = matrix_one()
        do i = 1, size(ans%elements, dim=1)
            print *, (ans%elements(i,j), j = 1, size(ans%elements, dim=2))
        end do
        ans = matrix_plus(m1, m2)
        do i = 1, size(ans%elements, dim=1)
            print *, (ans%elements(i,j), j = 1, size(ans%elements, dim=2))
        end do
        ans = matrix_times(m1, m2)
        do i = 1, size(ans%elements, dim=1)
            print *, (ans%elements(i,j), j = 1, size(ans%elements, dim=2))
        end do
        ans = matrix_minus(m1, m2)
        do i = 1, size(ans%elements, dim=1)
            print *, (ans%elements(i,j), j = 1, size(ans%elements, dim=2))
        end do
    end subroutine
end module

program main
    use matrix_example, only: run_it
    implicit none
    call run_it
end program